module Conversor ( convert ) where

import Data.List
import Text.Regex.PCRE ((=~))


type Group = [String]


(|>) :: a -> (a -> b) -> b
a |> f = f a


convert :: String -> String
convert html =
  html
    |> getTags
    |> setWeight
    |> fmap (normalizeClassesWithWeight . getClassesWithWeight)
    |> fmap compileHtml
    |> (\css -> case css of
                  Nothing -> "Error: Can't convert this HTML File"
                  Just str -> str)


-- Match tags. Group 1 is open tags and group 2 is closed tags
regexTags :: String
regexTags = "(<[^/].*?[^/]>)|(</.*?[^/]>)"

-- Match class os className
regexClasses :: String
regexClasses = "class(?:Name)?=(?:\"|').*?(?:\"|')"

-- Match text between double quot
regexText :: String
regexText = "(?<=\").*?(?=\")"


getTags :: String -> [Group]
getTags = (=~ regexTags)

getClass :: String -> String
getClass = (=~ regexClasses)

clearClass :: String -> String
clearClass = (=~ regexText)


getGroup :: Int -> [Group] -> Maybe Group
getGroup i groups =
  if 0 <= i && i < (length . head) groups
     then groups
            |> map (!! i)
            |> filter (/= "")
            |> Just
     else Nothing

tagIsOpen :: [Group] -> String -> Bool
tagIsOpen tags tag
  | (elem tag <$> getGroup 1 tags) == Just True = True
  | otherwise = False


boolToWeight :: Bool -> Int
boolToWeight bool =
  if bool then 1 else -1

sumWeight :: [Int] -> [Int]
sumWeight [x] = [x]
sumWeight ws = (sumWeight base) ++ [(sum base) + w]
  where base = init ws
        w = last ws

setWeight :: [Group] -> Maybe [(String, Int)]
setWeight groups =
  tags
    |> (fmap . map) (tagIsOpen groups)
    |> (fmap . map) boolToWeight
    |> fmap sumWeight
    |> (<*>) (fmap zip tags)
    where tags = getGroup 0 groups

getClassesWithWeight :: [(String, Int)] -> [(String, Int)]
getClassesWithWeight tws =
  tws
    |> map (\(t, w) -> ((clearClass . getClass) t, w))
    |> filter (\t -> fst t /= "")


normalizer :: Ord a => [a] -> (a -> Int)
normalizer xs =
  let
    unique = (map head . group . sort) xs
    normalized = [1..length unique]
  in
    (\x -> case (normalized !!) <$> (findIndex (== x) unique) of
             Nothing -> 0
             Just n  -> n)

normalizeClassesWithWeight :: [(String, Int)] -> [(String, Int)]
normalizeClassesWithWeight tws =
  tws
    |> map (\(t, w) -> (t, normalize w))
    where normalize = normalizer (map snd tws)


toClass :: String -> String
toClass = (unwords . map ('.' :) . words)

indent :: Int -> String
indent n = replicate (2*n - 2) ' '

closeClass :: Int -> Int -> String
closeClass 0 _ = ""
closeClass close lvl =
  indent lvl ++ "}\n\n" ++ closeClass (close - 1) (lvl - 1)

compileTag :: (String, Int) -> Int -> String
compileTag tag next =
  indent lvl ++ name ++ " {\n\n" ++ (closeClass untilClose lvl)
    where name = (toClass . fst) tag
          lvl = snd tag
          untilClose = max 0 (lvl - next + 1)


compileHtml :: [(String, Int)] -> String
compileHtml [tag] = compileTag tag 1
compileHtml (tag:tags) =
  compileTag tag next ++ compileHtml tags
    where next = (snd . head) tags

