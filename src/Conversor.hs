{--
module Lib
    ( someFunc
    ) where
--}

module Conversor where


import Text.Regex.PCRE ((=~))


type Group = [String]


(|>) :: a -> (a -> b) -> b
a |> f = f a


-- Mock
html :: String
html = "<body><div class=\"content\"><span class=\"test-1\">test 1 <div><p class=\"paragraph red\">Lorem ipsum</p></div></span><span class=\"test-2\">test 2</span><div class=\"divA\">A</div><p>B</p><div>C</div></div></body>"


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

planify :: [Int] -> [Int]
planify xs

getClassesWithWeight :: [(String, Int)] -> [(String, Int)]
getClassesWithWeight tws =
  tws
    |> map fst
    |> map (clearClass . getClass)
    |> (`zip` weights)
    |> filter (\t -> fst t /= "")
    where weights = map snd tws


toClass :: String -> String
toClass = (unwords . map ('.' :) . words)

compileTag :: (String, Int) -> Int -> String
compileTag tag next =
  space ++ toClass name ++ bracket
    where name = fst tag
          lvl = snd tag
          space = replicate (2*lvl - 2) ' '
          bracket
            | lvl < next  = "{\n\n"
            | lvl == next = "{\n\n" ++ space ++ "\n\n}"
            | lvl > next  = "{\n\n" ++ space ++ "\n\n}" ++ (tail . tail) space

compileHtml :: [(String, Int)] -> String
compileHtml [tag] = compileTag tag (snd tag - 1)
compileHtml (tag:tags) =
  compileTag tag next ++ compileHtml tags
    where next = (snd . head) tags


-- TODO: Tests and join the functions
-- fmap compileHtml $ fmap getClassesWithWeight $ (setWeight . getTags) html
