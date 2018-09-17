module Main where

import Data.List
import Text.Regex.PCRE ((=~))

(|>) :: a -> (a -> b) -> b
x |> f = f x

main :: IO ()
main = putStrLn "CSS Generator"

{-- Mocks --}

html :: String
html = "<body><div class=\"content\"><span class=\"test-1\">test 1 <div><p class=\"paragraph\">Lorem ipsum</p></div></span><span class=\"test-2\">test 2</span><div class=\"divA\">A</div><p>B</p><div>C</div></div></body>"

regex :: String
regex = "(<[^/].*?>)|(</.*?>)"

--}

type Group = [String]


match :: String -> String -> [Group]
match text pattern = text =~ pattern

matchGroup :: Int -> [[String]] -> Maybe Group
matchGroup index match =
  if index < 0 || index >= (length . head) match
     then Nothing
     else match
            |> map (!! index)
            |> filter (\s -> s /= "")
            |> Just

{-- Dependent on mock --}

getGroup :: Int -> Maybe Group
getGroup i = matchGroup i $ match html regex

--}

getTags :: Maybe Group -> Group
getTags Nothing = []
getTags (Just g) = g

tags :: Group
tags = getTags $ getGroup 0


tagClass :: String -> Maybe Bool
tagClass tag =
  elem tag <$> getGroup 1

tagValue :: Maybe Bool -> Int
tagValue bool
  | bool == Just False  = -1
  | bool == Just True   = 1
  | otherwise           = 0


sumLevels :: [Int] -> [Int]
sumLevels lvls = 
  map (sumLevel lvls) [1..(length lvls)]

sumLevel :: [Int] -> Int -> Int
sumLevel lvls 1 = head lvls
sumLevel lvls i = (lvls !! (i-1)) + (sumLevel lvls (i-1))


levels :: Group -> [Int]
levels getGroup =
  getGroup
    |> map tagClass
    |> map tagValue
    |> sumLevels


tagsWithLevel :: [(String, Int)]
tagsWithLevel = 
  tags `zip` (levels tags)


getClass :: String -> String
getClass tag = 
  let match = (tag =~ "(?<=class=(?:\"|')).*?(?=(?:\"|'))") in
      if match == [[]] || match == []
         then ""
         else (head . head) match

classesWithLevel :: [(String, Int)]
classesWithLevel =
  tagsWithLevel
    |> map (\(t, l) -> (getClass t, l))
    |> filter (\(c, l) -> c /= "")

classesWithLevelPlan :: [(String, Int)]
classesWithLevelPlan =
  classesWithLevel
    |> map (\(c, l) -> (c, (getInt . index) l))
    where index = getIndex $ map snd classesWithLevel


-- Hack
getInt :: Maybe Int -> Int
getInt Nothing = 0
getInt (Just n) = n

getIndex :: Ord a => [a] -> (a -> Maybe Int)
getIndex list =
  let 
    planList = (map head . group . sort) list
    indexes = [1..length planList]
  in
    (\n ->
      (indexes !!) <$> (findIndex ((==) n) planList))


{--
All tags:    (<.*?>)
Open tags:   (<[^/].*?>)
Close tags:  (<\/.*?>)
Classes:     (?<=class=(?:"|')).*?(?=(?:"|'))
--}
