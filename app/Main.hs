module Main where

import Text.Regex.PCRE ((=~))

(|>) :: a -> (a -> b) -> b
x |> f = f x

main :: IO ()
main = putStrLn "CSS Generator"

{-- Mocks --}

html :: String
html = "<body><div><span>test 1</span><span>test 2</span><div>A</div><p>B</p><div>C</div></div></body>"

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

group :: Int -> Maybe Group
group i = matchGroup i $ match html regex

--}

getTags :: Maybe Group -> Group
getTags Nothing = []
getTags (Just group) = group

tags :: Group
tags = getTags $ group 0


tagClass :: String -> Maybe Bool
tagClass tag =
  elem tag <$> group 1

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
levels group =
  group
    |> map tagClass
    |> map tagValue
    |> sumLevels


tagsWithLevel :: [(String, Int)]
tagsWithLevel = 
  tags `zip` (levels tags)
--    |> map (\t -> ((tagClass . fst) t, fst t, snd t))
--    |> filter (\(b, _, _) -> b == Just True)
--    |> map (\(_, a, b) -> (a, b))

{--
All tags:    (<.*?>)
Open tags:   (<[^/].*?>)
Close tags:  (<\/.*?>)
--}
