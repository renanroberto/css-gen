module Main where

import Conversor

main :: IO ()
main = do
  putStrLn "CSS Generator\n"
  (putStrLn . convert) html


{--}
html :: String
html = "<body><div class=\"content\"><span class=\"test-1\">test 1 <div><p class=\"paragraph red\">Lorem ipsum</p></div></span><span class=\"test-2\">test 2</span><div class=\"divA\">A <p className=\"warning red\"</div><p>B</p><div>C</div></div></body>"
--}
