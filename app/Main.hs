module Main where

import System.IO
import System.Environment
import Conversor

main :: IO ()
main = do
  putStrLn "SASS Generator!\n" 
  args <- getArgs
  case args of
    [] -> putStrLn "No arguments has been passed"
    [file] -> do
      putStrLn "Searchin file..."
      html <- readFile file
      putStrLn "generating style..."
      let scss = convert html
      writeFile "style.scss" scss
      putStrLn "style.scss generated!"

{--
  putStrLn "CSS Generator\n"
  putStr "Type the name of the HTML file: "
  file <- getLine
  html <- readFile file
  let scss = convert html
  writeFile "style.scss" scss
--}

{--
html :: String
html = "<body><div class=\"content\"><span class=\"test-1\">test 1 <div><p class=\"paragraph red\">Lorem ipsum</p></div></span><span class=\"test-2\">test 2</span><div class=\"divA\">A <p className=\"warning red\"</div><p>B</p><div>C</div></div></body>"
--}
