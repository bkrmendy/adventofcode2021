{-# LANGUAGE CPP #-}
module Advent where
import System.FilePath (takeDirectory, (</>))

baseDir :: String
baseDir = takeDirectory $ takeDirectory __FILE__

readInput :: String -> IO String
readInput day = readFile $ baseDir </> "input" </> day <> ".txt"

challenge
    :: (Show output)
    => Int
    -> (String -> input)
    -> (input -> output)
    -> (input -> output)
    -> IO ()
challenge day parse part1 part2 = do
    inputText <- readInput (show day)
    let structured = parse inputText
    print $ part1 structured
    print $ part2 structured