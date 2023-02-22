module Main where

import System.IO
import System.Environment (getArgs)
import System.Console.GetOpt
import Data.Maybe (fromMaybe)
import Control.Monad
import Text.Printf
import qualified Data.ByteString as BS (length)
import qualified Data.ByteString.UTF8 as UTF8 (fromString)

data Flag
    = Version
    | CountBytes
    | CountLines
    | CountCharacters
    | CountWords
    deriving (Show)

trim :: String -> String
trim = (unwords.words)

options :: [OptDescr Flag]
options =
    [ Option ['V'] ["version"] (NoArg Version)
        "Display version number"
    , Option ['c'] [] (NoArg CountBytes)
        "The number of bytes in each file is written to stdout"
    , Option ['l'] [] (NoArg CountLines) 
        "The number of lines in each file is written to stdout"
    -- , Option ['m'] [] (NoArg CountCharacters) 
    --     "The number of characters in each file is written to stdout"
    , Option ['w'] [] (NoArg CountWords) 
        "The number of words in each file is written to stdout"
    ]

wcOpts :: [String] -> IO ([Flag], [String])
wcOpts argv =
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: wc [OPTION...] [FILES...]"

-- countBytes :: String -> IO Integer
-- countBytes s = withFile "src/wc.hs" ReadMode $ \h -> do
--             size <- hFileSize h
--             return size

-- countBytes :: FilePath -> IO Integer
-- countBytes path = withFile path ReadMode $ \h -> hFileSize h

countBytes :: String -> Int
countBytes = BS.length . UTF8.fromString

countLines :: String -> Int
countLines = length.lines

countCharacters :: String -> Int
countCharacters = length

countWords :: String -> Int
countWords = length.words

wc :: Handle -> IO (Int, Int, Int)
wc fp = do
    content <- hGetContents fp
    return (countLines content, countWords content, countBytes content)

printWc :: FilePath -> IO ()
printWc path = withFile path ReadMode (\fp ->
        wc fp >>= (\(l,w,b) -> putStrLn $ printf "%8d %7d %7d %s" l w b path))

main :: IO ()
main = do
    (opts, files) <- (getArgs >>= wcOpts)

    if null files then
        printWc
    else
        forM_ files printWc


