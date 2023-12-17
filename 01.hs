import Data.Char (isDigit)
import Data.IntMap (fromList)
import Data.Text (pack, replace, unpack)
import System.IO

fileName = "01data.txt"

main :: IO ()
main = do
  withFile
    fileName
    ReadMode
    ( \h -> do
        contents <- hGetContents h
        print $ solution01b contents
    )

solution01a :: String -> Int
solution01a = sum . map extractDigits . lines

extractDigits :: String -> Int
extractDigits string = parseInt . headAndLast $ filter isDigit string

headAndLast :: String -> String
headAndLast d = [head d, last d]

parseInt :: String -> Int
parseInt d = read d :: Int

digitsDict :: [(String, String)]
digitsDict =
  [ ("one", "one1one"),
    ("two", "two2two"),
    ("three", "three3three"),
    ("four", "four4four"),
    ("five", "five5five"),
    ("six", "six6six"),
    ("seven", "seven7seven"),
    ("eight", "eight8eight"),
    ("nine", "nine9nine")
  ]

solution01b :: String -> Int
solution01b = sum . calibratedValues

calibratedValues :: String -> [Int]
calibratedValues = map calibratedValue . lines

calibratedValue :: String -> Int
calibratedValue = extractDigits . replacedString

replacedString :: String -> String
replacedString = unpack . replaceAllWords . pack
  where
    replaceAllWords = composeAll $ map replaceWord $ digitsDict
    replaceWord pair = replace (pack (fst pair)) (pack (snd pair))

composeAll :: [a -> a] -> a -> a
composeAll = foldr (.) id
