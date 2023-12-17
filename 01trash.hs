import Data.Foldable (minimumBy)
import Data.Function (on)

replaceUntilExhaustion :: [(String, String)] -> String -> String
replaceUntilExhaustion dict haystack = case replaceOnce dict haystack of
  (text, False) -> text
  (text, True) -> replaceUntilExhaustion dict text

replaceOnce :: [(String, String)] -> String -> (String, Bool)
replaceOnce dict haystack = case positions of
  [] -> (haystack, False)
  _ -> (replaceAtPos $ minByPosition positions, True)
  where
    replaceAtPos (s, (start, end)) = slice 0 start haystack ++ s ++ slice end (length haystack) haystack
    minByPosition = minimumBy (compare `on` fst . snd)
    positions = unwrapFilter $ replacedPositions dict
    unwrapFilter xs = [(d, pos) | (d, Just pos) <- xs]
    replacedPositions = map (\(w, d) -> (d, substringFirstPosition w haystack))

substringFirstPosition :: String -> String -> Maybe (Int, Int)
substringFirstPosition [] _ = Nothing
substringFirstPosition needle haystack = helper needle haystack 0
  where
    helper needle haystack p
      | isPrefix needle haystack = Just (p, p + length needle)
      | length needle > length haystack = Nothing
      | otherwise = helper needle (tail haystack) (p + 1)

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x : xs) (y : ys)
  | x == y = isPrefix xs ys
  | otherwise = False

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)