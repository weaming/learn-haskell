module Ch4
    ( splitLines
    )
where
import           Data.Char
import Data.List (foldl')

splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of
            ('\r' : '\n' : rest) -> splitLines rest
            ('\r'        : rest) -> splitLines rest
            ('\n'        : rest) -> splitLines rest
            _                    -> []

isLineTerminator c = c == '\r' || c == '\n'


-- 1
safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]
safeLast :: [a] -> Maybe a
safeInit :: [a] -> Maybe [a]

safeList :: ([a] -> b) -> [a] -> Maybe b
safeList fn [] = Nothing
safeList fn xs = Just (fn xs)

safeHead = safeList head
safeTail = safeList tail
safeLast = safeList last
safeInit = safeList init

-- 2
-- splitWith :: (a -> Bool) -> [a] -> [[a]]

-- 3


-- Exercies
-- 1
asInt_fold :: String -> Int
asInt_fold xs =  (foldl' step 0 (map digitToInt xxs)) * carry
                    where step acc x = acc * 10 + x
                          (carry, xxs)  = case xs of
                                    ('-': xxs) -> (-1, xxs)
                                    otherwise -> (1, xs)
