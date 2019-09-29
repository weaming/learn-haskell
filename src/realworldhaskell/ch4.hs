module Ch4
    ( splitLines
    )
where

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
splitWith :: (a -> Bool) -> [a] -> [[a]]
