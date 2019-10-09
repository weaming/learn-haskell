-- ch4 exercise 3
import           Ch4                            ( splitLines )
import           Ch4Cli                         ( mainWithFuncAnd2Args )

firstWordOfLines :: String -> String
firstWordOfLines cs = unlines (map iHead (map words (lines cs)))
  where
    iHead :: [[Char]] -> [Char]
    iHead (x : _) = x
    iHead _       = ""

main = mainWithFuncAnd2Args firstWordOfLines
