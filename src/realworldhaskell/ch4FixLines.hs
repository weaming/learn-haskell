import           Ch4                            ( splitLines )
import           Ch4Cli                         ( mainWithFuncAnd2Args )

fixLines :: String -> String
fixLines input = unlines (splitLines input)

main = mainWithFuncAnd2Args fixLines
