-- compile executable:
-- ghc -o simple-json ch5Main.hs
-- ghc -o pretty-json ch5Main.hs

module Main where
import           SimpleJSON
-- import           PutJSON -- simple version
-- or
import           PrettyJSON                     ( putJValue ) -- pretty version

main = putJValue (JObject [("foo", JNumber 1), ("bar", JBool False)])
