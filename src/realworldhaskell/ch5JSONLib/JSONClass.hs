{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module JSONClass
    ( JAry(..)
    , JObj(..)
    , JValue(..)
    )
where

import           Data.List
import           Control.Arrow                  ( second )

type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

data JValue = JString String
    | JNumber Double
    | JBool Bool
    | JNull
    | JObject (JObj JValue)   -- was [(String, JValue)]
    | JArray (JAry JValue)    -- was [JValue]
      deriving (Eq, Ord, Show)

instance JSON JValue where
    toJValue   = id
    fromJValue = Right

-- data Maybe a = Nothing
--             | Just a
--             deriving (Eq, Ord, Read, Show)

-- data Either a b = Left a
--                 | Right b
--                   deriving (Eq, Ord, Read, Show)

instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _         = Left "not a JSON boolean"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _           = Left "not a JSON number"

instance JSON Int where
    toJValue   = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue   = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue   = JNumber
    fromJValue = doubleToJValue id


class Foo a where
    foo :: a -> String

instance Foo a => Foo [a] where
    foo = concat . intersperse ", " . map foo

instance Foo Char where
    foo c = [c]

instance Foo Int where
    foo i = show i

instance Foo String where
    foo = id

newtype JAry a = JAry
    { fromJAry :: [a]
    } deriving (Eq, Ord, Show)

newtype JObj a = JObj
    { fromJObj :: [(String, a)]
    } deriving (Eq, Ord, Show)

instance (JSON a) => JSON (JAry a) where
    toJValue   = jaryToJValue
    fromJValue = jaryFromJValue

instance (JSON a) => JSON (JObj a) where
    toJValue = JObject . JObj . map (second toJValue) . fromJObj

    fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
        where unwrap (k, v) = whenRight (k, ) (fromJValue v)
    fromJValue _ = Left "not a JSON object"

jaryToJValue :: (JSON a) => JAry a -> JValue
jaryToJValue = JArray . JAry . map toJValue . fromJAry

listToJValues :: (JSON a) => [a] -> [JValue]
listToJValues = map toJValue

jvaluesToJAry :: [JValue] -> JAry JValue
jvaluesToJAry = JAry

jaryOfJValuesToJValue :: JAry JValue -> JValue
jaryOfJValuesToJValue = JArray

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryFromJValue (JArray (JAry a)) = whenRight JAry (mapEithers fromJValue a)
jaryFromJValue _                 = Left "not a JSON array"

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left  err) = Left err
whenRight f (Right a  ) = Right (f a)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x : xs) = case mapEithers f xs of
    Left  err -> Left err
    Right ys  -> case f x of
        Left  err -> Left err
        Right y   -> Right (y : ys)
mapEithers _ _ = Right []

