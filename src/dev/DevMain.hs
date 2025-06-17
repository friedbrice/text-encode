{-# LANGUAGE OverloadedStrings #-}

module DevMain where

import Text.Encode

import Text.Encode.Aeson ()
import Text.Encode.Cassava ()
import Text.Encode.Persistent ()

import Data.Functor (($>))
import Data.ByteString.Lazy (LazyByteString)

import qualified Data.Aeson as Aeson
import qualified Data.Csv as Cassava
import qualified Database.Persist as Persistent

data MyType = MyTypeFoo | MyTypeBar | MyTypeFooBar
    deriving stock (Read, Show, Eq, Ord, Bounded, Enum)
  deriving TextEncode
    via DeriveTextEncode (Cased 'Pascal 'QuietSnake (DropPrefix "MyType" ReadShowEncode)) MyType
  deriving
    ( Aeson.FromJSON, Aeson.ToJSON
    , Cassava.FromField, Cassava.ToField
    , Persistent.PersistField
    )
    via ViaTextEncode MyType

data YourType = YourTypeFoo | YourTypeBar | YourTypeFooBar
  deriving stock (Read, Show, Eq, Ord, Bounded, Enum)
  deriving TextEncode
    via DeriveTextEncode BoundedEnumEncode YourType
  deriving
    ( Aeson.FromJSON, Aeson.ToJSON
    , Cassava.FromField, Cassava.ToField
    , Persistent.PersistField
    )
    via ViaTextEncode YourType

myCases :: [(MyType, LazyByteString)]
myCases =
  [ (MyTypeFoo, "foo")
  , (MyTypeBar, "bar")
  , (MyTypeFooBar, "foo_bar")
  ]

yourCases :: [(YourType, LazyByteString)]
yourCases =
  [ (YourTypeFoo, "0")
  , (YourTypeBar, "1")
  , (YourTypeFooBar, "2")
  ]

quoted :: LazyByteString -> LazyByteString
quoted = ("\"" <>) . (<> "\"")

checkCase :: (Eq a, Show a, Aeson.FromJSON a, Aeson.ToJSON a) => (a, LazyByteString) -> IO Int
checkCase (dec, enc) = do
  let enc' = quoted enc
  putStr $ "Checking:\t" <> show dec <> " <-> " <> show enc
  let encodingWorks = Aeson.encode dec == enc'
  let decodingWorks = Aeson.decode enc' == Just dec
  if encodingWorks && decodingWorks
    then putStrLn "\tPassed." $> 0
    else putStrLn "\tFAILED!" $> 1

main :: IO ()
main = do
  myFailure <- traverse checkCase myCases
  yourFailures <- traverse checkCase yourCases
  let failures = sum (myFailure <> yourFailures)
  if failures == 0
    then putStrLn "All tests passed."
    else putStrLn $ "There were " <> show failures <> " failures."
