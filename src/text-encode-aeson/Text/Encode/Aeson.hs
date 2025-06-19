module Text.Encode.Aeson (
  module Text.Encode,
  JsonEncode,
) where

import Text.Encode

import Data.Aeson (
  FromJSON (..),
  FromJSONKey (..),
  ToJSON (..),
  ToJSONKey (..),
  Value (..),
  eitherDecode,
  encode
 )
import Data.Coerce (coerce)
import Data.Typeable (Typeable)

import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE

instance (TextEncode a, Typeable a) => FromJSON (ViaTextEncode a) where
  parseJSON (String txt) = either fail pure $ decodeText txt
  parseJSON raw = fail $ typedError @a "parseJSON" "Expected String, got " $ encode raw

instance TextEncode a => ToJSON (ViaTextEncode a) where
  toJSON = String . encodeText
  toEncoding = toEncoding . encodeText

  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance (TextEncode a, Typeable a) => FromJSONKey (ViaTextEncode a)
instance TextEncode a => ToJSONKey (ViaTextEncode a)

data JsonEncode

instance (FromJSON a, ToJSON a) => TextEncode (DeriveTextEncode JsonEncode a) where
  encodeLazyByteString = coerce $ encode @a
  decodeLazyByteString = coerce $ eitherDecode @a
  encodeByteString = LC8.toStrict . encodeLazyByteString
  decodeByteString = decodeLazyByteString . LC8.fromStrict
  encodeString = LC8.unpack <$> encodeLazyByteString
  decodeString = decodeLazyByteString . LC8.pack
  encodeLazyText = LTE.decodeLatin1 . encodeLazyByteString
  decodeLazyText = decodeLazyByteString . LTE.encodeUtf8
  encodeText = LT.toStrict . encodeLazyText
  decodeText = decodeLazyText . LT.fromStrict

  {-# INLINE encodeLazyByteString #-}
  {-# INLINE decodeLazyByteString #-}
  {-# INLINE encodeByteString #-}
  {-# INLINE decodeByteString #-}
  {-# INLINE encodeString #-}
  {-# INLINE decodeString #-}
  {-# INLINE encodeLazyText #-}
  {-# INLINE decodeLazyText #-}
  {-# INLINE encodeText #-}
  {-# INLINE decodeText #-}
