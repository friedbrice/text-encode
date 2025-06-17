module Text.Encode.Aeson (
  module Text.Encode,
  JsonEncode,
) where

import Text.Encode

import Data.Aeson
import Data.Coerce
import Data.Typeable

import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT

instance (TextEncode a, Typeable a) => FromJSON (ViaTextEncode a) where
  {-# INLINE parseJSON #-}
  parseJSON (String txt) = either fail pure $ decodeText txt
  parseJSON raw = fail $ typedError @a "parseJSON" "Expected String, got " $ encode raw

instance TextEncode a => ToJSON (ViaTextEncode a) where
  {-# INLINE toJSON #-}
  toJSON = String . encodeText

  {-# INLINE toEncoding #-}
  toEncoding = toEncoding . encodeText

instance (TextEncode a, Typeable a) => FromJSONKey (ViaTextEncode a)
instance TextEncode a => ToJSONKey (ViaTextEncode a)

data JsonEncode

instance (FromJSON a, ToJSON a) => TextEncode (DeriveTextEncode JsonEncode a) where
  {-# INLINE encodeLazyByteString #-}
  encodeLazyByteString = coerce $ encode @a

  {-# INLINE decodeLazyByteString #-}
  decodeLazyByteString = coerce $ eitherDecode @a

  {-# INLINE encodeByteString #-}
  encodeByteString = LC8.toStrict . encodeLazyByteString

  {-# INLINE decodeByteString #-}
  decodeByteString = decodeLazyByteString . LC8.fromStrict

  {-# INLINE encodeString #-}
  encodeString = LC8.unpack <$> encodeLazyByteString

  {-# INLINE decodeString #-}
  decodeString = decodeLazyByteString . LC8.pack

  {-# INLINE encodeLazyText #-}
  encodeLazyText = LT.decodeLatin1 . encodeLazyByteString

  {-# INLINE decodeLazyText #-}
  decodeLazyText = decodeLazyByteString . LT.encodeUtf8

  {-# INLINE encodeText #-}
  encodeText = T.decodeLatin1 . encodeByteString

  {-# INLINE decodeText #-}
  decodeText = decodeByteString . T.encodeUtf8
