module Text.Encode.Cassava (
  module Text.Encode,
  CsvEncode,
) where

import Text.Encode

import Data.Csv
import Data.Coerce

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT

instance TextEncode a => FromField (ViaTextEncode a) where
  {-# INLINE parseField #-}
  parseField = either fail pure . coerce (decodeByteString @a)

instance TextEncode a => ToField (ViaTextEncode a) where
  {-# INLINE toField #-}
  toField = coerce $ encodeByteString @a

data CsvEncode

instance (FromField a, ToField a) => TextEncode (DeriveTextEncode CsvEncode a) where
  {-# INLINE encodeByteString #-}
  encodeByteString = coerce $ toField @a

  {-# INLINE decodeByteString #-}
  decodeByteString = coerce $ runParser . parseField @a

  {-# INLINE encodeLazyByteString #-}
  encodeLazyByteString = LC8.fromStrict . encodeByteString

  {-# INLINE decodeLazyByteString #-}
  decodeLazyByteString = decodeByteString . LC8.toStrict

  {-# INLINE encodeText #-}
  encodeText = T.decodeLatin1 . encodeByteString

  {-# INLINE decodeText #-}
  decodeText = decodeByteString . T.encodeUtf8

  {-# INLINE encodeLazyText #-}
  encodeLazyText = LT.decodeLatin1 . encodeLazyByteString

  {-# INLINE decodeLazyText #-}
  decodeLazyText = decodeLazyByteString . LT.encodeUtf8

  {-# INLINE  encodeString #-}
  encodeString = C8.unpack . encodeByteString

  {-# INLINE  decodeString #-}
  decodeString = decodeByteString . C8.pack
