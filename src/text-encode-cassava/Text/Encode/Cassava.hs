module Text.Encode.Cassava (
  module Text.Encode,
  CsvEncode,
) where

import Text.Encode

import Data.Coerce (coerce)
import Data.Csv (FromField (..), ToField (..), runParser)

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.Encoding as T

instance TextEncode a => FromField (ViaTextEncode a) where
  parseField = either fail pure . coerce (decodeByteString @a)

  {-# INLINE parseField #-}

instance TextEncode a => ToField (ViaTextEncode a) where
  toField = coerce $ encodeByteString @a

  {-# INLINE toField #-}

data CsvEncode

instance (FromField a, ToField a) => TextEncode (DeriveTextEncode CsvEncode a) where
  encodeByteString = coerce $ toField @a
  decodeByteString = coerce $ runParser . parseField @a

  encodeText = T.decodeLatin1 . encodeByteString
  decodeText = decodeByteString . T.encodeUtf8
  encodeString = C8.unpack . encodeByteString
  decodeString = decodeByteString . C8.pack

  {-# INLINE encodeByteString #-}
  {-# INLINE decodeByteString #-}
  {-# INLINE encodeText #-}
  {-# INLINE decodeText #-}
  {-# INLINE  encodeString #-}
  {-# INLINE  decodeString #-}
