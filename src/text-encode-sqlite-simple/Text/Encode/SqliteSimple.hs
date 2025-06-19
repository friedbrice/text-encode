module Text.Encode.SqliteSimple (
  module Text.Encode,
  SqliteSimpleEncode,
  TextEncodeSqlSimpleError,
) where

import Text.Encode

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))

import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS

newtype TextEncodeSqlSimpleError = TextEncodeSqlSimpleError String
  deriving (Show)

instance Exception TextEncodeSqlSimpleError

instance (TextEncode a, Typeable a) => FromField (ViaTextEncode a) where
  fromField = undefined

  {-# INLINE fromField #-}

instance (TextEncode a, Typeable a) => ToField (ViaTextEncode a) where
  toField = undefined

  {-# INLINE toField #-}

data SqliteSimpleEncode

instance (FromField a, ToField a) => TextEncode (DeriveTextEncode SqliteSimpleEncode a) where
  encodeByteString = undefined
  decodeByteString = undefined

  encodeText = TE.decodeLatin1 . encodeByteString
  decodeText = decodeByteString . TE.encodeUtf8
  encodeString = BS.unpack . encodeByteString
  decodeString = decodeByteString . BS.pack

  {-# INLINE encodeByteString #-}
  {-# INLINE decodeByteString #-}
  {-# INLINE encodeText #-}
  {-# INLINE decodeText #-}
  {-# INLINE encodeString #-}
  {-# INLINE decodeString #-}
