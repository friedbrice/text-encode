module Text.Encode.PostgresqlSimple (
  module Text.Encode,
  PostgresqlSimpleEncode,
  TextEncodePostgresqlSimpleError,
) where

import Text.Encode

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))

import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS

newtype TextEncodePostgresqlSimpleError = TextEncodePostgresqlSimpleError String
  deriving (Show)

instance Exception TextEncodePostgresqlSimpleError

instance (TextEncode a, Typeable a) => FromField (ViaTextEncode a) where
  fromField = undefined

  {-# INLINE fromField #-}

instance (TextEncode a, Typeable a) => ToField (ViaTextEncode a) where
  toField = undefined

  {-# INLINE toField #-}

data PostgresqlSimpleEncode

instance (FromField a, ToField a) => TextEncode (DeriveTextEncode PostgresqlSimpleEncode a) where
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
