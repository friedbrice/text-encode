module Text.Encode.PostgresqlSimple (
  module Text.Encode,
) where

import Text.Encode

import Control.Exception
import Data.Typeable

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

newtype FromFieldViaTextEncodeError = FromFieldViaTextEncodeError String
  deriving (Show)

instance Exception FromFieldViaTextEncodeError

instance (TextEncode a, Typeable a) => FromField (ViaTextEncode a) where
  {-# INLINE fromField #-}
  fromField x y =
    either (conversionError . FromFieldViaTextEncodeError) (pure . ViaTextEncode)
      . decodeByteString
      =<< fromField x y

instance (TextEncode a, Typeable a) => ToField (ViaTextEncode a) where
  {-# INLINE toField #-}
  toField (ViaTextEncode x) = toField $ encodeByteString @a x

data PostgresqlSimpleEncode

instance (FromField a, ToField a) => TextEncode (DeriveTextEncode PostgresqlSimpleEncode a) where
  {-# INLINE encodeLazyByteString #-}
  encodeLazyByteString = undefined

  {-# INLINE decodeLazyByteString #-}
  decodeLazyByteString = undefined

  {-# INLINE encodeByteString #-}
  encodeByteString = undefined

  {-# INLINE decodeByteString #-}
  decodeByteString = undefined

  {-# INLINE encodeString #-}
  encodeString = undefined

  {-# INLINE decodeString #-}
  decodeString = undefined

  {-# INLINE encodeLazyText #-}
  encodeLazyText = undefined

  {-# INLINE decodeLazyText #-}
  decodeLazyText = undefined

  {-# INLINE encodeText #-}
  encodeText = undefined
