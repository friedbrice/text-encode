module Text.Encode.SqliteSimple (
  module Text.Encode,
) where

import Text.Encode

import Data.Typeable

import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

instance (TextEncode a, Typeable a) => FromField (ViaTextEncode a) where
  {-# INLINE fromField #-}
  fromField = undefined

instance (TextEncode a, Typeable a) => ToField (ViaTextEncode a) where
  {-# INLINE toField #-}
  toField = undefined

data SqliteSimpleEncode

instance (FromField a, ToField a) => TextEncode (DeriveTextEncode SqliteSimpleEncode a) where
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
