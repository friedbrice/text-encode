module Text.Encode.Persistent (
  module Text.Encode,
) where

import Text.Encode

import Data.Typeable

import Database.Persist.Class

instance (TextEncode a, Typeable a) => PersistField (ViaTextEncode a) where
  {-# INLINE fromPersistValue #-}
  fromPersistValue = undefined

  {-# INLINE toPersistValue #-}
  toPersistValue = undefined

data PersistentEncode

instance PersistField a => TextEncode (DeriveTextEncode PersistentEncode a) where
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
