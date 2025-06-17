module Text.Encode.Persistent (
  module Text.Encode,
  PersistentEncode,
) where

import Text.Encode

import Data.Typeable
import Data.Coerce
import Data.Bifunctor
import Database.Persist.Class
import Database.Persist.PersistValue

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS

instance (TextEncode a, Typeable a) => PersistField (ViaTextEncode a) where
  {-# INLINE toPersistValue #-}
  toPersistValue = coerce $ PersistText . encodeText @(ViaTextEncode a)

  fromPersistValue (PersistText bs) = coerce $ first T.pack $ decodeText @(ViaTextEncode a) bs
  fromPersistValue x = Left $ T.pack $ "DeriveTextEncode PersistentEncode requires PersistText: " <> show x

data PersistentEncode

instance PersistField a => TextEncode (DeriveTextEncode PersistentEncode a) where
  encodeText (DeriveTextEncode x) =
    case toPersistValue x of
      (PersistText txt) -> txt
      x' -> error $ "DeriveTextEncode PersistentEncode requires PersistText: " <> show x'

  {-# INLINE decodeText #-}
  decodeText = bimap T.unpack DeriveTextEncode . fromPersistValue . PersistText

  {-# INLINE encodeLazyText #-}
  encodeLazyText = LT.fromStrict . encodeText

  {-# INLINE decodeLazyText #-}
  decodeLazyText = decodeText . LT.toStrict

  {-# INLINE encodeByteString #-}
  encodeByteString = TE.encodeUtf8 . encodeText

  {-# INLINE decodeByteString #-}
  decodeByteString = decodeText . TE.decodeLatin1

  {-# INLINE encodeLazyByteString #-}
  encodeLazyByteString = LBS.fromStrict . encodeByteString

  {-# INLINE decodeLazyByteString #-}
  decodeLazyByteString = decodeByteString . LBS.toStrict

  {-# INLINE encodeString #-}
  encodeString = T.unpack . encodeText

  {-# INLINE decodeString #-}
  decodeString = decodeText . T.pack
