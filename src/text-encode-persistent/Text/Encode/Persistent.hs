module Text.Encode.Persistent (
  module Text.Encode,
  PersistentEncode,
) where

import Text.Encode

import Data.Bifunctor
import Data.Coerce
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Typeable
import Database.Persist.Class
import Database.Persist.PersistValue

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

  decodeText = bimap T.unpack DeriveTextEncode . fromPersistValue . PersistText

  encodeByteString = TE.encodeUtf8 . encodeText
  decodeByteString = decodeText . TE.decodeLatin1
  encodeString = T.unpack . encodeText
  decodeString = decodeText . T.pack

  {-# INLINE decodeText #-}
  {-# INLINE encodeByteString #-}
  {-# INLINE decodeByteString #-}
  {-# INLINE encodeString #-}
  {-# INLINE decodeString #-}
