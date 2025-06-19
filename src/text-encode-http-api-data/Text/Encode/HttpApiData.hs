module Text.Encode.HttpApiData (
  module Text.Encode,
  UrlEncode,
) where

import Text.Encode

import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Web.FormUrlEncoded (FromFormKey (..), ToFormKey (..))
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

instance TextEncode a => ToFormKey (ViaTextEncode a) where
  toFormKey = coerce $ encodeText @a

  {-# INLINE toFormKey #-}

instance TextEncode a => FromFormKey (ViaTextEncode a) where
  parseFormKey = coerce $ first T.pack . decodeText @a

  {-# INLINE parseFormKey #-}

instance TextEncode a => ToHttpApiData (ViaTextEncode a) where
  toUrlPiece = coerce $ encodeText @a
  toHeader = TE.encodeUtf8 . toUrlPiece
  toQueryParam = toUrlPiece

  {-# INLINE toUrlPiece #-}
  {-# INLINE toHeader #-}
  {-# INLINE toQueryParam #-}

instance TextEncode a => FromHttpApiData (ViaTextEncode a) where
  parseUrlPiece = coerce $ first T.pack . decodeText @a
  parseHeader = parseUrlPiece . TE.decodeLatin1
  parseQueryParam = parseUrlPiece

  {-# INLINE parseUrlPiece #-}
  {-# INLINE parseHeader #-}
  {-# INLINE parseQueryParam #-}

data UrlEncode

instance (FromHttpApiData a, ToHttpApiData a) => TextEncode (DeriveTextEncode UrlEncode a) where
  encodeText = coerce $ toUrlPiece @a
  decodeText = coerce $ first T.unpack . parseUrlPiece @a

  encodeString = T.unpack . encodeText
  decodeString = decodeText . T.pack
  encodeByteString = TE.encodeUtf8 . encodeText
  decodeByteString = decodeText . TE.decodeLatin1

  {-# INLINE encodeText #-}
  {-# INLINE decodeText #-}
  {-# INLINE encodeString #-}
  {-# INLINE decodeString #-}
  {-# INLINE encodeByteString #-}
  {-# INLINE decodeByteString #-}
