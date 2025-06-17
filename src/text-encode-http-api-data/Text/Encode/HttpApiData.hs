module Text.Encode.HttpApiData (
  module Text.Encode,
  UrlEncode,
) where

import Text.Encode

import Data.Bifunctor
import Data.Coerce
import Web.FormUrlEncoded
import Web.HttpApiData

-- import qualified Data.ByteString.Char8 as C8
-- import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T
-- import qualified Data.Text.Lazy.Encoding as LT
-- import qualified Network.HTTP.Types.URI as URI

instance TextEncode a => ToFormKey (ViaTextEncode a) where
  {-# INLINE toFormKey #-}
  toFormKey = coerce $ encodeText @a

instance TextEncode a => FromFormKey (ViaTextEncode a) where
  {-# INLINE parseFormKey #-}
  parseFormKey = coerce $ first T.pack . decodeText @a

instance TextEncode a => ToHttpApiData (ViaTextEncode a) where
  {-# INLINE toUrlPiece #-}
  toUrlPiece = undefined

  {-# INLINE toEncodedUrlPiece #-}
  toEncodedUrlPiece = undefined

  {-# INLINE toHeader #-}
  toHeader = undefined

  {-# INLINE toQueryParam #-}
  toQueryParam = undefined

  {-# INLINE toEncodedQueryParam #-}
  toEncodedQueryParam = undefined

instance TextEncode a => FromHttpApiData (ViaTextEncode a) where
  {-# INLINE parseUrlPiece #-}
  parseUrlPiece = undefined

  {-# INLINE parseHeader #-}
  parseHeader = undefined

  {-# INLINE parseQueryParam #-}
  parseQueryParam = undefined

data UrlEncode

instance (FromHttpApiData a, ToHttpApiData a) => TextEncode (DeriveTextEncode UrlEncode a) where
  {-# INLINE encodeString #-}
  encodeString = undefined

  {-# INLINE decodeString #-}
  decodeString = undefined

  {-# INLINE encodeText #-}
  encodeText = undefined

  {-# INLINE decodeText #-}
  decodeText = undefined

  {-# INLINE encodeLazyText #-}
  encodeLazyText = undefined

  {-# INLINE decodeLazyText #-}
  decodeLazyText = undefined

  {-# INLINE encodeByteString #-}
  encodeByteString = undefined

  {-# INLINE decodeByteString #-}
  decodeByteString = undefined

  {-# INLINE encodeLazyByteString #-}
  encodeLazyByteString = undefined

  {-# INLINE decodeLazyByteString #-}
  decodeLazyByteString = undefined
