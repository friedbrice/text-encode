-- | This module provides classes and newtypes for deriving uniform textual encodings.
--
-- An instance @'TextEncode' A@ defines a uniform textual representation for data of type @A@.
-- Writing instances is usually straightforward, or they may be derived using 'DeriveTextEncode'.
-- Use 'ViaTextEncode' to derive instances of other classes based on a type's 'TextEncode' instance.
--
-- For example,
-- @
--     import Text.Encode
--
--     import Text.Encode.Aeson ()
--     import Text.Encode.Cassava ()
--     import Text.Encode.Persistent ()
--
--     data MyType = MyTypeFoo | MyTypeBar | MyTypeFooBar
--       deriving stock (Read, Show)
--       deriving 'TextEncode'
--         via 'DeriveTextEncode' ('Cased' 'Pascal' 'QuietSnake' ('DropPrefix' "MyType" 'ReadShowEncode')) MyType
--       deriving
--         ( Aeson.FromJSON, Aeson.ToJSON
--         , Cassava.FromField, Cassava.ToField
--         , Persistent.PersistField
--         )
--         via 'ViaTextEncode' MyType
-- @
--
-- This will derive a 'TextEncode' instance for @MyType@ based on the stock 'Read' and 'Show' instances,
-- modified by the 'Cased' and 'DropPrefix' options. Uniform, mutually-consistent instances for Aeson,
-- Cassava, and Persistent classes are then derived from the derived 'TextEncode' instance.
module Text.Encode (
  -- * Textual Encodings
  TextEncode (..),
  -- * Deriving 'TextEncode'
  DeriveTextEncode (..),
  -- ** Base encodings
  BoundedEnumEncode,
  ReadShowEncode,
  -- ** Add or remove prefixes
  DropPrefix,
  AddPrefix,
  -- ** Transform casing
  Casing (..),
  CaseConversion (..),
  Cased,
  -- * Deriving other classes
  ViaTextEncode (..),
  -- * Utilities
  typedError,
  decodeError,
  maybeDecode,
) where

import Text.Encode.Casing

import Control.Monad (guard, (<=<))
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Typeable (Typeable, Proxy (..), typeRep)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Text.Read (readMaybe)

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

-- | An instance @'TextEncode' a@ defines a uniform textual representation for data of type @a@.
--
-- Instances are assumed to follow these mutual-coherence properties:
-- * @decodeString . encodeString === Right@
-- * @fmap encodeString . decodeString === Right@
-- * @decodeText . "Data.Text".'pack' . encodeString === Right@
-- * @fmap encodeString . decodeByteString === Right . "Data.ByteString.Char8".'unpack'@
-- and permutations thereof.
--
-- Mutually-coherent default definitions of the methods are provided, though
-- users may supply their own implementations for performance reasons.
-- It is the user's responsibility to ensure that these definitions mutually cohere.
class TextEncode a where
  {-# MINIMAL encodeString, decodeString #-}
  encodeString :: a -> String
  decodeString :: String -> Either String a
  encodeByteString :: a -> C8.ByteString
  decodeByteString :: C8.ByteString -> Either String a
  encodeLazyByteString :: a -> LC8.ByteString
  decodeLazyByteString :: LC8.ByteString -> Either String a
  encodeText :: a -> T.Text
  decodeText :: T.Text -> Either String a
  encodeLazyText :: a -> LT.Text
  decodeLazyText :: LT.Text -> Either String a

  encodeText = T.pack . encodeString
  decodeText = decodeString . T.unpack
  encodeLazyText = LT.fromStrict . encodeText
  decodeLazyText = decodeText . LT.toStrict
  encodeByteString = C8.pack . encodeString
  decodeByteString = decodeString . C8.unpack
  encodeLazyByteString = LC8.fromStrict . encodeByteString
  decodeLazyByteString = decodeByteString . LC8.toStrict

  {-# INLINE encodeByteString #-}
  {-# INLINE decodeByteString #-}
  {-# INLINE encodeLazyByteString #-}
  {-# INLINE decodeLazyByteString #-}
  {-# INLINE encodeText #-}
  {-# INLINE decodeText #-}
  {-# INLINE encodeLazyText #-}
  {-# INLINE decodeLazyText #-}

-- | Derive instances of various classes based on an instance of 'TextEncode'.
newtype ViaTextEncode a = ViaTextEncode a
  deriving TextEncode via a

instance TextEncode a => Show (ViaTextEncode a) where
  show (ViaTextEncode a) = encodeString a

instance TextEncode a => Read (ViaTextEncode a) where
  readsPrec _ = either (const []) (pure . (, "") . ViaTextEncode) . decodeString

newtype DeriveTextEncode (opt :: Type) a = DeriveTextEncode a
  deriving (Eq, Ord, Read, Show, Bounded, Enum) via a

data BoundedEnumEncode

instance (Bounded a, Enum a, Typeable a) => TextEncode (DeriveTextEncode BoundedEnumEncode a) where
  encodeString = show . fromEnum
  decodeString = coerce $ maybeDecode @a $ toEnumMaybe <=< readMaybe

  {-# INLINE encodeString #-}
  {-# INLINE decodeString #-}

data ReadShowEncode

instance (Read a, Show a, Typeable a) => TextEncode (DeriveTextEncode ReadShowEncode a) where
  encodeString = show
  decodeString = coerce $ maybeDecode @a readMaybe

  {-# INLINE encodeString #-}
  {-# INLINE decodeString #-}

data DropPrefix (pfx :: Symbol) (opt :: Type)

instance (KnownSymbol pfx, TextEncode (DeriveTextEncode opt a)) => TextEncode (DeriveTextEncode (DropPrefix pfx opt) a) where
  encodeString = coerce $ dropPfx @pfx . encodeString @(DeriveTextEncode opt a)
  decodeString = coerce $ decodeString @(DeriveTextEncode opt a) . (symbolVal (Proxy @pfx) <>)

  {-# INLINE encodeString #-}
  {-# INLINE decodeString #-}

data AddPrefix (pfx :: Symbol) (opt :: Type)

instance (KnownSymbol pfx, TextEncode (DeriveTextEncode opt a)) => TextEncode (DeriveTextEncode (AddPrefix pfx opt) a) where
  encodeString = coerce $ (symbolVal (Proxy @pfx) <>) . encodeString @(DeriveTextEncode opt a)
  decodeString = coerce $ decodeString @(DeriveTextEncode opt a) . dropPfx @pfx

  {-# INLINE encodeString #-}
  {-# INLINE decodeString #-}

data Cased (decoding :: Casing) (encoding :: Casing) (opt :: Type)

instance (CaseConversion d e, TextEncode (DeriveTextEncode opt a)) => TextEncode (DeriveTextEncode (Cased d e opt) a) where
  encodeString = coerce $ encoding @d @e . encodeString @(DeriveTextEncode opt a)
  decodeString = coerce $ decodeString @(DeriveTextEncode opt a) . decoding @d @e

  {-# INLINE encodeString #-}
  {-# INLINE decodeString #-}

type FunctionName = String
type Message = String
type Input a = a

typedError :: forall a b. (Typeable a, Show b) => FunctionName -> Message -> Input b -> String
typedError fn msg input = mconcat [fn, " @", show (typeRep $ Proxy @a), ": ", msg, show input]

decodeError :: forall a b. Typeable a => Input String -> Either String b
decodeError input = Left $ typedError @a "decode" "Failed to decode " input

maybeDecode :: forall a. Typeable a => (String -> Maybe a) -> Input String -> Either String a
maybeDecode f raw = maybe (decodeError @a raw) Right $ f raw

toEnumMaybe :: forall a. (Bounded a, Enum a) => Int -> Maybe a
toEnumMaybe n = toEnum n <$ guard (fromEnum (minBound @a) <= n && n <= fromEnum (maxBound @a))

dropPfx :: forall pfx. KnownSymbol pfx => String -> String
dropPfx = drop $ length $ symbolVal $ Proxy @pfx

{-# INLINE typedError #-}
{-# INLINE decodeError #-}
{-# INLINE maybeDecode #-}
{-# INLINE toEnumMaybe #-}
{-# INLINE dropPfx #-}
