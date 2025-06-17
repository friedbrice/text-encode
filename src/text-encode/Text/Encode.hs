-- | This module provides classes and newtypes for deriving uniform textual encodings.
--
-- An instance @'TextEncode' A@ defines a uniform textual representation for data of type @A@.
-- Writing instances is usually straightforward, or they may be derived using 'DeriveTextEncode'.
-- Use 'ViaTextEncode' to derive instances of other classes based on a type's 'TextEncode' instance.
--
-- For example,
-- @
--     data MyType = MyTypeFoo | MyTypeBar | MyTypeFooBar
--       deriving stock (Read, Show)
--       deriving 'TextEncode'
--         via 'DeriveTextEncode' ('Cased' 'QuietSnake' ('DropPrefix' "MyType" 'ReadShowEncode')) MyType
--       deriving
--         ( Aeson.FromJSON, Aeson.ToJSON
--         , Cassava.FromField, Cassava.ToField
--         , Persistent.PersistField
--         ) via 'ViaTextEncode' MyType
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

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Control.Monad
import Data.Coerce
import Data.Kind
import Data.Typeable
import GHC.TypeLits
import Text.Casing
import Text.Read

-- | An instance @'TextEncode' a@ defines a uniform textual representation for data of type @a@.
--
-- Instances are assumed to follow these mutual-coherence properties:
-- * @'decodeString' . 'encodeString' == 'Right'@
-- * @'encodeString' . 'decodeString' == 'Right'@
--
--
-- Minimal definitions consist of 'encodeString', 'decodeString'.
-- Mutually-coherent default definitions of the remaining methods are provided, though
-- users may supply their own implementations for performance reasons.
-- It is the user's responsibility to ensure that these definitions mutually cohere.
class TextEncode a where
  {-# MINIMAL encodeString, decodeString #-}
  encodeString :: a -> String
  decodeString :: String -> Either String a

  {-# INLINE encodeText #-}
  encodeText :: a -> T.Text
  encodeText = T.pack . encodeString

  {-# INLINE decodeText #-}
  decodeText :: T.Text -> Either String a
  decodeText = decodeString . T.unpack

  {-# INLINE encodeLazyText #-}
  encodeLazyText :: a -> LT.Text
  encodeLazyText = LT.pack . encodeString

  {-# INLINE decodeLazyText #-}
  decodeLazyText :: LT.Text -> Either String a
  decodeLazyText = decodeString . LT.unpack

  {-# INLINE encodeByteString #-}
  encodeByteString :: a -> C8.ByteString
  encodeByteString = C8.pack . encodeString

  {-# INLINE decodeByteString #-}
  decodeByteString :: C8.ByteString -> Either String a
  decodeByteString = decodeString . C8.unpack

  {-# INLINE encodeLazyByteString #-}
  encodeLazyByteString :: a -> LC8.ByteString
  encodeLazyByteString = LC8.pack . encodeString

  {-# INLINE decodeLazyByteString #-}
  decodeLazyByteString :: LC8.ByteString -> Either String a
  decodeLazyByteString = decodeString . LC8.unpack

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
  {-# INLINE encodeString #-}
  encodeString = show . fromEnum

  {-# INLINE decodeString #-}
  decodeString = coerce $ maybeDecode @a $ toEnumMaybe <=< readMaybe

data ReadShowEncode

instance (Read a, Show a, Typeable a) => TextEncode (DeriveTextEncode ReadShowEncode a) where
  {-# INLINE encodeString #-}
  encodeString = show

  {-# INLINE decodeString #-}
  decodeString = coerce $ maybeDecode @a readMaybe

data DropPrefix (pfx :: Symbol) (opt :: Type)

instance (KnownSymbol pfx, TextEncode (DeriveTextEncode opt a)) => TextEncode (DeriveTextEncode (DropPrefix pfx opt) a) where
  {-# INLINE encodeString #-}
  encodeString = coerce $ dropPfx @pfx . encodeString @(DeriveTextEncode opt a)

  {-# INLINE decodeString #-}
  decodeString = coerce $ decodeString @(DeriveTextEncode opt a) . (symbolVal (Proxy @pfx) <>)

data AddPrefix (pfx :: Symbol) (opt :: Type)

instance (KnownSymbol pfx, TextEncode (DeriveTextEncode opt a)) => TextEncode (DeriveTextEncode (AddPrefix pfx opt) a) where
  {-# INLINE encodeString #-}
  encodeString = coerce $ (symbolVal (Proxy @pfx) <>) . encodeString @(DeriveTextEncode opt a)

  {-# INLINE decodeString #-}
  decodeString = coerce $ decodeString @(DeriveTextEncode opt a) . dropPfx @pfx

data Casing
  = Camel
  | Kebab
  | Pascal
  | QuietSnake
  | ScreamingSnake
  | Snake

class CaseConversion (d :: Casing) (e :: Casing) where
  encoding :: String -> String
  decoding :: String -> String

instance CaseConversion c c where
  {-# INLINE encoding #-}
  encoding = id

  {-# INLINE decoding #-}
  decoding = id

instance CaseConversion e d => CaseConversion d e where
  {-# INLINE encoding #-}
  encoding = decoding @e @d

  {-# INLINE decoding #-}
  decoding = encoding @e @d

instance CaseConversion 'Camel 'Kebab where
  {-# INLINE encoding #-}
  encoding = toKebab . fromHumps

  {-# INLINE decoding #-}
  decoding = toCamel . fromKebab

instance CaseConversion 'Camel 'Pascal where
  {-# INLINE encoding #-}
  encoding = toPascal . fromHumps

  {-# INLINE decoding #-}
  decoding = toCamel . fromHumps

instance CaseConversion 'Camel 'QuietSnake where
  {-# INLINE encoding #-}
  encoding = toQuietSnake . fromHumps

  {-# INLINE decoding #-}
  decoding = toCamel . fromSnake

instance CaseConversion 'Camel 'ScreamingSnake where
  {-# INLINE encoding #-}
  encoding = toScreamingSnake . fromHumps

  {-# INLINE decoding #-}
  decoding = toCamel . fromSnake

instance CaseConversion 'Camel 'Snake where
  {-# INLINE encoding #-}
  encoding = toSnake . fromHumps

  {-# INLINE decoding #-}
  decoding = toCamel . fromSnake

instance CaseConversion 'Kebab 'Pascal where
  {-# INLINE encoding #-}
  encoding = toPascal . fromKebab

  {-# INLINE decoding #-}
  decoding = toKebab . fromHumps

instance CaseConversion 'Kebab 'QuietSnake where
  {-# INLINE encoding #-}
  encoding = toQuietSnake . fromKebab

  {-# INLINE decoding #-}
  decoding = toKebab . fromSnake

instance CaseConversion 'Kebab 'ScreamingSnake where
  {-# INLINE encoding #-}
  encoding = toScreamingSnake . fromKebab

  {-# INLINE decoding #-}
  decoding = toKebab . fromSnake

instance CaseConversion 'Kebab 'Snake where
  {-# INLINE encoding #-}
  encoding = toSnake . fromKebab

  {-# INLINE decoding #-}
  decoding = toKebab . fromHumps

instance CaseConversion 'Pascal 'QuietSnake where
  {-# INLINE encoding #-}
  encoding = toQuietSnake . fromHumps

  {-# INLINE decoding #-}
  decoding = toPascal . fromSnake

instance CaseConversion 'Pascal 'ScreamingSnake where
  {-# INLINE encoding #-}
  encoding = toScreamingSnake . fromHumps

  {-# INLINE decoding #-}
  decoding = toPascal . fromSnake

instance CaseConversion 'Pascal 'Snake where
  {-# INLINE encoding #-}
  encoding = toSnake . fromHumps

  {-# INLINE decoding #-}
  decoding = toPascal . fromSnake

instance CaseConversion 'QuietSnake 'ScreamingSnake where
  {-# INLINE encoding #-}
  encoding = toScreamingSnake . fromSnake

  {-# INLINE decoding #-}
  decoding = toQuietSnake . fromSnake

instance CaseConversion 'QuietSnake 'Snake where
  {-# INLINE encoding #-}
  encoding = toSnake . fromSnake

  {-# INLINE decoding #-}
  decoding = toQuietSnake . fromSnake

data Cased (decoding :: Casing) (encoding :: Casing) (opt :: Type)

instance (CaseConversion d e, TextEncode (DeriveTextEncode opt a)) => TextEncode (DeriveTextEncode (Cased d e opt) a) where
  {-# INLINE encodeString #-}
  encodeString = coerce $ encoding @d @e . encodeString @(DeriveTextEncode opt a)

  {-# INLINE decodeString #-}
  decodeString = coerce $ decodeString @(DeriveTextEncode opt a) . decoding @d @e

type FunctionName = String
type Message = String
type Input a = a

{-# INLINE typedError #-}
typedError :: forall a b. (Typeable a, Show b) => FunctionName -> Message -> Input b -> String
typedError fn msg input = mconcat [fn, " @", show (typeRep $ Proxy @a), ": ", msg, show input]

{-# INLINE decodeError #-}
decodeError :: forall a b. Typeable a => Input String -> Either String b
decodeError input = Left $ typedError @a "decode" "Failed to decode " input

{-# INLINE maybeDecode #-}
maybeDecode :: forall a. Typeable a => (String -> Maybe a) -> Input String -> Either String a
maybeDecode f raw = maybe (decodeError @a raw) Right $ f raw

{-# INLINE toEnumMaybe #-}
toEnumMaybe :: forall a. (Bounded a, Enum a) => Int -> Maybe a
toEnumMaybe n = toEnum n <$ guard (fromEnum (minBound @a) <= n && n <= fromEnum (maxBound @a))

{-# INLINE dropPfx #-}
dropPfx :: forall pfx. KnownSymbol pfx => String -> String
dropPfx = drop $ length $ symbolVal $ Proxy @pfx
