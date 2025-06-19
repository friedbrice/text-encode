module Text.Encode.PostgresqlSimple (
  module Text.Encode,
  TextEncodePostgresqlSimpleError,
) where

import Text.Encode

import Control.Exception (Exception)
import Control.Monad ((<=<))
import Data.Coerce (coerce)
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.FromField (FromField (..), conversionError)
import Database.PostgreSQL.Simple.ToField (ToField (..))

newtype TextEncodePostgresqlSimpleError = TextEncodePostgresqlSimpleError String
  deriving (Show)

instance Exception TextEncodePostgresqlSimpleError

instance (TextEncode a, Typeable a) => FromField (ViaTextEncode a) where
  fromField f =
    either (conversionError . TextEncodePostgresqlSimpleError) (pure . ViaTextEncode)
      . decodeText @a
      <=< fromField @T.Text f

  {-# INLINE fromField #-}

instance (TextEncode a, Typeable a) => ToField (ViaTextEncode a) where
  toField = coerce $ toField . encodeText @a

  {-# INLINE toField #-}
