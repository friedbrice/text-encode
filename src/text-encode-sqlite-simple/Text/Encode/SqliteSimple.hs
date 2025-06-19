module Text.Encode.SqliteSimple (
  module Text.Encode,
  TextEncodeSqliteSimpleError,
) where

import Text.Encode

import Control.Exception (Exception, toException)
import Control.Monad ((<=<))
import Data.Coerce (coerce)
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.ToField (ToField (..))

newtype TextEncodeSqliteSimpleError = TextEncodeSqliteSimpleError String
  deriving (Show)

instance Exception TextEncodeSqliteSimpleError

instance (TextEncode a, Typeable a) => FromField (ViaTextEncode a) where
  fromField =
    either (Errors . pure . toException . TextEncodeSqliteSimpleError) (pure . ViaTextEncode)
      . decodeText @a
      <=< fromField @T.Text

  {-# INLINE fromField #-}

instance (TextEncode a, Typeable a) => ToField (ViaTextEncode a) where
  toField = coerce $ toField . encodeText @a

  {-# INLINE toField #-}
