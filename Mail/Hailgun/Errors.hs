module Mail.Hailgun.Errors
  ( HailgunErrorResponse (..), -- TODO Make it so that herMessage is a hidden detail in the next version
    toHailgunError,
    serverError,
    unexpectedError,
    gatherErrors,
    mapError,
  )
where

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Aeson.Key as K

-- TODO make this Hailgun specific and different for the Mailgun api. That way there is the correct
-- separation of concerns.

-- | An error that comes from Mailgun or the Hailgun API.
data HailgunErrorResponse = HailgunErrorResponse
  { -- | A generic message describing the error.
    herMessage :: String
  }
  deriving (Show)

toHailgunError :: String -> HailgunErrorResponse
toHailgunError = HailgunErrorResponse

instance FromJSON HailgunErrorResponse where
  parseJSON (Object v) =
    HailgunErrorResponse
      <$> v .: K.fromString "message"
  parseJSON _ = mzero

serverError :: Either HailgunErrorResponse a
serverError = retError "Server Errors - something is wrong on Mailgun’s end"

unexpectedError :: Int -> Either HailgunErrorResponse a
unexpectedError x = retError $ "Unexpected Non-Standard Mailgun Error: " ++ show x

retError :: String -> Either HailgunErrorResponse a
retError = Left . toHailgunError

mapError :: Either String a -> Either HailgunErrorResponse a
mapError = either retError Right

gatherErrors :: Either HailgunErrorResponse HailgunErrorResponse -> Either HailgunErrorResponse a
gatherErrors = either Left Left
