{-# LANGUAGE OverloadedStrings #-}

module Mail.Hailgun.Domains
  ( getDomains,
    HailgunDomainResponse (..),
    HailgunDomain (..),
  )
where

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Text as T
import Mail.Hailgun.Communication
import Mail.Hailgun.Errors
import Mail.Hailgun.Internal.Data
import Mail.Hailgun.MailgunApi
import Mail.Hailgun.Pagination
import qualified Network.HTTP.Client as NC
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- | Make a request to Mailgun for the domains against your account. This is a paginated request so you must specify
-- the pages of results that you wish to get back.
getDomains ::
  -- | The context to operate in which specifies which account to get the domains from.
  HailgunContext ->
  -- | The page of results that you wish to see returned.
  Page ->
  -- | The IO response which is either an error or the list of domains.
  IO (Either HailgunErrorResponse HailgunDomainResponse)
getDomains context page = do
  request <- getRequest url context (toQueryParams . pageToParams $ page)
  mgr <- NC.newManager tlsManagerSettings
  response <- NC.httpLbs request mgr
  return $ parseResponse response
  where
    url = mailgunApiPrefix ++ "/domains"

data HailgunDomainResponse = HailgunDomainResponse
  { hdrTotalCount :: Integer,
    hdrItems :: [HailgunDomain]
  }
  deriving (Show)

instance FromJSON HailgunDomainResponse where
  parseJSON (Object v) =
    HailgunDomainResponse
      <$> v .: "total_count"
      <*> v .: "items"
  parseJSON _ = mzero

data HailgunDomain = HailgunDomain
  { domainName :: T.Text,
    domainSmtpLogin :: String,
    domainSmtpPassword :: String,
    domainCreatedAt :: HailgunTime,
    domainWildcard :: Bool,
    domainSpamAction :: String -- TODO the domain spam action is probably better specified
  }
  deriving (Show)

instance FromJSON HailgunDomain where
  parseJSON (Object v) =
    HailgunDomain
      <$> v .: "name"
      <*> v .: "smtp_login"
      <*> v .: "smtp_password"
      <*> v .: "created_at"
      <*> v .: "wildcard"
      <*> v .: "spam_action"
  parseJSON _ = mzero
