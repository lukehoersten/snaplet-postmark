{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Postmark where

import           Control.Applicative        ((<$>))
import           Control.Monad              (when)
import           Control.Monad.State        (get)
import           Control.Monad.Trans        (MonadIO, liftIO)
import           Control.Monad.Trans.Writer (WriterT (..), tell)
import qualified Data.Configurator          as C
import           Data.List                  (intercalate)
import           Data.Maybe                 (fromMaybe, isNothing)
import qualified Data.Text                  as T
import           Network.Api.Postmark       (Email (..), PostmarkError,
                                             PostmarkResponse, PostmarkSettings,
                                             Sent, email, emails, postmarkHttps,
                                             request)
import           Snap.Snaplet               (Handler, SnapletInit,
                                             getSnapletUserConfig, makeSnaplet)


newtype Postmark = Postmark { unPostmark :: PostmarkSettings } deriving Show


class (MonadIO m, Functor m) => HasPostmark m where
    getPostmark :: m Postmark


instance HasPostmark (Handler b Postmark) where
    getPostmark = get


logErr :: MonadIO m => t -> IO (Maybe a) -> WriterT [t] m (Maybe a)
logErr err m = do
  res <- liftIO m
  when (isNothing res) (tell [err])
  return res


initPostmark :: SnapletInit b Postmark
initPostmark = makeSnaplet "postmark" "Postmark email client" Nothing $ do
  config <- getSnapletUserConfig
  (mscfg, errs) <- runWriterT $ do
    secretKey <- logErr "Must specify Strip secret key" $ C.lookup config "secret_key"
    return $ Postmark <$> (postmarkHttps <$> (T.pack <$> secretKey))
  return $ fromMaybe (error $ intercalate "\n" errs) mscfg


withPS :: HasPostmark m => (PostmarkSettings -> m a) -> m a
withPS f = unPostmark <$> getPostmark >>= f


sendEmail :: HasPostmark m => Email -> m (PostmarkResponse PostmarkError Sent)
sendEmail msg = withPS $ liftIO . flip request (email msg)


sendEmails :: HasPostmark m => [Email] -> m (PostmarkResponse PostmarkError [Sent])
sendEmails msgs = withPS $ liftIO . flip request (emails msgs)
