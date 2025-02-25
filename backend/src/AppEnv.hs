{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppEnv
    ( AppEnv(..)
    , AppM
    , AppError(..)
    , AppAction
    , runAppM
    , runAppAction
    , throwAppError
    , catchAppError
    , liftAppAction
    , withAppEnv
    ) where

import Config (AppConfig)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM)
import Control.Monad.Except (MonadError, runExceptT, throwError, catchError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Pool (Pool)
import Data.Text (Text)
import Database.Persist.Sql (SqlBackend)
import Network.HTTP.Client (Manager)
import AbuseProtection (UrlContentFilter)
import RateLimiter (RateLimiter)

-- Application environment with all dependencies
data AppEnv = AppEnv
    { envConfig :: AppConfig
    , envPool :: Pool SqlBackend
    , envRateLimiter :: RateLimiter
    , envContentFilter :: UrlContentFilter
    , envHttpManager :: Manager
    }

-- Application error types
data AppError
    = DatabaseError Text
    | ValidationError Text
    | RateLimitError
    | ResourceNotFound Text
    | SecurityError Text
    | GeneralError Text
    deriving (Show, Eq)

-- Application monad stack
newtype AppM a = AppM
    { unAppM :: ReaderT AppEnv IO a
    } deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AppEnv
    )

-- Application action with error handling
type AppAction a = ExceptT AppError AppM a

-- Run an AppM action with the given environment
runAppM :: AppEnv -> AppM a -> IO a
runAppM env action = runReaderT (unAppM action) env

-- Run an AppAction, converting failures to Either
runAppAction :: AppEnv -> AppAction a -> IO (Either AppError a)
runAppAction env action = runAppM env (runExceptT action)

-- Run an AppAction with the given environment, throwing errors as exceptions
withAppEnv :: AppEnv -> AppAction a -> IO a
withAppEnv env action = do
    result <- runAppAction env action
    case result of
        Left err -> error $ "Unhandled AppError: " ++ show err
        Right val -> return val

-- Throw an AppError
throwAppError :: AppError -> AppAction a
throwAppError = throwError

-- Catch an AppError
catchAppError :: AppAction a -> (AppError -> AppAction a) -> AppAction a
catchAppError = catchError

-- Lift an AppM action into AppAction
liftAppAction :: AppM a -> AppAction a
liftAppAction = lift