module Adapter.InMemory.Auth where

import ClassyPrelude
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Data.Has (Has (getter))
import qualified Domain.Auth as D
import Text.StringRandom

data State = State
  { stateAuths :: [(D.UserId, D.Auth)],
    stateUnverifiedEmails :: Map D.VerificationCode D.Email,
    stateVerifiedEmails :: Set D.Email,
    stateUserIdCounter :: Int,
    stateNotifications :: Map D.Email D.VerificationCode,
    stateSessions :: Map D.SessionId D.UserId
  }
  deriving (Eq, Show)

initialState :: State
initialState =
  State
    { stateAuths = [],
      stateUnverifiedEmails = mempty,
      stateVerifiedEmails = mempty,
      stateUserIdCounter = 0,
      stateNotifications = mempty,
      stateSessions = mempty
    }

type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

addAuth :: InMemory r m => D.Auth -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth auth = do
  tvar <- asks getter
  -- generate verification code
  vCode <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    -- check whether the given email is duplicate
    let auths = stateAuths state
        email = D.authEmail auth
        isDuplicate = any (email ==) . map (D.authEmail . snd) $ auths
    when isDuplicate $ throwError D.RegistrationErrorEmailTaken
    -- otherwise, update the state with new Email
    let newUserId = stateUserIdCounter state + 1
        newAuths = (newUserId, auth) : auths
        unVerifiedEmails = stateUnverifiedEmails state
        newUnverifiedEmails = insertMap vCode email unVerifiedEmails
        newState =
          state
            { stateAuths = newAuths,
              stateUserIdCounter = newUserId,
              stateUnverifiedEmails = newUnverifiedEmails
            }
    lift $ writeTVar tvar newState
    return (newUserId, vCode)

setEmailAsVerified :: InMemory r m => D.VerificationCode -> m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerified vCode = do
  tvar <- asks getter
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let unverifiedEmails = stateUnverifiedEmails state
        verifiedEmails = stateVerifiedEmails state
        mayEmail = lookup vCode unverifiedEmails
    email <- mayEmail `orThrow` D.EmailVerificationErrorInvalidCode
    let auths = stateAuths state
        mayUserId = map fst . find ((email ==) . D.authEmail . snd) $ auths
    uId <- mayUserId `orThrow` D.EmailVerificationErrorInvalidCode
    let newVerifiedEmails = insertSet email verifiedEmails
        newUnVerifiedEmails = deleteMap vCode unverifiedEmails
        newState =
          state
            { stateUnverifiedEmails = newUnVerifiedEmails,
              stateVerifiedEmails = newVerifiedEmails
            }
    lift $ writeTVar tvar newState
    return (uId, email)

orThrow :: MonadError e m => Maybe a -> e -> m a
orThrow Nothing e = throwError e
orThrow (Just a) _ = return a

findUserByAuth :: InMemory r m => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth auth = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  let mayUserId = map fst . find ((auth ==) . snd) $ stateAuths state
  case mayUserId of
    Nothing -> return Nothing
    Just uId -> do
      let verifiedEmails = stateVerifiedEmails state
          email = D.authEmail auth
          isVerified = elem email verifiedEmails
      return $ Just (uId, isVerified)

findEmailFromUserId :: InMemory r m => D.UserId -> m (Maybe D.Email)
findEmailFromUserId userId = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  let myAuth = map snd . find ((userId ==) . fst) $ stateAuths state
  return $ D.authEmail <$> myAuth

getNotificationsForEmail :: InMemory r m => D.Email -> m (Maybe D.VerificationCode)
getNotificationsForEmail email = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  return $ lookup email $ stateNotifications state

notifyEmailVerification :: InMemory r m => D.Email -> D.VerificationCode -> m ()
notifyEmailVerification email vCode = do
  tvar <- asks getter
  atomically $ do
    state <- readTVar tvar
    let notifications = stateNotifications state
        newNotifications = insertMap email vCode notifications
        newState = state {stateNotifications = newNotifications}
    writeTVar tvar newState

newSession :: InMemory r m => D.UserId -> m D.SessionId
newSession userId = do
  tvar <- asks getter
  sessionId <- liftIO $ ((tshow userId) <>) <$> stringRandomIO "[A-Za-z0-9]{16}"
  atomically $ do
    state <- readTVar tvar
    let sessions = stateSessions state
        newSessions = insertMap sessionId userId sessions
        newState = state {stateSessions = newSessions}
    writeTVar tvar newState
    return sessionId

findUserIdBySessionId :: InMemory r m => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId sessionId = do
  tvar <- asks getter
  liftIO $ lookup sessionId . stateSessions <$> readTVarIO tvar