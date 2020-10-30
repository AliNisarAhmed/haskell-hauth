module Domain.Auth
  ( -- * Types
    Auth (..),
    Email,
    mkEmail,
    rawEmail,
    Password,
    mkPassword,
    rawPassword,
    UserId,
    VerificationCode,
    SessionId,
    RegistrationError (..),
    EmailVerificationError (..),
    LoginError (..),

    -- * Ports
    AuthRepo (..),
    EmailVerificationNotif (..),
    SessionRepo (..),

    -- * Use cases
    register,
    verifyEmail,
    login,
    resolveSessionId,
    getUser,
  )
where

import ClassyPrelude
import Control.Monad.Except
import Domain.Validation
import Katip (KatipContext, Severity (..), katipAddContext, logTM, ls, sl)
import Text.RawString.QQ (r)
import Text.Regex.PCRE

data Auth = Auth
  { authEmail :: Email,
    authPassword :: Password
  }
  deriving (Eq, Show)

data RegistrationError
  = RegistrationErrorEmailTaken
  deriving (Eq, Show)

newtype Email = Email {emailRaw :: Text}
  deriving (Eq, Show, Ord)

rawEmail :: Email -> Text
rawEmail = emailRaw

mkEmail :: Text -> Either [Text] Email
mkEmail =
  validate
    Email
    [ regexMatches [r|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}|] "Not a valid email"
    ]

newtype Password = Password {passwordRaw :: Text}
  deriving (Eq, Show)

mkPassword :: Text -> Either [Text] Password
mkPassword =
  validate
    Password
    [ lengthBetween 5 50 "Should be between 5 and 50 Characters",
      regexMatches [r|\d|] "Should contain number",
      regexMatches [r|[A-Z]|] "Should contain uppercase letter",
      regexMatches [r|[a-z]|] "Should contain lowercase letter"
    ]

rawPassword :: Password -> Text
rawPassword = passwordRaw

---- Registration ----

type VerificationCode = Text

class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError (UserId, VerificationCode))
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError (UserId, Email))
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  findEmailFromUserId :: UserId -> m (Maybe Email)

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

register ::
  (AuthRepo m, EmailVerificationNotif m, KatipContext m) =>
  Auth ->
  m (Either RegistrationError ())
register auth = runExceptT $ do
  (uId, vCode) <- ExceptT $ addAuth auth
  let email = authEmail auth
  lift $ notifyEmailVerification email vCode
  withUserIdContext uId $ $(logTM) InfoS $ ls (rawEmail email) <> " is registered successfully"

withUserIdContext :: (KatipContext m) => UserId -> m a -> m a
withUserIdContext uId = katipAddContext (sl "userId" uId)

---- Email Verification ----

data EmailVerificationError = EmailVerificationErrorInvalidCode
  deriving (Eq, Show)

verifyEmail :: (KatipContext m, AuthRepo m) => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail vCode = runExceptT $ do
  (uId, email) <- ExceptT $ setEmailAsVerified vCode
  withUserIdContext uId $
    $(logTM) InfoS $ ls (rawEmail email) <> " is verified successfully"

---- User Login ----

type UserId = Int

type SessionId = Text

data LoginError
  = LoginErrorInvalidAuth
  | LoginErrorEmailNotVerified
  deriving (Eq, Show)

class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)

login ::
  (AuthRepo m, SessionRepo m, KatipContext m) =>
  Auth ->
  m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing -> throwError LoginErrorInvalidAuth
    Just (_, False) -> throwError LoginErrorEmailNotVerified
    Just (uId, _) -> withUserIdContext uId . lift $ do
      sId <- newSession uId
      $(logTM) InfoS $ ls (rawEmail $ authEmail auth) <> " logged in successfully"
      return sId

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId

---- User Page ----

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId