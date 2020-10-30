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
  addAuth :: Auth -> m (Either RegistrationError VerificationCode)
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError ())
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  findEmailFromUserId :: UserId -> m (Maybe Email)

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

-- instance AuthRepo IO where
--   addAuth (Auth email pass) = do
--     putStrLn $ "Adding auth: " <> rawEmail email
--     return $ Right "fake verification code"

-- instance EmailVerificationNotif IO where
--   notifyEmailVerification email vcode =
--     putStrLn $ "Notify " <> rawEmail email <> " - " <> vcode

register ::
  (AuthRepo m, EmailVerificationNotif m) =>
  Auth ->
  m (Either RegistrationError ())
register auth = runExceptT $ do
  vCode <- ExceptT $ addAuth auth
  let email = authEmail auth
  lift $ notifyEmailVerification email vCode

---- Email Verification ----

data EmailVerificationError = EmailVerificationErrorInvalidCode
  deriving (Eq, Show)

verifyEmail :: AuthRepo m => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail = setEmailAsVerified

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
  (AuthRepo m, SessionRepo m) =>
  Auth ->
  m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing -> throwError LoginErrorInvalidAuth
    Just (_, False) -> throwError LoginErrorEmailNotVerified
    Just (uId, _) -> lift $ newSession uId

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId

---- User Page ----

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId