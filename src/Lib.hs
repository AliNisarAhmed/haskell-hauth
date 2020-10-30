module Lib where

import qualified Adapter.InMemory.Auth as M
import ClassyPrelude
import Control.Monad (MonadFail)
import Data.Aeson
import Data.Aeson.TH
import Domain.Auth
import Domain.Validation
import Language.Haskell.TH.Syntax (nameBase)

someFunc :: IO ()
someFunc = do
  state <- newTVarIO M.initialState
  run state action

action :: App ()
action = do
  let email = either undefined id $ mkEmail "ali@test.com"
      password = either undefined id $ mkPassword "punchP@ss1"
      auth = Auth email password
  register auth
  Just vCode <- M.getNotificationsForEmail email
  verifyEmail vCode
  Right session <- login auth
  Just uId <- resolveSessionId session
  Just registeredEmail <- getUser uId
  print (session, uId, registeredEmail)

type State = TVar M.State

newtype App a = App
  {unApp :: ReaderT State IO a}
  deriving (Applicative, Functor, Monad, MonadReader State, MonadIO, MonadFail)

run :: State -> App a -> IO a
run state = flip runReaderT state . unApp

instance AuthRepo App where
  addAuth = M.addAuth
  setEmailAsVerified = M.setEmailAsVerified
  findUserByAuth = M.findUserByAuth
  findEmailFromUserId = M.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
  newSession = M.newSession
  findUserIdBySessionId = M.findUserIdBySessionId