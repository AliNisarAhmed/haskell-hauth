module Lib where

import qualified Adapter.InMemory.Auth as M
import qualified Adapter.PostgreSQL.Auth as PG
import ClassyPrelude
import Control.Monad (MonadFail)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson
import Data.Aeson.TH
import Domain.Auth
import Domain.Validation
import Katip
import Language.Haskell.TH.Syntax (nameBase)

someFunc :: IO ()
someFunc = withKatip $ \le -> do
  mState <- newTVarIO M.initialState
  PG.withState pgCfg $ \pgState -> run le (pgState, mState) action
  where
    pgCfg =
      PG.Config
        { PG.configUrl = "postgresql://localhost/hauth",
          PG.configStripeCount = 2,
          PG.configMaxOpenConnPerStripe = 5,
          PG.configIdleConnTimeout = 10
        }

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

---- Application ----

type State = (PG.State, TVar M.State)

newtype App a = App
  {unApp :: ReaderT State (KatipContextT IO) a}
  deriving (Applicative, Functor, Monad, MonadReader State, MonadIO, MonadFail, KatipContext, Katip, MonadThrow)

run :: LogEnv -> State -> App a -> IO a
run le state = runKatipContextT le () mempty . flip runReaderT state . unApp

instance AuthRepo App where
  addAuth = PG.addAuth
  setEmailAsVerified = PG.setEmailAsVerified
  findUserByAuth = PG.findUserByAuth
  findEmailFromUserId = PG.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
  newSession = M.newSession
  findUserIdBySessionId = M.findUserIdBySessionId

---- Logging ----

runKatip :: IO ()
runKatip = withKatip $ \le ->
  runKatipContextT le () mempty logSomething

withKatip :: (LogEnv -> IO a) -> IO a
withKatip app =
  bracket createLogEnv closeScribes app
  where
    createLogEnv = do
      logEnv <- initLogEnv "HAuth" "prod"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

logSomething :: (KatipContext m) => m ()
logSomething = do
  $(logTM) InfoS "Log in no namespace"
  katipAddNamespace "ns1" $
    $(logTM) InfoS "Log in ns1"
  katipAddNamespace "ns2" $
    $(logTM) WarningS "Log in ns2"
  katipAddNamespace "ns3" $
    katipAddContext (sl "userId" $ asText "12") $ do
      $(logTM) InfoS "Log in ns2.ns3 with userId context"
      katipAddContext (sl "Country" $ asText "Canada") $
        $(logTM) InfoS "Log in ns2.ns3 with UserId and country context"