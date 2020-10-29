module Lib where

import ClassyPrelude
import Data.Aeson
import Data.Aeson.TH
import Domain.Validation
import Language.Haskell.TH.Syntax (nameBase)

someFunc :: IO ()
someFunc = putStrLn "Main"

data User = User
  { userId :: Int,
    userName :: Text,
    userHobbies :: [Text]
  }
  deriving (Show)

-- instance ToJSON User where
--   toJSON (User uId name hobbies) =
--     object
--       [ "id" .= uId,
--         "name" .= name,
--         "hobbies" .= hobbies
--       ]

-- instance FromJSON User where
--   parseJSON = withObject "User" $ \v ->
--     User <$> v .: "id"
--       <*> v .: "name"
--       <*> v .: "hobbies"

$( let structName = nameBase ''User
       lowercaseFirst (x : xs) = toLower [x] <> xs
       lowercaseFirst xs = xs
       options = defaultOptions {fieldLabelModifier = lowercaseFirst . drop (length structName)}
    in deriveJSON options ''User
 )

data Test
  = TestNullary
  | TestUnary Int
  | TestProduct Int Text Double
  | TestRecord {recA :: Bool, recB :: Int}

$(deriveJSON defaultOptions ''Test)

isBelow10 :: Int -> Either Text ()
isBelow10 n = if n < 10 then Right () else Left $ error "above 10"

result = isBelow10 20

-- run :: IO ()
-- run =
--   case result of
--     Left e -> do
--       putStrLn "something went wrong"
--       putStrLn e
--     Right _ ->
--       putStrLn "All good"

data ServerException
  = ServerOnFireException
  | ServerNotPluggedIn
  deriving (Show)

instance Exception ServerException

data MyException
  = ThisException
  | ThatException
  deriving (Show)

instance Exception MyException

run :: IO () -> IO ()
run action =
  action
    `catch` (\e -> putStrLn $ "ServerException: " <> tshow (e :: ServerException))
    `catch` (\e -> putStrLn $ "MyException: " <> tshow (e :: MyException))
    `catchAny` (\e -> putStrLn $ tshow e)