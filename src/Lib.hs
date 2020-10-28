module Lib where

import ClassyPrelude
import Data.Aeson
import Data.Aeson.TH
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