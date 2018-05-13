module Pixy.Data.Name where

import Data.Text (Text)

import Pixy.Data.Unique

data Name = Name
    { displayName :: Text
    , uniqueName :: Int
    }
    deriving (Show)

instance Eq Name where
    n1 == n2 = uniqueName n1 == uniqueName n2

instance Ord Name where
    compare n1 n2 = compare (uniqueName n1) (uniqueName n2)

instance Uniquable (Text -> Name) where
    mkUnique u name = Name { displayName = name, uniqueName = u }