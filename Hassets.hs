--
-- Haskell port of ost.assets (financial system core).
--

import Data.Map (Map)

data AssetType = AssetType
        { code :: String
        , name :: String
        , description :: String
        , precision :: Integer
        } deriving Show

data AssetHolder = AssetHolder
        { name :: String
        , description :: String
        , accounts :: Map AssetType Integer
        } deriving Show

