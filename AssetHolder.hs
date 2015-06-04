--
-- Asset holders and accounts
--

module AssetHolder where

import Asset (AssetAmount, Portfolio, pAdd, negate)

data AssetHolder = AssetHolder
        { name :: String
        , descr :: String
        , portfolio :: Portfolio
        } deriving (Show, Eq)

debit amt (AssetHolder n d p) = AssetHolder n d $ pAdd amt p
credit amt ah = debit (Asset.negate amt) ah
