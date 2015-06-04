--
-- Asset holders and accounts
--

module AssetHolder where

import Asset (Amount, Portfolio, pAdd, negate)

data Holder = Holder
        { name :: String
        , descr :: String
        , portfolio :: Portfolio
        } deriving (Show, Eq)

debit amt (Holder n d p) = Holder n d $ pAdd amt p
credit amt ah = debit (Asset.negate amt) ah
