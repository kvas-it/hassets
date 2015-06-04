--
-- Haskell port of ost.assets (financial system core).
--

import Asset
import AssetHolder

data Transaction = Transaction
        { senderName :: String
        , receiverName :: String
        , amount :: Asset.Amount
        } deriving (Show, Eq)

data Domain = Domain
        { assetTypes :: [Asset.Type]
        , assetHolders :: [AssetHolder.Holder]
        , transactions :: [Transaction]
        } deriving Show

usd = Asset.Type "USD" "US Dollar" "US Dollar" "$" 2
eur = Asset.Type "EUR" "Euro" "Euro" "€" 2
_USD = Asset.Amount usd
_EUR = Asset.Amount eur

kvas = AssetHolder.Holder "kvas" "Vasily Kuznetsov" [(_USD 1000)]
vmik = AssetHolder.Holder "vmik" "Mikhail Vartanyan" [(_EUR 1000)]

dom1 = Domain
        { assetTypes = [usd, eur]
        , assetHolders = [kvas, vmik]
        , transactions = []
        }
