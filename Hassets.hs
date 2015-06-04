--
-- Haskell port of ost.assets (financial system core).
--

import Asset
import AssetHolder

data Transaction = Transaction
        { senderName :: String
        , receiverName :: String
        , assetAmount :: Asset.Amount
        } deriving (Show, Eq)

data Domain = Domain
        { assetTypes :: [Asset.Type]
        , assetHolders :: [AssetHolder.Holder]
        , transactions :: [Transaction]
        } deriving Show


applyToHolders t [] = []
applyToHolders t (h:hs) =
        h':(applyToHolders t hs)
    where
        hName = AssetHolder.name h
        h' = if hName == (senderName t)
                then (credit (assetAmount t) h)
                else if hName == (receiverName t)
                    then (debit (assetAmount t) h)
                    else h

addTransaction :: Transaction -> Domain -> Domain
addTransaction t (Domain types holders transactions) =
        Domain types mHolders (transactions ++ [t])
    where
        mHolders = applyToHolders t holders


usd = Asset.Type "USD" "US Dollar" "US Dollar" "$" 2
eur = Asset.Type "EUR" "Euro" "Euro" "€" 2
_USD = Asset.Amount usd
_EUR = Asset.Amount eur

kvas = AssetHolder.Holder "kvas" "Vasily Kuznetsov" [(_USD 1000)]
vmik = AssetHolder.Holder "vmik" "Mikhail Vartanyan" [(_EUR 1000)]

dom = Domain
        { assetTypes = [usd, eur]
        , assetHolders = [kvas, vmik]
        , transactions = []
        }

t1 = Transaction "kvas" "vmik" (_USD 100)
t2 = Transaction "vmik" "kvas" (_EUR 100)

dom1 = addTransaction t1 dom
dom2 = addTransaction t2 dom1

main = putStrLn $ show dom2
