--
-- Haskell port of ost.assets (financial system core).
--

import Data.IORef

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
        hName = holderName h
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


usd = Type "USD" "US Dollar" "US Dollar" "$" 2
_USD = makeAmount usd

eur = Type "EUR" "Euro" "Euro" "€" 2
_EUR = makeAmount eur

kvas = Holder "kvas" "Vasily Kuznetsov" [
    makeAccount "kvas-USD" usd,
    makeAccount "kvas-EUR" eur]
vmik = Holder "vmik" "Mikhail Vartanyan" [
    makeAccount "vmik-EUR" eur,
    makeAccount "vmik-USD" usd]

dom = Domain
    { assetTypes = [usd, eur]
    , assetHolders = [kvas, vmik]
    , transactions = []
    }

main = do
    ref <- newIORef dom
    putStrLn "Initial state:"
    readIORef ref >>= print
    modifyIORef ref $ addTransaction $ Transaction "kvas" "vmik" (_USD 100)
    putStrLn "\nAfter first transaction:"
    readIORef ref >>= print
    modifyIORef ref $ addTransaction $ Transaction "vmik" "kvas" (_EUR 100)
    putStrLn "\nAfter second transaction:"
    readIORef ref >>= print
