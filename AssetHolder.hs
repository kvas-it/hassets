--
-- Asset holders and accounts
--

module AssetHolder (
    Account(Account)
  , isOfType
  , makeAccount
  , Holder(Holder)
  , holderName
  , holderDescr
  , hasAccountFor
  , debit
  , credit
  ) where

import Asset

data Account = Account
        { accountNumber :: String
        , balance :: Amount
        } deriving (Eq, Show)

isOfType :: Account -> Type -> Bool
isOfType a t = (assetType $ balance a) == t

makeAccount :: String -> Type -> Account
makeAccount an at = Account an (makeAmount at 0.0)

data Holder = Holder
        { holderName :: String
        , holderDescr :: String
        , accounts :: [Account]
        } deriving (Show, Eq)

hasAccountFor :: Holder -> Type -> Bool
hasAccountFor h t = any (\a -> a `isOfType` t) (accounts h) 

-- Add amount to holder's account if he has one for the currency (otherwise
-- return the holder unchanged).
hAdd' :: Amount -> Holder -> Holder
hAdd' amt (Holder n d accs) = Holder n d (addAmountTo accs)
    where
        addAmountTo [] = []
        addAmountTo (acc:rest) = if acc `isOfType` aType
                              then (Account number newBalance):rest
                              else acc:(addAmountTo rest)
            where
                aType = assetType amt
                number = accountNumber acc
                oldBalance = balance acc
                newBalance = oldBalance `addPoints` (points amt)

-- Add amount to holder's account if he has one for the currency (otherwise
-- return Nothing).
hAdd :: Amount -> Holder -> Maybe Holder
hAdd amt h = if h `hasAccountFor` (assetType amt)
                 then Just $ hAdd' amt h
                 else Nothing

-- TODO: This silently ignores the situation when holder doesn't have
-- proper account. The return type should be Maybe Holder.
debit :: Amount -> Holder -> Holder
debit amt holder = case res of
                    Just mholder -> mholder
                    otherwise -> holder
    where res = hAdd amt holder

credit :: Amount -> Holder -> Holder
credit amt holder = debit (negateAmount amt) holder
