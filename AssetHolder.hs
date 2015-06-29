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

aAdd :: Amount -> [Account] -> Maybe [Account]
aAdd amt [] = Nothing
aAdd amt (acc:rest) = if acc `isOfType` aType
                        then Just $ newAcc:rest
                        else do
                            newRest <- aAdd amt rest
                            return $ acc:newRest
    where
        aType = assetType amt
        oldBalance = balance acc
        newBalance = oldBalance `addPoints` (points amt)
        newAcc = Account (accountNumber acc) newBalance

hAdd :: Amount -> Holder -> Maybe Holder
hAdd amt (Holder n d accs) = do
        newAccs <- aAdd amt accs
        return $ Holder n d newAccs

-- TODO: This silently ignores the situation when holder doesn't have
-- proper account. The return type should be Maybe Holder.
debit :: Amount -> Holder -> Holder
debit amt holder = case res of
                    Just mholder -> mholder
                    otherwise -> holder
    where res = hAdd amt holder

credit :: Amount -> Holder -> Holder
credit amt holder = debit (negateAmount amt) holder
