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

accAdd :: Amount -> Account -> Maybe Account
accAdd amt@(Amount at p) acc@(Account num b)
        | acc `isOfType` at = Just $ Account num (b `addPoints` p)
        | otherwise = Nothing

accsAdd :: Amount -> [Account] -> Maybe [Account]
accsAdd amt [] = Nothing
accsAdd amt (acc:rest) =
        case (accAdd amt acc) of
            Just newAcc -> Just $ newAcc:rest
            Nothing -> do
                newRest <- accsAdd amt rest
                return $ acc:newRest

holderAdd :: Amount -> Holder -> Maybe Holder
holderAdd amt (Holder n d accs) =
    do
        newAccs <- accsAdd amt accs
        return $ Holder n d newAccs

-- TODO: This silently ignores the situation when holder doesn't have
-- proper account. The return type should be Maybe Holder.
debit :: Amount -> Holder -> Holder
debit amt holder =
    case holderAdd amt holder of
        Just mholder -> mholder
        otherwise -> holder

credit :: Amount -> Holder -> Holder
credit amt holder = debit (negateAmount amt) holder
