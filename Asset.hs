-- 
-- Asset types and amounts
--

module Asset where

import Text.Printf (printf)

data AssetType = AssetType
        { code :: String
        , name :: String
        , descr :: String
        , symbol :: String
        , precision :: Integer
        } deriving Eq

instance Show AssetType where
        show (AssetType _ n _ _ _) = n

data AssetAmount = AssetAmount
        { assetType :: AssetType
        , amount :: Integer
        } deriving Eq

famount :: AssetAmount -> Float
famount a = (fromInteger amt) / (fromInteger $ 10 ^ prec)
    where
        amt = amount a
        prec = precision $ assetType a

instance Show AssetAmount where
        show aa = (symbol $ at) ++ (printf fmt $ famount aa)
            where
                at = assetType aa
                fmt = "%0." ++ (show $ precision at) ++ "f"

negate (AssetAmount t a) = AssetAmount t (-a)

-- Convenience type
type Portfolio = [AssetAmount]

-- Helper function for AssetHolder.debit
pAdd :: AssetAmount -> Portfolio -> Portfolio
pAdd amt [] = [amt]
pAdd amt (p:rest) =
        if tAmt == tP
            then (AssetAmount tP $ aP + aAmt):rest
            else p:(pAdd amt rest)
    where
        tAmt = assetType amt
        aAmt = amount amt
        tP = assetType p
        aP = amount p
