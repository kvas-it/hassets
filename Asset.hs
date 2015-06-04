-- 
-- Asset types and amounts
--

module Asset where

import Text.Printf (printf)

data Type = Type
        { code :: String
        , name :: String
        , descr :: String
        , symbol :: String
        , precision :: Integer
        } deriving Eq

instance Show Type where
        show (Type _ n _ _ _) = n

data Amount = Amount
        { assetType :: Type
        , amount :: Integer
        } deriving Eq

famount :: Amount -> Float
famount a = (fromInteger amt) / (fromInteger $ 10 ^ prec)
    where
        amt = amount a
        prec = precision $ assetType a

instance Show Amount where
        show aa = (symbol $ at) ++ (printf fmt $ famount aa)
            where
                at = assetType aa
                fmt = "%0." ++ (show $ precision at) ++ "f"

negate (Amount t a) = Amount t (-a)

-- Convenience type
type Portfolio = [Amount]

-- Helper function for AssetHolder.debit
pAdd :: Amount -> Portfolio -> Portfolio
pAdd amt [] = [amt]
pAdd amt (p:rest) =
        if tAmt == tP
            then (Amount tP $ aP + aAmt):rest
            else p:(pAdd amt rest)
    where
        tAmt = assetType amt
        aAmt = amount amt
        tP = assetType p
        aP = amount p
