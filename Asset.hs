-- 
-- Asset types and amounts
--

module Asset (
    Type(Type)
  , Amount(Amount)
  , makeAmount
  , assetType
  , points
  , addPoints
  , amount
  , negateAmount
  ) where

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
    , points :: Integer
    } deriving Eq

makeAmount :: Type -> Float -> Amount
makeAmount t a = Amount t p
    where
        p = floor $ a * 10 ^ (precision t)

addPoints :: Amount -> Integer -> Amount
addPoints (Amount t p) inc = Amount t $ p + inc

amount :: Amount -> Float
amount a = (fromInteger pts) / (fromInteger $ 10 ^ prec)
    where
        pts = points a
        prec = precision $ assetType a

instance Show Amount where
    show aa = (symbol $ at) ++ (printf fmt $ amount aa)
        where
            at = assetType aa
            fmt = "%0." ++ (show $ precision at) ++ "f"

negateAmount :: Amount -> Amount
negateAmount (Amount t p) = Amount t (-p)
