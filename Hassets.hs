--
-- Haskell port of ost.assets (financial system core).
--

--                         code   name   descr  precision
data AssetType = AssetType String String String Integer
    deriving (Show, Eq)

--                                   assetType balance
data HoldingAccount = HoldingAccount AssetType Integer
    deriving (Show, Eq)

--                             name   descr  accounts
data AssetHolder = AssetHolder String String [HoldingAccount]
    deriving (Show, Eq)


class Named a where
    name :: a -> String
    descr :: a -> String

instance Named AssetType where
    name (AssetType _ n _ _) = n
    descr (AssetType _ _ d _) = d

instance Named AssetHolder where
    name (AssetHolder n _ _) = n
    descr (AssetHolder _ d _) = d

code (AssetType c _ _ _) = c
precision (AssetType _ _ _ p) = p

assetType (HoldingAccount a _) = a
balance (HoldingAccount _ b) = b
fbalance (HoldingAccount t b) = (fromInteger b) / (fromInteger $ 10 ^ (precision t))
