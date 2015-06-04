--
-- Haskell port of ost.assets (financial system core).
--

type ID = Integer

--                         code   name   descr  precision
data AssetType = AssetType String String String Integer
    deriving (Show, Eq)

--                             type      amount
data AssetAmount = AssetAmount AssetType Integer
    deriving (Show, Eq)

--                             name   descr  accounts
data AssetHolder = AssetHolder String String [AssetAmount]
    deriving (Show, Eq)

--                             name   descr  source      destination amount
data Transaction = Transaction String String AssetHolder AssetHolder AssetAmount
    deriving (Show, Eq)

--             name   descr  transactions
data Lot = Lot String String [Transaction]
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

instance Named Transaction where
    name (Transaction n _ _ _ _) = n
    descr (Transaction _ d _ _ _) = d

instance Named Lot where
    name (Lot n _ _) = n
    descr (Lot _ d _) = d


code (AssetType c _ _ _) = c
precision (AssetType _ _ _ p) = p


assetType (AssetAmount t _) = t
amount (AssetAmount _ a) = a
inverse (AssetAmount t a) = AssetAmount t (-a)

famount :: AssetAmount -> Float
famount a = (fromInteger amt) / (fromInteger $ 10 ^ prec)
    where
        amt = amount a
        prec = precision $ assetType a

-- helper function to add amount to set of amounts
debitA amt [] = [amt]
debitA amt (acc:accs) =
        if tAmt == tAcc
            then (AssetAmount tAcc $ aAcc + aAmt):accs
            else acc:(debitA amt accs)
    where
        tAmt = assetType amt
        aAmt = amount amt
        tAcc = assetType acc
        aAcc = amount acc

debit :: AssetAmount -> AssetHolder -> AssetHolder
debit amt (AssetHolder n d accs) = AssetHolder n d $ debitA amt accs
