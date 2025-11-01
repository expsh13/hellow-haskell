data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq, Ord, Enum)

instance Show SixSidedDie where
  show S1 = "one"
  show S2 = "two"
  show S3 = "three"
  show S4 = "four"
  show S5 = "five"
  show S6 = "six"

-- instance Enum SixSidedDie where
--   toEnum 0 = S1
--   toEnum 1 = S2
--   toEnum 2 = S3
--   toEnum 3 = S4
--   toEnum 4 = S5
--   toEnum 5 = S6
--   toEnum _ = error "toEnum: out of bounds" 

--   fromEnum S1 = 0
--   fromEnum S2 = 1
--   fromEnum S3 = 2
--   fromEnum S4 = 3
--   fromEnum S5 = 4
--   fromEnum S6 = 5


-- instance Eq SixSidedDie where
--   (==) S1 S1 = True
--   (==) S2 S2 = True
--   (==) S3 S3 = True
--   (==) S4 S4 = True
--   (==) S5 S5 = True
--   (==) S6 S6 = True
--   (==) _  _  = False

-- instance Ord SixSidedDie where
--   compare S1 S1 = EQ
--   compare S1 _  = LT
--   compare _  S1 = GT
--   compare S2 S2 = EQ
--   compare S2 _  = LT
--   compare _  S2 = GT
--   compare S3 S3 = EQ
--   compare S3 _  = LT
--   compare _  S3 = GT
--   compare S4 S4 = EQ
--   compare S4 _  = LT
--   compare _  S4 = GT
--   compare S5 S5 = EQ
--   compare S5 _  = LT
--   compare _  S5 = GT
--   compare S6 S6 = EQ
--   compare S6 _  = LT
--   compare _  S6 = GT