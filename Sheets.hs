{-# Language Rank2Types #-}
{-# Language DeriveFunctor #-}
{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}
module Sheets
  ( Field(..)
  , classes
  , label
  , field
  , Table(..)
  , title
  , fields
  , items
  , rows
  , columns
  , split
  , split'
  , counter
  , see
  , blank
  , (*.)
  , (!.)
  , (%.)
  , Layout(..)
  , horizontal )
  where
-- base
import Control.Applicative
import Data.Monoid
-- mtl
import Control.Monad.State
-- Text
import Data.Text (Text)
import Data.Text (pack)
-- lens
import Control.Lens hiding (transform)

data Field a b = Field
  { _classes :: [String]
  , _label :: Maybe Text
  , _field :: a -> b
  } deriving
  ( Functor
  )
makeLenses ''Field

data Table a b = Table
  { _title  :: Maybe Text
  , _fields :: [Field a b]
  , _items  :: [a]
  } deriving
  ( Functor
  )
makeLenses ''Table 

-- | Traverse the rows in a table.
rows :: Applicative f => ([a] -> f b) -> Table x a -> f [b]
rows fn (Table _ cs is) = flip traverse is $ \i ->
  fn $ map (($ i) . view field) cs

-- | Traverse the columns in a table.
columns :: Applicative f => ([t] -> f b) -> Table a t -> f [b]
columns fn (Table _ cs is) = flip traverse cs $ \c ->
  fn $ map (view field c) is

-- | Split a table, given the number of rows each should have.
split :: Int -> Table m a -> [Table m a]
split n (Table t fs is) = Table t fs `map` taking n is where
  taking :: Int -> [a] -> [[a]]
  taking _ [] = []
  taking n x = let (a, b) = splitAt n x in a : taking n b

-- | Split a table into a table with twice as many fields and
-- half as many items.
split' :: Table a b -> Table (a, a) b
split' (Table t fs is) = Table t
  (map (over field (. fst)) fs ++ map (over field (. snd)) fs)
  . uncurry zip . splitAt (length is `div` 2) $ is
  
-- | A column taking a lens into a number in the state and
-- counting up from that number.
counter :: (Show n, Num n, MonadState s m) =>
  Simple Lens s n -> Field x (m Text)
counter l = Field ["count"] Nothing . const $ (l += 1)
  >> ((pack . show) `liftM` use l)

-- | The identity field.
see :: Field a a
see = Field [] Nothing id

-- | A field showing an empty value.
blank :: Monoid b => Field a b
blank = Field [] Nothing $ const mempty

-- | Add a class to a 'Field'.
(*.) :: Field m a -> String -> Field m a
(*.) f s = classes <>~ [s] $ f

-- | Label a field.
(!.) :: Field m a -> Text -> Field m a
(!.) f n = label .~ Just n $ f

-- | Set the function of a field.
(%.) :: Field a b -> (c -> d) -> Field c d
(%.) f g = field .~ g $ f

data Layout a
  = Column [Either a (Layout a)]
  | Adjacent [Either a (Layout a)]
  deriving
  ( Eq
  , Ord
  , Show
  )

-- | Split a table horizontally into tables with the given
-- number of columns.
horizontal :: Int -> Table m a -> Layout (Table m a)
horizontal n = Adjacent . map Left . split n

-- | Split a table vertically into tables with the given
-- number of columns.
vertical :: Int -> Table m a -> Layout (Table m a)
vertical n = Column . map Left . split n

