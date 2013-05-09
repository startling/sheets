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
  , transform
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

data Field m a b = Field
  { _classes :: [String]
  , _label :: Maybe b
  , _field :: a -> m b
  } deriving
  ( Functor
  )
makeLenses ''Field

data Table m a b = Table
  { _title  :: Maybe b
  , _fields :: [Field m a b]
  , _items  :: [a]
  } deriving
  ( Functor
  )
makeLenses ''Table 

-- -- | Traverse the rows in a table.
rows :: Monad m => ([a] -> m b) -> Table m x a -> m [b]
rows fn (Table _ cs is) = forM is $ \i ->
  forM cs (($ i) . view field) >>= fn

-- | Monadically traverse the columns in a table.
columns :: Monad t => ([t1] -> t b) -> Table t a t1 -> t [b]
columns fn (Table _ cs is) = forM cs $ \c ->
  forM is (view field c) >>= fn

-- | Transform the monad held by a table.
transform :: (m b -> n b) -> Table m a b -> Table n a b
transform fn = over fields (map $ over field (fn .))

-- | Split a table, given the number of rows each should have.
split :: Int -> Table m a b -> [Table m a b]
split n (Table t fs is) = Table t fs `map` taking n is where
  taking :: Int -> [a] -> [[a]]
  taking _ [] = []
  taking n x = let (a, b) = splitAt n x in a : taking n b

-- | Split a table into a table with twice as many fields and
-- half as many items.
split' :: Table m a b -> Table m (a, a) b
split' (Table t fs is) = Table t
  (map (over field (. fst)) fs ++ map (over field (. snd)) fs)
  . uncurry zip . splitAt (length is `div` 2) $ is
  
-- | A column taking a lens into a number in the state and
-- counting up from that number.
counter :: (Show n, Num n, MonadState s m) =>
  Simple Lens s n -> Field m x Text
counter l = Field ["count"] Nothing . const $ (l += 1)
  >> ((pack . show) `liftM` use l)

-- | The identity field.
see :: Monad m => Field m b b
see = Field [] Nothing return

-- | A field showing an empty value.
blank :: (Monad m, Monoid b) => Field m a b
blank = Field [] Nothing . const . return $ mempty

-- | Add a class to a 'Field'.
(*.) :: Field m a b -> String -> Field m a b
(*.) f s = classes <>~ [s] $ f

-- | Label a field.
(!.) :: Field m a b -> b -> Field m a b
(!.) f n = label .~ Just n $ f

-- | Set the monadic function of a field.
($.) :: Field m a b -> (a1 -> m1 b) -> Field m1 a1 b
($.) f g = field .~ g $ f

-- | Set the monadic function of a field, from an ordinary function.
(%.) :: Monad m => Field m a b -> (z -> b) -> Field m z b
(%.) f g = field .~ return . g $ f

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
horizontal :: Int -> Table m a b -> Layout (Table m a b)
horizontal n = Adjacent . map Left . split n

-- | Split a table vertically into tables with the given
-- number of columns.
vertical :: Int -> Table m a b -> Layout (Table m a b)
vertical n = Column . map Left . split n

