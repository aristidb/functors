module Data.Functor.Syntax
(
  (.)
, unary
, (.:)
, binary
, (.::)
, trinary
, flip
, (&)
)
where
  
import Control.Applicative
import Prelude             hiding ((.), flip)

-- | Generalised version of '.' that is equivalent to 'fmap', but with the fixity of standard '.'.
-- 
-- @infixr 9 .@
(.) :: Functor f => (a -> b) -> f a -> f b
(.) = fmap
infixr 9 .

-- | Alias for 'fmap'.
unary :: Functor f => (a -> b) -> f a -> f b
unary = (.)

-- | Nested 'fmap' for 'Functor's inside Functors (two levels).
-- 
-- @infixr 9 .:@
(.:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.:) = fmap . fmap
infixr 9 .:

-- | Alias for '.:'.
binary :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
binary = (.:)

-- | Nested 'fmap' for 'Functor's inside Functors inside Functors (three levels).
-- 
-- @infixr 9 .::@
(.::) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(.::) = fmap . fmap . fmap
infixr 9 .::

-- | Alias for '.::'.
trinary :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
trinary = (.::)

-- | Generalised version of 'flip'.
flip :: Functor f => f (a -> b) -> a -> f b
flip a b = fmap ($ b) a

-- | Alternative syntax for '<*>' that fits better with the one-letter '.' from this module.
-- 
-- @infixl 4 &@
(&) :: Applicative f => f (a -> b) -> f a -> f b
(&) = (<*>)
infixl 4 &
