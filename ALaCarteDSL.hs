{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE GADTs                 #-}

module ALaCarteDSL where

import ALaCarte

import           Control.Monad
import           Control.Monad.Reader (MonadReader, ReaderT)
import qualified Control.Monad.Reader as R
import           Control.Monad.State
import           Data.Maybe
import           Data.Functor.Identity

import           Prelude hiding (lookup)
import qualified Prelude as P

-- |a  language for modelling a calculator memory
-- We can increment the memory, recall it, or clear it.
data Calculator' a where
  Incr'   :: Int -> Calculator' ()
  Recall' :: Calculator' Int
  Clear'  :: Calculator' ()

-- the same, but CPS transformed

data Calculator'' a -- implement

instance Functor Calculator'' where
  fmap = undefined

-- broken up into parts

data Incr a   = Incr Int
data Recall a = Recall (Int -> a)
data Clear a  = Clear

-- and put back together again

type Calculator = Incr :+: Recall :+: Clear

instance Functor Incr where
  fmap f (Incr i) = Incr i

instance Functor Recall where
  fmap f (Recall k) = Recall (f . k)

instance Functor Clear where
  fmap f Clear = Clear

-- smart constructors

incr :: (Incr :<: f) => Int -> Free f ()
incr i = inject $ Incr i

recall :: (Recall :<: f) => Free f Int
recall = inject $ Recall Pure

clear :: (Clear :<: f) => Free f ()
clear = inject Clear

-- a simple language for user interaction

data Ask a = Ask String (String -> a) -- deriving Functor
data Tell a = Tell String a

instance Functor Ask where
  fmap f (Ask s k) = Ask s (f . k)

instance Functor Tell where
  fmap f (Tell s a) = Tell s (f a)

type Console = Ask :+: Tell

ask :: (Ask :<: f) => String -> Free f String
ask s = inject $ Ask s Pure

tell :: (Tell :<: f) => String -> Free f ()
tell s = inject $ Tell s (Pure ())

-- adder: like the calculator, but increment can overflow

data Adder k = Increment Int (Bool -> k)
             | Reset k
             | Total (Int -> k)

instance Functor Adder where
  fmap = undefined

increment :: (Adder :<: f) => Int -> Free f Bool
increment = undefined

reset :: (Adder :<: f) => Free f ()
reset = undefined

total :: (Adder :<: f) => Free f Int
total = undefined

-- key value lookup
data Lookup k v a = Lookup k (Maybe v -> a)

instance Functor (Lookup k v) where
  fmap = undefined

lookup :: ((Lookup k v) :<: f) => k -> Free f (Maybe v)
lookup = undefined

-- test programs

sayHelloProg :: (Ask :<: f, Tell :<: f) => Free f ()
sayHelloProg = do
  name <- ask "What's your name?"
  tell $ "Hello " ++ name

lookupProg :: Free (Ask :+: Lookup String Int :+: Tell) ()
lookupProg = do
  name  <- ask "What's your name?"
  quota <- fromMaybe (0 :: Int) <$> lookup name
  tell $ "Hi " ++ name ++ ", your quota is " ++ show quota

tick :: (Recall :<: f, Incr :<: f) => Free f Int
tick = do
  y <- recall
  incr 1
  return y

-- findLimit should return the limit, i.e. the number of times you
-- can increment an adder initialized at 0 before getting an overflow
--
-- hint: use execStateT :: Monad m => StateT s m a -> s -> m s
findLimit :: (Adder :<: f) => Free f Int
findLimit = undefined

-- helper function
findLimit' :: (Adder :<: f) => StateT Int (Free f) ()
findLimit' = undefined

tellLimit :: (Adder :<: f, Tell :<: f) => Free f ()
tellLimit = undefined
