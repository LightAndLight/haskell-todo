module ProgramState(ProgramState) where

import Control.Applicative
import Contro.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

data ProgramState s l = ProgramState { 
	saved :: Bool
	, list :: MaybeT IO TodoList
	}

instance Monad m => Functor (ProgramState m) where
	fmap f p = ProgramState (saved p) $ fmap f (list p)

instance Monad m => Applicative (ProgramState m) where
	pure a = ProgramState False . pure $ a
	(<*>) f a = ProgramState (saved a) $ (list f) <*> (list a)

instance Monad m => Monad (ProgramState m) where
	return = pure
	(>>=) p f = ProgramState (saved p) . list . f $ list p
