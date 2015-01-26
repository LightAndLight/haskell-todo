module Main where

import Main.Model
import Main.View
import Main.Controller

import Control.Monad.State

program :: StateT TodoList IO ()
program = do
	StateT $ \xs -> do
		(choice,_) <- runStateT requestChoice xs
		(_,xs') <- runStateT (parseChoice choice) xs
		runStateT printList xs'
		runStateT program xs'

main = runStateT program []
