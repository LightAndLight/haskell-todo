module Main.View(printList) where

import Main.Model
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import System.IO

printList :: TodoList -> MaybeT IO TodoList
printList tdl = do
	lift . putStrLn . show $ tdl
	return tdl 
