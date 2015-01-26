module Main.View(printList) where

import Main.Model
import Control.Monad.State.Lazy
import System.IO

printList :: StateT TodoList IO ()
printList = StateT $ \xs -> mapM_ (putStrLn . show) xs >> return ((),xs)
