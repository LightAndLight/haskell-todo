module Main.View(printList) where

import Main.Model
import Control.Monad.State.Lazy
import System.IO

printList :: ProgramState
printList = StateT $ \(ProgramData b l) -> mapM_ (putStrLn . show) l >> return ((),ProgramData b l)
