module Main.Controller(requestChoice,parseChoice) where

import Main.Model
import Prelude hiding (getLine,putStrLn)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Text (Text,split,toLower,append,pack)
import Data.Text.IO (getLine,putStrLn)
import Data.Text.Read (decimal)
import System.Exit

requestChoice :: IO Text
requestChoice = do
	putStrLn (pack "Enter command")
	getLine

empty :: Text
empty = pack ""

parseChoice :: Text -> TodoList -> MaybeT IO TodoList
parseChoice xs tdl
	| first == pack "add" = addItem (TodoItem second third) tdl
	| first == pack "remove" = removeItem (parseInt second) tdl
	| first == pack "save" = saveList second tdl
	| first == pack "load" = loadList second tdl
	| first == pack "quit" = lift $ exitSuccess
	| otherwise = invalidChoice first tdl
	where	args =  split (=='|') (toLower xs)
		first = head args
		second = if length args > 1 then args !! 1 else empty
		third = if length args > 2 then args !! 2 else empty

invalidChoice :: Text -> TodoList -> MaybeT IO TodoList
invalidChoice cmd tdl = do
	lift $ putStrLn $ append cmd $ pack " is an invalid command."
	return tdl

parseInt :: Text -> Maybe Int
parseInt tx = case decimal tx of
	Left _ -> Nothing
	Right (n,xs) -> if xs == empty then Just n else Nothing
