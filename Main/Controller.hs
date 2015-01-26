{-# LANGUAGE OverloadedStrings #-}

module Main.Controller(requestChoice,parseChoice) where

import Main.Model
import Prelude hiding (getLine,putStrLn)
import Control.Monad.State.Lazy
import Data.Text (Text,split,toLower,append)
import Data.Text.IO (getLine,putStrLn)
import Data.Text.Read (decimal)
import System.Exit

requestChoice :: StateT TodoList IO Text
requestChoice = StateT $ \xs -> do 
	putStrLn ("Enter command") 
	line <- getLine
	return (line,xs)

parseChoice :: Text -> StateT TodoList IO ()
parseChoice choice
	| first == "add" = addItem $ TodoItem second third
	| first == "remove" = removeItem $ parseInt second
	| first == "save" = saveList second
	| first == "load" = loadList second
	| first == "quit" = StateT $ \_ -> exitSuccess
	| otherwise = invalidChoice first
	where	args =  split (=='|') (toLower choice)
		first = head args
		second = if length args > 1 then args !! 1 else ""
		third = if length args > 2 then args !! 2 else ""

invalidChoice :: Text -> StateT TodoList IO ()
invalidChoice cmd = StateT invalid 
	where invalid = \xs -> do
		putStrLn $ append cmd " is an invalid command."
		return ((),xs)

parseInt :: Text -> Maybe Int
parseInt tx = case decimal tx of
	Left _ -> Nothing
	Right (n,xs) -> if xs == "" then Just n else Nothing
