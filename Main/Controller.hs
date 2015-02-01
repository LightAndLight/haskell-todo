{-# LANGUAGE OverloadedStrings #-}

module Main.Controller(requestChoice,parseChoice) where

import Main.Model
import Prelude hiding (getLine,putStrLn)
import Control.Monad.State.Lazy
import Data.Text (Text,split,toLower,append)
import Data.Text.IO (getLine,putStrLn)
import Data.Text.Read (decimal)
import System.Exit

requestChoice :: StateT (ProgramData Bool TodoList) IO Text
requestChoice = StateT $ \xs -> do 
	putStrLn ("Enter command") 
	line <- getLine
	return (line,xs)

parseChoice :: Text -> ProgramState
parseChoice choice =
    case split (== '|') (toLower choice) of
        ["add", date, message] -> addItem $ TodoItem date message
        ["remove", index]      -> removeItem $ parseInt index
        ["save", file]         -> saveList file
        ["load", file]         -> loadList file
        ["quit"]               -> lift exitSuccess
        (invalid:_)            -> invalidChoice invalid

invalidChoice :: Text -> ProgramState
invalidChoice cmd = StateT invalid 
	where invalid = \ps -> do
		putStrLn $ append cmd " is an invalid command."
		return ((),ps)

parseInt :: Text -> Maybe Int
parseInt tx = case decimal tx of
	Left _ -> Nothing
	Right (n,xs) -> if xs == "" then Just n else Nothing
