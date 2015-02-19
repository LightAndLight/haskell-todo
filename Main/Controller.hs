{-# LANGUAGE OverloadedStrings #-}

module Main.Controller(requestChoice,parseChoice) where

import Main.Model
import Prelude hiding (getLine,putStrLn,words,unwords,takeWhile,drop,length)
import Control.Monad.State.Lazy
import Data.Text (Text,split,toLower,append,words,unwords,takeWhile,drop,length)
import Data.Text.IO (getLine,putStrLn)
import Data.Text.Read (decimal)
import System.Exit (exitSuccess)

requestChoice :: ProgramState Text
requestChoice = StateT $ \ps -> do 
	putStrLn ("Enter command") 
	line <- getLine
	return (line,ps)

parseChoice :: Text -> ProgramState ()
parseChoice choice =
    let (command,arguments) = splitAtFirst ' ' choice
        (arg1,rest) = splitAtFirst ' ' arguments
    in
    case command of
        "add"       -> addItem $ TodoItem arg1 rest
        "remove"    -> removeItem . parseInt $ arg1
        "save"      -> saveList arg1
        "load"      -> loadList arg1
        "quit"      -> quit 
        _ -> invalidChoice command

invalidChoice :: Text -> ProgramState ()
invalidChoice cmd = StateT $ \ps -> do
		putStrLn $ append cmd " is an invalid command."
		return ((),ps)

quit :: ProgramState ()
quit = StateT $ \(ProgramData s l fp) -> if s
    then exitSuccess
    else do
        putStrLn "Current list is unsaved. Quit without saving? (y/n)"
        choice <- getLine
        case choice of
            "y" -> exitSuccess
            "n" -> return ((),ProgramData s l fp)
            _   -> runStateT quit $ ProgramData s l fp

parseInt :: Text -> Maybe Int
parseInt tx = case decimal tx of
	Left _ -> Nothing
	Right (n,xs) -> if xs == "" then Just n else Nothing

splitAtFirst :: Char -> Text -> (Text,Text)
splitAtFirst c xs = (first,rest)
    where first = takeWhile (/=c) xs
          rest = drop (length first + 1) xs
