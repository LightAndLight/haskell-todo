{-# LANGUAGE OverloadedStrings #-}

module Main.Controller(runCommand) where

import Main.Model
import Control.Applicative ((<$>),(<*>))
import Control.Monad.State.Lazy
import Data.Aeson
import Data.ByteString.Lazy as B (ByteString, readFile, writeFile)
import Data.Text (pack)
import Data.Text.Read (decimal)
import System.Exit (exitSuccess)

instance FromJSON TodoItem where
	parseJSON (Object v) = TodoItem <$>
		v .: "date" <*>
		v .: "message"
	parseJSON _ = mzero

instance ToJSON TodoItem where
	toJSON (TodoItem date message) = object ["date" .= date,
						"message" .= message]

runCommand :: String -> ProgramData -> IO ProgramData
runCommand cmd pd =
    let (command,arguments) = splitAtFirst ' ' cmd
        (arg1,rest) = splitAtFirst ' ' arguments
        parg1 = pack arg1
        prest = pack rest
    in
    case command of
        "add"       -> return $ addItem (TodoItem parg1 prest) pd
        "remove"    -> return $ removeItem (parseInt arg1) pd
        "save"      -> saveList arg1 pd
        "load"      -> loadList arg1 pd
        "quit"      -> quit  pd
        _           -> return $ invalidChoice command pd

invalidChoice :: String -> ProgramData -> ProgramData
invalidChoice cmd pd = pd { errorMsg = Just (cmd ++ "is an invalid command") }

loadList :: String -> ProgramData -> IO ProgramData
loadList fp pd = do
    contents <- B.readFile fp
    return $ case (decode contents :: Maybe TodoList) of
        Just list   -> ProgramData True list fp Nothing
        Nothing     -> pd { errorMsg = Just "Parse error in JSON file" }

saveList :: String -> ProgramData -> IO ProgramData
saveList "" pd = do
    B.writeFile (filepath pd) . encode $ list pd
    return $ pd { 
        saved = True
        , errorMsg = Nothing
    }

saveList fp pd = do 
    B.writeFile fp . encode $ list pd
    return $ pd { 
        saved = True 
        , filepath = fp
        , errorMsg = Nothing
    }

quit :: ProgramData -> IO ProgramData
quit pd = if saved pd
    then exitSuccess
    else do
        putStrLn "Current list is unsaved. Quit without saving? (y/n)"
        choice <- getLine
        case choice of
            "y" -> exitSuccess
            "n" -> return pd
            _   -> quit pd

parseInt :: String -> Int
parseInt tx = case decimal ptx of
	Left _ -> -1
	Right (n,xs) -> if xs == "" then n else -1
    where ptx = pack tx

splitAtFirst :: Char -> String -> (String,String)
splitAtFirst c xs = (first,rest)
    where first = takeWhile (/=c) xs
          rest = drop (length first + 1) xs
