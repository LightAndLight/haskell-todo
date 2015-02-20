{-# LANGUAGE OverloadedStrings #-}

module Main.Controller(
    getCommand
    , runCommand
) where

import Main.Model
import Util.Date

import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.Text.Read (decimal)
import System.Exit (exitSuccess)

instance FromJSON TodoItem where
	parseJSON (Object v) = TodoItem <$>
		v .: "date" <*>
		v .: "message"
	parseJSON _ = mzero

instance ToJSON TodoItem where
	toJSON (TodoItem date message) = object [ "date" .= date, "message" .= message ]

instance FromJSON Date where
	parseJSON (Object v) = Date <$> v .: "year" <*> v .: "month" <*> v .: "day"
	parseJSON _ = mzero

instance ToJSON Date where
	toJSON (Date year month day) = object [ "year" .= year, "month" .= month, "day" .= day ]

data Command = 
    Add TodoItem
    | Remove Int
    | Save
    | SaveTo FilePath
    | Load
    | LoadFrom FilePath
    | Quit 

splitWord :: Text -> (Text,Text)
splitWord w = (first,T.stripStart rest)
    where (first,rest) = T.break isSpace w

parseCommand :: Text -> Either String Command
parseCommand s = 
    let (command,arguments) = splitWord s
        (arg1,rest) = splitWord arguments
    in
    case command of
        "add"    -> case parseDate (T.unpack arg1) of
                        Right date -> Right . Add $ TodoItem date rest
                        Left e     -> Left e
        "remove" -> case decimal arg1 of
                        Right (n,"") -> Right $ Remove n
                        Left e       -> Left e
                        _            -> Left "Invalid index"
        "save"   -> case arg1 of
                        "" -> Right Save
                        _  -> Right . SaveTo . T.unpack $ arg1
        "load"   -> Right . LoadFrom . T.unpack $ arg1 
        "quit"   -> Right Quit
        _        -> Left $ T.unpack command ++ " is an invalid command."

getCommand :: IO Command
getCommand = do
    putStrLn "Enter command:"
    input <- T.getLine
    case parseCommand input of
        Right command -> return command
        Left error    -> do putStrLn error
                            getCommand

runCommand :: Command -> ProgramState -> IO ProgramState
runCommand cmd ps =
    case cmd of
        Add item         -> return . modifyList (item:) $ ps { saved = False }

        Remove n         -> return . modifyList (removeAt n) $ ps { saved = False }

        Save             -> do
            saveTo (filepath ps) (list ps)
            return $ ps { saved = True }

        SaveTo fp        -> do
            saveTo fp (list ps)
            return $ ps { saved = True, filepath = fp }

        LoadFrom fp      -> do
            result <- loadFrom fp
            case result of
                Right l -> return $ ps { saved = True, list = l, filepath = fp }
                Left e  -> do
                    putStrLn e
                    return ps

        Quit | saved ps  -> exitSuccess
             | otherwise -> do
                putStrLn "Are you sure you want to quit without saving? (y/n)"
                (first:_) <- getLine
                case toLower first of
                    'y' -> exitSuccess
                    'n' -> return ps
                    _   -> do
                        putStrLn "Enter y or n"
                        runCommand Quit ps

loadFrom :: FilePath -> IO (Either String TodoList)
loadFrom fp = do
    contents <- B.readFile fp
    return $ case (decode contents :: Maybe TodoList) of
        Just l   -> Right l
        Nothing  -> Left "Syntax error in JSON file"
 
saveTo :: FilePath -> TodoList -> IO ()
saveTo fp l = B.writeFile fp . encode $ l
