module Main.Model(
	TodoList(..)
	, TodoItem(..)
	, loadList
	, saveList
	, addItem
	, removeItem
	) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.IO as S

newtype TodoList = TodoList { list :: [TodoItem] }

instance Show TodoList where
	show tdl = foldl (\ acc x -> acc ++ '\n':(show x)) "" (list tdl)

data TodoItem = TodoItem { 
	date :: T.Text
	, message :: T.Text
	}

instance FromJSON TodoItem where
	parseJSON (Object v) = TodoItem <$>
		v .: (T.pack "date") <*>
		v .: (T.pack "message")
	parseJSON _ = mzero

instance ToJSON TodoItem where
	toJSON (TodoItem date message) = object [(T.pack "date") .= date,
						(T.pack "message") .= message]

instance Show TodoItem where
	show (TodoItem d m) = "Date: " ++ T.unpack d 
		++ "\t\t\tMessage: " ++ T.unpack m

loadList :: T.Text -> TodoList -> MaybeT IO TodoList 
loadList fp _ = (lift . B.readFile . T.unpack) fp >>= dec
	where dec bs = case (decode bs :: Maybe [TodoItem]) of
		Just ls -> return $ TodoList ls
		Nothing -> do
			lift $ S.putStrLn "Invalid json file"
			return $ TodoList []

saveList :: T.Text -> TodoList -> MaybeT IO TodoList 
saveList fp tdl = do
		lift $ B.writeFile (T.unpack fp) ((encode . list) tdl)
		return tdl

addItem :: TodoItem -> TodoList -> MaybeT IO TodoList
addItem tdi tdl = return . TodoList $ tdi:(list tdl)

removeItem :: Maybe Int -> TodoList -> MaybeT IO TodoList
removeItem Nothing tdl = do
		lift $ S.putStrLn "Invalid index (integer was not entered)"
		return tdl
removeItem (Just n) tdl = if n <= ((length . list) tdl) - 1 
		then return . TodoList $ removeAt n $ list tdl
		else do
			lift $ S.putStrLn "Invalid index (too large)"
			return tdl


removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt n xs = (take n xs) ++ (drop (n + 1) xs)

