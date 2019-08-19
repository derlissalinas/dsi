module MagicHaskeller.C1_Logs where
import Language.Haskell.TH as TH
import Data.Char
import Text.Printf
import MagicHaskeller.DataWrangling_Words
import MagicHaskeller.DataWrangling_Constants
import MagicHaskeller.DataWrangling_Dates


-------------
--Constantes |
-------------
isDot :: Char -> Bool
isDot x 
  | x == '.' = True
  | otherwise = False


--------------------------------------
--Extrae la fecha de un log de Spring |
--------------------------------------
extractDate :: [Char] -> [Char]
extractDate x = unwords (take 2 (splitStringByPunctuation x " "))


--------------------------------------
--Funciones para extraer el Log level |
--------------------------------------
--Lista de Levels predefinidas
logLevelList :: [[Char]]
logLevelList = [ "DEBUG", 
            "INFO",
            "ERROR",
            "WARN",
            "TRACE"]


getLogLevelString :: [Char] -> [Char]
getLogLevelString x = getLogLevelArray (words x)


getLogLevelArray :: [[Char]] -> [Char]
getLogLevelArray (x:xs)
  | elem x logLevelList = x
  | otherwise = getLogLevelArray xs
getLogLevelArray _ = []


-----------------------------------------------
--Funciones para extraer el nombre de la clase |
-----------------------------------------------
getClass :: [Char] -> [Char]
getClass [] = ""
getClass x = reverse( takeWhile (/='.') (reverse (last (takeWhile (/=":") (words x)))))


------------------------
--Funcion para extraer el Log Message
------------------------
getLogMessage :: [Char] -> [Char]
getLogMessage x = getLastSymbolOccurrenceToEnd x doubleDot


-------------------------------------------------------
--Funcion para extraer el tipo de Log (START || OTHER) |
-------------------------------------------------------
getType :: [Char] -> [Char]
getType x
  |  "START" `elem` ["START" | x <- (words x) , x `elem` ["Starting","Initializing","initialization","Started"]] = "START"
  |  otherwise = "OTHER_MESSAGE"