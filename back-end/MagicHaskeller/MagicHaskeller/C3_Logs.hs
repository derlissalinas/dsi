module MagicHaskeller.Logs where
import Language.Haskell.TH as TH
import Data.Char
import Text.Printf
import MagicHaskeller.DataWrangling_Words
import MagicHaskeller.DataWrangling_Constants
import MagicHaskeller.DataWrangling_Dates


extractApacheDate :: [Char] -> [Char]
extractApacheDate x = getStartToFirstSymbolOccurrence (getLastSymbolOccurrenceToEnd x lBracket) rBracket


extractApacheDate2 :: [Char] -> [Char]
extractApacheDate2 x = getStartToFirstSymbolOccurrence (getLastSymbolOccurrenceToEnd x lBracket) space

extractApacheDate3 :: [Char] -> [Char]
extractApacheDate3 (x:xs)
        | x == head lBracket = getStartToFirstSymbolOccurrence xs space
        | otherwise = extractApacheDate3 xs
extractApacheDate3 _ = ""

extractApacheDate4 :: [Char] -> [Char]
extractApacheDate4 x = if elem '[' x then extractApacheDateArray4 (words x) else ""

extractApacheDateArray4 :: [String] -> [Char]
extractApacheDateArray4 (x:xs)
  | lBracket == headString x = getLastSymbolOccurrenceToEnd x lBracket
  | otherwise = extractApacheDateArray4 xs
extractApacheDateArray4 [] = ""





------------------------------
headString :: String -> String
headString [] = []
headString (x:xs) = [x]
------------------------------



headMaybe :: [a] -> Maybe a
headMaybe (first : rest) = Just first
headMaybe _ = Nothing





class Nothingish a where
    nada :: a

instance Nothingish (Maybe a) where
    nada = Nothing

instance Nothingish [a] where
    nada = []

instance Nothingish Char where
    nada = ' '


eliminate :: (Nothingish a) => Maybe a -> a
eliminate (Just a) = a
eliminate Nothing  = nada
eliminate _ = nada

------------------------
--Funciones para extraer el log level
------------------------


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


------------------------
--Funciones para extraer el nombre de la clase
------------------------

isDot :: Char -> Bool
isDot x 
  | x == '.' = True
  | otherwise = False


getClass :: [Char] -> [Char]
getClass [] = ""
getClass x = reverse( takeWhile (/='.') (reverse (last (takeWhile (/=":") (words x)))))

------------------------
--Funcion para extraer el Log Message
------------------------

--intento #3--

getLogMessage :: [Char] -> [Char]
getLogMessage x = getLastSymbolOccurrenceToEnd x doubleDot


--intento #2--
--getLogMessage x = unwords (tail (dropWhile (/=":") (words x)))

--intento #1--
--getLogMessage (x:xs)
-- | (x==':') && isSpace y = tail xs
-- | otherwise = getLogMessage xs
-- where y = head xs


-------------------------
--Funcion para extraer el tipo de Log (START o OTHER)
-------------------------

getType :: [Char] -> [Char]
getType x
  |  "START" `elem` ["START" | x <- (words x) , x `elem` ["Starting","Initializing","initialization","Started"]] = "START"
  |  otherwise = "OTHER_MESSAGE"
