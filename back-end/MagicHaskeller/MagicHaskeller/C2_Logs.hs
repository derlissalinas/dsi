module MagicHaskeller.C2_Logs where
import Language.Haskell.TH as TH
import Data.Char
import Text.Printf
import MagicHaskeller.DataWrangling_Words
import MagicHaskeller.DataWrangling_Constants
import MagicHaskeller.DataWrangling_Dates


-----------------
--Comprobaciones | 
-----------------

isDate :: [[Char]] -> Bool
isDate [x,y,z]
   | "" `elem` [x,y,z] = False
   | isDMY [x,y,z] || isMDY [x,y,z] || isMYD [x,y,z] || isYMD [x,y,z] || isYDM [x,y,z] = True
   | otherwise = False
isDate _ = False

isHour :: [Char] -> Bool
isHour x
        | 0 <= val && val <= 24 = True
        | otherwise = False
        where val = read x :: Int

isMinute :: [Char] -> Bool
isMinute x
        | 0 <= val && val <= 59 = True
        | otherwise = False
        where val = read x :: Int

isSecond :: [Char] -> Bool
isSecond x
        | 0 <= val && val <= 59 = True
        | otherwise = False
        where val = read x :: Float

isLogTime :: [[Char]] -> Bool
isLogTime [h,m,s]
   | isHour h && isMinute m && isSecond s = True
   | otherwise = False
isLogTime _ = False


-------------------------------
--Función para obtener la Hora |
-------------------------------

getLogTime :: [Char]-> [Char]
getLogTime x = getLogTimeArray (words x)

getLogTimeArray :: [[Char]] -> [Char]
getLogTimeArray (x:xs)
   | isLogTime (splitStringByPunctuation x ":") = x
   | isLogTime $ tail (splitStringByPunctuation x ":") = changePunctuationString (unwords $ tail (splitStringByPunctuation x ":")) ":"
   | otherwise = getLogTimeArray xs
getLogTimeArray _ = []


----------------------------------------------
--Funciones específicas para obtener la fecha |
--de un log de Spring                         |
----------------------------------------------

getDateSpring :: [Char] -> [Char]
getDateSpring x = getDateSpringArray (words x)
getDateSpring [] = []


getDateSpringArray :: [[Char]] -> [Char]
getDateSpringArray [] = ""
getDateSpringArray (x:xs)
   | isDate (splitStringWithPunctuation x) = x
   | otherwise = getDateSpringArray xs


----------------------------------------------
--Funciones específicas para obtener la fecha |
--de un log de Apache (Access)                |
----------------------------------------------

getDateApacheAccess :: [Char]-> [Char]
getDateApacheAccess x = 
   if isDate (splitStringTakeOffPunctuation (getStartToFirstSymbolOccurrence ( head (getDateApacheAccessArray (words x))) ":"))
   then getStartToFirstSymbolOccurrence ( head (getDateApacheAccessArray (words x))) ":"
   else []


getDateApacheAccessArray :: [[Char]] -> [[Char]]
getDateApacheAccessArray [x] = []
getDateApacheAccessArray [] = []
getDateApacheAccessArray x = [ tail y | y <- x, (eliminate $ headMaybe y) == '['  ]


----------------------------------------------
--Funciones específicas para obtener la fecha |
--de un log de Apache (Error)                 |
----------------------------------------------

getDateApacheError :: [Char] -> [Char]
getDateApacheError x
   | (isMonth $ getDateApacheErrorArray x !! 0) && (isDay $ getDateApacheErrorArray x !! 1) && (isLongYear $ getDateApacheErrorArray x !! 3) = getDateApacheErrorArray x !! 0 ++ " " ++ getDateApacheErrorArray x !! 1 ++ " " ++ getDateApacheErrorArray x !! 3
   | otherwise = ""


getDateApacheErrorArray :: [Char] -> [[Char]]
getDateApacheErrorArray x = tail $ splitStringByPunctuation (getStartToFirstSymbolOccurrence (getFirstSymbolOccurrenceToEnd x "[") "]") " "


----------------------------------------------
--Función que generaliza los logs presentados |
--arriba                                      |
----------------------------------------------
getDate :: [Char] -> [Char]
getDate x
  | getDateSpring x /= [] = getDateSpring x
  | getDateApacheAccess x /= [] = getDateApacheAccess x
  | getDateApacheError x /= "" = getDateApacheError x
  | otherwise = ""


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
--getClass x = last (takeWhile (/=":") (words x))


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
