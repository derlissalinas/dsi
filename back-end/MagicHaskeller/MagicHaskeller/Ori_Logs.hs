module MagicHaskeller.Logs where
import Language.Haskell.TH as TH
import Data.Char
import Text.Printf
import MagicHaskeller.DataWrangling_Words
import MagicHaskeller.DataWrangling_Constants
import MagicHaskeller.DataWrangling_Dates



---Extrae la fecha de un log de Spring
extractDate :: [Char] -> [Char]
extractDate x = unwords (take 2 (splitStringByPunctuation x " "))

extractApacheDate :: [Char] -> [Char]
--extractApacheDate x = getStartToFirstSymbolOccurrence ( head (getDateApacheAccessArray (words x))) ":" 
--extractApacheDate x = head [ tail y | y <- (words x), (head y == '[')  ]
--extractDate (x:xs)
--        | x == head lBracket = getStartToFirstSymbolOccurrence xs space
--        | otherwise = extractDate xs
extractApacheDate x = getStartToFirstSymbolOccurrence (getLastSymbolOccurrenceToEnd x lBracket) rBracket


extractApacheDate2 :: [Char] -> [Char]
extractApacheDate2 x = getStartToFirstSymbolOccurrence (getLastSymbolOccurrenceToEnd x lBracket) space
--extractApacheDate2 (x:xs)
--        | x == head lBracket = getStartToFirstSymbolOccurrence xs space
--        | otherwise = extractApacheDate2 xs


extractApacheDate3 :: [Char] -> [Char]
extractApacheDate3 (x:xs)
        | x == head lBracket = getStartToFirstSymbolOccurrence xs space
        | otherwise = extractApacheDate3 xs
extractApacheDate3 _ = ""

extractApacheDate4 :: [Char] -> [Char]
extractApacheDate4 x = if elem '[' x then extractApacheDateArray4 (words x) else ""
--extractApacheDate4 _ = ""

extractApacheDateArray4 :: [String] -> [Char]
extractApacheDateArray4 = go
   where 
	go [] = ""
	go (x:xs) = if lBracket == headString x
			then getLastSymbolOccurrenceToEnd x lBracket
			else extractApacheDateArray4 xs

--extractApacheDateArray4 (x:xs)
--  | lBracket == headString x = getLastSymbolOccurrenceToEnd x lBracket
--  | otherwise = extractApacheDateArray4 xs
--extractApacheDateArray4 _ = ""



{-
headString :: [Char] -> Char
headString x
   | null x = '!'
   | otherwise = x!!0
-}

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
{-
--extractDate (x:xs)
--  | isDigit x || isPunctuation x =  [x] ++ extractDate xs
--  | isSpace x && isDigit y = [x] ++ extractDate xs
--  | isSpace x = []
--  where y = head xs


--getLogDate :: [Char] -> [Char]
--getLogDate x
--  | isYMD ((words (extractDate x))!!0) = "TRUE"
--  | otherwise = "FALSE"

-}



---AGREGADO 17-06-------

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


-- getLogTime :: [Char]-> [Char]
-- getLogTime x = getLogTimeArray (words x)

getLogTimeArray :: [[Char]] -> [Char]
getLogTimeArray (x:xs)
   | isLogTime (splitStringByPunctuation x ":") = x
   | isLogTime $ tail (splitStringByPunctuation x ":") = changePunctuationString (unwords $ tail (splitStringByPunctuation x ":")) ":"
   | otherwise = getLogTimeArray xs
getLogTimeArray _ = []


{- Copia solo para ver cerca y tener de referencia

getLogLevelString :: [Char] -> [Char]
getLogLevelString x = getLogLevelArray (words x)

getLogLevelArray :: [[Char]] -> [Char]
getLogLevelArray (x:xs)
  | elem x logLevelList = x
  | otherwise = getLogLevelArray xs
getLogLevelArray _ = []
-}


getDateSpring :: [Char] -> [Char]
--getDateSpring x = if isDate $ splitStringWithPunctuation $ eliminate (headMaybe $ words x) then eliminate (headMaybe $ words x) else [] --getDateSpringArray (words x)
getDateSpring x = getDateSpringArray (words x)
getDateSpring [] = []
--getDateSpring (x:y:xs) --  -> getDateSpring (getStartToFirstSymbolOccurrence a lBracket)
--   | x == '2' = 
--getDateSpring _ = ""

splitBySpace :: [Char] -> [[Char]]
splitBySpace x = words x


getDateSpringArray :: [[Char]] -> [Char]
getDateSpringArray (x:xs)
--   | x == "---" || x == "-"  = ""
-- isDate (splitStringWithPunctuation x) = x  ----orig.
--   | True = x -- -> getDateSpring (getStartToFirstSymbolOccurrence a lBracket))
--   | isDate ["05","05","2005"] = x  -- -> getDateSpring (getStartToFirstSymbolOccurrence a lBracket) 
--   | splitStringWithPunctuation "05-05-2005" == ["05","05","2005"] = x   -> getDateSpring (getStartToFirstSymbolOccurrence a lBracket))
--  | isDate (splitStringWithPunctuation "05-05-2005") = x
   | isDate (splitStringWithPunctuation x) = x --x
--   | "2017-03-25" == x = "entro_bien"
   | otherwise = getDateSpringArray xs--getDateSpringArray xs
---getDateSpringArray [] = "#"
--getDateSpringArray _ = "#"
getDateSpringArray [] = ""
getDateSpringArray _ = ""

{- original
getDateApacheAccess :: [Char]-> [Char]
getDateApacheAccess x = if isDate (splitStringTakeOffPunctuation (getStartToFirstSymbolOccurrence ( head (getDateApacheAccessArray (words x))) ":"))
   then getStartToFirstSymbolOccurrence ( head (getDateApacheAccessArray (words x))) ":"
   else []
-}

getDateApacheAccess :: [Char] -> [Char]
getDateApacheAccess [] = ""
getDateApacheAccess x = if (entry /= "") && (isDate (splitStringTakeOffPunctuation entry))
			then entry
			else ""
			where entry = getStartToFirstSymbolOccurrence (eliminate $ headMaybe (getDateApacheAccessArray (words x))) ":"

getDateApacheAccessArray :: [[Char]] -> [[Char]]
getDateApacheAccessArray [x] = []
getDateApacheAccessArray [] = []
getDateApacheAccessArray x = [ tail y | y <- x, (eliminate $ headMaybe y) == '['  ]
--getDateApacheAccessArray [] = []
--getDateApacheAccessArray _ = []


getDateApacheError :: [Char] -> [Char]
getDateApacheError x
   | (isMonth $ getDateApacheErrorArray x !! 0) && (isDay $ getDateApacheErrorArray x !! 1) && (isLongYear $ getDateApacheErrorArray x !! 3) = getDateApacheErrorArray x !! 0 ++ " " ++ getDateApacheErrorArray x !! 1 ++ " " ++ getDateApacheErrorArray x !! 3
   | otherwise = ""


getDateApacheErrorArray :: [Char] -> [[Char]]
getDateApacheErrorArray x = tail $ splitStringByPunctuation (getStartToFirstSymbolOccurrence (getFirstSymbolOccurrenceToEnd x "[") "]") " "

{-
getDate :: [Char] -> [Char]
getDate x
  | getDateSpring x /= [] = getDateSpring x
  | getDateApacheAccess x /= [] = getDateApacheAccess x
  | getDateApacheError x /= "" = getDateApacheError x
  | otherwise = ""
-}









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


--getClass :: [Char] -> [Char]
--getClass (x:xs)
--  | isDot (toChar x) && isUpper y = head (words xs)
--  | otherwise = getClass xs
--  where y = head xs
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
