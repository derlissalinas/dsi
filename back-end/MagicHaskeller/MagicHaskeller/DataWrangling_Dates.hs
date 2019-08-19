module MagicHaskeller.DataWrangling_Dates where
import Language.Haskell.TH as TH
import Data.List
import Data.Char
import qualified Data.Map
import Data.Function
import MagicHaskeller.DataWrangling_Words
import MagicHaskeller.DataWrangling_Constants
import MagicHaskeller.DataWrangling_General


-----------------------------
--- Comprobaciones (Bool) ---
-----------------------------
isDay :: [Char] -> Bool
isDay x 
	| 1 <= val && val <= 31 = True
	| otherwise = False
--	where val = read x :: Int
        where val = if [ y | y <- x, isNumber y] == x
                        then read x :: Int
                        else 32
	
isMonth :: [Char] -> Bool
isMonth x 
	| (elem x monthList == True || elem x monthListShort == True) = True
	| isNumeric x && 1 <= val && val <= 12 = True
	| otherwise = False
--	where val = read x :: Int
        where val = if [ y | y <- x, isNumber y] == x
                        then read x :: Int
                        else 13
	

isShortYear :: [Char] -> Bool
isShortYear x 
	| 0 <= val && val <= 99 = True
	| otherwise = False
--	where val = read x :: Int
        where val = if [ y | y <- x, isNumber y] == x
                        then read x :: Int
                        else 100
	
isLongYear :: [Char] -> Bool
isLongYear x 
	| isNumeric x && length x == 4 = True
	| otherwise = False
	
isDMY :: [[Char]] -> Bool
isDMY [d,_,m,_,y]
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isDMY [d,m,y] 
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isDMY _ = False

isDYM :: [[Char]] -> Bool
isDYM [d,_,y,_,m]
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isDYM [d,y,m] 
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isDYM _ = False

isMDY :: [[Char]] -> Bool
isMDY [m,_,d,_,y]
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isMDY [m,d,y] 
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isMDY _ = False

isMYD :: [[Char]] -> Bool
isMYD [m,_,y,_,d]
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isMYD [m,y,d] 
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isMYD _ = False

isYMD :: [[Char]] -> Bool
isYMD [y,_,m,_,d]
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isYMD [y,m,d] 
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isYMD _ = False

isYDM :: [[Char]] -> Bool
isYDM [y,_,d,_,m]
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isYDM [y,d,m] 
	| isDay d && isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isYDM _ = False

isDM :: [[Char]] -> Bool
isDM [d,m] 
	| isDay d && isMonth m = True
	| otherwise = False
isDM _ = False

isMD :: [[Char]] -> Bool
isMD [m,d] 
	| isDay d && isMonth m = True
	| otherwise = False
isMD _ = False

isDY :: [[Char]] -> Bool
isDY [d,y] 
	| isDay d && (isShortYear y || isLongYear y) = True
	| otherwise = False
isDY _ = False

isMY :: [[Char]] -> Bool
isMY [m,y] 
	| isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isMY _ = False

isYM :: [[Char]] -> Bool
isYM [y,m] 
	| isMonth m && (isShortYear y || isLongYear y) = True
	| otherwise = False
isYM _ = False

isYD :: [[Char]] -> Bool
isYD [y,d] 
	| isDay d && (isShortYear y || isLongYear y) = True
	| otherwise = False
isYD _ = False

isWeekDay :: [Char] -> Bool
isWeekDay x 
	| (elem x weekDayList == True || elem x weekDayListShort) = True
	|otherwise = False


--------------------------------------------
----- Funciones específicas de fechas ------ ###################
--------------------------------------------

-- Día

-- simple... no tiene en cuenta fechas sin signos de puntuación o espacios.
-- puede extraer un dia, siendo otro el día, si los dos o los tres son < 12
getDayCardinalString :: [Char] -> [Char]
getDayCardinalString x = getDayCardinalArray (splitStringTakeOffPunctuation x)

getDayCardinalArray :: [[Char]] -> [Char]
getDayCardinalArray [x,y]
	| isDay x && (isMonth y || isLongYear y || isShortYear y) = x
	| isDay y && (isMonth x || isLongYear x || isShortYear x) = y
	| isDay x = x
	| isDay y = y
	|otherwise = []
getDayCardinalArray [x,y,z]
	| (read x::Int)>12 && isDay x && (isShortYear y || isShortYear z || isLongYear y || isLongYear z) = x
	| (read y::Int)>12 && isDay y && (isShortYear x || isShortYear z || isLongYear x || isLongYear z) = y
	| (read z::Int)>12 && isDay z && (isShortYear y || isShortYear x || isLongYear y || isLongYear x) = z
	| isDay x && (isMonth y || isMonth z) = x
	| isDay y && (isMonth x || isMonth z) = y
	| isDay z && (isMonth x || isMonth z) = z
	| isDay x = x
	| isDay y = y
	| isDay z = z
	|otherwise = []
getDayCardinalArray [x,_,y,_,z]
	| isDay x && (isMonth y || isLongYear y || isShortYear y) && (isMonth z || isLongYear z || isShortYear z) = x
	| isDay y && (isMonth x || isLongYear x || isShortYear x) && (isMonth z || isLongYear z || isShortYear z) = y
	| isDay z && (isMonth x || isLongYear x || isShortYear x) && (isMonth z || isLongYear z || isShortYear z) = z
	| isDay x = x
	| isDay y = y
	| isDay z = z
	|otherwise = []
getDayCardinalArray (x:xs)
	| isDay x = x
	| otherwise = getDayCardinalArray xs
getDayCardinalArray _ = []

--getDayOrdinal :: [Char] -> [Char]
getDayOrdinal :: [Char] -> [Char]
getDayOrdinal x 
	| day == [] = []
	| day == "01" || day == "1" = "1" ++ "st"
	| day == "02" || day == "2" = "2" ++ "nd"
	| day == "03" || day == "3" = "3" ++ "rd"
	| otherwise = day ++ "th"
	where day = getDayCardinalString x	
	

getWeekDayArray :: [[Char]] -> [Char]
getWeekDayArray (x:xs)
	| (elem x weekDayList == True || elem x weekDayListShort) = x
	| otherwise = getWeekDayArray xs
getWeekDayArray _ = []

getWeekDayString :: [Char] -> [Char]
getWeekDayString x = getWeekDayArray (splitStringTakeOffPunctuation x)


-- simple... no tiene en cuenta fechas sin signos de puntuación o espacios.
---- ACTUALIZADO 20/11/2017
---- NO TIENE ENCUENTA ABREVIATURAS
getMonthNameString :: [Char] -> [Char]
getMonthNameString x = getMonthNameArray (splitStringTakeOffPunctuation x)

getMonthNameArray :: [[Char]] -> [Char]
getMonthNameArray (x:xs)
	| (elem x monthList == True || elem x monthListShort == True) = x
	| otherwise = getMonthNameArray xs
getMonthNameArray _ = []


convertMonth :: [Char] -> [Char]
convertMonth x
	| isNumeric x && val<13 = monthList!!(val-1)
	| month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
	| month /= Nothing && monthNumber < 13 = show monthNumber
	| otherwise = []
	where 
		val = read x :: Int
		month = findIndex (==x) monthList
		monthNumber = ((\(Just i)->i) $ (month))+1	
		
-- Año

--MagicHaskeller: !!! getYearString:: [Char] -> [Char]
getYearString :: [Char] -> [Char]
getYearString x = getYearArray (splitStringTakeOffPunctuation x)
getYearString _ = []

-- puede sacar un año q no lo sea cuando son cortos < 31 
--- ACTUALIZADO 20/11/2017 Arreglar para detectar años cortos (solo lo detecta bien si es el primer número de la fecha)
--- 03/01/2018 si entra un string se muere: "Missing page numbers, 1993" .read no parse
getYearArray :: [[Char]] -> [Char]
getYearArray (x:xs)
	| isLongYear x = x
	| isShortYear x = x
	| otherwise = getYearArray xs
getYearArray _ = []



	
convertMonthToNumeric :: [Char] -> [Char]
convertMonthToNumeric x
	| isNumeric x && val<13 = x
	| month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
	| month /= Nothing && monthNumber < 13 = show monthNumber
	| otherwise = []
	where 
		val = read x :: Int
		month = findIndex (==x) monthList
		monthNumber = ((\(Just i)->i) $ (month))+1	
		
convertMonthToString :: [Char] -> [Char]
convertMonthToString x
	| isNumeric x && val<13 = monthList!!(val-1)
	| month /= Nothing && monthNumber < 13 = x
	| otherwise = []
	where 
		val = read x :: Int
		month = findIndex (==x) monthList
		monthNumber = ((\(Just i)->i) $ (month))+1	

-- take one element of an array 
takeTwoOfThreeArray :: [[Char]] -> Int -> Int -> [[Char]]
takeTwoOfThreeArray x y z = [x!!y] ++ [x!!z]

-- Mes
getMonthArray :: [[Char]] -> [Char]
getMonthArray [x,y]
	| isMD [x,y] == True = x
	| isDM [x,y] == True = y
	| isYM [x,y] == True = y
	| isMY [x,y] == True = x
	| otherwise = []
getMonthArray [x,y,z]
	| isMDY [x,y,z] == True = x
	| isMYD [x,y,z] == True = x
	| isDMY [x,y,z] == True = y
	| isDYM [x,y,z] == True = z
	| isYMD [x,y,z] == True = y
	| isYDM [x,y,z] == True = z
	|otherwise = []
getMonthArray (x:xs)
	| isMonth x = x
	| otherwise = getMonthArray xs
getMonthArray _ = []

getMonthString :: [Char] -> [Char]
getMonthString x = getMonthArray (splitStringTakeOffPunctuation x)

convertMonthToNumericWithinArray :: [[Char]] -> [[Char]]
convertMonthToNumericWithinArray [x,y]
	| isMD [x,y] == True = [convertMonthToNumeric x] ++ [y]
	| isDM [x,y] == True = [x]++[convertMonthToNumeric y]
	| isYM [x,y] == True = [x]++[convertMonthToNumeric y]
	| isMY [x,y] == True = [convertMonthToNumeric x] ++ [y]
	| otherwise = []
convertMonthToNumericWithinArray [x,y,z]
	| isMDY [x,y,z] == True = [convertMonthToNumeric x]++[y]++[z]
	| isMYD [x,y,z] == True = [convertMonthToNumeric x]++[y]++[z]
	| isDMY [x,y,z] == True = [x]++[convertMonthToNumeric y]++[z]
	| isDYM [x,y,z] == True = [x]++[y]++[convertMonthToNumeric z]
	| isYMD [x,y,z] == True = [x]++[convertMonthToNumeric y]++[z]
	| isYDM [x,y,z] == True = [x]++[y]++[convertMonthToNumeric z]
convertMonthToNumericWithinArray  (x:xs)
	| isMonth x = [convertMonthToNumeric x] ++ xs
	| otherwise = [x] ++ convertMonthToNumericWithinArray xs
convertMonthToNumericWithinArray _ = []

convertMonthToStringWithinArray :: [[Char]] -> [[Char]]
convertMonthToStringWithinArray [x,y]
	| isMD [x,y] == True = [convertMonthToString x] ++ [y]
	| isDM [x,y] == True = [x]++[convertMonthToString y]
	| isYM [x,y] == True = [x]++[convertMonthToString y]
	| isMY [x,y] == True = [convertMonthToString x] ++ [y]
	| otherwise = []
convertMonthToStringWithinArray [x,y,z]
	| isMDY [x,y,z] == True = [convertMonthToString x]++[y]++[z]
	| isMYD [x,y,z] == True = [convertMonthToString x]++[y]++[z]
	| isDMY [x,y,z] == True = [x]++[convertMonthToString y]++[z]
	| isDYM [x,y,z] == True = [x]++[y]++[convertMonthToString z]
	| isYMD [x,y,z] == True = [x]++[convertMonthToString y]++[z]
	| isYDM [x,y,z] == True = [x]++[y]++[convertMonthToString z]
convertMonthToStringWithinArray  (x:xs)
	| isMonth x = [convertMonthToString x] ++ xs
	| otherwise = [x] ++ convertMonthToStringWithinArray xs
convertMonthToStringWithinArray _ = []

-- Reducir string mes (January -> Jan o Jan.)
reduceMonthWithinArray :: [[Char]] -> [[Char]]	
reduceMonthWithinArray  (x:xs)
	| isMonth x = [reduceMonth ([x]!!0)] ++ xs
	| otherwise = [x] ++ reduceMonthWithinArray xs
reduceMonthWithinArray _ = []

--- 03/01/2018 cómo narices va a funcionar si los formatos son strings???? hay que rehacerlo y hacer que los formatos sean constantes!!
changeDateFormat :: [Char] -> [Char] -> [[Char]]
	-- DMY
changeDateFormat x "DMY"
	| isDMY date = [x]
	| isDYM date = [date!!0]++[date!!2]++[date!!1]
	| isMDY date = [date!!1]++[date!!0]++[date!!2]
	| isMYD date = [date!!2]++[date!!0]++[date!!1]
	| isYMD date = [date!!2]++[date!!1]++[date!!0]
	| isYDM date = [date!!1]++[date!!2]++[date!!0]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- DYM
changeDateFormat x "DYM"
	| isDYM date = [x]
	| isDMY date = [date!!0]++[date!!2]++[date!!1]
	| isMDY date = [date!!1]++[date!!2]++[date!!0]
	| isMYD date = [date!!2]++[date!!1]++[date!!0]
	| isYMD date = [date!!2]++[date!!0]++[date!!1]
	| isYDM date = [date!!1]++[date!!0]++[date!!2]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- MDY
changeDateFormat x "MDY"
	| isMDY date = [x]
	| isDMY date = [date!!1]++[date!!0]++[date!!2]
	| isDYM date = [date!!2]++[date!!0]++[date!!1]
	| isMYD date = [date!!0]++[date!!2]++[date!!1]
	| isYMD date = [date!!1]++[date!!2]++[date!!0]
	| isYDM date = [date!!2]++[date!!1]++[date!!0]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- YMD
changeDateFormat x "YMD"
	| isYMD date = [x]
	| isDMY date = [date!!2]++[date!!1]++[date!!0]
	| isDYM date = [date!!1]++[date!!2]++[date!!0]
	| isMYD date = [date!!1]++[date!!0]++[date!!2]
	| isMDY date = [date!!2]++[date!!0]++[date!!1]
	| isYDM date = [date!!0]++[date!!2]++[date!!1]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- MYD
changeDateFormat x "MYD"
	| isMYD date = [x]
	| isDMY date = [date!!1]++[date!!2]++[date!!0]
	| isDYM date = [date!!2]++[date!!1]++[date!!0]
	| isYMD date = [date!!1]++[date!!0]++[date!!2]
	| isMDY date = [date!!0]++[date!!2]++[date!!1]
	| isYDM date = [date!!2]++[date!!0]++[date!!1]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- YDM
changeDateFormat x "YDM"
	| isYDM date = [x]
	| isDMY date = [date!!1]++[date!!2]++[date!!0]
	| isDYM date = [date!!1]++[date!!0]++[date!!2]
	| isMYD date = [date!!1]++[date!!2]++[date!!0]
	| isMDY date = [date!!2]++[date!!1]++[date!!0]
	| isYMD date = [date!!0]++[date!!2]++[date!!1]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- DM
changeDateFormat x "DM"
	| isDM date = [x]
	| isMD date = [date!!1]++[date!!0]
	| isYDM date = [date!!1]++[date!!2]
	| isDMY date = [date!!0]++[date!!1]
	| isDYM date = [date!!0]++[date!!2]
	| isMYD date = [date!!2]++[date!!0]
	| isMDY date = [date!!1]++[date!!0]
	| isYMD date = [date!!2]++[date!!1]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
		-- DY
changeDateFormat x "DY"
	| isDY date = [x]
	| isYD date = [date!!1]++[date!!0]
	| isYDM date = [date!!1]++[date!!0]
	| isDMY date = [date!!0]++[date!!2]
	| isDYM date = [date!!0]++[date!!1]
	| isMYD date = [date!!2]++[date!!1]
	| isMDY date = [date!!1]++[date!!2]
	| isYMD date = [date!!2]++[date!!0]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
		-- MD
changeDateFormat x "MD"
	| isMD date = [x]
	| isDM date = [date!!1]++[date!!0]
	| isYDM date = [date!!2]++[date!!1]
	| isDMY date = [date!!1]++[date!!0]
	| isDYM date = [date!!2]++[date!!0]
	| isMYD date = [date!!0]++[date!!2]
	| isMDY date = [date!!0]++[date!!1]
	| isYMD date = [date!!1]++[date!!2]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
		-- MY
changeDateFormat x "MY"
	| isMY date = [x]
	| isYM date = [date!!1]++[date!!0]
	| isYDM date = [date!!2]++[date!!0]
	| isDMY date = [date!!1]++[date!!2]
	| isDYM date = [date!!2]++[date!!1]
	| isMYD date = [date!!0]++[date!!1]
	| isMDY date = [date!!0]++[date!!2]
	| isYMD date = [date!!1]++[date!!0]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
		-- YM
changeDateFormat x "YM"
	| isYM date = [x]
	| isMY date = [date!!1]++[date!!0]
	| isYDM date = [date!!0]++[date!!2]
	| isDMY date = [date!!2]++[date!!1]
	| isDYM date = [date!!1]++[date!!2]
	| isMYD date = [date!!2]++[date!!0]
	| isMDY date = [date!!0]++[date!!2]
	| isYMD date = [date!!1]++[date!!0]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- YD
changeDateFormat x "YD"
	| isYD date = [x]
	| isDY date = [date!!1]++[date!!0]
	| isYDM date = [date!!0]++[date!!1]
	| isDMY date = [date!!2]++[date!!0]
	| isDYM date = [date!!1]++[date!!0]
	| isMYD date = [date!!1]++[date!!2]
	| isMDY date = [date!!2]++[date!!1]
	| isYMD date = [date!!0]++[date!!2]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- D
changeDateFormat x "D"
	| isDM date = [date!!0]
	| isDY date = [date!!0]
	| isMD date = [date!!1]
	| isYD date = [date!!1]
	| isYDM date = [date!!1]
	| isDMY date = [date!!0]
	| isDYM date = [date!!0]
	| isMYD date = [date!!2]
	| isMDY date = [date!!1]
	| isYMD date = [date!!2]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- M
changeDateFormat x "M"
	| isDM date = [date!!1]
	| isMY date = [date!!0]
	| isMD date = [date!!0]
	| isYM date = [date!!1]
	| isYDM date = [date!!2]
	| isDMY date = [date!!1]
	| isDYM date = [date!!2]
	| isMYD date = [date!!0]
	| isMDY date = [date!!0]
	| isYMD date = [date!!1]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	-- Y
changeDateFormat x "Y"
	| isDY date = [date!!1]
	| isMY date = [date!!1]
	| isYD date = [date!!0]
	| isYM date = [date!!0]
	| isYDM date = [date!!0]
	| isDMY date = [date!!2]
	| isDYM date = [date!!1]
	| isMYD date = [date!!1]
	| isMDY date = [date!!2]
	| isYMD date = [date!!0]
	| otherwise = []
	where date = splitStringTakeOffPunctuation x
	


--getDayOrdinal en un array.. no hay diferencia en fisrt, second, third. todo th
convertDayOrdinalWithinArray :: [[Char]] -> [[Char]]
convertDayOrdinalWithinArray  (x:xs)
	| isNumeric x && isDay x = [getDayOrdinal x] ++ xs
	| otherwise = [x] ++ convertDayOrdinalWithinArray xs
convertDayOrdinalWithinArray _ = []

--- ACTUALIZADO 20/11/2017 No hay uno paraalargar el año.

reduceYear :: [Char] -> [Char]
reduceYear x
	| (isNumeric x) && (isLongYear x) = drop 2 x
	| (isNumeric x) && (isShortYear x) = x
	| otherwise = []
	
reduceYearWithinArray :: [[Char]] -> [[Char]]
reduceYearWithinArray  (x:xs)
	| (isNumeric x) && (isLongYear x) = [reduceYear x] ++ xs
	| otherwise = [x] ++ reduceYearWithinArray xs
reduceYearWithinArray _ = []

-- Reducir string mes (January -> Jan o Jan.)
reduceMonth :: [Char] -> [Char]
reduceMonth x = reduceWord x 3

--- ######
-- Añadir signos de puntuación en string (6 cifras, 8 cifras, con palabras)
-- Se suponen 0 en las cifras de 2 digitos
addPunctuationString :: [Char] -> [Char] -> [[Char]]
addPunctuationString a f
	| length a == 6 = [take 2 a] ++ [f] ++ [take 2 (drop 2 a)] ++ [f] ++ [take 2 (drop 4 a)]
	| length a == 8 && ( (isDay (take 2 (drop 4 a)) && isMonth (take 2 (drop 6 a))) || (isDay (take 2 (drop 6 a)) && isMonth (take 2 (drop 4 a))) ) && isLongYear (take 4 a) = [take 4 a] ++ [f] ++ [take 2 (drop 4 a)] ++ [f] ++ [take 2 (drop 6 a)]
	| length a == 8 && ( (isDay (take 2 a) && isMonth (take 2 (drop 2 a))) || (isDay (take 2 (drop 2 a)) && isMonth (take 2 a)) ) && isLongYear (take 4 (drop 4 a)) = [take 2 a] ++ [f] ++ [take 2 (drop 2 a)] ++ [f] ++ [take 4 (drop 4 a)]
	| otherwise = []
	
-- Separar cadena sin signos de puntuación (6 cifras, 8 cifras, con palabras)
-- Se suponen 0 en las cifras de 2 digitos
splitStringWithoutPunctuation :: [Char] -> [[Char]]
splitStringWithoutPunctuation a
	| length a == 6 = [take 2 a] ++ [take 2 (drop 2 a)] ++ [take 2 (drop 4 a)]
	| length a == 8 && ( (isDay (take 2 (drop 4 a)) && isMonth (take 2 (drop 6 a))) || (isDay (take 2 (drop 6 a)) && isMonth (take 2 (drop 4 a))) ) && isLongYear (take 4 a) = [take 4 a] ++ [take 2 (drop 4 a)] ++ [take 2 (drop 6 a)]
	| length a == 8 && ( (isDay (take 2 a) && isMonth (take 2 (drop 2 a))) || (isDay (take 2 (drop 2 a)) && isMonth (take 2 a)) ) && isLongYear (take 4 (drop 4 a)) = [take 2 a] ++ [take 2 (drop 2 a)] ++ [take 4 (drop 4 a)]
	| length a == 9 = [take 2 a] ++ [take 3 (drop 2 a)] ++ [take 2 (drop 5 a)] ++ [take 2 (drop 7 a)]
	| length a == 10 = [take 3 a] ++ [take 3 (drop 3 a)] ++ [take 4 (drop 6 a)]
	| length a == 12 = [take 3 del] ++ [take 3 (drop 3 del)] ++ [take 4 (drop 6 del)]
	| otherwise = []
	where del=deletePunctuationString a

convertMonthES :: [Char] -> [Char]
convertMonthES x
        | isNumeric x && val<13 = monthListES!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListES
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListES :: [[Char]]
monthListES = [   "Enero",
                "Febrero",
                "Marzo",
                "Abril",
                "Mayo",
                "Junio",
                "Julio",
                "Agosto",
                "Septiembre",
                "Octubre",
                "Noviembre",
                "Diciembre"]

convertMonthDE :: [Char] -> [Char]
convertMonthDE x
        | isNumeric x && val<13 = monthListDE!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListDE
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListDE :: [[Char]]
monthListDE = [   "Januar",
                "Februar",
                "März",
                "April",
                "Mai",
                "Juni",
                "Juli",
                "August",
                "September",
                "Oktober",
                "November",
                "Dezember"]

convertMonthIT :: [Char] -> [Char]
convertMonthIT x
        | isNumeric x && val<13 = monthListIT!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListIT
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListIT :: [[Char]]
monthListIT = [   "Gennaio",
                "Febbraio",
                "Marzo",
                "Aprile",
                "Maggio",
                "Giugno",
                "Luglio",
                "Agosto",
                "Settembre",
                "Ottobre",
                "Novembre",
                "Dicembre"]

convertMonthPT :: [Char] -> [Char]
convertMonthPT x
        | isNumeric x && val<13 = monthListPT!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListPT
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListPT :: [[Char]]
monthListPT = [   "Janeiro",
                "Fevereiro",
                "Março",
                "Abril",
                "Maio",
                "Junho",
                "Julho",
                "Agosto",
                "Setembro",
                "Outubro",
                "Novembro",
                "Dezembro"]


convertMonthFR :: [Char] -> [Char]
convertMonthFR x
        | isNumeric x && val<13 = monthListFR!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListFR
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListFR :: [[Char]]
monthListFR = [   "Janvier",
                "Février",
                "Mars",
                "Avril",
                "Mai",
                "Juin",
                "Juillet",
                "Août",
                "Septembre",
                "Octobre",
                "Novembre",
                "Décembre"]

convertMonthCAT :: [Char] -> [Char]
convertMonthCAT x
        | isNumeric x && val<13 = monthListCAT!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListCAT
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListCAT :: [[Char]]
monthListCAT = [   "Gener",
                "Febrer",
                "Març",
                "Abril",
                "Maig",
                "Juny",
                "Juliol",
                "Agost",
                "Setembre",
                "Octubre",
                "Novembre",
                "Desembre"]

convertMonthCRO :: [Char] -> [Char]
convertMonthCRO x
        | isNumeric x && val<13 = monthListCRO!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListCRO
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListCRO :: [[Char]]
monthListCRO = [    "Siječanj",
                 "Veljača",
                 "Ožujak",
                 "Travanj",
                 "May",
                 "Lipanj",
                 "Julio",
                 "Kolovoz",
                 "Rujan",
                 "Listopad",
                 "Studeni",
                 "Prosinac"]

convertMonthDAN :: [Char] -> [Char]
convertMonthDAN x
        | isNumeric x && val<13 = monthListDAN!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListDAN
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListDAN :: [[Char]]
monthListDAN = ["January",
                 "Februar",
                 "Marts",
                 "April",
                 "May",
                 "Juni",
                 "Julio",
                 "August",
                 "September",
                 "Oktober",
                 "November",
                 "December"]

convertMonthESP :: [Char] -> [Char]
convertMonthESP x
        | isNumeric x && val<13 = monthListESP!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListESP
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListESP :: [[Char]]
monthListESP = ["Januaro",
                 "Februaro",
                 "Marto",
                 "Aprilo",
                 "Majo",
                 "Junio",
                 "Julio",
                 "Aŭgusto",
                 "Septembro",
                 "Oktobro",
                 "Novembro",
                 "Decembro"]


convertMonthFIN :: [Char] -> [Char]
convertMonthFIN x
        | isNumeric x && val<13 = monthListFIN!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListFIN
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListFIN :: [[Char]]
monthListFIN = ["Tammikuu",
                 "Helmikuu",
                 "Maaliskuu",
                 "Huhtikuu",
                 "Voi",
                 "Kesäkuu",
                 "Julio",
                 "Elokuu",
                 "Syyskuu",
                 "Lokakuu",
                 "Marraskuun",
                 "Joulukuu"]


convertMonthHUN :: [Char] -> [Char]
convertMonthHUN x
        | isNumeric x && val<13 = monthListHUN!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListHUN
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListHUN :: [[Char]]
monthListHUN = ["Január",
                 "Február",
                 "March",
                 "Április",
                 "May",
                 "Június",
                 "Julio",
                 "Augusztus",
                 "Szeptember",
                 "Október",
                 "November",
                 "December"]

convertMonthIRL :: [Char] -> [Char]
convertMonthIRL x
        | isNumeric x && val<13 = monthListIRL!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListIRL
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListIRL :: [[Char]]
monthListIRL = ["Eanáir",
                 "Feabhra",
                 "Márta",
                 "Aibreán",
                 "Bealtaine",
                 "Meitheamh",
                 "Julio",
                 "Lúnasa",
                 "Meán Fómhair",
                 "Deireadh Fómhair",
                 "Samhain",
                 "Nollaig"]

convertMonthISL :: [Char] -> [Char]
convertMonthISL x
        | isNumeric x && val<13 = monthListISL!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListISL
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListISL :: [[Char]]
monthListISL = ["janúar",
                 "Febrúar",
                 "Mars",
                 "Apríl",
                 "Maí",
                 "Júní",
                 "Julio",
                 "Ágúst",
                 "September",
                 "Október",
                 "Nóvember",
                 "Desember"]


convertMonthLAT :: [Char] -> [Char]
convertMonthLAT x
        | isNumeric x && val<13 = monthListLAT!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListLAT
                monthNumber = ((\(Just i)->i) $ (month))+1


monthListLAT :: [[Char]]
monthListLAT = ["Ianuarii",
                 "Februarii",
                 "Martius",
                 "Aprilis",
                 "May",
                 "June",
                 "Julio",
                 "August",
                 "September",
                 "Octobris",
                 "November",
                 "Decembris"]

convertMonthLUX :: [Char] -> [Char]
convertMonthLUX x
        | isNumeric x && val<13 = monthListLUX!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListLUX
                monthNumber = ((\(Just i)->i) $ (month))+1


monthListLUX :: [[Char]]
monthListLUX = ["Januar",
                 "Februar",
                 "March",
                 "Abrëll",
                 "Kann",
                 "Juni",
                 "Julio",
                 "August",
                 "September",
                 "Oktober",
                 "November",
                 "Dezember"]


convertMonthMAL :: [Char] -> [Char]
convertMonthMAL x
        | isNumeric x && val<13 = monthListMAL!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListMAL
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListMAL :: [[Char]]
monthListMAL = ["Januari",
                 "Februari",
                 "Mac",
                 "April",
                 "Boleh",
                 "Jun",
                 "Julio",
                 "Ogos",
                 "September",
                 "Oktober",
                 "November",
                 "Disember"]

convertMonthHOL :: [Char] -> [Char]
convertMonthHOL x
        | isNumeric x && val<13 = monthListHOL!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListHOL
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListHOL :: [[Char]]
monthListHOL = ["Januari",
                 "Februari",
                 "Maart",
                 "April",
                 "Mei",
                 "Juni",
                 "Julio",
                 "August",
                 "September",
                 "Oktober",
                 "November",
                 "December"]


convertMonthPOL :: [Char] -> [Char]
convertMonthPOL x
        | isNumeric x && val<13 = monthListPOL!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListPOL
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListPOL :: [[Char]]
monthListPOL = ["styczeń",
                 "Luty",
                 "Marzec",
                 "Kwiecień",
                 "Maj",
                 "Czerwiec",
                 "Julio",
                 "Sierpień",
                 "Wrzesień",
                 "Październik",
                 "Listopad",
                 "Grudzień"]


convertMonthRUM :: [Char] -> [Char]
convertMonthRUM x
        | isNumeric x && val<13 = monthListRUM!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListRUM
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListRUM :: [[Char]]
monthListRUM = ["Ianuarie",
                 "Februarie",
                 "Martie",
                 "Aprilie",
                 "Mai",
                 "Iunie",
                 "Julio",
                 "August",
                 "Septembrie",
                 "Octombrie",
                 "Noiembrie",
                 "Decembrie"]


convertMonthSAM :: [Char] -> [Char]
convertMonthSAM x
        | isNumeric x && val<13 = monthListSAM!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListSAM
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListSAM :: [[Char]]
monthListSAM = ["Ianuari",
                 "Fepuari",
                 "Mati",
                 "Aperila",
                 "Me",
                 "Iuni",
                 "Julio",
                 "Aokuso",
                 "Setema",
                 "Oketopa",
                 "Novema",
                 "Tesema"]



convertMonthSOM :: [Char] -> [Char]
convertMonthSOM x
        | isNumeric x && val<13 = monthListSOM!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListSOM
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListSOM :: [[Char]]
monthListSOM = ["January",
                 "February",
                 "Maarso",
                 "Abriil",
                 "May",
                 "Juun",
                 "Julio",
                 "Agoosto",
                 "Sebtembar",
                 "Oktoobar",
                 "November",
                 "December"]



convertMonthSUA :: [Char] -> [Char]
convertMonthSUA x
        | isNumeric x && val<13 = monthListSUA!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListSUA
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListSUA :: [[Char]]
monthListSUA = ["Januari",
                 "Februari",
                 "Machi",
                 "Aprili",
                 "Mei",
                 "Juni",
                 "Julio",
                 "Agosti",
                 "Septemba",
                 "Oktoba",
                 "Novemba",
                 "Desemba"]




convertMonthSUE :: [Char] -> [Char]
convertMonthSUE x
        | isNumeric x && val<13 = monthListSUE!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListSUE
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListSUE :: [[Char]]
monthListSUE = [ "Januari",
                 "Februari",
                 "Mars",
                 "April",
                 "May",
                 "June",
                 "Julio",
                 "August",
                 "September",
                 "Oktober",
                 "November",
                 "December"]




convertMonthSUN :: [Char] -> [Char]
convertMonthSUN x
        | isNumeric x && val<13 = monthListSUN!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListSUN
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListSUN :: [[Char]]
monthListSUN = ["January",
                 "Pebruari",
                 "March",
                 "April",
                 "Mei",
                 "Juni",
                 "Julio",
                 "Agustus",
                 "September",
                 "Oktober",
                 "Nopémber",
                 "Désémber"]




convertMonthTAG :: [Char] -> [Char]
convertMonthTAG x
        | isNumeric x && val<13 = monthListTAG!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListTAG
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListTAG :: [[Char]]
monthListTAG = ["Enero",
                 "Pebrero",
                 "Marso",
                 "Abril",
                 "Mayo",
                 "Hunyo",
                 "Julio",
                 "Agosto",
                 "Setyembre",
                 "Oktubre",
                 "Nobyembre",
                 "Disyembre"]




convertMonthTUR :: [Char] -> [Char]
convertMonthTUR x
        | isNumeric x && val<13 = monthListTUR!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListTUR
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListTUR :: [[Char]]
monthListTUR = [ "Ocak",
                 "Şubat",
                 "Mart",
                 "Nisan",
                 "Mayıs",
                 "June",
                 "Julio",
                 "Ağustos",
                 "September",
                 "Ekim",
                 "November",
                 "Aralık"]



convertMonthUZB :: [Char] -> [Char]
convertMonthUZB x
        | isNumeric x && val<13 = monthListUZB!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListUZB
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListUZB :: [[Char]]
monthListUZB = ["Yanvar",
                 "Fevral",
                 "Mart",
                 "Aprel",
                 "May",
                 "Iyun",
                 "Julio",
                 "Avgust",
                 "Sentyabr",
                 "Oktyabr",
                 "Noyabr",
                 "Dekabr"]




convertMonthVIE :: [Char] -> [Char]
convertMonthVIE x
        | isNumeric x && val<13 = monthListVIE!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListVIE
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListVIE :: [[Char]]
monthListVIE = ["Tháng giêng",
                 "Tháng hai",
                 "Tháng ba",
                 "Tháng Tư",
                 "Có thể",
                 "Tháng Sáu",
                 "Julio",
                 "Tháng Tám",
                 "Tháng 9",
                 "Tháng 10",
                 "Tháng 11",
                 "Tháng 12"]




convertMonthXHO :: [Char] -> [Char]
convertMonthXHO x
        | isNumeric x && val<13 = monthListXHO!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListXHO
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListXHO :: [[Char]]
monthListXHO = ["UJanuwari",
                 "Februwari",
                 "Matshi",
                 "Epreli",
                 "Ngamana",
                 "NgoJuni",
                 "Julio",
                 "Agasti",
                 "Septemba",
                 "Oktobha",
                 "NgoNovemba",
                 "Disemba"]



convertMonthYOR :: [Char] -> [Char]
convertMonthYOR x
        | isNumeric x && val<13 = monthListYOR!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListYOR
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListYOR :: [[Char]]
monthListYOR = ["Oṣù",
                 "Kínní",
                 "Oṣù",
                 "Kẹrin",
                 "Le",
                 "Okudu",
                 "Julio",
                 "Oṣù",
                 "Kẹsán",
                 "Oṣu Kẹwa",
                 "Kọkànlá Oṣù",
                 "Kejìlá"]





convertMonthZUL :: [Char] -> [Char]
convertMonthZUL x
        | isNumeric x && val<13 = monthListZUL!!(val-1)
        | month /= Nothing && monthNumber < 10 = "0" ++ show monthNumber
        | month /= Nothing && monthNumber < 13 = show monthNumber
        | otherwise = []
        where
                val = read x :: Int
                month = findIndex (==x) monthListZUL
                monthNumber = ((\(Just i)->i) $ (month))+1

monthListZUL :: [[Char]]
monthListZUL = ["Januwari",
                 "February",
                 "Mashi",
                 "April",
                 "Kwangathi",
                 "Juni",
                 "Julio",
                 "Agasti",
                 "September",
                 "Okthoba",
                 "Novemba",
                 "Disemba"]


convertMonthAbstract :: [Char] -> [Char]
convertMonthAbstract x
        | convertMonth x /= [] = convertMonth x
        | convertMonthES x /= [] = convertMonthES x
        | convertMonthDE x /= [] = convertMonthDE x
        | convertMonthIT x /= [] = convertMonthIT x
        | convertMonthPT x /= [] = convertMonthPT x
        | convertMonthFR x /= [] = convertMonthFR x
        | convertMonthCAT x /= [] = convertMonthCAT x
        | convertMonthCRO x /= [] = convertMonthCRO x
        | convertMonthDAN x /= [] = convertMonthDAN x
        | convertMonthESP x /= [] = convertMonthESP x
        | convertMonthFIN x /= [] = convertMonthFIN x
        | convertMonthHUN x /= [] = convertMonthHUN x
        | convertMonthIRL x /= [] = convertMonthIRL x
        | convertMonthISL x /= [] = convertMonthISL x
        | convertMonthLAT x /= [] = convertMonthLAT x
        | convertMonthLUX x /= [] = convertMonthLUX x
        | convertMonthMAL x /= [] = convertMonthMAL x
        | convertMonthHOL x /= [] = convertMonthHOL x
        | convertMonthPOL x /= [] = convertMonthPOL x
        | convertMonthRUM x /= [] = convertMonthRUM x
        | convertMonthSAM x /= [] = convertMonthSAM x
		| convertMonthSOM x /= [] = convertMonthSOM x
		| convertMonthSUA x /= [] = convertMonthSUA x
		| convertMonthSUE x /= [] = convertMonthSUE x
		| convertMonthSUN x /= [] = convertMonthSUN x
		| convertMonthTAG x /= [] = convertMonthTAG x
		| convertMonthTUR x /= [] = convertMonthTUR x
		| convertMonthUZB x /= [] = convertMonthUZB x
		| convertMonthVIE x /= [] = convertMonthVIE x
		| convertMonthXHO x /= [] = convertMonthXHO x
		| convertMonthYOR x /= [] = convertMonthYOR x
		| convertMonthZUL x /= [] = convertMonthZUL x
        | otherwise = ""


-------FUNCIONES getMonthName en multiples idiomas

getMonthNameStringES :: [Char] -> [Char]
getMonthNameStringES x = getMonthNameArrayES (words x)

getMonthNameArrayES :: [[Char]] -> [Char]
getMonthNameArrayES (x:xs)
	| elem x monthListES == True = x
	| otherwise = getMonthNameArrayES xs
getMonthNameArrayES _ = []

getMonthNameStringDE :: [Char] -> [Char]
getMonthNameStringDE x = getMonthNameArrayDE (words x)

getMonthNameArrayDE :: [[Char]] -> [Char]
getMonthNameArrayDE (x:xs)
	| elem x monthListDE == True = x
	| otherwise = getMonthNameArrayDE xs
getMonthNameArrayDE _ = []

getMonthNameStringIT :: [Char] -> [Char]
getMonthNameStringIT x = getMonthNameArrayIT (words x)

getMonthNameArrayIT :: [[Char]] -> [Char]
getMonthNameArrayIT (x:xs)
	| elem x monthListIT == True = x
	| otherwise = getMonthNameArrayIT xs
getMonthNameArrayIT _ = []

getMonthNameStringPT :: [Char] -> [Char]
getMonthNameStringPT x = getMonthNameArrayPT (words x)

getMonthNameArrayPT :: [[Char]] -> [Char]
getMonthNameArrayPT (x:xs)
	| elem x monthListPT == True = x
	| otherwise = getMonthNameArrayPT xs
getMonthNameArrayPT _ = []



getMonthNameStringFR :: [Char] -> [Char]
getMonthNameStringFR x = getMonthNameArrayFR (words x)

getMonthNameArrayFR :: [[Char]] -> [Char]
getMonthNameArrayFR (x:xs)
	| elem x monthListFR == True = x
	| otherwise = getMonthNameArrayFR xs
getMonthNameArrayFR _ = []


getMonthNameStringCAT :: [Char] -> [Char]
getMonthNameStringCAT x = getMonthNameArrayCAT (words x)

getMonthNameArrayCAT :: [[Char]] -> [Char]
getMonthNameArrayCAT (x:xs)
	| elem x monthListCAT == True = x
	| otherwise = getMonthNameArrayCAT xs
getMonthNameArrayCAT _ = []



getMonthNameStringCRO :: [Char] -> [Char]
getMonthNameStringCRO x = getMonthNameArrayCRO (words x)

getMonthNameArrayCRO :: [[Char]] -> [Char]
getMonthNameArrayCRO (x:xs)
	| elem x monthListCRO == True = x
	| otherwise = getMonthNameArrayCRO xs
getMonthNameArrayCRO _ = []



getMonthNameStringDAN :: [Char] -> [Char]
getMonthNameStringDAN x = getMonthNameArrayDAN (words x)

getMonthNameArrayDAN :: [[Char]] -> [Char]
getMonthNameArrayDAN (x:xs)
	| elem x monthListDAN == True = x
	| otherwise = getMonthNameArrayDAN xs
getMonthNameArrayDAN _ = []



getMonthNameStringESP :: [Char] -> [Char]
getMonthNameStringESP x = getMonthNameArrayESP (words x)

getMonthNameArrayESP :: [[Char]] -> [Char]
getMonthNameArrayESP (x:xs)
	| elem x monthListESP == True = x
	| otherwise = getMonthNameArrayESP xs
getMonthNameArrayESP _ = []



getMonthNameStringFIN :: [Char] -> [Char]
getMonthNameStringFIN x = getMonthNameArrayFIN (words x)

getMonthNameArrayFIN :: [[Char]] -> [Char]
getMonthNameArrayFIN (x:xs)
	| elem x monthListFIN == True = x
	| otherwise = getMonthNameArrayFIN xs
getMonthNameArrayFIN _ = []



getMonthNameStringHUN :: [Char] -> [Char]
getMonthNameStringHUN x = getMonthNameArrayHUN (words x)

getMonthNameArrayHUN :: [[Char]] -> [Char]
getMonthNameArrayHUN (x:xs)
	| elem x monthListHUN == True = x
	| otherwise = getMonthNameArrayHUN xs
getMonthNameArrayHUN _ = []



getMonthNameStringIRL :: [Char] -> [Char]
getMonthNameStringIRL x = getMonthNameArrayIRL (words x)

getMonthNameArrayIRL :: [[Char]] -> [Char]
getMonthNameArrayIRL (x:xs)
	| elem x monthListIRL == True = x
	| otherwise = getMonthNameArrayIRL xs
getMonthNameArrayIRL _ = []



getMonthNameStringISL :: [Char] -> [Char]
getMonthNameStringISL x = getMonthNameArrayISL (words x)

getMonthNameArrayISL :: [[Char]] -> [Char]
getMonthNameArrayISL (x:xs)
	| elem x monthListISL == True = x
	| otherwise = getMonthNameArrayISL xs
getMonthNameArrayISL _ = []



getMonthNameStringLAT :: [Char] -> [Char]
getMonthNameStringLAT x = getMonthNameArrayLAT (words x)

getMonthNameArrayLAT :: [[Char]] -> [Char]
getMonthNameArrayLAT (x:xs)
	| elem x monthListLAT == True = x
	| otherwise = getMonthNameArrayLAT xs
getMonthNameArrayLAT _ = []



getMonthNameStringLUX :: [Char] -> [Char]
getMonthNameStringLUX x = getMonthNameArrayLUX (words x)

getMonthNameArrayLUX :: [[Char]] -> [Char]
getMonthNameArrayLUX (x:xs)
	| elem x monthListLUX == True = x
	| otherwise = getMonthNameArrayLUX xs
getMonthNameArrayLUX _ = []



getMonthNameStringMAL :: [Char] -> [Char]
getMonthNameStringMAL x = getMonthNameArrayMAL (words x)

getMonthNameArrayMAL :: [[Char]] -> [Char]
getMonthNameArrayMAL (x:xs)
	| elem x monthListMAL == True = x
	| otherwise = getMonthNameArrayMAL xs
getMonthNameArrayMAL _ = []



getMonthNameStringHOL :: [Char] -> [Char]
getMonthNameStringHOL x = getMonthNameArrayHOL (words x)

getMonthNameArrayHOL :: [[Char]] -> [Char]
getMonthNameArrayHOL (x:xs)
	| elem x monthListHOL == True = x
	| otherwise = getMonthNameArrayHOL xs
getMonthNameArrayHOL _ = []



getMonthNameStringPOL :: [Char] -> [Char]
getMonthNameStringPOL x = getMonthNameArrayPOL (words x)

getMonthNameArrayPOL :: [[Char]] -> [Char]
getMonthNameArrayPOL (x:xs)
	| elem x monthListPOL == True = x
	| otherwise = getMonthNameArrayPOL xs
getMonthNameArrayPOL _ = []



getMonthNameStringRUM :: [Char] -> [Char]
getMonthNameStringRUM x = getMonthNameArrayRUM (words x)

getMonthNameArrayRUM :: [[Char]] -> [Char]
getMonthNameArrayRUM (x:xs)
	| elem x monthListRUM == True = x
	| otherwise = getMonthNameArrayRUM xs
getMonthNameArrayRUM _ = []



getMonthNameStringSAM :: [Char] -> [Char]
getMonthNameStringSAM x = getMonthNameArraySAM (words x)

getMonthNameArraySAM :: [[Char]] -> [Char]
getMonthNameArraySAM (x:xs)
        | elem x monthListSAM == True = x
        | otherwise = getMonthNameArraySAM xs
getMonthNameArraySAM _ = []



getMonthNameStringSOM :: [Char] -> [Char]
getMonthNameStringSOM x = getMonthNameArraySOM (words x)

getMonthNameArraySOM :: [[Char]] -> [Char]
getMonthNameArraySOM (x:xs)
        | elem x monthListSOM == True = x
        | otherwise = getMonthNameArraySOM xs
getMonthNameArraySOM _ = []



getMonthNameStringSUA :: [Char] -> [Char]
getMonthNameStringSUA x = getMonthNameArraySUA (words x)

getMonthNameArraySUA :: [[Char]] -> [Char]
getMonthNameArraySUA (x:xs)
        | elem x monthListSUA == True = x
        | otherwise = getMonthNameArraySUA xs
getMonthNameArraySUA _ = []




getMonthNameStringSUE :: [Char] -> [Char]
getMonthNameStringSUE x = getMonthNameArraySUE (words x)

getMonthNameArraySUE :: [[Char]] -> [Char]
getMonthNameArraySUE (x:xs)
        | elem x monthListSUE == True = x
        | otherwise = getMonthNameArraySUE xs
getMonthNameArraySUE _ = []



getMonthNameStringSUN :: [Char] -> [Char]
getMonthNameStringSUN x = getMonthNameArraySUN (words x)

getMonthNameArraySUN :: [[Char]] -> [Char]
getMonthNameArraySUN (x:xs)
        | elem x monthListSUN == True = x
        | otherwise = getMonthNameArraySUN xs
getMonthNameArraySUN _ = []



getMonthNameStringTAG :: [Char] -> [Char]
getMonthNameStringTAG x = getMonthNameArrayTAG (words x)

getMonthNameArrayTAG :: [[Char]] -> [Char]
getMonthNameArrayTAG (x:xs)
        | elem x monthListTAG == True = x
        | otherwise = getMonthNameArrayTAG xs
getMonthNameArrayTAG _ = []




getMonthNameStringTUR :: [Char] -> [Char]
getMonthNameStringTUR x = getMonthNameArrayTUR (words x)

getMonthNameArrayTUR :: [[Char]] -> [Char]
getMonthNameArrayTUR (x:xs)
        | elem x monthListTUR == True = x
        | otherwise = getMonthNameArrayTUR xs
getMonthNameArrayTUR _ = []




getMonthNameStringUZB :: [Char] -> [Char]
getMonthNameStringUZB x = getMonthNameArrayUZB (words x)

getMonthNameArrayUZB :: [[Char]] -> [Char]
getMonthNameArrayUZB (x:xs)
        | elem x monthListUZB == True = x
        | otherwise = getMonthNameArrayUZB xs
getMonthNameArrayUZB _ = []




getMonthNameStringVIE :: [Char] -> [Char]
getMonthNameStringVIE x = getMonthNameArrayVIE (words x)

getMonthNameArrayVIE :: [[Char]] -> [Char]
getMonthNameArrayVIE (x:xs)
        | elem x monthListVIE == True = x
        | otherwise = getMonthNameArrayVIE xs
getMonthNameArrayVIE _ = []




getMonthNameStringXHO :: [Char] -> [Char]
getMonthNameStringXHO x = getMonthNameArrayXHO (words x)

getMonthNameArrayXHO :: [[Char]] -> [Char]
getMonthNameArrayXHO (x:xs)
        | elem x monthListXHO == True = x
        | otherwise = getMonthNameArrayXHO xs
getMonthNameArrayXHO _ = []




getMonthNameStringYOR :: [Char] -> [Char]
getMonthNameStringYOR x = getMonthNameArrayYOR (words x)

getMonthNameArrayYOR :: [[Char]] -> [Char]
getMonthNameArrayYOR (x:xs)
        | elem x monthListYOR == True = x
        | otherwise = getMonthNameArrayYOR xs
getMonthNameArrayYOR _ = []




getMonthNameStringZUL :: [Char] -> [Char]
getMonthNameStringZUL x = getMonthNameArrayZUL (words x)

getMonthNameArrayZUL :: [[Char]] -> [Char]
getMonthNameArrayZUL (x:xs)
        | elem x monthListZUL == True = x
        | otherwise = getMonthNameArrayZUL xs
getMonthNameArrayZUL _ = []



getMonthNameAbstract :: [Char] -> [Char]
getMonthNameAbstract x
        | getMonthNameString x /= [] = getMonthNameString x
        | getMonthNameStringES x /= [] = getMonthNameStringES x
        | getMonthNameStringDE x /= [] = getMonthNameStringDE x
        | getMonthNameStringIT x /= [] = getMonthNameStringIT x
        | getMonthNameStringPT x /= [] = getMonthNameStringPT x
        | getMonthNameStringFR x /= [] = getMonthNameStringFR x
        | getMonthNameStringCAT x /= [] = getMonthNameStringCAT x
        | getMonthNameStringCRO x /= [] = getMonthNameStringCRO x
        | getMonthNameStringDAN x /= [] = getMonthNameStringDAN x
        | getMonthNameStringESP x /= [] = getMonthNameStringESP x
        | getMonthNameStringFIN x /= [] = getMonthNameStringFIN x
        | getMonthNameStringHUN x /= [] = getMonthNameStringHUN x
        | getMonthNameStringIRL x /= [] = getMonthNameStringIRL x
        | getMonthNameStringISL x /= [] = getMonthNameStringISL x
        | getMonthNameStringLAT x /= [] = getMonthNameStringLAT x
        | getMonthNameStringLUX x /= [] = getMonthNameStringLUX x
        | getMonthNameStringMAL x /= [] = getMonthNameStringMAL x
        | getMonthNameStringHOL x /= [] = getMonthNameStringHOL x
        | getMonthNameStringPOL x /= [] = getMonthNameStringPOL x
        | getMonthNameStringRUM x /= [] = getMonthNameStringRUM x
        | getMonthNameStringSAM x /= [] = getMonthNameStringSAM x
		| getMonthNameStringSOM x /= [] = getMonthNameStringSOM x
		| getMonthNameStringSUA x /= [] = getMonthNameStringSUA x
		| getMonthNameStringSUE x /= [] = getMonthNameStringSUE x
		| getMonthNameStringSUN x /= [] = getMonthNameStringSUN x
		| getMonthNameStringTAG x /= [] = getMonthNameStringTAG x
		| getMonthNameStringTUR x /= [] = getMonthNameStringTUR x
		| getMonthNameStringUZB x /= [] = getMonthNameStringUZB x
		| getMonthNameStringVIE x /= [] = getMonthNameStringVIE x
		| getMonthNameStringXHO x /= [] = getMonthNameStringXHO x
		| getMonthNameStringYOR x /= [] = getMonthNameStringYOR x
		| getMonthNameStringZUL x /= [] = getMonthNameStringZUL x
        | otherwise = ""