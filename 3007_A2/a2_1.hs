import Codec.BMP
import Data.ByteString
import Data.Either
import GHC.Word
import System.IO.Unsafe

loadBitmap :: FilePath -> [[(Int, Int, Int)]]
loadBitmap filename = repackAs2DList (either returnEmptyOnError processDataOnBMP (unsafePerformIO (readBMP filename)))

returnEmptyOnError :: Error -> ([(Int, Int, Int)], (Int, Int))
returnEmptyOnError _ = ([], (0, 0))

processDataOnBMP :: BMP -> ([(Int, Int, Int)], (Int, Int))
processDataOnBMP bmp = ((parseIntoRGBVals (convertToInts (unpack (unpackBMPToRGBA32 bmp)))), (bmpDimensions bmp))

convertToInts :: [Word8] -> [Int]
convertToInts [] = []
convertToInts (h:t) = (fromIntegral (toInteger h)) : (convertToInts t)

parseIntoRGBVals :: [Int] -> [(Int, Int, Int)]
parseIntoRGBVals [] = []
parseIntoRGBVals (h:i:j:_:t) = (h,i,j) : (parseIntoRGBVals t)

repackAs2DList :: ([(Int, Int, Int)], (Int, Int)) -> [[(Int, Int, Int)]]
repackAs2DList (pixels, (width, height)) = (Prelude.reverse (repackAs2DList' pixels width height))

repackAs2DList' :: [(Int, Int, Int)] -> Int -> Int -> [[(Int, Int, Int)]]
repackAs2DList' []  width  height = []
repackAs2DList' pixels width height = (Prelude.take width pixels) : (repackAs2DList' (Prelude.drop width pixels) width height)

showAsASCIIArt :: [[Char]] -> IO ()
showAsASCIIArt pixels = Prelude.putStr (unlines pixels)


-- get a string, bool value and a list of tuple of 3 as inputs
--  then convert list of tuples to a list of characters by using the provided string, boolean values.
oneDimension :: [Char]-> Bool -> [(Int,Int,Int)] -> [Char]
oneDimension a b c
            |c == [] = []
            |b  == True  && getHead c == (255,255,255) = (getHead a: oneDimension a b (getTail c))
            |b  == True && getHead c == (0,0,0) = (a !! ((size a)-1) : oneDimension a b (getTail c))
            |b  == False && getHead c == (255,255,255) = (a !! ((size a)-1) : oneDimension a b (getTail c))
            |b  == False && getHead c == (0,0,0) = (getHead a : oneDimension a b (getTail c))
            | otherwise = (a !! (getIndex (getFirst (getHead c)) (convertCharToInt a (getRanger a ) (size a))) : oneDimension a b (getTail c))

twoDimension :: [Char]-> Bool -> [[(Int,Int,Int)]] -> [[Char]]
twoDimension a b c
            | c == [[]] = [[]]
            | size c == 0 = [[]]
            |otherwise = (oneDimension a b (getHead c) : twoDimension a b (getTail c))


-- SUPPORT FUNCTIONS

-- getFirst elements of the tuple
getFirst :: (Int,Int,Int) -> Int
getFirst (x,_,_) = x

-- decreasing order
convertCharToInt :: [Char] -> Int -> Int-> [Int]
convertCharToInt a b c
                | a == [] = []
                | otherwise = ((b*c) : convertCharToInt (getTail a) b (c-1))

--get range
getRanger :: [Char] -> Int
getRanger a  = 255 `div` size (a)


-- get a tuple, a list of integer, and an index
-- then return the position based on tuple's value
getIndex :: Int -> [Int] -> Int
getIndex a b
        | b == [] = 0
        | getHead b > a  && size b > 1 = 1 + getIndex a (getTail b)
        | otherwise = 0

-- get size of a list
size :: [a] -> Int
size [] = 0
size (_:x) = 1 + size x

-- get head of a list
getHead :: [a] -> a
getHead (x:_) = x

-- get tail of a list
getTail :: [a] -> [a]
getTail [] = []
getTail (_:x) = x
