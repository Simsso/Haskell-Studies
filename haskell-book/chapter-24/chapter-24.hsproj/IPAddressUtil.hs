{-# OPTIONS_GHC -Wall #-}

module IPAddressUtil (ipv4Parser, ipv6Parser) where
  
import Control.Applicative (empty)
import Data.Bits (shiftL, (.|.), (.&.))
import Data.Char (digitToInt)
import Data.Word (Word8, Word16, Word32, Word64)
import Text.Trifecta
import Data.List

data IPAddress = IPAddress Word32
  deriving (Eq, Ord)
  
instance Show IPAddress where
  show (IPAddress w) = intercalate "." $ showBytes w
  
showBytes :: Word32 -> [String]
showBytes w = map show $ splitBytes w

splitBytes :: Word32 -> [Word32]
splitBytes w = undefined where
  maskBytes :: Word32 -> [Word32]
  maskBytes = undefined
  byteIndices = enumFromThen 0 8
  
ipv4Parser :: Parser IPAddress
ipv4Parser = IPAddress . cast32 <$> (byte `sepBy` dot <* eof >>= lengthIs 4)

byte :: Parser Word8
byte = integer >>= rangeCheck where 
  rangeCheck i = if inByteRange i then return $ fromIntegral i else empty
    

--cast8 :: Foldable f => f Word8 -> Word8
--cast8 = foldl (\w b -> shiftL w 4 .|. b) 0

-- converts a foldable with 4 elements into an unsigned short
cast16 :: Foldable f => f Word8 -> Word16
cast16 = foldl (\w b -> shiftL w 4 .|. fromIntegral b) 0

-- converts a foldable with 4 elements into an unsigned integer
cast32 :: Foldable f => f Word8 -> Word32
cast32 = foldl (\w b -> shiftL w 8 .|. fromIntegral b) 0

cast64 :: Foldable f => f Word16 -> Word64
cast64 = foldl (\w b -> shiftL w 16 .|. fromIntegral b) 0

cast128 :: [Word16] -> (Word64, Word64)
cast128 ws = let highlen = max 0 $ (-4) + length ws
             in  (cast64 (take highlen ws), cast64 (drop highlen ws))

inRange :: Ord a => (a, a) -> a -> Bool
inRange (minv, maxv) x = x >= minv && x <= maxv

inByteRange :: (Ord a, Num a) => a -> Bool
inByteRange = inRange (0, 255)

lengthIs :: Int -> [a] -> Parser [a]
lengthIs n xs = if length xs == n then return xs else empty

lengthAtMost :: Int -> [a] -> Parser [a]
lengthAtMost n xs = if length xs <= n then return xs else empty


data IPAddress6 = IPAddress6 Word64 Word64
  deriving (Eq, Ord)
  
instance Show IPAddress6 where
  show (IPAddress6 w1 w0) = show val where
    val = (toInteger w1) * twopow64 + (toInteger w0)
    twopow64 :: Integer
    twopow64 = foldr (*) 1 (take 64 $ repeat 2)
    
data IPAddress6Block = Values Word16 | Zeros deriving (Eq, Show)
  
ipv6Parser :: Parser IPAddress6
ipv6Parser = --some (try hexByte <* (try colon >> pure () <|> eof) <|> zeroShortcut)
  let parts :: Parser [IPAddress6Block]
      parts = hexByte `sepBy` colon
      bytes = parts >>= unpackZerosShortcut >>= lengthAtMost 8
  in  (uncurry IPAddress6) . cast128 <$> bytes

unpackZerosShortcut :: [IPAddress6Block] -> Parser [Word16]
unpackZerosShortcut blocks = 
  let countBlocks = length blocks
      zerosCount = length . filter (Zeros ==) $ blocks
      parseError = empty
      zerosMissing = countBlocks - zerosCount
      toWords :: IPAddress6Block -> [Word16]
      toWords Zeros = replicate zerosMissing 0
      toWords (Values x) = [x]
  in if zerosCount > 1 || countBlocks >= 9 then parseError else
     if zerosCount < 1 && countBlocks /= 8 then parseError else
     return $ foldr ((++) . toWords) [] blocks

hexByte :: Parser IPAddress6Block
hexByte = let pchars = many hexChar >>= lengthAtMost 4
              pword8 = (fmap . fmap) (fromIntegral . digitToInt) pchars
              --countP = fmap length pword8
          --in  countP >>= (\n -> if n == 0 then return Zeros else return (Values . cast16 <$> pword8))
          in Values . cast16 <$> pword8

hexChar :: Parser Char 
hexChar = oneOf "0123456789abcdefABCDEF"
