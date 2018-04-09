{-# OPTIONS_GHC -Wall #-}

module IPAddressUtil where
  
import Control.Applicative
import Text.Trifecta
import Data.Word
import Data.Bits

data IPAddress = IPAddress Word32
  deriving (Eq, Ord, Show)
  
ipv4Parser :: Parser IPAddress
ipv4Parser = do
  b3 <- byteParser
  _ <- char '.'
  b2 <- byteParser
  _ <- char '.'
  b1 <- byteParser
  _ <- char '.'
  b0 <- byteParser
  _ <- eof
  return $ IPAddress . uint8ListToUint32 $ [b3, b2, b1, b0]

byteParser :: Parser Word8
byteParser = do
  i <- integer
  if inByteRange i
    then return $ fromIntegral i
    else empty -- parse error
    
uint8ListToUint32 :: Foldable f => f Word8 ->  Word32
uint8ListToUint32 = foldl (\w b -> shiftL w 8 .|. fromIntegral b) 0

inRange :: Ord a => (a, a) -> a -> Bool
inRange (minv, maxv) x = x >= minv && x <= maxv

inByteRange :: (Ord a, Num a) => a -> Bool
inByteRange = inRange (0, 255)