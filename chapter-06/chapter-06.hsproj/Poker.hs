module Poker where
  
class ShortShow a where
  shortshow :: a -> String


-- Suit
----------
data Suit = Spade | Diamond | Club | Heart
  deriving (Show, Enum)

-- Eq is manually implemented but could also be derived
instance Eq Suit where
  (==) Spade Spade = True
  (==) Diamond Diamond = True
  (==) Club Club = True
  (==) Heart Heart = True
  (==) _ _ = False
  
instance ShortShow Suit where
  shortshow Spade   = "S"
  shortshow Diamond = "D"
  shortshow Club    = "C"
  shortshow Heart   = "H"


-- Rank
----------
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Ord, Show, Eq, Enum)
  
instance ShortShow Rank where
  shortshow Two   = "2"
  shortshow Three = "3"
  shortshow Four  = "4"
  shortshow Five  = "5"
  shortshow Six   = "6"
  shortshow Seven = "7"
  shortshow Eight = "8"
  shortshow Nine  = "9"
  shortshow Ten   = "T"
  shortshow Jack  = "J"
  shortshow Queen = "Q"
  shortshow King  = "K"
  shortshow Ace   = "A"


-- Card
----------
data Card r s = Card Rank Suit

instance (Eq r, Eq s) => Eq (Card r s) where
  (==) (Card r s) (Card r' s') = r == r' && s == s'
  
instance Show (Card r s) where
  show (Card r s) = show r ++ " " ++ (show s)
  
instance (Ord r, Ord s) => Ord (Card r s) where
  compare (Card r _) (Card r' _) = compare r r'
  
instance (ShortShow r, ShortShow s) => ShortShow (Card r s) where
  shortshow (Card r s) = shortshow r ++ (shortshow s)
  

-- Hand
----------
data Hand c = Hand (Card c c) (Card c c)

instance Show (Hand c) where
  show (Hand c1 c2) = "(" ++ show c1 ++ ", " ++ show c2 ++ ")"
  
instance (ShortShow c) => ShortShow (Hand c) where
  shortshow (Hand c1 c2) = "(" ++ (shortshow c1) ++ "," ++ (shortshow c2) ++ ")"
  