-- This is ONLY the DOMAIN of the haskell game.  

-- importing the necessary functions. 
import Data.List     (foldl', group, sort)
import Data.Set      (Set, empty, insert)

-- Rank of the cards.
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | J | Q | K | A
    deriving (Bounded, Enum, Eq, Ord)

instance Show Rank where
    show R2  = "2"
    show R3  = "3"
    show R4  = "4"
    show R5  = "5"
    show R6  = "6"
    show R7  = "7"
    show R8  = "8"
    show R9  = "9"
    show R10 = "10"
    show J   = "J"
    show Q   = "Q"
    show K   = "K"
    show A   = "A"
-- suit represents spades, hearts, diamonds, and clovers.
data Suit = S | H | D | C
    deriving (Bounded, Enum, Eq, Ord, Show)
-- Each card consists of a rank and a suit. 
data Card = Card { rank :: Rank, suit :: Suit }
    deriving (Eq, Ord)

instance Show Card where
    show Card { rank = r, suit = s } = show r ++ show s
-- the Deck data type is an alias of a list of cards. 
type Deck = [Card]

-- representing the full Deck and the piquet Deck (the piquet Deck is a french suited deck of 32 cards from 7 UPwards to Ace of all four suits).
fullDeck, piquetDeck :: Deck
fullDeck   = [Card R2 H, Card R3 H, Card R4 H, Card R5 H, Card R6 H, Card R7 H, Card R8 H, Card R9 H, Card R10 H, Card J H, Card Q H, Card K H, Card A H,
              Card R2 S, Card R3 S, Card R4 S, Card R5 S, Card R6 S, Card R7 S, Card R8 S, Card R9 S, Card R10 S, Card J S, Card Q S, Card K S, Card A S,
              Card R2 D, Card R3 D, Card R4 D, Card R5 D, Card R6 D, Card R7 D, Card R8 D, Card R9 D, Card R10 D, Card J D, Card Q D, Card K D, Card A D,
              Card R2 C, Card R3 C, Card R4 C, Card R5 C, Card R6 C, Card R7 C, Card R8 C, Card R9 C, Card R10 C, Card J C, Card Q C, Card K C, Card A C]

piquetDeck = [Card R7 H, Card R8 H, Card R9 H, Card R10 H, Card J H, Card Q H, Card K H, Card A H,
              Card R7 S, Card R8 S, Card R9 S, Card R10 S, Card J S, Card Q S, Card K S, Card A S,
              Card R7 D, Card R8 D, Card R9 D, Card R10 D, Card J D, Card Q D, Card K D, Card A D,
              Card R7 C, Card R8 C, Card R9 C, Card R10 C, Card J C, Card Q C, Card K C, Card A C]

-- This datatype is just a wrapper for the list of cards which form a hand.
newtype Hand = Hand { unHand :: [Card] } deriving (Eq, Show)

-- The types that a hand has points. Notice the ORD deriving which means the constructs
-- from left to right (or in this case from up to bottom) become bigger in a sense. 
-- to find out the meaning of the constructors see the poker game rules in wikipedia.
-- HighCard has a field which is the list of the ranks of the cards in hand. 
-- OnePair has two fields, the first of which is the rank of the card which is in pair 
-- and the second is a list of the ranks of the other cards. StraightFlush has one field
-- representing the highest rank card in the hand which has a straight flush. Other reasonings
-- are the same. 

data HandCategory
    = HighCard      [Rank]
    | OnePair       Rank [Rank]
    | TwoPair       Rank Rank Rank
    | ThreeOfAKind  Rank Rank Rank
    | Straight      Rank
    | Flush         [Rank]
    | FullHouse     Rank Rank
    | FourOfAKind   Rank Rank
    | StraightFlush Rank
    deriving (Eq, Ord, Show)
    
-- checks if all the cards in the hand are of the same suit.
-- this will be handy in defining straightflush and flush. 
sameSuits :: Hand -> Bool
sameSuits (Hand xs) = all ((==S).suit) xs || all ((==H).suit) xs || all ((==D).suit) xs || all ((==C).suit) xs

-- checks if the ranks are stright if so returns the highest order of the sequence if not returns nothing.
-- notice that Ace is the minimum in the first sequence but the maximum in the last one.
-- This will be hand in comparing the hands when they both are straight. 
isStraight :: [Rank] -> Maybe Rank
isStraight xs
  | ys == [R2,R3,R4,R5,A]   = Just R5
  | ys == [R2,R3,R4,R5,R6]  = Just R6
  | ys == [R3,R4,R5,R6,R7]  = Just R7
  | ys == [R4,R5,R6,R7,R8]  = Just R8
  | ys == [R5,R6,R7,R8,R9]  = Just R9
  | ys == [R6,R7,R8,R9,R10] = Just R10
  | ys == [R7,R8,R9,R10,J]  = Just J
  | ys == [R8,R9,R10,J,Q]   = Just Q
  | ys == [R9,R10,J,Q,K]    = Just K
  | ys == [R10,J,Q,K,A]     = Just A
  | otherwise               = Nothing
  where ys = sort xs

-- given a hand checks whether the hand consists of straight cards or not.
-- this will be useful for seeing whether a hand is straight or not. 
isStraight' :: Hand -> Bool
isStraight' x = case isStraight (map rank (unHand x)) of
    Just _ -> True
    _      -> False

-- given a hand returns the ranks of the cards in hand in decsending order. 
ranks :: Hand -> [Rank]
ranks x = reverse (sort (map rank (unHand x)))

order :: Hand -> [(Int, Rank)]
order x = reverse ( sort (order' (ranks x) 0))

order' :: [Rank] -> Int -> [(Int,Rank)]
order' []  _ = []
order' [x] num = [(num+1,x)]
order' (x:y:xs) num 
  | y == x = order' (y:xs) (num+1)
  | y /= x = (num+1, x) : order' (y:xs) 0

-- given a hand categorizes them using the predefined functions. 
handCategory :: Hand -> HandCategory
handCategory x
  | sameSuits x && isStraight' x = case isStraight (ranks x) of
                                   Just R5 -> StraightFlush b
                                   _       -> StraightFlush a
  | a == b && b == c && c == d   = FourOfAKind a e
  | b == c && c == d && d == e   = FourOfAKind e a
  | a == b && b == c && d == e   = FullHouse a d
  | a == b && c == d && d == e   = FullHouse c a
  | sameSuits x                  = Flush (ranks x)
  | isStraight' x                = case isStraight (ranks x) of
                                   Just R5 -> Straight b
                                   _       -> Straight a
  | a == b && b == c             = ThreeOfAKind a d e
  | b == c && c == d             = ThreeOfAKind b a e
  | c == d && d == e             = ThreeOfAKind c a b 
  | a == b && c == d             = TwoPair a c e 
  | b == c && d == e             = TwoPair b e a
  | a == b && d == e             = TwoPair a e c
  | a == b                       = OnePair a [c,d,e]
  | b == c                       = OnePair b [a,d,e]
  | c == d                       = OnePair c [a,b,e]
  | d == e                       = OnePair d [a,b,c]
  | otherwise                    = HighCard (ranks x)
  where [a,b,c,d,e]= ranks x

-- now to compare two hands we use the above function. 
instance Ord Hand where
    compare (Hand x) (Hand y) = compare (handCategory (Hand x)) (handCategory (Hand y))

-- the combination function. We can think of it this way:
-- the C(0,_) will be 1 meaning that picking zero from anything will result to 1. [[]]
-- the second line means that picking anything (except from zero) from an empty list will be 0. []
-- and finally if we have a list which has an object x, then each group of n elements from
-- the list will be of two states: 1- consists of x 2- does not consist of x.
-- if it does have x the number of these subgroups is equal to picking (n-1) elements from the rest of the list.
-- if it does not have x the number of these subgroups is equal to picking n elements from the rest of the list.
-- the total will be the sum of these two subgroups. This way we have defined our recursion.
combs :: Int -> [a] -> [[a]]
combs 0 _ = [[]]
combs _ [] = []
combs n (x:xs) = map (x:) (combs (n-1) xs) ++ combs n xs

-- given a deck gives out the list of all possible hands.
allHands :: Deck -> [Hand]
allHands xs = map Hand (combs 5 xs)

-- given a deck returns a set of all possible hands. 
-- The only difference with the above function is that it does not include repetitive hands.
distinctHands :: Deck -> Set Hand
distinctHands xs = foldr insert empty (allHands xs)