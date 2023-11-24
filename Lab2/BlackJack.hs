module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random

implementation = Interface
  { iFullDeck = fullDeck
  , iValue    = value
  , iDisplay  = display
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffleDeck
  }

main :: IO () 
main = runGame implementation

------------------------------------------------------------
-- A0: 
{-
size hand2 computed by hand:
size hand2
  = size (Add (Card (Numeric 2) Hearts)
              (Add (Card Jack Spades) Empty))
  = 1 + size Add (Card Jack Spades) Empty)
  = 1 + 1 + size Empty
  = 2
-}
hand2 :: Hand
hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)

            

sizeSteps :: [Integer]
sizeSteps = [size hand2
            , size (Add (Card (Numeric 2) Hearts)
              (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size Empty
            , 2]

------------------------------------------------------------

-- A1: Implement a function that, given a hand,
--     shows the cards in it in a nice format.

display :: Hand -> String
display Empty = ""
display (Add c h) = displayCard c ++ ['\n'] ++ display h

displayCard :: Card -> String
displayCard (Card (Numeric r) c) = show r ++ " of " ++ show c
displayCard (Card r c) = show r ++ " of " ++ show c


------------------------------------------------------------

-- A2: Given a hand, there should be a function 
--     that calculates the value of the hand according 
--     to the rules of Blackjack.
value :: Hand -> Integer
value Empty = 0
value hand = if val > 21 then val - 10*numberOfAces hand else val 
  where 
    val = value' hand 0


  
  
-- Helper with an accumulator to recursively calculate the value of a hand.
value' :: Hand -> Integer -> Integer
value' Empty v                        = v
value' (Add (Card Ace c) h) v         = value' h (11+v) 
value' (Add (Card (Numeric n) c) h) v = value' h (n+v) 
value' (Add _ h) v                    = value' h (10+v)


-- Recursively calculates the number of aces in a hand.
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card r c) h) 
              | r == Ace  = 1 + numberOfAces h
              | otherwise = numberOfAces h


------------------------------------------------------------

-- A3: Given a hand, is the player bust?
gameOver :: Hand -> Bool
gameOver h | value h > 21 = True
           | otherwise    = False

------------------------------------------------------------

-- A4: Given one hand for the guest and one for the bank (in that order), 
--     which player has won?
winner :: Hand -> Hand -> Player
winner h1 h2 | gameOver h1         = Bank
             | gameOver h2         = Guest
             | value h1 > value h2 = Guest
             | otherwise           = Bank

------------------------------------------------------------

-- B1: Given two hands, <+ puts the first one on top of the second one:
(<+) :: Hand -> Hand -> Hand
Empty <+ hy     = hy
(Add c h) <+ hy = Add c (h <+ hy)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool 
prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1 <+ h2)

------------------------------------------------------------

-- B2: You also need to define a function that returns a full deck of cards
fullDeck :: Hand 
fullDeck = foldr (<+) Empty [Add (Card (Numeric n) s) Empty | n <- [2..10],        s <- suits] <+
           foldr (<+) Empty [Add (Card r s) Empty | r <- [Ace, Jack, Queen, King], s <- suits]
           where suits = [Diamonds, Spades, Hearts, Clubs]
------------------------------------------------------------

-- B3: Given a deck and a hand, draw one card from the deck and put on the hand. Return both the deck and the hand (in that order). 
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty h      = error "draw: The deck is empty."
draw (Add c rd) h = (rd, Add c h)

-- If the deck is empty, report an error using error:
-- error "draw: The deck is empty."

------------------------------------------------------------

-- B4: Given a deck, play for the bank according to the rules above (starting with an empty hand), and return the bank’s final hand:
-- The bank draws cards until its score is 16 or higher, and then it stops.
playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty

playBankHelper :: Hand -> Hand -> Hand
playBankHelper deck hand = if value biggerHand >= 16 then biggerHand else playBankHelper smallerDeck biggerHand
    where (smallerDeck,biggerHand) = draw deck hand

------------------------------------------------------------

-- B5: This is the hard question! It involves the use of a new type, 
-- StdGen, a random number generator which is explained below 
-- (but note that random number generation is not what makes this a harder assignment).

-- Given a StdGen and a hand of cards, shuffle the cards and return the shuffled hand:
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck _ Empty = Empty
shuffleDeck g hand=  Add card (shuffleDeck g' restHand)
            where                    
                  (x, g') = randomR (0, size hand - 1) g
                  (card, restHand) = takeCardByIndex Empty hand x
                  takeCardByIndex :: Hand -> Hand -> Int -> (Card, Hand)
                  takeCardByIndex checked (Add c rest) 0 = (c, checked <+ rest)
                  takeCardByIndex checked (Add c rest) n = takeCardByIndex (Add c checked) rest (n-1) 

--Property for shuffleDeck, if a deck has been shuffled, the shuffled deck contains the same cards.
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool 
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffleDeck g h
        where  belongsTo :: Card -> Hand -> Bool 
               c `belongsTo` Empty      = False 
               c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

--Property for shuffleDeck, perserving size of hand after shuffling it
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g hand = size hand == size (shuffleDeck g hand)