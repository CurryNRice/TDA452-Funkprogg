module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random

------------------------------------------------------------
-- B6: 
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

jackOfhearts = Card Jack Hearts
twoOfClubs = Card (Numeric 2) Clubs
aceOfSpades = Card Ace Spades
exhand = (Add (twoOfClubs)
              (Add (jackOfhearts) 
                  (Add (aceOfSpades) Empty)))

exBustHand = Add jackOfhearts exhand

display :: Hand -> String
display Empty = ""
display (Add c h) = ['\n'] ++ displayCard c  ++ display h

displayCard :: Card -> String
displayCard (Card (Numeric r) c) = show r ++ " of " ++ show c
displayCard (Card r c) = show r ++ " of " ++ show c


------------------------------------------------------------

-- A2: Given a hand, there should be a function 
--     that calculates the value of the hand according 
--     to the rules given above:

{-
    A hint for writing the value function: 
    defining a few helper functions will be useful, 
    but it can be done in different ways:

    (Option 1) Define a function 

        initialValue :: Hand -> Integer

    that uses 11 for the value of aces, and a function 

        numberOfAces :: Hand -> Integer

    that can be used when computing the final value, 
    if the initial value is over 21.

    (Option 2) Define a function aceValue which takes the 
    value you want to use for the aces (1 or 11) as an extra argument. 
    Call this function to compute the initial value, and then call it 
    again to compute the final value, if the initial value is over 21.

    Regardless of which option you choose, 
    it will be useful to have a (local) helper function 
    to compute the values of a given rank:

        valueRank :: Rank -> Integer
-}

value :: Hand -> Integer
value Empty = 0
value (Add (Card r c) h) = if val > 21 then (val - 10*numberOfAces hand) else val 
  where 
    hand = (Add (Card r c) h)
    val = value' hand 0
  
  

value' :: Hand -> Integer -> Integer
value' Empty v                        = v
value' (Add (Card Ace c) h) v         = value' h (11+v) 
value' (Add (Card King c) h) v        = value' h (10+v) 
value' (Add (Card Queen c) h) v       = value' h (10+v) 
value' (Add (Card Jack c) h) v        = value' h (10+v) 
value' (Add (Card (Numeric n) c) h) v = value' h (n+v) 



numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card r c) h) 
              | r == Ace = 1 + numberOfAces h
              | otherwise = numberOfAces h


------------------------------------------------------------

-- A3: Given a hand, is the player bust?
gameOver :: Hand -> Bool
gameOver Empty = False
gameOver h = value h > 21

------------------------------------------------------------

-- A4: Given one hand for the guest and one for the bank (in that order), 
--     which player has won?
winner :: Hand -> Hand -> Player
winner h1 h2 | value h1 == 21 = Guest
             | gameOver h1 = Bank
             | gameOver h2 = Guest
             | value h1 > value h2 = Guest
             | otherwise = Bank


------------------------------------------------------------

-- B1: Given two hands, <+ puts the first one on top of the second one:
(<+) :: Hand -> Hand -> Hand
Empty <+ hy = hy
(Add c h) <+ hy = Add c (h <+ hy)

------------------------------------------------------------

-- B2: You also need to define a function that returns a full deck of cards
ranks = [Numeric 2, Numeric 3, Numeric 4, Numeric 5, Numeric 6, Numeric 7,
         Numeric 8, Numeric 9, Numeric 10, Jack, Queen, King, Ace]
suits = [Hearts, Spades, Diamonds, Clubs]
fullDeck :: Hand
fullDeck = fromListToHand [Card r s | s <- suits, r <- ranks]

fromListToHand :: [Card] -> Hand
fromListToHand [] = Empty
fromListToHand (c:cd) = Add c (fromListToHand cd)  
------------------------------------------------------------

-- B3: Given a deck and a hand, draw one card from the deck and put on the hand. Return both the deck and the hand (in that order). 
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty h = error "draw: The deck is empty."
draw (Add c rd) h = (rd, (Add c h))

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
                  (card, restHand) = takeCardByIndex x hand

takeCardByIndex :: Int -> Hand -> (Card, Hand)
takeCardByIndex n h = (card, (hHead <+ hTail))
          where 
            (Add card hTail) = dropCards n h 
            hHead = takeCards n h

takeCards :: Int -> Hand -> Hand
takeCards 0 _ = Empty
takeCards 1 (Add c h) = (Add c Empty)
takeCards n (Add c h) = Add c (takeCards (n-1) h)

dropCards :: Int -> Hand -> Hand 
dropCards 0 h = h
dropCards n (Add c h) = dropCards (n-1) h

