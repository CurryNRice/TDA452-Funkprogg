module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

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
value hand = if val > 21 then (val - 10*numberOfAces hand) else val 
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
              | r == Ace = 1 + numberOfAces h
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