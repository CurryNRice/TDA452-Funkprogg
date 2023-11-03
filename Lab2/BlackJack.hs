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

jackOfhearts = Card Jack Hearts
twoOfClubs = Card (Numeric 2) Clubs
aceOfSpades = Card Ace Spades
exhand = (Add (twoOfClubs)
              (Add (jackOfhearts) 
                  (Add (aceOfSpades) Empty)))

display :: Hand -> String
display Empty = ""
display (Add c h) = displayCard c ++ ['\n'] ++ display h

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
value (Add (Card r c) h) = if val > 21 then (val - 13*numberOfAces hand) else val 
  where 
    hand = (Add (Card r c) h)
    val = value' hand 0
  
  

value' :: Hand -> Integer -> Integer
value' Empty v                        = v
value' (Add (Card Ace c) h) v         = value h (14+v) 
value' (Add (Card King c) h) v        = value h (13+v) 
value' (Add (Card Queen c) h) v       = value h (12+v) 
value' (Add (Card Jack c) h) v        = value h (11+v) 
value' (Add (Card (Numeric n) c) h) v = value h (n+v) 



numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card r c) h) 
              | r == Ace = 1 + numberOfAces h
              | otherwise = numberOfAces h


------------------------------------------------------------

-- A3: Given a hand, is the player bust?
gameOver :: hand -> Bool
gameOver h = undefined

------------------------------------------------------------

-- A4: Given one hand for the guest and one for the bank (in that order), 
--     which player has won?
winner :: Hand -> Hand -> Player
winner h1 h2 = undefined

------------------------------------------------------------

-- 