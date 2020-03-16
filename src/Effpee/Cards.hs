module Effpee.Cards where

import Control.Monad (Monad)
import Data.Foldable (foldr)
import Data.List     (nub)
import Effpee
import GHC.Num

-- | An example of an "enum" type or simple "sum" type representing color of a suit
data Color
  = Red
  | Black
  deriving (Generic, Show, Eq, Bounded)

-- | Represents the suits in a deck of cards
-- This should include: clubs, diamonds, hearts, spades
data Suit
  -- | Clubs
  = Clubs
  | Diamonds
  | Hearts
  | Spades
  -- TODO: flesh out the rest of this "enum" type (also called "sum" types)
  deriving (Generic, Show, Enum, Eq, Bounded)

-- | Takes a value of `Suit` and produces the corresponding `Color`.
-- DONE: implement this function
evalColor :: Suit -> Color
evalColor Diamonds = Red
evalColor Hearts   = Red
evalColor Clubs    = Black
evalColor Spades   = Black

-- | Represents the rank of a card in a deck. The rank would be numeric or picture on
-- the card independent of suit or color.
-- DONE: Decide how you want to model this data as there are multiple encodings that
-- equivalent and have different trade-offs depending how you need to use this data.
-- There is no /one perfect way/ to encode this without the additional context of usage.
data Rank
  = Ace
  | King
  | Queen
  | Jack
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two
  deriving (Generic, Show, Enum, Eq, Bounded)

-- | Represents the card composed of the rank and suit.
-- DONE: Fill in the blanks
data Card
  = MkCard
    -- DONE: Uncomment the front of the following three lines and then fill in the types
    { cardSuit :: Suit -- ^ <- fill in the type here
    , cardRank :: Rank -- ^ <- fill in the type here
    }
    deriving (Generic, Show, Eq, Bounded)

-- NOTES: sometime we don't need to define a new enum, sum of products or product type.
-- We might just need to alias or wrap an existing type. Aliasing a type merely gives
-- better readability in Haskell source. There is no type checking differentiating
-- between the alias target or the alias. However, we might want to make sure that
-- a numeric type is wrapped up as a `Celsius` to distinguish at typechecking from
-- `Kelvin`. There are different trade-offs and you might need one over the other
-- for different purposes, even when pointing to the same alias target. Using `type`
-- gives us type aliasing (I wish the keyword was `alias` but Haskell has made lots
-- of legacy decision like every older language, so I forgive it). We can declare
-- a `newtype` as a way to "wrap" a type target and providing strong type checking
-- guarantees without run-time overhead.

-- | Represents a "hand" that a player has available to them at any time.
type Hand = [Card]

-- TODO: You can comment out the alias for the newtype at the end of all the exercises
-- to see if it compiles and if not, do the exercise of fixing all the type checking
-- errors. This will help inform you of the relative strengths of each approach
-- so you can choose the appropriate technique for your subsequent applications
-- of this.
-- newtype Hand = MkHand [Card]


-- NOTES: We sometimes have a need to define recursive structures. We could represent
-- a deck of cards as such a recursive structure representing the "head" of the deck.
-- data CardDeck
--   = CardDeckEnd Card
--   | CardDeckNext Card CardDeck
--   deriving (Generic, Show)

-- This is another way to represent a deck of cards by using a newtype wrapper.
newtype CardDeck
  = MkCardDeck        -- data constructor
  { unDeck :: [Card]  -- generates helper function called unDeck
  } deriving (Show)

-- Smart constructors
fullDeck :: CardDeck
fullDeck = [MkCard s r | s <- suits, r <- ranks]

ranks :: [rank]
ranks = enumFrom minBound

suits :: [suit]
suits = enumFrom minBound

-- | a function that consumes a `Card` value and produces its score.
-- We will assume for this exercise that Ace produces 13.
score :: Rank -> Int
score Ace   = 14
score King  = 13
score Queen = 12
score Jack  = 11
score Ten   = 10
score Nine  = 9
score Eight = 8
score Seven = 7
score Six   = 6
score Five  = 5
score Four  = 4
score Three = 3
score Two   = 2

scoreCard :: Card -> Int
scoreCard (MkCard _ rank) = score rank

-- | a function that, when given a `Hand`, will sum up the scores of each `Card` in
-- the `Hand``.
scoreHand :: Hand -> Int
scoreHand = foldr ((+) <<< scoreCard) 0

-- | a function that checks whether the deck of cards is complete in count.
-- i.e. it checks that there is exactly 52 cards in the deck given.
cardDeckFull :: CardDeck -> Bool
cardDeckFull deck = length deck == 52

-- | a function that checks whether the deck of cards has 52 unique cards such that
-- only cards from one deck of cards is present (i.e. we haven't mixed up cards from
-- two decks).
cardDeckValid :: CardDeck -> Bool
cardDeckValid deck = length (nub deck) == 52

-- TODO: Spotter needs to define and introduce the Random effect in the Effpee.Random
-- module first before we tackle this. Ignore for the first week.
shuffle :: Monad m => CardDeck -> m CardDeck
shuffle = todo "Effpee.Cards.shuffle"
