{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main
  ( drawFromDeck
  , Rank(..)
  , Suit(..)
  , MajorArcana(..)
  , Tarot(..)
  , randomMinorCard
  , randomMajorCard
  , randomTarot
  , main
  ) where

import Data.Char     (isLower)
import System.IO     (hFlush,stdout)
import System.Random.Stateful
import GHC.Generics  (Generic)

{- | 'Rank' defines the 14 possible ranks of the minor arcana,
 - 10 pip cards and 4 court/face cards.
 -}
data Rank
  = Ace   | Two    | Three | Four | Five
  | Six   | Seven  | Eight | Nine | Ten
  | Paige | Knight | Queen | King
  -- | Ranks are bounded, sequenced, finite, and have uniform distribution.
  -- Deriving these classes allow us to have type-level functions for getting random elements,
  -- a theme we'll repeat with other typeclasses.
  deriving (Bounded, Enum, Eq, Ord, Show, Generic, Finite, Uniform)

-- | 'Suit' defines the possible suits for the minor arcana.
data Suit = Wands | Cups | Swords | Pentacles
  deriving (Bounded, Enum, Eq, Show, Generic, Finite, Uniform)

-- | 'MajorArcana' defines the 22 major arcana.
data MajorArcana
  = TheFool        | TheMagician | TheHighPriestess | TheEmpress | TheEmperor
  | TheHierophant  | TheLovers   | TheChariot       | Strength   | TheHermit
  | WheelOfFortune | Justice     | TheHangedMan     | Death      | Temperance
  | TheDevil       | TheTower    | TheStar          | TheMoon    | TheSun
  | Judgement      | TheWorld
  deriving (Eq, Enum, Show, Bounded, Generic, Finite, Uniform)

{- | 'Tarot' has two kinds, for minor and major arcana, as well as a bool for
 - representing when the card is reversed.
 - 'Minor' has rank, suit, and reversed status.
 - 'Major' has the major arcana and the reversed status.
 -}
data Tarot
  = MinorCard Rank Suit Bool
  | MajorCard MajorArcana Bool
  deriving (Eq, Generic, Finite, Uniform)

{- | Enumeration rules for tarot cards:
 - Minor Arcana => 14 Ranks * 4 Suits * 2 Reversed states = 112 values
 - Major Arcana => 22 arcana * 2 Reversed states = 44 values
 - total: 156
 -}
instance Enum Tarot where
  toEnum index
    | index < 0 || 156 <= index = error "Out of bounds for Tarot cards"
    | index < 112 = MinorCard (toEnum rank) (toEnum suit) (toEnum reversed)
    | 112 <= index && index < 134 = MajorCard (toEnum arcana) False
    | otherwise = MajorCard (toEnum arcana) True
    where
      (index', rank) = index `divMod` 14
      (reversed, suit) = index' `divMod` 4
      arcana = (index - 112) `mod` 22

  -- | Shorcut the enumeration for minor and major cards separately to ease the
  -- math burden some.
  fromEnum (MinorCard rank suit reversed)
    = fromEnum rank
    + fromEnum suit * 14
    + fromEnum reversed * 56
  fromEnum (MajorCard arcana reversed)
    = 112
    + fromEnum arcana
    + fromEnum reversed * 22

  -- | Add the canonical boundaries to the enumerate commands to prevent errors
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y bound
    where
      bound | fromEnum y >= fromEnum x = maxBound
            | otherwise                = minBound

-- | Set the minimum and maximum bounds to those of the minor and major arcana cards, respectively
instance Bounded Tarot where
  minBound = MinorCard minBound minBound minBound
  maxBound = MajorCard maxBound maxBound

-- | Show instance of 'Tarot' for pretty printing.
instance Show Tarot where
  show (MinorCard rank suit False) = show rank ++ " of " ++ show suit
  show (MinorCard rank suit True)  = show rank ++ " of " ++ show suit ++ " (reversed)"
  show (MajorCard arcana isReversed)
    | isReversed = words' (show arcana) ++ " (reversed)"
    | otherwise  = words' (show arcana)
    where
      -- | words' is a helper function that will separate arcana into words
      -- wherever there's a capital letter
      words' [] = []
      words' (capital:rest)
        = (capital : takeWhile isLower rest)
        ++ case dropWhile isLower rest of
            "" -> ""
            nextCapital' -> ' ' : words' nextCapital'

{- | 'randomTarot' generates a random tarot card, either Minor or Major Arcana,
 - with equal probability. The rank, suit, or card, and orientation (reversed or not)
 - are chosen randomly.
 -}
randomTarot :: IO Tarot
randomTarot = uniformM globalStdGen

{- | 'randomMinorCard' generates a random Minor Arcana tarot card,
 - choosing randomly among all possible ranks, suits, and orientations (reversed or not).
 -}
randomMinorCard :: IO Tarot
randomMinorCard = uniformEnumRM (MinorCard minBound minBound minBound, MinorCard maxBound maxBound maxBound) globalStdGen

{- | 'randomMajorCard' generates a random Major Arcana tarot card,
 - choosing randomly among all possible cards and orientations (reversed or not).
 -}
randomMajorCard :: IO Tarot
randomMajorCard = uniformEnumRM (MajorCard minBound minBound, MajorCard maxBound maxBound) globalStdGen

{- | 'drawCards' generates a list of randomly chosen tarot cards.
 - The length of the list is determined by the input argument.
 - This function doesn't care about duplicates; for that, see `drawFromDeck`.
 -}
drawCards :: Int -> IO [Tarot]
drawCards num = uniformListM num globalStdGen

{- | 'shuffle' is a naive implementation of a shuffling function for a list.
 - It takes the input list and "cuts" it at random points, adding it to the end.
 - It does this for the lenght of the list.
 -}
shuffle :: [a] -> IO [a]
shuffle []  = return []
shuffle [x] = return [x]
shuffle xs = do
    i <- randomRIO (0, length xs - 1)
    let (ys, zs) = splitAt i xs
    rest <- shuffle (ys ++ drop 1 zs)
    return $ (xs !! i) : rest

{- | `drawFromDeck` generates a list of tarot cards as if drawn from a deck.
 - It creates a list of all possible cards, then `shuffle`s the list.
 - The input is the number of cards to draw from this deck.
 - The function produces an error on invalid inputs:
 -   1) an input greater than the size of the deck of all possible cards
 -   2) a number less than 1
 -}
drawFromDeck :: Int -> IO (Either String [Tarot])
drawFromDeck n
  | n < 0 = return $ Left "Cannot draw a negative number of cards"
  | n > length fullDeck = return $ Left "Cannot draw more cards than the size of the deck"
  | otherwise = do
      shuffledDeck <- shuffle fullDeck
      return $ Right $ take n shuffledDeck
  where
    fullDeck = enumFrom minBound :: [Tarot]

{- | 'ordinal' takes an integer and returns its ordinal representation as a string.
 - For example, 'ordinal 3' returns "3rd".
 -}
ordinal :: Int -> String
ordinal 11 = "11th"
ordinal 12 = "12th"
ordinal 13 = "13th"
ordinal index = show index ++ case (last $ show index) of
  '1' -> "st"
  '2' -> "nd"
  '3' -> "rd"
  _   -> "th"

{- | The 'main' function prompts the user for a number of cards to draw,
 - then generates and prints that many random tarot cards.
 - Each card is printed with its position in the draw (1st, 2nd, etc.) and its details.
 -}
main :: IO ()
main = do
  putStr "How many cards shall I draw? "
  hFlush stdout
  numberOfCards <- readLn :: IO Int
  result <- drawFromDeck numberOfCards
  case result of
    Left err    -> putStrLn $ "Error: " ++ err
    Right cards -> mapM_ printOrderCard $ zip [1..] cards
  where
    printOrderCard (index,card) = putStrLn $ ordinal index ++ ": " ++ show card
