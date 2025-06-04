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

import GHC.Generics  (Generic)
import System.IO     (hFlush, stdout, stdin, hIsTerminalDevice)
import System.Random.Stateful ( randomIO
                              , globalStdGen
                              , uniformShuffleListM
                              , Finite
                              , Uniform(..)
                              )

{- | 'Rank' defines the 14 possible ranks of the minor arcana,
 - 10 pip cards and 4 court/face cards.
 -}
data Rank
  = Ace   | Two    | Three | Four | Five
  | Six   | Seven  | Eight | Nine | Ten
  | Page  | Knight | Queen | King
  -- | Ranks are bounded, sequenced, finite, and have uniform distribution.
  -- Deriving these classes allow us to have type-level functions for getting random elements,
  -- a theme we'll repeat with other typeclasses.
  deriving (Bounded, Enum, Eq, Ord, Show, Generic, Finite, Uniform)

-- | 'Suit' defines the possible suits for the minor arcana.
data Suit = Wands | Cups | Swords | Pentacles
  deriving (Bounded, Enum, Eq, Show, Generic, Finite, Uniform)

-- | 'MinorArcana' combines Ranks and Suits to form its members.
data MinorArcana = MinorArcana
  { rank :: Rank
  , suit :: Suit
  } deriving (Bounded, Eq, Generic, Finite, Uniform)

instance Enum MinorArcana where
  fromEnum (MinorArcana rank suit) =
    (fromEnum rank + (fromEnum suit * 14))
  toEnum x
    | x < 0 || x >= 56 = error "index out of bounds"
    | x == 0 = MinorArcana minBound minBound
    | otherwise = MinorArcana rank suit
    where rank = toEnum $ x `mod` 14 :: Rank
          suit = toEnum $ x `div` 14 :: Suit

instance Show MinorArcana where
  show (MinorArcana rank suit) = (show rank) <> " of " <> (show suit)

-- | 'MajorArcana' defines the 22 major arcana.
data MajorArcana
  = TheFool        | TheMagician | TheHighPriestess | TheEmpress | TheEmperor
  | TheHierophant  | TheLovers   | TheChariot       | Strength   | TheHermit
  | WheelOfFortune | Justice     | TheHangedMan     | Death      | Temperance
  | TheDevil       | TheTower    | TheStar          | TheMoon    | TheSun
  | Judgement      | TheWorld
  deriving (Eq, Enum, Bounded, Ord, Generic, Finite, Uniform)

instance Show MajorArcana where
  show x = case x of
        TheFool -> "The Fool"
        TheMagician -> "The Magician"
        TheHighPriestess -> "The High Priestess"
        TheEmpress -> "The Empress"
        TheEmperor -> "The Emperor"
        TheHierophant -> "The Hierophant"
        TheLovers -> "The Lovers"
        TheChariot -> "The Chariot"
        Strength -> "Strength"
        TheHermit -> "The Hermit"
        WheelOfFortune -> "Wheel Of Fortune"
        Justice -> "Justice"
        TheHangedMan -> "The Hanged Man"
        Death -> "Death"
        Temperance -> "Temperance"
        TheDevil -> "The Devil"
        TheTower -> "The Tower"
        TheStar -> "The Star"
        TheMoon -> "The Moon"
        TheSun -> "The Sun"
        Judgement -> "Judgement"
        TheWorld -> "The World"

{- | 'Tarot' has two kinds, for minor and major arcana, as well as a bool for
 - representing when the card is reversed.
 - 'MinorCard' has MinorArcana and reversed status.
 - 'MajorCard' has MajorArcana and reversed status.
 -}
data Tarot where
  MinorCard :: MinorArcana -> Bool -> Tarot
  MajorCard :: MajorArcana -> Bool -> Tarot
  deriving (Eq, Generic, Finite, Uniform)

{- | Enumeration rules for tarot cards:
 - Minor Arcana => 14 Ranks * 4 Suits * 2 Reversed states = 112 values
 - Major Arcana => 22 arcana * 2 Reversed states = 44 values
 - total: 156
 - Tarot is enumerated by minor arcana, then major arcana, all upright.
 - Then repeats with cards in reversed status.
 -}
instance Enum Tarot where
  toEnum index
    | index < 0 || 156 <= index = error "Out of bounds for Tarot cards"
    | index < 56   = MinorCard minor False -- ^ [0-55): Minor arcana (upright)
    | index < 78   = MajorCard major False -- ^ [56-77): Major arcana (upright)
    | index < 134  = MinorCard minor True  -- ^ [78-133): Minor arcana (reversed)
    | 134 <= index = MajorCard major True  -- ^ [134-156): Major arcana (reversed)
    where
      minor = toEnum (index `mod` 78)
      major = toEnum $ (index `mod` 78) - 56

  -- | Shorcut the enumeration for minor and major cards separately to ease the
  -- math burden some.
  fromEnum (MinorCard arcana reversed)
    = fromEnum arcana
    + fromEnum reversed * 78
  fromEnum (MajorCard arcana reversed)
    = 56
    + fromEnum arcana
    + fromEnum reversed * 78

  -- | Add the canonical boundaries to the enumerate commands to prevent errors
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y bound
    where
      bound | fromEnum y >= fromEnum x = maxBound
            | otherwise                = minBound

-- | Set the minimum and maximum bounds to those of the minor and major arcana cards, respectively
instance Bounded Tarot where
  minBound = MinorCard minBound minBound
  maxBound = MajorCard maxBound maxBound

-- | Show instance of 'Tarot' for pretty printing.
instance Show Tarot where
  show (MinorCard arcana False) = show arcana
  show (MinorCard arcana True)  = show arcana ++ " (reversed)"
  show (MajorCard arcana False) = show arcana
  show (MajorCard arcana True)  = show arcana ++ " (reversed)"

{- | 'randomTarot' generates a random tarot card, either Minor or Major Arcana,
 - with equal probability. The rank, suit, or card, and orientation (reversed or not)
 - are chosen randomly.
 -}
randomTarot :: IO Tarot
randomTarot = uniformM globalStdGen

{- | 'randomMinorCard' generates a random Minor Arcana tarot card.
 -}
randomMinorCard :: IO Tarot
randomMinorCard = do
   arcana <- uniformM globalStdGen :: IO MinorArcana
   bool <- uniformM globalStdGen :: IO Bool
   return $ MinorCard arcana bool

{- | 'randomMajorCard' generates a random Major Arcana tarot card.
 -}
randomMajorCard :: IO Tarot
randomMajorCard = do
   arcana <- uniformM globalStdGen :: IO MajorArcana
   bool <- uniformM globalStdGen :: IO Bool
   return $ MajorCard arcana bool

{- | `drawFromDeck` generates a list of tarot cards as if drawn from a deck.
 - It creates a list of all possible cards, then `shuffle`s the list.
 - The input is the number of cards to draw from this deck.
 - The function produces an error on invalid inputs:
 -   1) an input greater than the size of the deck of all possible cards
 -   2) a number less than 1
 -}
drawFromDeck :: Int -> IO [Tarot]
drawFromDeck n = do
  -- | Generate a full deck of all upright cards
  let fullDeck = enumFromTo (MinorCard minBound False) (MajorCard maxBound False) :: [Tarot]
      flip :: Tarot -> IO Tarot
      flip (MajorCard arcana _) =  return . MajorCard arcana =<< (randomIO :: IO Bool)
      flip (MinorCard arcana _) =  return . MinorCard arcana =<< (randomIO :: IO Bool)
  -- | Shuffle them
  shuffledDeck <- uniformShuffleListM fullDeck globalStdGen
  -- | Randomly assign upright and reversed status
  results <- mapM flip shuffledDeck
  return $ take n results

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
  isTerminal <- hIsTerminalDevice stdin
  if isTerminal
  then do                
    -- | For standard input from terminal, ask for cards and flush output
    putStr "How many cards shall I draw? "
    hFlush stdout
  else do
    -- | For pipes, just flush the output
    hFlush stdout
  numberOfCards <- readLn :: IO Int
  cards <- drawFromDeck numberOfCards
  mapM_ printOrderCard $ zip [1..] cards
  where
    printOrderCard (index,card) = putStrLn $ ordinal index ++ ": " ++ show card
