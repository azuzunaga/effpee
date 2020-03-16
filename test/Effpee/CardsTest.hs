module Effpee.CardsTest (suite) where
import           Data.List      (concat)
import           Data.Ord
import           Effpee
import           Effpee.Test
import           GHC.Enum       (enumFrom)
import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

-- module under test
import Effpee.Cards
suite
  = testGroup "Effpee.Cards"
    -- evalColor test cases
    [ testCase "evalColor Diamonds == Red" $ Red @=? evalColor Diamonds
    , testCase "evalColor Hearts == Red"   $ Red @=? evalColor Hearts
    , testCase "evalColor Clubs == Black"  $ Black @=? evalColor Clubs
    , testCase "evalColor Spades == Black" $ Black @=? evalColor Spades

    -- scoreCard tests
    , testCase "scoreCard (MkCard Diamonds Ace) == 14" $ 14 @=? scoreCard (MkCard Diamonds Ace)
    , testCase "scoreCard (MkCard Hearts Three) == 3" $ 3 @=? scoreCard (MkCard Hearts Three)
    , testProperty "Suit should not factor into scoreCard result" propScoreCardIgnoresSuit
    -- enough confidence that it works given your chosen representation for Card

    -- scoreHand tests
    -- TODO: same as above but for scoreHand
    -- , testProperty "The score for a hand of 7 cards should not go over 98" propScoreHandDoesntExceedValue
    , testCase "scoreHand [(MkCard Spades Two), (MkCard Clubs Ten)] == 12" $ 12 @=? scoreHand [MkCard Spades Two, MkCard Clubs Ten]
    , testProperty "The score for a hand of 7 cards should not go over 98" propScoreHand

    -- cardDeckFull tests
    -- , testCase "cardDeckFull (CardDeckNext (MkCard Spades Two) $ CardDeckNext (MkCard Spades Two) $ CardDeckEnd (MkCard Spades Two)) == False" $ False @=? cardDeckFull (CardDeckNext (MkCard Spades Two) <<< CardDeckNext (MkCard Spades Two) $ CardDeckEnd (MkCard Spades Two))
    , testProperty "Correctly identifies full decks" propValidatesFullDeck

    -- cardDeckValid tests
    , testProperty "Correctly identifies valid decks" propValidatesValidDeck
    ]

genSuit :: MonadGen m => m Suit
genSuit = Gen.choice (pure <$> [Clubs, Diamonds, Hearts, Spades])

genRank :: MonadGen m => m Rank
genRank = Gen.choice (pure <$> enumFrom Ace)

genCard :: MonadGen m => m Card
genCard = MkCard <$> genSuit <*> genRank
-- genCard = do
--   suit <- genSuit
--   rank <- genRank
--   pure (MkCard suit rank)

genHand :: MonadGen m => m Hand
genHand = Gen.list (Range.constant 1 7) genCard

propScoreHand = property $ do
  hand <- forAll genHand
  scoreHand hand `compare` 99 === LT

propScoreCardIgnoresSuit = property $ do
  rank <- forAll genRank
  suit0 <- forAll genSuit
  suit1 <- forAll genSuit
  scoreCard (MkCard suit0 rank) === scoreCard (MkCard suit1 rank)

genFullDeck :: MonadGen m => m CardDeck
genFullDeck = MkCardDeck <$> Gen.list (Range.constant 52 52) genCard
-- genFullDeck = pure fullDeck

genNotFullDeck :: MonadGen m => m CardDeck
genNotFullDeck = MkCardDeck <$> Gen.list (Range.constant 5 5) genCard

propValidatesFullDeck = property $ do
  deck <- forAll genFullDeck
  invalidDeck <- forAll genNotFullDeck
  cardDeckFull deck === True
  cardDeckFull invalidDeck === False

genValidDeck :: MonadGen m => m CardDeck
genValidDeck = pure fullDeck

genInvalidDeck :: MonadGen m => m CardDeck
genInvalidDeck = pure (MkCardDeck $ concat [ MkCard s r:[MkCard s r] | s <- suits, r <- ranks ])

propValidatesValidDeck = property $ do
  deck <- forAll genValidDeck
  invalidDeck <- forAll genFullDeck
  invalidDeck' <- forAll genInvalidDeck
  cardDeckValid deck === True
  cardDeckValid invalidDeck === False
  cardDeckValid invalidDeck' === False
