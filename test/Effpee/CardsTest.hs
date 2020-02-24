module Effpee.CardsTest (suite) where
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

    -- cardDeckFull tests
    , testCase "cardDeckFull (CardDeckNext (MkCard Spades Two) $ CardDeckNext (MkCard Spades Two) $ CardDeckEnd (MkCard Spades Two)) == False" $ False @=? cardDeckFull (CardDeckNext (MkCard Spades Two) <<< CardDeckNext (MkCard Spades Two) $ CardDeckEnd (MkCard Spades Two))
    -- TODO: same as above but for cardDeckFull

    -- cardDeckValid tests
    -- TODO: same as above but for cardDeckValid
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


propScoreCardIgnoresSuit = property $ do
  rank <- forAll genRank
  suit0 <- forAll genSuit
  suit1 <- forAll genSuit
  scoreCard (MkCard suit0 rank) === scoreCard (MkCard suit1 rank)
