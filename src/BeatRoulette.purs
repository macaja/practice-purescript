module BeatRoulette where

import Prelude
import Prelude (bind, (<$>),(<*>), ($), (>>=))

import Effect
import Data.Maybe
import Data.Maybe (fromMaybe)
import Data.Array ((..), head, cons)
import Data.Eq ((==))
import Control.Applicative (pure)
import Data.Functor (map)
import Effect.Random (randomInt)
import Data.Foldable (or)
import Test.Unit.Console (print)

initialCash :: Array Int
initialCash = [50]

initialTabValue :: Array Int
initialTabValue = [1]

betCoverTabs :: Array (Array Int)
betCoverTabs = [[1,2,5,4],[2,3,6,5],[4,5,7,8],[5,6,8,9],[7,8,10,11],[8,9,11,12]]

landNumber :: Effect Int
landNumber = randomInt 1 37

checkIfLandOnBet :: Effect Int -> Array (Array Int) -> Effect Boolean
checkIfLandOnBet number myBet = do
        n <- number
        pure $ or $ mapArray n myBet
        where
            mapArray :: Int -> Array (Array Int) -> Array Boolean
            mapArray loanNumber myBet = do
                cover <- myBet
                pure $ checkArray loanNumber cover
                where
                    checkArray :: Int -> Array Int -> Boolean
                    checkArray loanNumber coverBet = do or $ (\n -> n == loanNumber) <$> coverBet

bet :: Int -> Int -> Effect Int
bet cash tabValue = do
    win <- checkIfLandOnBet landNumber betCoverTabs
    pure $ moneyGotten win
    where
        moneyGotten :: Boolean -> Int
        moneyGotten win = case win of
            true -> (9 * tabValue) - (6 * tabValue)
            false -> -(6 * tabValue)

play :: Array Int -> Array Int -> Maybe (Effect Int)
play cash tabValue = do
    c <- head cash
    tv <- head tabValue
    let b = bet c tv
    pure $ newCashValue b c
    where
        newCashValue :: Effect Int -> Int -> Effect Int
        newCashValue betEff lastCashValue = do
                            betValue <- betEff
                            pure $ lastCashValue + betValue

unsafeGetPlay :: Array Int -> Array Int -> Effect Int
unsafeGetPlay initC initTV = fromMaybe (landNumber) (play initC initTV)

xxx :: Array Int -> Array Int -> Effect (Array Int)
xxx cashs tabs = do
    result <- unsafeGetPlay cashs tabs
    if(result > 0)
    then xxx (cons result cashs) tabs
    else pure $ cashs


newTabValue :: Int -> Int -> Int
newTabValue bet tV = if (bet > 0)
                    then 1
                    else tV * 2











