module BeatRoulette where

import Prelude
import Prelude (bind, (<$>),(<*>), ($), (>>=))

import Effect
import Data.Maybe
import Data.Maybe (fromMaybe)
import Data.Array ((..), head, cons,length)
import Data.Eq ((==))
import Control.Applicative (pure)
import Data.Functor (map)
import Effect.Random (randomInt)
import Data.Foldable (or)
import Test.Unit.Console (print)

type BetResult = {betCash::Int,tabValue::Int}

type Result = {cash::Int, tabValue::Int}

type EventResult = {cashEvents:: Array Int, tabValueEvents::Array Int}

initialCash :: Array Int
initialCash = [100]

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

bet :: Int -> Int -> Effect BetResult
bet cash tabValue = do
    win <- checkIfLandOnBet landNumber betCoverTabs
    pure $ moneyGotten win
    where
        moneyGotten :: Boolean -> BetResult
        moneyGotten win = case win of
            true -> {betCash: (9 * tabValue) - (6 * tabValue),tabValue:1}
            false -> {betCash: -(6 * tabValue),tabValue:tabValue*2}

play :: Array Int -> Array Int -> Maybe (Effect Result)
play cash tabValue = do
    c <- head cash
    tv <- head tabValue
    let b = bet c tv
    pure $ newCashValue b c
    where
        newCashValue :: Effect BetResult -> Int -> Effect Result
        newCashValue betEff lastCashValue = do
                            betResult <- betEff
                            pure $ {cash:lastCashValue + betResult.betCash,tabValue:betResult.tabValue}

unsafeGetPlay :: Array Int -> Array Int -> Effect Result
unsafeGetPlay initC initTV = fromMaybe (pure $ {cash: -123,tabValue: -321}) (play initC initTV)

xxx :: Array Int -> Array Int -> Effect EventResult
xxx cashs tabs = do
    result <- unsafeGetPlay cashs tabs
    if(result.cash > 0)
    then xxx (cons result.cash cashs) (cons result.tabValue tabs)
    else pure $ {cashEvents: cashs,tabValueEvents: tabs}










