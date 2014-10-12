{-# LANGUAGE TemplateHaskell #-}
module Main where

import AI.HNN.FF.Network
import Numeric.LinearAlgebra

import Control.Monad.Trans.State
import System.Random
import Control.Monad (replicateM, when)
import Control.Monad.IO.Class
import Control.Monad.Morph (hoist, generalize)
import System.Random.Shuffle (shuffleM)
import Control.Lens
import Data.Char (toLower)

type NW = Network Double
data SRP = Scissors | Rock | Paper deriving (Eq, Ord)

instance Show SRP where
    show Scissors = "S"
    show Rock     = "R"
    show Paper    = "P"

readSRP c = case toLower c of
    's' -> Just Scissors
    'r' -> Just Rock
    'p' -> Just Paper
    otherwise -> Nothing

winningMove :: SRP -> SRP
winningMove Scissors = Rock
winningMove Rock     = Paper
winningMove Paper    = Scissors

moveToProp :: SRP -> [Double]
moveToProp Scissors = [1,0,0]
moveToProp Rock     = [0,1,0]
moveToProp Paper    = [0,0,1]

bestFit :: [Double] -> (SRP, Double)
bestFit [s,r,p] | s >= r && s >= p = (Scissors, s)
                | r >= s && r >= p = (Rock, r)
                | p >= s && p >= r = (Paper, p)
                | otherwise = (Rock, 0.3333333333)

data GameStat = GameStat {
                    _wins   :: Int,
                    _losses :: Int,
                    _playerTries   :: [SRP],
                    _computerTries :: [SRP],
                    _network :: NW
                }

makeLenses ''GameStat

makeStat nw = GameStat 0 0 [Scissors, Rock, Paper, Scissors, Rock, Paper] [Scissors, Rock, Paper, Scissors, Rock, Paper] nw

type GameState = State GameStat

hist = 5

initSamp :: [[Double]]
initSamp = do
    poss <- replicateM hist [Rock, Paper, Scissors]
    return $ concatMap moveToProp poss

makeNet :: IO NW
makeNet = do
    nw <- createNetwork (hist*3) [250] 3
    let samp = [(fromList x, fromList [1/3, 1/3, 1/3]) | x <- initSamp]
        nw' = trainNTimes 10 0.5 tanh tanh' nw samp
    return nw'

getPlayerProp :: [SRP] -> [Double]
getPlayerProp = concatMap moveToProp . take hist

makeSample :: [SRP] -> SRP -> Sample Double
makeSample pt ct = (fromList pt', fromList ct')
    where
        pt' = getPlayerProp pt
        ct' = moveToProp ct

trainNet :: GameState ()
trainNet = do
    (pt:pts) <- use playerTries
    let ct = winningMove pt
    network %= \nw -> trainNTimes 10 0.5 tanh tanh' nw [makeSample pts ct]

getCompTry :: GameState (SRP, Double)
getCompTry = do
    nw <- use network
    pt <- use playerTries
    let ct  = bestFit $ toList $ output nw tanh $ fromList $ getPlayerProp pt
    computerTries %= (fst ct :)
    return ct

putPlayerTry :: SRP -> GameState ()
putPlayerTry pt = playerTries %= (pt:)


gameIter :: SRP -> GameState (SRP, Double)
gameIter pt = do
    ct <- getCompTry
    putPlayerTry pt
    trainNet

    when (fst ct == winningMove pt) $ wins %= (+1)
    when (pt == winningMove (fst ct)) $ losses %= (+1)

    return ct

playerFeed = concat $ replicate 200 [Rock, Paper, Scissors, Rock]

gameLoop :: StateT GameStat IO ()
gameLoop = do
    liftIO $ putStrLn "begin simulation"
    pf <- liftIO $ shuffleM playerFeed
    -- res <- mapM (hoist generalize . gameIter) playerFeed

    loop

    win <- use wins
    los <- use losses

    liftIO $ do
        putStrLn "Wins and losses:"
        putStrLn $ "Player " ++ show los ++ " Computer " ++ show win
        -- print $ playerTries st
        -- print $ computerTries st
  where loop :: StateT GameStat IO ()
        loop = do
            liftIO $ putStrLn "Your move please: R,P,S"
            choice <-liftIO getLine
            let ch' = readSRP $ head choice
            case ch' of
                Nothing -> return ()
                Just pt -> do
                    (ct, crt) <- (hoist generalize . gameIter) pt
                    win <- use wins
                    los <- use losses
                    liftIO . putStrLn $ "Computer tried " ++ show ct ++ " with certainty of " ++ show crt ++ " score: " ++ show los ++ ":" ++ show win
                    loop
main :: IO ()
main = do
    nw <- makeNet
    let gs = makeStat nw

    evalStateT gameLoop gs