module Markov where

import Data.List
import Data.Maybe
import Data.Monoid
import System.Random
import Text.PrettyPrint.HughesPJ hiding ((<>))

import Control.Monad.Trans.State
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Sequence as Q

sections :: Integer -> [a] -> [[a]]
sections n xs = zipWith const (map (genericTake n) (tails xs)) (genericDrop (n-1) xs)

counts :: (Ord k) => [k] -> M.Map k Integer
counts = foldl' (\m k -> M.alter (Just . (1 +) . fromMaybe 0) k m) M.empty

-- map from prefices to a sequence sorted by snd of key, cumulative count
newtype Markov k = Mkv (M.Map (Q.Seq k) (M.Map k Integer))
  deriving (Show)

instance (Ord k) => Monoid (Markov k) where
  mempty = Mkv M.empty
  mappend (Mkv ma) (Mkv mb) = Mkv (M.unionWith merge ma mb)
    where
      merge a b = toCumul (M.unionWith (+) (fromCumul a) (fromCumul b))
      toCumul = snd . M.mapAccum (\acc v -> (acc + v, acc + v)) 0
      fromCumul = snd . M.mapAccum (\acc v -> (v, v - acc)) 0

-- This is rubbish.
countsToMarkov :: (Ord k) => M.Map (Q.Seq k) Integer -> Markov k
countsToMarkov counts = Mkv $ M.foldrWithKey f M.empty counts
  where
    f k count m = case Q.viewr k of
      Q.EmptyR -> error "countsToMarkov: empty key"
      init Q.:> last -> M.alter (Just . g count last) init m
    g count last Nothing = M.singleton last count
    g count last (Just m) = case M.splitLookup last m of
      (less, this, greater) ->
        M.union less
          (M.map (+ count) (M.insert last (fromMaybe 0 this) greater))

fromCorpus :: (Ord k) => Integer -> [k] -> Markov k
fromCorpus len inp = countsToMarkov (counts (map Q.fromList (sections len inp)))

pickFromCumulMap :: (Ord k, Functor m) => (Integer -> m Integer)
  -> M.Map k Integer -> m (Maybe k)
pickFromCumulMap rng m = fmap k (rng total)
  where
    Just (total, _) = M.maxView m
    -- could binary search here
    k ix = fmap fst (find (\ (_, v) -> v >= ix) (M.toAscList m))

stepMarkov :: (Ord k) => Markov k -> (StdGen, Q.Seq k) -> Maybe (k, (StdGen, Q.Seq k))
stepMarkov (Mkv m) (rseed, mseed) = do
  cm <- M.lookup mseed m
  let pick = pickFromCumulMap (\bound -> state (randomR (1, bound))) cm
  case runState pick rseed of
    (Nothing, _) -> Nothing
    (Just x, rseed') -> Just (x, (rseed', Q.drop 1 mseed Q.|> x))

runMarkov :: (Ord k) => Markov k -> StdGen -> Q.Seq k -> [k]
runMarkov m rseed mseed = unfoldr (stepMarkov m) (rseed, mseed)

pretty :: (Show k) => Markov k -> String
pretty (Mkv m) = (render . vcat . map line) assoc
  where
    assoc = M.toList m
    tshow x = (text . show) x
    line (k, v) = tshow (F.toList k) <> nest keyWidth (tshow (M.toList v))
    keyWidth = foldl' max 0 (map (Q.length . fst) assoc)
