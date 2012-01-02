module Viterbi (

) where


import qualified Data.HashTable.IO as M
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Data.Maybe
import Control.Monad
import System.IO.Unsafe


type Table k v = M.BasicHashTable k v
type ByteString = B.ByteString
type NestedMap = Table ByteString (Table ByteString Float)


data Vit = Vit { projections :: NestedMap
               , inherants   :: NestedMap
               , lastWord    :: Maybe ByteString
               } deriving Show


incTraining :: Vit -> ByteString -> IO Vit
incTraining vit str =
    let spl = B.splitWith splFunc str
    in newVit >>= \nv -> foldM mergeV nv spl >>= toPercent

    where splFunc :: Char -> Bool
          splFunc w = w == ' ' || w == '\n' || w == '\t'


toPercent :: Vit -> IO Vit
toPercent v =
    let proj = projections v
        inhe = inherants v
    in M.mapM_ toPercent' proj >> M.mapM_ toPercent' inhe >> return v

    where toPercent' :: (ByteString, Table ByteString Float) -> IO ()
          toPercent' (_, m) = let ttl = M.foldM (\acc (_, v) -> return $ acc + v) 0 m
                              in ttl >>= \t -> M.mapM_ (\(k, v) -> return $ (v / t) * 100) m


mergeV :: Vit -> ByteString -> IO Vit
mergeV v str =
    let spl = B.split '/' str
    in add (lastWord v) (spl !! 1) (projections v) >>= \a ->
       add (Just $ spl !! 0) (spl !! 1) (inherants v) >>= \b ->
       return $ Vit a b (Just $ spl !! 0)


add :: Maybe ByteString -> ByteString -> NestedMap -> IO NestedMap

add Nothing tag m = return m

add (Just wd) tag m
    | depthOfGiven m wd tag == 0 = M.new >>= \a -> M.insert a tag 1 >> M.insert m wd a >> return m
    | depthOfGiven m wd tag == 1 = m `M.lookup` wd >>= deJust >>= \a -> M.insert a tag 1 >> return m
    | depthOfGiven m wd tag == 2 = m `M.lookup` wd >>= deJust >>= \a -> a `M.lookup` tag >>= deJust >>= \i ->
                                  M.insert a tag (1 + i) >> return m

    where deJust :: Maybe a -> IO a
          deJust = return . fromJust


depthOfGiven :: NestedMap -> ByteString -> ByteString -> Int
depthOfGiven m word tag = unsafePerformIO $
    m `M.lookup` word >>= \res ->
    case res of
        Nothing -> return 0
        Just x  -> x `M.lookup` tag >>= \res1 ->
                  case res1 of
                      Nothing -> return 1
                      Just y  -> return 2


newVit :: IO Vit
newVit = M.new >>= \a -> M.new >>= \b -> return $ Vit a b Nothing


