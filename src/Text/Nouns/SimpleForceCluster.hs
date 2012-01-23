module Text.Nouns.SimpleForceCluster (
    simpleForceCluster
) where


import Data.Maybe
import qualified Data.List as L
import qualified Data.HashTable.IO as M
import Control.Concurrent
import Control.Monad
import Database.HDBC
import Database.HDBC.PostgreSQL
import Text.Nouns
import Text.Nouns.Visualization
import Text.Nouns.Graph
import Utility


----------------------------------------------------------------------------------------------------

-- Simple Force Cluster Algorithm.

-- Nouns are clustered based on attractive and repulsive forces. Force strengths are determined
-- by distance and the nature of the noun-noun relationship.

----------------------------------------------------------------------------------------------------

--

simpleForceCluster :: String -> IO ()
simpleForceCluster src = do
    (dimen, verts, edges) <- representGraph src
    case src of
        "user" -> do
            chan <- prepVisualOut
            runCluster' chan dimen src edges verts
        _ -> do
            return ()


----------------------------------------------------------------------------------------------------

--

runCluster' :: Chan Vertices -> Float -> String -> Edges -> Vertices -> IO ()
runCluster' chan dimen src edges verts =
    case src of
        "user" -> do
            (attrF, replF) <- userConfigureForces dimen
            config <- return $ CC attrF replF edges verts
            enterClusterCycle chan config 0
            stat <- confirmExit
            case stat of
                0 -> simpleForceCluster src
                1 -> runCluster' chan dimen src edges verts
                2 -> return ()

        _      -> do
            return ()


----------------------------------------------------------------------------------------------------

--

prepVisualOut :: IO (Chan (Table Int Point2D))
prepVisualOut = do
    chan <- newChan
    forkIO (waitVisualizationReq chan)
    return chan


----------------------------------------------------------------------------------------------------

--

userConfigureForces :: Float -> IO (ForceFunc, ForceFunc)
userConfigureForces dimen = do
    putStrLn ">> Enter a value for the max-move divisor."
    putStrLn (">> Note: Current cluster-space size is " ++ (show dimen) ++ ".")
    moveDivisor <- getLine >>= return . read

    moveMax <- return $ dimen / moveDivisor
    scMax <- scoreMax

    putStrLn ">> Enter a value for the repulsion threshold."
    repulThresh <- getLine >>= return . read

    modA <- buildForce "Attractive"
    modR <- buildForce "Repulsive"

    attrF <- return $ configAttractiveMod modA dimen moveMax scMax
    replF <- return $ configRepulsiveMod modR moveMax repulThresh
    return (attrF, replF)


----------------------------------------------------------------------------------------------------

--

buildForce :: String -> IO (Float -> Float -> Float)
buildForce str = do
    putStrLn (">> How would you like to modulate " ++ str ++ " Forces?")
    putStrLn ">> 1 : Greater with distance."
    putStrLn ">> 2 : Lesser with distance."
    partA <- getLine >>= return . read
    putStrLn (">> Which function type should be used to modulate " ++ str ++ " Forces?")
    putStrLn ">> 1 : Linear"
    putStrLn ">> 2 : Exponential"
    putStrLn ">> 3 : Fractinal exponential"
    partB <- getLine >>= return . read
    return $ case (partA, partB) of
                 (1, 1) -> linearGD
                 (1, 2) -> exponentialGD
                 (1, 3) -> fracExponentialGD
                 (2, 1) -> linearLD
                 (2, 2) -> exponentialLD
                 (2, 3) -> fracExponentialLD


----------------------------------------------------------------------------------------------------

--

confirmExit :: IO Int
confirmExit = do
    putStrLn ">> Enter one of the following numbers to continue."
    putStrLn ">> 0 : Run another cluster, update all cached data."
    putStrLn ">> 1 : Run another cluster, use existing cache."
    putStrLn ">> 2 : Exit"
    getLine >>= return . read


----------------------------------------------------------------------------------------------------

--

enterClusterCycle :: Chan Vertices -> ClusterConfiguration -> Int -> IO ()
enterClusterCycle chan config count =
     case count == 100 of
        True -> do
            putStrLn ">> 100 cycles completed, continue? y/n"
            input <- getLine
            case affirmative input of
                True  -> enterClusterCycle chan config 0
                False -> return ()

        False -> do
            let (e, v, a, r) = ((_edges config), (_verts config), (_attrF config), (_replF config))
            reportNewPositions chan v
            newVerts <- clusterCycle e v a r
            enterClusterCycle chan (CC a r e newVerts) (count + 1)


----------------------------------------------------------------------------------------------------

-- apply all forces to each vertex once

clusterCycle :: Edges -> Vertices -> ForceFunc -> ForceFunc -> IO Vertices
clusterCycle edges verts attrF replF = do
    newVerts <- M.new
    edgeList <- M.toList edges
    mapM_ (\edj -> reposition newVerts verts edj attrF replF) edgeList
    return newVerts


----------------------------------------------------------------------------------------------------

-- reposition a vertex according to the forces acting upon it

reposition :: Vertices -> Vertices -> (Int, RelationSummary) -> ForceFunc -> ForceFunc -> IO ()
reposition tbl verts (id, rcache) attrF repulF = do
    let cleanLookup x = liftM fromJust $ M.lookup verts x

    curPos <- cleanLookup id

    rels <- mapM (prepCache verts) $ related rcache
    unrels <- mapM cleanLookup (unrelated rcache) >>= \xs -> return $ L.zip xs $ repeat 0

    tempPos <- return $ L.foldl' release curPos $ L.map (coil attrF) rels
    finalPos <- return $ L.foldl' release tempPos $ L.map (coil repulF) unrels

    M.insert tbl id finalPos

    where prepCache :: Vertices -> Strength -> IO (Point2D, Int)
          prepCache tbl s = M.lookup tbl (target s) >>= \pt -> return (fromJust pt, strength s)

          coil :: ForceFunc -> (Point2D, Int) -> (Point2D -> Point2D)
          coil ff (pt, scr) = applyForce scr ff pt

          release :: Point2D -> (Point2D -> Point2D) -> Point2D
          release acc f = f acc


----------------------------------------------------------------------------------------------------

-- calculate the new position of a vertex

applyForce :: Int -> ForceFunc -> Point2D -> Point2D -> Point2D
applyForce score (ForceFunc ff) (xa, ya) (xb, yb) =
    let curDist = pythagoras (xa - xb) (ya - yb)
        move = ff score curDist
    in case move == 0 of
           True  -> (xb, yb)
           False -> makePt move (xa, ya) (ya, yb)

    where makePt :: Float -> (Float, Float) -> (Float, Float) -> Point2D
          makePt move (xa, ya) (xb, yb) =
              let (xz, yz) = ((move * (xa - xb)) + xb, (move * (ya - yb)) + yb)
              in (sanitize xz, sanitize yz)

          sanitize :: Float -> Float
          sanitize = lwrBnd . uprBnd

          uprBnd x = case (x > 1000.0) of
                           False -> x
                           True  -> 1000.0

          lwrBnd x = case (x < 0.0) of
                         False -> x
                         True  -> 0.0


----------------------------------------------------------------------------------------------------

-- configure a repulsive force function for easy passing

configRepulsiveMod :: (Float -> Float -> Float) -> Float -> Float -> ForceFunc
configRepulsiveMod f moveMax thresh = ForceFunc (modRepulsiveForce f moveMax thresh)


----------------------------------------------------------------------------------------------------

-- configure an attractive force function for easy passing

configAttractiveMod :: (Float -> Float -> Float) -> Float -> Float -> Int -> ForceFunc
configAttractiveMod f dimen moveMax scoreMax =
    ForceFunc (modAttractiveForce f dimen moveMax scoreMax)


----------------------------------------------------------------------------------------------------

-- modulate the repulsive move according to a linear function

modRepulsiveForce :: (Float -> Float -> Float) -> Float -> Float -> Int -> Float -> Float
modRepulsiveForce f moveMax thresh _ dist =
    let trespass = f dist thresh
    in case dist < thresh of
           False -> 0
           True  -> trespass * moveMax


----------------------------------------------------------------------------------------------------

-- modulate the attractive force on a vertex according to distance and the chosen mod function

modAttractiveForce :: (Float -> Float -> Float) -> Float -> Float -> Int -> Int -> Float -> Float
modAttractiveForce f dimen moveMax scoreMax score dist =
    let fscore = fromIntegral score
        fscoreMax = fromIntegral scoreMax
        naiveMove = (fscore / fscoreMax) * moveMax   -- move distance before modulation
        scHypotenuse = sqrt ((dimen^2) + (dimen^2))  -- greatest dist possible
        moveMultiplier = f dist scHypotenuse         -- apply modulating function

    in naiveMove * moveMultiplier


----------------------------------------------------------------------------------------------------

-- modulating function for attractive forces, linear

linearGD :: Float -> Float -> Float
linearGD a b = a / b


----------------------------------------------------------------------------------------------------

--

linearLD :: Float -> Float -> Float
linearLD a b = 1 - (linearGD a b)


----------------------------------------------------------------------------------------------------

-- modulating function for attractive forces, exponential

exponentialGD :: Float -> Float -> Float
exponentialGD a b = (a^2) / (b^2)


----------------------------------------------------------------------------------------------------

--

exponentialLD :: Float -> Float -> Float
exponentialLD a b = 1 - (exponentialGD a b)

----------------------------------------------------------------------------------------------------

-- modulating function for repulsive forces, opposite exponential

fracExponentialGD :: Float -> Float -> Float
fracExponentialGD a b = 1 - (fracExponentialLD a b)


----------------------------------------------------------------------------------------------------

--

fracExponentialLD :: Float -> Float -> Float
fracExponentialLD a b = (sqrt a) / (sqrt b)


----------------------------------------------------------------------------------------------------

-- determine the highest current relation score

scoreMax :: IO Int
scoreMax = let sel = "select score from edge order by score desc limit 1;"
               max = wrap (\c -> quickQuery' c sel [] >>= return . fromSql . flip (!!) 0 . L.concat)
           in  liftM fromJust max


----------------------------------------------------------------------------------------------------

--

reportNewPositions :: Chan (Table Int Point2D) -> Table Int Point2D -> IO ()
reportNewPositions chan tbl = writeChan chan tbl


----------------------------------------------------------------------------------------------------
