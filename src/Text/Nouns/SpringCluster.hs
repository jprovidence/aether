module Text.Nouns.SpringCluster (
    springCluster
) where


import Data.Maybe
import qualified Data.List as L
import qualified Data.HashTable.IO as M
import Text.Nouns
import Text.Nouns.Graph
import Utility


springCluster :: IO ()
springCluster = do
    scc <- userConfigure
    (edges, verts) <- initializeSpringCluster scc

    putStrLn ">> Press any key to begin."
    _ <- getLine

    clusterCycle scc edges verts 0


userConfigure :: IO SpringClusterConfiguration
userConfigure = do
    putStrLn ">> Enter a dimension for the initial layout."
    dimen <- getLine >>= return . read

    putStrLn ">> Enter a figure for the spring rest distance."
    restdist <- getLine >>= return . read

    putStrLn ">> Entry a figure for the point of move symmetry."
    symm <- getLine >>= return . read

    putStrLn ">> What edge selection function would you like."
    putStrLn ">> 1 : No Filter."
    putStrLn ">> 2 : Less Than X."
    putStrLn ">> 3 : Greater Than X."
    putStrLn ">> 4 : Within N Deviations."
    selection <- getLine >>= return . read

    putStrLn ">> Enter a digit for X/N, or 0 for No Filter."
    xn <- getLine >>= return . read
    selFunc <- return $ case selection of
                           1 -> noScoreFilter
                           2 -> scLessThan xn
                           3 -> scGreaterThan xn
                           4 -> withinNDeviations xn

    scores <- allScores
    let avgScore = (fromIntegral $ L.foldl' (+) 0 scores) / (fromIntegral $ L.length scores)
    sprFunc <- return $ configureSpringFunc symm restdist avgScore

    putStrLn ">> Entry a figure for the repulsion force, or 'sym' to use the move symmetry point."
    input <- getLine
    force <- return $ case input of
                         "sym" -> symm
                         _     -> read input
    replFunc <- return $ configureRepulsionFunc (fromIntegral force) (fromIntegral symm)
                        (fromIntegral restdist) avgScore

    return $ SCC selFunc sprFunc replFunc dimen restdist




clusterCycle :: SpringClusterConfiguration -> Edges -> Vertices3D -> Int -> IO ()
clusterCycle scc edges verts count =
    case count == 100 of
        True -> do
            putStrLn ">> 100 cycles completed, continue? y/n"
            input <- getLine >>= return . read
            case affirmative input of
                True  -> clusterCycle scc edges verts 0
                False -> return ()
        False -> do
            newVerts <- M.new
            edgeList <- M.toList edges
            mapM_ (\edj -> reposition newVerts (_sprFunc scc) (_replFunc scc) edj verts) edgeList
            clusterCycle scc edges newVerts (count + 1)


reposition :: Vertices3D -> SpringFunc -> ReplFunc -> (Int, RelationSummary) -> Vertices3D -> IO ()
reposition nverts sfunc rfunc (id, summ) verts = do

    vtx <- M.lookup verts id >>= return . fromJust

    relateds <- mapM (\st -> M.lookup verts (target st) >>= return . fromJust) $ related summ
    unrelateds <- mapM (\ur -> M.lookup verts ur >>= return . fromJust) $ unrelated summ

    let strens = L.map strength $ related summ
    let targets = L.zip relateds strens

    tempPos <- return $ L.foldl' release vtx $ L.map (\(t, s) -> coil (sfunc) s t) targets
    finalPos <- return $ L.foldl' release tempPos $ L.map (coil' (rfunc)) unrelateds

    M.insert nverts id finalPos


release :: Point3D -> (Point3D -> Point3D) -> Point3D
release acc f = f acc


coil :: SpringFunc -> Int -> Point3D -> Point3D -> Point3D
coil func sc a b = (runS func) sc a b

coil' :: ReplFunc -> Point3D -> Point3D -> Point3D
coil' func a b = (runR func) a b


configureSpringFunc :: Int -> Int -> Float -> SpringFunc
configureSpringFunc symm restdist avgScore =
    SFunc $ buildSpringFunc (fromIntegral symm) (fromIntegral restdist) avgScore


buildSpringFunc :: Float -> Float -> Float -> Int -> Point3D -> Point3D -> Point3D
buildSpringFunc symm restdist avgScore score (tox, toy, toz) (frx, fry, frz) =
    let dist = sqrt (((frx - tox)^2) + ((fry - toy)^2) + ((frz - toz)^2))
        move = generateMove symm restdist dist avgScore score
        moveRatio = move / dist
        (movx, movy, movz) = (moveRatio * (tox - frx), moveRatio * (toy - fry), moveRatio * (toz - frz))
    in (frx + movx, fry + movy, frz + movz)


generateMove :: Float -> Float -> Float -> Float -> Int -> Float
generateMove symmetryPt restdist dist avgScore score =
    let m = restdist / (symmetryPt - restdist)
        b = 0 - (m * restdist)
    in ((m * dist) + b) * ((fromIntegral score) / avgScore)


configureRepulsionFunc :: Float -> Float -> Float -> Float -> ReplFunc
configureRepulsionFunc force symm restdist avgScore =
    RFunc $ buildRepulsionFunc force symm restdist avgScore


buildRepulsionFunc :: Float -> Float -> Float -> Float -> Point3D -> Point3D -> Point3D
buildRepulsionFunc force symm restdist avgScore (tox, toy, toz) (frx, fry, frz) =
    let dist = sqrt (((frx - tox)^2) + ((fry - toy)^2) + ((frz - toz)^2))
        move = generateMove symm restdist force avgScore 1
        moveRatio = move / dist
        (movx, movy, movz) = (moveRatio * (tox - frx), moveRatio * (toy - fry), moveRatio * (toz - frz))
    in case dist < (2 * restdist) of
           True -> (frx + movx, fry + movy, frz + movz)
           False -> (frx, fry, frz)

