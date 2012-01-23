module Text.Nouns (
    ByteString
,   Table
,   Chart
,   ForceFunc(ForceFunc, applyF)
,   ClusterConfiguration(CC, _attrF, _replF, _edges, _verts)
,   SpringClusterConfiguration(SCC, _selFunc, _sprFunc, _replFunc, _dimen, _restDist)
,   VertexCache
,   EdgeCache
,   DBStatus(Update, Create)
,   VertexData(VData, vid, vcount, vstatus)
,   EdgeData(EData, eid, escore, estatus)
,   VFunc(VFunc, runV)
,   EFunc(EFunc, runE)
,   Point2D
,   Point3D
,   Vertices
,   Vertices3D
,   Edges
,   Strength(Strength, target, strength)
,   RelationSummary(Summary, related, unrelated)
,   SpringFunc(SFunc, runS)
,   ReplFunc(RFunc, runR)
) where


import qualified Data.ByteString.Char8 as B
import qualified Data.HashTable.IO as M


----------------------------------------------------------------------------------------------------

-- Datatypes, Universal

----------------------------------------------------------------------------------------------------

type ByteString = B.ByteString
type Table k v = M.CuckooHashTable k v




----------------------------------------------------------------------------------------------------

-- Datatypes, Simple Force Cluster

----------------------------------------------------------------------------------------------------

-- typedefs

type Chart = Table ByteString [(ByteString, Int)]


----------------------------------------------------------------------------------------------------

--

newtype ForceFunc = ForceFunc { applyF :: (Int -> Float -> Float) }


----------------------------------------------------------------------------------------------------

--

data ClusterConfiguration = CC { _attrF :: ForceFunc
                               , _replF :: ForceFunc
                               , _edges :: Edges
                               , _verts :: Vertices
                               }




----------------------------------------------------------------------------------------------------

-- Datatypes, Document-Graph conversion

----------------------------------------------------------------------------------------------------


-- typedefs

type VertexCache = Table ByteString VertexData
type EdgeCache = Table ByteString (Table ByteString EdgeData)


----------------------------------------------------------------------------------------------------

-- data type to indicate whether a particular record must be updated or inserted to the database
-- upon completion of graph-conversion

data DBStatus = Update
              | Create
    deriving Show


----------------------------------------------------------------------------------------------------

-- data type to represent the state (or future state) of a given vertex in the database

data VertexData = VData { vid     :: Int
                        , vcount  :: Int
                        , vstatus :: DBStatus
                        } deriving Show


----------------------------------------------------------------------------------------------------

-- data type to represent the state (or future state) of a given edge in the database

data EdgeData = EData { eid     :: Int
                      , escore  :: Int
                      , estatus :: DBStatus
                      } deriving Show


----------------------------------------------------------------------------------------------------

-- function that will be partially applied with information regarding updates to a vertex. Will fire
-- when the VertexCache is in scope and can be applied

newtype VFunc = VFunc { runV :: (VertexCache -> IO ()) }


----------------------------------------------------------------------------------------------------

-- same as EFunc, for edge data instead. Note: all VFuncs generated during a conversion must be
-- fired before EFuncs will evaluate correctly

newtype EFunc = EFunc { runE :: (EdgeCache -> IO ()) }




----------------------------------------------------------------------------------------------------

-- Datatypes, Simple Force Cluster graph representation

----------------------------------------------------------------------------------------------------

-- typedefs

-- Point2D, a simple tuple to represent a point in a 2-d space.
-- Verticies

type Point2D = (Float, Float)
type Vertices = Table Int Point2D
type Edges = Table Int RelationSummary


----------------------------------------------------------------------------------------------------

-- data type to summarize a single relationship

data Strength = Strength { target   :: Int
                         , strength :: Int
                         } deriving Show


----------------------------------------------------------------------------------------------------

-- data type to summarize edges on a vertex

data RelationSummary = Summary { related  :: [Strength]
                               , unrelated :: [Int]
                               } deriving Show




----------------------------------------------------------------------------------------------------

-- Datatypes, Spring graph representation

----------------------------------------------------------------------------------------------------

-- typedefs

type Point3D = (Float, Float, Float)
type Vertices3D = Table Int Point3D


data SpringClusterConfiguration = SCC { _selFunc  :: ([Float] -> Int -> [String])
                                      , _sprFunc  :: SpringFunc
                                      , _replFunc :: ReplFunc
                                      , _dimen    :: Int
                                      , _restDist :: Int
                                      }


newtype SpringFunc = SFunc { runS :: (Int -> Point3D -> Point3D -> Point3D) }

newtype ReplFunc = RFunc { runR :: (Point3D -> Point3D -> Point3D) }


