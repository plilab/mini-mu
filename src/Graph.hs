module Graph (graphMain) where

import Data.GraphViz
import qualified Data.GraphViz.Attributes.Complete as A
-- import qualified Data.GraphViz.Types as T

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text.Lazy (pack)
import Eval (Config, step)

graphMain :: IO ()
graphMain = do
  let dotGraph = graphElemsToDot nonClusteredParams nodes edges
      nodes :: [(Int, [A.Attribute])]
      nodes =
        [ (1, [A.Label $ A.StrLabel $ pack "Node 1"]),
          (2, [A.Label $ A.StrLabel $ pack "Node 2"]),
          (3, [A.Label $ A.StrLabel $ pack "Node 3"])
        ]
      edges :: [(Int, Int, [A.Attribute])]
      edges = [(1, 2, [A.Label $ A.StrLabel $ pack "Edge 1->2"])]
  print $ printDotGraph dotGraph
  addrs <- runGraphvizCommand Dot dotGraph Svg "graph.svg"
  print addrs

type EvalGraph = Map.Map Config (Int, String)

buildEvaluationGraph :: Config -> (EvalGraph, Int)
buildEvaluationGraph initialConfig =
  let initialGraph = Map.singleton initialConfig (0, showConfig initialConfig)
   in explore initialGraph initialConfig 0 Set.empty
  where
    explore :: EvalGraph -> Config -> Int -> Set.Set Config -> (EvalGraph, Int)
    explore graph current nodeCount visited
      | Set.member current visited = (graph, nodeCount) -- Already explored
      | otherwise =
          let nextConfigs = step current
              visited' = Set.insert current visited

              -- Add new nodes and continue exploration
              (graph', nodeCount', visited'') =
                foldl
                  ( \(g, nc, vs) next ->
                      if Map.member next g
                        then (g, nc, vs) -- Node exists
                        else
                          let newId = nc + 1
                              g' = Map.insert next (newId, showConfig next) g
                              (g'', nc'') = explore g' next newId vs
                           in (g'', nc'', Set.insert next vs)
                  )
                  (graph, nodeCount, visited')
                  nextConfigs
           in (graph', nodeCount')

showConfig :: Config -> String
showConfig = show

generateEdges :: EvalGraph -> [(Int, Int, [A.Attribute])]
generateEdges graph =
  let configs = Map.keys graph
   in concatMap
        ( \src ->
            let (srcId, _) = graph Map.! src
                nextConfigs = step src
             in map
                  ( \dst ->
                      let (dstId, _) = graph Map.! dst
                       in (srcId, dstId, [])
                  )
                  nextConfigs
        )
        configs

evalGraphParams :: GraphvizParams Int String String () String
evalGraphParams =
  nonClusteredParams
    { globalAttributes = [GraphAttrs [A.RankDir A.FromLeft]],
      fmtNode = \(_, label) -> [A.Label $ A.StrLabel $ pack label],
      fmtEdge = \(_, _, _) -> []
    }