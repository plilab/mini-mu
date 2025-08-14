module Graph (visualizeEvalTree, generateTreeGraph) where

import Data.GraphViz
import qualified Data.GraphViz.Attributes.Complete as A

import Eval (Config(..))
import EvalTree (EvalTree(..), generateEvalTreeWithDepth)
import Pretty (renderPretty, prettyConfig)
import qualified Data.Text.Lazy as TL

-- | Generate a graph representation of an evaluation tree
generateTreeGraph :: Config -> Int -> Bool -> ([(Int, String)], [(Int, Int, String)])
generateTreeGraph initialConfig maxDepth _show =
  let tree = generateEvalTreeWithDepth maxDepth initialConfig
      (nodes, edges, _) = treeToGraph tree 0 _show
      edgesWithLabels = map (\(src, dst) -> (src, dst, "")) edges
  in (nodes, edgesWithLabels)

-- | Convert EvalTree to nodes and edges for GraphViz
treeToGraph :: EvalTree -> Int -> Bool -> ([(Int, String)], [(Int, Int)], Int)
treeToGraph (EvalNode config children) nodeId _show =
  let configStr = configToString config _show
      currentNode = (nodeId, configStr)
      (childNodes, childEdges, nextId) =
        foldl
          (\(nodes, edges, currentId) child ->
             let (childNodes', childEdges', newId) = treeToGraph child (currentId + 1) _show
                 newEdges = (nodeId, currentId + 1) : edges
             in (nodes ++ childNodes', newEdges ++ childEdges', newId)
          )
          ([], [], nodeId)
          children
  in (currentNode : childNodes, childEdges, nextId)

configToString :: Config -> Bool -> String
configToString config _show = renderPretty $ prettyConfig config _show

-- | Visualize an evaluation tree and save as SVG
visualizeEvalTree :: Config -> Int -> Bool -> String -> IO ()
visualizeEvalTree initialConfig maxDepth _show outputFile = do
  let (nodes, edges) = generateTreeGraph initialConfig maxDepth _show
      dotGraph = graphElemsToDot treeGraphParams nodes edges
  _ <- runGraphvizCommand Dot dotGraph Svg outputFile
  putStrLn $ "Evaluation tree saved to " ++ outputFile

-- | GraphViz parameters for tree visualization
treeGraphParams :: GraphvizParams Int String String () String
treeGraphParams =
  nonClusteredParams
    { globalAttributes = [GraphAttrs [A.RankDir A.FromTop]],
      fmtNode = \(_, label) -> [A.Label $ A.StrLabel $ TL.pack label, A.Shape A.BoxShape],
      fmtEdge = \(_, _, _) -> [A.Dir A.Forward]
    }