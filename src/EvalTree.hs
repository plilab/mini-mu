module EvalTree
  ( EvalTree(..)
  , generateEvalTree
  , printEvalTree
  , generateEvalTreeWithDepth
  ) where

import Eval (Config(..), step)
import qualified Data.Set as Set
import Pretty (renderPretty, prettyConfig)

-- | EvalTree represents the complete evaluation tree where each node
-- is a CommandConfig and branches represent all possible evaluation paths
data EvalTree = EvalNode Config [EvalTree]
  deriving (Show, Eq)

-- | Generate the complete evaluation tree from an initial configuration
-- This explores all possible evaluation paths, branching at each mu expression
generateEvalTree :: Config -> EvalTree
generateEvalTree = generateEvalTreeWithVisited Set.empty

-- | Generate evaluation tree with a maximum depth limit to avoid infinite trees
generateEvalTreeWithDepth :: Int  -> Config -> EvalTree
generateEvalTreeWithDepth maxDepth = generateEvalTreeWithDepthAndVisited maxDepth Set.empty

-- | Internal function that tracks visited configurations to handle cycles
generateEvalTreeWithVisited :: Set.Set Config -> Config -> EvalTree
generateEvalTreeWithVisited visited config
  | Set.member config visited = EvalNode config []  -- Cycle detected
  | otherwise =
      let nextConfigs = step config
          visited' = Set.insert config visited
          children = map (generateEvalTreeWithVisited visited') nextConfigs
      in EvalNode config children

-- | Internal function with depth limit and cycle detection
generateEvalTreeWithDepthAndVisited :: Int -> Set.Set Config -> Config -> EvalTree
generateEvalTreeWithDepthAndVisited depth visited config
  | depth <= 0 = EvalNode config []  -- Depth limit reached
  | Set.member config visited = EvalNode config []  -- Cycle detected
  | otherwise =
      let nextConfigs = step config
          visited' = Set.insert config visited
          children = map (generateEvalTreeWithDepthAndVisited (depth - 1) visited') nextConfigs
      in EvalNode config children

-- | Pretty print the evaluation tree with indentation
printEvalTree :: EvalTree -> Bool -> String
printEvalTree tree _show = printEvalTreeWithIndent 0 _show tree

-- | Internal function for pretty printing with indentation
printEvalTreeWithIndent :: Int -> Bool -> EvalTree -> String
printEvalTreeWithIndent indent _show (EvalNode config children) =
  let indentStr = replicate (indent * 2) ' '
      configStr = indentStr ++ configToString config _show
      childrenStr = concatMap (("\n" ++) . printEvalTreeWithIndent (indent + 1) _show) children
  in configStr ++ childrenStr

configToString :: Config -> Bool -> String
configToString config _show = renderPretty $ prettyConfig config _show