module Codewars where

import qualified Data.List as List
import Data.Function
import qualified Data.Map as Map

type Node = Char
type Arc  = (Node, Node)

solveGraph :: Node -> Node -> [Arc] -> Bool
solveGraph s e arcs =
  let arcMap = toMap arcs in
  let nextNodes = Map.lookup s arcMap in
  recurseThroughArcs arcMap e nextNodes


toMap :: [Arc] -> Map.Map Node [Node]
toMap = List.foldl (\curMap (from, to) -> Map.insertWith (++) from [to] curMap) Map.empty

findTargets :: Node -> Map.Map Node [Node] -> Maybe [Node]
findTargets = Map.lookup


recurseThroughArcs :: Map.Map Node [Node] -> Node -> Maybe [Node] -> Bool
recurseThroughArcs arcMap end =
  maybe False (\nextNodes ->
                 end `List.elem` nextNodes || List.all (\node -> let ns = Map.lookup node arcMap in
                                                                          recurseThroughArcs arcMap end ns
                                                                          )   nextNodes
              )