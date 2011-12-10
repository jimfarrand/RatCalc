--
--
-- Copynext (c) Krasimir Angelov 2008.
-- Copynext (c) Iavor S. Diatchki 2008.
--
-- Some minor modifications Copyright (c) 2011 Jim Farrand
--
-- Generic zipper implementation for Data.Tree
--
--

module RatCalc.Data.GenericTree.Zipper
  ( TreePos
  , PosType, Empty, Full

  -- * Context
  , before, after, forest, tree, parents

  -- * Conversions
  , fromTree
  , fromForest
  , toForest
  , toTree

  -- * Moving around
  , parent
  , root
  , prevSpace, prevTree, prev, first, spaceAt
  , nextSpace, nextTree, next, last
  , children, firstChild, lastChild, childAt

  -- * Node classification
  , isRoot
  , isFirst
  , isLast
  , isLeaf
  , isContained
  , hasChildren

  -- * Working with the current tree
  , insert
  , delete
  , setTree
  , modifyTree
  -- , modifyLabel
  -- , setLabel
  ) where

import RatCalc.Data.GenericTree as GT
import Prelude hiding (last)

-- | A position within a 'Tree'.
-- The parameter 't' inidcates if the position is pointing to
-- a specific tree (if 't' is 'Full'), or if it is pointing in-between
-- trees (if 't' is 'Empty').
data TreePos t l b  = Loc
  { _content   :: t l b        -- ^ The currently selected tree.
  , _before    :: Forest l b
  , _after     :: Forest l b
  , _parents   :: [(Forest l b, b, Forest l b)]
  } deriving (Read,Show,Eq)


-- | Siblings before this position, closest first.
before         :: PosType t => TreePos t l b -> Forest l b
before          = _before

-- | Siblings after this position, closest first.
after          :: PosType t => TreePos t l b -> Forest l b
after           = _after

-- | The contexts of the parents for this position.
parents        :: PosType t => TreePos t l b -> [(Forest l b, b, Forest l b)]
parents         = _parents

-- | Position which does not point to a tree (e.g., it is between two trees).
data Empty l b    = E deriving (Read,Show,Eq)

-- | Position which points to a tree.
newtype Full l b  = F { unF :: GenericTree l b } deriving (Read,Show,Eq)


-- | Positions may be either 'Full' or 'Empty'.
class PosType t where
  _prev      :: TreePos t l b -> Maybe (TreePos t l b)
  _next      :: TreePos t l b -> Maybe (TreePos t l b)
  _forest    :: TreePos t l b -> Forest l b


instance PosType Full where
  _prev       = prevTree . prevSpace
  _next       = nextTree . nextSpace
  _forest loc = foldl (flip (:)) (tree loc : after loc) (before loc)

instance PosType Empty where
  _prev       = fmap prevSpace . prevTree
  _next       = fmap nextSpace . nextTree
  _forest loc = foldl (flip (:)) (after loc) (before loc)




-- XXX: We do this because haddock insist on placing methods
-- in the class...

-- | The sibling before this location.
prev    :: PosType t => TreePos t l b -> Maybe (TreePos t l b)
prev     = _prev

-- | The sibling after this location.
next     :: PosType t => TreePos t l b -> Maybe (TreePos t l b)
next      = _next

-- | All trees at this location
-- (i.e., the current tree---if any---and its siblings).
forest   :: PosType t => TreePos t l b -> Forest l b
forest    = _forest





-- Moving around ---------------------------------------------------------------

-- | The parent of the given location.
parent :: PosType t => TreePos t l b -> Maybe (TreePos Full l b)
parent loc =
  case parents loc of
    (ls,a,rs) : ps -> Just
      Loc { _content  = F (Branch a (forest loc))
          , _before   = ls
          , _after    = rs
          , _parents  = ps
          }
    [] -> Nothing


-- | The top-most parent of the given location.
root :: TreePos Full l b -> TreePos Full l b
root loc = maybe loc root (parent loc)

-- | The space immediately before this location.
prevSpace :: TreePos Full l b -> TreePos Empty l b
prevSpace loc = loc { _content = E, _after = tree loc : after loc }

-- | The tree before this location, if any.
prevTree :: TreePos Empty l b -> Maybe (TreePos Full l b)
prevTree loc =
  case before loc of
    t : ts -> Just loc { _content = F t, _before = ts }
    []     -> Nothing


-- | The space immediately after this location.
nextSpace :: TreePos Full l b -> TreePos Empty l b
nextSpace loc = loc { _content = E, _before = tree loc : before loc }


-- | The tree after this location, if any.
nextTree :: TreePos Empty l b -> Maybe (TreePos Full l b)
nextTree loc =
  case after loc of
    t : ts -> Just loc { _content = F t, _after = ts }
    []     -> Nothing


-- | The location at the beginning of the forest of children.
children :: TreePos Full l b -> Maybe (TreePos Empty l b)
children loc =
    case GT.branchLabel (tree loc) of
        Nothing -> Nothing
        Just label ->
            Just
              Loc { _content  = E
                  , _before   = []
                  , _after    = subForest (tree loc)
                  , _parents  = (before loc, label, after loc)
                              : parents loc
                  }

-- | The first space in the current forest.
first :: TreePos Empty l b -> TreePos Empty l b
first loc = loc { _content  = E
                , _before   = []
                , _after    = reverse (before loc) ++ after loc
                }

-- | The last space in the current forest.
last :: TreePos Empty l b -> TreePos Empty l b
last loc = loc { _content = E
               , _before  = reverse (after loc) ++ before loc
               , _after   = []
               }

-- | The empty space at the given index.  The first space is at index 0.
-- For indexes that are negative or too large, we return the first and last
-- position in the tree, respectively.
spaceAt :: Int -> TreePos Empty l b -> TreePos Empty l b
spaceAt n loc = loc { _content = E
                    , _before  = reverse as
                    , _after   = bs
                    }
  where (as,bs) = splitAt n (forest loc)


-- | The first child of the given location.
firstChild :: TreePos Full l b -> Maybe (TreePos Full l b)
firstChild loc = children loc >>= nextTree

-- | The last child of the given location.
lastChild :: TreePos Full l b -> Maybe (TreePos Full l b)
lastChild loc = children loc >>= return . last >>= prevTree -- prevTree . last . children

-- | The child at the given index in the tree.
-- The first child is at index 0.
childAt :: Int -> TreePos Full l b -> Maybe (TreePos Full l b)
childAt n _ | n < 0 = Nothing
childAt n loc     = children loc >>= return . spaceAt n >>= nextTree -- nextTree . spaceAt n . children


-- Conversions -----------------------------------------------------------------

-- | A location corresponding to the root of the given tree.
fromTree :: GenericTree l b -> TreePos Full l b
fromTree t = Loc { _content = F t, _before = [], _after = [], _parents = [] }

-- | The location at the beginning of the forest.
fromForest :: Forest l b -> TreePos Empty l b
fromForest ts = Loc { _content = E, _before = [], _after = ts, _parents = [] }

-- | The tree containing this location.
toTree :: TreePos Full l b -> GenericTree l b
toTree loc = tree (root loc)

-- | The forest containing this location.
toForest :: PosType t => TreePos t l b -> Forest l b
toForest loc = case parent loc of
                 Nothing -> forest loc
                 Just p  -> toForest p -- polymprphic recursion


-- Queries ---------------------------------------------------------------------

-- | Are we at the top of the tree?
isRoot :: PosType t => TreePos t l b -> Bool
isRoot loc = null (parents loc)

-- | Are we the first position (of its kind) in a forest.
isFirst :: PosType t => TreePos t l b -> Bool
isFirst loc = null (before loc)

-- | Are we the last position (of its kind) in a forest.
isLast :: PosType t => TreePos t l b -> Bool
isLast loc = null (after loc)

-- | Are we at the bottom of the tree?
isLeaf :: TreePos Full l b -> Bool
isLeaf loc = null (subForest (tree loc))

-- | Do we have a parent?
isContained :: PosType t => TreePos t l b -> Bool
isContained loc = not (isRoot loc)

-- | Do we have children?
hasChildren :: TreePos Full l b -> Bool
hasChildren loc = not (isLeaf loc)


-- The current tree -----------------------------------------------------------


-- | The selected tree.
tree :: TreePos Full l b -> GenericTree l b
tree x = unF (_content x)

-- | The current label.
branchLabel :: TreePos Full l b -> Maybe b
branchLabel loc = GT.branchLabel (tree loc)

-- | Insert a new tree at the current position.
insert :: GenericTree l b -> TreePos Empty l b -> TreePos Full l b
insert t loc = loc { _content = F t }

-- | Remove the tree at the current position.
delete :: TreePos Full l b -> TreePos Empty l b
delete loc = loc { _content = E }



-- | Change the current tree.
setTree :: GenericTree l b -> TreePos Full l b -> TreePos Full l b
setTree t loc = loc { _content = F t }

-- | Modify the current tree.
modifyTree :: (GenericTree l b -> GenericTree l b) -> TreePos Full l b -> TreePos Full l b
modifyTree f loc = setTree (f (tree loc)) loc

-- | Modify the label at the current node.
-- modifyLabel :: (b -> b) -> TreePos Full l b -> TreePos Full l b
-- modifyLabel f loc = setLabel (f (label loc)) loc

-- | Change the label at the current node.
--setLabel :: b -> TreePos Full l b -> TreePos Full l b
--setLabel v loc = modifyTree (\t -> t { rootLabel = v }) loc


--------------------------------------------------------------------------------






