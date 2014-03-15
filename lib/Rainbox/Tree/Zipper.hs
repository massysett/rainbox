module Rainbox.Tree.Zipper where

import Rainbox.Tree hiding (children)
import qualified Rainbox.Tree as T


data TreePos t a = Loc
  { content :: t a
  , before :: Forest a
  , after :: Forest a
  , parents :: [(Forest a, Box a, Forest a)]
  } deriving (Eq, Show)

class PosType t where
  prev :: TreePos t a -> Maybe (TreePos t a)
  next :: TreePos t a -> Maybe (TreePos t a)
  forest :: TreePos t a -> Forest a

data Empty a = Empty deriving (Eq, Show)

instance PosType Empty where
  prev = fmap prevSpace . prevTree
  next = fmap nextSpace . nextTree
  forest loc = foldl (flip (:)) (after loc) (before loc)

newtype Full a = Full { unFull :: Box a } deriving (Eq, Show)

instance PosType Full where
  prev = prevTree . prevSpace
  next = nextTree . nextSpace
  forest loc = foldl (flip (:)) (tree loc : after loc) (before loc)

prevSpace :: TreePos Full a -> TreePos Empty a
prevSpace loc = loc { content = Empty, after = tree loc : after loc }

prevTree :: TreePos t a -> Maybe (TreePos Full a)
prevTree loc = case before loc of
  t:ts -> Just loc { content = Full t, before = ts }
  [] -> Nothing

nextSpace :: TreePos Full a -> TreePos Empty a
nextSpace loc = loc { content = Empty, before = tree loc : before loc }

nextTree :: TreePos t a -> Maybe (TreePos Full a)
nextTree loc = case after loc of
  t:ts -> Just loc { content = Full t, after = ts }
  [] -> Nothing

parent :: TreePos t a -> Maybe (TreePos Full a)
parent loc = case parents loc of
  (ls,a,rs) : ps -> Just Loc
    { content = Full a
    , before = ls
    , after = rs
    , parents = ps
    }
  [] -> Nothing

root :: TreePos Full a -> TreePos Full a
root loc = maybe loc root (parent loc)

children :: TreePos Full a -> Maybe (TreePos Empty a)
children pos = case contents . unFull . content $ pos of
  Left _ -> Nothing
  Right c -> Just $ Loc
    { content = Empty
    , before = []
    , after = T.children c
    , parents = (before pos, unFull (content pos), after pos)
                  : parents pos
    }

tree :: TreePos Full a -> Box a
tree = unFull . content
