module Scrod.Extra.Data where

import qualified Data.Data as Data
import qualified Data.List as List
import qualified GHC.Types.Name.Occurrence as OccName
import qualified Scrod.Extra.SrcLoc as SrcLoc

-- | Adapted from <https://hackage.haskell.org/package/syb-0.7.2.4>
-- and <https://chrisdone.com/posts/data-typeable/>.
showS :: (Data.Data a) => a -> ShowS
showS x
  | isTuple x =
      showParen True
        . foldr (.) id
        . List.intersperse (showString ", ")
        $ Data.gmapQ showS x
  | Just string <- Data.cast x = shows (string :: String)
  | isList x,
    [h, t] <- Data.gmapQ showS x =
      showParen True $ h . showString " : " . t
  | Just occName <- Data.cast x = showS $ OccName.occNameString occName
  | Just srcSpan <- Data.cast x = SrcLoc.srcSpanToShowS srcSpan
  | Just realSrcSpan <- Data.cast x = SrcLoc.realSrcSpanToShowS realSrcSpan
  | otherwise =
      let xs = Data.gmapQ ((showChar ' ' .) . showS) x
       in showParen (not $ null xs) $
            showString (Data.showConstr $ Data.toConstr x)
              . foldr (.) id xs

isTuple :: (Data.Data a) => a -> Bool
isTuple =
  all (== ',')
    . filter (\c -> c /= '(' && c /= ')')
    . Data.showConstr
    . Data.toConstr

isList :: (Data.Data a) => a -> Bool
isList = (==) "(:)" . Data.showConstr . Data.toConstr
