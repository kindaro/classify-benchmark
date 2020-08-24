module Projection where

import qualified Data.List as List
import Data.Function

classifyByProjection ∷ Ord π ⇒ (a → π) → [a] → [[a]]
classifyByProjection f = List.groupBy ((==) `on` f) . List.sortBy (compare `on` f)
