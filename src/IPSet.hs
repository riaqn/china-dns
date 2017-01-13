module IPSet
  ( module IPSet
  , IPv4(IPv4)
  ) where

import Resolve.DNS.Types (IPv4(IPv4))
import Data.IntervalMap.Strict (IntervalMap, Interval)
import qualified Data.IntervalMap.Strict as IM

type IPRange e = Interval e

range :: e -> e -> IPRange e
range = IM.ClosedInterval

type IPSet e = IntervalMap e ()

create :: IPSet e
create = IM.empty

add :: (Ord e) => IPSet e -> IPRange e -> IPSet e
add s e = IM.insert e () s

test :: (Ord e) => IPSet e -> e -> Bool
test s e = not $ IM.null $ IM.containing s e 

size :: IPSet e -> Int
size s = IM.size s


