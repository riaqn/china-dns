module IPSet where

import Data.IntervalMap.Generic.Strict (IntervalMap, Interval)
import qualified Data.IntervalMap.Generic.Strict as IM


import Data.IP hiding (fromIPv4,toIPv4)
import Data.IP.Internal
import Data.Bits
import Data.Word

instance Interval (AddrRange IPv4) IPv4 where
  lowerBound i = let (a, b) = addrRangePair i
                 in toIPv4 $ (fromIPv4 a) .&. (complement ((bit $ 32 - b) - 1))
  upperBound i = let (a, b) = addrRangePair i
                 in toIPv4 $ (fromIPv4 a) .|. ((bit $ 32 - b) - 1)

type IPSet e = IntervalMap (AddrRange e) ()

fromIPv4 :: IPv4 -> Word32
fromIPv4 ip = case ip of
  IP4 a' -> a'

toIPv4 :: Word32 -> IPv4
toIPv4 ip = IP4 ip

create :: IPSet e
create = IM.empty

add :: Interval (AddrRange e) e => IPSet e -> AddrRange e -> IPSet e
add s e = IM.insert e () s

test :: Interval (AddrRange e) e => IPSet e -> e -> Bool
test s e = not $ IM.null $ IM.containing s e 

size :: IPSet e -> Int
size s = IM.size s
