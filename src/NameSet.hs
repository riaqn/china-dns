module NameSet
  ( module NameSet
  , NAME(NAME)
  )
where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Resolve.DNS.Types
import Data.List (reverse)

newtype NameSet = NameSet (Maybe (HashMap LABEL NameSet))

create :: NameSet
create = NameSet $ Just $ HM.empty

add :: NameSet -> NAME -> NameSet
add (NameSet hm) (NAME n) = NameSet (add' hm $ reverse n)
                            where add' Nothing _ = Nothing
                                  add' (Just _) [] = Nothing
                                  add' (Just hm) (x : xs) = Just $ HM.alter
                                    (\v' -> case v' of
                                        Nothing -> Just $ NameSet $ add' (Just HM.empty) xs
                                        Just v -> case v of
                                          NameSet hm -> Just $ NameSet $ add' hm xs
                                    ) x hm

test :: NameSet -> NAME -> Bool
test (NameSet hm) (NAME n) = test' hm $ reverse n
                             where test' Nothing _ = True
                                   test' (Just _) [] = False
                                   test' (Just hm) (x : xs) = case HM.lookup x hm of
                                     Nothing -> False
                                     Just (NameSet m) -> test' m xs
