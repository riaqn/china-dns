{-# LANGUAGE RankNTypes #-}
module Parse where

import Text.Parsec
import IPSet

import Data.Bits
import Data.Functor.Identity
import Control.Monad

import Data.IP (addrRangePair, fromIPv4)

import Text.Read hiding (choice)

import System.Log.Logger

type Range4 = Range IPv4
type Parse s u a = (Stream s Identity Char) => Parsec s u a

comment :: Parse s u String
comment = do
  void $ char '#'
  manyTill anyChar (try eof)

spaces' :: Parse s u ()
spaces' =   void $ many $ oneOf " \t\f\v"

line :: Line -> Parse s u (Maybe Range4)
line i = do
  pos <- getPosition
  setPosition (setSourceLine pos i)
  spaces'
  choice [ comment >> return Nothing
         , eof >> return Nothing
         , do
             r <- range4
             spaces'
             eof
             return $ Just r
         ]
  
range4 :: Parse s u (Range IPv4)
range4 = do
  x <- manyTill anyChar (space)
  case readEither x of
    Left e -> error e
    Right r -> do
      let (ip, mlen) = addrRangePair r
      let ip' =  foldl (\b a -> (b `shift` 8) .|. fromIntegral a) 0 (fromIPv4 ip)
      let mask = (1 `shift` (32 - mlen)) - 1
      return $ range (IPv4 $ ip' .&. (complement mask)) (IPv4 $ ip' .|. mask)

logpair :: Parse s u (String, Priority)
logpair = do
  name <- many1 (alphaNum <|> char '.')
  spaces
  void $ char '='
  spaces
  level <- many1 alphaNum
  spaces
  l <- case readMaybe level of
    Nothing -> error $ (show level) ++ " is not a log level"
    Just x -> return x
  return (name, l)
  
logline :: Parse s u [(String, Priority)]
logline = do
  x <- sepEndBy logpair (char ',' >> spaces)
  eof
  return x
  
