{-# LANGUAGE RankNTypes #-}
module Parse where

import Text.Parsec
import IPSet

import Data.Word
import Data.Bits
import Data.Functor.Identity
import Control.Monad

import Text.Read hiding (choice)

import System.Log.Logger

type Range4 = IPRange IPv4
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
  
int :: Parse s u Int
int = do
  many1 digit >>= return . read

ipv4 :: Parse s u Word32
ipv4 = do
  f <- int `sepBy1` (char '.')
  when (length f /= 4) $ error "IPv4 should be of 4 components"
  when (any (\x -> x < 0 || x >= 256) f) $ error "every components of IPv4 should be in [0, 256)"
  return $ foldl (\b a -> (b `shift` 8) .|. fromIntegral a) 0 f

range4 :: Parse s u (IPRange IPv4)
range4 = do
  addr <- ipv4
  option (range (IPv4 addr) (IPv4 addr)) $ do
    void $ char '/'
    mlen <- int
    when (mlen < 0 || mlen > 32) $
      error "mask length should be in [0, 32)"
    let mask = (bit $ 32 - mlen) - 1
    return $ range (IPv4 $ addr .&. (complement mask)) (IPv4 $ addr .|. mask)

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
  
