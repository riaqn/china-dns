{-# LANGUAGE RankNTypes #-}
module Parse where

import Text.Parsec
import IPSet
import NameSet

import Data.ByteString.Char8 (pack)

import Data.Bits
import Data.Char
import Data.Functor.Identity
import Control.Monad

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

ip_line :: Line -> Parse s u (Maybe Range4)
ip_line i = do
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

name_line :: Line -> Parse s u (Maybe NAME)
name_line i = do
  pos <- getPosition
  setPosition (setSourceLine pos i)
  spaces'
  choice [ comment >> return Nothing
         , eof >> return Nothing
         , do
             r <- name
             spaces'
             eof
             return $ Just r
         ]

name :: Parse s u (NAME)
name = do
  l <- sepBy1 (many (alphaNum <|> char '-')) (char '.')
  return $ NAME $ map pack (foldr (:) [""] l)

dig :: Parse s u Int
dig = do
  l <- many1 digit
  return $ foldl (\a b -> a * 10 + b) 0 $ map digitToInt l

ipv4 :: Parse s u IPv4
ipv4 = do
    as <- dig `sepBy1` char '.'
    check as
    return $ IPv4 $ foldl (\a b -> (a `shift` 8) .|. (fromIntegral b)) 0 as
  where
    test adr = when (adr < 0 || 255 < adr) (parserFail "every componenent should be in [0, 256)")
    check as = do
        when (length as /= 4) $ parserFail "IPv4 should have 4 componenets"
        mapM_ test as
  
range4 :: Parse s u (Range IPv4)
range4 = do
  ip <- ipv4
  mlen <- option 32 $ char '/' >> dig
  when (mlen < 0 || mlen > 32) (parserFail "IPv4 mask length should be in [0, 32]")
  let mask = (1 `shift` (32 - mlen)) - 1
  return $ range (IPv4 $ (unIPv4 ip) .&. (complement mask)) (IPv4 $ (unIPv4 ip) .|. mask)

log_pair :: Parse s u (String, Priority)
log_pair = do
  name <- many (alphaNum <|> char '.')
  spaces
  void $ char '='
  spaces
  level <- many1 alphaNum
  spaces
  l <- case readMaybe level of
    Nothing -> error $ (show level) ++ " is not a log level"
    Just x -> return x
  return (name, l)
  
log_line :: Parse s u [(String, Priority)]
log_line = do
  x <- sepEndBy log_pair (char ',' >> spaces)
  eof
  return x
  
