{-# LANGUAGE NoMonomorphismRestriction #-}

module Ust(
    bylines, bywords, showln, regexBool, uniq, rpt, 
    take', drop', head', tail', tail10, tac,
    rev, rev_w, wc_c, wc_l, wc_w, space, unspace,
    remove, upper, clean, clean', clean'',
    blank, join, tr, tr_d, grep, grep_v,
    cksum
) where

import Control.Monad.Instances
import Data.List
import Data.Char
import Data.Maybe
import Text.Printf
import System.Environment
import Text.Regex.Posix
 
-- First, three helpers
bylines f = (unlines . f . lines)
bywords f = (unwords . f . words)
showln  = (++ "\n") . show
 
regexBool r l = l =~ r :: Bool -- simple boolean regex matching
 
-- remove duplicate lines from a file (like uniq)
uniq    = nub   -- Warning: Unix uniq discards *consecutive* dupes,
                -- but 'nub' discards all dupes.
 
-- repeat the input file infinitely
rpt     = cycle
 
-- Return the head -10 line of a file
take'   = take 10
 
-- Remove the first 10 lines of a file
drop'   = drop 10
 
-- Return the head -1 line of a file
head'   = head
 
-- Return the tail -1 line of a file
tail'   = last
 
-- return the last ten lines of a file
tail10  = drop =<< subtract 10 . length
 
-- Reverse lines in a file (tac)
tac     = reverse
 
-- Reverse characters on each line (rev)
rev     = map reverse
 
-- Reverse words on each line
rev_w   = map (unwords . reverse . words)
 
-- Count number of characters in a file (like wc -c)
wc_c    = showln . length
 
-- Count number of lines in a file, like wc -l
wc_l    = showln . length . lines
 
-- Count number of words in a file (like wc -w)
wc_w    = showln . length . words
 
-- double space a file
space   = intersperse ""
 
-- undo double space
unspace = filter (not.null)
 
-- remove the first occurrence of the line "str"
remove  = delete
 
-- make a string all upper case
upper   = map toUpper
 
-- remove leading space from each line
clean   = map (dropWhile isSpace)
 
-- remove trailing whitespace
clean'  = map (reverse . dropWhile isSpace . reverse)
 
-- delete leading and trailing whitespace
clean'' = map (f . f)
    where f = reverse . dropWhile isSpace
 
-- insert blank space at beginning of each line
blank   = map (s ++)
     where s = replicate 8 ' '
 
-- join lines of a file
join = return . concat
 
-- Translate the letter 'e' to '*', like tr 'e' '*' (or y// in sed)
tr a b = interact (map f)
    where f c = if c == a then b else c
 
-- Delete characters from a string.
tr_d a = tr a ' '
 
-- lines matching the regular expression "[bf]oo" from a file
grep = filter (regexBool "[bf]oo")
 
-- lines not matching the regular expression "[bf]oo" from a file
grep_v  = filter (not . regexBool "[bf]oo")
 
-- number each line of a file
-- num  = zipWith (printf "%3d %s") ([1..] :: [Integer])
 
-- Compute a simple cksum of a file
cksum   =  foldl' k 5381
   where k h c = h * 33 + ord c
