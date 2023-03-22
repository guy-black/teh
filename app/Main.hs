module Main (main) where

import System.Environment         -- for getArgs
import Text.Read                  -- for readMaybe
import qualified Data.Map as M    -- for M.Map
import Data.Map ((!?))
import Data.List (sort, isPrefixOf)

main :: IO ()
main = do
  args <- getArgs
  arg <- getLine -- ooh this isn't getting all the lines of input
  putStrLn $ teh (args <+> arg) []

teh :: [String] -> [Change] -> String
teh [] [] = "whoosie doopsie run me with arguments or run teh -h for help" -- no arguments were passed
teh [x] [] =
  if x == "-h" then help
  else if x == "penguin" then pod
  else "whoosie doopsie run me with more than one argument or run teh -h for help" -- only one argument was passed
teh [x] xs = doChanges xs x -- base case, one argument left and changes to apply to it
teh [] xs = "whoops I have these changes to do but nothing to do them too " <> (show xs)
teh (x:xs) chgs =
  if x == "-h" then help
  else if x == "penguin" then pod
  else -- check if it can be read as a Change
    case (readMaybe x :: Maybe Change) of
      Just ch -> teh xs $ chgs <+> ch -- save this Change in a list and recurse until the last argument
      Nothing ->  -- check for custom macro
        case (chgMacros !? x) of
          Nothing -> -- macro not found
            x <> " not recognized as command"
          Just macchgs -> -- macro found, add to list of changes and recurse
            teh xs (chgs <> macchgs)

doChanges :: [Change] -> String -> String
doChanges [] txt = txt
doChanges (x:xs) txt = doChanges xs $ doChange x txt

doChange :: Change -> String -> String
doChange (Ch Whole wht) txt = -- applying change to whole blob of text
  case wht of
    Ins tx n ->
      if n >= 0 then -- counting forward
        (take n txt) <> tx <> (drop n txt)
      else -- counting backward
        (dropEnd ((abs n)-1) txt) <> tx <> (takeEnd ((abs n)-1) txt)
    Rem a b ->
      if b==0 then
        txt -- delete nothing
      else if a >= 0 then -- counting foward for skipped letters
        if b > 0 then -- deleting foward
          (take a txt) <> (drop (a+b) txt)
        else -- deleting back
          (dropEnd (abs b) (take a txt)) <> drop a txt
      else -- counting back for skipped letters
        if b > 0 then -- deleting foward
          (dropEnd (abs a) txt) <> (drop b (takeEnd (abs a) txt))
        else -- deleting back
          dropEnd ((abs a)+(abs b)) txt <> (takeEnd (abs a) txt)
    Fr find repl ->
      frfr find repl txt

doChange (Ch Each wht) txt =
  unlines ( map (doChange (Ch Whole wht)) (lines txt))

doChange (Ch (Only ns) wht) txt =
  let idxLns = zip [1..] (lines txt) in -- indexed list of lines
    unlines $ map snd(mapIf (\(x,y)-> (x,(doChange (Ch Whole wht) y))) (\(x,_)-> x `elem` ns) idxLns)

data Change = Ch Which What
  deriving (Read, Show)

data Which = Whole
           | Each
           | Only [Int]
  deriving (Read, Show)

data What = Fr String String
          | Ins String Int
          | Rem Int Int
  deriving (Read, Show)

(<+>) :: [a] -> a -> [a]
(<+>) xs x = xs<>[x]

pod :: String
pod =
 "hi every1 im new!!!!!!! *holds up spork* my name is katy but u can call me t3h PeNgU1N oF d00m!!!!!!!! lol…as u can see im very random!!!! thats why i came here, 2 meet random ppl like me ^_^… im 13 years old (im mature 4 my age tho!!) i like 2 watch invader zim w/ my girlfreind (im bi if u dont like it deal w/it) its our favorite tv show!!! bcuz its SOOOO random!!!! shes random 2 of course but i want 2 meet more random ppl =) like they say the more the merrier!!!! lol…neways i hope 2 make alot of freinds here so give me lots of commentses!!!!\n\
\DOOOOOMMMM!!!!!!!!!!!!!!!! <--- me bein random again ^_^ hehe…toodles!!!!!\n\
\\n\
\love and waffles,\n\
\\n\
\t3h PeNgU1N oF d00m"

help :: String
help = "I'll get to it"

chgMacros :: M.Map String [ Change ]
chgMacros = M.empty

takeEnd :: Int -> [a] -> [a]
takeEnd n  = (reverse . take n . reverse)

dropEnd :: Int -> [a] -> [a]
dropEnd n  = (reverse . drop n . reverse)

droplen :: [a] -> [a] -> [a]
droplen stub whole = drop (length stub) whole

frfr :: Eq a => [a] -> [a] -> [a] -> [a]
frfr _ _ [] = []
frfr find repl xss@(x:xs) =
  if find `isPrefixOf` xss then
    repl <> (frfr find repl $ droplen find xss)
  else
    x:(frfr find repl xs)

mapIf :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapIf _ _ [] = []
mapIf f b (x:xs) =
  if b x then
    (f x):(mapIf f b xs)
  else
    x : (mapIf f b xs)
