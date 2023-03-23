module Main (main) where

import System.Environment (getArgs)
import System.Directory (doesFileExist, XdgDirectory(XdgConfig), getXdgDirectory)
import Text.Read (readMaybe)
import qualified Data.Map as M    -- for M.Map
import Data.Map ((!?))
import Data.List (isPrefixOf)
import GHC.IO.StdHandles (stderr)
import GHC.IO.Handle (hPutStr)

main :: IO ()
main = do
  args' <- getArgs -- get args passed in
  confDir <- getXdgDirectory XdgConfig ".teh" -- get path for where the user .teh conffile would be if it exists
  (confErr, confMac) <- seekMacs confDir -- Tuple of String and Map String [Change] if it was read with no problem string will
  (localErr, localMac) <- seekMacs ".teh" -- be empty, if no file or parse error string will say that with empty map
  (args, b) <- rdFrmStdIn args' -- if last argument is a valid file name then replace it with the contents and/or if
  if b then do -- there is a nostdin flag passed remove it and set b to false to skip reading arg from stdin
    arg <- getContents
    case (teh (confMac <> localMac) (args <+> arg) []) of
      Left err -> putStdErr err
      Right succ -> putStr succ
  else
    case (teh (confMac <> localMac) args []) of
      Left err -> putStdErr err
      Right succ -> putStr succ

teh :: M.Map String [ Change ] -> [String] -> [Change] -> Either String String
teh _ [] [] = Left "whoosie doopsie run me with arguments or run teh -h for help" -- no arguments were passed
teh mac [x] [] =
  if x == "-h" then Right help
  else if x == "penguin" then Right pod
  else if x == "--listmacros" then Right (show mac) -- TODO pretty print this
  else Left "whoosie doopsie run me with more than one argument or run teh -h for help" -- only one argument was passed
teh _ [x] xs = Right $ doChanges xs x -- base case, one argument left and changes to apply to it
teh _ [] xs = Left $ "whoops I have these changes to do but nothing to do them too " <> (show xs)
teh mac (x:xs) chgs =
  if x == "-h" then Right help
  else if x == "penguin" then Right pod
  else if x == "--listmacros" then Right (show mac) -- TODO pretty print this
  else -- check if it can be read as a Change
    case (readMaybe x :: Maybe Change) of
      Just ch -> teh mac xs $ chgs <+> ch -- save this Change in a list and recurse until the last argument
      Nothing ->  -- check for custom macro
        case (mac !? x) of
          Nothing -> -- macro not found
            Left $ x <> " not recognized as command"
          Just macchgs -> -- macro found, add to list of changes and recurse
            teh mac xs (chgs <> macchgs)

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
  unlines $ map snd(mapIf (\(x,y)-> (x,(doChange (Ch Whole wht) y))) (\(x,_)-> x `elem` ns) (zip [1..] (lines txt)))
    -- this has to be one of the top ten ugliest lines of haskell i've ever written i love it so much

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

seekMacs :: FilePath ->  IO (String, (M.Map String [Change]))
seekMacs f = do
  exist <- doesFileExist f
  if exist then do
    file <- readFile f
    case readMaybe file::Maybe (M.Map String [Change]) of
      Just m -> return ("",m)
      Nothing -> return ("could not parse " <> f, M.empty)
  else
    return (f <> " not found", M.empty)

{--
    M.fromList [("indent"
              , [ Ch Each (Ins "  " 0)])
             ,("paren"
              , [ Ch Whole (Ins "("   0  )
                , Ch Whole (Ins ")" (-1))])
             ]
--}

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

putStdErr :: String -> IO()
putStdErr = hPutStr stderr

rdFrmStdIn :: [String] -> IO ([String], Bool)
rdFrmStdIn [] = return ([],False)
rdFrmStdIn sts = do
  if "--nostdin" `elem` sts then
    let (pre, post) = break ("--nostdin"==) sts in -- remove nostdin arg and don't read stdin for next arg
      return (pre <> (tail post),False)
  else do
    isfile <- doesFileExist (head $ takeEnd 1 sts)
    if isfile then do -- replace filename withe file contents and return list with False for no stdin
      file <- readFile (head $ takeEnd 1 sts)
      return ((dropEnd 1 sts)<+>file,False)
    else  -- return list of text unmodified with True to read last arg from stdin
      return (sts, True)
