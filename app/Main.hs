{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getArgs)
import System.Directory (doesFileExist, XdgDirectory(XdgConfig), getXdgDirectory)
import Text.Read (readMaybe)
import qualified Data.Map as M    -- for M.Map
import Data.Map ((!?))
import Data.List (isPrefixOf)
import qualified Data.Text as T
import GHC.IO.StdHandles (stderr)
import GHC.IO.Handle (hPutStr)

main :: IO ()
main = do
  args' <- getArgs >>= (return . (map T.pack)) -- get args passed in
  confDir <- getXdgDirectory XdgConfig ".teh" >>= (return . T.pack) -- get path for where the user .teh conffile would be if it exists
  (confErr, confMac) <- seekMacs confDir -- Tuple of T.Text and Map T.Text [Change] if it was read with no problem string will
  (localErr, localMac) <- seekMacs ".teh" -- be empty, if no file or parse error string will say that with empty map
  (args, b) <- rdFrmStdIn args' -- if last argument is a valid file name then replace it with the contents and/or if
  if b then do -- there is a nostdin flag passed remove it and set b to false to skip reading arg from stdin
    arg <- getContents >>= (return . T.pack)
    case (teh ((confErr, confMac), (localErr, localMac)) (args <+> arg) []) of
      Left err -> putStdErr err
      Right succ -> putStr (T.unpack succ)
  else
    case (teh ((confErr, confMac), (localErr, localMac)) args []) of
      Left err -> putStdErr err
      Right succ -> putStr (T.unpack succ)

teh :: ((T.Text, M.Map T.Text [ Change ]),(T.Text, M.Map T.Text [ Change ])) -> [T.Text] -> [Change] -> Either T.Text T.Text
teh _ [] [] = Left "whoosie doopsie run me with arguments or run teh -h for help" -- no arguments were passed
teh m@((cErr,cMac),(lErr,lMac)) [x] [] =
  if x == "-h" then Right help
  else if x == "penguin" then Right pod
  else if x == "--listmacros" then Right $ printMacros m
  else Left "whoosie doopsie run me with more than one argument or run teh -h for help" -- only one argument was passed
teh _ [x] xs = Right $ doChanges xs x -- base case, one argument left and changes to apply to it
teh _ [] xs = Left $ "whoops I have these changes to do but nothing to do them too " <> (showT xs)
teh m@((cErr,cMac),(lErr,lMac)) (x:xs) chgs =
  if x == "-h" then Right help
  else if x == "penguin" then Right pod
  else if x == "--listmacros" then Right $ printMacros m
  else -- check if it can be read as a Change
    case (readMaybeT x :: Maybe Change) of
      Just ch -> teh m xs $ chgs <+> ch -- save this Change in a list and recurse until the last argument
      Nothing ->  -- check for custom macro
        -- <> for Map favors left
        -- if both Maps have a macro with the same key, use local one
        case ((lMac<>cMac) !? x) of
          Nothing -> -- macro not found
            Left $ x <> " not recognized as command"
          Just macchgs -> -- macro found, add to list of changes and recurse
            teh m xs (chgs <> macchgs)

doChanges :: [Change] -> T.Text -> T.Text
doChanges [] txt = txt
doChanges (x:xs) txt = doChanges xs $ doChange x txt

doChange :: Change -> T.Text -> T.Text
doChange (Ch Whole wht) txt = -- applying change to whole blob of text
  case wht of
    Ins tx n ->
      if n >= 0 then -- counting forward
        (T.take n txt) <> tx <> (T.drop n txt)
      else -- counting backward
        (dropEnd ((abs n)-1) txt) <> tx <> (takeEnd ((abs n)-1) txt)
    Rem a b ->
      if b==0 then
        txt -- delete nothing
      else if a >= 0 then -- counting foward for skipped letters
        if b > 0 then -- deleting foward
          (T.take a txt) <> (T.drop (a+b) txt)
        else -- deleting back
          (T.dropEnd (abs b) (T.take a txt)) <> T.drop a txt
      else -- counting back for skipped letters
        if b > 0 then -- deleting foward
          (dropEnd (abs a) txt) <> (T.drop b (takeEnd (abs a) txt))
        else -- deleting back
          dropEnd ((abs a)+(abs b)) txt <> (takeEnd (abs a) txt)
    Fr find repl ->
      T.replace find repl txt

doChange (Ch Each wht) txt =
  T.unlines ( map (doChange (Ch Whole wht)) (T.lines txt))

doChange (Ch (Only ns) wht) txt =
  T.unlines $ map snd(mapIf (\(x,y)-> (x,(doChange (Ch Whole wht) y))) (\(x,_)-> x `elem` ns) (zip [1..] (T.lines txt)))
    -- this has to be one of the top ten ugliest T.lines of haskell i've ever written i love it so much

data Change = Ch Which What
  deriving (Read, Show)

data Which = Whole
           | Each
           | Only [Int]
  deriving (Read, Show)

data What = Fr T.Text T.Text
          | Ins T.Text Int
          | Rem Int Int
  deriving (Read, Show)

(<+>) :: [a] -> a -> [a]
(<+>) xs x = xs<>[x]

pod :: T.Text
pod =
 "hi every1 im new!!!!!!! *holds up spork* my name is katy but u can call me t3h PeNgU1N oF d00m!!!!!!!! lol…as u can see im very random!!!! thats why i came here, 2 meet random ppl like me ^_^… im 13 years old (im mature 4 my age tho!!) i like 2 watch invader zim w/ my girlfreind (im bi if u dont like it deal w/it) its our favorite tv show!!! bcuz its SOOOO random!!!! shes random 2 of course but i want 2 meet more random ppl =) like they say the more the merrier!!!! lol…neways i hope 2 make alot of freinds here so give me lots of commentses!!!!\n\
\DOOOOOMMMM!!!!!!!!!!!!!!!! <--- me bein random again ^_^ hehe…toodles!!!!!\n\
\\n\
\love and waffles,\n\
\\n\
\t3h PeNgU1N oF d00m"

help :: T.Text
help =
  "Usage: teh [options]\n\
  \   by default teh reads from StdIn for text to apply changes to.  To disable this either pass the --nostdin flag or\n\
  \   pass a filename as the final argument \
  \   -h will bring you this menu and --listmacros will list all macros found in the config folder and in the local directory\
  \   macros can be written in the .teh file in a directory or in your xdgconfig directory\
  \   commands are written in the form of 'Ch <Whole, Each, Only [1, 2, 3]> (<Fr \"find\" \"replce\", Rem 0 1, Ins 0 \"insert\">)'\
  \   See README for more details "

seekMacs :: T.Text ->  IO (T.Text, (M.Map T.Text [Change]))
seekMacs f = do
  exist <- doesFileExist $ T.unpack f
  if exist then do
    file <- (readFile $ T.unpack f) >>= (return . T.pack)
    case readMaybeT (uncomment file)::Maybe (M.Map T.Text [Change]) of
      Just m -> return ("",m)
      Nothing -> return ("could not parse " <> f, M.empty)
  else
    return (f <> " not found", M.empty)

takeEnd :: Int -> T.Text -> T.Text
takeEnd n  = (T.reverse . T.take n . T.reverse)

dropEnd :: Int -> T.Text -> T.Text
dropEnd n  = (T.reverse . T.drop n . T.reverse)

droplen :: T.Text -> T.Text -> T.Text
droplen stub whole = T.drop (T.length stub) whole

takeLast :: Int -> [a] -> [a]
takeLast n = (reverse . take n . reverse)

dropLast :: Int -> [a] -> [a]
dropLast n = (reverse . drop n . reverse)

mapIf :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapIf _ _ [] = []
mapIf f b (x:xs) =
  if b x then
    (f x):(mapIf f b xs)
  else
    x : (mapIf f b xs)

putStdErr :: T.Text -> IO()
putStdErr = hPutStr stderr . T.unpack

rdFrmStdIn :: [T.Text] -> IO ([T.Text], Bool)
rdFrmStdIn [] = return ([],False)
rdFrmStdIn sts = do
  if "--nostdin" `elem` sts then
    let (pre, post) = break ("--nostdin"==) sts in -- remove nostdin arg and don't read stdin for next arg
      return (pre <> (tail post),False)
  else do
    isfile <- doesFileExist $ T.unpack (head $ takeLast 1 sts)
    if isfile then do -- replace filename withe file contents and return list with False for no stdin
      file <- (readFile $ T.unpack (head $ takeLast 1 sts)) >>= (return . T.pack)
      return ((dropLast 1 sts)<+>file,False)
    else  -- return list of text unmodified with True to read last arg from stdin
      return (sts, True)

-- filter out any lines that begin with the character #
uncomment :: T.Text -> T.Text
uncomment = (T.unlines . filter (\x->T.head x /= '#') . T.lines)

-- eventually this will be replaced with a funtion that will generate a pretty piece of text to show all in scope macros and any parse errors
-- for now it just kind of sorta technically works if you squint a bit and don't enjoy life
printMacros :: ((T.Text, M.Map T.Text [ Change ]),(T.Text, M.Map T.Text [ Change ])) -> T.Text
printMacros ((cErr,cMac),(lErr,lMac)) = ("from <xdgconfig>/.teh\n"<>cErr<>(showT cMac)<>"\n"<>"from <cwd>/.teh\n"<>lErr<>(showT lMac))

showT :: Show a => a -> T.Text
showT = T.pack . show

readMaybeT :: Read a => T.Text -> Maybe a
readMaybeT = readMaybe . T.unpack

readT :: Read a => T.Text ->  a
readT = read . T.unpack

