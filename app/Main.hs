{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getArgs)
import System.Directory (doesFileExist, XdgDirectory(XdgConfig), getXdgDirectory)
import Text.Read (readMaybe)
import qualified Data.Map as M    -- for M.Map
import Data.Map ((!?))
import Data.List (isPrefixOf)
import Data.Either (partitionEithers)
import qualified Data.Text as T
import GHC.IO.StdHandles (stderr)
import GHC.IO.Handle (hPutStr)

main :: IO ()
main = do
  args' <- getArgs >>= (return . (map T.pack)) -- get args passed in
  confDir <- getXdgDirectory XdgConfig ".teh" >>= (return . T.pack) -- get path for where the user .teh conffile would be if it exists
  conmac@(conerr, conmacs) <- seekMacs confDir -- Tuple of T.Text and Map T.Text [Change] if it was read with no problem string will
  locmac@(locerr, locmacs) <- seekMacs ".teh" -- be empty, if no file or parse error string will say that with empty map
  proceed <- howToProceed args' -- determine whether to end early or not, and whether to read from stdin
  let (macErrors, combMacs) = combineMacros conmac locmac
  case proceed of
    Stop Help -> putTxt help
    Stop Macs -> putTxt $ printMacros (macErrors, combMacs)
    Stop Penguin -> putTxt pod
    WithStdIn ->
      wstdin (parseargs combMacs args')
    WoStdIn (newargs, txt) ->
      wostdin (parseargs combMacs newargs) txt

wstdin :: Either T.Text [Edit] -> IO ()
wstdin (Left err) = putStdErr err
wstdin (Right pargs) = do
  stdin <- getContents >>= (return . T.pack)
  putTxt (teh pargs stdin)

wostdin :: Either T.Text [Edit] -> T.Text -> IO ()
wostdin (Left err) _ = putStdErr err
wostdin (Right pargs) txt = putTxt (teh pargs txt)

seekMacs :: T.Text ->  IO (T.Text, Macros)
seekMacs f = do
  exist <- doesFileExist $ T.unpack f
  if exist then do
    file <- (readFile $ T.unpack f) >>= (return . T.pack)
    let (err, m) = parseMacs file in
      if err=="" then
        return (f<> "parsed with no errors", m)
      else
        return ("errors for "<>f<>"\n"<>err, m)
  else
    return (f <> " not found", M.empty)

parseMacs :: T.Text -> (T.Text, Macros)
parseMacs txt =
  (\(xs, ms)->(T.unlines xs, M.fromList ms)) $ -- convert list of errors to one big error, and list of (Text, Edit) to Macros
  partitionEithers $ -- convert list of Either error (Text, Edit) to (list of errors, list of (Text, Edit))
  map parseMac $ -- convert each line into either an error, or a (Text, Edit)
  zip [1..] $ -- number each line
  map (\x->"("<>x<>")") $ -- wrap each line in parenthesis
  T.lines txt -- break into list of lines of text
--  M.fromList $
--  map (\(tx,ta,ch)->(tx,(ta,ch))) $
--  map (\x-> read x::(T.Text, Target, [Change])) $
--  map "("<>x<>")"
-- this worked in ghci
-- here slightly different
-- first let linestxt = lines txt
-- then map \x->"("<>x<>")" linestxt
-- then func to either read as (Text, Target, [Change]) or give error
-- from list Tupes into macros, and save rest as reported errros

parseMac :: (Int, T.Text) -> Either T.Text (T.Text, Edit)
parseMac (i, txt) =
  case (readMaybeT $ "("<>txt<>")"  :: Maybe (T.Text, Target, [Change])) of
    Just m ->
      Right $ (\(tx,ta,ch)->(tx,(ta,ch))) m
    Nothing ->
      case (readMaybeT $ "("<>txt<>")"  :: Maybe (T.Text, Target, Change)) of
        Just m ->
          Right $ (\(tx,ta,ch)->(tx,(ta,[ch]))) m
        Nothing -> Left $ "could not parse "<>txt<>" on line "<>showT i


-- teh list of edits Text to change
-- teh :: [Edit] -> T.Text -> T.Text
{--
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
--}
-- this can probably be a fold
-- essentially a wrapper to call doEdit with each of the list of Edits
teh :: [Edit] -> T.Text -> T.Text
teh [] txt = txt
teh (x:xs) txt = teh xs $ doEdit x txt

-- first checks if the edit has ano changes, and if so ignores it all together
-- if the list of changes is nonempty then use the Target from the edit to only doChanges to the correct part of the text
doEdit :: Edit -> T.Text -> T.Text
doEdit (_, []) txt = txt -- no changes means no edit to do
-- do changes just to the whole body of text
doEdit (Whole, chgs) txt =
  doChanges chgs txt
-- do changes to each individual line of text
doEdit (Each, chgs) txt =
  T.unlines ( map (doChanges chgs) (T.lines txt))
-- only do changes to the numbered lines given to Only
doEdit (Only ns, chgs) txt =
  T.unlines $ map snd(mapIf (\(x,y)-> (x,(doChanges chgs y))) (\(x,_)-> x `elem` ns) (zip [1..] (T.lines txt)))

-- unconcerned with target, only has a list of Changes to do and a Text to do them to
doChanges :: [Change] -> T.Text -> T.Text
doChanges chg txt = -- applying change to whole blob of text
  case chg of
    [] -> txt -- no more changes to do return final text
    (Ins tx n):chgs ->
      if n >= 0 then -- counting forward
        doChanges chgs ((T.take n txt) <> tx <> (T.drop n txt))
      else -- counting backward
        doChanges chgs ((dropEnd ((abs n)-1) txt) <> tx <> (takeEnd ((abs n)-1) txt))
    (Rem a b):chgs ->
      if b==0 then
        doChanges chgs txt -- delete nothing
      else if a >= 0 then -- counting foward for skipped letters
        if b > 0 then -- deleting foward
          doChanges chgs ((T.take a txt) <> (T.drop (a+b) txt))
        else -- deleting back
          doChanges chgs ((T.dropEnd (abs b) (T.take a txt)) <> T.drop a txt)
      else -- counting back for skipped letters
        if b > 0 then -- deleting foward
          doChanges chgs ((dropEnd (abs a) txt) <> (T.drop b (takeEnd (abs a) txt)))
        else -- deleting back
          (dropEnd ((abs a)+(abs b)) txt <> (takeEnd (abs a) txt))
    (Fr find repl):chgs ->
      doChanges chgs (T.replace find repl txt)


type Edit  = (Target, [Change])

data Change = Fr T.Text T.Text
            | Ins T.Text Int
            | Rem Int Int
  deriving (Read, Show)

data Target = Whole
            | Each
            | Only [Int]
  deriving (Read, Show)

type Macros = M.Map T.Text Edit




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

-- takes the config file then the local file
combineMacros :: (T.Text, Macros) -> (T.Text, Macros) -> (T.Text, Macros)
combineMacros (cErr, cMac) (lErr,lMac) =
  (cErr<>lErr, lMac<>cMac) -- <> for Map favors left if both have same key and local overrides conf so local on left


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


{-- pretty sure I can remove this whole thing
-- process raw list of args to check whether or not to read from stdIn
-- if --nostdin is passed as an argument, ignore stdin and treat the text of the last arguemnt as the text to modify
-- else if last argument is a valid filename, ignore stdin and treat the text of the file as the text to modify
-- else read from stdin for text to modify
rdFrmStdIn :: [T.Text] -> IO ([T.Text], Bool)
rdFrmStdIn [] = return ([],False)
rdFrmStdIn sts = do
  if "--nostdin" `elem` sts then -- --nostdin passed as an argument, the last argument passed will be treated as the target text
    let (pre, post) = break ("--nostdin"==) sts in -- remove nostdin arg and don't read stdin for next arg
      return (pre <> (tail post),False)
  else do
    isfile <- doesFileExist $ T.unpack (head $ takeLast 1 sts)
    if isfile then do -- replace filename withe file contents and return list with False for no stdin
      file <- (readFile $ T.unpack (head $ takeLast 1 sts)) >>= (return . T.pack)
      return ((dropLast 1 sts)<+>file,False)
    else  -- return list of text unmodified with True to read last arg from stdin
      return (sts, True)
      --}


howToProceed :: [T.Text] -> IO Proceed
howToProceed sts =
  if "-h" `elem` sts || "--help" `elem` sts then
    return $ Stop Help
  else if "--show-macros" `elem` sts then
    return $ Stop Macs
  else if "--penguin" `elem` sts then
    return $ Stop Penguin
  else if "--nostdin" `elem` sts then -- --nostdin passed as an argument, the last argument passed will be treated as the target text
    let (pre, post) = break ("--nostdin"==) sts
        (eds, txt)  = ((pre<>(((drop 1) . (dropLast 1)) post)),(takeLast 1 post)) -- remove nostdin arg and don't read stdin for next arg
    in return $ WoStdIn (eds, mconcat txt)
  else do
    isfile <- doesFileExist $ T.unpack (head $ takeLast 1 sts)
    if isfile then do -- replace filename withe file contents and return list with False for no stdin
      file <- (readFile $ T.unpack (head $ takeLast 1 sts)) >>= (return . T.pack)
      return $ WoStdIn ((dropLast 1 sts), file)
    else  -- return list of text unmodified with True to read last arg from stdin
      return WithStdIn


data Proceed = WithStdIn
             | WoStdIn ([T.Text], T.Text)
             | Stop Cause

data Cause = Help
           | Macs
           | Penguin

-- filter out any lines that begin with the character #
uncomment :: T.Text -> T.Text
uncomment = (T.unlines . filter (\x->T.head x /= '#') . T.lines)


printMacros :: (T.Text, Macros) -> T.Text
printMacros _ = "I'll do it this afternooooon"
-- eventually this will be replaced with a funtion that will generate a pretty piece of text to show all in scope macros and any parse errors
-- for now it just kind of sorta technically works if you squint a bit and don't enjoy life
-- printMacros :: ((T.Text, M.Map T.Text [ Change ]),(T.Text, M.Map T.Text [ Change ])) -> T.Text
-- printMacros ((cErr,cMac),(lErr,lMac)) = ("from <xdgconfig>/.teh\n"<>cErr<>(showT cMac)<>"\n"<>"from <cwd>/.teh\n"<>lErr<>(showT lMac))

showT :: Show a => a -> T.Text
showT = T.pack . show

readMaybeT :: Read a => T.Text -> Maybe a
readMaybeT = readMaybe . T.unpack

readT :: Read a => T.Text ->  a
readT = read . T.unpack

putTxt :: T.Text -> IO ()
putTxt = putStr . T.unpack

parseargs :: Macros -> [T.Text] -> Either T.Text [Edit]
parseargs _ _ = Left "error"
