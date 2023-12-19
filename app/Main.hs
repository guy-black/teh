{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getArgs)
import System.Directory (doesFileExist, XdgDirectory(XdgConfig), getXdgDirectory)
import Text.Read (readMaybe)
import qualified Data.Map.Strict as M    -- for M.Map
import Data.Map.Strict ((!?))
import Data.List (isSuffixOf, sortOn, (\\), nub)
import Data.Either (partitionEithers)
import Data.Char (isSpace, isDigit)
import qualified Data.Text as T
import GHC.IO.StdHandles (stderr)
import GHC.IO.Handle (hPutStr)
import Control.Exception (try, SomeException, evaluate)


main :: IO ()
main = do
  args <- getArgs >>= (return . (map T.pack)) -- get args passed in
  if (any (`elem`args) ["-h", "--help"]) || null args then -- checking for an early exit condition
    putTxt help
  else if args == ["--penguin"] then do  -- hehe we have fun here :)
    putTxt pod
  else do  -- no early exit case, start compiling list of macro files
    let ignoreErrors = "-ie" `elem` args
    let args' = filter (/= "-ie") args
    confDir <- getXdgDirectory XdgConfig ".teh" >>= (return . T.pack) -- get path for where the user .teh file should be
    let (cflagmacs', argsminuscflag) = extArgs (=="-c") args'
    let (macstoignore', maclessargs) = extArgs (=="-ic") argsminuscflag
    let cflagmacs = map snd cflagmacs' -- had to make extArgs return the flag with the argument so I need to
    let macstoignore = map snd macstoignore'-- add these lines to just have the argument
    let finalmaclist = (dedupe $ confDir:".teh":cflagmacs) `remAll` macstoignore -- final list of files to look for macros
    parsedMacsAndErrs <- mapM seekMacs finalmaclist -- convert our files to look for macros into a [(errors::T.Text, Macros)]
    if "--show-macros" `elem` maclessargs then -- we don't actually need to edit text, just parse macros and print details
      putTxt $ printMacros parsedMacsAndErrs -- TODO actually make printMacros do something useful
    else do  -- we can squish our parsed marcros into one big Macros and toss forget errors or which file they came from
      let finalMacs = mconcat $ reverse $ map snd parsedMacsAndErrs
      -- mconcat will favor values to the left, but I want to favor values to the right, so reverse it first
      let nwflags = (any (`elem`maclessargs) ["-n", "-N"],any (`elem`maclessargs) ["-w", "-W"]) -- (Bool, Bool) for -n and -w flags
      let argsSansNW = maclessargs `remAll` ["-n", "-N", "-w", "-W"] -- arg list without -n and -w flags
      let (tfargs, argsSansTF) = extArgs (\x-> any (x==) ["-t", "-T", "-f", "-F"]) argsSansNW
      let editsOnly = filter (/= "-stdin") argsSansTF -- no -n -w -t -f -ie or -stdin flags left, should only be edits now
      let (edErrs, pEdits) = parseEdits finalMacs editsOnly ([],[])
      if not $ null tfargs then do -- some t and/or f flags were found
        let readStdIn = "--stdin" `elem` argsSansTF
        (tfErrs, tfVals) <- whatTF ([],[]) tfargs
        if (not ignoreErrors)&&((not $ null tfErrs)||(not $ null edErrs)) then -- if there are errors and we aren't ignoring them
          putStdErr $ "errors on parsing -t and -f flags \n" <>  (T.unlines tfErrs)
                         <> "\nerrors on parsing edits\n" <> (T.unlines edErrs)
        else -- there are no errors, or we're just ignoring them
          if readStdIn then -- we are editing tfVals and stdIn
            return ()
          else -- we are only editing tfVals
            return ()
      else -- no t or f flags, only editing text from stdin
        if (not ignoreErrors) && (not $ null edErrs) then -- if there are errors and we aren't ignoring them
          putStdErr $ "errors on parsing edits\n" <>  (T.unlines edErrs)
        else do
        return ()

{--
  conmac@(conerr, conmacs) <- seekMacs confDir -- Tuple of T.Text and Map T.Text [Change] if it was read with no problem string will
  locmac@(locerr, locmacs) <- seekMacs ".teh" -- be empty, if no file or parse error string will say that with empty map
  let (macErrors, combMacs) = combineMacros conmac locmac
  proceed <- howToProceed combMacs args -- determine whether to end early or not, and whether to read from stdin
  case proceed of
    Stop Help -> putTxt help
    Stop Macs -> putTxt $ printMacros (macErrors, combMacs)
    Stop Penguin -> putTxt pod
    Stop (ArgParseErr err) -> putStdErr err
    WithStdIn edits -> do
      stdin <- getContents >>= (return . T.pack)
      putTxt $ teh edits stdin
    WoStdIn (edits, txt) ->
      putTxt $ teh edits txt
--}

-- take an accumulator, list of (flag, argument), and returns accumulator
-- should only have to call once so won't bother with ergonomic wrapper
whatTF :: ([T.Text], [T.Text]) -> [(T.Text, T.Text)] -> IO ([T.Text], [T.Text])
whatTF acc@(eacc, tacc) tf =
  case tf of
    [] -> return acc -- base case
    (hf, ha):t ->
      if any (hf ==) ["-t", "-T"] then -- ha is a piece of text to edit, no error to report
        whatTF (eacc, tacc<+>ha) t -- add it to tacc and recurse on t
      else if any (hf ==) ["-f", "-F"] then do-- ha is a file to try and read
        exists <- doesFileExist $ T.unpack ha -- check if ha exists
        if not exists then -- the file doesn't exist
          whatTF (eacc<+>("file " <> ha <> " doesn't seem to exist"), tacc) t
        else do
          file <- (try $ (readFile $ T.unpack ha) >>= (return . T.pack) >>= evaluate) :: IO (Either SomeException T.Text)
          case file of -- check if ha can be read
            Left _    -> whatTF (eacc<+>("file " <> ha <> " cannot be read as text"), tacc) t
            Right txt -> whatTF (eacc, tacc<+>txt) t
      else -- well this ain't right
        whatTF (eacc <+> ("Something went wront with either extArgs and " <> hf <> " got marked as a t or f flag \
                          \even though it's not.  Maybe I should've actually made a real data type for the flags \
                          \idk.  If you're me then fix this, if you're not me then please file an issue at \
                          \github.com/guy-black/teh please and thanks :)"), tacc) t


seekMacs :: T.Text ->  IO (T.Text, Macros)
seekMacs f = do
  exist <- doesFileExist $ T.unpack f
  if exist then do
    file <- (try $ (readFile $ T.unpack f) >>= (return . T.pack) >>= evaluate) :: IO (Either SomeException T.Text)
    case file of
      Left _ -> return (f <> " could not be read", M.empty)
      Right file' ->
        let (err, m) = parseMacs file' in
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
  filter (\x->T.take 1 x /= "#") $ -- remove comments from .teh
  T.lines txt -- break into list of lines of text

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


-- this can probably be a fold
-- essentially a wrapper to call doEdit with each of the list of Edits
teh :: (Bool, Bool) -> [Edit] -> T.Text -> T.Text
teh _ [] txt = txt
teh (nflag, wflag) (x:xs) txt =
  -- not T.Null txt to the left of T.next will force the expression to resolve to false before erroring by calling T.last on ""
  if nflag && (not $ T.null txt) && (T.last txt == '\n') then
    -- on recurse set nflag to false as the trailing \n is already removed
    (teh (False, wflag) xs $ doEdit wflag x $ T.dropEnd 1 txt) <> "\n"
  else -- either nflag is false or there is no trailing \n
    -- set nflag false becasue either it is false and there's no difference, or there is no trailing \n
    -- and setting it to false makes && resolve correctly to false witout needing to check last character
    -- also avoids ignoring any trailing \n added by previously applied edit
    teh (False, wflag) xs $ doEdit wflag x txt

-- first checks if the edit has ano changes, and if so ignores it all together
-- if the list of changes is nonempty then use the Target from the edit to only doChanges to the correct part of the text
doEdit :: Bool -> Edit -> T.Text -> T.Text
doEdit _ (_, []) txt = txt -- no changes means no edit to do
-- do changes just to the whole body of text
doEdit wflag (Whole, chgs) txt =
  doChanges wflag chgs txt
-- do changes to each individual line of text
doEdit wflag (Each, chgs) txt =
  T.unlines ( map (doChanges wflag chgs) (T.lines txt))
-- only do changes to the numbered lines given to Only
doEdit wflag (Only ns, chgs) txt =
  T.unlines $ map snd(mapIf (\(x,y)-> (x,(doChanges wflag chgs y))) (\(x,_)-> x `elem` ns) (zip [1..] (T.lines txt)))

-- unconcerned with target, only has a list of Changes to do and a Text to do them to
doChanges :: Bool -> [Change] -> T.Text -> T.Text
doChanges wflag chg txt = -- applying change to whole blob of text
  case chg of
    [] -> txt -- no more changes to do return final text
    (Ins tx n):chgs ->
      if n >= 0 then -- counting forward
        doChanges wflag chgs ((T.take n txt) <> tx <> (T.drop n txt))
      else -- counting backward
        doChanges wflag chgs ((T.dropEnd ((abs n)-1) txt) <> tx <> (T.takeEnd ((abs n)-1) txt))
    (Rem a b):chgs ->
      if b==0 then
        doChanges wflag chgs txt -- delete nothing
      else if a >= 0 then -- counting foward for skipped letters
        if b > 0 then -- deleting foward
          doChanges wflag chgs ((T.take a txt) <> (T.drop (a+b) txt))
        else -- deleting back
          doChanges wflag chgs ((T.dropEnd (abs b) (T.take a txt)) <> T.drop a txt)
      else -- counting back for skipped letters
        if b > 0 then -- deleting foward
          doChanges wflag chgs ((T.dropEnd (abs a) txt) <> (T.drop b (T.takeEnd (abs a) txt)))
        else -- deleting back
          (T.dropEnd ((abs a)+(abs b)) txt <> (T.takeEnd (abs a) txt))
    (Fr find repl):chgs ->
      doChanges wflag chgs (T.replace find repl txt)



{-- all of this funcitonality should be moved into main I think I'll be fine removing it all together
howToProceed :: Macros -> [T.Text] -> IO Proceed
howToProceed mac sts =
  case sts of
        []     -> return $ Stop Help
        (s:ts) ->
          if "-h" `elem` sts || "--help" `elem` sts then
            return $ Stop Help
          else if "--show-macros" `elem` sts then
            return $ Stop Macs
          else if "--penguin" `elem` sts then
            return $ Stop Penguin
          else if "--nostdin" `elem` sts then -- --nostdin passed as an argument, the last argument passed will be treated as the target text
            let (pre, post) = break ("--nostdin"==) sts
                (eds, txt)  = ((pre<>(((drop 1) . (dropLast 1)) post)),(takeLast 1 post)) -- remove nostdin arg and don't read stdin for next arg
            in
              case parseargs mac eds of
                Left err -> return $ Stop $ ArgParseErr err
                Right edits -> return $ WoStdIn (edits, mconcat txt)
          else do
            isfile <- doesFileExist $ T.unpack (mconcat $ takeLast 1 ts)
            if isfile then -- replace filename withe file contents and return list with False for no stdin
              case parseargs mac $ dropLast 1 sts of
                Left err -> return $ Stop $ ArgParseErr err
                Right edits -> do
                  file <- (readFile $ T.unpack (mconcat $ takeLast 1 ts)) >>= (return . T.pack)
                  return $ WoStdIn (edits, file)
            else
              case parseargs mac sts of
                Left err -> return $ Stop $ ArgParseErr err
                Right edits -> return $ WithStdIn edits

--}

-- takes the config file then the local file
combineMacros :: [Macros] -> Macros
combineMacros = mconcat . reverse -- <> for Map favors left if both have same key and local overrides conf so local on left

printMacros :: [(T.Text, Macros)] -> T.Text
printMacros _ = "I'll do it this afternooooon"

-- parseargs :: Macros -> [T.Text] -> Either T.Text [Edit]
-- parseargs macs ts = parseargs' macs ts []

-- parseargs' :: Macros -> [T.Text] -> [Edit] -> Either T.Text [Edit]
-- parseargs' _ [] eds = Right $ reverse eds
-- parseargs' macs (t:ts) eds =
--   case parsearg macs t of
--     Left err -> Left err
--     Right ed -> parseargs' macs ts (ed:eds)

-- macros, list of edits to parse, acc errs,edits, final errs,edits
parseEdits :: Macros -> [T.Text] -> ([T.Text], [Edit]) -> ([T.Text], [Edit])
parseEdits _ [] acc = acc -- base case
parseEdits m (h:t) (eracc, edacc) =
  case parseEdit m h of
    Left err -> parseEdits m t (eracc<+>err, edacc)
    Right ed -> parseEdits m t (eracc, edacc<+>ed)

parseEdit :: Macros -> T.Text -> Either T.Text Edit
-- to safely call head on the list from T.words on the Text value
parseEdit _ "" = Left "Cannot parse empty argument.  Please check that you properly formatted your argumens"
parseEdit macs txt =
  case macs !? txt of-- check if the whole argument is just a macro
    Just ed -> Right ed
    Nothing -> -- argument is not just a macro, it must start with a Target
      let t = head $ T.words txt
          c = T.unwords $ drop 1 $ T.words txt in
        if any (t==) ["Whole","whole"] then  -- target is Whole
          case macs !? c of -- the change is just a macro
            Just (_,chs) -> Right (Whole, chs)
            Nothing -> -- change needs to be parsed
              case parseChgs macs c of
                Left err -> Left err
                Right chs -> Right (Whole, chs)
        else if any (t==) ["Each", "each"] then  -- target is Each
          case macs !? c of -- the change is just a macro
            Just (_,chs) -> Right (Each, chs)
            Nothing -> -- change needs to be parsed
              case parseChgs macs c of
                Left err -> Left err
                Right chs -> Right (Each, chs)
        else if any (t==) ["Only", "only"] then -- target is some Only, but need to find the numbers
          let (l', r') = span (\ch -> or $ map ($ ch) [isSpace, isDigit]) $ T.unpack c -- TODO, rewrite with T.span why did I do this?
              (l, r) = (T.pack l', T.pack r') in -- this pulls any numbers or white space immediately after the word Only
            case (readMaybeT ("["<>(T.intercalate "," $ T.words l)<>"]")::Maybe [Int]) of
              Nothing -> Left $ txt <> " is read with a Target Only, but " <> l <> " could not be read as [Int].  this is weird and you should look into it, assuming you is me.  if you is not me then you should make an issue on the github for this project to let me know kthx"
              Just lns -> -- we have only and we have a full target now
                case macs !? r of
                  Just (_,chs) -> Right (Only lns, chs)
                  Nothing -> -- change needs to be parsed
                    case parseChgs macs r of
                      Left err -> Left err
                      Right chs -> Right (Only lns, chs)
        else -- argument is not just a macro, and doesn't start with a Target
          Left $ txt <> " is not recognized as a saved macro, and does not start with a Target."
          -- at this point I know an empty argument would be passed out as an error

parseChgs :: Macros -> T.Text -> Either T.Text [Change]
-- handling an argument with no changes so parseChgs' doesn't have to
-- parseChgs' interprets and empty [Text] as the base case and returns the accumulator [Changes]
-- this prevents parseChgs' from producing and empty [Changes]
parseChgs _ "" = Left "Argument only has a target, arguments must include either a macro, or a target followed by either\
                       \ a macro or changes.  Please check the formatting of your argmuents"
parseChgs m t = parseChgs' m (wordsandquo t) []

parseChgs' :: Macros -> [T.Text] -> [Change] -> Either T.Text [Change]
parseChgs' m [] acc = Right $ reverse acc
parseChgs' m (t:ts) acc = -- undefined
  if any (t==) ["fr","FR","Fr"] then
    case ts of
      (a:b:cs) ->
        if T.null a then -- a must be non empty, b can be empty
          Left $ "first value passed to " <> t <> " must be nonempty"
        else
          parseChgs' m cs ((Fr a b):acc)
      _ ->
        Left $ "Error: " <> t <> " needs to be followed by two strings, but I found less than two"
  else if any (t==) ["ins", "in", "Ins", "In"] then
    case ts of
      (a:b:cs) ->
        case readMaybeT b::Maybe Int of
          Nothing -> Left $ "Error: " <> t <> " expects a string and an integer, but " <> b <> " could not be parsed as an integer"
          Just bint ->
            if T.null a then
              Left $ "Warning: " <> (T.unwords [t,a,b]) <> " is inserting and empty text, which does nothing.  Did you mean to remove that change or add text ot insert?"
            else
              parseChgs' m cs ((Ins a bint):acc)
      _ ->
        Left $ "Error: " <> t <> " needs to be followed by a string htenb a number, but I found less than two arguments after it"
  else if any (t==) ["Rem", "rem", "Rm", "rm"] then
    case ts of
      (a:b:cs) ->
        case readMaybeT a::Maybe Int of
          Nothing -> Left $ "Error: " <> t <> " expects two integers but " <> a <> " could not be read as an integer"
          Just aint ->
            case readMaybeT b::Maybe Int of
              Nothing -> Left $ "Error: " <> t <> " expects two integers but " <> b <> " could not be read as an integer"
              Just bint -> parseChgs' m cs ((Rem aint bint):acc)
      _ -> Left $ "Error: " <> t <> " expects two integers, but less than two arguments follow it"
  else
    Left $ "Error: " <> (T.unwords $ t:ts) <> " was expected to be a Change, but changes must start with Fr, Rem, or Ins.  Run with -h for more info"


-- -----------*
--  datatypes
-- -----------*

data Proceed = WithStdIn [Edit]
             | WoStdIn ([Edit], T.Text)
             | Stop Cause

data Cause = Help
           | Macs
           | Penguin
           | ArgParseErr T.Text

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

-- -----------------*
-- helper functions
-- -----------------*

-- like T.words, but concatenates a word that start's with '"' with all subsequent words until found a word that ends
-- with '"' only if it's not preceded by a '\'
-- TODO: ignore closing escaped quote mark ending a quote
-- ALSO: let's just strip the quotes here to make it easy on parseChgs'
-- and then we get non quoted single word text arguments for free
wordsandquo :: T.Text -> [T.Text]
wordsandquo t =
  let (pre, rst) = break (\x->T.take 1 x == "\"") $ T.words t in
    if rst == [] then -- there are no words that start with a quote, T.words will work fine
      T.words t
    else -- there are quotes
      case takeUntil (\x->T.takeEnd 1 x == "\"" && T.takeEnd 2 x /= "\\\"") rst of -- take words up to the first one to end with a non escaped "
        ([], _) ->  -- there was no closing quote, just treat as regular words
          T.words t
        (quo, aftquo) -> pre <> [(T.unwords quo)] <> (wordsandquo $ T.unwords aftquo)

-- if no value within ls is True for b then
--   takeUntil b ls == ([], ls)
-- if some value a within ls is True for b then for the first a
--   takeuntil b ls == (prea<+>a,posta)
takeUntil ::  (a -> Bool) -> [a] -> ([a],[a])
takeUntil b ls =
  case break b ls of
    (_, []) -> ([], ls)
    (pre, (r:st)) -> (pre<+>r,st)

-- split a list into the values after a value that returns true and the rest of the list with the true values and their successor
-- more concretely, will be used to pull out all instances of a given flag and the arg that follows from a bigger list
extArgs :: (a -> Bool) -> [a] -> ([(a,a)], [a])
extArgs f ls =
  case ls of
    [] -> ([],[]) -- an empty list, just double it
    [a] -> ([],[a]) -- again can't do anything with a singleton list
    _ -> extArgs' f ([], []) ls -- this list has two or more values so now we can call the real function

{-- TODO: figure out why defining extArgs like this makes ghc unhappy
    "Multiple declarations of 'extArg', but it only pointed out the first and the third one, not the second
    this was before it started complaining that I was calling it as extArgs above
extArg _ [] = ([],[]) -- an empty list, just double it
estArg _ (a:[]) = ([],[a]) -- again can't do anything with a singleton list
extArg f ls@(a:aa) = extArg' f ([], []) ls -- this list has two or more values so now we can call the real function
--}

extArgs' :: (a -> Bool) -> ([(a,a)], [a]) -> [a] -> ([(a,a)], [a]) -- the less ergonomic function we actually need
extArgs' _ (wnt, othr) [] = (wnt, othr) -- empty list to sort through, base case.  I remember when recursion was hard to understand
extArgs' _ (wnt, othr) [a] = (wnt, othr<+>a) -- one item list, other base case.  Now I just slap this together without even thinking
extArgs' f (wnt, othr) (h:hh:rem) =
  if f h then -- checking if we want to add to the wanted accumulator
    extArgs' f (wnt<+>(h,hh), othr) rem -- we found what we want so we drop the flag and the argument and recurse with what's left
  else
    extArgs' f (wnt, othr<+>h) (hh:rem) -- not a flag we wanted, add it to the others and recurse

showT :: Show a => a -> T.Text
showT = T.pack . show

readMaybeT :: Read a => T.Text -> Maybe a
readMaybeT = readMaybe . T.unpack

readT :: Read a => T.Text ->  a
readT = read . T.unpack

putTxt :: T.Text -> IO ()
putTxt = putStr . T.unpack


-- these functions were already in T.Text idk why I never noticed tf
-- takeEnd :: Int -> T.Text -> T.Text
-- takeEnd n  = (T.reverse . T.take n . T.reverse)
--
-- dropEnd :: Int -> T.Text -> T.Text
-- dropEnd n  = (T.reverse . T.drop n . T.reverse)

droplen :: T.Text -> T.Text -> T.Text
droplen stub whole = T.drop (T.length stub) whole

takeLast :: Int -> [a] -> [a]
takeLast n = (reverse . take n . reverse)

dropLast :: Int -> [a] -> [a]
dropLast n = (reverse . drop n . reverse)

-- remove any duplicate values from a list, preserving order, leaving last iteration of an item in the list
-- nub typically only saves first instance of a value and removing remaing instances
-- I want to save the last instance of each value and remove their predecessors and retain order, so wrap it in reverses
dedupe :: Eq a => [a] -> [a]
dedupe = reverse . nub . reverse -- using reverse twice feels icky so maybe time this with a custom recursive version later

-- remove every item from first list that is in second list
remAll :: Eq a => [a] -> [a] -> [a]
remAll ls [] = ls -- base case, no more values to remove
remAll ls (h:t) = remAll (filter (/=h) ls) (filter (/=h) t) -- recurse filtering out all values of head from both lists

-- a conditional map
-- only works with a function that evaluates to a value of the same type as it's input
-- it is only applied to values of a list that satisfy a function that returns bool
mapIf :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapIf _ _ [] = []
mapIf f b (x:xs) =
  if b x then
    (f x):(mapIf f b xs)
  else
    x : (mapIf f b xs)

putStdErr :: T.Text -> IO()
putStdErr = hPutStr stderr . T.unpack . (<>"\n")

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
