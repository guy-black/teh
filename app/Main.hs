{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getArgs)
import System.Directory (doesFileExist, XdgDirectory(XdgConfig), getXdgDirectory)
import System.Console.GetOpt
import System.Exit
import qualified Data.Map.Strict as M    -- for M.Map and related functions
import Data.Map.Strict ((!?))
import Data.List (nub, partition)
import Data.Either (partitionEithers)
import Data.Char (isSpace, isDigit)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)
import GHC.IO.StdHandles (stderr)
import GHC.IO.Handle (hPutStr)
import Control.Exception (try, SomeException, evaluate)
-- import Control.Monad (forever)
-- will uncomment again when I implement stream editing if I still need it then

main :: IO ()
main = do
  args <- getArgs -- nvm need them as strings for getOpt
  if unwords args == "penguin of doom" then   -- hehe we have fun here :)
    TIO.putStrLn pod
  else do
    let (flags, edits', errs') = getOpt Permute options args
    if (Help `elem` flags) || null args then -- if there is nothing passed then just display help
      TIO.putStrLn help
    else do -- generate list of macros
      confDir <- getXdgDirectory XdgConfig ".teh" -- get path for where the user .teh file should be
      let (newFlags, preMacList) = makeMacFileList ([], [confDir, "./.teh"]) flags
      wereMacsFound <- mapM doesFileExist preMacList -- make list of bools corresponding with whether each in preMacList exists
      let (found', notFound') = partition fst $ zip wereMacsFound preMacList -- sort found and not found mac files
      let notFound = filter (\x -> not $ x `elem` [confDir, ".teh"]) $ map snd notFound' -- strip bool and defaults for ease
      if not $ null notFound then -- if there were still config files not found
        die $ "Couldn't find the following macros files " ++ (unwords notFound) -- literally just complain and die
      else do
        let foundMacsFiles = map snd found' -- we got our list of macro files lfgggggggg
        -- parse out macros files
        -- if ShowMacros `elem` flags then show macros
        return ()

{--
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
      putTxt $ printAllMacros parsedMacsAndErrs
    else do  -- we can squish our parsed marcros into one big Macros and toss forget errors or which file they came from
      let finalMacs = mconcat $ reverse $ map snd parsedMacsAndErrs
      -- mconcat will favor values to the left, but I want to favor values to the right, so reverse it first
      let nflag = any (`elem`maclessargs) ["-n", "-N"] -- Bool for -n flag
      let argsSansN = maclessargs `remAll` ["-n", "-N"] -- arg list without -n and -w flags
      let (tfargs, argsSansTF) = extArgs (\x-> any (x==) ["-t", "-T", "-f", "-F"]) argsSansN
      let editsOnly = filter (/= "--stdin") argsSansTF -- no -n -t -f -ie or -stdin flags left, should only be edits now
      let (edErrs, pEdits) = parseEdits finalMacs editsOnly ([],[])
      if not $ null tfargs then do -- some t and/or f flags were found
        let readStdIn = "--stdin" `elem` argsSansTF
        (tfErrs, tfVals) <- whatTF ([],[]) tfargs
        if (not ignoreErrors)&&((not $ null tfErrs)||(not $ null edErrs)) then -- if there are errors and we aren't ignoring them
          putStdErr $ "errors on parsing -t and -f flags \n" <>  (T.unlines tfErrs) -- I'm okay with the extra
                         <> "errors on parsing edits\n" <> (T.unlines edErrs) -- newline reporting errors
        else -- there are no errors, or we're just ignoring them
          if readStdIn then do -- we are editing tfVals and stdIn
            putTxt $ T.unlines $ map (teh nflag pEdits) tfVals -- I'm okay with the extra newline between tf text and stdin
            getContents >>= (return . T.pack) >>= (\x -> putTxt $ teh nflag pEdits x)
          else -- we are only editing tfVals
            putTxt $ T.unlines $ map (teh nflag pEdits) tfVals
      else -- no t or f flags, only editing text from stdin
        if (not ignoreErrors) && (not $ null edErrs) then -- if there are errors and we aren't ignoring them
          putStdErr $ "errors on parsing edits\n" <>  (T.unlines edErrs)
        else
          getContents >>= (return . T.pack) >>= (\x -> putTxt $ teh nflag pEdits x)
--}

          -- forever $ getLine >>= (return . T.pack) >>= (\x -> putTxt $ teh nflag pEdits x)
          -- this /kinda/ works for editing stream from std in if you only want to work with each line, and ony insert
          -- and delete relative to the beginning of the line
          -- overall not good enough


-- -----------*
--  datatypes
-- -----------*

data Flag  = Help
           | AddMacros FilePath
           | RemoveMacros FilePath
           | ShowMacros
           | FileToEdit FilePath
           | TextToEdit T.Text
           | IncludeNewline
           | StdIn
  deriving (Show, Eq)

options :: [OptDescr Flag] -- okay not really a datatype but it makes sense to have it next to where Flag is defined
options =
  [ Option "h" ["help"]            (NoArg  Help)                        "show Help message"
  , Option "m" ["macros"]          (ReqArg AddMacros            "FILE") "add a file to read Macros from"
  , Option "r" ["remove-macros"]   (ReqArg RemoveMacros         "FILE") "Remove a file to read macros from"
  , Option "s" ["show-macros"]     (NoArg  ShowMacros)                  "Show all macros that will be active"
  , Option "f" ["file-to-edit"]    (ReqArg FileToEdit           "FILE") "Tile of text to apply edits to"
  , Option "t" ["text-to-edit"]    (ReqArg (TextToEdit . T.pack)"TEXT") "Text to apply edits to"
  , Option "n" ["include-newline"] (NoArg  IncludeNewline)              "include Newline in changes done"
  , Option "s" ["stdin"]           (NoArg  StdIn)                       "edit text from Stdin even with -t or -t flag"
  ]

strFn :: (T.Text -> a) -> String -> a
strFn f s = f $ T.pack s

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

-- -------------------------------*
--  functions to work with Macros
-- -------------------------------*

-- take a list of flags and return a tuple of the list of the rest of the flags, and a list of files to read macros from
makeMacFileList :: ([Flag], [FilePath]) -> [Flag] -> ([Flag], [FilePath])
makeMacFileList acc [] = acc -- base(d)case
makeMacFileList (flagAcc, fileAcc) (flag:xs) =
  case flag of
    AddMacros file ->
      if file `elem` fileAcc then -- if that file is already in the file list
        makeMacFileList (flagAcc, (filter (/=file) fileAcc)<+>file) xs -- remove it before tacking it on the end
      else
        makeMacFileList (flagAcc, fileAcc <+> file) xs -- otherwise just tack it on the end
    RemoveMacros file ->
      makeMacFileList (flagAcc, (filter (/=file) fileAcc)) xs
    _ -> makeMacFileList (flagAcc <+> flag, fileAcc) xs

-- takes a filepath to look for macros, tries to read them, and tries to have them parsed
-- returns a Text value stating any errors that occured in parsing, and a Macros value
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
            return (f<>" parsed with no errors", m)
          else
            return (f<>" parsed with errors.\nerrors for "<>f<>":\n"<>err, m)
  else
    return (f <> " not found", M.empty)

-- takes the raw text of a file and generates a Text value listing any parsing errors, and a Macros of what could be parsed
parseMacs :: T.Text -> (T.Text, Macros)
parseMacs txt =
  (\(xs, ms)->(fxunlines xs, M.fromList ms)) $ -- convert list of errors to one big error, and list of (Text, Edit) to Macros
  partitionEithers $ -- convert list of Either error (Text, Edit) to (list of errors, list of (Text, Edit))
  catMaybes $ -- remove the nothings that represent commented and blank lines
  map parseMac $ -- convert each line into either an error, or a (Text, Edit)
  zip [1..] $ -- number each line
  T.lines txt -- break into list of lines of text

-- takes a single line from a macros file and produces either a Text value descri
parseMac :: (Int, T.Text) -> Maybe (Either T.Text (T.Text, Edit))
parseMac (i, txt) =
  if T.null txt || (T.head txt /= '"') then -- T.null on the left to avoid running T.head on txt if it's empty
    Nothing -- This line is either blank, or doesn't start with a quote so treat it as a comment
  else -- this line isn't empty and starts with a "
    case (finishQuo "" $ T.unpack $ T.drop 1 txt) of
      Nothing -> -- couldn't find a closing quote to get the name of the macro, retrn error
        Just (Left $ "could not parse "<>txt<>" on line "<>showT i<>".  No closing quote found for the macro's name.")
      Just (n, e) -> -- got the macro name!
        let (name, rawEd) = (T.pack n, T.pack e) in
          case parseEdit M.empty $ T.stripStart rawEd of -- try to parse this Edit with an empty Macro and remove leading whitespace
            Left err -> -- edit failed to parse
              Just (Left $ "Could not parse "<>txt<>" on line "<>showT i<>". Edit parse failed with the following error: " <> err)
            Right ed -> -- edit parsed successfully
              Just (Right (name, ed))

-- converts the list of all macros teh can see and prints them in a neat and orderly fashion
printAllMacros :: [(T.Text, Macros)] -> T.Text
printAllMacros = (T.unlines . map printMacros)

printMacros :: (T.Text, Macros) -> T.Text
printMacros (fande, macs) =
  T.unlines $ fande:(map printMacro $ M.toList macs)<+>"\n name   default target      changes\n"

printMacro :: (T.Text, Edit) -> T.Text
printMacro (name, (targ, chgs)) = name <> "     " <> showT targ <> "      " <> showT chgs


{-- this is ugly but I could make it nicer later maybe
-- -or maybe I just spent too long typing it and can't bring myself to delete it yet idk

printMacro :: (T.Text, Edit) -> T.Text
printMacro (name, (targ, chgs)) = name <> " will " <> (printChgs chgs) <> " on " <> (printTarg targ)

printChgs :: [Change] -> T.Text
printChgs [] = "whoops, somehow the function printChgs got an empty list of changes to print, that shouldn't happen.  \
             \ if you're me, then me should go fix it, if you're not me then please file an issue at github.com/guy-black/teh"
printChgs (chg:[]) = printChg chg
printChgs (chg:chgg:[]) = printChg chg <> ", and " <> printChg chgg
printChgs (chg:chgs) = printChg chg <> ", " <> printChgs chgs

printChg :: Change -> T.Text
printChg chg =
  case chg of
    Fr f r -> "find all "<>f<>" and replace them with "<>r
    Ins t i ->
      if i>=0 then
        "insert "<>t<>" after the first "<>showT i<>" character(s)"
      else
        "insert "<>t<>" after the "<>(showT $ abs i)<>"(st/nd/rd/th) char from the end"
    Rem s e ->
      if s>=0 then
        if e>=0 then
          "remove the next "<>showT e<>" characters after the first "<>showT s
        else
          "remove the previous "<>(showT $ abs e)<>" characters before the first "<>showT s
      else
        if e>=0 then
          "remove the next "<>showT e<>" characters after the first "<>(showT $ abs s)<>" chars from the end"
        else
          "remove the previous "<>(showT $ abs e)<>" characters before the first "<>(showT $ abs s)<>" chars from the end"

printTarg :: Target -> T.Text
printTarg Whole = "the whole text"
printTarg Each = "each line of text"
printTarg (Only is) = "only lines " <> printInts is <> " of text"

printInts :: [Int] -> T.Text
printInts [] = "whoops, somehow the function printInts got an empty list of ints to print, that shouldn't happen.  \
             \ if you're me, then me should go fix it, if you're not me then please file an issue at github.com/guy-black/teh"
printInts (i:[]) = showT i
printInts (i:is:[]) = showT i <> ",and " <> showT is
printInts (i:is) = showT i <> ", " <> showT is
--}

-- -----------------*
-- argument parsing
-- -----------------*

-- parses all -t and -f flags
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

-- --------------------------*
-- parsing edits and changes
-- --------------------------*

-- parses all edits
-- macros, list of edits to parse, acc errs,edits, final errs,edits
parseEdits :: Macros -> [T.Text] -> ([T.Text], [Edit]) -> ([T.Text], [Edit])
parseEdits _ [] acc = acc -- base case
parseEdits m (h:t) (eracc, edacc) =
  case parseEdit m h of
    Left err -> parseEdits m t (eracc<+>err, edacc)
    Right ed -> parseEdits m t (eracc, edacc<+>ed)

-- parses each edit giving either an error in Text or and Edit
parseEdit :: Macros -> T.Text -> Either T.Text Edit
-- to safely call head on the list from T.words on the Text value
parseEdit _ "" = Left "Cannot parse empty argument.  Please check that you properly formatted your argumens"
parseEdit macs txt =
  case macs !? txt of-- check if the whole argument is just a macro
    Just ed -> Right ed
    Nothing -> -- argument is not just a macro, it must start with a Target
      let t = head $ T.words txt -- head can fail if txt is only white space
          c = T.unwords $ drop 1 $ T.words txt in
        if any (t==) ["Whole","whole","W","w"] then  -- target is Whole
          case macs !? c of -- the change is just a macro
            Just (_,chs) -> Right (Whole, chs)
            Nothing -> -- change needs to be parsed
              case parseChgs macs c of
                Left err -> Left err
                Right chs -> Right (Whole, chs)
        else if any (t==) ["Each", "each", "E", "e"] then  -- target is Each
          case macs !? c of -- the change is just a macro
            Just (_,chs) -> Right (Each, chs)
            Nothing -> -- change needs to be parsed
              case parseChgs macs c of
                Left err -> Left err
                Right chs -> Right (Each, chs)
        else if any (t==) ["Only", "only", "O", "o"] then -- target is some Only, but need to find the numbers
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

-- wrapper function that calls the real Change parsing function
parseChgs :: Macros -> T.Text -> Either T.Text [Change]
-- handling an argument with no changes so parseChgs' doesn't have to
parseChgs _ "" = Left "Argument only has a target, arguments must include either a macro, or a target followed by either\
                       \ a macro or changes.  Please check the formatting of your argmuents"
parseChgs m t = parseChgs' m (wordsandquo t) []

-- the real function that parses Changes
parseChgs' :: Macros -> [T.Text] -> [Change] -> Either T.Text [Change]
parseChgs' _ [] acc = Right $ reverse acc
parseChgs' m (t:ts) acc =
  if any (t==) ["fr","FR","Fr"] then
    case ts of
      (a:b:cs) ->
        if T.null a then -- a must be non empty, b can be empty
          Left $ "first value passed to " <> t <> " must be nonempty"
        else
          parseChgs' m cs ((Fr a b):acc)
      _ ->
        Left $ "Error: " <> t <> " needs to be followed by two strings, but I found less than two"
  else if any (t==) ["ins", "Ins"] then
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

-- -----------------------*
-- text editing functions
-- -----------------------*

-- main entry point to the text editing functions
-- removes and returns any trailng \n and applies doEdit on the text for each edit to do
teh :: Bool -> [Edit] -> T.Text -> T.Text
teh _ [] txt = txt
teh nflag (x:xs) txt =
  -- not T.Null txt to the left of T.next will force the expression to resolve to false before erroring by calling T.last on ""
  if nflag && (not $ T.null txt) && (T.last txt == '\n') then
    -- on recurse set nflag to false as the trailing \n is already removed
    (teh False xs $ doEdit x $ T.dropEnd 1 txt) <> "\n"
  else -- either nflag is false or there is no trailing \n
    -- set nflag false becasue either it is false and there's no difference, or there is no trailing \n
    -- and setting it to false makes && resolve correctly to false witout needing to check last character
    -- also avoids ignoring any trailing \n added by previously applied edit
    teh False xs $ doEdit x txt

-- takeas an Edit and a Text and only applies the changes to the correct part of the text depending on Target
-- first checks if the edit has no changes, and if so ignores it all together
doEdit :: Edit -> T.Text -> T.Text
doEdit (_, []) txt = txt -- no changes means no edit to do
-- do changes just to the whole body of text
doEdit (Whole, chgs) txt =
  doChanges chgs txt
-- do changes to each individual line of text
doEdit (Each, chgs) txt =
  if (T.takeEnd 1 txt == "\n") then -- standard T.unlines will work fine
    T.unlines ( map (doChanges chgs) (T.lines txt))
  else -- need T.unlines will add a surprise newline
    T.dropEnd 1 $ T.unlines ( map (doChanges chgs) (T.lines txt))
-- only do changes to the numbered lines given to Only
doEdit (Only ns, chgs) txt =
  if (T.takeEnd 1 txt == "\n") then -- standard T.unlines will work fine
    T.unlines $ map snd(mapIf (\(x,y)-> (x,(doChanges chgs y))) (\(x,_)-> x `elem` ns) (zip [1..] (T.lines txt)))
  else -- need T.unlines will add a surprise newline
    T.dropEnd 1 $ T.unlines $ map snd(mapIf (\(x,y)-> (x,(doChanges chgs y))) (\(x,_)-> x `elem` ns) (zip [1..] (T.lines txt)))

-- unconcerned with target, only has a list of Changes to do and a Text to do them to
doChanges :: [Change] -> T.Text -> T.Text
doChanges chg txt = -- applying change to whole blob of text
  case chg of
    [] -> txt -- no more changes to do return final text
    (Ins tx n):chgs ->
      if ((T.length $ T.take (abs n) txt) == abs n) then -- a lazy way to check if the lenght of txt >= the index to insert txt
        if n >= 0 then -- counting forward
          doChanges chgs ((T.take n txt) <> tx <> (T.drop n txt))
        else -- counting backward
          doChanges chgs ((T.dropEnd ((abs n)-1) txt) <> tx <> (T.takeEnd ((abs n)-1) txt))
      else -- the index to inster goes out of bounds of the text to insert in
        doChanges chgs txt -- just skip skip the change
    (Rem a b):chgs ->
      if b==0 then
          doChanges chgs txt -- delete nothing
      else
        if a >= 0 then -- counting foward for skipped letters
          if b > 0 then -- deleting foward
            doChanges chgs ((T.take a txt) <> (T.drop (a+b) txt))
          else -- deleting back
            doChanges chgs ((T.dropEnd (abs b) (T.take a txt)) <> T.drop a txt)
        else -- counting back for skipped letters
          if b > 0 then -- deleting foward
            doChanges chgs ((T.dropEnd (abs a) txt) <> (T.drop b (T.takeEnd (abs a) txt)))
          else -- deleting back
            (T.dropEnd ((abs a)+(abs b)) txt <> (T.takeEnd (abs a) txt))
    (Fr find repl):chgs ->
      doChanges chgs (T.replace find repl txt)

-- -----------------*
-- helper functions
-- -----------------*

-- like T.words, but combines any group of words wrapped in quotes as a single word, preserving internal whitespace
wordsandquo :: T.Text -> [T.Text]
wordsandquo t = map T.pack $ waq [] [] $ T.unpack t
-- convert text to list of char, and feed it to real function with empty accumulator lists
-- then convert the results back into T.Texts

-- waq wAcc wsAcc txt
-- wAcc is the word accumulator
-- wsAcc is words accumulator
-- txt is the text that needs to be split
waq :: String -> [String] -> String -> [String]
waq wAcc wsAcc txt=
  case txt of
    [] -> -- if there's no text left to split -- all base cases
      if null wAcc then -- if the wAcc is empty don't append it to wsAcc
        wsAcc -- just resolve to wsAcc
      else -- wAcc is not empty
        wsAcc<+>wAcc -- resolve wsAcc with wAcc tacked on the end
    c:[] -> -- if txt only has one character left -- all base cases
      if c == ' ' then -- sike it was no character just a space
        if null wAcc then
          wsAcc
        else
          wsAcc<+>wAcc -- same logic as when txt is empty
      else -- txt only has one char left and it's not just a space, append that char to wAcc and append that to wsAcc and return
        wsAcc<+>(wAcc<+>c)
    h:hh:t -> -- txt is atleast two characters long, no more base cases, now for the messy logic
      if h == ' ' then -- h is a space, toss it and append wAcc to wsAcc if it's not empty and recurse
        if null wAcc then -- no wAcc to append to wsAcc
          waq wAcc wsAcc (hh:t)
        else -- there is a wAcc to append
          waq "" (wsAcc<+>wAcc) (hh:t)
      else if h == '\\' then -- hh is supposed to be escaped -- this broke other escape codes
        if (hh::Char) `elem` ("btnvr"::[Char]) then -- check if hh is an actual escape code
           if hh == 'b' then -- surely there has to be a better way to do this that covers all ofthe escape codes
             waq (wAcc<+>'\b') wsAcc t -- this
           else if hh == 't' then --      feels
             waq (wAcc<+>'\t') wsAcc t -- like
           else if hh == 'n' then --      the
             waq (wAcc<+>'\n') wsAcc t -- wrong
           else if hh == 'v' then --      way
             waq (wAcc<+>'\v') wsAcc t -- to
           else if hh == 'r' then --      do
             waq (wAcc<+>'\r') wsAcc t -- this
           else undefined -- we can only go down this branch if hh is in "btnvr" so it has to be
                          -- either a b, t, n v, or r.  I feel fine leaving this as undefined
        else waq (wAcc<+>hh) wsAcc t -- append hh to wAcc, leave wsAcc alone, and recurse with t
      else if h == '"' then -- a quote is starting, let's try to get it
        case finishQuo "" (hh:t) of
          Nothing -> -- no closing quote found in the rest of the txt almost a base case, see comment below for why not
            waq (wAcc<+>h) wsAcc (hh:t)
          Just (quo, rst) -> -- got the rest of the quote! recurse with wAcc (unless it's empty) and quo appended to wsAcc
            if null wAcc then
              waq wAcc (wsAcc<+>quo) rst
            else
              waq "" (wsAcc<+>wAcc<+>quo) rst
      else -- h is just a regular old char, append it to wAcc and recurse
        waq (wAcc<+>h) wsAcc (hh:t)

{--
 On why Nothing from finishQuo isn't a base case after all
 I also accidentally introduced the ability to escape a space which I kinda like and would break by
 making this a base case, so instead I will treat the '"' in h as if it were escaped
--}

-- meant to be called on a portion of a string following an opening quote
-- returns Nothing if closing quote cannot be found
-- retursn Just (String, String) with the rest of the quote, and the rest of the text
-- if an unescaped closing quote can be found
-- takes an accumulator string, and the string to find the closing quote in
-- and returns either Just (quo, rst) or if no closing quote can be found, nothing
finishQuo :: String -> String -> Maybe (String, String)
finishQuo _ "" = Nothing -- got to the end of the string with no closing quote found, return nothign -- base case
finishQuo wAcc ('"':txt) = Just (wAcc, txt) -- we found the end of the quote! -- base case
finishQuo wAcc ('\\':c:txt) = -- there are atleast two chars left but the first is an escape character
  if (c::Char) `elem` ("btnvr"::[Char]) then -- check if hh is an actual escape code
    if c == 'b' then --              this
      finishQuo (wAcc<+>'\b') txt -- may
    else if c == 't' then --         or
      finishQuo (wAcc<+>'\t') txt -- may
    else if c == 'n' then --         not
      finishQuo (wAcc<+>'\n') txt -- be
    else if c == 'v' then --         a
      finishQuo (wAcc<+>'\v') txt -- cry
    else if c == 'r' then --         for
      finishQuo (wAcc<+>'\r') txt -- help
    else undefined -- we can only go down this branch if hh is in "btnvr" so it has to be
                          -- either a b, t, n v, or r.  I feel fine leaving this as undefined
  else finishQuo (wAcc<+>c) txt -- throw away the \, tack the next char onto wAcc and recurse
finishQuo wAcc (c:txt) = -- we found another character, but it's not the closing quote
  finishQuo (wAcc<+>c) txt -- tack it on to the wAcc and recurse

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
extArgs' f (wnt, othr) (h:hh:rst) =
  if f h then -- checking if we want to add to the wanted accumulator
    extArgs' f (wnt<+>(h,hh), othr) rst -- we found what we want so we drop the flag and the argument and recurse with what's left
  else
    extArgs' f (wnt, othr<+>h) (hh:rst) -- not a flag we wanted, add it to the others and recurse

showT :: Show a => a -> T.Text
showT = T.pack . show

readMaybeT :: Read a => T.Text -> Maybe a
readMaybeT = readMaybe . T.unpack

putTxt :: T.Text -> IO ()
putTxt = putStr . T.unpack

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

-- for some silly reason T.unlines adds a \n to the end of the new text.
-- which works perfect if the [T.Text] came from applying T.lines to a T.text
-- that ends with a newline, but starts adding mysterious newlines out of no
-- where for a [T.Text] that came any other way
fxunlines :: [T.Text] -> T.Text
fxunlines = (T.dropEnd 1 . T.unlines)

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
help = "if you're seeing this that means I forgot to update the help text oops go bug me on github about it"
--   "Usage: teh [options]\n\
--   \   by default teh reads from StdIn for text to apply changes to.  To disable this either pass the --nostdin flag or\n\
--   \   pass a filename as the final argument \
--   \   -h will bring you this menu and --listmacros will list all macros found in the config folder and in the local directory\
--   \   macros can be written in the .teh file in a directory or in your xdgconfig directory\
--   \   commands are written in the form of 'Ch <Whole, Each, Only [1, 2, 3]> (<Fr \"find\" \"replce\", Rem 0 1, Ins 0 \"insert\">)'\
--   \   See README for more details "
