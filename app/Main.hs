{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getArgs)
import System.Directory (doesFileExist, XdgDirectory(XdgConfig), getXdgDirectory)
import Text.Read (readMaybe)
import qualified Data.Map as M    -- for M.Map
import Data.Map ((!?))
import Data.List (isSuffixOf, sortOn)
import Data.Either (partitionEithers)
import Data.Char (isSpace, isDigit)
import qualified Data.Text as T
import GHC.IO.StdHandles (stderr)
import GHC.IO.Handle (hPutStr)

main :: IO ()
main = do
  args <- getArgs >>= (return . (map T.pack)) -- get args passed in
  confDir <- getXdgDirectory XdgConfig ".teh" >>= (return . T.pack) -- get path for where the user .teh conffile would be if it exists
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

data Proceed = WithStdIn [Edit]
             | WoStdIn ([Edit], T.Text)
             | Stop Cause

data Cause = Help
           | Macs
           | Penguin
           | ArgParseErr T.Text

printMacros :: (T.Text, Macros) -> T.Text
printMacros _ = "I'll do it this afternooooon"
-- eventually this will be replaced with a funtion that will generate a pretty piece of text to show all in scope macros and any parse errors
-- for now it just kind of sorta technically works if you squint a bit and don't enjoy life
-- printMacros :: ((T.Text, M.Map T.Text [ Change ]),(T.Text, M.Map T.Text [ Change ])) -> T.Text
-- printMacros ((cErr,cMac),(lErr,lMac)) = ("from <xdgconfig>/.teh\n"<>cErr<>(showT cMac)<>"\n"<>"from <cwd>/.teh\n"<>lErr<>(showT lMac))

-- if no value within ls is True for b then
--   takeUntil b ls == ([], ls)
-- if some value a within ls is True for b then for the first a
--   takeuntil b ls == (prea<+>a,posta)
takeUntil ::  (a -> Bool) -> [a] -> ([a],[a])
takeUntil b ls =
  case break b ls of
    (_, []) -> ([], ls)
    (pre, (r:st)) -> (pre<+>r,st)

showT :: Show a => a -> T.Text
showT = T.pack . show

readMaybeT :: Read a => T.Text -> Maybe a
readMaybeT = readMaybe . T.unpack

readT :: Read a => T.Text ->  a
readT = read . T.unpack

putTxt :: T.Text -> IO ()
putTxt = putStr . T.unpack

parseargs :: Macros -> [T.Text] -> Either T.Text [Edit]
parseargs macs ts = parseargs' macs ts []

parseargs' :: Macros -> [T.Text] -> [Edit] -> Either T.Text [Edit]
parseargs' _ [] eds = Right $ reverse eds
parseargs' macs (t:ts) eds =
  case parsearg macs t of
    Left err -> Left err
    Right ed -> parseargs' macs ts (ed:eds)

parsearg :: Macros -> T.Text -> Either T.Text Edit
-- to safely call head on the list from T.words on the Text value
parsearg _ "" = Left "Cannot parse empty argument.  Please check that you properly formatted your argumens"
parsearg macs txt =
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
parseChgs _ "" = Left "Argument only has a target, arguments must include either a macro, or a target followed by either a macro or changes.  Please check the formatting of your argmuents"
parseChgs m t = parseChgs' m (wordsandquo t) []

parseChgs' :: Macros -> [T.Text] -> [Change] -> Either T.Text [Change]
parseChgs' m [] acc = Right $ reverse acc
parseChgs' m (t:ts) acc = -- undefined
  if any (t==) ["fr","FR","Fr"] then
    undefined
    -- this is a find and replace
    -- the first two Texts of ts MUST start and end with "quotation marks"
    -- if yes strip the marks of them and pass them to the Fr constructor and recurse with it appended to  acc
  else undefined -- to make ghc happy until I finish this function

-- like T.words, but concatenates a word that start's with '"' with all subsequent words until found a word that ends
-- with '"' only if it's not preceded by a '\'
wordsandquo :: T.Text -> [T.Text]
wordsandquo t =
  let (pre, rst) = break (\x->T.take 1 x == "\"") $ T.words t in
    if rst == [] then -- there are no words that start with a quote, T.words will work fine
      T.words t
    else -- there are quotes
      case takeUntil (\x->takeEnd 1 x == "\"") rst of -- take words up to the first one to end with a "
        ([], _) ->  -- there was no closing quote, just treat as regular words
          T.words t
        (quo, aftquo) -> pre <> [(T.unwords quo)] <> (wordsandquo $ T.unwords aftquo)

tmptxt :: T.Text
tmptxt = "this is a Text value with no quotes"

tmptxt2 :: T.Text
tmptxt2 = "this is a \"Text typed value\" with \"no quotes\" ;)"

tmptxt3 :: T.Text
tmptxt3 = "this is a text value with a \"quote that never closes oh nooooo"
