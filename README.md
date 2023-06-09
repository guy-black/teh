# teh

Because sed gets clunky and unpleasant for anything more than a find and replace.  Teh is meant to be much simpler to use for common things I wish I knew how to use sed for without googling how to do blank in sed and sifting through so much cruft instead of just getting back to what I was sediting text for anyway

TODO

  Macro are now Map Text Edit
  Edit is just newtype of ([Change], Target)
  macro can still be read in from .teh and read with the read instance
  new syntax will look like in .teh2

  need to write a funtion to parse each arg passed better than just read

  before all this strip all white space
  first check and see if arg == some index in macros
    if so load that macro else
  then check if arg can be read as a [Change]
    if so read it as the list of changes with the def target
  try to read a target from beginning of list
    if found,
       try to read rest as [ Change ]
        if so return that [Change] with the Target
        if not return error
    if not return error

lookForTarget :: Text -> Text -> Maybe Target
lookForTarget _ "" = Nothing
lookForTarget pre post =
  case (readMaybe (pre <> (T.head post))::Target) of
    Nothing -> lookForTarget (pre<>(T.head post)) (T.tail post)
    Just t -> Just t

  make a more substantial default .teh file
    -- bracketing, square bracketing, shaving off an amount, code comments
  polish this readme

TODO but like later in the future

  give Change a recursive constructor to refer to a macro or something?
    just want a macro to be able to call another macro in it's changes

  pretty printing funtion to show list of all in scope macros or errors associated with
    macro files to find
    solution: improve printMacros function

  better syntax for writing out macro files and parse error messages
    solution: write a proper parser for a simpler syntax and use it to replace readMaybe in seekMacs

  flag to add another place to read a macro file and add it's errors to --listmacros

  paramaterized macros



***** note ******
switch back to stack lts resolver when ghc verstion hits 9.2.8
