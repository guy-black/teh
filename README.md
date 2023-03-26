# teh

Because sed gets clunky and unpleasant for anything more than a find and replace.  Teh is meant to be much simpler to use for common things I wish I knew how to use sed for without googling how to do blank in sed and sifting through so much cruft instead of just getting back to what I was sediting text for anyway

TODO
  keep reworking Change for all constructors to have a [What] to be done to the lines

  macros should instead be a Map Text ([ What ], ([What] -> Change))
  a Text label mapped to a list of What paired with a default Constructor
  to make a Change if not already given
  may need to make a separate datatype called chgCons with instances of Read/Show
  to make into the ([What] -> Change) type value
  this way a macro can be specifically applied to either Whole Each or Only, for
  versatility, but also have a defualt constructor to use
  ie the paren macro should default to Whole if not specified for Each or Only

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

