# teh

Because sed gets clunky and unpleasant for anything more than a find and replace.  Teh is meant to be much simpler to use for common things I wish I knew how to use sed for without googling how to do blank in sed and sifting through so much cruft instead of just getting back to what I was sediting text for anyway

TODO
  fill out help text
  move from String to Text
  add confErr and localErr to what --list shows
  allow comments in conf/ignore any lines that start with #


TODO but like later in the future

  give Change a recursive constructor to refer to a macro or something?
    just want a macro to be able to call another macro in it's changes

  pretty printing funtion to show list of all in scope macros or errors associated with
    macro files to find

  better syntax for writing out macro files and parse error messages

  flag to add another place to read a macro file and add it's errors to --listmacros

  paramaterized macros

