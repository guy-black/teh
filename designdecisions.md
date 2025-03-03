The document that used to be my TODO list for this project.

it now stands and as a document of design decisions I've made while working on the project and ideas regarding features to add later.
mostly just here for my own reference for the next time I need to work on this project after being away for so long and don't
remember why I made the choices I made while making this



# ideas on how to implement a stream editing mode
  - if you have a command that regularly prints data to stdout (eg vmstat 1 or exoutput) and pipe it into teh
    teh should apply edits to every thing it prints to stdout in real time then print it to std out
  - forever $ getLine >>= (return . T.pack) >>= (\x -> putTxt $ teh nflag pEdits x)` *kinda* works but only if:
    - the target is Each and
    - any rm or ins are done relative to the begining of the line
  - maybe could be done using hClose and Control.Concurrent.threadDelay, idk need to learn more how concurrent haskell works

# misc notes on necessity of -f flag
- but then what if the last and only argument is both a readable filename and a macro?
  this could mean apply that macro to all stdin or use it's contents as text to edit with no edits
  I could only read last arg as text to edit if there is more than one argument so if it's meant to be a macro
  it could be moved back to avoid confusion, but then what if the edits need to be a particular order?
  So I should add a -f flag to indicate a file to be read from

# thought on the trouble of adding a wrap around

so if we have teh -t foobar 'Whole Rem 3 5'
- without -w the standard behavior is clear, skip the first three chars, foo, and delete the last three, bar and
  stop there since nothign is left leaving only foo
- and with -w it's still clear we should skip foo and delete the last three bar, then for the last two wrap around and
  delete the first two characters fo leaving only o
but what if we have teh -t foobar Rem 8 5
- the 8 chars to skip exceeds foobar length by two, should the offset also loop around thust starting between fo and obar?
  or should it be a no op for starting outside the bounds of the word?

  if the off set is placed between fo and obar should all of obar be removed and then loop around to remove the f too leaving only o?


  at that point what is the practical point of -w?  it feels like creating unpredictable behavior just for the sake of adding a
  new feature to this genreal text modifying algorithm.  I can't personally see a reason why I'd prefer
    teh -w -t foobar 'Whole Rem 4 3' over teh -t foobar 'Whole Rem 0 1 Rem 4 2'
  I suppose for determined -t value the -w allows me to remove a change from the list and still be pretty sure of what's expected
  but in general the command without -w feels more explicit and unambiguous about the intended outcome for most noncontrived
  situations.

  If anyone says they think the -w flag would be helpful and would like to define clear rules for how it should behave then
  I will implement it.  Hell I'd be over the moon if anyone else even finds any use in this little tool at all.  For now
  I don't think I can gain any use for a -w flag so I'm scrapping that whole idea for now pending the existence of other people
  using this tool and wanting such a feature

# thoughts on feature ideas I scrapped

  "don't require target if input is only 1 line", adding w as target is way simpler than dealing with input from
  something else suddenly having more than one line

  "syntax to access overridden macro" no, if you need to do this you're doing something wrong to beginwith



# thoughts on improved argument parsing
- remove ignore errors option all together.  if there's an error in your command it will cause unpredictable behavior

- new flags will be:

```
-h, --help: show the help and exit
--penguin: we like to have fun here okay :P
-m, --macros=<path/to/macro/file>: adds a file of macros to list of macros files to use, errors if file can't be found

-r, --remove-macros=<path/to/macro/file>: removes a file from list of macrso files to use, does nothing if the file isn't listed

*note on macros files*
  by default the list of macros files to use is ["$XDG_CONFIG/.teh", "./.teh"]
  - -m and -r flags are added and removed to the list sequentially
  - if multiple files list the different macros under the same name, files later in the list override earlier
  - if a macro file shows up in the list multiple times, the last instance is kept and earlier ones are removed

-S, --show-macros: displays the final list of macros file in the order they are read in, and the list of macros that are available

-f, --file-to-edit=<path/to/file>: path to a file to edit, errors if file can't be found
  note: this does not change the file, reccomended usages are:
    teh -f filetoedit [edits] > filetoedit.new # create a new file with changes
    teh -f filetoedit [edits] > filetoedit     # actually edit the file

-t, --text-to-edit=<text>: a block of text to edit
  ex: teh -t "this text will be edited"
      teh -t $(someCmd) #edit the output of that command

-n, --include-newlines: include trailing newline at the end of text to edit.
  - by default, if there is a newline at the end of a line or chunk of text to edit, it will be removed before the changes
    are done and replaced after, with this the newline is part of the changes

future (-g, --generate-sed-cmd: don't edit any text just generate a corresponding sed cmd)

-s, --stdin: edits text from stdin. Only necesaary if -t or -f flags used, otherwise this is default behavior
```

# order of operations
first the flags are parsed by getOpts

if -h, --help are present then ignore all other flags, print that flag's info, and close without parsing macros

else generate list of macros files and list of macros to use in accordance to -m, -r, --macros, and --remove-macros flags

if any macros files can't be found or can't be parsed then exit early and display appropriate error

else if -S, or --show-macros is present then ignore all other flags and show the dang macros and exit

else -t or -f flags are present then generate list of texts to edit, if any -f files can't be found then exit early with error

now parse all non-option arguments as edits to perform

if parse errors occur then exit early with the appropriate error

else iterate over every text to edit

  if -n and the text ends in a newline then
    - remove trailing newline
    - edit text
    - add the newline back
    - print the new text to stdout

if -s or --stdin, or no texts to edit, edit any text from stdin "respecting -n flag", and print it to stdout


# parsing edits
  after using getOpt the remaining args should be edits

  if no args after getOpt, then there are no edits, exit early with help message

  else check if first word matches with valid target "whole, w, each, e, only, o"

    if no check if it's a valid macro

      if yes check if next arg is a number

        if no append that macro to list of edits, start parsing over with rest of args

        if yes append that macro to the list of edits that number of times recure on args and keep parsing

      else exit early with a parse error "expected target or macro but instead found <wrongword>"

    if yes and the target is only or o check if next argument is a number

      if yes then read all numbers until you find none number to complete the only target, skip to parse a change below

      else exit with error "expected numbers for target only but instead found <not number>

    else if yes then make that the target and check if next arg is a valid change "fr, ins, rem"

      if not check if macro
