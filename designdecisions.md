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
