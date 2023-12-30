# teh

## A simpler sed for simpler minds.

Some among us are Unix chads that have committed the entirety of sed's man page to memory and can fearlessly type out a
sed command in the middle of a shell script, and that sed command will do exactly what you wanted it to do the first time.

For the rest of us, I made teh.

Anything teh can do sed can also do, and sed can do a lot of things teh cannot do, but teh can do most of the things that
I use sed for on a regular basis, and for anything that can be done by teh, the way to do it should be fairly straightforward
and obvious with no need to reread any instruction manual or ask the internet, and it'll probably do what you wanted it
to do the first time you try it.

## How to use

`teh [options] [edits]`

tehs options are:

`-n` : ignore any trailing newline on text being edited
The newline will be removed before edits are applied, and added back after all edits are done.

`-ie` : ignore errors.  By default if there is an error parsing any arguments or edits when running teh it will exit early
and tell you what you did wrong.  passing `-ie` tells teh to ignore any errors and just do what it can with what isn't an error

`-f <path/to/file>` : instead of editing text from stdin, edit text from a given file and print the results to stdout

`-t "text to edit"` : instead of editing text from stdin, edit any text passed as an argument after this and print the results
to stdout.  You can use as many `-f` and `-t` flags as you like, text will be processed and output in the order given

`--stdin` : if you use `-t` or `-f` flags, `--stdin` will also edit text from stdin after all `-t` and `-f` flags

`-c <path/to/macro/file>` : You can create .teh macro files to common edits you may use to use more quickly.  By default
teh will look for .teh files in `$XDG_CONFIG/.teh` and `./.teh`

`-ic <pact/to/macro/file>` : a macro file to ignore.  teh generates a list of locations to look for .teh files starting with
the defaults and then the `-c` flags in the order they're passed in.  after that any files matching a `-ic` flag are removed

`--show-macros` : shows a list of all macro files in scope and any errors that occored while parsing

`-h` show help text

teh edits can be either just a macro, a target followed by a macro, or a target and a list of changes

targets can be either `whole` to apply changes to the whole text, `each` to apply changes to each line of text, or  `only
<some space separated numbers>` to only apply changes to certain lines of the text

changes can either be:
 - `fr <"text"> <"text">`
 `FR`, `Fr`, or 'fr` is the simplest change, a simple find and replace.  The first bit of text after it is what text to look for, and the next is what text to replace it with.

 - `ins <"text"> <num>`
 `Ins` or `ins` takes a piece of text to insert, and an offset to insert it at.
  If the offset 0 or greater, that is the number of characters to skip before inserting the text ex. an offset of 0 will place the text at teh beginning, and an offset of 3 will insert the text after the first 3 chars
  If the offet is a negative number then the text will be placed after the absolute value of the offset charactes from the end. ex and offset of -1 will place text after the first character from the end which is the last character so at the end of the text.  an offset of -3 will place the text after the 3rd character from the end, so right before the last two characters

 - `rem <num> <num>`
  `Rem`, `rem`, `Rm`, or `rm` takes a number for offset, and a number of characters to delete.
  Like with ins, if the off set is negative it will start from the absolute value of the offset characters from the end.  An offset of -1 will start before the last character, an offset of -3 will start from before the 3rd character from the end
  If the number of characters to delete is >0 it will delete that many characters to the right from the start, and it will delete to the left for negative numbers.

## Example usage

Let's use the first section from this README as sample text to edit.

```
Some among us are Unix chads that have committed the entirety of sed's man page to memory and can fearlessly type out a
sed command in the middle of a shell script, and that sed command will do exactly what you wanted it to do the first time.

For the rest of us, I made teh.

Anything teh can do sed can also do, and sed can do a lot of things teh cannot do, but teh can do most of the things that
I use sed for on a regular basis, and for anything that can be done by teh, the way to do it should be fairly straightforward
and obvious with no need to reread any instruction manual or ask the internet, and it'll probably do what you wanted it
to do the first time you try it.
```

You could have this text saved to a file and run `teh` with a `-f path/to/this/file` flag
You could run `teh` with a `-t "..."` flag and paste the text in where I put the ellipsis
You could echo the text or cat a file with the text and pipe it in to `teh -n`
Or you could do what I'm doing to write this section, and the usage I originally had in mind for this, highlight the text
in your favorite text editor and pipe it into `teh`.

You could replace all of "sed" with "GNU sed" with `teh 'w fr sed "GNU sed"'`
```
Some among us are Unix chads that have committed the entirety of GNU sed's man page to memory and can fearlessly type out a
GNU sed command in the middle of a shell script, and that GNU sed command will do exactly what you wanted it to do the first time.

For the rest of us, I made teh.

Anything teh can do GNU sed can also do, and GNU sed can do a lot of things teh cannot do, but teh can do most of the things that
I use GNU sed for on a regular basis, and for anything that can be done by teh, the way to do it should be fairly straightforward
and obvious with no need to reread any instruction manual or ask the internet, and it'll probably do what you wanted it
to do the first time you try it.
```

You could wrap the 2nd, 4th, and 8th lines in <angle brackets>
`teh 'o 2 4 8 ins < 0 ins > -1'`
```
Some among us are Unix chads that have committed the entirety of sed's man page to memory and can fearlessly type out a
<sed command in the middle of a shell script, and that sed command will do exactly what you wanted it to do the first time.>

<For the rest of us, I made teh.>

Anything teh can do sed can also do, and sed can do a lot of things teh cannot do, but teh can do most of the things that
I use sed for on a regular basis, and for anything that can be done by teh, the way to do it should be fairly straightforward
<and obvious with no need to reread any instruction manual or ask the internet, and it'll probably do what you wanted it>
to do the first time you try it.
```
You could also remove the first four characters from each line, then add 4 asterisks to the start of each line and four question marks to the end with
`teh 'e rm 0 4 ins **** 0 ins ???? -1`
```
**** among us are Unix chads that have committed the entirety of sed's man page to memory and can fearlessly type out a????
****command in the middle of a shell script, and that sed command will do exactly what you wanted it to do the first time.????
****????
****the rest of us, I made teh.????
****????
****hing teh can do sed can also do, and sed can do a lot of things teh cannot do, but teh can do most of the things that????
****e sed for on a regular basis, and for anything that can be done by teh, the way to do it should be fairly straightforward????
****obvious with no need to reread any instruction manual or ask the internet, and it'll probably do what you wanted it????
****o the first time you try it.????
```

All of these examples are very contrived just to demonstrate all targets and types of changes that can be done, and would also be a bit clunky to type out regularly if you really had to.
To see how to make clunky teh commands less clunky keep reading to the Macros section, or to see more realistic uses of `teh` look at the example .teh file in the reposiory for practical macros.

## Macros

Macros are a way to save an edit to a single word.  For example to parenthesize a body of text you might use the command:
`teh 'Whole Ins "(" 0 Ins ")" -1'`
Over the `whole` body of text `insert "("` before `0` characters from the start then `insert ")"` before `1` character from the end
If you wanted to make the command shorter you could add the following to your macros
`"par" Whole Ins "(" 0  Ins ")" -1`
Then you could parenthesize a whole portion of text using the command:
 `teh par`
You can also override the default target and parenthesize each line with
 `teh each par`

You can see an example of a teh macro file in the `.teh` file in the root directory of this repo.
By default teh will look for macros first in your `$XDG_CONFIG/.teh` and then in`./.teh`, and then in any location you pass in with a -c flag.
If two different macros are defined in multiple file then the last one checked will override the others.  For example, if you have  macro named "example" in `XGD_CONFIG/.teh`, and `./.teh`, then the version in `./.teh` will be used.
If you also define it in another file that you pass in with a `-c` flag then that version will be used

## Installation

If you don't already have it, [install Stack](https://docs.haskellstack.org/en/stable/#how-to-install-Stack).
Once you have Stack installed, clone this repo and run `stack install` from within the root of this directory.

## features left to add

These are feature I want to have working in order for this project to feel completely complete, in order of how much I want it


- Stream editing
  - If you have a command that regular prints something to `stdout` like `vmstat 1`, I want to pipe that command into teh and edit every block of text as I gets printed.
  - I suspect this is tricky becaue haskell does IO lazily and expects to read in all stdin before doing any computations on it. I've made it work when the target is Each and rm and ins are relative to the start of the line, but that's not good enough
  - If you know how to Haskell and have ideas on how to implement and could at least give me a nudge in the right direction I'd be HUGELY greatful
  - Everything else on this list I do feel like I could implement and is more of a roadmap of stuff to do when I get back around to this.

- `--show-macros` to visually indicate a macro is overridden
  - when you run `teh --show-macros` it shows parsed macros from lowest priority to the highest priority
  - ex. if you have two different macros with the same label the one that will take precedence is the one that gets printed last
  - I would like to be able to mark a macro as overridden if another macro further down has the same name

- syntax to access overridden macro
  - if you have a macro that is overridden, but you absolutely need to use it without changing which macros are inscope, give a way to preface the name of the macro to specify which file you want to pullfrom for it

- edit repeats
  - for instance look at the macro "in" for indenting from the examples
  - "in"     Each  Ins " " 0
  - `teh in` will indent each line by one space, `teh in in` by two, `teh in in in` by 3, etc etc.
  - I want to add a repeating syntax something maybe like
  - `teh in *5` or something that will be shorthand for `teh in in in in in`
- other changes
  - like `reverse x y` to reverse all of the characters from x to y in the text.
    - eg `reverse 1 -1` to reverse the entire text
    - `reverse 3 10` to reverse from the second character to the 10th caracter
  - `swap x1 y1 x2 y2` to swap all characters from x1-y1 with all the character from x2-y2


If there are any other features you think would improve this project or find any bug please do open an issue to let me know and I'll see if I can implement or fix it!
