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
to remove text

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

## Installation

## features left to add

These are feature I want to have working in order for this project to feel completely complete, in order of how much I want it


- Stream editing
- variable in macro names
- Configurable `--show-macros` behavior
- syntax to access overridden macro
- `--show-macros` to visually indicate a macro is overridden
- reference other macros while defining macros
