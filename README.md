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

changes can either be `fr <"text"> <"text">`, `ins <"text"> <num>` to insert text at a certain spot, or `rem <num> <num>`
to remove text

## Macros

## features left to add

These are feature I want to have working in order for this project to feel completely complete, in order of how much I want it

- Stream editing
- Configurable `--show-macros` behavior
- syntax to access overridden macro
- `--show-macros` to visually indicate a macro is overridden
- reference other macros while defining macros
