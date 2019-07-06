# pmd, the poor man's debugger #

## Purpose ##

Small package to automate the tedium of inserting debug-print statements in the code. It includes an interactive function that accepts a mini-language for printing multiple variables.

![Visual explanation](https://github.com/dcluna/pmd.el/blob/master/screencast.gif)

## Modifiers ##

### Require comma escape ###

The command splits input whenever it sees a comma. If you want to check input where the comma is significant (i.e., in Ruby, if you want to inspect an array like "[a,b,c]" as an unit, you don't want it to break on every comma), use the prefix "re", which will only break when it sees a slash followed by a comma ("\,").

### Generate input from code ###

The modifiers "el", "sh", "rb" and "pl processes the given input as an elisp/shell/ruby/perl program. Useful for generating longer sequences that follow a pattern with less typing.
