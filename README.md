## An Emacs brainfuck interpreter 

A small project to create a brainfuck interpreter for emacs, because "why not?".

This is mostly an exercise to refresh my elisp knowledge so don't expect much from this project: there are at least 2 other brainfuck interpreters in the emacs packages repositories :)

### Status

There is a brainfuck major mode and a hook associated with `.bf` files.

The operators work, but the output is ugly and needs work, also there is a nice memory visualization buffer.

There is some interactive highlighting: when the program run the code is highlighted as it's interpreted.

The interpreter highlights the currently used symbol and the memory cell it's on.

You can set the speed of the interpreter with the variable `brainfuck-interpret-delay`.

This code:

    (setq brainfuck-interpret-delay 0.5)
    
will slow the interpret to a symbol evaluation every ½ second.

Characters that are outside the range `+-><[].,` are ignored, which is neat for commenting the code (and God knows if you'll need to…).

### How to use it

You can use the command `M-x brainfuck-interpret` to read the program at point and get the status of the memory at the end. 

The `,` and `.` operators read from the minibuffer and write to the `*Messages*` buffer.

The interpret ignores all invalid characters and reads until the end of the buffer.

### License

This code is provided "as it is", is unfit for any use or purpose, not covered by any warranty, and is released under the GNU GPL version 3, which I'm pretty sure you can Google yourself.
