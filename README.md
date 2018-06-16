## An Emacs brainfuck interpreter 

A small work-in-progress project to create a brainfuck interpreter for emacs, because "why not?".

This is mostly an exercise to refresh my elisp knowledge so don't expect much from this project.

### Status

The operators should (kind of) work, and if the project is entertaining enough, I'll consider adding a major mode with a visual memory debugger.

There is some interactive highlighting: when the program run the code is highlighted as it's interpreted.

You can set the speed of the interpreter with the variable `brainfuck-interpret-delay`.

This code:

    (setq brainfuck-interpret-delay 0.5)
    
will slow the interpret to a symbol evaluation every ½ second.

Characters that are outside the range `+-><[].,` are ignored, which is neat for commenting the code (and God knows if you'll need to…).

### How to use it

You can use the command `M-x brainfuck-interpret` to read the program at point and get the status of the memory at the end. 

Any message is printed in the *Message* buffer for now.

The interpret ignores all invalid characters and reads until the end of the buffer.

### License

This code is provided "as it is", is unfit for any use or purpose, not covered by any warranty, and is released under the GNU GPL version 3, which I'm pretty sure you can Google yourself.

