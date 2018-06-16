## An Emacs brainfuck interpreter 

A small work-in-progress project to create a brainfuck interpreter for emacs, because "why not?".

This is mostly an exercise to refresh my elisp knowledge so don't expect much from this project.

## Status

I'm finishing the operators (yeah, I know it's like 10 lines of C to do it, but did I mention I'm very lame with emacs lisp?), then if the project is entertaining enough, I'll consider adding a major mode with a visual memory debugger.

## How to use it

You can use the command `M-x brainfuck-interpret` to read the program at point and get the status of the memory at the end. 

Any message is printed in the *Message* buffer for now.

The interpret ignores all invalid characters and reads until the end of the buffer.
