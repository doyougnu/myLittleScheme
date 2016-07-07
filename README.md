# myLittleScheme
A Scheme implementation writtin in Haskell 
done by following this wikibook: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

## Thoughts at the end
If you somehow found your way here with an interest in the book I would 
recommend against it. The book is seriously outdated, the exercises stop after
chapter 5 and so does any explanation. I really felt like the book should be
about twice as long as it is. It simply just isn't enough even for an 
intermediate Haskeller such as me.

Furthermore the book never deals with any sort of tooling that a serious haskell
project would need in order to be "real world". Its almost as if the author
never thought anyone would use stack or cabal (stack my not have been around
at the time of writing). I tried to place the project in a stack directory to
get a good feel for how a project of this magnitude would be represented by 
stack. I was disappointed, the author assumes that the reader is writing one
monolithic file. So placing the parser and the evaluator in seperate files and
modules creates a ton of dependency issues. That is when I decided to stop the
working through the book. Lastly the author overuses "($)", they consistently
write code like this: func $ func arg $ arg, instead of: func . func arg $ arg
perhaps this is also due to how outdated this book is.
