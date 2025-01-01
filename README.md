# wyovm

[source here](https://www.jmeiners.com/lc3-vm/)


status: LDR was broken, should've tested it. Hello World now works!

## Lessons

 - `Index` and `IndexMut` are really handy.
 - Break down functions so they can be unit-tested.
 - Extension traits, and how we can get around the orphaned trait rule.
 - Shifting & masking, but need more practice.
 - ~~Two's complement requires a *lot* more reading and practice.~~ I get it now, it takes advantage of wrapping adds to work seamlessley between signed and unsigned.

## Remarks

 - I'm spoiled a bit by codecrafters, and their piecemeal tests for quick wins. I've tried to replicate this by testing my code (hey maybe there's a good lesson in that). However, this leaves an impasse, am I testing what *I* think is correct, or what is *actually* correct.
 - The book's use of clever C code is a little unhelpful if you're not learning C. Sure, these are normal and code you'd expect to see if you're reading C, but for those wanting to just, write your own virtual machine, it's a bit of extra friction.
