# MS Basic Interpreter in Scala #

Following the Z80 simulator in Scala (https://github.com/krzys9876/z80_sim_scala), this is 
a MS Basic interpreter to run similar programs. As always it's mostly for fun and for practicing 
FP, immutability, class composition and parser combinators.

I did not intend to implement all Basic commands - the list is quite long but I realised that
actually most of my example programs use just a small portion of all available statements. 
So I chose the core: printing, loop, jumps, conditionals.

My implementation is not strict, e.g. I did not put constraints on numbers, string length, 
printing is a little different too bit overall the programs do what they are intended to.
I did not implement string operations at all as I hardly use them in Basic.

I've spent quite some time on parser combinators, learning and browsing for simple solutions.
I tried hard not to create a huge one-class-to-parse-them-all. This is where scala traits shine - 
they allow separating logical bits from one another yet combining them seamlessly (just be aware of
circular dependencies).

## Keywords and functionality

<img src="https://img.shields.io/badge/PRINT-Done-green.svg"/></a>

<img src="https://img.shields.io/badge/FOR / NEXT-Done-green.svg"/></a>

<img src="https://img.shields.io/badge/IF-Done-green.svg"/></a>

<img src="https://img.shields.io/badge/GOTO-Done-green.svg"/></a>

<img src="https://img.shields.io/badge/GOSUB / RETURN-Done-green.svg"/></a>

<img src="https://img.shields.io/badge/Numeric Expressions-Done-green.svg"/></a>

<img src="https://img.shields.io/badge/Text expressions-Partially done-lightgray.svg"/></a>

<img src="https://img.shields.io/badge/DIM / Arrays-Done-green.svg"/></a>

<img src="https://img.shields.io/badge/DATA / READ-Done-green.svg"/></a>

<img src="https://img.shields.io/badge/STOP-Done-green.svg"/></a>

This is enough to run tic-tac-toe (10x10) game, which was a test program for Z80 simulator. 

