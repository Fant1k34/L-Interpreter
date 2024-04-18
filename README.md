# Projects

## Hard deadline: 24 hours before the exam

A project amounts to 40% of your final grade.
A project is to be done by a single student, no teamwork allowed. 
The maximum number of points for a project is 100, with concrete subtasks having different values. 

Each project should be followed by a written report in which the following topics are discussed: 

* What the task was.
* The architecture of your solution. 
* Why certain architecture dessisions were done.
* Why certain libraries were chosen. 
* Investigation of the performance.

Sign up for a project [here](https://docs.google.com/spreadsheets/d/1BL1v-81dswbfy0YID5IKiXuDbYcdHAvvFoz0IbbWxAo/edit?usp=sharing)

### Interpreter for the language L

[Language description](https://drive.google.com/file/d/1evYQJFJAechTD-nL59YQmp1VWmokjsbU/view?usp=sharing)

Minimum:

* (10 points) Console (or other user-) interface.
* (15 points) Parser (you are encouraged to use a library).
* (20 points) Interpreter of a program in L that writes the result into an output file. 
* (10 points) Error reporting
* (5 points) Unit tests

Additional tasks:

* (10 points) Add functions into the language
* (10 points) Add a type system (GADTs)
* (10 points) A comparison of the small-step and big-step semantics
* (10 points) Property-based testing

### Interpreter for the PCF language 

[Language description](http://www.cse.chalmers.se/edu/year/2018/course/DAT350/PCF.pdf)

Minimum:

* (10 points) Console (or other user-) interface.
* (15 points) Parser (you are encouraged to use a library).
* (15 points) Typechecker (either write your own or use GADTs)
* (15 points) Interpreter of a program in PCF that writes the result into an output file. 
* (10 points) Error reporting
* (5 points) Unit tests

Additional tasks:

* (10 points) Addition of recursive types into the language
* (20 points) A comparison of the small-step and big-step semantics
* (10 points) Property-based testing

### Hindley–Milner type inference

Minimum:

* (10 points) Console (or other user-) interface
* (15 points) Parser (you are encouraged to use a library)
* (20 points) Type inference
* (10 points) Error reporting
* (5 points) Unit tests

Additional tasks:

* (15 points) Add recursive definitions into the language
* (15 points) Add overloading
* (10 points) Property-based testing

### Demostration of different reduction strategies for lambda-terms

This task is about creating a nice user experience when demonstrating reduction strategies for lambda-terms. 

Minimum:

* (20 points) Web (or other user-) interface
* (15 points) Parser (you are encouraged to use a library)
* (15 points) Step-by-step reduction
* (5 points) Error reporting
* (5 points) Unit tests

Additional tasks:

* (10 points) Interface to choose either to show reductions step-by-step, the next k steps, or just compute the normal form
* (20 points) Stastistics and comparison of different strategies
* (10 points) Property-based testing

### Finite automata simulation

This task is about creating a nice user experience when demonstrating how a finite automaton works. 

Minimum:

* (20 points) Web (or other user-) interface
* (15 points) Parser of some finite automaton description (you are encouraged to use a library)
* (15 points) Step-by-step simulation of the automaton's work
* (5 points) Error reporting
* (5 points) Unit tests

Additional tasks:

* (10 points) Interface to choose either to show reductions step-by-step, the next k steps, or just compute the result
* (20 points) Non-deterministic finite automata
* (10 points) Property-based testing

### Wordle solver

This task is about performance 

Minimum:

* (10 points) Console (or other user-) interface
* (20 points) The user choses a word and you return the clues which letters of the user's input is in the word and which are at their correct positions
* (20 points) A search for words that satisfy the known clues
* (5 points) Error reporting
* (5 points) Unit tests

Additional tasks:

* (20 points) Compute the best first word, meaning the word that ensures that you can guess any word from a dictionary in the minimum number of steps. Here the dictionary is a parameter.
* (10 points) Performance comparison between the naive implementation over `Strings` and the optimized version. 
* (10 points) Property-based testing

### Tic-tac-toe

Minimum:

* (20 points) Web (or other user-) interface
* (10 points) Field 3*3, 2 players
* (15 points) An option to choose the size of the field and the number of players
* (10 points) Error reporting
* (5 points) Unit tests
  
Additional tasks:

* (10 points) An infinite field
* (20 points) Play agaist the computer
* (10 points) Property-based testing


### Computational mathematics

You can read about different computational methods for integrals [here](https://habr.com/ru/post/420867/)

Minimum:

* (10 points) A library
* (10 points) The user choose the error and a definite integral as a Haskell function
* (30 points) The implementation of 3 different strategies
* (5 points) Error reporting
* (5 points) Unit tests

Additional tasks:

* (10 points) Console interface allowing the user to specify the integral without writing any Haskell code. 
* (10 points) Computation of the error and how many steps it took to get to the final result
* (10 points) Comparison of the methods
* (10 points) Property-based testing

### Graph algorithms

This is supposed to be a library making it easy to use graphs in Haskell. 

* (10 points) Graph representation
* (15 points) BFS
* (15 points) DFS
* (10 points) Examples of library use
* (5 points) Error reporting
* (5 points) Unit test

Additional tasks: 

* (10 points) Make it possible to specify the type of vertices and edges
* (10 points) Dijkstra's algorithm
* (10 points) Render graphs nicely (you can use DOT)
* (10 points) Property-based testing

### Byte-Pair encoding tokenizer

This is a task for Georgii Gerasev who should contact me to help me formulate concrete tasks. 
