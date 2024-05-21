# L-Interpreter

## Description
L Interpreter is a Haskell-writen CLI Application that parse and execute code written to L language

## Syntax

### Types
L Language has types:
- Float (as a number)
- String
- Boolean

### Variables
```L language
x := 5
y := 15.423
z := x * y + 1
stringVar := "Example"  
```

### Expressions
#### Expression examples
L language support expression in the format below
```L language
exprNum := -8 * 91
exprBool := True || True && False
exprStr := "Example of " + "string concatenation"
```

L language does not support "(" and ")" in expression, but it is not nessecary, because of possibility of encapsulating complex logic under the expression into the variables.

#### Priorities for operation in expressions
L language has different operation priority within expressions:
- Priority "4" garantee that operation will be executed firstly;
- Priority "0" garantee that operation will be executed at the end.

| Operation| Priority |
| ---------| ---------|
| *        | 4        |
| /        | 4        |
| +        | 3        |
| -        | 3        |
| ==       | 2        |
| !=       | 2        |
| >        | 2        |
| >=       | 2        |
| <        | 2        |
| <=       | 2        |
| &&       | 1        |
| ||       | 0        |

#### Types of Expressions
Expressions in L language does not have strict type. Variables could changes the type depending on defition. Below you can find example of valid expression named expr and reassigning this expr.
```L language
getInput varName <- {
    write "Input value for var named " + varName
    read
}

a := getInput "a"
b := getInput "b"

expr := a + b
expr := 5 + 7
expr := 12.58
expr := "String again"
expr := True
```

#### Lazy computations

Expressions provide mechanism of lazy computations

Example below demonstates the application on this mechanism. In this case, expr is not going to be evaluated fairy, because the first part is always True. Then there is no need to evaluate the other options with the || operator.
```L language
a := 5
b := 7

expr := True || a * 35.48 / 41 > b * b - 44 || ... 
```

The verse logic works for False with && operator. Variable expr would take False value without evaluation the rest of whole expression.
```L language
a := 5
b := 7

expr := False && a * 35.48 / 41 > b * b - 44 || ... 
```

#### Conditional computations
Not strict types provide light and usefull mechanism to use conditional computations. The code below demonstates the easy behavior of if-else statement without else branch.

```L language
notification message <- {
    write "A problem with appeared with: " + message
}

a := 5
b := 7

a < b || notification ("5 is not greater than 7")
```

As you can see, the left part is simply dropped. This means the whole expression could have as Boolean type (if the left part is True/False with corresponding functions), as any other type depending on the rest part of expression.

### If - else
If-else statement exists in L language and describes possibility of program behavior depending on the condition. In L language both "then" and "else" branches are required.

```L language
a := 5
b := 6

if a == b then { 
    write "Math does not work"
 } else {
    write "Math, hopefully, works"
 }
```

### While loop
If-else statement exists in L language and describes possibility of program behavior depending on the condition. In L language both "then" and "else" branches are required.

```L language
getUserInput type <- {
    write "Please, input your, " + type
    read
}

userInput := getUserInput("name")

while userInput != "Nikita" do { 
    write "You are not Nikita! Please, call author of L Interpreter"
    userInput := getUserInput("name")
}

```

### Skip
Keyword that provide you to skip the branch or the body is "skip"

### Write
Command "write" provide you to write any kind of information to the file. The argument of command write is an expression, so write firstly evaluates it and then writes to the file.

The file, that "write" writes anything, has default value of "output.txt". However, it's possibly to redirect out of the program. The only thing you need to do is override variable "outputFile". This variable is accessible everywhere

It's possible to override it during some scope to provide logging, and any other kind of interaction. See the example below with evaluation of fib.
```L language
fib n <- 
    {   
        outputFile := "logs.txt" 
        write n
        if n == 0 || n == 1 then { 1 } else { fib(n - 1) + fib(n - 2) }
    }

outputFile := "output.txt" 
write fib(15)
```

Moreover, you can easily override it to the value "str.out" to provide program write to console.

### Read
Command read provide ability to interact with user througth the terminal. It returns the user input.
As you can see, read is a statement, not an expression. Therefore we need to wrap it into the function and then call a funtion to provide interaction. Otherwise it would not work, because variable binding accepts expression part on the left (not statement)

```L language
getUserInput type <- {
    write "Please, input your, " + type
    read
}

x := getUserInput "example"
```

### Functions

Function is the core element of execution
You can declare it througth the sytax

```L language
mySum a b <- {
    a + b
}
```

The last expression in the function define the return value in the whole function

Functions can have their own function declaration and private variable inside of this scope.
For example this code will throw Error, because abs(-5) does not exist outside of the scope "mySum"
```L language
mySum a b <- {
    abs value <- {
        if value > 0 then { value } else { -value }
    }

    abs(a) + abs(b)
}

mySum(1, -1)
abs(-5)
```

## Lexical environment
