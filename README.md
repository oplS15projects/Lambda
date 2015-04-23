##Lambda
###ShuddaWuddaCdr

####Members:
- Brian Carlson
- Joshua Caravetta
- Norman Mutunga


###Project Definition:
The goal is to have a mathematical engine that is simple , powerful and easy to build upon. Simple in terms that it allows the user to enter their expression using english keywords with infix notation, and not needing to worrying about equation format. The power of the Lambda will come from the ease at which the mathematical engine backend can be expanded on to add new math functionality.

###Project Status:
- Working frontend GUI with input, output and plot canvas fields.
- Working general expression parser that handles initial keyword and equation parsing.
- Working infix->prefix parser that handles transforming input equation to prefix for evaluation (handles operator precedence).
- High and Low level abstracted backend that forms a dynamic database of key pairs for additional mathematical procedures based on keywords.
- Evaluation of basic equations with any combination of the following operators: **-,+,/,\*,^**.
- Plot of basic equations with any combination of the following operators: **-,+,/,\*,^** and variable **x**. 

- Working keywords: eval, plot
- Planned keywords: simplify, derivative

###How to use:

1. Open and run Lambda.rkt
2. Input expression is typed into the `Input`, using syntax: `keyword equation`
3. Output is seen in the `Ouput` or lower canvas depending on keywords used.

**Example 1:** `plot x+2*x+3` - support for only one variable **x** is currently available.

This will plot `x+2*x+3` in the lower canvas field

**Example 2:** `eval 1+2*3^4` or `1+2*3^4` will both evaluate to: 163. `eval` is the default keyword if no keyword is entered.

If `eval` is used on an equation with a variable, such as `x+2`, it will return `cannot evaluate: x+2` since there is a variable.
