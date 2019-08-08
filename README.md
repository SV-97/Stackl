# Stackl

Small stack based language

## Grammar

```Ruby

program :=
    expression*.

expression :=
    expr.

expr :=
    orExpr.

orExpr :=
    andExpr (Or andExpr)*.

andExpr :=
    compExpr (And compExpr)*.

compExpr :=
    eqExpr ((Greater | Less | LessOrEq | GreaterOrEq)  eqExpr)?.

eqExpr :=
    lineExpr ((Equal | NotEqual )  lineExpr)?.

lineExpr :=
    dotExpr ((Add | Sub) dotExpr)*.

dotExpr :=
    unaryExpr ((Mul | Div | Mod) unaryExpr)*.

unaryExpr :=
    (Not | Sub) unaryExpr
    | primaryExpr.

primaryExpr :=
    block
    | ifExpr
    | parenthesizedExpr
    | assignmehtExpr
    | literal.

block :=
    Do expression+ End.

ifExpr :=
    If expression Do expression+ (End | (Else block)).

parenthesizedExpr :=
    LPar expression RPar.

assignmentExpr :=
    Id Equals expression.

literal := # todo add string literal
    variable
    | number
    | bool.

variable :=
    Id.

bool :=
    True
    | False.

number :=
    Float
    | ScientificFloat
    | Integer.

```

There also are *Indentation* tokens that represent two or more spaces.

## Operator Precedence

From strong to weak

| Operators | Associativity |
|       --- |           --- |
| `not`, unary `-` | - |
| `*`, `/`, `%` | Left |
| `+`, `-` | Left |
| `==`, `!=` | - |
| `<`, `>`, `<=`, `>=` | - |
| `and` | Left |
| `or` | Left |

## Example Code

```Ruby

let abcd = 5.0
mut b = 1.0

if abcd + b >= 3:
    print(a)
end

b = :
    abcd -= 2
    1 + 2
end # assign 3.0 to b

if not abcd != b:
    print(b)
end

def f(x):
    print(abcd + b)
    let c = 3
end

```

## Parser

Stackl uses a LL(1) Recursive-Descent Parser.
