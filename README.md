# Stackl

Small stack based language

## Grammar

```Ruby

program :=
    expression*.

block :=
    Colon expression+ End
    | If expression block.

assignments :=
    Id Equals expression.

expression :=
    parenthesized
    | literal
    | unop
    | binop
    | block.

parenthesized :=
    LPar expression RPar.

binop :=
    expression (
        Add
        | Sub
        | Mul
        | Div
        | Modulo
        | And
        | Or
        | Greater
        | Less
        | LessOrEq
        | GreaterOrEq
    )
    expression.

unop :=
    (Not | Sub) expression.

literal :=
    Id
    | number
    | bool.

bool :=
    True
    | False.

number :=
    Float
    | ScientificFloat
    | Integer.

```

There also are *Indentation* tokens that represent two or more spaces.

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