# Whilec
Whilecは「SEMANTICS WITH APPLICATIONS A Formal Introduction」の第二章で定義されたWhile言語のインタプリターです。

## While`s BNF
    <arithmeticexpression> ::= n 
                            | x 
                            | <arithmetic expression> "+" <arithmetic expression> 
                            | <arithmetic expression> "-" <arithmetic expression> 
                            | <arithmetic expression> "*" <arithmetic expression>
                            | "(" <arithmetic expression> ")"

    <boolean expression> ::= "true" 
                            | "false" 
                            | <arithmetic expression> "=" <arithmetic expression> 
                            | <arithmetic expression> "<=" <arithmetic expression> 
                            | "!" <boolean expression> 
                            | <boolean expression> "&&" <boolean expression>
                            | "(" <boolean expression> ")"

    <statement> ::= x ":=" <arithmetic expression> ";" 
                    | "skip" 
                    | "if" <boolean expression> "then" <block> "else" <block> "end"
                    | "while" <boolean expression> "do" <block> "end"

    <block> ::= (<statement>)+

構文解析の曖昧さを無くすためにwhile規則とif規則にendキーワードを導入した.

## Example

```scala
$While> x := 1;
state = Map(x -> 2)

$While> x := 1 + 2 * 3 + (1 + 1) * 2 - (1 - 2);
state = Map(x -> 12)

$While> if true then  x := 1; y := 3; else x := 4; end
state = Map(x -> 1, y -> 3)

$While> x := 2;while x <= 10 do x := x * x; end
state = Map(x -> 16, y -> 3)

$While> x := 5; y := 1;  while 1 <= x do y := y * x; x := x - 1; end 
state = Map(x -> 0, y -> 120)

$While> state.refresh
state = Map()

$While> :q
finish While REPL. SEE YOU AGAIN!

```