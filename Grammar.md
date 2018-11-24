# Parser Grammar

```BNF
Original:

Expr    ::= Expr + Expr | Expr * Expr | ( Expr ) | Nat

Nat     ::= 0 | 1 | 2 | ...

Fix Mult > Add Precedence:

Expr ::= Expr + Expr | Term

Term ::= Term * Term | Factor

Factor ::= ( Expr ) | Nat

Nat ::= 0 | 1 | 2 | ...

Fix Right-Assoc:

Expr ::= Term + Expr | Term

Term ::= Factor * Term | Factor

...

Simplify:

Expr ::= Term ( + Expr | ε )

Term ::= Factor ( * Term | ε )

Factor ::= ( Expr ) | Nat

Nat ::= 0 | 1 | 2 | ...
```
