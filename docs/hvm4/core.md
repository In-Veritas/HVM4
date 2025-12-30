# HVM4 Core Terms

This document defines the core surface syntax for HVM4 terms. These terms are
parsed into static (book) terms and later instantiated as dynamic terms during
execution. See `docs/hvm4/memory.md` for the dynamic/static memory layout.

## Grammar

```
Term ::=
  | Var  Name                                        -- variable
  | Got  Name                                        -- mov-bound variable
  | Dp0  Name "₀"                                    -- first dup variable
  | Dp1  Name "₁"                                    -- second dup variable
  | Ref  "@" Name                                    -- reference
  | Nam  "^" Name                                    -- name (stuck head)
  | Dry  "^" "(" Term " " Term ")"                   -- dry (stuck application)
  | Era  "&{}"                                       -- erasure
  | Sup  "&" Label "{" Term "," Term "}"             -- superposition
  | Dup  "!" Name "&" Label "=" Term ";" Term        -- duplication term
  | Mov  "%" Name "=" Term ";" Term                  -- move term
  | Ctr  "#" Name "{" Term,* "}"                     -- constructor
  | Mat  "λ" "{" "#" Name ":" Term ";" Term "}"      -- pattern match
  | Swi  "λ" "{" Num ":" Term ";" Term "}"           -- number switch
  | Use  "λ" "{" Term "}"                            -- use (unbox)
  | Lam  "λ" Name "." Term                           -- lambda
  | App  "(" Term " " Term ")"                       -- application
  | Num  integer                                     -- number literal
  | Op2  "(" Term Oper Term ")"                      -- binary operation
  | Eql  "(" Term "==" Term ")"                      -- equality test
  | And  "(" Term ".&." Term ")"                     -- short-circuit AND
  | Or   "(" Term ".|." Term ")"                     -- short-circuit OR
  | DSu  "&" "(" Term ")" "{" Term "," Term "}"      -- dynamic superposition
  | DDu  "!" Name "&" "(" Term ")" "=" Term ";" Term -- dynamic duplication term
  | Red  Term "~>" Term                              -- reduction
  | Inc  "↑" Term                                    -- priority wrapper
  | Alo  "@" "{" Name,* "}" Term                     -- allocation
  | Uns  "!" "$" "{" Name "," Name "}" ";" Term      -- unscoped binding

Name  ::= [_A-Za-z0-9]+
Label ::= Name
Oper  ::= "+" | "-" | "*" | "/" | "%" | "&&" | "||"
        | "^" | "~" | "<<" | ">>" | "==" | "!="
        | "<" | "<=" | ">" | ">="
```

## Notes

- Variables are affine: each variable is used at most once.
- MOV-bound variables are linear and may appear multiple times; the parser does
  not enforce branch separation.
- MOV-bound variables share the same surface syntax as regular variables; the
  binder determines whether a name is VAR or GOT.
- Variables are global: a variable can occur outside its binder's lexical scope.
- Labels determine how duplications and superpositions interact; equal labels
  annihilate, different labels commute.
