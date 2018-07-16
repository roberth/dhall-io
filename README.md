
# `dhall-io`

*Configure your IO with Dhall!*

This proof of concept introduces IO into Dhall, because an effectful program is just a configuration too!

Initially I wanted to add a looping construct like a Y combinator, but that made me feel bad for defeating (almost) the entire purpose of Dhall :).

# Implementation notes

In a nutshell, this uses the Dhall library to read and check a Dhall function that takes the `IO` type and a record of library functions as an argument, so a program starts like this:

    let IOModule = /ioModule.dhall in \(IO : Type -> Type) -> \(io : IOModule IO) ->

Here `/ioModule.dhall` is a builtin Dhall expression that provides the types for operations like `readLn`, `bind`, etc.

When the program is loaded and checked, it will be invoked to fill in the `IO` and `io` arguments. This uses the `Embed` constructor in the Dhall `Expr` AST.

This results in an expression of type `IO a` that will be by the Haskell `run` function,

    run :: Expr Src IOExpr -> IO (Expr Src IOExpr)

which normalizes the expression and pattern matches on the root of the expression to figure out what actions to perform.
