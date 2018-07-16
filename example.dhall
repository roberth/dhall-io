let IOModule = /ioModule.dhall in \(IO : Type -> Type) -> \(io : IOModule IO) ->

let read = \(r : Type) -> \(c : Text -> IO r) -> io.bind Text r io.readLn c in
let write = \(r : Type) -> \(t : Text) -> \(c : IO r) -> io.bind {} r (io.writeLn t) (\(_:{}) -> c) in
let pure = io.pure in

-- A socially awkward bartender

let r = {} in

  write r ("What is your name?") (

  read r (\(name : Text) ->

  write r ("Hello, " ++ name ++ ", what would you like to drink?") (

  read r (\(drink : Text) ->

  write r ("Hi, " ++ name ++ ", here's your " ++ drink ++ ". Enjoy!") (

  pure r {=}

  )))))

{-

Alternative implementation

let r = {} in
io.bind {} r (io.writeLn "What is your name?") (\(_ : {}) ->
  io.bind Text r io.readLn (\(name : Text) ->
    io.bind {} r (io.writeLn ("Hello, " ++ name ++ ", what would you like to drink?")) (\(_ : {}) ->
      io.bind Text r io.readLn (\(drink : Text) ->
        io.writeLn ("Hi, " ++ name ++ ", here's your " ++ drink ++ ". Enjoy!")
      )
    )
  )
)
-}