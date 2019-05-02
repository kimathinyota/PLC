myinterpreter : Grammar.hs Tokens.hs
	ghc Main.hs -o myinterpreter

Grammar.hs : Grammar.y
	happy Grammar.y

Tokens.hs : Tokens.x
	alex Tokens.x

clean :
	rm myinterpreter Tokens.hs Grammar.hs Tokens.hi Grammar.hi Tokens.o Grammar.o Main.o Main.hi