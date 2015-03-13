type 'a list' =
	| vide'
	| nonvide' of 'a * 'a'

type _ grammar = 
	| Fail
	| Rule : 'a rule * 'a grammar -> 'a grammar

and  _ rule = 
	| Empty : 'a -> 'a rule
	| Terminal : char list * 'a grammar * (char -> 'a -> 'b) -> 'b rule
	| NonTerminal : 'a grammar * 'b grammar * ('a -> 'b ->'c) -> 'c rule
	
let extend c i =
	Char.code c - Char.code '0') * 10 + i
	
let rec int_grammar : int grammar = 
	Rule(Terminal(['0';'1';'2';'3';'4';'5';'6';'7';'8';'9'],
					int_opt_grammar,
					
and int_opt_grammar : int grammar = 
	Rule(Empty 0, int_grammar)
