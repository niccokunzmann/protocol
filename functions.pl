
number_binaryAtom(0, '0b0') :- !.
number_binaryAtom(Num, Atom) :- 
		nonvar(Num),
		binaryTail(Num, AtomTail), 
		concat('0b', AtomTail, Atom).
number_binaryAtom(Num, Atom) :- 
		nonvar(Atom),
		concat('0b', AtomTail, Atom),
		binaryTail(Num, AtomTail).

binaryTail(0, '') :- !.
binaryTail(N, A) :- 
		nonvar(N), 
		1 is N /\ 1, !,  
		N2 is N >> 1,
		binaryTail(N2, X),
		concat(X, '1', A).
binaryTail(N, A) :- 
		nonvar(N), !, 
		N2 is N >> 1,
		binaryTail(N2, X),
		concat(X, '0', A).
binaryTail(N, A) :- 
		nonvar(A), 
		concat(X, '0', A), !,
		binaryTail(N2, X), 
		N is N2 << 1.
binaryTail(N, A) :- 
		nonvar(A), 
		concat(X, '1', A), !,
		binaryTail(N2, X), 
		N is N2 << 1 + 1.

bin([], []).
bin([N|LN], [A|LA]) :- bin(N, A), bin(LN, LA).
bin(Number, Atom) :- number_binaryAtom(Number, Atom).

equal(X, X).
equal(X, Y, X, Y).
equal(X, Y, Z, X, Y, Z).
equal(X, Y, Z, A, X, Y, Z, A).

map(_, [], []).
map(Func, [E|L], [O|M]):-
		call(Func, E, O),
		map(Func, L, M).

		
% foreach(Variable, List, Function) 
% execute the Function, binding Variable to every List item
foreach(_, [], _).
foreach(Predicate, [Value|Values], Function):-
		(	equal(Predicate, Value), 
			(call(Function) -> fail
			;	throw(error(functionDidNotSucceed(Function, Value)))
			)
		) ;
		foreach(Predicate, Values, Function).
