/*  SWI-Prolog

    Author:        Nicco Kunzmann
    E-mail:        
    WWW:           
    Copyright (C): 2011+

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(functions,
	[	bin/2, 
		number_binaryAtom/2, 
		foreach/3, 
		map/3,
		equal/2, equal/4, equal/6, equal/8, equal/10, 
		range/4
	]).

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
equal(X, Y, Z, A, B, X, Y, Z, A, B).

map(_, [], []).
map(Func, [E|L], [O|M]):-
		call(Func, E, O),
		map(Func, L, M).

		
%% foreach(Variable, List, Function) 
% execute the Function, binding Variable to every List item
foreach(_, [], _).
foreach(Predicate, [Value|Values], Function):-
		(	equal(Predicate, Value), 
			(call(Function) -> fail
			;	throw(error(functionDidNotSucceed(Function, Value)))
			)
		) ;
		foreach(Predicate, Values, Function).

range(Start, Stop, _, []) :- 
		Start >= Stop, !.
range(Start, Stop, Step, [Start|L]) :- 
		Next is Start + Step, 
		range(Next, Stop, Step, L).