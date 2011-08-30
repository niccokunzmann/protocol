equal(X, X).
equal(X, Y, X, Y).
equal(X, Y, Z, X, Y, Z).
equal(X, Y, Z, A, X, Y, Z, A).

map(_, [], []).
map(Func, [E|L], [O|M]):-
		call(Func, E, O),
		map(Func, L, M).

%%%%%%%%%%%%%%%%%%%%      string stream     %%%%%%%%%%%%%%%%%%%%      

% readChars(+String, +Count, -String, -NewRead)
%   NewRead(+count, -chars, -nextRead)
% create a stream from a string
readChars(X, 0, "", readChars(X)).

readChars([], _, "", NewRead):-
		readChars("", 0, _, NewRead).

readChars([Char| String], I, [Char | String2], NewRead):-
		I >= 1, I2 is I - 1, 
		readChars(String, I2, String2, NewRead).

		
%%%%%%%%%%%%%%%%%%%%      stream operations     %%%%%%%%%%%%%%%%%%%%      

% readUntil(+Read, +CharList, String, LastChar, -NewRead)
%		Read(+count, -chars, -nextRead)
%	 NewRead(+count, -chars, -nextRead)
% read until a character of the String occours
readUntil(Read, CharList, String, LastChar, NewRead):-
		equal(CharList, [_|_]),
		readWhile(Read, readUntilCondition(CharList), String, LastChar, NewRead).

% readUntilCondition(+List, Char)
% true if Char in List
readUntilCondition(List, Char) :- \+ member(Char, List).
		

% readWhile(+Read, Condition, String, LastChar, -NewRead)
%	   Read(+count, -chars, -nextRead)
%	NewRead(+count, -chars, -nextRead)
%	Condition(Char)
% read a character from Read while the condition ist True
% 	if nothing was read LastChar will be ""
% 	otherwise LastChar will be the "%" string [Integer]
readWhile(Read, Condition, String, LastChar, NewRead):-
		call(Read, 1, S, NewRead1),!, (
			equal(S, [Char]) -> 
			(	call(Condition, Char) -> 
				 	readWhile(NewRead1, Condition, String2, LastChar, NewRead), 
					equal(String, [Char|String2])
				;	NewRead = NewRead1, 
					equal(LastChar, S),
					equal(String, [])
			) ;
			(	S == [], 
				equal(String, []), 
				LastChar = "", 
				NewRead = NewRead1
			)
		).
		
		
% readSkip(+Read, +CharList, String, LastChar, -NewRead)
%	   Read(+count, -chars, -nextRead)
%	NewRead(+count, -chars, -nextRead)
% read until no character of CharList is read
readSkip(Read, CharList, String, LastChar, NewRead):-
		readWhile(Read, readSkipCondition(CharList), String, LastChar, NewRead).
		
% readSkip(+Read, +CharList, LastChar, -NewRead)
%	   Read(+count, -chars, -nextRead)
%	NewRead(+count, -chars, -nextRead)
% read until no character of CharList is read
readSkip(Read, CharList, LastChar, NewRead) :- 
		readSkip(Read, CharList, _, LastChar, NewRead).
		
% readSkipCondition(+List, Char)
% true if List contains Char
readSkipCondition(List, Char) :- member(Char, List).

% readBound(-Read, -Bound, String, +NewRead)
% todo:
% readBound(Read, [FirstChar])

% todo: read from static function

%%%%%%%%%%%%%%%%%%%%      Function Database     %%%%%%%%%%%%%%%%%%%%      

noValue(Stream, Value, NewStream) :- readTerm(Stream, Value, NewStream).
hasValue(Value, Stream, Value, Stream).

% readToken(-Read, token, +NewRead)
% read a token until whitespace and skip the preceeding whitespaces
readToken(Read, [FistChar|Token], NewRead):-
		readSkip(Read, " \t\n\r\f\v", [FistChar], Read2),
		readUntil(Read2, " \t\n\r\f\v", Token, _, NewRead).

% protocolStream(Read, Stack, Predicates)
%	Predicates is [pStreamFunc("name", predicate), ...]
%	Stack is ["...", ]
%	predicate(+Stream, -NewStream)

% newProtocolInputStream(-Read, +protocolStream(...))
newProtocolInputStream(Read, protocolStream(Read, [], Predicates)) :- 
		findall(inputPredicate(String, Name), inputPredicate(String, Name), Predicates).
	
% readTerm(-ProtocolStream, Value, +NewProtocolStream)
% read a term from the Stream
readTerm(protocolStream(Read, Stack, Predicates), Value, protocolStream(NewRead, NewStack, NewPredicates)) :-
		readToken(Read, Command, ReadToken), 
		(	member(inputPredicate(Command, Predicate), Predicates) -> (
				definePredicate(
						Predicate, 
						protocolStream(ReadToken, Stack, Predicates), 
						protocolStream(ReadAfter, StackAfter, PredicatesAfter), 
						Result
					) ->
						call(	Result, 
								protocolStream(ReadAfter, StackAfter, PredicatesAfter), 
								Value,
								protocolStream(NewRead, NewStack, NewPredicates)
							)
					;
						trow(couldNotOperateOn(Predicate, protocolStream(Read, Stack, Predicates)))
				) ;
			string_to_atom(Command, CommandAtom),
			throw(invalidCommand(CommandAtom))
		).

% streamRead(+Stream, +Count, -Chars, -NewRead)
% 	Stream is protocolStream
% wrap around stream read
streamRead(protocolStream(Read, _, _), Count, Chars, NewRead) :- call(Read, Count, Chars, NewRead).
		
inputPredicate("push", push).
inputPredicate("int", int).
inputPredicate("stop", stop).
inputPredicate("stop", list).
inputPredicate("stop", bound).
inputPredicate("stop", stop).

definePredicate(push,	protocolStream(Read, Stack, P), 
						protocolStream(NewRead, [Token|Stack], P), noValue) :-
		readToken(Read, Token, NewRead).

definePredicate(int,	protocolStream(Read, [S|Stack], P), 
						protocolStream(Read, [I|Stack], P), noValue) :-
		number_codes(I, S).

definePredicate(stop,	protocolStream(Read, [S|Stack], P), 
						protocolStream(Read, Stack, P), hasValue(S)).

definePredicate(int,	protocolStream(Read, [S|Stack], P), 
						protocolStream(Read, [I|Stack], P), noValue) :-
		number_codes(I, S).

definePredicate(int,	protocolStream(Read, [S|Stack], P), 
						protocolStream(Read, [I|Stack], P), noValue) :-
		number_codes(I, S).

						
