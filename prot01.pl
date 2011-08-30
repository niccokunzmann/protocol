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
		(	Predicate is Value, 
			call(Function), fail
		) ;
		foreach(Predicate, Values, Function).
		
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
readTerm(Stream, Term, NewStream) :-
		readToken(streamRead(Stream), Command, ReadAfter),
		newRead(ReadAfter, Stream, StreamAfter),
		callStreamCommand(ReadAfter, Command, Result, NewStream),
		call( Result, StreamAfter, Term, NewStream).

% streamRead(+Stream, +Count, -Chars, -NewRead)
% 	Stream is protocolStream
% wrap around stream read
streamRead(protocolStream(Read, _, _), Count, Chars, NewRead) :- call(Read, Count, Chars, NewRead).

% newRead(NewRead, OldProtocolStream, NewProtocolStream)
% replace the Read of the old prtocolStream with the new Read
newRead(NewRead, protocolStream(_, Stack, Predicates), protocolStream(NewRead, Stack, Predicates)).

% callStreamCommand(-ProtocolStream, -Command,  +Value, +ProtocolStream)
% call a command on a given stream
callStreamCommand(protocolStream(Read, Stack, Predicates), Command,  Result, NewStream) :- 
		member(inputPredicate(Command, Predicate), Predicates) -> (
			definePredicate(
					Predicate, 
					protocolStream(Read, Stack, Predicates), 
					NewStream,
					Result
				) -> true
			;	throw(couldNotOperateOn(Predicate, protocolStream(Read, Stack, Predicates)))
		) ;
			string_to_atom(Command, CommandAtom),
			throw(invalidCommand(CommandAtom)).
		
%%%%%%%%%%%%%%%%%%%%      given protocol functions     %%%%%%%%%%%%%%%%%%%%      
		
inputPredicate("push", push).
inputPredicate("int", int).
inputPredicate("stop", stop).
inputPredicate("list", list).
inputPredicate("bound", bound).
inputPredicate("switch", switch).
inputPredicate("insert", insert).
inputPredicate("head", head).
inputPredicate("save", save).
inputPredicate("dup", dup).
inputPredicate("def", def).

definePredicate(push,	protocolStream(Read, Stack, P), 
						protocolStream(NewRead, [Token|Stack], P), noValue) :-
		readToken(Read, Token, NewRead).

definePredicate(bound,	protocolStream(Read, Stack, P), 
						protocolStream(NewRead, [Token|Stack], P), noValue) :-
		readToken(Read, Token, NewRead).

definePredicate(int,	protocolStream(Read, [S|Stack], P), 
						protocolStream(Read, [I|Stack], P), noValue) :-
		number_codes(I, S).

definePredicate(stop,	protocolStream(Read, [S|Stack], P), 
						protocolStream(Read, Stack, P), hasValue(S)).

definePredicate(list,	protocolStream(Read, Stack, P), 
						protocolStream(Read, [[]|Stack], P), noValue).

definePredicate(switch,	protocolStream(Read, [A, B|Stack], P), 
						protocolStream(Read, [B, A|Stack], P), noValue).

definePredicate(insert,	protocolStream(Read, [A, L|Stack], P), 
						protocolStream(Read, [[A|L]|Stack], P), noValue).
						
definePredicate(head,	protocolStream(Read, [[A|L]|Stack], P), 
						protocolStream(Read, [A, L|Stack], P), noValue).

definePredicate(save,	protocolStream(Read, [Value|Stack], P), 
						protocolStream(NewRead, Stack, 
						[inputPredicate(Command, restore(Value))|P]), noValue):-
		readToken(Read, Command, NewRead).

definePredicate(head,	protocolStream(Read, [[A|L]|Stack], P), 
						protocolStream(Read, [A, L|Stack], P), noValue).

definePredicate(dup,	protocolStream(Read, [A|Stack], P), 
						protocolStream(Read, [A, A|Stack], P), noValue).

definePredicate(restore(V),	protocolStream(Read, Stack, P), 
						protocolStream(Read, [V|Stack], P), noValue).

definePredicate(def,	protocolStream(Read, Stack, P), 
						protocolStream(NewRead, Stack, 
						[inpurPredicate(Name, defined(Commands))|P]), noValue) :-
		readToken(Read, Name, Read1), 
		readUntil(readTokens(Read1), [Name], Commands, Name2, readTokens(NewRead)),
		assert([Name] == Name2).
		
definePredicate(defined([Command|Commands]), OldStream, NewStream, noValue) :-
		callCommand(OldStream, Command, _, Stream1),
		definePredicate(defined(Commands), Stream1, NewStream, noValue).
definePredicate(defined([]), Stream, Stream, noValue).

		
% for def
% readTokens(+Read, +Count, -[Tokens], -NewRead)
%   NewRead(+count, -chars, -nextRead)
% a token stream from Read of chars
readTokens(Read, 0, [], readToken(Read)).
readTokens(Read, I, [Token | Result], NewRead):-
		I >= 1, I2 is I - 1, 
		readToken(Read, Token, NewRead),
		readTokens(Read, I2, Result, NewRead).

		
