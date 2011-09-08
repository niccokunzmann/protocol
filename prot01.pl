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


:- use_module(encoding).
:- use_module(functions).

		
%%%%%%%%%%%%%%%%%%%%      character stream     %%%%%%%%%%%%%%%%%%%%      


characterStream(String, characterStream(String)).

%% characterStreamRead(+String, +Count, -String, -NewRead)
%   NewRead(+count, -chars, -nextRead)
% read characters from a stream
characterStreamRead(X, 0, "", characterStreamRead(X)) :- !.

characterStreamRead("", _, "", NewRead):-
		% yield nothing if stream is empty
		characterStreamRead("", 0, _, NewRead).

characterStreamRead([Char| String], I, [Char | String2], NewRead):-
		I >= 1, I2 is I - 1, 
		characterStreamRead(String, I2, String2, NewRead).

%% characterStreamWrite(+String, +String, -CharsWritten, -NewRead)
%   NewRead(+String, -CharsWritten, -NewRead)
% write characters to a stream

characterStreamWrite(	String, [Char | Written], C, 
						characterStreamWrite([Char|String2])):-
		characterStreamWrite(String, C2, Written, characterStreamWrite(String2)),
		C is C2 + 1.
		
characterStreamWrite(X, "", 0, characterStreamWrite(X)) :- !.

characterStream(String, characterStream(String)).


% todo: read from static function

/* characterStream("character", Stream).
 * streamRead(+-Stream, -+Read)
 * streamWrite(+-Stream, -+Read)
 * call(Read, Count, [...], NewRead)
 * call(Write, [], Count, NewWrite)
 * 
 * 
 */

%%%%%%%%%%%%%%%%%%%%      stream read and write     %%%%%%%%%%%%%%%%%%%%      


 
streamRead(characterStream(String), characterStreamRead(String)).

streamWrite(characterStream(String), characterStreamWrite(String)).

%%%%%%%%%%%%%%%%%%%%      stream operations     %%%%%%%%%%%%%%%%%%%%      

%% readUntil(+Read, +CharList, String, LastChar, -NewRead)
%		Read(+count, -chars, -nextRead)
%	 NewRead(+count, -chars, -nextRead)
% read until a character of the String occours
readUntil(Read, CharList, String, LastChar, NewRead):-
		equal(CharList, [_|_]),
		readWhile(Read, readUntilCondition(CharList), String, LastChar, NewRead).

%% readUntilCondition(+List, Char)
% true if Char in List
readUntilCondition(List, Char) :- \+ member(Char, List).
		

%% readWhile(+Read, Condition, String, LastChar, -NewRead)
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
		
		
%% readSkip(+Read, +CharList, String, LastChar, -NewRead)
%	   Read(+count, -chars, -nextRead)
%	NewRead(+count, -chars, -nextRead)
% read until no character of CharList is read
readSkip(Read, CharList, String, LastChar, NewRead):-
		readWhile(Read, readSkipCondition(CharList), String, LastChar, NewRead).
		
%% readSkip(+Read, +CharList, LastChar, -NewRead)
%	   Read(+count, -chars, -nextRead)
%	NewRead(+count, -chars, -nextRead)
% read until no character of CharList is read
readSkip(Read, CharList, LastChar, NewRead) :- 
		readSkip(Read, CharList, _, LastChar, NewRead).
		
%% readSkipCondition(+List, Char)
% true if List contains Char
readSkipCondition(List, Char) :- member(Char, List).

%% readBound(-Read, -Bound, String, +NewRead)
% 
readBound(Read, Bound, String, NewRead) :- 
		length(Bound, BoundLen),
		call(Read, BoundLen, StartString, Read1), !,
		readBound(Read1, StartString, BoundLen, Bound, String, NewRead).
		
readBound(Read, Bound, _, Bound, [], Read) :- !.
readBound(Read, StartString, BoundLen, Bound, Out, NewRead):- 
		length(StartString, StrLen),
		CharCountToRead is BoundLen - StrLen,
		call(Read, CharCountToRead, ReadString, Read1), !, 
		append(StartString, ReadString, String),
		findBoundMatch(Bound, String, Index, _, _),
		append(NoBound, StringBoundPart, String),
		length(NoBound, Index), !,
		% recursion here
		readBound(Read1, StringBoundPart, BoundLen, Bound, Out2, NewRead),
		append(NoBound, Out2, Out).
		
matchBound([C|Bound], [C|String], Count, BoundEnd) :- 
		matchBound(Bound, String, Count1, BoundEnd), 
		Count is 1 + Count1, !.
matchBound(Bound, [], 0, Bound) :- !.
matchBound([], _, 0, []).

findBoundMatch(Bound, [], 0, 0, Bound) :- !.
findBoundMatch(Bound, String, 0, Len, BoundEnd) :-	
		matchBound(Bound, String, Len, BoundEnd), !.
findBoundMatch(Bound, [_|String], Index, Len, BoundEnd) :-
		findBoundMatch(Bound, String, Index0, Len, BoundEnd),
		Index is Index0 + 1 .

%%%%%%%%%%%%%%%%%%%%      Function Database     %%%%%%%%%%%%%%%%%%%%      

noValue(Stream, Value, NewStream) :- readTerm(Stream, Value, NewStream).
hasValue(Value, Stream, Value, Stream).

%% readToken(-Read, token, +NewRead)
% read a token until whitespace and skip the preceeding whitespaces
readToken(Read, [FistChar|Token], NewRead):-
		readSkip(Read, " \t\n\r\f\v", [FistChar], Read2),
		readUntil(Read2, " \t\n\r\f\v", Token, _, NewRead).


%% readTokens(+Read, +Count, -[Tokens], -NewRead)
%   NewRead(+count, -chars, -nextRead)
% a token stream from Read of chars
% read Count tokens from the string
readTokens(Read, 0, [], readTokens(Read)) :- !.
readTokens(Read, I, [Token | Result], NewRead):-
		I >= 1, I2 is I - 1, 
		readToken(Read, Token, TRead),
		readTokens(TRead, I2, Result, NewRead).

%% protocolStream(Read, Stack, Predicates)
%	Predicates is [pStreamFunc("name", predicate), ...]
%	Stack is ["...", ]
%	predicate(+Stream, -NewStream)

% newProtocolInputStream(-Read, +protocolStream(...))
newProtocolInputStream(Read, protocolStream(Read, [], Predicates)) :- 
		findall(inputPredicate(String, Name), inputPredicate(String, Name), Predicates).
	
%% readTerm(-ProtocolStream, Value, +NewProtocolStream)
% read a term from the Stream
readTerm(Stream, Term, NewStream) :-
		readToken(streamRead(Stream), Command, ReadAfter),
		newRead(ReadAfter, Stream, StreamAfter),
		callStreamCommand(StreamAfter, Command, Result, CStream),
		call( Result, CStream, Term, NewStream).

%% streamRead(+Stream, +Count, -Chars, -NewRead)
% 	Stream is protocolStream
% wrap around stream read
streamRead(protocolStream(Read, _, _), Count, Chars, NewRead) :- call(Read, Count, Chars, NewRead).

%% newRead(NewRead, OldProtocolStream, NewProtocolStream)
% replace the Read of the old prtocolStream with the new Read
newRead(NewRead, protocolStream(_, Stack, Predicates), protocolStream(NewRead, Stack, Predicates)).

%% callStreamCommand(-ProtocolStream, -Command,  +Value, +ProtocolStream)
% call a command on a given stream
callStreamCommand(protocolStream(Read, Stack, Predicates), Command, Result, NewStream) :- 
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

%% inputPredicate(String, Atom)
% predicates taht are defined as stream operations
inputPredicate("push", push).
inputPredicate("stop", stop).
inputPredicate("list", list).
inputPredicate("bound", bound).
inputPredicate("switch", switch).
inputPredicate("insert", insert).
inputPredicate("head", head).
inputPredicate("save", save).
inputPredicate("dup", dup).
inputPredicate("def", def).
inputPredicate("base64", base64).
inputPredicate("utf8", utf8).
inputPredicate("decode", decode).
inputPredicate(S, A):- member(S, ["int", "num", "float"]), string_to_atom(S, A).

%%%%%%%%%%%%%%%%%%%%% Prolog specific %%%%%%%%%%%%%%%%%%%

%% definePredicate(+Name, +Stream, -NewStream, -Value) 
% the operations defined on the stream

definePredicate(Num,	protocolStream(Read, [S|Stack], P), 
						protocolStream(Read, [I|Stack], P), noValue) :-
		member(Num, [int, float, num]),
		number_codes(I, S).
		
definePredicate(list,	protocolStream(Read, Stack, P), 
						protocolStream(Read, [[]|Stack], P), noValue).

definePredicate(insert,	protocolStream(Read, [A, L|Stack], P), 
						protocolStream(Read, [[A|L]|Stack], P), noValue).
						
definePredicate(head,	protocolStream(Read, [[A|L]|Stack], P), 
						protocolStream(Read, [A, L|Stack], P), noValue).

definePredicate(base64,	protocolStream(Read, [A|Stack], P), 
						protocolStream(Read, [B|Stack], P), noValue) :- 
		encoding(base64, B, A).

definePredicate(utf8,	protocolStream(Read, [A|Stack], P), 
						protocolStream(Read, [B|Stack], P), noValue) :- 
		encoding(utf8, B, A).
		
definePredicate(decode,	protocolStream(Read, [A|Stack], P), 
						protocolStream(NewRead, [B|Stack], P), noValue) :- 
		readToken(Read, Token, NewRead), !,
		normalizeEncodingName(Token, Encoding),
		encoding(Encoding, B, A).

%%%%%%%%%%%%%%%%%%%%% Protocol specific %%%%%%%%%%%%%%%%%%%

definePredicate(switch,	protocolStream(Read, [A, B|Stack], P), 
						protocolStream(Read, [B, A|Stack], P), noValue).

definePredicate(push,	protocolStream(Read, Stack, P), 
						protocolStream(NewRead, [Token|Stack], P), noValue) :-
		readToken(Read, Token, NewRead).

		
definePredicate(bound,	protocolStream(Read, Stack, P), 
						protocolStream(NewRead, [String|Stack], P), noValue) :-
		% todo:
		readToken(Read, Bound, Read1),
		readBound(Read1, Bound, String, NewRead).

		
definePredicate(stop,	protocolStream(Read, [S|Stack], P), 
						protocolStream(Read, Stack, P), hasValue(S)).

						
definePredicate(save,	protocolStream(Read, [Value|Stack], P), 
						protocolStream(NewRead, Stack, 
						[inputPredicate(Command, restore(Value))|P]), noValue):-
		readToken(Read, Command, NewRead).

definePredicate(restore(V),	protocolStream(Read, Stack, P), 
						protocolStream(Read, [V|Stack], P), noValue).

						
definePredicate(dup,	protocolStream(Read, [A|Stack], P), 
						protocolStream(Read, [A, A|Stack], P), noValue).
						
						
definePredicate(def,	protocolStream(Read, Stack, P), 
						protocolStream(NewRead, Stack, 
						[inputPredicate(Name, defined(Commands))|P]), noValue) :-
		readToken(Read, Name, Read1), 
		readUntil(readTokens(Read1), [Name], Commands, Name2, readTokens(NewRead)),
		(([Name] \= Name2) -> 
			throw(couldNotEndDefClause(Name, Commands, Name2))
		;	true
		).
		
definePredicate(defined([Command|Commands]), OldStream, NewStream, noValue) :-
		callStreamCommand(OldStream, Command, _, Stream1),
		definePredicate(defined(Commands), Stream1, NewStream, noValue).
definePredicate(defined([]), Stream, Stream, noValue).

		
		
