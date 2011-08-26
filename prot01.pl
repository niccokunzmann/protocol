
equal(X, X).

% readChars(+String, +Count, -String, -NewRead)
%   NewRead(+count, -chars, -nextRead)
readChars(X, 0, "", readChars(X)).

readChars([], _, "", NewRead):-
		readChars("", 0, _, NewRead).

readChars([Char| String], I, [Char | String2], NewRead):-
		I >= 1, I2 is I - 1, 
		readChars(String, I2, String2, NewRead).


% readUntil(+Read, +CharList, String, LastChar, -NewRead)
%	   Read(+count, -chars, -nextRead)
%	NewRead(+count, -chars, -nextRead)
% read until a character of the String occuors
readUntil(Read, CharList, String, LastChar, NewRead):-
		equal(CharList, [_|_]),
		readWhile(Read, readUntilCondition(CharList), String, LastChar, NewRead).
		
% readUntilCondition(+List, Char)
% true if Char in List
readUntilCondition(List, Char):- \+ member(Char, List).
		

% readWhile(+Read, Condition, String, LastChar, -NewRead)
%	   Read(+count, -chars, -nextRead)
%	NewRead(+count, -chars, -nextRead)
% 	Condition(Char)
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
				LastChar == "", 
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
readSkip(Read, CharList, LastChar, NewRead):- 
		readSkip(Read, CharList, _, LastChar, NewRead).

% readSkipCondition(+List, Char)
% true if List contains Char
readSkipCondition(List, Char):- member(Char, List).




:- begin_tests(prot).
 
%%%%%%%%%%%%%%%%%%%%      Test String Reader     %%%%%%%%%%%%%%%%%%%%      
test(testtest):-
        reverse([1,2], [2,1]).
 
test(reader1):- 
        S = "abcdefg", readChars(S, 3, "abc", _).

test(reader2):- 
        S = "abcdefg", readChars(S, 3, "abc", NewRead), 
		call(NewRead, 3, "def", _).

test(reader3):- 
        S = "abcdefg", readChars(S, 8, _, NewRead), 
		call(NewRead, 3, [], _).

test(reader4):- 
        S = "abcdefg", 
		Read0 = readChars(S),
		call(Read0, 1, "a", Read1), 
		call(Read1, 1, "b", Read2),
		call(Read2, 1, "c", Read3),
		call(Read3, 1, "d", Read4),
		call(Read4, 1, "e", Read5),
		call(Read5, 1, "f", Read6),
		call(Read6, 1, "g", Read7),
		equal(Read7, readChars("")).

urks(A, B, C):- C =:= A + B.
test(urks) :-
		call(urks(1, 1), 2).


%%%%%%%%%%%%%%%%%%%%      Test readUntil     %%%%%%%%%%%%%%%%%%%%      

test(readUntil1) :-
	%	readUntil(+Read, +CharList, -String, -LastChar, -NewRead)
		readUntil(readChars("123456789"), "3", "12", "3", _).

test(readUntil2) :-
	%	readUntil(+Read, +CharList, -String, -LastChar, -NewRead)
		readUntil(readChars(""), "a", "", "", _).

test(readUntil3) :-
	%	readUntil(+Read, +CharList, -String, -LastChar, -NewRead)
		readUntil(readChars("1234567890"), "a", "1234567890", "", _).

%%%%%%%%%%%%%%%%%%%%      Test readSkip     %%%%%%%%%%%%%%%%%%%%      

test(readSkip1) :-
	%	readSkip(+Read,           +CharList, -String, -LastChar, -NewRead)
		readSkip(readChars("123456789"), "12", "12", "3", _).
		
test(readSkip2) :-
	%	readSkip(+Read,           +CharList, -String, -LastChar, -NewRead)
		readSkip(readChars(""), "a", "", "", _).
		
		
		
		
:- end_tests(prot).
 
main:- 
        consult(prot01), main2.
        
main2:- 
        write('--------------------------testing...---------------------------')
        , nl,
        run_tests.
