:- begin_tests(prot). 
%%%%%%%%%%%%%%%%%%%%      Test string reader     %%%%%%%%%%%%%%%%%%%%      
test(testtest) :-
        reverse([1,2], [2,1]).
		
test(whitespace) :-
		length(" \t\n\r\f\v", 6).
 
test(reader1) :- 
		S = "abcdefg", readChars(S, 3, "abc", _).

test(reader2) :- 
        S = "abcdefg", readChars(S, 3, "abc", NewRead), 
		call(NewRead, 3, "def", _).

test(reader3) :- 
        S = "abcdefg", readChars(S, 8, _, NewRead), 
		call(NewRead, 3, [], _).

test(reader4) :- 
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

urks(A, B, C) :- C =:= A + B.
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
		
	
%%%%%%%%%%%%%%%%%%%%      Test stream     %%%%%%%%%%%%%%%%%%%%      

% protocolInputStream(Read, Stack, Predicates)
% 	Predicates is [pStreamFunc("name", predicate), ...]
% 	Stack is ["...", ]
% 	predicate(+Stream, -NewStream)

test(newStream) :- newProtocolInputStream(readChars(""), _).

test(readToken1) :- readToken(readChars("  123456 "), "123456", _).

test(readToken2) :- readToken(readChars("  1234"), "1234", _).


%%%%%%%%%%%%%%%%%%%%      Test protocol     %%%%%%%%%%%%%%%%%%%%      

testEqual(Value, String) :- 
		Read = readChars(String),
		newProtocolInputStream(Read, Stream),
		readTerm(Stream, Value, _).
	
test(proto_1) :- 
		testEqual(123, "push 123 int stop").
test(proto_2) :- 
        testEqual(123, "def :int push int :int :int 123 stop").
test(proto_3) :- 
        testEqual(11111111111111111111, "push 11111111111111111111 int stop").
test(proto_4) :- 
        testEqual(1.3344, "push 1.3344 float stop").
test(proto_5) :- 
        testEqual(1.3344, "def :float push float :float :float 1.3344 stop").
test(proto_6) :- 
        testEqual("hello world", "bound ' hello world' stop").
test(proto_7) :- 
        testEqual("hello world", "bound ' hello world' decode ascii stop").
test(proto_8) :- 
        testEqual("hello world", "bound ' aGVsbG8gd29ybGQ=\n' decode base64 stop").
test(proto_9) :- 
        testEqual([1,2,3], "list push 1 int switch push 2 int switch push 3 int switch switch insert switch insert switch insert stop").
test(proto_10) :- 
        testEqual([1,2,3],   "list def :insint push int insert :insint :insint 3 :insint 2 :insint 1 stop").


		
:- end_tests(prot).
 
main:- 
        consult(prot01), 
        consult(prot01help), 
		main2.
        
main2:- 
        write('--------------------------testing...---------------------------')
        , nl,
        run_tests.
