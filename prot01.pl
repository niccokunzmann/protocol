
%  read

readChars(X, 0, [], readChars(X)).

readChars([], _, [], NewRead):-
		readChars([], 0, _, NewRead).

readChars([Char| String], I, [Char | String2], NewRead):-
		I >= 1, I2 is I - 1, 
		readChars(String, I2, String2, NewRead).




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

urks(A, B, C):- C =:= A + B.
test(urks) :-
		call(urks(1, 1), 2).


%%%%%%%%%%%%%%%%%%%%      Test read until     %%%%%%%%%%%%%%%%%%%%      

test(readUntil1) :-
		readUntil(readChars("123456789"), 100, "123456789").

test(readUntil2) :-
		readUntil(readChars(""), "a", []).

		
		
		
		
		
:- end_tests(prot).
 
main:- 
        consult(prot01), main2.
        
main2:- 
        write('--------------------------testing...---------------------------')
        , nl,
        run_tests.
