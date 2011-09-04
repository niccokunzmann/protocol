


:- use_module(utf8).
:- use_module(functions).

writeOut(X) :- 
		number(X), !,
		bin(X, B),
		write(B), write(', '), 
		writeln(X).
writeOut(L) :- foreach(I, L, writeOut(I)).

wba(N) :- 
		number(N), !,
		atom_codes(X, [N]),
		wba(X).
wba(C) :- 
		utf8(C, X), 
		writeln(X), 
		atom_codes(X, S),
		% bin(S, X2), gtrace,
		writeOut(S), !.
		

:- begin_tests(utf8). 

%%%%%%%%%%%%%%%%%%% Encoding Tests %%%%%%%%%%%%%%%%%%%%

test(utf8_encode_nothing) :- 
		equal(Y, ''),
		utf8(Y, X),
		equal('', X).

test(utf8_encode_hello_world) :- 
		utf8('hello world!', 'hello world!').

test(utf8_encode_1234) :- 
		utf8('\u1234', X),
		equal(X, '\u00e1\u0088\u00b4').

test(utf8_encode_ffff) :- 
		utf8('\uffff', X),
		equal(X, '\u00ef\u00bf\u00bf').
		
% Buchstabe y 	
% 	U+0079
% 	0x79
test(utf8_encode_y) :- 
		utf8('\u0079', X),
		equal(X, '\u0079').

% Buchstabe ä 	
% 	U+00E4 	
% 	0xC3 0xA4
test(utf8_encode_a_uml) :- 
		utf8('\u00E4', X),
		equal(X, '\u00c3\u00a4').

% Zeichen für eingetragene Marke 
% 	U+00AE 
% 	0xC2 0xAE
test(utf8_encode_trademark) :- 
		utf8('\u00AE', X),
		equal(X, '\u00C2\u00AE').

% Eurozeichen 
% 	U+20AC 	
% 	0xE2 0x82 0xAC
test(utf8_encode_euro_sign) :- 
		utf8('\u20ac', X),
		equal(X, '\u00e2\u0082\u00ac').

% Violinschlüssel 
% 	U+1D11E
% 	0xF0 0x9D 0x84 0x9E
test(utf8_encode_Violin) :- 
		utf8([0x1d11e], X),
		equal(X, "\u00F0\u009D\u0084\u009e").

%%%%%%%%%%%%%%%%%%% Decoding Tests %%%%%%%%%%%%%%%%%%%%
		
test(utf8_decode_nothing) :- 
		equal(Y, ''),
		utf8(X, Y),
		equal('', X).

test(utf8_decode_1234) :- 
		equal(Y, '\u00e1\u0088\u00b4'),
		utf8(X, Y),
		equal('\u1234', X).

test(utf8_decode_ffff) :-
		equal(Y, '\u00ef\u00bf\u00bf'),
		utf8(X, Y),
		equal('\uffff', X).
		
% Buchstabe y 	
% 	U+0079
% 	0x79
test(utf8_decode_y) :- 
		equal(Y, '\u0079'),
		utf8(X, Y),
		equal('\u0079', X).

% Buchstabe ä 	
% 	U+00E4 	
% 	0xC3 0xA4
test(utf8_decode_a_uml) :- 
		equal(Y, '\u00c3\u00a4'),
		utf8(X, Y),
		equal('\u00E4', X).

% Zeichen für eingetragene Marke 
% 	U+00AE 
% 	0xC2 0xAE
test(utf8_decode_trademark) :- 
		equal(Y, '\u00C2\u00AE'),
		utf8(X, Y),
		equal('\u00AE', X).

% Eurozeichen 
% 	U+20AC 	
% 	0xE2 0x82 0xAC
test(utf8_decode_euro_sign) :- 
		equal(Y, '\u00e2\u0082\u00ac'),
		utf8(X, Y),
		equal('\u20ac', X).

% Violinschlüssel 
% 	U+1D11E
% 	0xF0 0x9D 0x84 0x9E
test(utf8_decode_Violin) :- 
		equal(Y, "\u00F0\u009D\u0084\u009e"),
		utf8(X, Y),
		equal([0x1d11e], X).

% sadly this is not working 
% ?- atom_codes(A, [0x1d11e]), atom_codes(A, [0x1d11e]).
% false.

%%%%%%%%%%%%%%%%%%% End Tests %%%%%%%%%%%%%%%%%%%%

:- end_tests(utf8).


main:- 
		compile(utf8),
		compile(utf8test),
		main2.
        
main2:- 
        write('--------------------------testing...---------------------------')
        , nl,
        run_tests.


