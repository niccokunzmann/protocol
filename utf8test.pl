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

%%%%%%%%%%%%%%%%%%% Encode with BOM %%%%%%%%%%%%%%%%%%%%
test(utf8_decode_stump_of_tree_reversed) :- 
		% with BOM
		% U+233B4
		equal([0x233B4], X),
		utf8(X, Y, reversedByteOrder),
		equal(Y, [	0xBF , 0xBB , 0xEF, 
					0xB4 , 0x8E , 0xA3 , 0xF0]).

test(utf8_decode_stump_of_tree_with_BOM) :- 
		% with BOM
		% U+233B4
		equal([0x233B4], X),
		utf8(X, Y, normalByteOrderWithMark),
		equal(Y, [	0xEF, 0xBB , 0xBF , 
					0xF0 , 0xA3 , 0x8E , 0xB4]).

%%%%%%%%%%%%%%%%%%% Decode with BOM %%%%%%%%%%%%%%%%%%%%
test(utf8_decode_stump_of_tree) :- 
		% with BOM
		% U+233B4
		equal(Y, [	0xEF, 0xBB , 0xBF , 
					0xF0 , 0xA3 , 0x8E , 0xB4]),
		utf8(X, Y, BO),
		equal([0x233B4], X),
		equal(BO, normalByteOrderWithMark).

test(utf8_decode_stump_of_tree_reversed) :- 
		% with BOM
		% U+233B4
		equal(Y, [	0xBF , 0xBB , 0xEF, 
					0xB4 , 0x8E , 0xA3 , 0xF0]),
		utf8(X, Y, BO),
		equal([0x233B4], X),
		equal(BO, reversedByteOrder).

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


