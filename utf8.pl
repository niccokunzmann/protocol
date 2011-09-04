
/*  $Id$

    Part of SWI-Prolog

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
	
	The module structure is highly inspired by base64.pl of Jan Wielemaker.
*/

		
:- module(utf8,
	  [ utf8/2,			% ?PlainText, ?Encoded
		utf8/3, 		% ?PlainText, ?Encoded, ?ByteOrder
	    utf8//2			% ?PlainText, ?ByteOrder
	  ]).

:- import(functions:equal).

%%	utf8(+Plain, -Encoded) is det.
%%	utf8(+Plain, -Encoded, ByteOrder) is det.
%%	utf8(-Plain, +Encoded) is det. % todo
%%	utf8(-Plain, +Encoded, ByteOrder) is det. % todo
%
%	Translates between plaintext and utf8  encoded atom or string.
%	See also utf8//2.
% 	ByteOrder is one of 
%		normalByteOrder        		% without bom
%		normalByteOrderWithMark 	% with BOM
%		reversedByteOrder       	% with BOM
%	utf8/2 determines the byte order automatically
%	utf8/3 outputs and takes information about the byte order 

list([]).
list([_|_]).

utf8(Plain, Encoded) :-
	utf8(Plain, Encoded, _).
	
utf8(Plain, Encoded, ByteOrder) :-
	nonvar(Encoded), list(Encoded), !,
	phrase_utf8(Plain, Encoded, ByteOrder).
utf8(Plain, Encoded, ByteOrder) :- 
	nonvar(Plain), list(Plain), !,
	phrase_utf8(Plain, Encoded, ByteOrder).
	
utf8(Plain, Encoded, ByteOrder) :-
	nonvar(Plain), !,
	atom_codes(Plain, PlainCodes),
	phrase_utf8(PlainCodes, EncCodes, ByteOrder),
	atom_codes(Encoded, EncCodes).
utf8(Plain, Encoded, ByteOrder) :-
	nonvar(Encoded), !,
	atom_codes(Encoded, EncCodes),
	phrase_utf8(PlainCodes, EncCodes, ByteOrder),
	atom_codes(Plain, PlainCodes).
	
utf8(_, _, _) :-
	throw(error(instantiation_error, _)).

phrase_utf8(Plain, Encoded, ByteOrder) :- 
	phrase(utf8(Plain, ByteOrder), Encoded).

utf8(Input, ByteOrder) -->
	{ 	nonvar(Input), !,
		(var(ByteOrder) -> 
			equal(ByteOrder, normalByteOrder)
		;	true
		)
	},
 	byteOrderMark(Input, ByteOrder), 
	encode(Input, ByteOrder).

utf8(Output, ByteOrder) -->
	%{gtrace},
	byteOrderMark(Output, ByteOrder), 
	decode(Output, ByteOrder), !.

%%%%%%%%%%%%%%%%%%%%% ByteOrder Mark %%%%%%%%%%%%%%%%%%%

normalByteOrder(A) --> [A] .
normalByteOrder(A, B) --> [A, B] .
normalByteOrder(A, B, C) --> [A, B, C] .
normalByteOrder(A, B, C, D) --> [A, B, C, D] .
normalByteOrderWithMark(A) --> [A] .
normalByteOrderWithMark(A, B) --> [A, B] .
normalByteOrderWithMark(A, B, C) --> [A, B, C] .
normalByteOrderWithMark(A, B, C, D) --> [A, B, C, D] .
reversedByteOrder(A) --> [A] .
reversedByteOrder(A, B) --> [A, B] .
reversedByteOrder(A, B, C) --> [C, B, A] .
reversedByteOrder(A, B, C, D) --> [D, C, B, A] .

byteOrderMark(_, normalByteOrderWithMark) -->
	% try to remove the byte order mark
	normalByteOrder(0xEF, 0xBB, 0xBF), !.
byteOrderMark(_, reversedByteOrder) -->
	% try to remove the byte order mark
	reversedByteOrder(0xEF, 0xBB, 0xBF), !.
	
byteOrderMark(_, normalByteOrder) --> 
	% cannot determine input byte order
	% assuming normal byte order
	% -> wait for error in decodeChar
	[], !.
byteOrderMark(_, reversedByteOrder) -->
	% cannot determine input byte order 
	% -> wait for error in decodeChar
	% assuming reversed byte order
	[], !.

	
%%%%%%%%%%%%%%%%%%%%% Encoding %%%%%%%%%%%%%%%%%%%
	
encodeChar(X, ByteOrder) -->
	{	X =< 0x7F }, !, 
	call(ByteOrder, X).

encodeChar(X, ByteOrder) -->
	{	X =< 0x7FF }, !,
	call(ByteOrder, B, A), 
	{	B is ((X >> 6) /\ 0b00011111) \/ 0b11000000,
		A is ( X       /\ 0b00111111) \/ 0b10000000
	}.
	
encodeChar(X, ByteOrder) -->
	{	X =< 0xFFFF }, !,
	call(ByteOrder, C, B, A), 
	{	C is ((X >> 12) /\ 0b00001111) \/ 0b11100000,
		B is ((X >> 6 ) /\ 0b00111111) \/ 0b10000000,
		A is ((X      ) /\ 0b00111111) \/ 0b10000000
	}.

encodeChar(X, ByteOrder) -->
	{	X =< 0x10FFFF }, !,
	call(ByteOrder, D, C, B, A), 
	{	D is ((X >> 18) /\ 0b00000111) \/ 0b11110000,
		C is ((X >> 12) /\ 0b00111111) \/ 0b10000000,
		B is ((X >> 6 ) /\ 0b00111111) \/ 0b10000000,
		A is ((X      ) /\ 0b00111111) \/ 0b10000000
	}.

encodeChar([C|_], _) -->
	{	throw(error(unicodeOutOfRange(C)))
	},
	[].

encode([X|E], ByteOrder) -->
	encodeChar(X, ByteOrder),
	encode(E, ByteOrder).

encode([], _) -->
	[].
	
%%%%%%%%%%%%%%%%%%%%% Decoding %%%%%%%%%%%%%%%%%%%

decodeChar(A, ByteOrder) -->
	call(ByteOrder, A), 
	{	0b00000000 is A /\ 0b10000000 }, !.
	
decodeChar(X, ByteOrder) -->
	call(ByteOrder, A, B), 
	{	0b11000000 is A /\ 0b11100000 }, !, 
	{	X is (A /\ 0b00011111) << 6 + 
			 (B /\ 0b00111111)
	}.
	
decodeChar(X, ByteOrder) -->
	call(ByteOrder, A, B, C), 
	{	0b11100000 is A /\ 0b11110000 }, !, 
	{	X is (A /\ 0b00001111) << 12 + 
			 (B /\ 0b00111111) <<  6 + 
			 (C /\ 0b00111111)
	}.

decodeChar(X, ByteOrder) -->
	call(ByteOrder, A, B, C, D), 
	{	0b11110000 is A /\ 0b11111000 }, !, 
	{	X is (A /\ 0b00000111) << 18 + 
			 (B /\ 0b00111111) << 12 + 
			 (C /\ 0b00111111) <<  6 + 
			 (D /\ 0b00111111)
	}.
	
decode([Char|E], ByteOrder) -->
	decodeChar(Char, ByteOrder),
	decode(E, ByteOrder).
	
decode([], _) -->
	[].
