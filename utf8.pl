
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
	
	The module structure is highly inspired by the base64.pl of Jan Wielemaker.
*/

		
:- module(utf8,
	  [ utf8/2,			% ?PlainText, ?Encoded
	    utf8//1			% ?PlainText
	  ]).



%%	utf8(+Plain, -Encoded) is det.
%%	utf8(-Plain, +Encoded) is det.
%
%	Translates between plaintext and utf8  encoded atom or string.
%	See also utf8//1.

list([]).
list([_|_]).

utf8(Plain, Encoded) :-
	nonvar(Encoded), list(Encoded), !,
	phrase(utf8(Plain), Encoded).
utf8(Plain, Encoded) :- 
	nonvar(Plain), list(Plain), !,
	phrase(utf8(Plain), Encoded).
utf8(Plain, Encoded) :-
	nonvar(Plain), !,
	atom_codes(Plain, PlainCodes),
	phrase(utf8(PlainCodes), EncCodes),
	atom_codes(Encoded, EncCodes).
utf8(Plain, Encoded) :-
	nonvar(Encoded), !,
	atom_codes(Encoded, EncCodes),
	phrase(utf8(PlainCodes), EncCodes),
	atom_codes(Plain, PlainCodes).
utf8(_, _) :-
	throw(error(instantiation_error, _)).


utf8(Input) -->
	{ nonvar(Input) }, !,
% 	byteOrderMark(Input), % todo
	encode(Input).
utf8(Output) -->
	decode(Output).
	
encode([X|E]) -->
	{	X =< 0x7F }, !, 
	[X],
	encode(E).

encode([X|E]) -->
	{	X =< 0x7FF }, !,
	[B, A], 
	{	B is ((X >> 6) /\ 0b00011111) \/ 0b11000000,
		A is ( X       /\ 0b00111111) \/ 0b10000000
	}, 
	encode(E).
	
encode([X|E]) -->
	{	X =< 0xFFFF }, !,
	[C, B, A], 
	{	C is ((X >> 12) /\ 0b00001111) \/ 0b11100000,
		B is ((X >> 6 ) /\ 0b00111111) \/ 0b10000000,
		A is ((X      ) /\ 0b00111111) \/ 0b10000000
	}, 
	encode(E).

encode([X|E]) -->
	{	X =< 0x10FFFF }, !,
	[D, C, B, A], 
	{	D is ((X >> 18) /\ 0b00000111) \/ 0b11110000,
		C is ((X >> 12) /\ 0b00111111) \/ 0b10000000,
		B is ((X >> 6 ) /\ 0b00111111) \/ 0b10000000,
		A is ((X      ) /\ 0b00111111) \/ 0b10000000
	}, 
	encode(E).

encode([C|_]) -->
	{	throw(error(unicodeOutOfRange(C)))
	},
	[].

encode([]) -->
	[].
	
decode([A|E]) -->
	{	A =< 0b01111111 }, !, 
	[A], 
	decode(E).
	
decode([A, B|E]) -->
	{	A =< 0b11011111 }, !, 
	[X],
	{	X is (A /\ 0b00011111) << 6 + 
			 (B /\ 0b00111111)
	},
	decode(E).
	
