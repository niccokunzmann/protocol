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

:- module(encoding,
		[	encoding/3,
			normalizeEncodingName/2
		]).

equal(X, X).

% encoding(NormedEncodingName, Plain, Encoded) det.
% normalizeEncodingName(Name, NormedEncodingName)

:- use_module(base64).
:- use_module(utf8).

encoding(base64, Plain, Encoded) :- !,
		base64(Plain, Encoded).

encoding(utf8, Plain, Encoded) :- !,
		utf8(Plain, Encoded).

encoding(utf8_be, Plain, Encoded) :- !, 
		utf8(Plain, Encoded, normalByteOrderWithMark).

encoding(utf8_le, Plain, Encoded) :- !,
		utf8(Plain, Encoded, reversedByteOrder).

encoding(ascii, Plain, Plain) :- !.

encoding(Enc, _, _) :-
		throw(error(couldNotFindEncoding(Enc))).

normalizeEncodingName(Name, NormalizedName) :- 
		norm(Name, S1), 
		(	append("utf_", S_2, S1) ->
				append("utf", S_2, S2)
			; 	S2 = S1
		),
		atom_codes(NormalizedName, S2), !.

normalizeEncodingName(Name, NormalizedName) :- 
		atom(Name),
		atom_codes(Name, NameString),
		normalizeEncodingName(NameString, NormalizedName).

norm([Upper|UpperTail], [Lower|LowerTail]) :-
		% to lower case
		between(65, 90, Upper), 
		Lower is Upper+32,
		norm(UpperTail, LowerTail), !.
norm([0'- |T1], [0'_ |T2]) :- 
		norm(T1, T2), !.
		
norm([C| T], [C| T2]) :-
		norm(T, T2), !.
norm([], []).

removeWhiteSpace([W|L], L2) :- 
		whitespace(W), !,
		removeWhitespace(L, L2).
removeWhiteSpace([X|L], [X|L2]) :- 
		removeWhitespace(L, L2).
removeWhitespace([], []).

whitespace(0' ).
whitespace(0'\t).
whitespace(0'\n).
whitespace(0'\r).
whitespace(0'\f).
whitespace(0'\v).
