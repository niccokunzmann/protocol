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


:- module(stream, 
		[	ioStream/2,
			characterStream/2,
			characterStreamRead/4,
			characterStreamWrite/4,
			streamRead/2,
			streamWrite/2,
			staticCharRead/4,
			staticCharWrite/4,
			mutableRead/5,
			mutableRead3/5,
			mutableRead4/5,
			mutableWrite/5,
			mutableWrite3/5,
			mutableWrite4/5
		]).

:- use_module(functions).


%%%%%%%%%%%%%%%%%%%%      character stream     %%%%%%%%%%%%%%%%%%%%      


%% characterStreamRead(-String, -Count, +String, +NewRead)
%   NewRead(+count, -chars, -nextRead)
% read characters from a stream
characterStreamRead(X, 0, "", characterStreamRead(X)) :- !.

characterStreamRead("", _, "", NewRead):-
		% yield nothing if stream is empty
		characterStreamRead("", 0, _, NewRead).

characterStreamRead([Char| String], I, [Char | String2], NewRead):-
		I >= 1, I2 is I - 1, 
		characterStreamRead(String, I2, String2, NewRead).

%% characterStreamWrite(-String, -String, +CharsWritten, +NewRead)
%   NewRead(+String, -CharsWritten, -NewRead)
% write characters to a stream

characterStreamWrite(	String, Bytes, C, 
						characterStreamWrite(String2)):-
		length(Bytes, C),
		append(String, Bytes, String2).

%%%%%%%%%%%%%%%%%%%%      static read and write     %%%%%%%%%%%%%%%%%%%%      

staticCharRead(Read, 0, "", staticCharRead(Read)) :- !.
staticCharRead(Read, I, String, staticCharRead(Read)):-
		call(Read, Byte),
		( Byte is -1 -> 
			String = []
		; (	I >= 1, I2 is I - 1, 
			staticCharRead(Read, I2, String2, _),
			equal(String, [Byte | String2])
			)
		).
 
 
staticCharWrite(Read, "", 0, staticCharWrite(Read)) :- !.
staticCharWrite(Write, [Byte | String],  I, staticCharWrite(Write)):-
		call(Write, Byte),
		staticCharWrite(Write, String, I2, _),
		I is I2 + 1 .
		
%%%%%%%%%%%%%%%%%%%%      mutable read and write     %%%%%%%%%%%%%%%%%%%%      

mutableRead3(Read, Stream, 0, [], mutableRead3(Read, Stream)) :- !.
mutableRead3(Read, Stream, I, String, mutableRead3(Read, NewStream)):-
		I >= 1, I2 is I - 1, 
		call(Read, Stream, X, Stream2),
		mutableRead(	Read, Stream2, I2, String2, 
		            	mutableRead3(Read, NewStream)
					),
		equal(String, [X | String2])
		.

mutableRead4(Read, Stream, Count, Out, mutableRead4(Read, NewStream)) :- 
		call(Read, Stream, Count, Out, NewStream).
		
mutableRead(Read, Stream, Count, Out, NewRead):- 
		Read2 =.. [Read, Stream, Count, Out, NewRead],
		callable(Read2) ->
			mutableRead4(Read, Stream, Count, Out, NewRead)
		;	mutableRead3(Read, Stream, Count, Out, NewRead).

mutableWrite3(Write, Stream, [], 0, mutableWrite3(Write, Stream)) :- !.
mutableWrite3(Write, Stream, List, C, mutableWrite3(Write, NewStream)):-
		call(Write, Stream, Element, Stream2) -> (
			mutableWrite3(	Write, Stream2, C2, List2, 
							mutableWrite3(Write, NewStream)
						),
			C is C2 - 1,
			equal(List, [Element | List2])
			)
		;(	equal(List, []),
			equal(C, 0), 
			equal(Stream, NewStream)
			).

mutableWrite(Write, Stream, Count, Out, NewRead):- 
		Write2 =.. [Write, Stream, Count, Out, NewRead],
		callable(Write2) ->
			mutableWrite4(Write, Stream, Count, Out, NewRead)
		;	mutableWrite3(Write, Stream, Count, Out, NewRead).

mutableWrite4(Write, Stream, Count, Out, mutableWrite4(Write, NewStream)) :- 
		call(Write, Stream, Count, Out, NewStream).
		
		
%%%%%%%%%%%%%%%%%%%%      streams     %%%%%%%%%%%%%%%%%%%%      

% ioStream(+-Stream, -+IOStream)
% requires put_byte and get_byte

ioStream(Stream, ioStream(Stream)) :- is_stream(Stream).

characterStream(String, characterStream(String)).


%%%%%%%%%%%%%%%%%%%%      stream read and write     %%%%%%%%%%%%%%%%%%%%      

/* characterStream("characters", Stream).
 * ioStream(Stream, IOStream) 
 * 
 * 
 * streamRead(+-Stream, -+Read)
 * streamWrite(+-Stream, -+Read)
 * call(Read, Count, [...], NewRead)
 * call(Write, [], Count, NewWrite)
 * 
 * 
 */

:- multifile(streamRead).

streamRead(characterStream(String), characterStreamRead(String)) :- !.
streamRead(ioStream(Stream), staticCharRead(get_byte(Stream))) :- !.

:- multifile(streamWrite).

streamWrite(characterStream(String), characterStreamWrite(String)) :- !.
streamWrite(ioStream(Stream), staticCharWrite(put_byte(Stream))) :- !.
