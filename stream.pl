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

characterStreamWrite(	String, [Char | Written], C, 
						characterStreamWrite([Char|String2])):-
		characterStreamWrite(String, C2, Written, characterStreamWrite(String2)),
		C is C2 + 1.
		
characterStreamWrite(X, "", 0, characterStreamWrite(X)) :- !.

%%%%%%%%%%%%%%%%%%%%      static read and write     %%%%%%%%%%%%%%%%%%%%      

staticRead(Read, 0, "", staticRead(Read)) :- !.
staticRead(Read, I, String, Read):-
		call(Read, Byte),
		( Byte is -1 -> 
			String = []
		; (	I >= 1, I2 is I - 1, 
			staticRead(Read, I2, String2, _),
			equal(String, [Byte | String2])
			)
		).
 
 
staticRead(Read, 0, "", staticRead(Read)) :- !.
staticRead(Read, I, String, Read):-
		call(Read, Byte),
		( Byte is -1 -> 
			String = []
		; (	I >= 1, I2 is I - 1, 
			staticRead(Read, I2, String2, _),
			equal(String, [Byte | String2])
			)
		).

%%%%%%%%%%%%%%%%%%%%      streams     %%%%%%%%%%%%%%%%%%%%      

ioStream(Stream, ioStream(Stream)).

characterStream(String, characterStream(String)).


%%%%%%%%%%%%%%%%%%%%      stream read and write     %%%%%%%%%%%%%%%%%%%%      

/* characterStream("character", Stream).
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


streamRead(characterStream(String), characterStreamRead(String)).
streamRead(ioStream(String), staticRead(String)).

streamWrite(characterStream(String), characterStreamWrite(String)).
streamWrite(ioStream(String), ioStreamWrite(String)).
