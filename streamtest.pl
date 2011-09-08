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

:- use_module(stream).

:- begin_tests(stream). 
%%%%%%%%%%%%%%%%%%%%      Test string characterStream_     %%%%%%%%%%%%%%%%%%%%      
test(characterStream_1) :- 
		S = "abcdefg", 
		characterStream(S, Stream),
		streamRead(Stream, Read0),
		call(Read0, 3, "abc", _).

test(characterStream_2) :- 
        S = "abcdefg", 
		characterStream(S, Stream),
		streamRead(Stream, Read0),
		call(Read0, 3, "abc", NewRead), 
		call(NewRead, 3, "def", _).

test(characterStream_3) :- 
        S = "abcdefg", 
		characterStream(S, Stream),
		streamRead(Stream, Read0),
		call(Read0, 8, _, NewRead), 
		call(NewRead, 3, [], _).

test(characterStream_4) :- 
        S = "abcdefg", 
		characterStream(S, Stream),
		streamRead(Stream, Read0),
		call(Read0, 1, "a", Read1), 
		call(Read1, 1, "b", Read2),
		call(Read2, 1, "c", Read3),
		call(Read3, 1, "d", Read4),
		call(Read4, 1, "e", Read5),
		call(Read5, 1, "f", Read6),
		call(Read6, 1, "g", Read7),
		equal(Read7, characterStreamRead("")).

:- end_tests(stream). 
 
main:- 
        consult(encoding), 
        consult(base64), 
        consult(utf8), 
		consult(stream),
		main2.
        
main2:- 
        write('--------------------------testing...---------------------------')
        , nl,
        run_tests.
