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

:- import(functions).

test_encoding(Encoding, X) :- 
		encoding(Encoding, X, Y),
		encoding(Encoding, Z, Y), 
		equal(X, Z).
		
test_hello_world_string(Encoding) :- 
		X = "hello world my dear!",
		test_encoding(Encoding, X).

test_hello_world_atom(Encoding) :- 
		X = 'hello world my dear!',
		test_encoding(Encoding, X).

test_unicode_string(Encoding) :- 
		range(0, 37, 60000, X),
		test_encoding(Encoding, X).

test_unicode_atom(Encoding) :- 
		range(0, 37, 60000, Z),
		atom_codes(X, Z),
		test_encoding(Encoding, X).

		
:- begin_tests(encoding).

%%%%%%%%%%%% normalize encoding %%%%%%%%%%%%

test(normalize_encoding_01) :- 
		normalizeEncodingName('utf8', 'utf8').

test(normalize_encoding_02) :- 
		normalizeEncodingName('utf-8', 'utf8').

test(normalize_encoding_02) :- 
		normalizeEncodingName("utf-8", 'utf8').

test(normalize_encoding_03) :- 
		normalizeEncodingName('utf-8-le', 'utf8_le').

test(normalize_encoding_04) :- 
		normalizeEncodingName('Ascii', 'ascii').

test(normalize_encoding_05) :- 
		normalizeEncodingName('UTF-8', 'utf8').

%%%%%%%%%%%% base64 - hello world %%%%%%%%%%%%

test(encode_base64_hw) :- 
		test_hello_world_string(base64).

test(encode_base64_hw_atom) :- 
		test_hello_world_atom(base64).

test(encode_base64_hw_whitespace) :- 
		X = "aGV   sbG8gd29ybGQ   gbXk gZGVhciE=\n", 
		%gtrace,
		encoding(base64, Text, X),
		Text = "hello world my dear!".
		
test(encode_base64_hw_whitespace_base) :- 
		X = " aGVsbG8gd29ybGQgbXkgZGVhciE=", 
		%gtrace,
		encoding(base64, Text, X),
		Text = "hello world my dear!".

%%%%%%%%%%%% utf8 - hello world %%%%%%%%%%%%

test(encode_utf8_hw) :- 
		test_hello_world_string(utf8).

test(encode_utf8_hw_atom) :- 
		test_hello_world_atom(utf8).

test(encode_utf8_le_hw) :- 
		test_hello_world_string(utf8_le).

test(encode_utf8_le_hw_atom) :- 
		test_hello_world_atom(utf8_le).

test(encode_utf8_be_hw) :- 
		test_hello_world_string(utf8_be).

test(encode_utf8_be_hw_atom) :- 
		test_hello_world_atom(utf8_be).


%%%%%%%%%%%% utf8 - unicode %%%%%%%%%%%%

test(encode_utf8_unicode) :- 
		test_unicode_string(utf8).

test(encode_utf8_unicode_atom) :- 
		test_unicode_atom(utf8).

test(encode_utf8_le_unicode) :- 
		test_unicode_string(utf8_le).

test(encode_utf8_le_unicode_atom) :- 
		test_unicode_atom(utf8_le).

test(encode_utf8_be_unicode) :- 
		test_unicode_string(utf8_be).

test(encode_utf8_be_unicode_atom) :- 
		test_unicode_atom(utf8_be).




:- end_tests(encoding).


main:- 
		compile(base64),
		compile(utf8),
		compile(functions),
		compile(encoding),
		compile(encodingtest),
		main2.
        
main2:- 
        write('--------------------------testing...---------------------------')
        , nl,
        run_tests.



