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


%%%%%%%%%%%%%%%%%%%%      Help     %%%%%%%%%%%%%%%%%%%%      
helpProt(helpProt):- writeln('\
helpProt\nhelpProt(atom)\n\
	get help to the specified predicate\n').
helpProt(readChars):- writeln('\
readChars(+String, +Count, -String, -NewRead)\n\
	\tNewRead(+count, -chars, -nextRead)\n\
	create a stream from a string\n\
	').
helpProt(readUntil):- writeln('\
readUntil(+Read, +CharList, String, LastChar, -NewRead)\n\
	\t    Read(+count, -chars, -nextRead)\n\
	\t NewRead(+count, -chars, -nextRead)\n\
	read until a character of the String occours\n\
	').
helpProt(readUntilCondition):- writeln('\
readUntilCondition(+List, Char)\n\
	true if Char in List\n\
	').
helpProt(readWhile):- writeln('\
readWhile(+Read, Condition, String, LastChar, -NewRead)\n\
	\t   Read(+count, -chars, -nextRead)\n\
	\tNewRead(+count, -chars, -nextRead)\n\
	\tCondition(Char)\n\
	read a character from Read while the condition ist True\n\
	\tif nothing was read LastChar will be ""\n\
	\totherwise LastChar will be the "%" string [Integer]\n\
	').
helpProt(readSkip):- writeln('\
readSkip(+Read, +CharList, LastChar, -NewRead)\n\
readSkip(+Read, +CharList, String, LastChar, -NewRead)\n\
	\t   Read(+count, -chars, -nextRead)\n\
	\tNewRead(+count, -chars, -nextRead)\n\
	read until no character of CharList is read\n\
	').
helpProt:- helpProt(_), fail; !.

