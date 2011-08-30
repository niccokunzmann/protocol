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

