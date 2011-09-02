
:- module(encoding,
		[	encoding/3,
			normalizeEncodingName/2
		]).

equals(X, X).

:- consult(base64).

encoding(base64, Plain, Encoded) :- 
		( atom(Plain); atom(Encoded) ) -> 
			base64(Plain, Atom).
encoding(base64, Plain, Encoded) :- 
		phrase(base64(PlainCodes), EncCodes).

encoding(ascii, Plain, Plain).


normalizeEncodingName(Name, NormalizedName) :- 
		toLower(Name, NormalizedName), 
		equals(Name, "utf-8") -> equals(NormalizedName, "utf8").

normalizeEncodingName(Name, NormalizedName) :- 
		atom(Name),
		atom_codes(Name, NameString),
		findEncoding(NameString, NormalizedString),
		atom_codes(NormalizedName, NormalizedString).
		

toLower([], []).
toLower([Upper|UpperTail], [Lower|LowerTail]) :-
		( 	between(65, 90, Upper), Lower is Upper+32
		; 	Lower is Upper 
		),
		toLower(UpperTail, LowerTail).