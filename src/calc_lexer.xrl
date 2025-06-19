Definitions.

Digit           = [0-9]
WS 	  	= ([\000-\s]|%.*)
Plus	  	= \+
Minus	  	= \-
Multiply 	= \*
Divide		= \/
Power		= \^
Function	= [A-Za-z]
Open		= \(
Close		= \)
Op		= [\+|\^|\*|\/]

Rules.

\-?{Digit}+		  		    : {token, {float, TokenLine, convert_to_float(TokenChars)}}.
\-?{Digit}+\.{Digit}+((E|e)(\+|\-)?{D}+)?   : {token, {float, TokenLine, list_to_float(TokenChars)}}.
{Plus}			           	    : {token, {add, TokenLine, list_to_atom(TokenChars)}}.
{Minus}		           	    	    : {token, {minus, TokenLine, list_to_atom(TokenChars)}}.
{Multiply}		           	    : {token, {multiply, TokenLine, list_to_atom(TokenChars)}}.
{Divide}	  	           	    : {token, {divide, TokenLine, list_to_atom(TokenChars)}}.
{Power}			           	    : {token, {power, TokenLine, list_to_atom(TokenChars)}}.
{Open}			           	    : {token, {open, TokenLine, list_to_atom(TokenChars)}}.
{Close}			           	    : {token, {close, TokenLine, list_to_atom(TokenChars)}}.
{Function}+                        	    : {token, {fn, TokenLine, list_to_atom(TokenChars)}}.
{WS}+    		           	    : skip_token.

Erlang code.

convert_to_float(List) ->
    list_to_float(List++".0000000").
