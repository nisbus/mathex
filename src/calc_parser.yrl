Nonterminals
	eval expr term factor.

Terminals
	open close add minus multiply divide float power fn.

Rootsymbol eval.

Left 100 float.
Left 200 open.
Left 300 close.
Left 400 fn.
Left 500 add.
Left 600 minus.
Left 700 multiply.
Left 800 divide.
Left 900 power.

eval -> expr		     : '$1'.

expr -> expr add term	     : add('$1','$3').
expr -> expr minus term	     : subtract('$1','$3').
expr -> term                 : '$1'.

term -> term multiply factor : multiply('$1','$3').
term -> term divide   factor : divide('$1','$3').
term -> term power    factor : power('$1','$3').
term -> term fn       factor : unwrap('$3').
term -> factor		     : '$1'.

factor -> float		     : unwrap('$1').
factor -> open expr close    : '$2'.
factor -> fn factor          : math(unwrap('$1'),'$2').
factor -> fn                 : math(unwrap('$1')).
    
Erlang code.



unwrap({_,_,V}) -> V;
unwrap({_,V}) -> V;
unwrap(V) -> V.

add(A,B) ->
    A+B.
subtract(A,B) ->
    A-B.
divide(A,B) ->
    A/B.
multiply(A,B) ->
    A*B.
power(A,B) ->
    math:pow(A,B).
math(pi) ->
    math:pi();
math(Other) ->
    io:format("Unknown operator ~p~n",[Other]),
    0.0.
math(sqrt,A) ->
    math:sqrt(A);
math(sin,A) ->
    math:sin(A);
math(cos,A) ->
    math:cos(A);
math(tan,A) ->
    math:tan(A);
math(asin,A) ->
    math:asin(A);
math(acos,A) ->
    math:acos(A);
math(atan,A) ->
    math:atan(A);
math(sinh,A) ->
    math:sinh(A);
math(cosh,A) ->
    math:cosh(A);
math(tanh,A) ->
    math:tanh(A);
math(asinh,A) ->
    math:asinh(A);
math(acosh,A) ->
    math:acosh(A);
math(atanh,A) ->
    math:atanh(A);
math(exp,A) ->
    math:exp(A);
math(log,A) ->
    math:log(A);
math(erf,A) ->
    math:erf(A);
math(erfc,A) ->
    math:erfc(A);
math(Other,_A) ->
    io:format("Unknown operator ~p~n",[Other]),
    0.0.
