%%%-------------------------------------------------------------------
%%% @doc
%%%   Evaluates string algebra expressions and simple formulas for
%%%   timeseries data. Ported from babelstats project.
%%% @end
%%%-------------------------------------------------------------------
-module(mathex_calc).

%% API
-export([eval/1, calc/1, calc_series/1]).

%%--------------------------------------------------------------------
%% @doc Evaluate an algebraic expression represented as a string.
-spec eval(string()) -> float().
%%--------------------------------------------------------------------
eval(Algebra) ->
    {ok, Ts, _} = calc_lexer:string(Algebra),
    {ok, Result} = calc_parser:parse(Ts),
    Result.

%%--------------------------------------------------------------------
%% @doc Calculate a math expression and return the result.
-spec calc(string()) -> float().
%%--------------------------------------------------------------------
calc(Expr) ->
    eval(Expr).

%%--------------------------------------------------------------------
%% @doc Evaluate a list of {Date, Expression} tuples.
-spec calc_series([{calendar:datetime(), string()}]) ->
                 [{calendar:datetime(), float()}].
%%--------------------------------------------------------------------
calc_series(Series) ->
    lists:map(fun({Date, Expr}) ->
                      {Date, eval(Expr)}
              end, Series).
