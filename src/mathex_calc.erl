%%%-------------------------------------------------------------------
%%% @doc
%%%   Evaluates string algebra expressions and simple formulas for
%%%   timeseries data. Ported from babelstats project.
%%% @end
%%%-------------------------------------------------------------------
-module(mathex_calc).

%% API
-export([eval/1, calculate/1]).

%%--------------------------------------------------------------------
%% @doc Evaluate an algebraic expression represented as a string.
-spec eval(string()) -> float().
%%--------------------------------------------------------------------
eval(Algebra) ->
    {ok, Ts, _} = calc_lexer:string(Algebra),
    {ok, Result} = calc_parser:parse(Ts),
    Result.

%%--------------------------------------------------------------------
%% @doc Evaluate a list of {Date, Expression} tuples.
-spec calculate([{calendar:datetime(), string()}]) ->
                 [{calendar:datetime(), float()}].
%%--------------------------------------------------------------------
calculate(Series) ->
    lists:map(fun({Date, Expr}) ->
                      {Date, eval(Expr)}
              end, Series).
