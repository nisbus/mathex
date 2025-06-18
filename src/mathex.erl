%%%-------------------------------------------------------------------
%%% @author nisbus <nisbus@gmail.com>
%%% @copyright (C) 2013, 
%%% @doc
%%%   A statistics module with some common statistics.
%%%   Take note that this lib will not handle non numerical data
%%%   gracefully.
%%% @end
%%% Created : 23 Feb 2013 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(mathex).

%% API
-export([moving_average/1,average/1,sum/1,
         stdev_sample/1,stdev_population/1,
         skew/1,kurtosis/1,variance/1,
         covariance/2,correlation/2,correlation_matrix/1]).
%%%===================================================================
%%% API
%%%===================================================================
-spec moving_average(Data :: [] | [float()|integer]) -> []|[float()].
%% @doc Calculate the cumulative moving average for `Data`.
moving_average([]) -> 
    [];
moving_average([H|_] = Data) when is_list(Data) ->
    {_,Res,_} = lists:foldl(fun(X,Acc) -> 
			{Count, Res,CurrentAv} = Acc,
			case Count of
			    0 -> {1, [X], X};
			    _ ->  
				Cav = ((CurrentAv * Count+X))/(Count+1),
				{Count+1,[Cav|Res], Cav}
			end 
		end,{0,[],H},Data),
    lists:reverse(Res).

-spec stdev_sample(Data :: [] | [float()|integer()]) -> float().
%% @doc Sample standard deviation using n-1 in the denominator.
stdev_sample([])->
     0.0;
stdev_sample(Data) when is_list(Data) ->
    case length(Data) of
        N when N < 2 ->
            0.0;
        C ->
            A = average(Data),
            math:sqrt(lists:foldl(fun(X,Acc) ->
                                    Acc+math:pow(X-A,2)/(C-1)
                                end, 0, Data))
    end.

-spec stdev_population(Data :: [] | [float()|integer()]) -> float().
%% @doc Population standard deviation.
stdev_population([]) ->
    0.0;
stdev_population(Data) when is_list(Data) -> 
    A = average(Data),
    math:sqrt(average(lists:map(fun(X) ->
			math:pow(X-A,2)
		end,Data))).

-spec skew(Data :: [] | [float()|integer()]) -> float().
%% @doc Unbiased estimator of skewness.
skew([])->
    0.0;
skew(Data) when is_list(Data) ->
    C = length(Data),
    case C < 3 of
        true -> 0.0;
        false ->
            A = average(Data),
            Std = stdev_sample(Data),
            Mult = C / ((C-1)*(C-2)),
            sum(lists:map(fun(X) ->
                                  math:pow(((X-A)/Std),3)
                          end, Data)) * Mult
    end.
    
-spec kurtosis(Data :: [] | [float()|integer()]) -> float().
%% @doc Unbiased estimator of kurtosis.
kurtosis([]) ->
    0.0;
kurtosis(Data) when is_list(Data) ->
    C = length(Data),
    case C < 4 of
        true -> 0.0;
        false ->
            A = average(Data),
            Std = stdev_sample(Data),
            Mult = (C * (C+1)) / ((C-1) * (C-2) * (C-3)),
            Sub = 3 * math:pow(C-1,2) / ((C-2) * (C-3)),
            Mult * sum(lists:map(fun(X) ->
                                        math:pow(((X-A)/Std),4)
                                end, Data)) - Sub
    end.

-spec variance(Data :: [] | [float()|integer()]) -> float().
%% @doc Population variance.
variance([]) ->
    0.0;
variance(Data) when is_list(Data) ->
    A = average(Data),
    C = length(Data),
    sum(lists:map(fun(X) ->
			  math:pow(X - A,2)
		  end,Data))/C.

-spec covariance(Xs :: [] | [float()|integer()],Ys :: [] | [float()|integer()]) -> float().
%% @doc Sample covariance of two data sets. Lists of unequal length are truncated to the shortest.
covariance([],_) ->
    0.0;
covariance(_,[]) ->
    0.0;
covariance(Xs,Ys) when is_list(Xs) and is_list(Ys) ->
    Pairs = lists:zip(Xs,Ys),
    case length(Pairs) of
        N when N < 2 -> 0.0;
        Len ->
            X1 = [X || {X,_} <- Pairs],
            Y1 = [Y || {_,Y} <- Pairs],
            AveX = average(X1),
            AveY = average(Y1),
            sum([ (X-AveX)*(Y-AveY) || {X,Y} <- Pairs ])/(Len-1)
    end.
    
-spec correlation(Xs :: [] | [float()|integer()],Ys :: [] | [float()|integer()]) -> float().
%% @doc Pearson correlation coefficient for two data sets.
correlation([],_) ->
    0.0;
correlation(_,[]) ->
    0.0;
correlation(Xs,Ys) when is_list(Xs) and is_list(Ys) ->
    Pairs = lists:zip(Xs,Ys),
    case length(Pairs) of
        N when N < 2 -> 0.0;
        _ ->
            X1 = [X || {X,_} <- Pairs],
            Y1 = [Y || {_,Y} <- Pairs],
            StdX = stdev_sample(X1),
            StdY = stdev_sample(Y1),
            case StdX =:= 0.0 orelse StdY =:= 0.0 of
                true -> 0.0;
                false ->
                    Cov = covariance(X1,Y1),
                    Cov/(StdX*StdY)
            end
    end.

-spec correlation_matrix(ListOfLists :: [] | [[float()|integer()]]) -> [{integer(),integer(),float()}].
%% @doc Correlate each list in `ListOfLists` with the others.
correlation_matrix([]) ->
    [];
correlation_matrix(ListOfLists) ->
    {_,R} = lists:foldl(fun(X,{Index,Res}) ->
			{_,C} = get_correlations(Index,X,ListOfLists),			
			{Index+1,[{Index,C}|Res]}
			end, {1,[]},ListOfLists),
    lists:reverse(lists:flatten(lists:map(fun({L,Res}) ->
		      lists:map(fun({I,X}) ->
					{L,I,X}
				end,Res)
	      end,R))).

-spec average(Data :: [] | [float()|integer()]) -> float().
%% @doc Arithmetic mean of the list.
average([]) ->
    0.0;
average(Data) ->
    sum(Data)/length(Data).

-spec sum(Data :: [] | [float()|integer()]) -> float()|integer().
%% @doc Sum of all elements in `Data`.
sum([]) ->
    0.0;
sum(Data) ->
    lists:foldl(fun(X,Acc) ->
			Acc+X
		end,0,Data).
%%%===================================================================
%%% Internal functions
%%%===================================================================
get_correlations(Index,Item,ListOfLists) ->
    lists:foldl(fun(X,{Idx,Res}) ->
			case Idx =:= Index of
			    true -> {Idx+1,Res};
			    false ->			
				Correl = correlation(Item,X),
				{Idx+1,[{Idx,Correl}|Res]}
			end
		end,{1,[]},ListOfLists).
