-module(mathex_tests).
-include_lib("eunit/include/eunit.hrl").

moving_average_test() ->
    ?assertEqual([1,1.5,2.0], mathex:moving_average([1,2,3])).

average_test() ->
    ?assertEqual(2.0, mathex:average([1,2,3])).

sum_test() ->
    ?assertEqual(6, mathex:sum([1,2,3])).

stdev_sample_test() ->
    R = mathex:stdev_sample([2,4,4,4,5,5,7,9]),
    ?assert(abs(R - 2.138089935299395) < 1.0e-6).

stdev_population_test() ->
    R = mathex:stdev_population([2,4,4,4,5,5,7,9]),
    ?assert(abs(R - 2.0) < 1.0e-6).

variance_test() ->
    R = mathex:variance([1,2,3,4]),
    ?assert(abs(R - 1.25) < 1.0e-6).

covariance_test() ->
    R = mathex:covariance([2.1,2.5,4.0,3.6],[8,12,14,10]),
    ?assert(abs(R - 1.5333333333333332) < 1.0e-6).

correlation_test() ->
    R = mathex:correlation([2.1,2.5,4.0,3.6],[8,12,14,10]),
    ?assert(abs(R - 0.6625738822030289) < 1.0e-6).

correlation_matrix_test() ->
    R = mathex:correlation_matrix([[1,2,3],[2,3,4]]),
    ?assertEqual([{1,2,1.0},{2,1,1.0}], R).

%% edge case tests
single_element_safe_test() ->
    ?assertEqual(0.0, mathex:stdev_sample([1])),
    ?assertEqual(0.0, mathex:covariance([1],[1])),
    ?assertEqual(0.0, mathex:correlation([1],[1])).

calc_eval_test() ->
    ?assertEqual(10.0, mathex_calc:calc("5+5")).

calc_series_test() ->
    D1 = {{2024,1,1},{0,0,0}},
    D2 = {{2024,1,2},{0,0,0}},
    Series = mathex_calc:calc_series([{D1,"1+1"},{D2,"2*3"}]),
    ?assertEqual([{D1,2.0},{D2,6.0}], Series).
