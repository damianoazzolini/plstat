:- module(test_plstat, [test_plstat/0,test_plstat_rev/0]).
:- use_module(library(plunit)).

:- set_random(seed(100)).
:- ['../src/plstat'].

test_list([
    mean,
    sum_of_squares,
    variance,
    std_dev,
    range,
    midrange,
    mean_absolute_deviation,
    occurrences
  ]).

test_plstat:-
    test_list(L),
	run_tests(L).

test_plstat_rev:-
    test_list(L),
	reverse(L,Rev),
    run_tests(Rev).

close_to(V,V1):-
    VU is V1 + 0.0005, 
    VL is V1 - 0.0005,
    V >= VL,
    V =< VU.

:- begin_tests(mean, []).
test(mean_1):- mean([1,2,3],2).
:- end_tests(mean).

:- begin_tests(sum_of_squares, []).
test(sum_of_squares_1):- sum_of_squares([1,2,3],2).
:- end_tests(sum_of_squares).

:- begin_tests(variance, []).
test(variance_1):- variance([1,2,4,6,7,8,9],V),close_to(V,9.2380952).
:- end_tests(variance).

:- begin_tests(std_dev, []).
test(variance_1):- std_dev([1,2,4,6,7,8,9],S),close_to(S,3.039424).
:- end_tests(std_dev).

:- begin_tests(range, []).
test(range_1):- range([1,2,4,6,7,8,9],8).
:- end_tests(range).

:- begin_tests(midrange, []).
test(midrange_1):- midrange([1,2,4,6,7,8,9],4).
:- end_tests(midrange).

:- begin_tests(mean_absolute_deviation, []).
test(mean_absolute_deviation_1):- mean_absolute_deviation([1,2,4,6,7,8,9],M),close_to(M,2.5306122).
:- end_tests(mean_absolute_deviation).

:- begin_tests(occurrences, []).
test(occurrences_1):- occurrences([1,2,4,6,7,8,9,1],1,2).
:- end_tests(occurrences).