-module(tut).
-export([
         fac/1,
         double/1,
         mult/2,
         convert/2,
         fibo/1,
         list_max/1,
         reverse/1
        ]).

double(X) ->
    2 * X.

fac(1) ->
    1;

fac(N) ->
    N * fac(N - 1).

mult(X, Y) ->
    X * Y.

convert(M, inch) ->
    M / 2.54;

convert(M, centimeter) ->
    M * 2.54.

fibo(0) -> 0;
fibo(1) -> 1;
fibo(N) when N > 0 -> fibo(N - 1) + fibo(N - 2).

list_max([Head|Rest]) ->
    list_max(Rest, Head).

list_max([], Res) ->
    Res;

list_max([Head|Rest], Result_so_far) when Head > Result_so_far ->
    list_max(Rest, Head);

list_max([Head|Rest], Result_so_far) ->
    list_max(Rest, Result_so_far).

reverse(List) ->
    reverse(List, []).

reverse([Head | Rest], Reversed_List) ->
    reverse(Rest, [Head | Reversed_List]);

reverse([], Reversed_List) ->
    Reversed_List.
