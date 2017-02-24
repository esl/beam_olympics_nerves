-module(bo_oldschool_flags).

-behaviour(bo_task).

-export([description/0, spec/0, score/0, timeout/0, tests/0]).

%%==============================================================================
%% API
%%==============================================================================
-spec description() -> binary().
description() -> <<"Oldschool Flags: The thing about interfacing old systems is"
                   " using their standards. Make a function that receives a "
                   "list of booleans and the size of an integer (char, short, "
                   "int) and return the binary representation of those flags. "
                   "For example, if you receive ([:true, :false, :false, :true,"
                    " :true], :char), return <<0::size(1), 0::size(1), "
                    "0::size(1), 1::size(1), 0::size(1), 0::size(1), "
                    "1::size(1), 1::size(1)>>.">>.

-spec spec() -> bo_task:spec().
spec() -> #{ input => [<<"[boolean()]">>, <<"char | short | int">>]
           , output => <<"binary()">>
           }.

-spec score() -> 450.
score() -> 450.

-spec timeout() -> 5000.
timeout() -> 5000.

-spec tests() -> [bo_task:test()].
tests() -> [build_test(Case) || Case <- cases()].

build_test({Bools, Type}) ->
  R = solve(Bools, Type),
  fun(Fun) ->
    try Fun(Bools, Type) of
      R   -> ok;
      Bad -> {error, #{ input => [Bools, Type]
                      , output => Bad
                      , expected => R
                      }}
    catch
      _:Error ->
        {error, #{ input => [Bools, Type]
                 , output => Error
                 , stack => erlang:get_stacktrace()
                 , expected => <<"Well, it should at least work :P">>
                 }}
    end
  end.

%%==============================================================================
%% Utils
%%==============================================================================
cases() ->
  [build_case() || _ <- lists:seq(1, 20)].

build_case() ->
  Type = get_random_type(),
  {[get_random_bool() ||
    _ <- lists:seq(1, get_size(Type))], Type}.

get_random_type() -> lists:nth(rand:uniform(3), [char, short, int]).
get_random_bool() -> rand:uniform() > 0.5.

solve(L, Type) ->
  <<(sum(lists:reverse(L), 1, 0)):(get_size(Type))/unsigned-integer>>.

sum([], _, Acc) ->
  Acc;
sum([H | T], N, Acc) ->
  sum(T, N * 2, case H of true -> Acc + N; _ -> Acc end).

get_size(char)  -> 8;
get_size(short) -> 16;
get_size(int)   -> 32.