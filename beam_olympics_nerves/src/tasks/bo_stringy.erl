-module(bo_stringy).

-behaviour(bo_task).

-export([description/0, spec/0, score/0, timeout/0, tests/0]).

%%==============================================================================
%% API
%%==============================================================================
-spec description() -> binary().
description() -> <<"Stringy: We all love working with strings on elixir (erlang"
                   " binaries), why not have some fun capitalising every other "
                   "WORD in a sentence? So \"a horse is a horse, of course, of "
                   "course\" becomes \"A horse Is a Horse, of Course, of "
                   "Course\"">>.

-spec spec() -> bo_task:spec().
spec() -> #{input => [<<"binary()">>], output => <<"binary()">>}.

-spec score() -> 250.
score() -> 250.

-spec timeout() -> 5000.
timeout() -> 5000.

-spec tests() -> [bo_task:test()].
tests() -> [build_test(list_to_binary(Str)) || Str <- build_cases()].

build_test(Bin) ->
  fun(Fun) ->
    R = solve(Bin),
    try Fun(Bin) of
      R   -> ok;
      Bad -> {error, #{ input => Bin
                      , output => Bad
                      , expected => R
                      }}
    catch
      _:Error ->
        {error, #{ input => Bin
                 , output => Error
                 , stack => erlang:get_stacktrace()
                 , expected => <<"This isn't what \"Let it crash\" means">>
                 }}
    end
  end.

%%==============================================================================
%% Utils
%%==============================================================================
build_cases() ->
  [ "Is this really that complicated? It should be trivial... Unless someone "
    "went in and added a number, say 1234 in the middle of the phrase."
  , " Once upon a time, there was a boy. He lived in a village that no longer "
    "exists, in a house that no longer exists, on the edge of a field that no "
    "longer exists"
  , "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod "
    "tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim "
    "veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea "
    "commodo consequat"
  , "Programming today is a race between software engineers striving to build "
    "bigger and better idiot-proof programs, and the universe trying to build "
    "bigger and better idiots. So far, the universe is winning."
  , "Computer science education cannot make anybody an expert programmer any "
    "more than studying brushes and pigment can make somebody an expert "
    "painter."
  , "Debugging is twice as hard as writing the code in the first place. "
    "Therefore, if you write the code as cleverly as possible, you are, by "
    "definition, not smart enough to debug it."
  , "Pray, Mr. Babbage, if you put into the machine wrong figures, will the "
    "right answers come out?' I am not able rightly to apprehend the kind of "
    "confusion of ideas that could provoke such a question."
  , "There are two ways of constructing a software design. One way is to make "
    "it so simple that there are obviously no deficiencies. And the other way "
    "is to make it so complicated that there are no obvious deficiencies."
  , "Hofstadter's Law: It always takes longer than you expect, even when you "
    "take into account Hofstadter's Law."
  , "If Java had true garbage collection, most programs would delete themselves"
    " upon execution."
  , "Should array indices start at 0 or 1? My compromise of 0.5 was rejected "
    "without, I thought, proper consideration."
  , "It would be really mean to test how you do the alphabet wouldn't it? too "
    "bad, here it is: a b c d e f g h i j (would a parenthesis complicate "
    "things even more?) k l m n o p q r s t u (and what this: (())) v w x y z."
  ].

solve(Bin) ->
  Words = [binary_to_list(B) || B <- binary:split(Bin, <<" ">>, [global])],
  {_, Result} = lists:foldl(fun(W, {LastUpper, Acc}) ->
                                case {LastUpper, word(W)} of
                                  {true, true}   -> {false, [up(W)| Acc]};
                                  {false, false} -> {false, [W | Acc]};
                                  _Other         -> {true, [W | Acc]}
                                end
                            end, {true, []}, Words),
  list_to_binary(string:join(lists:reverse(Result), " ")).

word([$( | Rest]) ->
  word(Rest);
word([Char | _]) ->
  (Char >= $a andalso Char =< $z) orelse (Char >= $A andalso Char =< $Z);
word(_) ->
  false.

up([Char | Rest]) when Char >= $a, Char =< $z ->
  [Char + $A - $a | Rest];
up(Str) ->
  Str.