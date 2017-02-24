-module(bo_fun_with_numbers).

-behaviour(bo_task).

-export([description/0, spec/0, score/0, timeout/0, tests/0]).

%%==============================================================================
%% API
%%==============================================================================
-spec description() -> binary().
description() -> <<"Fun with numbers: Write a function that receives a string "
                   "with some numbers (like \"today at 12 we will gather our "
                   "300 men and defeat the Persians\") and returns a list of "
                   "the numbers in the string in the order they appear (in this"
                   " case, [12, 300]). Remember you are not writing a validator"
                   ", you can assume the strings make sense.">>.

-spec spec() -> bo_task:spec().
spec() -> #{input => [<<"binary()">>], output => <<"[integer() | float()]">>}.

-spec score() -> 200.
score() -> 200.

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
                 , expected => <<"Don't \"Let it crash\" this time">>
                 }}
    end
  end.

%%==============================================================================
%% Utils
%%==============================================================================
build_cases() ->
  [ "Intel continues to shrink its manufacturing technology: 45 nm with "
    "high-k/metal gate in 2007; 32 nm in 2009; and now 22 nm with the world's "
    "first 3D transistor in a high volume logic process beginning in 2011."
  , "Jamaican sprinter Usain Bolt is arguably the fastest man in the world, "
    "winning three gold medals at the 2008 Olympic Games in Beijing, China, and"
    " becoming the first man in Olympic history to win both the 100-meter and "
    "200-meter races in record times. Bolt also won three Olympic gold medals "
    "at the 2012 Summer Olympic Games in London. He ran the men's 100-meter "
    "race in 9.63 seconds, a new Olympic record, making him the first man in "
    "history to set three world records in Olympic competition. He made history"
    " again at the 2016 Summer Games in Rio when he won gold in the 100-meter "
    "and 200-meter race and 4x100-meter relay, completing a \"triple-triple,\" "
    "earning three gold medals at three consecutive Olympics for a total of 9 "
    "gold medals over the course of his Olympic career. [...] In addition to "
    "breaking barriers on the track, Bolt has made history off the track in "
    "recent years as the highest-paid athlete in the history of the sport. His "
    "$32.5 million in earnings over the last 12 months from appearances, prize "
    "money and sponsors is roughly 10 times what other track and field stars "
    "bank."
% , "Besides parsing those strings, there are other (more complex) examples "
%   "that would make anyone's life more difficult. For example, what if I tell "
%   "you that .1% of the speed of light (299,792,458 meters per seconds) is "
%   "1.079e+8? How would you even parse that?"
  , "Choosing what to do with periods is also complicated, for example I am "
    "going to finish a sentence with 93. Was that a too easy? Then I'm going to"
    " end this sentence with 32.1."
% , "Also, I see you got around the speed of light being 299,792,458 mps, but "
%   "299, 792 and 458 are not the same thing."
  , "And finally, the last test, this should be the one of the easier ones, can"
    "you handle something like 1.079e+8? That's 0.1% of the speed of light in "
    "case you are wondering. And as a last challenge, I'm finishing with a "
    "number: 123456"
  ].

% Solve accepts invalid numbers! It's not a validator and doesn't have to be.
solve(Bin) ->
  lists:reverse(solve(binary_to_list(Bin), empty_buffer(), [])).

solve([H | T], Buffer, Acc) ->
  case add_to_buffer(H, Buffer) of
    ignore ->
      solve(T, Buffer, Acc);
    discard ->
      solve(T, empty_buffer(), Acc);
    {changed, NewBuffer} ->
      solve(T, NewBuffer, Acc);
    {finished, FinalBuffer} ->
      solve(T, empty_buffer(), [parse_buffer(FinalBuffer) | Acc])
  end;
solve([], ".", Acc) ->
  Acc;
solve([], Buffer = [_ | _], Acc) ->
  [parse_buffer(Buffer) | Acc];
solve([], _EmptyBuffer, Acc) ->
  Acc.

empty_buffer() ->
  "".

% A comma can be ignored
add_to_buffer($,, "") ->
  ignore;
% Unless it's preceded by 3 numbers
add_to_buffer($,, [C1, C2, C3 | _] = Buffer) when C1 >= $0 andalso 
                                                  C1 =< $9 andalso
                                                  C2 >= $0 andalso
                                                  C2 =< $9 andalso
                                                  C3 >= $0 andalso
                                                  C3 =< $9 ->
  {changed, [$, | Buffer]};
% An e is used on scientific notation
add_to_buffer($e, [N | _] = Buffer) when N >= $0 andalso N =< $9 ->
  case lists:member($e, Buffer) of
    false -> {changed, [$e | Buffer]};
    true  -> {finished, Buffer}
  end;
% A + or a - sign preceded by an e are also part of the scientific notation
add_to_buffer($+, [$e | _] = Buffer) ->
  {changed, [$+ | Buffer]};
add_to_buffer($-, [$e | _] = Buffer) ->
  {changed, [$- | Buffer]};
% The basic case, adding a number to the buffer
add_to_buffer(N, Buffer) when N >= $0 andalso N =< $9 ->
  {changed, [N | Buffer]};
% Adding a point can only be done if there are no points on the number already
add_to_buffer($., ".") ->
  discard;
add_to_buffer($., Buffer) ->
  case lists:member($., Buffer) of
    false -> {changed, [$. | Buffer]};
    true  -> {finished, Buffer}
  end;
add_to_buffer(_, "") ->
  ignore;
add_to_buffer(_, ".") ->
  discard;
add_to_buffer(_, Buffer) ->
  {finished, Buffer}.

parse_buffer(Str) ->
  Str2 = lists:reverse(trim_str(Str)),
  case lists:member($., Str2) of
    true  -> case Str2 of
               [$. | _] -> list_to_float([$0 | Str2]);
               _        -> list_to_float(Str2)
             end;
    false -> list_to_integer(Str2)
  end.

trim_str([$. | Str]) ->
  trim_str(Str);
trim_str(Str) ->
  lists:filter(fun(C) -> C /= $, end, Str).