-module(bo_entangled).

-behaviour(bo_task).

-export([description/0, spec/0, score/0, timeout/0, tests/0]).

%%==============================================================================
%% API
%%==============================================================================
-spec description() -> binary().
description() -> <<"Entangled: We figured out how to transmit erlang data "
                   "faster than light! It's true that we don't know much about "
                   "Quantum Mechanics or any of that, but we are sure that if "
                   "we entangle two processes, we could use them to sync our "
                   "servers here with our planned Mars database (taxes are "
                   "really low on Mars). You need to provide us with 2 "
                   "entangled gen_servers with opposite spins ('up' and 'down' "
                   "atoms). Provide 2 call handlers, one to return the spin "
                   "(the atom 'get_spin') and another to change it (the atom "
                   "'invert_spin'). The gen_servers should always have opposite"
                   " spins.">>.

-spec spec() -> bo_task:spec().
spec() -> #{input => [], output => <<"{pid(), pid()}">>}.

-spec score() -> 450.
score() -> 450.

-spec timeout() -> 5000.
timeout() -> 5000.

-spec tests() -> [bo_task:test()].
tests() -> [build_test()].

build_test() ->
  fun(Fun) ->
    try Fun() of
      {Pid1, Pid2} ->
        try check(Pid1, Pid2) of
          ok -> ok
        catch
          throw:E1 ->
            {error, #{ error => E1 }};
          _:E2 ->
            {error, #{ error => E2
                     , stack => erlang:get_stacktrace()
                     , expected => <<"The gen_servers should not crash.">>
                     }}
        end;
      Bad ->
        {error, #{ input => []
                 , output => Bad
                 , expected => <<"{pid(), pid()}">>
                 }}
    catch
      _:Error ->
        {error, #{ input => []
                 , error => Error
                 , stack => erlang:get_stacktrace()
                 , expected => <<"Though I'm sure there's a parallel universe "
                                 "where it didn't crash, it is required to work"
                                 " reliably on this one.">>
                 }}
    end
  end.

%%==============================================================================
%% Utils
%%==============================================================================
check(Pid1, Pid2) ->
  S1 = get_spin(Pid1),
  S2 = get_spin(Pid2),

  % Check the spin values are valid
  _ = case valid_spin(S1) of
        true  -> ok;
        false -> throw(list_to_binary(io_lib:format("the spin ~p is not valid",
                                                    [S1])))
      end,
  _ = case valid_spin(S2) of
        true  -> ok;
        false -> throw(list_to_binary(io_lib:format("the spin ~p is not valid",
                                                    [S2])))
      end,

  % Check the spins are different
  _ = case S1 of
        S2 -> throw(<<"the gen_servers have the same spin!">>);
        S1 -> ok
      end,

  % Invert the spins
  _ = invert_spin(Pid1),
  % The spins should be inverted
  _ = case {get_spin(Pid1), get_spin(Pid2)}of
        {S2, S1} -> ok;
        _        -> throw(<<"The gen_servers did not change spins correctly">>)
      end,

  % Invert the spins again
  _ = invert_spin(Pid2),
  % The spins should be inverted
  _ = case {get_spin(Pid1), get_spin(Pid2)}of
        {S1, S2} -> ok;
        _        -> throw(<<"The gen_servers did not change spins correctly">>)
      end,

  % Do it lots of times in a row
  _ = invert_spin(Pid2), % inverted
  _ = invert_spin(Pid2), % reverted
  _ = invert_spin(Pid1), % inverted
  _ = invert_spin(Pid2), % reverted
  _ = invert_spin(Pid1), % inverted
  _ = invert_spin(Pid1), % reverted
  _ = invert_spin(Pid2), % inverted
  _ = invert_spin(Pid2), % reverted
  _ = invert_spin(Pid1), % inverted
  _ = invert_spin(Pid2), % reverted

  % The spins should not have changed
  _ = case {get_spin(Pid1), get_spin(Pid2)}of
        {S1, S2} -> ok;
        _        -> throw(<<"The gen_servers did not change spins correctly">>)
      end,

  % If nothing failed, then return ok
  ok.

get_spin(Pid)    -> gen_server:call(Pid, get_spin).
invert_spin(Pid) -> gen_server:call(Pid, invert_spin).

valid_spin(up)   -> true;
valid_spin(down) -> true;
valid_spin(_)    -> false.