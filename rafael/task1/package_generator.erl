-module(package_generator).

-export([start/0, loop/2]).

start() ->
    spawn(?MODULE, loop, [self(), 1]).

loop(GeneratorPid, Counter) ->
    receive
        stop ->
            io:format("Generator: Stopping ~n");
        {generate_package, BeltId, BeltPid} ->
            io:format("Generator: Creating package ~p for Belt~p ~p~n", [Counter, BeltId, BeltPid]),
            BeltPid ! {new_package, {Counter, 5}},
            loop(GeneratorPid, Counter + 1)
    end.
