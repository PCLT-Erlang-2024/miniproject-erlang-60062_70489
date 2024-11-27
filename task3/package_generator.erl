-module(package_generator).

-export([start/0, loop/2]).

start() ->
    spawn(?MODULE, loop, [self(), 1]).

loop(GeneratorPid, Counter) ->
    receive
        stop ->
            io:format("Generator: Stopping~n");
        {generate_package, BeltId, BeltPid} ->
            %% Generate random size for the package
            PackageSize = rand:uniform(10), %% Random size from 1 to 10
            io:format("Generator: Creating package ~p (size ~p) for Belt ~p~n",
                      [Counter, PackageSize, BeltId]),
            BeltPid ! {new_package, {Counter, PackageSize}},
            loop(GeneratorPid, Counter + 1)
    end.
