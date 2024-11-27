-module(truck).
-export([start/0]).

start() -> process_packages(0).

%Need to make sure that only receives the package if the truck is not full (somehow with pattern maybe?)
process_packages(Count) ->
    receive
        {PackageId, Conveyor} ->
            NewCount = Count + 1,
            io:format("Truck ~p: received Package ~p from Conveyor Belt ~p~n", [self(), PackageId, Conveyor]),
            if
                NewCount < 10 -> % truck capacity is 10, when it receives the 10th package it resets because this expression gets false
                    process_packages(NewCount);
                true ->
                    io:format("Truck ~p: full, reseting...~n", [self()]),
                    process_packages(0)
            end
    end.