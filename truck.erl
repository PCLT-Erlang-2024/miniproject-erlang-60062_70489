-module(truck).

-export([start/3, loop/6]).

%% Starting the truck
start(TruckId, NumTrucks, BeltPid) ->
    MaxCapacity = 20,  %% Default MaxCapacity
    CurrentLoad = 0,  %% Initial load of 0
    BeltEmpty = false, %% Initial state of the belt
    io:format("Truck ~p: Starting with max capacity ~p~n", [TruckId, MaxCapacity]),
    BeltPid ! {truck_dispatching, false},
    spawn(?MODULE, loop, [TruckId, NumTrucks, BeltPid, CurrentLoad, MaxCapacity, BeltEmpty]).

%% Main loop of the truck
loop(TruckId, NumTrucks, BeltPid, CurrentLoad, MaxCapacity, BeltEmpty) ->
    BeltPid ! {request_package_info, self()},
    receive
        %% Empty belt signal received
        empty_belt ->
            io:format("Truck ~p: Received empty belt signal. Dispatching load~n", [TruckId]),
            io:format("Truck ~p: Stopping~n", [TruckId]),
            dispatch(TruckId, CurrentLoad);
        %% Package size info received
        {package_size, PackageSize} when PackageSize > 0 ->
            io:format("Truck ~p: Package size ~p received~n", [TruckId, PackageSize]),
            %% Check if we exceed capacity
            if PackageSize + CurrentLoad > MaxCapacity ->
                   io:format("Truck ~p: Max capacity reached. Dispatching load~n", [TruckId]),
                   dispatch(TruckId, CurrentLoad, NumTrucks, BeltPid);
               true ->
                   BeltPid ! {request_package, self()},
                   loop(TruckId, NumTrucks, BeltPid, CurrentLoad, MaxCapacity, BeltEmpty)
            end;
        %% No packages available, retry after a short delay
        {package_size, 0} ->
            io:format("Truck ~p: No packages available. Retrying~n", [TruckId]),
            timer:sleep(500), %% Simulate waiting before retry
            loop(TruckId, NumTrucks, BeltPid, CurrentLoad, MaxCapacity, BeltEmpty);
        %% Package received
        {package, {PackageId, PackageSize}} ->
            io:format("Truck ~p: Loaded package ~p (size ~p)~n", [TruckId, PackageId, PackageSize]),
            timer:sleep(1000), %% Simulate loading delay
            loop(TruckId, NumTrucks, BeltPid, CurrentLoad + PackageSize, MaxCapacity, BeltEmpty)
    end.

%% Dispatch logic
dispatch(TruckId, CurrentLoad) ->
    io:format("Truck ~p: Final dispatching load ~p~n", [TruckId, CurrentLoad]),
    %% Simulate the final truck dispatching its load
    io:format("Truck ~p: Stopped~n", [TruckId]).

dispatch(TruckId, CurrentLoad, NumTrucks, BeltPid) ->
    BeltPid ! {truck_dispatching, true},
    io:format("Truck ~p: Dispatching load ~p~n",
              [TruckId, CurrentLoad]), %% Simulate the truck dispatching its load
    NewTruckId = TruckId + NumTrucks, %% Calculate new truck ID for replacement
    spawn(?MODULE, start, [NewTruckId, NumTrucks, BeltPid]).
