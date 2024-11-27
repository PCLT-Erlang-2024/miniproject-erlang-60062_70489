-module(truck).

-export([start/4, loop/7]).

%% Starting the truck
start(TruckId, MainPid, NumTrucks, BeltPid) ->
    MaxCapacity = 20,  %% Default MaxCapacity
    CurrentLoad = 0,  %% Initial load of 0
    BeltEmpty = false, %% Initial state of the belt
    io:format("Truck ~p: Starting with max capacity ~p~n", [TruckId, MaxCapacity]),
    BeltPid ! {truck_dispatching, false},
    spawn(?MODULE,
          loop,
          [TruckId, MainPid, NumTrucks, BeltPid, CurrentLoad, MaxCapacity, BeltEmpty]).

%% Main loop of the truck
loop(TruckId, MainPid, NumTrucks, BeltPid, CurrentLoad, MaxCapacity, BeltEmpty) ->
    io:format("Truck ~p: Requesting Package~n", [TruckId]),
    BeltPid ! {request_package, self()},
    receive
        %% Empty belt signal received
        empty_belt ->
            io:format("Truck ~p: Received empty belt signal. Dispatching load~n", [TruckId]),
            io:format("Truck ~p: Stopping~n", [TruckId]),
            dispatch(TruckId, CurrentLoad, MainPid);
        %% No packages available, retry after a short delay
        no_package ->
            io:format("Truck ~p: No packages available. Retrying~n", [TruckId]),
            timer:sleep(500), %% Simulate waiting before retry
            loop(TruckId, MainPid, NumTrucks, BeltPid, CurrentLoad, MaxCapacity, BeltEmpty);
        %% Package received
        {package, {PackageId, PackageSize}} ->
            if PackageSize + CurrentLoad > MaxCapacity ->
                   io:format("Truck ~p: Load[~p/~p] Not enough capacity for package ~p (size "
                             "~p). Returning to Belt~n",
                             [TruckId, CurrentLoad, MaxCapacity, PackageId, PackageSize]),
                   BeltPid ! {return_package, {PackageId, PackageSize}},
                   dispatch(TruckId, CurrentLoad, MainPid, NumTrucks, BeltPid);
               true ->
                   NewLoad = CurrentLoad + PackageSize,
                   io:format("Truck ~p: Loaded package ~p (size ~p). New Load: ~p / ~p~n",
                             [TruckId, PackageId, PackageSize, NewLoad, MaxCapacity]),
                   timer:sleep(1000), %% Simulate loading delay
                   loop(TruckId, MainPid, NumTrucks, BeltPid, NewLoad, MaxCapacity, BeltEmpty)
            end
    end.

%% Dispatch logic
dispatch(TruckId, CurrentLoad, MainPid) ->
    io:format("Truck ~p: Final dispatching load ~p~n", [TruckId, CurrentLoad]),
    io:format("Truck ~p: Stopped~n", [TruckId]),
    MainPid ! {truck_stopped, TruckId}. %% Notify main process.

dispatch(TruckId, CurrentLoad, MainPid, NumTrucks, BeltPid) ->
    BeltPid ! {truck_dispatching, true},
    io:format("Truck ~p: Dispatching load ~p~n", [TruckId, CurrentLoad]),
    NewTruckId = TruckId + NumTrucks,
    spawn(?MODULE, start, [NewTruckId, MainPid, NumTrucks, BeltPid]). %% Pass MainPid
