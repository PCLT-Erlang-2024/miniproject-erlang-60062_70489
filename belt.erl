-module(conveyor_belt).

-export([start/2, loop/4]).

%% Starts the conveyor belt process
start(BeltId, GeneratorPid) ->
    PackageQueue = [],
    Stopping = false,
    io:format("Belt ~p: Starting~n", [BeltId]),
    spawn(?MODULE, loop, [BeltId, GeneratorPid, PackageQueue, Stopping]).

%% Main loop of the conveyor belt
loop(BeltId, GeneratorPid, Queue, Stopping) ->
    receive
        %% Truck requests package size
        {request_package_info, TruckPid} ->
            handle_package_info_request(BeltId, TruckPid, Queue, Stopping, GeneratorPid),
            loop(BeltId, GeneratorPid, Queue, Stopping);
        %% Truck requests a package
        {request_package, TruckPid} ->
            {NewQueue, NewStopping} =
                handle_package_request(BeltId, TruckPid, Queue, Stopping, GeneratorPid),
            loop(BeltId, GeneratorPid, NewQueue, NewStopping);
        %% New package arrives from the generator
        {new_package, {PackageId, PackageSize}} ->
            io:format("Belt ~p: Received package ~p with size ~p from Generator~n",
                      [BeltId, PackageId, PackageSize]),
            loop(BeltId, GeneratorPid, [{PackageId, PackageSize} | Queue], Stopping);
        %% Stop signal
        stop ->
            io:format("Belt ~p: Received stop signal. Belt will stop after queue is "
                      "empty~n",
                      [BeltId]),
            loop(BeltId, GeneratorPid, Queue, true);
        {truck_dispatching, false} ->
            io:format("Belt ~p: Received truck_dispatching signal as false~n", [BeltId]),
            loop(BeltId, GeneratorPid, Queue, false);
        {truck_dispatching, true} ->
            io:format("Belt ~p: Received truck_dispatching signal as true~n", [BeltId]),
            loop(BeltId, GeneratorPid, Queue, false);
        %% Unexpected message
        _ ->
            io:format("Belt ~p: Received unexpected message~n", [BeltId]),
            loop(BeltId, GeneratorPid, Queue, Stopping)
    end.

%% Handles package info requests from trucks
handle_package_info_request(BeltId, TruckPid, Queue, Stopping, GeneratorPid) ->
    case {Queue, Stopping} of
        {[], true} ->
            io:format("Belt ~p: Queue empty and stopping. Informing Truck ~p with "
                      "empty_belt~n",
                      [BeltId, TruckPid]),
            TruckPid ! empty_belt;
        {[], _} ->
            io:format("Belt ~p: No packages available, informing Truck ~p~n", [BeltId, TruckPid]),
            TruckPid ! {package_size, 0},
            io:format("Belt ~p: Requesting next package from Generator~n", [BeltId]),
            GeneratorPid ! {generate_package, BeltId, self()};
        {[Package | _], _} ->
            io:format("Package_: ~p~n", [Package]),
            {_, PackageSize} = Package,
            io:format("Belt ~p: Sending package size ~p to Truck ~p~n",
                      [BeltId, PackageSize, TruckPid]),
            TruckPid ! {package_size, PackageSize}
    end.

%% Handles package requests from trucks
handle_package_request(BeltId, TruckPid, Queue, Stopping, GeneratorPid) ->
    case {Queue, Stopping} of
        {[], true} ->
            io:format("Belt ~p: Queue empty. Informing Truck ~p with empty_belt message~n",
                      [BeltId, TruckPid]),
            TruckPid ! empty_belt,
            {Queue, true};  %% Belt remains in stopping mode
        {[], _} ->
            io:format("Belt ~p: No package available, informing Truck ~p~n", [BeltId, TruckPid]),
            TruckPid ! {no_package},
            io:format("Belt ~p: Requesting next package from Generator~n", [BeltId]),
            GeneratorPid ! {generate_package, BeltId, self()},
            {Queue, Stopping};
        {[Package | Rest], true} ->
            {PackageId, PackageSize} = Package,
            io:format("Belt ~p: Sending package ~p with size ~p to Truck ~p~n",
                      [BeltId, PackageId, PackageSize, TruckPid]),
            TruckPid ! {package, {PackageId, PackageSize}},
            {Rest, true};  %% Belt remains in stopping mode
        {[Package | Rest], _} ->
            {PackageId, PackageSize} = Package,
            io:format("Belt ~p: Sending package ~p with size ~p to Truck ~p~n",
                      [BeltId, PackageId, PackageSize, TruckPid]),
            TruckPid ! {package, {PackageId, PackageSize}},
            io:format("Belt ~p: Requesting next package from Generator~n", [BeltId]),
            GeneratorPid ! {generate_package, BeltId, self()},
            {Rest, Stopping}
    end.
