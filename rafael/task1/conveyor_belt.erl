-module(conveyor_belt).

-export([start/3, loop/5]).

%% Starts the conveyor belt process
start(BeltId, GeneratorPid, MainPid) ->
    PackageQueue = [],
    Stopping = false,
    io:format("Belt ~p: Starting~n", [BeltId]),
    spawn(?MODULE, loop, [BeltId, GeneratorPid, MainPid, PackageQueue, Stopping]).

%% Main loop of the conveyor belt
loop(BeltId, GeneratorPid, MainPid, Queue, Stopping) ->
    receive
        {request_package, TruckPid} ->
            {NewQueue, NewStopping} =
                handle_package_request(BeltId, TruckPid, Queue, Stopping, GeneratorPid, MainPid),
            loop(BeltId, GeneratorPid, MainPid, NewQueue, NewStopping);
        %% New package arrives from the generator
        {new_package, {PackageId, PackageSize}} ->
            io:format("Belt ~p: Received package ~p with size ~p from Generator~n",
                      [BeltId, PackageId, PackageSize]),
            loop(BeltId, GeneratorPid, MainPid, [{PackageId, PackageSize} | Queue], Stopping);
        %% Stop signal
        stop ->
            io:format("Belt ~p: Received stop signal. Belt will stop after queue is "
                      "empty~n",
                      [BeltId]),
            loop(BeltId, GeneratorPid, MainPid, Queue, true);
        {truck_dispatching, false} ->
            io:format("Belt ~p: Received truck_dispatching signal as false~n", [BeltId]),
            loop(BeltId, GeneratorPid, MainPid, Queue, Stopping);
        {truck_dispatching, true} ->
            io:format("Belt ~p: Received truck_dispatching signal as true~n", [BeltId]),
            loop(BeltId, GeneratorPid, MainPid, Queue, Stopping);
        %% Unexpected message
        _ ->
            io:format("Belt ~p: Received unexpected message~n", [BeltId]),
            loop(BeltId, GeneratorPid, MainPid, Queue, Stopping)
    end.

%% Handles package requests from trucks
handle_package_request(BeltId, TruckPid, Queue, Stopping, GeneratorPid, MainPid) ->
    case {Queue, Stopping} of
        {[], true} ->
            io:format("Belt ~p: Queue empty. Informing Truck ~p with empty_belt message~n",
                      [BeltId, TruckPid]),
            TruckPid ! empty_belt,
            MainPid ! {belt_stopped, BeltId},
            {Queue, true};  %% Belt remains in stopping mode
        {[], false} ->
            io:format("Belt ~p: No package available, informing Truck ~p~n", [BeltId, TruckPid]),
            TruckPid ! no_package,
            io:format("Belt ~p: Requesting next package from Generator~n", [BeltId]),
            GeneratorPid ! {generate_package, BeltId, self()},
            {Queue, Stopping};
        {[Package | Rest], true} ->
            {PackageId, PackageSize} = Package,
            io:format("Belt ~p: Sending package ~p with size ~p to Truck ~p~n",
                      [BeltId, PackageId, PackageSize, TruckPid]),
            TruckPid ! {package, {PackageId, PackageSize}},
            {Rest, true};  %% Belt remains in stopping mode
        {[Package | Rest], false} ->
            {PackageId, PackageSize} = Package,
            io:format("Belt ~p: Sending package ~p with size ~p to Truck ~p~n",
                      [BeltId, PackageId, PackageSize, TruckPid]),
            TruckPid ! {package, {PackageId, PackageSize}},
            io:format("Belt ~p: Requesting next package from Generator~n", [BeltId]),
            GeneratorPid ! {generate_package, BeltId, self()},
            {Rest, Stopping}
    end.
