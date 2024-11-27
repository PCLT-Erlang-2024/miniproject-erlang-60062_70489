-module(conveyor_belt).

-export([start/3, loop/5]).

start(BeltId, GeneratorPid, MainPid) ->
    PackageQueue = [],
    Stopping = false,
    io:format("Belt ~p: Starting~n", [BeltId]),
    spawn(?MODULE, loop, [BeltId, GeneratorPid, MainPid, PackageQueue, Stopping]).

loop(BeltId, GeneratorPid, MainPid, Queue, Stopping) ->
    receive
        {request_package, TruckPid} ->
            {NewQueue, NewStopping} =
                handle_package_request(BeltId, TruckPid, Queue, Stopping, GeneratorPid, MainPid),
            loop(BeltId, GeneratorPid, MainPid, NewQueue, NewStopping);
        {new_package, {PackageId, PackageSize}} ->
            io:format("Belt ~p: Received package ~p (size ~p)~n", [BeltId, PackageId, PackageSize]),
            loop(BeltId, GeneratorPid, MainPid, [{PackageId, PackageSize} | Queue], Stopping);
        stop ->
            io:format("Belt ~p: Stopping after processing all packages~n", [BeltId]),
            loop(BeltId, GeneratorPid, MainPid, Queue, true);
        {return_package, {PackageId, PackageSize}} ->
            io:format("Belt ~p: Returned package ~p (size ~p) to the queue~n",
                      [BeltId, PackageId, PackageSize]),
            loop(BeltId, GeneratorPid, MainPid, [{PackageId, PackageSize} | Queue], Stopping);
        {truck_dispatching, false} ->
            io:format("Belt ~p: Received truck_dispatching signal as false~n", [BeltId]),
            loop(BeltId, GeneratorPid, MainPid, Queue, Stopping);
        {truck_dispatching, true} ->
            io:format("Belt ~p: Received truck_dispatching signal as true~n", [BeltId]),
            loop(BeltId, GeneratorPid, MainPid, Queue, Stopping);
        _ ->
            io:format("Belt ~p: Received unexpected message~n", [BeltId]),
            loop(BeltId, GeneratorPid, MainPid, Queue, Stopping)
    end.

handle_package_request(BeltId, TruckPid, Queue, Stopping, GeneratorPid, MainPid) ->
    case {Queue, Stopping} of
        {[], true} ->
            io:format("Belt ~p: No more packages, informing Truck ~p~n", [BeltId, TruckPid]),
            TruckPid ! empty_belt,
            MainPid ! {belt_stopped, BeltId},
            {Queue, true};
        {[], false} ->
            io:format("Belt ~p: Queue empty, informing Truck ~p and requesting a new "
                      "package~n",
                      [BeltId, TruckPid]),
            TruckPid ! no_package,
            GeneratorPid ! {generate_package, BeltId, self()},
            {Queue, Stopping};
        {[Package | Rest], _} ->
            {PackageId, PackageSize} = Package,
            io:format("Belt ~p: Sending package ~p (size ~p) to Truck ~p~n",
                      [BeltId, PackageId, PackageSize, TruckPid]),
            TruckPid ! {package, {PackageId, PackageSize}},
            {Rest, Stopping}
    end.
