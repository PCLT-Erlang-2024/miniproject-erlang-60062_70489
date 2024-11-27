-module(main).

-export([start/0, stop/0]).

start() ->
    SystemSize = 3,
    MainPid = self(), %% Store the main process PID

    GeneratorPid = package_generator:start(),
    Belts = [conveyor_belt:start(I, GeneratorPid, MainPid) || I <- lists:seq(1, SystemSize)],
    Trucks =
        [truck:start(I, MainPid, SystemSize, lists:nth(I, Belts))
         || I <- lists:seq(1, SystemSize)],

    io:format("System initialized~n"),

    %% Store counters and PIDs in the process dictionary for later access by stop/0
    put(underlying_trucks, Trucks),
    put(underlying_belts, Belts),
    put(underlying_generator, GeneratorPid),
    put(truck_shutdown_counter, length(Trucks)),
    put(belt_shutdown_counter, length(Belts)),

    io:format("System running~n").

%% stop/0 will be called to shut down the system
stop() ->
    %% Retrieve the stored trucks and belts from the process dictionary
    GeneratorPid = get(underlying_generator),
    TruckCounter = get(truck_shutdown_counter),
    BeltCounter = get(belt_shutdown_counter),

    %% Send stop signal to the generator
    GeneratorPid ! stop,

    %% Send stop signal to each truck
    Trucks = get(underlying_trucks),
    lists:foreach(fun(Truck) -> Truck ! stop end, Trucks),

    %% Send stop signal to each belt
    Belts = get(underlying_belts),
    lists:foreach(fun(Belt) -> Belt ! stop end, Belts),

    %% Wait for all trucks and belts to finish stopping
    io:format("Waiting for shutdowns~n"),
    wait_for_shutdown(TruckCounter, BeltCounter),

    io:format("System stopped~n").

%% Wait for all trucks and belts to stop before fully shutting down
wait_for_shutdown(0, 0) ->
    io:format("All trucks and belts have stopped~n");
wait_for_shutdown(TruckCounter, BeltCounter) ->
    io:format("Receiving shutdowns~n"),
    receive
        {truck_stopped, TruckId} ->
            io:format("Truck ~p: has stopped. Remaining trucks: ~p~n", [TruckId, TruckCounter - 1]),
            wait_for_shutdown(TruckCounter - 1, BeltCounter);
        {belt_stopped, BeltId} ->
            io:format("Belt ~p: has stopped. Remaining belts: ~p~n", [BeltId, BeltCounter - 1]),
            wait_for_shutdown(TruckCounter, BeltCounter - 1)
    end.
