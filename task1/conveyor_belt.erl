-module(conveyor_belt).
-export([start/1]).

start(Trucks) ->
    receive
        PackageId ->
            % Simulate passing the package to a truck
            io:format("Conveyor Belt ~p: Package ~p received from factory~n", [self(), PackageId]),
            dispatch_package(PackageId, Trucks),
            start(Trucks)
    end.

dispatch_package(PackageId, Trucks) ->
    % Simulate passing the package to a truck
    Truck = get_truck(Trucks),
    Truck ! {PackageId, self()},
    io:format("Conveyor Belt ~p: Package ~p dispatched to Truck ~p~n", [self(), PackageId, Truck]).

%Only gets the first truck   TODO!!!!!!
get_truck([Truck | _]) ->
    Truck.
