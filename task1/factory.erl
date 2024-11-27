-module(factory).
-export([start/0]).

start() ->
    % Start trucks
    Truck1 = spawn(truck, start, []),
    Truck2 = spawn(truck, start, []),
    log_truck(Truck1),
    log_truck(Truck2),

    Trucks = [Truck1, Truck2],

    % Start conveyor belts
    Conveyor1 = spawn(conveyor_belt, start, [Trucks]),
    Conveyor2 = spawn(conveyor_belt, start, [Trucks]),
    log_conveyor(Conveyor1),
    log_conveyor(Conveyor2),
    
    % Continuously generate packages
    generate_packages([Conveyor1, Conveyor2], 1).

%Logging functions
log_truck(Truck) -> io:format("Factory: Truck ~p was created~n", [Truck]).
log_conveyor(Conveyor) -> io:format("Factory: Conveyor Belt ~p was created~n", [Conveyor]).

% Simulate package generation
generate_packages(Conveyors, PackageCounter) ->
    receive
        after 1000 -> % Generates a package per conveyor every second
            NewPackageCounter = send_packages_to_conveyors(Conveyors, PackageCounter),
            generate_packages(Conveyors, NewPackageCounter)
    end.

send_packages_to_conveyors([], PackageId) -> PackageId;
send_packages_to_conveyors([Conveyor|OtherConveyors], PackageId) ->
    Conveyor ! PackageId,
    io:format("Factory: Sent Package ~p to Conveyor Belt ~p~n", [PackageId, Conveyor]),
    send_packages_to_conveyors(OtherConveyors, PackageId + 1).