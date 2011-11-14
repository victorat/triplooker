-module(pr).
-export([start/0, flight_recorder/1, parse_point/2, parse_flight_process/1, parse_flight/1]).

start_recorder() ->
	Pid = self(),
	register(flight_recorder, spawn(?MODULE, flight_recorder, [Pid])),
	receive ok -> ok end.
	
stop_recorder() ->
	flight_recorder ! stop.

start() ->
	start_recorder(),
	flight("1000"),
	flight("2000"),
	flight("3000"),
	flight("4000"),
	flight("5000"),
	timer:sleep(10000),
	stop_recorder().
	
flight(Flight) ->
	spawn(?MODULE, parse_flight, [{Flight, undefined, undefined}]).

flight_recorder(Pid) ->
	Pid ! ok,
	flight_recorder().
	
flight_recorder() ->
	receive
		stop ->
			io:format("flight_recorder: stop~n");
		{flight, Data} ->
			io:format("flight_recorder: flight ~p~n", [Data]),
			flight_recorder()
	end.

parse_point(Pid, Part) ->
	random:seed(now()),
	Wait = random:uniform(1000),
	timer:sleep(Wait),
	Pid ! {Part, Wait}.
	
parse_flight_process(Flight) ->
	{Info, Origin, Destination} = Flight,
	receive 
		{origin, Data} -> {Info, Data, Destination};
		{destination, Data} -> {Info, Origin, Data}
	end.

parse_flight(Flight) ->
	Pid = self(),
	spawn(?MODULE, parse_point, [Pid, origin]),
	spawn(?MODULE, parse_point, [Pid, destination]),
	Flight1 = parse_flight_process(Flight),
	Flight2 = parse_flight_process(Flight1),
	flight_recorder ! {flight, Flight2}.
