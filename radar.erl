-module(radar).
-export([list/1]).

-define(JSSeconds, 62167219200).
-define(Search, "'(\\w*)':\\['(\\w*)',[0-9\\-\\.]*,[0-9\\-\\.]*,'\\d*','([\\-\\d]*)','\\d*','\\d*','\\w*','\\w*','[\\w\\-\\+]*',([0-9]*)\\]").
%% -define(Search, "'(\\w*)':\\[").

process_flight([]) -> ok;

process_flight([[Flight, Hex, _, _] | Tail]) ->
	io:format("Flight = ~p, Hex = ~p~n", [Flight, Hex]),
	process_flight(Tail).

list(File) -> 
	{ok, Binary} = file:read_file(File),
	% {match, Flights} = re:run(Binary, "\\x22([A-Z]+[0-9]+)\\x22:\\[\\x22([0-9A-F]+)\\x22", [global, {capture, [1,2], binary}]),
	{match, Flights} = re:run(re:replace(Binary, "\\x22", "'", [global, {return, binary}]), ?Search, [global, {capture, [1,2,3,4], binary}]),
	process_flight(Flights).
