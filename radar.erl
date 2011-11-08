-module(radar).
-export([list/0]).

-define(JSSeconds, 62167219200).
-define(DataExtract, "'([A-Z]+[0-9]+)':\\['([0-9A-F]+)',[0-9\\-\\.]*,[0-9\\-\\.]*,'\\d*','([\\-\\d]*)','\\d*','\\d*','\\w*','\\w*','[\\w\\-\\+]*',([0-9]*)\\]").
-define(Search, "http://www.flightradar24.com/PlaneFeed.json").

%% -define(Search, "'(\\w*)':\\[").

process_flight([]) -> ok;

process_flight([[Flight, Hex, Altitude, Seconds] | Tail]) ->
	{Date, {HH,MM,SS}} = calendar:gregorian_seconds_to_datetime(list_to_integer(binary_to_list(Seconds)) + ?JSSeconds),
	io:format("Flight = ~p, Hex = ~p, Altitude = ~p, DateTime = ~p:~p:~p~n", [Flight, Hex, Altitude, HH, MM, SS]),
	process_flight(Tail).

list() ->
	inets:start(),
	{ok, {_Status, _Header, HTML}} = httpc:request(?Search),
    % {ok, Binary} = file:read_file(File),
 	% {match, Flights} = re:run(Binary, "\\x22([A-Z]+[0-9]+)\\x22:\\[\\x22([0-9A-F]+)\\x22", [global, {capture, [1,2], binary}]),
	{match, Flights} = re:run(re:replace(HTML, "\\x22", "'", [global, {return, binary}]), ?DataExtract, [global, {capture, [1,2,3,4], binary}]),
	process_flight(Flights).
