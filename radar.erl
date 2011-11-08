-module(radar).
-export([list/0, flight_statistics_append/3]).

-define(JSSeconds, 62167219200).
-define(DataExtract, "'([A-Z]+[0-9]+)':\\['([0-9A-F]+)',[0-9\\-\\.]*,[0-9\\-\\.]*,'\\d*','([\\-\\d]*)','\\d*','\\d*','\\w*','\\w*','[\\w\\-\\+]*',([0-9]*)\\]").
-define(Search, "http://www.flightradar24.com/PlaneFeed.json").

date_string(Date) ->
	{YYYY, MM, DD} = Date,
	integer_to_list(YYYY) ++ "-" ++ integer_to_list(MM) ++ "-" ++ integer_to_list(DD).

file_name(Flight, Date) ->
	"test/" ++ binary_to_list(Flight) ++ "-" ++ date_string(Date) ++ ".txt".

flight_statistics_create(Flight, Date) ->
	{ok, File} = file:open(file_name(Flight, Date), [write]),
	io:fwrite(File, "Flight = ~p~nDate = ~p~n", [Flight, Date]),
	file:close(File),
	fsparser:flight(binary_to_list(Flight), date_string(Date)).
	
flight_statistics_append(Flight, Date, Data) ->
	{ok, File} = file:open(file_name(Flight, Date), [write, append]),
	io:fwrite(File, "~p~n", [Data]),
	file:close(File).

process_flight([]) -> ok;

process_flight([[Flight, Hex, Altitude, Seconds] | Tail]) ->
	{Date, {HH, MM, SS}} = calendar:gregorian_seconds_to_datetime(list_to_integer(binary_to_list(Seconds)) + ?JSSeconds),
	io:format("Flight = ~p, Hex = ~p, Altitude = ~p, DateTime = ~p:~p:~p~n", [Flight, Hex, Altitude, HH, MM, SS]),
	flight_statistics_create(Flight, Date),
	process_flight(Tail).

list() ->
	inets:start(),
	{ok, {_Status, _Header, HTML}} = httpc:request(?Search),
    % {ok, Binary} = file:read_file(File),
 	% {match, Flights} = re:run(Binary, "\\x22([A-Z]+[0-9]+)\\x22:\\[\\x22([0-9A-F]+)\\x22", [global, {capture, [1,2], binary}]),
	{match, Flights} = re:run(re:replace(HTML, "\\x22", "'", [global, {return, binary}]), ?DataExtract, [global, {capture, [1,2,3,4], binary}]),
	process_flight(Flights).
