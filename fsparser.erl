%% ---------------------------------------------------------------------------------------------------------------------------------------------------

-module(fsparser).
-export([airport/1, flight/2, universal_seconds/1]).

%% ---------------------------------------------------------------------------------------------------------------------------------------------------

-define(Search, "http://mobile.flightstats.com/go/Mobile/airportDetails.do?airportCode=").
-define(Months, [{"Jan",1}, {"Feb",2}, {"Mar",3}, {"Apr",4}, {"May",5}, {"Jun",6}, {"Jul",7}, {"Aug",8}, {"Sep",9}, {"Oct",10}, {"Nov",11}, {"Dec",12}]).
-define(Flight, "http://mobile.flightstats.com/go/Mobile/flightStatusByFlight.do?").

%% ---------------------------------------------------------------------------------------------------------------------------------------------------

-record(airport, {iata, name, address, city, state, country, time, date, zone}).
-record(flight, {flight, name, date, status, origin, departure, destination, arrival}).
-record(schedule, {scheduled, estimated, actual, terminal}).

%% ---------------------------------------------------------------------------------------------------------------------------------------------------

parse_flight_value(Schedule, []) -> Schedule;

parse_flight_value(Schedule, [<<"Scheduled:">> | Tail]) ->
	[Value | Tail2] = Tail, 
	{match, [HH, MM]} = re:run(Value, "([0-9][0-9]):([0-9][0-9])", [{capture, [1,2], list}]),
	parse_flight_value(Schedule#schedule{scheduled = {list_to_integer(HH), list_to_integer(MM), 0}}, Tail2);
	
parse_flight_value(Schedule, [<<"Estimated:">> | Tail]) ->
	[Value | Tail2] = Tail, 
	{match, [HH, MM]} = re:run(Value, "([0-9][0-9]):([0-9][0-9])", [{capture, [1,2], list}]),
	parse_flight_value(Schedule#schedule{estimated = {list_to_integer(HH), list_to_integer(MM), 0}}, Tail2);

parse_flight_value(Schedule, [<<"Actual:">> | Tail]) ->
	[Value | Tail2] = Tail, 
	{match, [HH, MM]} = re:run(Value, "([0-9][0-9]):([0-9][0-9])", [{capture, [1,2], list}]),
	parse_flight_value(Schedule#schedule{actual = {list_to_integer(HH), list_to_integer(MM), 0}}, Tail2);

parse_flight_value(Schedule, [<<"Gate:">> | Tail]) ->
	[Value | Tail2] = Tail, 
	parse_flight_value(Schedule#schedule{terminal = Value}, Tail2);

parse_flight_value(Schedule, [<<"Terminal:">> | Tail]) ->
	[Value | Tail2] = Tail, 
	parse_flight_value(Schedule#schedule{terminal = Value}, Tail2);

parse_flight_value(Schedule, [_Head | Tail]) ->
	parse_flight_value(Schedule, Tail).

%% ---------------------------------------------------------------------------------------------------------------------------------------------------

parse_flight_attribute(Flight, [], _) -> Flight;

parse_flight_attribute(Flight, [<<"Departure">>], Tree) ->
	[Origin] = mochiweb_xpath:execute("//div/a/text()",Tree),
	[DepartureList] = mochiweb_xpath:execute("//div/text()",Tree),
	Departure = lists:filter(fun(X) -> X =/= <<>> end, re:split(re:replace(DepartureList, "/[\r\n ]*", <<"/">>, [global, {return, list}]), "\r\n *\t*", [{return, binary}])),
	Flight#flight{departure = parse_flight_value(#schedule{}, Departure), origin = airport(binary_to_list(Origin))};

parse_flight_attribute(Flight, [<<"Arrival">>], Tree) ->
	[Destination] = mochiweb_xpath:execute("//div/a/text()",Tree),
	[ArrivalList] = mochiweb_xpath:execute("//div/text()",Tree),
	Arrival = lists:filter(fun(X) -> X =/= <<>> end, re:split(re:replace(ArrivalList, "/[\r\n ]*", <<"/">>, [global, {return, list}]),  "\r\n *\t*", [{return, binary}])),
	Flight#flight{arrival = parse_flight_value(#schedule{}, Arrival), destination = airport(binary_to_list(Destination))};

parse_flight_attribute(Flight, [Text], _) ->
	{match, [Name]} = re:run(Text, "\\(\\) (.+)\r\n", [{capture, [1], binary}]),
	Flight#flight{name = Name}.
	
%% ---------------------------------------------------------------------------------------------------------------------------------------------------
	
parse_flight_component(Flight, []) -> Flight;

parse_flight_component(Flight, [Head|Tail]) -> 
	parse_flight_component(parse_flight_attribute(Flight, mochiweb_xpath:execute("//div/h2/text()",Head), Head), Tail).

%% ---------------------------------------------------------------------------------------------------------------------------------------------------

parse_flight(Flight, Tree) -> 
	StatusString = mochiweb_xpath:execute("//div[@class='content']/div[@class='uiComponent'][1]/strong/text()",Tree),
	case StatusString of
		[] -> error;
		__ ->
			{match, [Status]} = re:run(StatusString, "\r\n *([A-Za-z]+)", [{capture, [1], binary}]),
			[Airline] = mochiweb_xpath:execute("//div[@class='content']/div[@class='uiComponent'][1]/h2/a/text()",Tree),
			{_Airline, Number} = Flight#flight.flight,
			parse_flight_component(Flight#flight{flight = {Airline, Number}, status = Status}, mochiweb_xpath:execute("//div[@class='content']/div[@class='uiComponent']",Tree))
	end.

%% ----------------------------------------------------------------------------------------------

search_segments(_Flight, []) -> ok;

search_segments(Flight, [Segment|Tail]) ->
	case re:run(Segment, "\\?id=([0-9]+)$", [{capture, [1], list}]) of
		{match, [Id]} -> search_flight(Flight, lists:concat([?Flight, "id=", Id]));
		nomatch -> ok
	end,
	search_segments(Flight, Tail).
	
%% ----------------------------------------------------------------------------------------------

search_flight(Flight, Search) ->
	{ok, {_Status, _Header, HTML}} = httpc:request(Search),
	Tree = mochiweb_html:parse(re:replace(HTML, "(\\:\\&nbsp\\;)*", "", [{return, list}, global])),
	Segments = mochiweb_xpath:execute("//div[@class='listItem']/a[@href]/@href",Tree),
	case length(Segments) of
		0 -> io:format("~p~n", [parse_flight(Flight, Tree)]);
		_ -> search_segments(Flight, Segments)
	end.

%% ---------------------------------------------------------------------------------------------------------------------------------------------------

flight(Flight, Date) ->
	inets:start(),
	{match, [Airline, Number]} = re:run(Flight, "([A-Za-z]+)([0-9]+)", [{capture, [1,2], list}]),
	FlightStruct = {list_to_binary(Airline), list_to_integer(Number)},
	[YYYY, MM, DD] = re:split(Date, "-", [{return, list}]),
	DateStruct = {list_to_integer(YYYY), list_to_integer(MM), list_to_integer(DD)},
	search_flight(#flight{flight = FlightStruct, date = DateStruct}, lists:concat([?Flight, "airlineCode=", Flight, "&departureDate=", Date])).

%% ---------------------------------------------------------------------------------------------------------------------------------------------------

parse_airport(Airport, []) -> Airport;

parse_airport(Airport, [<<"Name">>, Value | Tail]) ->
	Name = re:replace(Value, lists:concat(["(\\(", binary_to_list(Airport#airport.iata), "\\) )"]), "", [{return, binary}]),
	parse_airport(Airport#airport{name=Name}, Tail);

parse_airport(Airport, [<<"Address">>, Value | Tail]) ->
	parse_airport(Airport#airport{address=Value}, Tail);

parse_airport(Airport, [<<"City">>, Value | Tail]) ->
	case re:run(Value, "[\n\t]*(.*)[\n\t]*(..)[,\n\t]*(.*)[\n\t]*", [{capture, [1,2,3], binary}]) of
		{match, [City, Country, <<>>]} -> State = undefined;
		{match, [City, State, Country]} -> ok;
		_ -> City = State = Country = undefined
	end,
	parse_airport(Airport#airport{city = City, state = State, country = Country}, Tail);
	
parse_airport(Airport, [<<"Current Time">>, Value | Tail]) ->
	{match,[HH,MM,SS]} = re:run(Value, "\\(([0-9][0-9]):([0-9][0-9]):([0-9][0-9])\\)", [{capture, [1,2,3], list}]),
	parse_airport(Airport#airport{time={list_to_integer(HH), list_to_integer(MM), list_to_integer(SS)}}, Tail);

parse_airport(Airport, [<<"Current Date">>, Value | Tail]) ->
	[Day, MonthString, Year] = re:split(Value, "-",[{return, list}]),
	{MonthString, Month} = lists:keyfind(MonthString, 1, ?Months),
	parse_airport(Airport#airport{date={list_to_integer(Year), Month, list_to_integer(Day)}}, Tail);

parse_airport(Airport, [<<"Time Zone">>, Value | Tail]) ->
	{match, [HH, MM]} = re:run(Value, "([-|+][0-9]+):([0-9]+)", [{capture, [1,2], list}]),
	parse_airport(Airport#airport{zone = {list_to_integer(HH), list_to_integer(MM), 0}}, Tail);

parse_airport(Airport, [_Attribute, _Value | Tail]) ->
	parse_airport(Airport, Tail).

%% ----------------------------------------------------------------------------------------------

universal_seconds(Airport) ->
	Date = Airport#airport.date,
	Time = Airport#airport.time,
	{HH, MM, SS} = Airport#airport.zone,
	calendar:datetime_to_gregorian_seconds({Date,Time}) - ((HH * 60 + MM) * 60 + SS).

%% ---------------------------------------------------------------------------------------------------------------------------------------------------

airport(Iata) ->
	inets:start(),
	Search = lists:concat([?Search, Iata]),
	{ok, {_Status, _Header, HTML}} = httpc:request(Search),
	Tree = mochiweb_html:parse(re:replace(HTML, "(\\:\\&nbsp\\;)*", "", [{return, list}, global])),
	List = mochiweb_xpath:execute("//div[@class='uiComponent'][1]/table/tr/td/text()",Tree),
	parse_airport(#airport{iata=list_to_binary(Iata)}, List).
