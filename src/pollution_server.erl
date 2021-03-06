%%%-------------------------------------------------------------------
%%% Created : 14. maj 2017 09:41
%%%-------------------------------------------------------------------
-module(pollution_server).
-export([start/0, stop/0, createMonitor/0, addStation/2, addValue/4, removeValue/3,
  getOneValue/3, getStationMean/2, getDailyMean/2, getAirQualityIndex/2]).

-export([init/0]).

start() ->
  register (pollutionServer, spawn(pollution_server, init, [])).

init() ->
  Monitor = pollution:createMonitor(),
  loop(Monitor).

loop(Monitor) ->
  receive
    {request, Pid, Function, Arguments} when Function == getOneValue
      orelse Function == getStationMean
      orelse Function == getDailyMean
      orelse Function == getAirQualityIndex ->

      Args = lists:append(Arguments, [Monitor]),
      P = apply(pollution, Function, Args),
      case P of
        {error, Message} -> Pid!{reply, {error, Message}};
        _                -> Pid!{reply, P}
      end,
      loop(Monitor);

    {request, Pid, stop, _} ->
      Pid!{reply, ok};

    {request, Pid, Function, Arguments} when Function == createMonitor
      orelse Function == addStation
      orelse Function == addValue
      orelse Function == removeValue ->

      Args = lists:append(Arguments, [Monitor]),
      M = apply(pollution, Function, Args),
      case M of
        {error, Message} ->
          Pid!{reply,{error,Message}},
          loop(Monitor);
        _ ->
          Pid!{reply, ok},
          loop(M)
      end
  end.


call(Function, Arguments) ->
  pollutionServer ! {request, self(), Function, Arguments},
  receive
    {reply, Reply} ->
      case Reply of
        {error, Message} -> Message;
        _                -> Reply
      end
  end.

createMonitor() -> call(createMonitor,[]).
addStation(Name, {Width, Height}) -> call(addStation,[Name,{Width, Height}]).
addValue(Station, Datetime, Type, Value) -> call(addValue, [Station, Datetime, Type, Value]).
removeValue(Station, Datetime, Type) -> call(removeValue, [Station, Datetime, Type]).
getOneValue(Station, Datetime, Type) -> call(getOneValue,[Station, Datetime, Type]).
getStationMean(Station, Type) -> call(getStationMean, [Station, Type]).
getDailyMean(Type, Date) -> call(getDailyMean, [Type, Date]).
getAirQualityIndex(Station, Datetime) -> call(getAirQualityIndex, [Station, Datetime]).

stop() -> call(stop,[]).