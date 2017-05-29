%%%-------------------------------------------------------------------
%%% Created : 16. maj 2017 10:34
%%%-------------------------------------------------------------------
-module(rPollution).

-behaviour(gen_server).

%% API
-export([start_link/1, init/1, handle_call/3]).

%% gen_server callbacks
-export([addStation/2
  ,addValue/4
  ,removeValue/3
  ,getOneValue/3
  ,getStationMean/2
  ,getDailyMean/2
  ,getAirQualityIndex/2
  ,terminate/0]).


start_link(InitValue) ->
  gen_server:start_link(
    {local,rPollution}
    ,rPollution
    ,InitValue,[]).

init(_) ->
  M = pollution:createMonitor(),
  {ok, M}.

addStation(Name, {Height, Width}) ->
  gen_server:call(rPollution,{addStation, {Name, {Height, Width}}}).

addValue(Station, Datetime, Type, Value) ->
  gen_server:call(rPollution,{addValue, {Station, Datetime, Type, Value}}).

removeValue(Station, Datetime, Type) ->
  gen_server:call(rPollution,{removeValue, {Station, Datetime, Type}}).

getOneValue(Station, Datetime, Type)->
  gen_server:call(rPollution, {getOneValue, {Station, Datetime, Type}}).

getStationMean(Station, Type) ->
  gen_server:call(rPollution, {getStationMean, {Station, Type}}).

getDailyMean(Type, Date) ->
  gen_server:call(rPollution, {getDailyMean, {Type, Date}}).

getAirQualityIndex(Station, Datetime) ->
  gen_server:call(rPollution, {getAirQualityIndex, {Station, Datetime}}).

handle_call({addStation, Args},_From,LoopData) ->
  {Name, Coordinates} = Args,
  Res = pollution:addStation(Name,Coordinates,LoopData),
  case Res of
    {error, _} -> {reply, Res, LoopData};
    _                -> {reply, ok, Res}
  end;

handle_call({addValue, Args},_From,LoopData) ->
  {Station, Datetime, Type, Value} = Args,
  Res = pollution:addValue(Station, Datetime, Type, Value,LoopData),
  case Res of
    {error, _} -> {reply, Res, LoopData};
    _                -> {reply, ok, Res}
  end;

handle_call({removeValue, Args},_From,LoopData) ->
  {Station, Datetime, Type} = Args,
  Res = pollution:removeValue(Station, Datetime, Type, LoopData),
  case Res of
    {error, _} -> {reply, Res, LoopData};
    _                -> {reply, ok, Res}
  end;

handle_call({getOneValue, Args},_From,LoopData) ->
  {Station, Datetime, Type} = Args,
  Res = pollution:getOneValue(Station, Datetime, Type, LoopData),
  {reply, Res, LoopData};


handle_call({getStationMean, Args},_From,LoopData) ->
  {Station, Type} = Args,
  Res = pollution:getStationMean(Station, Type, LoopData),
  {reply, Res, LoopData};

handle_call({getDailyMean, Args},_From,LoopData) ->
  {Type, Date} = Args,
  Res = pollution:getDailyMean(Type, Date, LoopData),
  {reply, Res, LoopData};

handle_call({getAirQualityIndex, Args},_From,LoopData) ->
  {Station, Datetime} = Args,
  Res = pollution:getAirQualityIndex(Station, Datetime, LoopData),
  {reply, Res, LoopData}.

terminate() -> gen_server:stop(rPollution).
