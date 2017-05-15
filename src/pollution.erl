%%%-------------------------------------------------------------------
%%% Created : 23. kwi 2017 08:28
%%%-------------------------------------------------------------------
-module(pollution).
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4,
  getOneValue/4, getStationMean/3, getDailyMean/3, getAirQualityIndex/3]).


-record(station, {name, place}).
-record(measurement, {datetime, type, value}).
-record(norm,{pm, rate}).


createMonitor() -> dict:new().

addStation(Name, {Width, Height}, Monitor) ->
  case dict:is_key(#station{name = Name}, Monitor) of
    true  -> Monitor;
    _     ->  case dict:is_key(#station{place = {Width, Height}}, Monitor) of
              true -> Monitor;
              _    -> dict:append_list(#station{name=Name, place={Width, Height}},[],Monitor)
              end
  end.

getKey(Station,Monitor) ->
  Keys = dict:fetch_keys(Monitor),
  Key = [S || S <- Keys, string:equal(S#station.name,Station)  or (S#station.place == Station)],
  case Key of
    [] -> no_key;
    _ -> [K] = Key,
          K
  end.


addValue(Station, Datetime, Type, Value, Monitor) ->
  K  = getKey(Station,Monitor),
  case K of
    no_key -> Monitor;
    _      ->
      case [X || X <- dict:fetch(K, Monitor), (X#measurement.datetime == Datetime) and (X#measurement.type == Type)] of
        [] -> dict:append(K,#measurement{datetime=Datetime, type=Type, value=Value},Monitor);
        _  -> Monitor
      end
  end.

removeValue(Station, Datetime, Type, Monitor) ->
  dict:update(getKey(Station,Monitor),
    fun (L) -> [X || X <- L, X#measurement.datetime /= Datetime or not string:equals(X#measurement.type, Type)] end,
    Monitor).

getOneValue(Station, Datetime, Type, Monitor)->
  Key = getKey(Station, Monitor),
  case Key of
    no_key -> no_station;
    _        ->
      Val = [V || V <- dict:fetch(Key,Monitor),
      (V#measurement.datetime == Datetime) and string:equal(V#measurement.type, Type) ],
      case Val of
        [] -> no_data;
        _  -> [V] = Val,
          V#measurement.value
      end
  end.


avg(Measurement,{Sum, Num}) ->
  lists:foldl(fun (M,{S,N}) -> {S + M#measurement.value, N+1} end, {Sum,Num}, Measurement).

getStationMean(Station, Type, Monitor) ->
  Key = getKey(Station, Monitor),
  case Key of
    no_key -> no_station;
    _      ->
      Measurements = [V || V <- dict:fetch(Key,Monitor), V#measurement.type == Type],
      {Sum, Num} = avg(Measurements,{0,0}),
      case Num of
        0 -> 0;
        _ -> Sum / Num
      end
  end.

getDate(Datetime) ->
  {D,_} = Datetime,
  D.

getDailyMean(Type, Date, Monitor) ->
  Fun = fun (_,Value, {Sum, Num}) ->
    M = [V || V <- Value, (getDate(V#measurement.datetime) == Date) and
      string:equal(V#measurement.type, Type)],
    avg(M,{Sum,Num})
        end,
  {Sum, Num} = dict:fold( Fun, {0,0}, Monitor),
  case Num of
    0 -> 0;
    _ -> Sum / Num
  end.

getNorms() ->
  [ #norm{pm="PM10" , rate=50},
    #norm{pm="PM2,5", rate=30}].

getAirQualityIndex(Station, Datetime, Monitor) ->
  Key = getKey(Station, Monitor),
  case Key of
    no_key -> no_station;
    _      ->
      L = [X || X <- dict:fetch(getKey(Station, Monitor),Monitor), (X#measurement.datetime == Datetime)],
      Norm = getNorms(),
      Result = [M#measurement.value / W#norm.rate * 100.0 || M <- L, W <- Norm, string:equal(M#measurement.type, W#norm.pm)],
      lists:foldl(fun (R,Acc) -> case R > Acc of
                                   true -> R;
                                   false -> Acc
                                 end
                  end, 0, Result)
  end.
