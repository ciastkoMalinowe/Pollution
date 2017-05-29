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

getKey(Station,Monitor) ->
  Keys = dict:fetch_keys(Monitor),
  Key = [S || S <- Keys, string:equal(S#station.name,Station)  or (S#station.place == Station)],
  case Key of
    [] -> no_key;
    _ -> [K] = Key,
      K
  end.

addStation(Name, {Width, Height}, Monitor) ->
  K1 = getKey(Name, Monitor),
  case K1 of
    no_key ->
      K2 = getKey({Width, Height}, Monitor),
      case K2 of
        no_key -> dict:append_list(#station{name=Name, place={Width, Height}},[],Monitor);
        _      -> {error, "station with that coordinates exists."}
      end;
    _      -> {error, "station with that name exists."}
  end.

addValue(Station, Datetime, Type, Value, Monitor) ->
  K  = getKey(Station,Monitor),
  case K of
    no_key -> {error, "choosen station do not exist"};
    _      ->
      Measurements = dict:fetch(K, Monitor),
      Eq = fun (M) -> (M#measurement.datetime == Datetime) and string:equal(M#measurement.type, Type) end,
      case lists:any(Eq, Measurements) of
        false -> dict:append(K,#measurement{datetime=Datetime, type=Type, value=Value},Monitor);
        true  -> {error, "measurement has already exists"}
      end
  end.

removeValue(Station, Datetime, Type, Monitor) ->
  Key = getKey(Station,Monitor),
  case Key of
    no_key -> {error, "choosen station do not exist"};
    _      ->
      Measurements = dict:fetch(Key, Monitor),
      Eq = fun (M) -> (M#measurement.datetime == Datetime) and string:equal(M#measurement.type, Type) end,
      case lists:any(Eq,Measurements) of
        false -> {error, "no matching measurement"};
        true  -> dict:update(Key, fun (M) -> [ X || X <- M, Eq(M) == false]end, Monitor)
      end
  end.

getOneValue(Station, Datetime, Type, Monitor)->
  Key = getKey(Station, Monitor),
  case Key of
    no_key -> {error, "choosen station do not exist"};
    _        ->
      Val = [V || V <- dict:fetch(Key,Monitor),
        (V#measurement.datetime == Datetime) and string:equal(V#measurement.type, Type) ],
      case Val of
        [] -> {error, "choosen measurement do not exists"};
        _  -> [V] = Val,
          V#measurement.value
      end
  end.


avg(Measurement,{Sum, Num}) ->
  lists:foldl(fun (M,{S,N}) -> {S + M#measurement.value, N+1} end, {Sum,Num}, Measurement).

getStationMean(Station, Type, Monitor) ->
  Key = getKey(Station, Monitor),
  case Key of
    no_key -> {error, "choosen station do not exist"};
    _      ->
      Measurements = [V || V <- dict:fetch(Key,Monitor), V#measurement.type == Type],
      {Sum, Num} = avg(Measurements,{0,0}),
      case Num of
        0 -> {error, "there are no measurements from choosen station"};
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
    0 -> {error, "there are no measurements on choosen day"};
    _ -> Sum / Num
  end.

getNorms() ->
  [ #norm{pm="PM10" , rate=50},
    #norm{pm="PM2,5", rate=30}].

getAirQualityIndex(Station, Datetime, Monitor) ->
  Key = getKey(Station, Monitor),
  case Key of
    no_key -> {error, "choosen station do not exist"};
    _      ->
      L = [X || X <- dict:fetch(getKey(Station, Monitor),Monitor), (X#measurement.datetime == Datetime)],
      Norm = getNorms(),
      Result = [M#measurement.value / W#norm.rate * 100.0 || M <- L, W <- Norm, string:equal(M#measurement.type, W#norm.pm)],
      AirQIndex = lists:foldl(fun (R,Acc) -> case R > Acc of
                                   true -> R;
                                   false -> Acc
                                 end
                  end, -1, Result),
      case AirQIndex of
        -1 -> {error, "no measurements to compute air quality index"};
        _  -> AirQIndex
      end
  end.
