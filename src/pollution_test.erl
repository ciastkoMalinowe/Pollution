%%%-------------------------------------------------------------------
%%% Created : 14. maj 2017 21:10
%%%-------------------------------------------------------------------
-module(pollution_test).

-include_lib("eunit/include/eunit.hrl").

-import(pollution, [createMonitor/0, addStation/3, addValue/5, getAirQualityIndex/3]).
simple_test() ->
  ?assert(true).

addValue_doublyAddedByName_test() ->
  P = createMonitor(),
  P1 = addStation("Station 1", {10, 10}, P),
  P2 = addValue("Station 1", {{2017,5,4},{20,52,0}}, "PM10", 100.0, P1),
  P3 = addValue("Station 1", {{2017,5,4},{20,52,0}}, "PM10", 100.0, P2),
  {E,_} = P3,
  ?assertEqual(error, E).

addValue_doublyAddedByCoordinates_test() ->
  P = createMonitor(),
  P1 = addStation("Station 1", {10, 10}, P),
  P2 = addValue({10, 10}, {{2017,5,4},{20,52,0}}, "PM10", 100.0, P1),
  P3 = addValue({10, 10}, {{2017,5,4},{20,52,0}}, "PM10", 100.0, P2),
  {E,_} = P3,
  ?assertEqual(error, E).

addValue_doublyAddedByNameAndByCoordinates_test() ->
  P = createMonitor(),
  P1 = addStation("Station 1", {10, 10}, P),
  P2 = addValue("Station 1", {{2017,5,4},{20,52,0}}, "PM10", 100.0, P1),
  P3 = addValue({10, 10}, {{2017,5,4},{20,52,0}}, "PM10", 100.0, P2),
  {E,_} = P3,
  ?assertEqual(error, E).

addValue_measurementsDiffValue_test() ->
  P = createMonitor(),
  P1 = addStation("Station 1", {10, 10}, P),
  P2 = addValue("Station 1", {{2017,5,4},{20,52,0}}, "PM10", 100.0, P1),
  P3 = addValue("Station 1", {{2017,5,4},{20,52,0}}, "PM10", 120.0, P2),
  {E,_} = P3,
  ?assertEqual(error, E).

addValue_notExistingStation_test() ->
  P = createMonitor(),
  P1 = addValue("Station 1", {{2017,5,4},{20,52,0}}, "PM10", 100.0, P),
  {E,_} = P1,
  ?assertEqual(error, E).

getAirQualityIndex_test() ->
  P = createMonitor(),
  P1 = addStation("Station 1", {10, 10}, P),
  P2 = addValue("Station 1", {{2017,5,4},{20,52,0}}, "PM10", 100.0, P1),
  P3 = addValue("Station 1", {{2017,5,4},{20,52,0}}, "PM2,5", 120.0, P2),
  ?assertEqual(400.0, getAirQualityIndex("Station 1", {{2017,5,4},{20,52,0}}, P3)).

getAirQualityIndex_noValues_test() ->
  P = createMonitor(),
  R = getAirQualityIndex("Station 1", {{2017,5,4},{20,52,0}}, P),
  {E, _} = R,
  ?assertEqual(error, E).