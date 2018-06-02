defmodule PollutionData do
  @moduledoc false

  def importLinesFromCSV(src) do
    conv_list = File.read!(src) |> String.split("\r\n") |> Enum.map(& convertOneLine/1)
    time1 = fn () -> loadStationsToServer(conv_list) end |> :timer.tc |> elem(0)
    time2 = fn () -> addAllMeasurementrsToMonitor(conv_list) end |> :timer.tc |> elem(0)
    IO.puts "Stations load: #{time1} us. Measurements load #{time2} us."
  end

  defp convertOneLine(line) do
    [date, time, len, width, value] = line |> String.split(",")
    {int_val, _} = Integer.parse(value)
    %{:datetime => dateTime2Tuple(date, time),
      :location => coords2Tuple(len, width), :pollutionLevel => int_val}
  end

  defp dateTime2Tuple(date, time) do
    date_tup = date |> String.split("-") |> Enum.reverse
                    |> Enum.map(& elem(Integer.parse(&1), 0)) |> :erlang.list_to_tuple
    time = time <> ":00"
    time_tup = time |> String.split(":")
                    |> Enum.map(& elem(Integer.parse(&1), 0)) |> :erlang.list_to_tuple
    {date_tup, time_tup}
  end

  defp coords2Tuple(len, width) do
    coords_list = [len, width]
    coords_list |> Enum.map(& elem(Float.parse(&1), 0)) |> :erlang.list_to_tuple
  end

  defp identifyStations(data_list) do
    data_list |> Enum.reduce(%{}, fn(%{:location => loc}, acc) -> Map.put(acc, loc, loc) end)
  end

  defp loadStationsToServer(data_list) do
    :pollution_gen_server_supervisor.start_link()
    data_list |> identifyStations
              |> attachStationItsName
              |> createStationsAtMonitor
  end

  defp attachStationItsName(stations) do
    stations |>  Enum.map(fn {k, {long, lat}} -> {k, "station_#{long}_#{lat}"} end)
  end

  defp createStationsAtMonitor(stations_with_names) do
    stations_with_names
      |> Enum.each(
           fn {coords, name} -> :pollution_gen_server.addStation(name, coords) end
         )
  end

  defp addAllMeasurementrsToMonitor(data_list) do
    data_list |> Enum.each(& addSingleMeasurementToMonitor/1)
  end

  defp addSingleMeasurementToMonitor(%{:datetime => date,
  :location => loc, :pollutionLevel => value}) do
    :pollution_gen_server.addValue(loc, date, 'PM10', value)
  end

end
