defmodule Pollutiondb.Parser do
  def parseLine(l) do
    [date_time,type,val,station_id,name,cords] = String.split(l,";")
    {:ok, datetime, 0} = DateTime.from_iso8601(date_time)
    [lon,lat] = String.split(cords,",")

    datetime = DateTime.truncate(datetime, :second)
    lon = String.to_float(lon)
    lat = String.to_float(lat)
    pollutionLevel = String.to_float(val)

    {datetime, {lon,lat}, pollutionLevel, type, name}
  end

  def readDataFromPath(path) do
    path
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(fn line -> parseLine(line) end)
  end

  def identifyStation(data) do
    data |> Enum.map(fn {datetime, {lon,lat}, pollutionLevel, type, name} -> {lon,lat, name} end)
    |> Enum.uniq_by(fn x -> x end)
  end

  #D:\\AGH\\AirlyData-ALL-50k.csv
  def loadData(path) do
    data = readDataFromPath(path)

    data
    |> identifyStation
    |> Enum.each(fn {lon,lat,name} -> Pollutiondb.Station.add(name,lon,lat) end)

    data
    |> Enum.each(fn {datetime, {lon, lat}, pollutionLevel, type, name} ->
      [station | _] = Pollutiondb.Station.find_by_name(name)

      date = DateTime.to_date(datetime)
      time = DateTime.to_time(datetime)

      Pollutiondb.Reading.add(station.id, date, time, type, pollutionLevel)
    end)
  end
end