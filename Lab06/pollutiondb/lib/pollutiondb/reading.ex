defmodule Pollutiondb.Reading do
  use Ecto.Schema
  require Ecto.Query

  schema "readings" do
    field :datetime, :utc_datetime #~D[2025-06-08], ~T[14:32:00]
    field :type, :string
    field :value, :float

    belongs_to :station, Pollutiondb.Station
  end

  def add_now(station_id, type, value) do
    %Pollutiondb.Reading{ station_id: station_id, type: type, value: value, datetime: DateTime.utc_now() |> DateTime.truncate(:second)}
     |> Pollutiondb.Repo.insert
  end

  def add(station_id, date, time, type, value) do
    case DateTime.new(date, time, "Etc/UTC") do
      {:ok, datetime} ->
        %Pollutiondb.Reading{
          station_id: station_id,
          datetime: DateTime.truncate(datetime, :second),
          type: type,
          value: value
        }
        |> Pollutiondb.Repo.insert()

      {:error, reason} ->
        {:error, reason}
    end
  end


  def find_by_date(date) do
    {:ok, start_dt} = DateTime.new(date, ~T[00:00:00])
    {:ok, end_dt} = DateTime.new(date, ~T[23:59:59])

    Ecto.Query.from(r in Pollutiondb.Reading,
      where: r.datetime >= ^start_dt and r.datetime <= ^end_dt
    )
    |> Pollutiondb.Repo.all()
  end
end