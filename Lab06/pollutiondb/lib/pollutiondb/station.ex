defmodule Pollutiondb.Station do
  use Ecto.Schema
  require Ecto.Query

  schema "stations" do
    field :name, :string
    field :lon, :float
    field :lat, :float

    has_many :readings, Pollutiondb.Reading
  end

  def add(station) do
    Pollutiondb.Repo.insert(station)
  end

  def get_all() do
    Pollutiondb.Repo.all(Pollutiondb.Station)
  end

  def get_by_id(id) do
    Pollutiondb.Repo.get(Pollutiondb.Station, id)
  end

  def remove(station) do
    Pollutiondb.Repo.delete(station)
  end

  def find_by_name(name) do
    Pollutiondb.Repo.all(
      Ecto.Query.where(Pollutiondb.Station, name: ^name) )
  end

  def find_by_location(lon, lat) do
    Ecto.Query.from(s in Pollutiondb.Station,
      where: s.lon == ^lon,
      where: s.lat == ^lat)
    |> Pollutiondb.Repo.all
  end

  def find_by_location_range(lon_min, lon_max, lat_min, lat_max) do
    Ecto.Query.from(s in Pollutiondb.Station,
      where: s.lon >= ^lon_min and s.lon <= ^lon_max and
             s.lat >= ^lat_min and s.lat <= ^lat_max
    ) |> Pollutiondb.Repo.all
  end

#  def update_name(station, newname) do
#    Ecto.Changeset.cast(station, %{name: newname}, [:name])
#    |> Ecto.Changeset.validate_required([:name])
#    |> Pollutiondb.Repo.update
#  end

  defp changeset(station, changesmap) do
    station
    |> Ecto.Changeset.cast(changesmap, [:name, :lon, :lat])
    |> Ecto.Changeset.validate_required([:name, :lon, :lat])
    |> Ecto.Changeset.validate_number(
         :lon, greater_than: -100.0, less_than: 100.0)
    |> Ecto.Changeset.validate_number(
         :lat, greater_than: -100.0, less_than: 100.0)
  end

  def add(name, lon, lat) do
    %Pollutiondb.Station{}
    |> changeset(%{name: name, lon: lon, lat: lat})
    |> Pollutiondb.Repo.insert()
  end

  def update_name(station, newname) do
    station
    |> changeset(%{name: newname})
    |> Pollutiondb.Repo.update
  end
end