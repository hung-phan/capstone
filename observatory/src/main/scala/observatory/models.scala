package observatory

import java.time.LocalDate

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int)

case class Station(id: String, latitude: Double, longitude: Double)

case class StationDate(year: Int, month: Int, day: Int) {
  def toLocalDate: LocalDate = LocalDate.of(year, month, day)
}

case class TemperatureRecord(id: String, day: Int, month: Int, year: Int, temperature: Double)

case class Joined(id: String, latitude:Double, longitude: Double, day: Int, month: Int, year: Int, temperature: Double)

case class JoinedFormat(date: StationDate, location: Location, temperature: Double)
