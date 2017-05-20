package observatory

import java.time.LocalDate

import com.sksamuel.scrimage.{Pixel, RGBColor}

import scala.math._

case class Location(lat: Double, lon: Double) {
  lazy val point: Point = Point(toRadians(lat), toRadians(lon))
}

case class Tile(x: Double, y: Double, zoom: Int) {
  lazy val location: Location = Location(
    lat = toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y / (1 << zoom))))),
    lon = x / (1 << zoom) * 360.0 - 180.0)

  def toURI = new java.net.URI("http://tile.openstreetmap.org/" + zoom + "/" + x + "/" + y + ".png")
}

case class Color(red: Int, green: Int, blue: Int) {
  def pixel(alpha: Int = 255): Pixel = RGBColor(red, green, blue, alpha).toPixel
}

case class Station(id: String, latitude: Double, longitude: Double)

case class StationDate(year: Int, month: Int, day: Int) {
  def toLocalDate: LocalDate = LocalDate.of(year, month, day)
}

case class TemperatureRecord(id: String, year: Int, month: Int, day: Int, temperature: Double)

case class Joined(id: String, latitude: Double, longitude: Double, day: Int, month: Int, year: Int, temperature: Double)

case class JoinedFormat(id: String, date: StationDate, location: Location, temperature: Double)

case class Point(ϕ: Double, λ: Double) {
  lazy val location: Location = Location(toDegrees(ϕ), toDegrees(λ))

  def haversineEarthDistance(other: Point): Double = {
    val r = 6372.8 // mean radius Earth in KM
    r * greatCircleDistance(other) * 1000
  }

  def greatCircleDistance(other: Point): Double =
    acos(sin(ϕ) * sin(other.ϕ) + cos(ϕ) * cos(other.ϕ) * cos(abs(λ - other.λ)))
}
