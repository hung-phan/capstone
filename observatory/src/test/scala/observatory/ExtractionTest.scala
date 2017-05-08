package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {
  test("Extraction.getStationsDataFrame should return correct result") {
    val ds = Extraction.getStationsDataFrame("/stations.csv")
    val data = ds.first()
    assert(data.id == "008268")
    assert(data.latitude == 32.950)
    assert(data.longitude == 65.567)
  }

  test("Extraction.getTemperaturesDataFrame should return correct result") {
    val ds = Extraction.getTemperaturesDataFrame(2015, "/2015.csv")
    val data = ds.first()
    assert(data.id == "007070")
    assert(data.year == 2015)
    assert(data.month == 9)
    assert(data.day == 25)
    assert((data.temperature - 30.9).abs <= 0.1)
  }

  test("Extraction.locateTemperatures should return correct result") {
    val result = Extraction.getJoinDataFrame(1975, "/stations.csv", "/1975.csv")
    assert(result.count() == 2176493)
  }

  test("Extraction.locateTemperatures should have correct temperature") {
    val result = Extraction.getJoinDataFrame(2015, "/stations.csv", "/2015.csv").take(3)
    println(result.mkString("\n"))
  }

}