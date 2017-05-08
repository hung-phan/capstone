package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._


/**
  * 1st milestone: data extraction
  */
object Extraction {
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Capstone")
      .config("spark.master", "local")
      .getOrCreate()

  import spark.implicits._


  def fsPath(resource: String): String =
    Paths.get(getClass.getResource(resource).toURI).toString


  def read(resource: String): DataFrame =
    spark.read.csv(fsPath(resource))


  def getStationsDataFrame(resource: String): Dataset[Station] =
    read(resource)
      .select(
        concat_ws("~", coalesce($"_c0", lit("")), $"_c1").alias("id"),
        $"_c2".alias("latitude").cast(DoubleType),
        $"_c3".alias("longitude").cast(DoubleType)
      )
      .where($"_c2".isNotNull && $"_c3".isNotNull && $"_c2" =!= 0.0 && $"_c3" =!= 0.0)
      .as[Station]


  def getTemperaturesDataFrame(year: Int, resource: String): Dataset[TemperatureRecord] =
    read(resource)
      .select(
        concat_ws("~", coalesce($"_c0", lit("")), $"_c1").alias("id"),
        lit(year).as("year"),
        $"_c2".alias("month").cast(IntegerType),
        $"_c3".alias("day").cast(IntegerType),
        (($"_c4" - 32) / 9 * 5).alias("temperature").cast(DoubleType)
      )
      .where($"_c2".between(1, 12))
      .where($"_c3".between(1, 31))
      .where($"_c4".between(-200, 200))
      .as[TemperatureRecord]


  def getJoinDataFrame(year: Int, stationsFile: String, temperaturesFile: String): Dataset[JoinedFormat] =
    getStationsDataFrame(stationsFile)
      .join(getTemperaturesDataFrame(year, temperaturesFile), usingColumn = "id")
      .as[Joined]
      .map(data => (
        data.id,
        StationDate(data.year, data.month, data.day),
        Location(data.latitude, data.longitude),
        data.temperature
      ))
      .toDF("id", "date", "location", "temperature")
      .as[JoinedFormat]


  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    getJoinDataFrame(year, stationsFile, temperaturesFile)
      .collect()
      .par
      .map(jf => (jf.date.toLocalDate, jf.location, jf.temperature))
      .seq
  }


  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records
      .par
      .groupBy(_._2)
      .mapValues(list => list.foldLeft(0.0)((temperature, tuple) => temperature + tuple._3) / list.size)
      .seq
  }
}
