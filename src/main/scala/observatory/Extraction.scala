package observatory

import java.time.LocalDate
import org.apache.spark.sql.SparkSession
import org.apache.log4j.{Level, Logger}

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
    val spark = SparkSession.builder().master("local").appName("Observatory").getOrCreate()
    import spark.implicits._
    val stationDS = spark.read.textFile(this.getClass.getResource(stationsFile).toString)  // use getClass.getResource to find where the text file is
                              .map(_.split(",", -1))
                              .filter((x: Array[String]) => x(2) != "" && x(3) != "" && (x(0) != "" || x(1) != ""))
                              .map(row => Station((row(0), row(1)), Location(row(2).toDouble, row(3).toDouble)))
                              
    val temperaturesDS = spark.read.textFile(this.getClass.getResource(temperaturesFile).toString)
                              .map(_.split(",", -1))
                              .filter((x: Array[String]) => x(2) != "" && x(3) != "" && x(4) != "" && (x(0) != "" || x(1) != ""))
                              .map(row => DayTemp((row(0), row(1)), (year, row(2).toInt, row(3).toInt), (row(4).toDouble - 32)/1.8))
                                  
    temperaturesDS.joinWith(stationDS, temperaturesDS.col("SiteID")===stationDS.col("SiteID"))
                  .map(row => (row._2.Location, row._1.Date, row._1.Temp))
                  .collect()
                  .map(row => (LocalDate.of(row._2._1, row._2._2, row._2._3), row._1, row._3))
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records.par.map(x => (x._2, x._3)).groupBy(_._1).map(kv => (kv._1, kv._2.map(_._2))).map(kv => (kv._1, (kv._2.reduce((x, y) => x + y) / kv._2.size))).seq
  }

}
