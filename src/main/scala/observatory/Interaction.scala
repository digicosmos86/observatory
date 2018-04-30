package observatory

import com.sksamuel.scrimage.{Image, RGBColor}
import scala.math.{pow, atan, sinh, toDegrees, Pi}
import observatory.Visualization.{interpolateColor, predictTemperature}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    tile match {
      case Tile(x, y, zoom) => {
        val n = pow(2, zoom)
        val lon = x / n * 360.0 - 180.0
        val lat = toDegrees(atan(sinh(Pi * (1 - 2 * y / n))))
        Location(lat, lon)
      }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val Tile(x, y, zoom) = tile
    val coordinates = for {
      y <- 0 until 256
      x <- 0 until 256
    } yield (x, y)
    val pixelArray = coordinates.par.map(r => Tile(256 * x + r._1, 256 * y + r._2, zoom + 8))
      .map(tileLocation)
      .map(predictTemperature(temperatures, _))
      .map(interpolateColor(colors, _))
      .map(color => color match { case Color(r, g, b) => RGBColor(r, g, b, 127).toPixel}).toSeq.toArray
    Image.apply(256, 256, pixelArray)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    (0 to 3).par.map(zoom => for { y <- 0 until pow(2, zoom).toInt; x <- 0 until pow(2, zoom).toInt } yield Tile(x, y, zoom))
      .map(r => r.par.foreach(tile => yearlyData.foreach(x => generateImage(x._1, tile, x._2))))
    ()
  }

}
