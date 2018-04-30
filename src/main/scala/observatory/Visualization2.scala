package observatory

import com.sksamuel.scrimage.{Image, RGBColor}
import scala.math.{floor, ceil, round}
import observatory.Interaction.{tileLocation}
import observatory.Visualization.{interpolateColor}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    val CellPoint(x, y) = point
    d00*(1-x)*(1-y) + d10*x*(1-y) + d01*(1-x)*y + d11*x*y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    
    def locationToTemperature(grid: GridLocation => Temperature, 
        location: Location): Temperature = {
      val Location(lat, lon) = location
      val List(latFloor, latCeil, lonFloor, lonCeil) = List(floor(lat), ceil(lat), floor(lon), ceil(lon)).map(round(_).toInt)
      val point = CellPoint(lat - floor(lat), lon - floor(lon))
      val List(d00, d01, d10, d11) = List(
          GridLocation(latFloor, lonFloor),
          GridLocation(latFloor, lonCeil),
          GridLocation(latCeil, lonFloor),
          GridLocation(latCeil, lonCeil)
      ).map(grid)
      bilinearInterpolation(point, d00, d01, d10, d11)
    }
    
    val Tile(x, y, zoom) = tile
    val coordinates = for {
      y <- 0 until 256
      x <- 0 until 256
    } yield (x, y)
    val pixelArray = coordinates.par.map(r => Tile(256 * x + r._1, 256 * y + r._2, zoom + 8))
      .map(tileLocation)
      .map(locationToTemperature(grid, _))
      .map(interpolateColor(colors, _))
      .map(color => color match { case Color(r, g, b) => RGBColor(r, g, b, 127).toPixel}).toSeq.toArray
      
    Image.apply(256, 256, pixelArray)
  }
}