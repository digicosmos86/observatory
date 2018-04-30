package observatory

import observatory.Visualization.{predictTemperature}
import java.util.concurrent.ConcurrentHashMap

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  
  
  def predictTemperatureGrid(temperatures: Iterable[(Location, Temperature)], gridLocation: GridLocation): Temperature = {
    val GridLocation(lat, lon) = gridLocation
    predictTemperature(temperatures, Location(lat.toDouble, lon.toDouble))
  }
  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    g: GridLocation => {
      val temp = temperatures
      var map = new ConcurrentHashMap[GridLocation, Temperature]()
      this.synchronized {
        if (map.containsKey(g))
          map.get(g)
        else
          map.putIfAbsent(g, predictTemperatureGrid(temperatures, g))
      }
    }
    
    
//    val gridLocations = for {
//      lat <- Range(-89, 91, 1)
//      lon <- Range(-180, 180, 1)
//    } yield GridLocation(lat, lon)
//
//   val gridMap = gridLocations.map { x  => 
//      val GridLocation(lat, lon) = x
//      val temp = predictTemperature(temperatures, Location(lat.toDouble, lon.toDouble))
//      (x, temp)
//    }.toMap
//    
//    val gridToTemp = { g: GridLocation => gridMap(g) }
//    
//    gridToTemp
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val temperatures = temperaturess.flatten.groupBy(_._1).mapValues(x => x.map(_._2)).map(p => (p._1, p._2.sum/p._2.size))
    makeGrid(temperatures)
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val present = makeGrid(temperatures)
    g: GridLocation => present(g) - normals(g)
  }


}

