package observatory

import com.sksamuel.scrimage.{Image, RGBColor, Pixel}
import scala.math.{sin, cos, acos, abs, toRadians, pow}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    def getDistance(loc1: Location, loc2: Location): Double = { // assumes the two locations are different
      (loc1, loc2) match {
        case (Location(lac1, lon1), Location(lac2, lon2)) => {
          List(lac1, lon1, lac2, lon2).map(toRadians) match {
            case phi1::lambda1::phi2::lambda2::Nil => {
              6371.2*acos(sin(phi1)*sin(phi2)+cos(phi1)*cos(phi2)*cos(abs(lambda1-lambda2)))
            }
            case _ => 0.0  // won't ever reach here
          }
        }
      }     
    }
    
    val temperaturesMap = temperatures.toMap
    temperaturesMap.get(location) match {
      case Some(temp) => temp
      case None => {
        val tempIter = temperatures.map(x => {
          val inverseDistance = 1/pow(getDistance(x._1, location), 1.5)
          (inverseDistance*x._2, inverseDistance)
        })
        tempIter.map(_._1).reduce(_+_) / tempIter.map(_._2).reduce(_+_)
      }
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val sortedPoints = points.toList.sortWith((x, y) => x._1 > y._1)
    value match {
      case value if (value >= sortedPoints.head._1) => sortedPoints.head._2
      case value if (value <= sortedPoints.last._1) => sortedPoints.last._2
      case _ => {
        val zippedPoints = sortedPoints.zip(sortedPoints.drop(1)++List(sortedPoints.last))
        def loop(points: List[((Temperature, Color), (Temperature, Color))], value: Temperature): Color = {
          points match {
            case x::xs => if (value >= x._2._1) {             
              (x._1._2, x._2._2) match {
                case (Color(r1,g1,b1), Color(r2,g2,b2)) => 
                  Color((r2 + (r1-r2)*((value-x._2._1).toDouble/(x._1._1-x._2._1))).floatValue.round,
                        (g2 + (g1-g2)*((value-x._2._1).toDouble/(x._1._1-x._2._1))).floatValue.round,
                        (b2 + (b1-b2)*((value-x._2._1).toDouble/(x._1._1-x._2._1))).floatValue.round
                       )        
              }
            } else {
              loop(xs, value)
            }
            case _ => Color(0,0,0) // won't ever reach here
          }
        }
        loop(zippedPoints, value)
      }
    }   
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val numberArray = for {
      x <- Range(-180, 180, 1)
      y <- Range(90, -90, -1)      
    } yield (x, y)
    val pixelArray = numberArray.sortBy(r => (-r._2, r._1)).par.map(r => Location(r._1, r._2))
              .map(predictTemperature(temperatures, _)).map(interpolateColor(colors, _))
              .map(r => r match {
                case Color(r,g,b) => RGBColor(r,g,b).toPixel
              }).toSeq.toArray
    Image.apply(360, 180, pixelArray)
  }

}

