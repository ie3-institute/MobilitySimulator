/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.ElectricCurrentType
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.mobsim.io.geodata.PoiEnums
import edu.ie3.mobsim.io.geodata.PoiEnums.CategoricalLocationDictionary
import edu.ie3.util.quantities.PowerSystemUnits.{KILOWATT, KILOWATTHOUR}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities.getQuantity

import java.time.temporal.ChronoUnit
import java.util.UUID
import javax.measure.quantity.Length
import scala.collection.immutable.Queue
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.Random

object ChargingBehavior extends LazyLogging {}
