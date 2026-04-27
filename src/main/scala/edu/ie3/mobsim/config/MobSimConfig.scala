/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.config

import com.typesafe.config.Config
import edu.ie3.mobsim.exceptions.IllegalConfigException
import pureconfig.{ConfigConvert, *}
import pureconfig.error.*
import pureconfig.generic.*
import pureconfig.generic.semiauto.deriveConvert

import scala.deriving.Mirror

final case class MobSimConfig(
    input: MobSimConfig.Input,
    output: MobSimConfig.Output,
    simulation: MobSimConfig.Simulation,
) derives ConfigConvert

object MobSimConfig {
  // pure config start
  implicit def productHint[T]: ProductHint[T] =
    ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))

  extension (c: ConfigConvert.type)
    inline def derived[A](using m: Mirror.Of[A]): ConfigConvert[A] =
      deriveConvert[A]

  /** Returns a writer for [[SimonaConfig]].
    */
  private def writer: ConfigWriter[MobSimConfig] = ConfigWriter[MobSimConfig]

  def apply(typeSafeConfig: Config): MobSimConfig =
    apply(ConfigSource.fromConfig(typeSafeConfig))

  def apply(confSrc: ConfigObjectSource): MobSimConfig =
    confSrc.at("mobsim").load[MobSimConfig] match {
      case Left(readerFailures) =>
        val detailedErrors = readerFailures.toList
          .map {
            case CannotParse(msg, origin) =>
              f"CannotParse => $msg, Origin: $origin \n"
            case _: CannotRead =>
              f"CannotRead => Can not read config source} \n"
            case ConvertFailure(reason, _, path) =>
              f"ConvertFailure => Path: $path, Description: ${reason.description} \n"
            case ThrowableFailure(throwable, origin) =>
              f"ThrowableFailure => ${throwable.getMessage}, Origin: $origin \n"
            case failure =>
              f"Unknown failure type => ${failure.toString} \n"
          }
          .mkString("\n")
        throw IllegalConfigException(
          s"Unable to load config due to following failures:\n$detailedErrors"
        )
      case Right(conf) => conf
    }

  // pure config end

  final case class CsvParams(
      colSep: String,
      path: String,
  ) derives ConfigConvert

  final case class Input(
      homePoiMapping: Option[MobSimConfig.CsvParams],
      mobility: MobSimConfig.CsvParams,
  ) derives ConfigConvert

  final case class Output(
      outputDir: String = "mobSim",
      writeMovements: Boolean = true,
  ) derives ConfigConvert

  final case class Simulation(
      averageCarUsage: Double = 0.6,
      location: MobSimConfig.Simulation.Location,
      numberOfEv: Int,
      round15: Boolean = false,
      targetHomeChargingShare: Double,
  ) derives ConfigConvert

  object Simulation {

    final case class Location(
        chargingHubThresholdDistance: Double = 50.0,
        maxDistanceToChargingStation: Double = 500.0,
        maxDistanceToHomeChargingStation: Double = 30.0,
    ) derives ConfigConvert
  }
}
