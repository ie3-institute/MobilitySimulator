/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.config

final case class MobSimConfig(
    mobsim: MobSimConfig.Mobsim
)
object MobSimConfig {
  final case class CsvParams(
      colSep: java.lang.String,
      path: java.lang.String
  )
  object CsvParams {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
    ): MobSimConfig.CsvParams = {
      MobSimConfig.CsvParams(
        colSep = $_reqStr(parentPath, c, "colSep", $tsCfgValidator),
        path = $_reqStr(parentPath, c, "path", $tsCfgValidator)
      )
    }
    private def $_reqStr(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
    ): java.lang.String = {
      if (c == null) null
      else
        try c.getString(path)
        catch {
          case e: com.typesafe.config.ConfigException =>
            $tsCfgValidator.addBadPath(parentPath + path, e)
            null
        }
    }

  }

  final case class Mobsim(
      input: MobSimConfig.Mobsim.Input,
      simulation: MobSimConfig.Mobsim.Simulation
  )
  object Mobsim {
    final case class Input(
        grid: MobSimConfig.Mobsim.Input.Grid,
        mobility: MobSimConfig.Mobsim.Input.Mobility
    )
    object Input {
      final case class Grid(
          name: java.lang.String,
          source: MobSimConfig.CsvParams
      )
      object Grid {
        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator
        ): MobSimConfig.Mobsim.Input.Grid = {
          MobSimConfig.Mobsim.Input.Grid(
            name = $_reqStr(parentPath, c, "name", $tsCfgValidator),
            source = MobSimConfig.CsvParams(
              if (c.hasPathOrNull("source")) c.getConfig("source")
              else com.typesafe.config.ConfigFactory.parseString("source{}"),
              parentPath + "source.",
              $tsCfgValidator
            )
          )
        }
        private def $_reqStr(
            parentPath: java.lang.String,
            c: com.typesafe.config.Config,
            path: java.lang.String,
            $tsCfgValidator: $TsCfgValidator
        ): java.lang.String = {
          if (c == null) null
          else
            try c.getString(path)
            catch {
              case e: com.typesafe.config.ConfigException =>
                $tsCfgValidator.addBadPath(parentPath + path, e)
                null
            }
        }

      }

      final case class Mobility(
          source: MobSimConfig.CsvParams
      )
      object Mobility {
        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator
        ): MobSimConfig.Mobsim.Input.Mobility = {
          MobSimConfig.Mobsim.Input.Mobility(
            source = MobSimConfig.CsvParams(
              if (c.hasPathOrNull("source")) c.getConfig("source")
              else com.typesafe.config.ConfigFactory.parseString("source{}"),
              parentPath + "source.",
              $tsCfgValidator
            )
          )
        }
      }

      def apply(
          c: com.typesafe.config.Config,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator
      ): MobSimConfig.Mobsim.Input = {
        MobSimConfig.Mobsim.Input(
          grid = MobSimConfig.Mobsim.Input.Grid(
            if (c.hasPathOrNull("grid")) c.getConfig("grid")
            else com.typesafe.config.ConfigFactory.parseString("grid{}"),
            parentPath + "grid.",
            $tsCfgValidator
          ),
          mobility = MobSimConfig.Mobsim.Input.Mobility(
            if (c.hasPathOrNull("mobility")) c.getConfig("mobility")
            else com.typesafe.config.ConfigFactory.parseString("mobility{}"),
            parentPath + "mobility.",
            $tsCfgValidator
          )
        )
      }
    }

    final case class Simulation(
        location: MobSimConfig.Mobsim.Simulation.Location,
        name: java.lang.String,
        numberOfEv: scala.Int,
        startDate: java.lang.String,
        targetHomeChargingShare: scala.Double
    )
    object Simulation {
      final case class Location(
          chargingHubThresholdDistance: scala.Double,
          maxDistanceToChargingStation: scala.Double,
          maxDistanceToHomeChargingStation: scala.Double
      )
      object Location {
        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator
        ): MobSimConfig.Mobsim.Simulation.Location = {
          MobSimConfig.Mobsim.Simulation.Location(
            chargingHubThresholdDistance =
              if (c.hasPathOrNull("chargingHubThresholdDistance"))
                c.getDouble("chargingHubThresholdDistance")
              else 50.0,
            maxDistanceToChargingStation =
              if (c.hasPathOrNull("maxDistanceToChargingStation"))
                c.getDouble("maxDistanceToChargingStation")
              else 500.0,
            maxDistanceToHomeChargingStation =
              if (c.hasPathOrNull("maxDistanceToHomeChargingStation"))
                c.getDouble("maxDistanceToHomeChargingStation")
              else 30.0
          )
        }
      }

      def apply(
          c: com.typesafe.config.Config,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator
      ): MobSimConfig.Mobsim.Simulation = {
        MobSimConfig.Mobsim.Simulation(
          location = MobSimConfig.Mobsim.Simulation.Location(
            if (c.hasPathOrNull("location")) c.getConfig("location")
            else com.typesafe.config.ConfigFactory.parseString("location{}"),
            parentPath + "location.",
            $tsCfgValidator
          ),
          name = $_reqStr(parentPath, c, "name", $tsCfgValidator),
          numberOfEv = $_reqInt(parentPath, c, "numberOfEv", $tsCfgValidator),
          startDate = $_reqStr(parentPath, c, "startDate", $tsCfgValidator),
          targetHomeChargingShare =
            $_reqDbl(parentPath, c, "targetHomeChargingShare", $tsCfgValidator)
        )
      }
      private def $_reqDbl(
          parentPath: java.lang.String,
          c: com.typesafe.config.Config,
          path: java.lang.String,
          $tsCfgValidator: $TsCfgValidator
      ): scala.Double = {
        if (c == null) 0
        else
          try c.getDouble(path)
          catch {
            case e: com.typesafe.config.ConfigException =>
              $tsCfgValidator.addBadPath(parentPath + path, e)
              0
          }
      }

      private def $_reqInt(
          parentPath: java.lang.String,
          c: com.typesafe.config.Config,
          path: java.lang.String,
          $tsCfgValidator: $TsCfgValidator
      ): scala.Int = {
        if (c == null) 0
        else
          try c.getInt(path)
          catch {
            case e: com.typesafe.config.ConfigException =>
              $tsCfgValidator.addBadPath(parentPath + path, e)
              0
          }
      }

      private def $_reqStr(
          parentPath: java.lang.String,
          c: com.typesafe.config.Config,
          path: java.lang.String,
          $tsCfgValidator: $TsCfgValidator
      ): java.lang.String = {
        if (c == null) null
        else
          try c.getString(path)
          catch {
            case e: com.typesafe.config.ConfigException =>
              $tsCfgValidator.addBadPath(parentPath + path, e)
              null
          }
      }

    }

    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
    ): MobSimConfig.Mobsim = {
      MobSimConfig.Mobsim(
        input = MobSimConfig.Mobsim.Input(
          if (c.hasPathOrNull("input")) c.getConfig("input")
          else com.typesafe.config.ConfigFactory.parseString("input{}"),
          parentPath + "input.",
          $tsCfgValidator
        ),
        simulation = MobSimConfig.Mobsim.Simulation(
          if (c.hasPathOrNull("simulation")) c.getConfig("simulation")
          else com.typesafe.config.ConfigFactory.parseString("simulation{}"),
          parentPath + "simulation.",
          $tsCfgValidator
        )
      )
    }
  }

  def apply(c: com.typesafe.config.Config): MobSimConfig = {
    val $tsCfgValidator: $TsCfgValidator = new $TsCfgValidator()
    val parentPath: java.lang.String = ""
    val $result = MobSimConfig(
      mobsim = MobSimConfig.Mobsim(
        if (c.hasPathOrNull("mobsim")) c.getConfig("mobsim")
        else com.typesafe.config.ConfigFactory.parseString("mobsim{}"),
        parentPath + "mobsim.",
        $tsCfgValidator
      )
    )
    $tsCfgValidator.validate()
    $result
  }
  final class $TsCfgValidator {
    private val badPaths =
      scala.collection.mutable.ArrayBuffer[java.lang.String]()

    def addBadPath(
        path: java.lang.String,
        e: com.typesafe.config.ConfigException
    ): Unit = {
      badPaths += s"'$path': ${e.getClass.getName}(${e.getMessage})"
    }

    def addInvalidEnumValue(
        path: java.lang.String,
        value: java.lang.String,
        enumName: java.lang.String
    ): Unit = {
      badPaths += s"'$path': invalid value $value for enumeration $enumName"
    }

    def validate(): Unit = {
      if (badPaths.nonEmpty) {
        throw new com.typesafe.config.ConfigException(
          badPaths.mkString("Invalid configuration:\n    ", "\n    ", "")
        ) {}
      }
    }
  }
}
