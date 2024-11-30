/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.config

import com.typesafe.config.{Config, ConfigFactory}
import scopt.OptionParser

import java.nio.file.Paths

object ArgsParser {
  final case class Arguments(
      mainArgs: Array[String],
      config: Option[Config] = None,
  )

  def prepareConfig(args: Array[String]): Option[MobSimConfig] =
    parse(args).flatMap(_.config).map(MobSimConfig(_))

  private def parse(args: Array[String]): Option[Arguments] =
    buildParser.parse(args, Arguments(args))

  private def buildParser: OptionParser[Arguments] = {
    new OptionParser[Arguments]("mobSim") {
      opt[String]("config")
        .action((value, args) => {
          args.copy(
            config = Some(parseTypesafeConfig(value))
          )
        })
        .validate(value =>
          if (value.trim.isEmpty) failure("config location cannot be empty")
          else success
        )
        .validate(value =>
          if (value.contains("\\"))
            failure("wrong config path, expected: /, found: \\")
          else success
        )
        .text("Location of the config file")
        .minOccurs(1)
    }
  }

  private def parseTypesafeConfig(fileName: String): Config = {
    val file = Paths.get(fileName).toFile
    if (!file.exists())
      throw new Exception(s"Missing config file on path $fileName")
    ConfigFactory.parseFile(file)
  }
}
