package com.crimzie.misc

import java.nio.charset.CodingErrorAction.REPLACE

import scala.io.Codec.UTF8
import scala.io.Source

object Comparator extends App {

  case class PokeData(tipe: (String, String), cpStep: Double, cpMax: Int, fast: Seq[String], charge: Seq[String])

  case class MoveData(tipe: String, group: String, time: Double, damage: Int, energy: Int)

  case class Compared(fastA: String, chargeA: String, movesB: Seq[(String, String, Double)], min: Double, avg: Double,
                      max: Double)

  implicit val codec =
    UTF8
      .onUnmappableCharacter(REPLACE)
      .onUnmappableCharacter(REPLACE)

  val types =
    List(
      "bug", "dar", "dra", "ele", "fai", "fig", "fir", "fly", "gho", "gra", "gro", "ice", "nor", "poi", "psy",
      "roc", "ste", "wat", "non"
    )

  val typeCoefficient: Map[String, Map[String, Double]] =
    types.zip {
      Source
        .fromInputStream(getClass.getResourceAsStream("/types.txt"))
        .getLines
        .map { x =>
          types.zip {
            x.split(',')
              .map(_.toDouble)
              .toSeq
          }.toMap
        }
        .toSeq
    }.toMap

  val moves: Map[String, MoveData] =
    Source
      .fromInputStream(getClass.getResourceAsStream("/attacks.txt"))
      .getLines
      .map { line =>
        val fields = line.split(',')
        (fields(0), MoveData(fields(1), fields(2), fields(4).toDouble, fields(3).toInt, fields(6).toInt))
      }
      .toMap

  val team: List[(String, (Int, String, String))] =
    Source
      .fromInputStream(getClass.getResourceAsStream("/team.txt"))
      .getLines
      .map { line =>
        val fields = line.split(',')
        (fields(0), (fields(1).toInt, fields(2), fields(3)))
      }
      .toList

  val pokemons: Map[String, PokeData] =
    Source
      .fromInputStream(getClass.getResourceAsStream("/pokemons.txt"))
      .getLines
      .map { line =>
        val fields = line.split(',')
        (fields(0), PokeData((fields(1), fields(2)), fields(6).toDouble, fields(7).toInt, fields(8).split(';'),
          fields(9).split(';')))
      }
      .toMap

  def pokeDps(pokemon: String, cp: Int, target: String): Map[String, Map[String, (Double, Double)]] = {
    val (tT1, tT2) = if (target.isEmpty) ("non", "non") else pokemons(target).tipe
    val PokeData((pT1, pT2), _, mCP, fast, charge) = pokemons(pokemon)
    val pCP = if (cp > 0) cp else mCP
    fast
      .map { x =>
        val MoveData(fT, _, fTime, fDmg, fEnrg) = moves(x)
        val fSTAB = if (fT == pT1 || fT == pT2) 1.25 else 1
        val fDps = (fDmg / fTime) * fSTAB * typeCoefficient(fT)(tT1) * typeCoefficient(fT)(tT2)
        val fEps = fEnrg / fTime
        (
          x,
          charge
            .map { y =>
              val MoveData(cT, _, cTime, cDmg, cEnrg) = moves(y)
              val cSTAB = if (cT == pT1 || cT == pT2) 1.25 else 1
              val cEps = -cEnrg / cTime
              val dps = (
                fDps * cEps + (cDmg / cTime) * cSTAB * typeCoefficient(cT)(tT1) * typeCoefficient(cT)(tT2) * fEps
                ) / (fEps + cEps)
              (y, (dps, dps * pCP / 2000))
            }
            .toMap
          )
      }
      .toMap
  }

  def pokeDps(pokemon: String, target: String): Map[String, Map[String, (Double, Double)]] = pokeDps(pokemon, 0, target)

  def pokeDps(pokemon: String, cp: Int): Map[String, Map[String, (Double, Double)]] = pokeDps(pokemon, cp, "")

  def pokeDps(pokemon: String): Map[String, Map[String, (Double, Double)]] = pokeDps(pokemon, "")

  def pokeCompare(pokemon: String, pCp: Int, vs: String, vCp: Int): Seq[Compared] = {
    val cp1 = if (pCp == 0) pokemons(pokemon).cpMax else pCp
    val cp2 = if (vCp == 0) pokemons(vs).cpMax else vCp
    pokeDps(pokemon, cp1, vs)
      .flatMap(fA => fA._2.map { cA =>
        val seq =
          pokeDps(vs, cp2, pokemon)
            .flatMap(fB => fB._2.map(cB => (fB._1, cB._1, cA._2._2 / cB._2._2)))
            .toSeq
            .sortBy(_._3)
        val num = seq.map(_._3)
        Compared(fA._1, cA._1, seq, num.min, num.sum / num.size, num.max)
      })
      .toSeq
      .sortBy(_.min)
  }

  def pokeCompare(pokemon: String, pCp: Int, vs: String): Seq[Compared] = pokeCompare(pokemon, pCp, vs, 0)

  def pokeCompare(pokemon: String, vs: String): Seq[Compared] = pokeCompare(pokemon, 0, vs)

  def printCounter(vs: String, n: Int) = {
    pokemons
      .map(p => (p._1, pokeCompare(p._1, p._2.cpMax, vs).last))
      .toSeq
      .sortBy(_._2.min)
      .reverse
      .take(n)
      .foreach { x =>
        println(s"\n${x._1} (${x._2.fastA} + ${x._2.chargeA}): min ${x._2.min.toString.take(4)}; " +
          s"avg ${x._2.avg.toString.take(4)}; max ${x._2.max.toString.take(4)}")
        x._2.movesB.foreach { y =>
          println(s"$vs (${y._1} + ${y._2}):  ${y._3.toString.take(4)}")
        }
      }
  }

  def printAssess(pokemon: String, fast: String, charge: String, cp: Int, n: Int) = {
    val limit = pokemons(pokemon).cpMax
    println(s" $cp CP / $limit CP maximum: ${(100.0 * cp / limit) formatted "%2.1f"}%\n")
    for {
      fast <- pokeDps(pokemon, cp)
      charge <- fast._2.toList.sortBy(_._2._1).reverse
    } println(s"${fast._1} + ${charge._1}: \t\t${charge._2._1 formatted "%2.2f"} \t${charge._2._2 formatted "%2.2f"}")
    println(s"\n$pokemon ($fast + $charge):")
    pokemons
      .map { p =>
        (p._1,
          pokeCompare(pokemon, p._1)
            .find(x => x.fastA == fast && x.chargeA == charge)
            .getOrElse(Compared("-", "-", Seq(("-", "-", 1.00)), 1.00, 1.00, 1.00)))
      }
      .toSeq
      .sortBy(_._2.min)
      .reverse
      .take(n)
      .foreach { x =>
        println(s"\n${x._1}: min ${x._2.min.toString.take(4)}; avg ${x._2.avg.toString.take(4)}; " +
          s"max ${x._2.max.toString.take(4)}")
        x._2.movesB.foreach { y => println(s"(${y._1} + ${y._2}):  ${y._3.toString.take(4)}") }
      }
  }

  def printTeamRoster(pokemon: String, cp: Int, n: Int) = {
    println(s"$pokemon $cp:")
    team
      .map {
        p =>
          (p._1,
            pokeCompare(p._1, p._2._1, pokemon, cp)
              .find(x => x.fastA == p._2._2 && x.chargeA == p._2._3)
              .getOrElse(Compared("-", "-", Seq(("-", "-", 1.00)), 1.00, 1.00, 1.00)),
            p._2._2,
            p._2._3)
      }
      .sortBy(_._2.min)
      .reverse
      .take(n)
      .foreach {
        x =>
          println(s"\n${
            x._1
          } (${
            x._3
          } + ${
            x._4
          }): min ${
            x._2.min.toString.take(4)
          }; avg ${
            x._2.avg.toString.take(4)
          }; " +
            s"max ${
              x._2.max.toString.take(4)
            }")
          x._2.movesB.foreach {
            y => println(s"(${
              y._1
            } + ${
              y._2
            }):   ${
              y._3.toString.take(4)
            }")
          }
      }
  }

//    printTeamRoster("Gyarados", 2415, 5)
//    println("\n==========\n")
//    printTeamRoster("Gyarados", 2334, 5)
//    println("\n==========\n")
//    printTeamRoster("Venusaur", 2151, 5)
//    println("\n==========\n")
//    printTeamRoster("Poliwrath", 2134, 5)
//    println("\n==========\n")
//    printTeamRoster("Nidoqueen", 2017, 5)
//    println("\n==========\n")
//    printTeamRoster("Nidoqueen", 1977, 5)
//    println("\n==========\n")
//    printTeamRoster("Lapras", 1927, 5)
//    println("\n==========\n")
//    printTeamRoster("Charizard", 1910, 5)
//    println("\n==========\n")
//    printTeamRoster("Flareon", 1686, 5)

  printAssess("Victreebel", "Razor Leaf", "Leaf Blade", 1801, 50)
}
