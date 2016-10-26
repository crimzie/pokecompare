package com.crimzie.misc

import java.nio.charset.CodingErrorAction.REPLACE

import scala.collection.breakOut
import scala.io.Codec.UTF8
import scala.io.Source

object Comparator extends App {

  case class PokeData(pType: (String, String), cpStep: Double, cpMax: Int, fast: Seq[String], charge: Seq[String])

  case class MoveData(mType: String, group: String, time: Double, damage: Int, energy: Int)

  case class Compared(
                       fastA: String = "-",
                       chargeA: String = "-",
                       movesB: Seq[(String, String, Double)] = Seq(("-", "-", 1.0)),
                       min: Double = 1.0,
                       avg: Double = 1.0,
                       max: Double = 1.0)

  implicit val codec =
    UTF8
      .onUnmappableCharacter(REPLACE)
      .onUnmappableCharacter(REPLACE)

  val types =
    List(
      "bug", "dar", "dra", "ele", "fai", "fig", "fir", "fly", "gho", "gra", "gro", "ice", "nor", "poi", "psy",
      "roc", "ste", "wat", "non")

  val typeCoefficient: Map[String, Map[String, Double]] =
    types
      .zip {
        for (line <- Source.fromInputStream(getClass getResourceAsStream "/types.txt").getLines.toSeq)
          yield
            types
              .zip[String, Double, Map[String, Double]] {
              line.split(',').map(_.toDouble)
            }(breakOut)
      }(breakOut)

  val moves: Map[String, MoveData] =
    Source
      .fromInputStream(getClass getResourceAsStream "/attacks.txt")
      .getLines
      .toSeq
      .map { line =>
        val fields = line.split(',')
        (fields(0), MoveData(fields(1), fields(2), fields(4).toDouble, fields(3).toInt, fields(6).toInt))
      }(breakOut)

  val team: List[(String, (Int, String, String))] =
    Source
      .fromInputStream(getClass getResourceAsStream "/team.txt")
      .getLines
      .toList
      .map { line =>
        val fields = line.split(',')
        (fields(0), (fields(1).toInt, fields(2), fields(3)))
      }(breakOut)

  val pokemons: Map[String, PokeData] =
    Source
      .fromInputStream(getClass getResourceAsStream "/pokemons.txt")
      .getLines
      .toSeq
      .map { line =>
        val fields = line.split(',')
        (fields(0), PokeData((fields(1), fields(2)), fields(6).toDouble, fields(7).toInt, fields(8).split(';'),
          fields(9).split(';')))
      }(breakOut)

  val frm: Double => String = _ formatted "%2.2f"

  def pokeDps(pokemon: String, cp: Int = 0, target: String = ""): Seq[(String, Seq[(String, (Double, Double))])] = {
    val (tT1, tT2) = if (target.isEmpty) ("non", "non") else pokemons(target).pType
    val PokeData((pT1, pT2), _, mCP, fast, charge) = pokemons(pokemon)
    val pCP = if (cp > 0) cp else mCP
    fast
      .map { x =>
        val MoveData(fT, _, fTime, fDmg, fEnrg) = moves(x)
        val fSTAB = if (fT == pT1 || fT == pT2) 1.25 else 1
        val fDps = (fDmg / fTime) * fSTAB * typeCoefficient(fT)(tT1) * typeCoefficient(fT)(tT2)
        val fEps = fEnrg / fTime
        val seq =
          charge
            .map { y =>
              val MoveData(cT, _, cTime, cDmg, cEnrg) = moves(y)
              val cSTAB = if (cT == pT1 || cT == pT2) 1.25 else 1
              val cEps = -cEnrg / cTime
              val dps = (
                fDps * cEps + (cDmg / cTime) * cSTAB * typeCoefficient(cT)(tT1) * typeCoefficient(cT)(tT2) * fEps
                ) / (fEps + cEps)
              (y, (dps, dps * pCP / 2000))
            }(breakOut)
        (x, seq)
      }(breakOut)
  }

  def pokeCompare(pokemon: String, pCp: Int = 0, vs: String, vCp: Int = 0): Seq[Compared] = {
    val cp1 = if (pCp == 0) pokemons(pokemon).cpMax else pCp
    val cp2 = if (vCp == 0) pokemons(vs).cpMax else vCp
    for {
      (fA, cAs) <- pokeDps(pokemon, cp1, vs)
      (cA, (_, dpsA)) <- cAs
    } yield {
      val seq =
        for {
          (fB, cBs) <- pokeDps(vs, cp2, pokemon)
          (cB, (_, dpsB)) <- cBs
        } yield (fB, cB, dpsA / dpsB)
      val num = seq.map(_._3)
      Compared(fA, cA, seq.sortBy(_._3), num.min, num.sum / num.size, num.max)
    }
  }

  def printCounter(vs: String, n: Int) = {
    pokemons
      .map { case (p, data) => (p, pokeCompare(p, data.cpMax, vs).sortBy(_.min).last) }(breakOut)
      .sortBy(_._2.min)
      .reverse
      .take(n)
      .foreach { case (p, Compared(fA, cA, mB, min, avg, max)) =>
        println(s"\n$p ($fA + $cA): min ${frm(min)}; avg ${frm(avg)}; max ${frm(max)}")
        for ((fB, cB, rel) <- mB) println(s"$vs ($fB + $cB): ${frm(rel)}")
      }
  }

  def printAssess(pokemon: String, fast: String, charge: String, cp: Int, n: Int) = {
    val limit = pokemons(pokemon).cpMax
    println(s" $cp CP / $limit CP maximum: ${frm(100.0 * cp / limit)}%\n")
    for {
      (fA, cAs) <- pokeDps(pokemon, cp)
      (cA, dps) <- cAs.sortBy(_._2._1).reverse
    } println(s"$fA + $cA: \t\t${frm(dps._1)} \t${frm(dps._2)}")
    println(s"\n$pokemon ($fast + $charge):")
    pokemons
      .map { case (p, _) =>
        val compared =
          pokeCompare(pokemon = pokemon, vs = p)
            .find(x => x.fastA == fast && x.chargeA == charge)
            .getOrElse(Compared())
        (p, compared)
      }(breakOut)
      .sortBy(_._2.min)
      .reverse
      .take(n)
      .foreach { case (p, Compared(_, _, mB, min, avg, max)) =>
        println(s"\n$p: min ${frm(min)}; avg ${frm(avg)}; max ${frm(max)}")
        for ((fB, cB, rel) <- mB) println(s"($fB + $cB):  ${frm(rel)}")
      }
  }

  def printTeamRoster(pokemon: String, cp: Int, n: Int) = {
    println(s"$pokemon $cp:")
    team
      .map { case (p, (pCp, fP, cP)) =>
        val compared =
          pokeCompare(p, pCp, pokemon, cp)
            .find(x => x.fastA == fP && x.chargeA == cP)
            .getOrElse(Compared())
        (p, compared, fP, cP)
      }
      .sortBy(_._2.min)
      .reverse
      .take(n)
      .foreach { case (p, Compared(_, _, mB, min, avg, max), fP, cP) =>
        println(s"\n$p ($fP + $cP): min ${frm(min)}; avg ${frm(avg)}; max ${frm(max)}")
        for ((fB, cB, rel) <- mB) println(s"($fB + $cB):   ${frm(rel)}")
      }
  }

  printTeamRoster("Exeggutor", 2075, 5)
  println("\n==========\n")
  printTeamRoster("Vaporeon", 1877, 5)
  println("\n==========\n")
  printTeamRoster("Wigglytuff", 1841, 5)
  println("\n==========\n")
  printTeamRoster("Arcanine", 1820, 5)
  println("\n==========\n")
  printTeamRoster("Clefable", 1543, 5)
  //    println("\n==========\n")
  //    printTeamRoster("Nidoqueen", 1977, 5)
  //    println("\n==========\n")
  //    printTeamRoster("Lapras", 1927, 5)
  //    println("\n==========\n")
  //    printTeamRoster("Charizard", 1910, 5)
  //    println("\n==========\n")
  //    printTeamRoster("Flareon", 1686, 5)

  //  printCounter("Vaporeon", 5)

  //  printAssess("Victreebel", "Razor Leaf", "Leaf Blade", 1801, 50)
}
