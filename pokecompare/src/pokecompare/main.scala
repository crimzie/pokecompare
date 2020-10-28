package pokecompare

import java.nio.charset.CodingErrorAction

import scala.io.Codec
import scala.io.Source
import scala.util.Try

private val replace: CodingErrorAction = CodingErrorAction.REPLACE.asInstanceOf[CodingErrorAction]
given Codec = Codec.UTF8.onUnmappableCharacter(replace).onMalformedInput(replace)

@main
def run(teamFile: String) =
  val teamTr = for 
    src <- Try(Source.fromFile(teamFile))
    team <- src.getLines.map { line =>
        val chunks = line.split(',')
        for
          pok <- Try(Pokemon.valueOf(chunks(0)))
          cp <- Try(chunks(1).toInt)
          fast <- Try(FastMove.valueOf(chunks(2)))
          charge <- Try(ChargeMove.valueOf(chunks(3)))
        yield UserPoke(pok, cp, fast, charge)
      }.foldLeft[Try[List[UserPoke]]](Try(Nil)) { (listT, pokT) =>
        for
          list <- listT
          pok <- pokT
        yield pok :: list
      }
  yield team
  teamTr.fold(println, team =>
    //TODO proper CLI with cyclic prompt
  ) 
  //bestRoster(Pokemon.Vaporeon, 1877, 5, team)
  //bestRoster(Pokemon.Exeggutor, 2075, 5, team)
  //printAssess(Pokemon.Snorlax, FastMove.Lick, ChargeMove.HyperBeam, 2414, 10)

  //  println("\n==========\n")
  //  
  //  println("\n==========\n")
  //  printTeamRoster("Wigglytuff", 1841, 5)
  //  println("\n==========\n")
  //  printTeamRoster("Arcanine", 1820, 5)
  //  println("\n==========\n")
  //  printTeamRoster("Clefable", 1543, 5)
  //    println("\n==========\n")
  //    printTeamRoster("Nidoqueen", 1977, 5)
  //    println("\n==========\n")
  //    printTeamRoster("Lapras", 1927, 5)
  //    println("\n==========\n")
  //    printTeamRoster("Charizard", 1910, 5)
  //    println("\n==========\n")
  //    printTeamRoster("Flareon", 1686, 5)

  //  printCounter("Vaporeon", 5)

  //    

