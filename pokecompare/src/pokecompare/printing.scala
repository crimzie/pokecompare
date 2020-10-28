package pokecompare

def printMPerfs(seq: Seq[(Pokemon, FastMove, ChargeMove, Perf)]): Unit =
  for (p, fA, cA, Perf(fBMap, max, avg, min)) <- seq do
    println('\n' + f"$p ($fA + $cA): min $min%2.2f; avg $avg%2.2f; max $max%2.2f")
    for 
      (fB, cBMap) <- fBMap
      (cB, dps) <- cBMap
    do println(f"vs ($fB + $cB): $dps%2.2f")

def printPerfs(seq: Seq[(Pokemon, Perf)]): Unit =
  for (p, Perf(fBMap, max, avg, min)) <- seq do
    println('\n' + f"vs $p: min $min%2.2f; avg $avg%2.2f; max $max%2.2f")
    for 
      (fB, cBMap) <- fBMap
      (cB, dps) <- cBMap
    do println(f"($fB + $cB): $dps%2.2f")

def printBestCounters(vs: Pokemon, n: Int): Unit =
  val seq = for
    p <- Pokemon.values
    (fA, cAMap) <- pokeCompare(p, vs)
    (cA, perf) <- cAMap
  yield (p, fA, cA, perf)
  printMPerfs(seq.sortBy(_._4.worst).reverse.take(n))

def printAnalysis(pokemon: Pokemon, fast: FastMove, charge: ChargeMove, cp: Int, n: Int): Unit = 
  val limit = pokemon.cpMax
  println(f" $cp CP / $limit CP maximum: ${100.0 * cp / limit}%2.2f%%" + '\n')

  for
    (fA, cAMap) <- pokeDps(pokemon, cp)
    (cA, dps) <- cAMap.toSeq.sortBy(_._2).reverse
  do println(f"$fA + $cA: $dps%2.2f")

  println(s"\n$pokemon ($fast + $charge):")
  val results = Pokemon.values
    .map { p => p -> pokeCompare(pokemon, p)(fast)(charge) }
    .sortBy(_._2.worst)

  println("\n===== Best against:")
  printPerfs(results.reverse.take(n))

  println("\n===== Worst against:")
  printPerfs(results.take(n))
end printAnalysis

def printBestRoster(pokemon: Pokemon, cp: Int, n: Int, team: List[UserPoke]): Unit = 
  println(s"$pokemon $cp:")
  val comp = for UserPoke(p, pCp, fP, cP) <- team yield
    val perf = pokeCompare(p, pokemon, pCp, cp)(fP)(cP)
    (p, fP, cP, perf)
  printMPerfs(comp.sortBy(_._4.worst).reverse.take(n))

