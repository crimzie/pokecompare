package pokecompare

def pokeDps(
  pokemon: Pokemon,
  cp: Int = 0, 
  targetOp: Option[Pokemon] = None,
): Map[FastMove, Map[ChargeMove, Double]] =
  val (tT1, tT2) = targetOp.map(_.pTypes).getOrElse(PokeType.None -> PokeType.None)
  val (pT1, pT2) = pokemon.pTypes
  val pCP = if cp > 0 then cp else pokemon.cpMax
  val fLst = for fast <- pokemon.fastL yield
    val fSTAB = if fast.mType == pT1 || fast.mType == pT2 then 1.25 else 1
    val fDps = (fast.damage / fast.time) * fSTAB * typeCoefs(fast.mType)(tT1) * typeCoefs(fast.mType)(tT2)
    val fEps = fast.energy / fast.time
    val cLst = for charge <- pokemon.chargeL yield
      val cSTAB = if charge.mType == pT1 || charge.mType == pT2 then 1.25 else 1
      val cDps = (charge.damage / charge.time) * cSTAB * typeCoefs(charge.mType)(tT1) * typeCoefs(charge.mType)(tT2)
      val cEps = -charge.energy / charge.time
      val dps = (fDps * cEps + cDps * fEps) / (fEps + cEps)
      charge -> dps * pCP / 2000
    fast -> cLst.toMap
  fLst.toMap

def pokeCompare(
  pokemon: Pokemon,
  vs: Pokemon,
  pCp: Int = 0,
  vCp: Int = 0,
): Map[FastMove, Map[ChargeMove, Perf]] =
  val cpA = if pCp == 0 then pokemon.cpMax else pCp
  val cpB = if vCp == 0 then vs.cpMax else vCp
  pokeDps(pokemon, cpA, Some(vs)).mapValues { 
    _.mapValues { dpsA => 
      val against =
        pokeDps(vs, cpB, Some(pokemon)).mapValues { _.mapValues { dpsA / _ }.toMap }.toMap
      val dpsL = against.values.flatMap(_.values)
      Perf(against, dpsL.max, dpsL.sum / dpsL.size, dpsL.min)
    }.toMap
  }.toMap

