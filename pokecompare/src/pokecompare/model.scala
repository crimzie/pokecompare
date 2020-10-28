package pokecompare

enum PokeType:
  case Bug, Dark, Dragon, Electric, Fairy, Fighter, Fire, Flying, Ghost, Grass, Ground, Ice,
    Normal, Poison, Psychic, Rock, Steel, Water, None

enum MoveType:
  case Bug, Dark, Dragon, Electric, Fairy, Fighter, Fire, Flying, Ghost, Grass, Ground, Ice,
    Normal, Poison, Psychic, Rock, Steel, Water

case class UserPoke(race: Pokemon, cp: Int, fast: FastMove, charge: ChargeMove)

case class Perf(
    against: Map[FastMove, Map[ChargeMove, Double]],
    best: Double,
    avg: Double,
    worst: Double,
)

