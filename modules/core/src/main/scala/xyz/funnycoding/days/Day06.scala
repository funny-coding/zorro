package xyz.funnycoding.days

import xyz.funnycoding.domain.solution._

object Day6 extends Serializable {
  type Planet         = String
  type Orbit          = String
  type UniversalOrbit = Map[Planet, List[Orbit]]

  def parse(data: List[String]): UniversalOrbit =
    data.foldLeft[UniversalOrbit](Map.empty)((map, str) => {
      val raw = str.split(')')
      map.get(raw.head) match {
        case None    => map.updated(raw.head, raw.tail.toList)
        case Some(v) => map.updated(raw.head, v ++ raw.tail.toList)
      }
    })

  def compute(universalOrbit: UniversalOrbit): Int =
    universalOrbit.values.map(_.map(orbit => indirectLink(orbit, universalOrbit)).sum).sum

  def indirectLink(orbit: Orbit, universalOrbit: UniversalOrbit): Int = {
    def go(orbit: Orbit, count: Int, map: UniversalOrbit): Int = {
      val filtered = map.filter {
        case (_, orbits) =>
          (orbits.contains(orbit))
      }
      if (filtered.isEmpty) count
      else go(filtered.keys.head, count + 1, map)
    }
    go(orbit, 0, universalOrbit)
  }

  def mkSol: List[String] => Solution = list => {
    val parsed = parse(list)
    val first  = compute(parsed)
    Solution(first.toString(), "")
  }
}
