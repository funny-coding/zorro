package xyz.funnycoding.days

import xyz.funnycoding.domain.solution._

object Day6 {
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

  def indirectLink(orbit1: Orbit, orbit2: Orbit, universalOrbit: UniversalOrbit): Int = {

    def ancestors(orbit: Orbit, count: Int, list: List[(Orbit, Int)], map: UniversalOrbit): List[(Orbit, Int)] = {
      val filtered = map.filter {
        case (_, orbits) =>
          (orbits.contains(orbit))
      }
      if (filtered.isEmpty) (orbit, count) :: list
      else ancestors(filtered.keys.head, count + 1, (orbit, count) :: list, map)
    }

    val or1 = universalOrbit
      .filter {
        case (_, orbits) =>
          (orbits.contains(orbit1))
      }
      .keys
      .head
    val or2 = universalOrbit
      .filter {
        case (_, orbits) =>
          (orbits.contains(orbit2))
      }
      .keys
      .head

    val anc1 = ancestors(or1, 0, List.empty, universalOrbit)
    println(anc1)
    val anc2 = ancestors(or2, 0, List.empty, universalOrbit)
    println(anc2)
    val common = anc1.map(_._1).intersect(anc2.map(_._1))
    println(common)
    val compute = common.map { ancestor =>
      for {
        swap1 <- anc1.filter { case (a, _) => a == ancestor }.headOption
        swap2 <- anc2.filter { case (a, _) => a == ancestor }.headOption
      } yield swap1._2 + swap2._2
    }.flatten
    compute.min
  }

  def mkSol: List[String] => Solution = list => {
    val parsed = parse(list)
    val first  = compute(parsed)
    val second = indirectLink("YOU", "SAN", parsed)
    Solution(first.toString(), second.toString())
  }
}
