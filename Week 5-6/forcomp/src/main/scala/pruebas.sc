val fruit = "apple"
(fruit map ((_, 1)) groupBy(_._1) map ( x => (x._1, x._2.length))).toList

fruit groupBy(k => k)
(fruit groupBy((k) => k) map ( x => (x._1, x._2.length))).toList