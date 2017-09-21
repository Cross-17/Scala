val list = List(1,1,2,3,4,5)
val m = list.groupBy(x => x + 1 ).map( x => (x._1,x._2.size)).toList
m(4)