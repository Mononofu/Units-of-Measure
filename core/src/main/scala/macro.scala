package macros

import macroimpl._
import MeasureImpl.{u, u_i}
import units._

object Main extends App {
  val a = u(2, "m")
  val b = u(3, "m")
  val h = u(4, "s")
  val d = u(5, "m*s")
  val e = u(10, "m*s/s")
  val f = u(32, "m*s*s")
  val g = new Measure[CUnit[SUnit[Meter, Pos1], SUnit[Second, Pos2]], Int](10)

  val test: u_i("m*s") = u(10, "m*s")

  println("\n==== raw numbers")
  println("a: " + a)
  println("automatic simplification:")
  println("u(10, 'm*s/s'): " + u(10, "m*s/s"))

  println("\n==== addition")
  val tmp: u_i("m") = a + b
  println(s"$a + $b: " + (a + b))
  println("ugly manually defined unit is reduced too:")
  println(s"$g + $f: " + (g.asInt + f) )


  // type checker catches unit mismatches
  //println(d + e)
  // u(5, "m") + u(4, "s")


  println("\n==== multiplication")
  println(s"$b * $h: ${b * h}")
  println(s"$e * $h: ${e * h}")

  println("10 m/s * 32 s: " + u(10, "m/s") * u(32, "s"))

  println("\n==== combined")
  println(s"$d + ($b * $h): ${d + (b * h)}")
  println(s"$d + ($b * $h): ${d + (b * h)}")

  val result: u_i("m*s") = b * h

  println("\n==== access units with macro trickery")
  println(s"${b.unit}")
  println(s"${h.unit}")
  println(s"${d.unit}")
  println(s"${e.unit}")
  println(s"${f.unit}")

  // type error
  // println("e + (b * h): " + (e + (b * h)))

  val numRuns = 40
  val runSize = 1000000

  val benchmarks = List(
    new BenchIntFlat(),
    new BenchMeasure(),
    new BenchMeasureIntMacro(),
    new BenchDoubleFlat(),
    new BenchMeasureDoubleMacro()
    )

  for(benchmark <- benchmarks) {
    var addTime = List[Double]()
    var mulTime = List[Double]()

    for(i <- 1 to 30) {
      val (add, mul) = benchmark.bench(numRuns, runSize, true)
      addTime ::= add.sum * 1.0 / add.length
      mulTime ::= mul.sum * 1.0 / mul.length
    }

    addTime = addTime.sorted
    mulTime = mulTime.sorted

    val addTimeMean = addTime.sum / addTime.length
    val mulTimeMean = mulTime.sum / mulTime.length

    val addTimeVar = addTime.map(n => Math.pow(Math.abs(addTimeMean - n), 2)).sum / (addTime.length - 1)
    val mulTimeVar = mulTime.map(n => Math.pow(Math.abs(mulTimeMean - n), 2)).sum / (mulTime.length - 1)

    // 0.975 quantile of the t-distribution for 1 to 100 degrees of freedom
    // generated in R with: qt(0.975, seq(100))
    val t = List(0, 12.706205, 4.302653, 3.182446, 2.776445, 2.570582, 2.446912, 2.364624,
      2.306004, 2.262157, 2.228139, 2.200985, 2.178813, 2.160369, 2.144787,
      2.131450, 2.119905, 2.109816, 2.100922, 2.093024, 2.085963, 2.079614,
      2.073873, 2.068658, 2.063899, 2.059539, 2.055529, 2.051831, 2.048407,
      2.045230, 2.042272, 2.039513, 2.036933, 2.034515, 2.032245, 2.030108,
      2.028094, 2.026192, 2.024394, 2.022691, 2.021075, 2.019541, 2.018082,
      2.016692, 2.015368, 2.014103, 2.012896, 2.011741, 2.010635, 2.009575,
      2.008559, 2.007584, 2.006647, 2.005746, 2.004879, 2.004045, 2.003241,
      2.002465, 2.001717, 2.000995, 2.000298, 1.999624, 1.998972, 1.998341,
      1.997730, 1.997138, 1.996564, 1.996008, 1.995469, 1.994945, 1.994437,
      1.993943, 1.993464, 1.992997, 1.992543, 1.992102, 1.991673, 1.991254,
      1.990847, 1.990450, 1.990063, 1.989686, 1.989319, 1.988960, 1.988610,
      1.988268, 1.987934, 1.987608, 1.987290, 1.986979, 1.986675, 1.986377,
      1.986086, 1.985802, 1.985523, 1.985251, 1.984984, 1.984723, 1.984467,
      1.984217, 1.983972)

    val addTimeIntervalSize = addTimeVar / Math.sqrt(addTime.length) * t(addTime.length - 1)
    val mulTimeIntervalSize = mulTimeVar / Math.sqrt(mulTime.length) * t(mulTime.length - 1)

    println(s"\tBenchmarked ${benchmark.getClass}")
    println(f"${addTime.min}%5.2f - ${addTime((addTime.length * 0.25).toInt)}%5.2f - ${addTime(addTime.length / 2)}%5.2f - ${addTime((addTime.length * 0.75).toInt)}%5.2f - ${addTime.max}%5.2f\t$addTimeMean%5.2f")
    println(f"[${addTimeMean - addTimeIntervalSize}%5.2f, ${addTimeMean + addTimeIntervalSize}%5.2f]")
    println(f"${mulTime.min}%5.2f - ${mulTime((mulTime.length * 0.25).toInt)}%5.2f - ${mulTime(mulTime.length / 2)}%5.2f - ${mulTime((mulTime.length * 0.75).toInt)}%5.2f - ${mulTime.max}%5.2f\t$mulTimeMean%5.2f")
    println(f"[${mulTimeMean - mulTimeIntervalSize}%5.2f, ${mulTimeMean + mulTimeIntervalSize}%5.2f]")
    println(addTime.mkString("[", ", ", "]"))
    println(mulTime.mkString("[", ", ", "]"))
    Utility.gc()
  }
}
