/**
 * User: deathnik
 * Date: 10/16/13
 * Time: 10:03 AM
 */
package classifiers

import scala.collection.mutable
import scala.Array

class Knn {
  var classif = Vector.empty[(Double, Boolean)]
  var mapper: (Array[String]) => Double = x => 0.0d

  def build(data: List[Array[String]], mapper: Array[String] => Double) {
    classif = (for (l <- data) yield (mapper(l), l(l.length - 1).equals("1"))).toVector
    this.mapper = mapper
  }

  def addPair(x: (Double, Boolean)) {
    classif = x +: classif
  }

  def addMapper(mapper: Array[String] => Double) {
    this.mapper = mapper
  }

  def normalize() {
    classif = classif.sortBy(e1 => e1._1)
  }

  def classify(proection: Double): Double = {
    //var proection = mapper(line)
    var i = 0
    while (i < classif.length && classif(i)._1 < proection) i = i + 1
    var j = i
    while (j < classif.length && classif(j)._1 == proection) j = j + 1
    var index = i + (j / 2)
    if (index - 5 < 0) index = 0
    else index -= 5
    if ((index + 10) >= classif.length) index = classif.length - 11
    if (index < 0) return 0.5
    var isGood = 0
    for (k <- 0 to 9) {
      if (classif(index + k)._2) isGood = isGood + 1
    }
    isGood / 10.0
  }
}

class BranchOfCategoryKnns {
  var branch = mutable.HashMap.empty[String, Vector[Knn]]
  var mappers = Array.empty[Array[String] => Double]

  def build(data: List[Array[String]], indexes: List[Int], replaceValue: Double, alchemeryCategorySet: mutable.Set[String]) {
    mappers = (for (i <- indexes) yield KnnHelper.createSimpleMapperWithReplace(i, replaceValue)).toArray
    for (s <- alchemeryCategorySet) {
      branch += (s -> (for (i <- indexes) yield new Knn).toVector)
    }
    for (line <- data) {
      val vectKnn = branch.get(line(3))
      for (i <- 0 to indexes.length - 1) {
        vectKnn.get(i).addPair(mappers(i).apply(line), line(line.length - 1).equals("1"))
      }
    }
    for (vKnn <- branch.values;
         i <- 0 to vKnn.length - 1) {
      vKnn(i).normalize()
      vKnn(i).addMapper(mappers(i))
    }
  }

  def advancePrediction(line: Array[String], prev: Double): Double = {
    var result = prev
    for (vect <- branch.get(line(3)).get) {
      val pred = vect.classify(vect.mapper(line))
      if (pred != 0 && pred != 1) result = (result * pred) / (result * pred + (1.0 - result) * (1.0 - pred))
    }
    result
  }

}

object KnnHelper {
  def createSimpleMapperWithReplace(index: Int, replaceValue: Double) = {
    (line: Array[String]) => {
      if (line(index).equals("?"))
        replaceValue
      else line(index).toDouble
    }
  }

}
