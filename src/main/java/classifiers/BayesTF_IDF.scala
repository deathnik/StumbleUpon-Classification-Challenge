package classifiers

import scala.collection.mutable

class BayesTF_IDF(_alchemeryCategorySet: mutable.Set[String], _splitingFunction: String => Array[String]) {
  var alchemeryCategorySet = _alchemeryCategorySet
  var splitingFunction: String => Array[String] = _splitingFunction

  var sizes = mutable.HashMap.empty[String, Int]
  val collections = mutable.HashMap[String, mutable.HashMap[String, Int]]()


  def train(data: List[Array[String]]) {
    prepareCollections(data, sizes)
  }

  def prepareCollections(data: List[Array[String]], sizes: mutable.HashMap[String, Int]) {
    for (cat <- alchemeryCategorySet) {
      sizes += (cat -> 0)
      collections += ("0" + cat -> mutable.HashMap[String, Int]())
      collections += ("1" + cat -> mutable.HashMap[String, Int]())
    }

   for (line <- data) {
      val targetMap = collections.get(line(26) + line(3)).get
      for (word <- splitingFunction(line(2)).toSet[String]) {
        if (targetMap.get(word) == None) targetMap.+=(word -> 1)
        else targetMap.update(word, targetMap.get(word).get + 1)
      }
      sizes.update(line(3), sizes.get(line(3)).get +1)
    }
  }

  def classify(line: Array[String]): Double = {
    var posScore = 0.000000001
    var negScore = 0.000000001
    val words = _splitingFunction(line(2))
    val size = words.length
    val occur = countWords(words)
    for (w: String <- words.toSet.seq) {
      val TF = 1.0 * occur.get(w).get / size
      var posOccur = 0
      var negOccur = 0
      if (collections.get("1" + line(3)).get.get(w) != None) posOccur = collections.get("1" + line(3)).get.get(w).get
      if (collections.get("0" + line(3)).get.get(w) != None) negOccur = collections.get("0" + line(3)).get.get(w).get
      if (negOccur + posOccur >= 1) {
        val IDF = Math.log(sizes(line(3)) / (negOccur + posOccur))
        posScore += TF * IDF * posOccur / (posOccur + negOccur)
        negScore += TF * IDF * negOccur / (posOccur + negOccur)
      }
    }
    posScore / (posScore + negScore)
  }

  def countWords(words: Array[String]): mutable.HashMap[String, Int] = {
    var result = mutable.HashMap[String, Int]()
    for (word <- words) {
      if (result.get(word) == None) result.+=(word -> 1)
      else result.update(word, result.get(word).get + 1)
    }
    result
  }

  def testScoreOnDataset(data: List[Array[String]], testName: String) {
    var truePos = 0
    var falsePos = 0
    var trueNeg = 0
    var falseNeg = 0
    for (line <- data) {
      val prediction = classify(line)
      if (line(26) == (if (prediction < 0.5) "0" else "1")) {
        if (line(26) == "1") truePos += 1
        else trueNeg += 1
      }
      else {
        if (line(26) == "0") falsePos += 1
        else falseNeg += 1
      }
    }
    println()
    println("Count:" + data.length)
    println(testName + 1.0 * (truePos + trueNeg) / (truePos + trueNeg + falseNeg + falsePos))
    println(trueNeg + " | " + falseNeg)
    println(falsePos + " | " + truePos)
  }

}
