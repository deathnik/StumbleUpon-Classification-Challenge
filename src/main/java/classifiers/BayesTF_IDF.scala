package classifiers

import scala.collection.mutable
import org.apache.mahout.classifier.evaluation.Auc

class BayesTF_IDF(alchemyCategorySet: mutable.Set[String], splittingFunction: String => Array[String]) {
  val wordsToTake = 24
  var sizes = mutable.HashMap.empty[String, Int]
  val collections = mutable.HashMap[String, mutable.HashMap[String, Int]]()



  def train(data: List[Array[String]]) {
    prepareCollections(data, sizes)
  }

  def prepareCollections(data: List[Array[String]], sizes: mutable.HashMap[String, Int]) {
    for (cat <- alchemyCategorySet) {
      sizes += (cat -> 0)
      collections += ("0" + cat -> mutable.HashMap[String, Int]())
      collections += ("1" + cat -> mutable.HashMap[String, Int]())
    }

    for (line <- data) {
      val targetMap = collections.get(line(26) + line(3)).get
      for (word <- splittingFunction(line(2)).toSet[String]) {
        if (targetMap.get(word) == None) targetMap.+=(word -> 1)
        else targetMap.update(word, targetMap.get(word).get + 1)
      }
      sizes.update(line(3), sizes.get(line(3)).get + 1)
    }
  }


  // ._1 - positive, ._2 -negative
  def getCounts(w:String,category:String):(Int,Int)=
    (collections.get("1" + category).get.get(w), collections.get("0" + category).get.get(w))  match  {
      case (None,None)  => (0,0)
      case (x,None) => (x.get, 0)
      case (None,y) => (0,y.get)
      case (x,y) => (x.get, y.get)
    }


  def classify(line: Array[String]): Double = {
    var posScore = 0.000000001
    var negScore = 0.000000001
    val words = splittingFunction(line(2))
    val occur = countWords(words)
    val weightedWordSet = mutable.HashMap.empty[String,Double]
    for (w: String <- words.toSet.seq) {
      val TF = 1.0 * occur.get(w).get / words.length
      val counts = getCounts(w,line(3))
      if (counts._1 + counts._2 >= 1) {
        val IDF = Math.log(sizes(line(3)) / (counts._1 + counts._2))
        if(weightedWordSet.get(w)== None )  weightedWordSet += (w -> TF*IDF)
        else weightedWordSet.update(w, weightedWordSet.get(w).get+TF*IDF)
      }
    }

    val array = weightedWordSet.toArray.sortBy(_._2).reverse.take(wordsToTake)

    for(wordWeightPair <- array){
      val counts = getCounts(wordWeightPair._1,line(3))
      posScore += wordWeightPair._2 * counts._1 / (counts._1 + counts._2)
      negScore += wordWeightPair._2 * counts._2 / (counts._1 + counts._2)
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
    val auc = new Auc()
    for(line <- data){
      val prediction = classify(line)
      auc.add(line(26).toInt, prediction)
    }

    if(data.size > 0)
      println(testName + "  " + auc.auc())

  }

}
