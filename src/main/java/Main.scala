import java.io.File
import scala.Array
import scala.collection.immutable.HashMap
import scala.collection.{mutable, immutable}
import scalax.io.JavaConverters._
import scalax.file.Path
import scalax.io.{Output, Resource}

/**
 * User: deathnik
 * Date: 9/26/13
 * Time: 12:54 AM
 */
object Main {
  val zero = int2Integer(0)

  def main(args: Array[String]) {
    val mainData = loadData("../train.tsv")
    val splitedData = util.Random.shuffle(mainData).splitAt((mainData.length * 1.0).toInt)
    var weightedSet = train(splitedData._1)
    var posScore = 0
    var negScore = 0
    var file = (new File("result.csv"))
    file.delete();
    val output:Output = Resource.fromFile("result.csv")
    val testDate = loadData("../test.tsv")
    output.write("urlid,label\n")
    for (line <- testDate) {
      output.write(line(1)+"," + classify(line, weightedSet)+"\n")
    }
  }

  def loadData(s: String): List[Array[String]] =
    (for (s <- scala.io.Source.fromFile(s).getLines()) yield s.split("\t").map(f => f.substring(1, f.length() - 1))).toList.tail

  def train(trainData: List[Array[String]]): (HashMap[String, mutable.HashMap[String, Int]], HashMap[String, mutable.HashMap[String, Int]]) = {
    var alchemeryCategorySet = mutable.Set[String]()
    for (line <- trainData) alchemeryCategorySet += line(3)
    var posClassif = immutable.HashMap[String, mutable.HashMap[String, Int]]()
    var negClassif = immutable.HashMap[String, mutable.HashMap[String, Int]]()
    for (category <- alchemeryCategorySet) {
      posClassif = posClassif + (category -> mutable.HashMap[String, Int]())
      negClassif = negClassif + (category -> mutable.HashMap[String, Int]())
    }

    var targetMap = mutable.HashMap[String, Int]()
    for (line <- trainData) {
      if (line(26).equals("1")) {
        targetMap = posClassif.get(line(3)).get
      } else {
        targetMap = negClassif.get(line(3)).get
      }
      for (word <- line(2).split("[ \":.,?-]").filter(s => s.length > 3)) {
        if (targetMap.get(word) == None) targetMap.+=(word -> 1)
        else targetMap.update(word, targetMap.get(word).get + 1)
      }
    }

    return (posClassif, negClassif)
  }

  def classify(line: Array[String], classify: (HashMap[String, mutable.HashMap[String, Int]], HashMap[String, mutable.HashMap[String, Int]])): String = {
    var scorePos = 0.0
    var scoreNeg = 0.0
    var count = 0
    var catClassify = (classify._1.get(line(3)).get, classify._2.get(line(3)).get)
    for (word <- line(2).split("[ \":.,?-]").filter(s => s.length > 3)) {
      if (catClassify._1.get(word) != None && catClassify._2.get(word) != None) {
        var div = (catClassify._1.get(word).get + catClassify._2.get(word).get) * (catClassify._1.get(word).get + catClassify._2.get(word).get)
        scorePos += catClassify._1.get(word).get/div
        scoreNeg += catClassify._2.get(word).get/div
      }
      else if (catClassify._1.get(word) == None && catClassify._2.get(word) == None) {}
      else if (catClassify._1.get(word) == None){
        scoreNeg += 1.0/( catClassify._2.get(word).get * catClassify._2.get(word).get)
      }
      else {
        scorePos +=  1.0/(catClassify._1.get(word).get  * catClassify._1.get(word).get)
      }

    }
    if ( scorePos > scoreNeg ) return "1"
    else return "0"
  }

}
