import java.io.File
import scala.Array
import scala.collection.immutable.HashMap
import scala.collection.{mutable, immutable}
import scalax.io.{Output, Resource}

/**
 * User: deathnik
 * Date: 9/26/13
 * Time: 12:54 AM
 */
object Main {
  val zero = int2Integer(0)
  var alchemeryCategorySet =mutable.Set[String]()
    var wordsRegexpr = "[^a-zA-Z]"
  def main(args: Array[String]) {
    val mainData = loadData("../train.tsv")
    for (line <- mainData) alchemeryCategorySet += line(3)
    /*for(i <- 4 to 25){
      println(i)
      getAverage(l => l(i).replace("?","-1").toDouble,l => l(3)=="health" && l(26)=="1",l => l(3)=="health" && l(26)=="0", mainData)
    }  */
    test(mainData)

    val file = new File("result.csv")
    file.delete()

    val weightedSet = train(mainData)
    //dumpSet(weightedSet)
    val output: Output = Resource.fromFile("result.csv")
    val testDate = loadData("../test.tsv")
    output.write("urlid,label\n")
    for (line <- testDate) {
      output.write(line(1) + "," + classify(line, weightedSet) + "\n")
    }
  }
  def dumpSet(weightedSet: (HashMap[String, mutable.HashMap[String, Int]], HashMap[String, mutable.HashMap[String, Int]])){
    val dump: Output = Resource.fromFile("../statistics/words");
    for(w <-weightedSet._1.iterator){
      dump.write("\n")
      dump.write(w._1+"\n")
      for(e<- w._2){
        dump.write(e._1+" "+"\n")
      }
    }
    dump.write("\n\n\nneg")
    for(w <-weightedSet._2.iterator){
      dump.write("\n")
      dump.write(w._1+"\n")
      for(e<- w._2){
        dump.write(e._1+" "+"\n")
      }
    }
  }

  def test(mainData: List[Array[String]]) {
    val splitedData = util.Random.shuffle(mainData).splitAt((mainData.length * 0.7).toInt)
    val weightedSet = train(splitedData._1)
    for(category <-alchemeryCategorySet) score("Score at "+category+":",splitedData._2.filter(s => s(3) == category),weightedSet)
    score("Overall score:",splitedData._2,weightedSet)
  }

  def score(testName: String, lines : List[Array[String]], weightedSet: (HashMap[String, mutable.HashMap[String, Int]], HashMap[String, mutable.HashMap[String, Int]])){
    var truePos = 0
    var falsePos = 0
    var trueNeg = 0
    var falseNeg = 0
    for (line <- lines) {
      if (line(26) == classify(line, weightedSet)) {
        if (line(26) == "1") truePos += 1
        else trueNeg += 1
      }
      else {
        if (line(26) == "0") falsePos += 1
        else falseNeg += 1
      }
    }
    println()
    println("Count:" + lines.length)
    println(testName+ 1.0 * (truePos + trueNeg) / (truePos + trueNeg + falseNeg + falsePos))
    println (trueNeg + " | " + falseNeg)
    println(falsePos + " | " + truePos)
  }

  def getStatistics(show:Boolean,posTest: Array[String]=> Boolean,negTest: Array[String]=> Boolean,data: List[Array[String]] ):Double={
      var pos = 0
      var neg = 0
    for(line<-data){
      if(posTest(line)) pos +=1
      else if(negTest(line)) neg+=1
    }
    if(show)println("pos:" + pos+ " neg:"+ neg)
    1.0 * pos/(pos+neg)
  }

  def getAverage(grabber: Array[String]=>Double,posTest: Array[String]=> Boolean,negTest: Array[String]=> Boolean,data: List[Array[String]]){
    var pos = 0.0
    var posCount = 0
    var neg = 0.0
    var negCount =0
    for(line<-data){
      if(posTest(line)){
        posCount +=1
        pos += grabber(line)
      }
      else if(negTest(line)){
        negCount+=1
        neg+= grabber(line)
      }
    }
    println("pos avrg:" + (pos/posCount))
    println("neg avrg:" + (neg/negCount))
  }

  def loadData(s: String): List[Array[String]] =
    (for (s <- scala.io.Source.fromFile(s).getLines()) yield s.split("\t").map(f => f.substring(1, f.length() - 1))).toList.tail

  def train(trainData: List[Array[String]]): (HashMap[String, mutable.HashMap[String, Int]], HashMap[String, mutable.HashMap[String, Int]]) = {
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
      for (word <- line(2).split(wordsRegexpr).filter(s => s.length > 3)) {
        if (targetMap.get(word) == None) targetMap.+=(word -> 1)
        else targetMap.update(word, targetMap.get(word).get + 1)
      }
    }
    return (posClassif, negClassif)
  }

  def classify(line: Array[String], classify: (HashMap[String, mutable.HashMap[String, Int]], HashMap[String, mutable.HashMap[String, Int]])): String = {
    var scorePos = 0.0
    var scoreNeg = 0.0
    val catClassify = (classify._1.get(line(3)).get, classify._2.get(line(3)).get)
    for (word <- line(2).split(wordsRegexpr).filter(s => s.length > 3)) {
      if (catClassify._1.get(word) != None && catClassify._2.get(word) != None) {
        val div = catClassify._1.get(word).get + catClassify._2.get(word).get  +1
        scorePos += catClassify._1.get(word).get / div
        scoreNeg += catClassify._2.get(word).get / div
      }
      else if (catClassify._1.get(word) == None && catClassify._2.get(word) == None) {}
      else if (catClassify._1.get(word) == None) {
        scoreNeg += 1.0 / (catClassify._2.get(word).get +1)
      }
      else {
        scorePos += 1.0 / (catClassify._1.get(word).get +1)
      }

    }
    if (scorePos > scoreNeg) return "1"
    else return "0"
  }

}
