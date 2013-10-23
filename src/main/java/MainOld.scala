import java.io.File
import scala.Array
import scala.collection.immutable.HashMap
import scala.collection.{mutable, immutable}
import scalax.io.{Output, Resource}
import org.json4s._
import org.json4s.native.JsonMethods._
import classifiers.BranchOfCategoryKnns
/**
 * User: deathnik
 * Date: 9/26/13
 * Time: 12:54 AM
 */
object MainOld {
  val zero = int2Integer(0)
  var alchemeryCategorySet = mutable.Set[String]()
  val branchKnns = new BranchOfCategoryKnns()

  def splitingFunction(text: String): Array[String] = text.toLowerCase().split("[^a-zA-Z-]").filter(s => s.length > 2)


  def mainOld(args: Array[String]) {
    val mainData = loadData("../train.tsv")
    val testDate = loadData("../test.tsv")
    for (line <- mainData) alchemeryCategorySet += line(3)

    /*for(cat <- alchemeryCategorySet){
      println(cat)
      getStatistics(true,line =>line(3)==cat && line(17)=="1"   && line(26) == "1",  line=>line(3)==cat && line(17)=="1" && line(26) == "0", mainData)
      getStatistics(true,line =>line(3)==cat ,  line=>line(3)==cat && line(17)=="1", testDate)

    }  */

    /*for(i <- 4 to 25){
      println(i)
      getAverage(l => l(i).replace("?","-1").toDouble,l => l(3)=="health" && l(26)=="1",l => l(3)=="health" && l(26)=="0", mainData)
    }  */
    //val indexes = List(5,10,13,15,16,19,24,25)
    val ind = List(10, 13, 15, 16, 19, 24)
    branchKnns.build(mainData, ind,-10.0,alchemeryCategorySet)
    test(mainData, s => true)

    val file = new File("result.csv")
    file.delete()


    val weightedSet = train(mainData)
    //dumpSet(weightedSet)
    val output: Output = Resource.fromFile("result.csv")
    output.write("urlid,label\n")
    for (line <- testDate) {
      var res = branchKnns.advancePrediction(line,0.5)
      val bayesRes = classify(line, weightedSet)
      res =  (res * bayesRes) / ( res * bayesRes + (1.0 - res)* (1-bayesRes))
      output.write(line(1) + "," + (res*1000).round/1000.0 + "\n")
    }
  }


  def dumpSet(weightedSet: (HashMap[String, mutable.HashMap[String, Int]], HashMap[String, mutable.HashMap[String, Int]])) {
    val dump: Output = Resource.fromFile("../statistics/words")
    for (w <- weightedSet._1.iterator) {
      dump.write("\n")
      dump.write(w._1 + "\n")
      for (e <- w._2) {
        dump.write(e._1 + " " + "\n")
      }
    }
    dump.write("\n\n\nneg")
    for (w <- weightedSet._2.iterator) {
      dump.write("\n")
      dump.write(w._1 + "\n")
      for (e <- w._2) {
        dump.write(e._1 + " " + "\n")
      }
    }
  }

  def test(mainData: List[Array[String]], testFilter: Array[String] => Boolean) {
    var splitedData = util.Random.shuffle(mainData).splitAt((mainData.length * 0.7).toInt)
    val weightedSet = train(splitedData._1)
    splitedData = (splitedData._1, splitedData._2.filter(s => testFilter(s)))
    for (category <- alchemeryCategorySet) score("Score at " + category + ":", splitedData._2.filter(s => s(3) == category), weightedSet)
    score("Overall score:", splitedData._2, weightedSet)
  }

  def score(testName: String, lines: List[Array[String]], weightedSet: (HashMap[String, mutable.HashMap[String, Int]], HashMap[String, mutable.HashMap[String, Int]])) {
    var truePos = 0
    var falsePos = 0
    var trueNeg = 0
    var falseNeg = 0
    for (line <- lines) {
      var prediction = classify(line, weightedSet)
      val branchPred = branchKnns.advancePrediction(line,0.5)
      prediction = (prediction * branchPred) /((prediction * branchPred)+ (1.0- prediction) * (1.0-branchPred))
      if (line(26) == ( if(prediction<0.5) "0" else "1")) {
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
    println(testName + 1.0 * (truePos + trueNeg) / (truePos + trueNeg + falseNeg + falsePos))
    println(trueNeg + " | " + falseNeg)
    println(falsePos + " | " + truePos)
  }

  def getStatistics(show: Boolean, posTest: Array[String] => Boolean, negTest: Array[String] => Boolean, data: List[Array[String]]): Double = {
    var pos = 0
    var neg = 0
    for (line <- data) {
      if (posTest(line)) {
        pos += 1
      }
      else if (negTest(line)) {
        neg += 1
      }
    }
    if (show) println("pos:" + pos + " neg:" + neg)
    1.0 * pos / (pos + neg)
  }

  def getAverage(grabber: Array[String] => Double, posTest: Array[String] => Boolean, negTest: Array[String] => Boolean, data: List[Array[String]]) {
    var pos = 0.0
    var posCount = 0
    var neg = 0.0
    var negCount = 0
    for (line <- data) {
      if (posTest(line)) {
        posCount += 1
        pos += grabber(line)
      }
      else if (negTest(line)) {
        negCount += 1
        neg += grabber(line)
      }
    }
    println("pos avrg:" + (pos / posCount))
    println("neg avrg:" + (neg / negCount))
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
      for (word <- splitingFunction(line(2)).toSet[String]) {
        if (targetMap.get(word) == None) targetMap.+=(word -> 1)
        else targetMap.update(word, targetMap.get(word).get + 1)
      }
    }
    (posClassif, negClassif)
  }

  def classify(line: Array[String], classify: (HashMap[String, mutable.HashMap[String, Int]], HashMap[String, mutable.HashMap[String, Int]])): Double = {
    var scorePos = 0.0
    var scoreNeg = 0.0
    val words = splitingFunction(line(2))
    val occur = words.groupBy((word: String) => word).mapValues(_.length)
    /*var character = 1.0 * words.size / occur.size
    if(occur.size < 20){
      println(line(26) + "  " + occur.size+" " + line(2))
      for(w <- occur){
        println(w._1 + " "+w._2)
      }
    } */
    val catClassify = (classify._1.get(line(3)).get, classify._2.get(line(3)).get)
    for (w <- occur) {
      val word = w._1
      if (catClassify._1.get(word) != None && catClassify._2.get(word) != None) {
        val div = catClassify._1.get(word).get + catClassify._2.get(word).get + 1
        scorePos += catClassify._1.get(word).get / div
        scoreNeg += catClassify._2.get(word).get / div
      }
      else if (catClassify._1.get(word) == None && catClassify._2.get(word) == None) {}
      else if (catClassify._1.get(word) == None) {
        scoreNeg += 1.0 / (catClassify._2.get(word).get + 1)
      }
      else {
        scorePos += 1.0 / (catClassify._1.get(word).get + 1)
      }

    }
    1.0*(scorePos+1)/(scorePos+scoreNeg+2)
  }


  def cleanAndSplitToArray(text: String): Array[String] = {
    val json = parse(text.replace("\"\"", "\""))
    for {
      JObject(child) <- json
      JField("title", JString(body)) <- child
    } return splitingFunction(body)
    //if no body
    Array[String]()
  }

}
