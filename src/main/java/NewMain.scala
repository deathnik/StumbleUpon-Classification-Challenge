import java.io.File
import scala.collection.{immutable, mutable}
import classifiers.BayesTF_IDF
import scalax.io.{Resource, Output}

/**
 * Created with IntelliJ IDEA.
 * User: deathnik
 * Date: 10/23/13
 * Time: 9:19 AM
 * To change this template use File | Settings | File Templates.
 */
object NewMain {
  var alchemeryCategorySet = mutable.Set[String]()

  def splitingFunction(text: String): Array[String] = text.toLowerCase().split("[^a-zA-Z-]").filter(s => s.length > 2)

  def main(args: Array[String]) {
    val data = loadData("../train.tsv")
    extractCategories(data)

    testClassifier(data, s => true)
    val classif =new  BayesTF_IDF(alchemeryCategorySet, s => splitingFunction(s))

    classifyAndOutputResult(data)
  }

  def loadData(s: String): List[Array[String]] =
    (for (s <- scala.io.Source.fromFile(s).getLines()) yield s.split("\t").map(f => f.substring(1, f.length() - 1))).toList.tail

  def extractCategories(data: List[Array[String]]) {
    for (line <- data) alchemeryCategorySet += line(3)
  }

  def testClassifier(mainData: List[Array[String]], testFilter: Array[String] => Boolean) {
    mainData.filter(testFilter)
    var splitedData = util.Random.shuffle(mainData).splitAt((mainData.length * 0.7).toInt)
    val classif =new  BayesTF_IDF(alchemeryCategorySet, s => splitingFunction(s))
    classif.train(splitedData._1)
    classif.testScoreOnDataset(splitedData._2,"Control Test: ")
    perfomCategoryTests(classif,splitedData._2)
  }

  def perfomCategoryTests(classif: BayesTF_IDF,data : List[Array[String]]){
    println()
    println("Category Tests: ")
    for(c<- alchemeryCategorySet) classif.testScoreOnDataset(data.filter( line => line(3) == c),c+" ")
  }

  def classifyAndOutputResult(trainData: List[Array[String]]){
    val file = new File("result.csv")
    file.delete()

    val output: Output = Resource.fromFile("result.csv")
    output.write("urlid,label\n")

    val classif =new  BayesTF_IDF(alchemeryCategorySet, s => splitingFunction(s))
    classif.train(trainData)

    val testDate = loadData("../test.tsv")
    for (line <- testDate) {
      var res = classif.classify(line)
      output.write(line(1) + "," + (res*1000).round/1000.0 + "\n")
    }
  }




}
