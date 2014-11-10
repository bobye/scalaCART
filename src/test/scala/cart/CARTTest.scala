package cart

object CARTTest {
  def main(args: Array[String]): Unit = {
    /////////////////////////////////////////
    // Load DataSet
    val source =  scala.io.Source.fromFile("spambase.data")
    val lines = source.getLines.toIndexedSeq.map(_.split(",").map(_.toDouble))
    source.close()
    
    val numOfSamples = lines.length
    val numOfFeats   = lines(0).length - 1
    
    val shuffledLines = scala.util.Random.shuffle(lines.toList)   
    val size = lines.length
    
    /////////////////////////////////////////
    // To separate training and testing set
    val trainSize = (0.8 * size).toInt
    
    val trainLines = (0 until trainSize).map(shuffledLines(_)).toIndexedSeq
    val testLines = (trainSize until size).map(shuffledLines(_)).toIndexedSeq
    
    val trainData = (0 until numOfFeats).toList.map(i => trainLines.map(f=>f(i)))
    val trainLabel= trainLines.map(f=>f(numOfFeats).toInt)

    val testData = (0 until numOfFeats).toList.map(i => testLines.map(f=>f(i)))
    val testLabel= testLines.map(f=>f(numOfFeats).toInt)
    
    ///////////////////////////////////////////
    // Learning a CART and test its accuracy
    val cart = new CARTx()
    cart.train(trainData, trainLabel)
    cart.test(testData, testLabel)
  }

}