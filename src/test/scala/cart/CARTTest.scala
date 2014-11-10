package cart

object CARTTest {
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }  
  def main(args: Array[String]): Unit = {
    /////////////////////////////////////////
    // Load DataSet
    val source =  scala.io.Source.fromFile("spambase.data")
    val lines = source.getLines.toIndexedSeq.map(_.split(",").map(_.toDouble))
    source.close()
    
    val numOfSamples = lines.length
    val numOfFeats   = lines(0).length - 1
    
    val runs = 20
    val onecart = new CARTx()
    val experiments = for (i<- 0 until runs) yield {
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
    
    onecart.train(trainData, trainLabel)
    val result = onecart.test(testData, testLabel)
    
    /* Comment This Part For Drawing Digraph of Trees
     * 
    import java.io._    
    printToFile(new File("graphTrain.dot")) { p => p.print(onecart.cTree.digraph(trainData, trainLabel))}
    printToFile(new File("graphTest.dot")) { p => p.print(onecart.cTree.digraph(testData, testLabel))}    
    if (onecart.cTree.numOfLeaves <= 15) exit(0)
    * 
    */
    result
    }
    
    val mean = experiments.sum/(runs)
    val std  = scala.math.sqrt(experiments.map(x => (x-mean)*(x-mean)).sum / (runs-1))
   
    println()
    println("Test Accuracy: " + mean + "(+-" + std + ")")
    

    
  }

}