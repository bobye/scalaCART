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
    val size = lines.length
        
    val experiments = for (i<- 0 until runs) yield {
    val shuffledLines = scala.util.Random.shuffle(lines.toList)   

    
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
    
    result
    }
    
    val mean = experiments.sum/(runs)
    val std  = scala.math.sqrt(experiments.map(x => (x-mean)*(x-mean)).sum / (runs-1))
   
    println()
    println("Test Accuracy: " + mean + "(+-" + std + ")")

    val allLines = scala.util.Random.shuffle(lines.toList).toIndexedSeq
    val data = (0 until numOfFeats).toList.map(i => allLines.map(f=>f(i)))
    val label= allLines.map(f=>f(numOfFeats).toInt)
    
    import java.io._    
    printToFile(new File("graph.dot")) { p => p.print(onecart.cTree.digraph(data, label))}

  }

}