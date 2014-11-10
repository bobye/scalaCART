package cart

/********************************************************************
 * 
 * Classification and Regression Tree (CART) 
 * Jianbo Ye <jxy198@ist.psu.edu> (c) 2014
 * 
 ********************************************************************/


class CARTx {
  var maxDepth = 20 // maximum length of cTree
  var D : List[IndexedSeq[Double]] = null
  var L : IndexedSeq[Int] = null
  var numOfLabels : Int = 0
  var cTree : Tree = null
  ////////////////////////////////////////////
  // Data Structure Of Classification Tree
  trait Tree {
    val numOfLeaves: Int
    def classify(x:List[Double]): Int
  }

  case class Leaf(val label: Int) extends Tree {
    val numOfLeaves = 1
    def classify(x:List[Double]) = label
  }
  
  case class Branch (val left: Tree, val right: Tree)
  	(val index:Int, val cutoff: Double) extends Tree  {
    val numOfLeaves = left.numOfLeaves + right.numOfLeaves
    def classify(x:List[Double]) = if (x(index) <= cutoff) left.classify(x) else right.classify(x)
  }
  ////////////////////////////////////////////
  // Impurity Functions
  private def Gini[C<: IndexedSeq[Int]](x:C): Double = {
    val counts = x.sum
    1 - (x.map(x => x*x).sum / (counts * counts).toDouble)
  }
  ////////////////////////////////////////////
  // Splitting Method
  private def percentage(I: IndexedSeq[Int]): Double = {
    (0 until numOfLabels).map(i=>I.filter(L(_) == i).length).max / I.length.toDouble
  }  
  private def majorityVote(I: IndexedSeq[Int]): Int = {
    (0 until numOfLabels).map(i=>i -> I.filter(L(_) == i).length).maxBy(_._2)._1
  }
  private def impuritySplit(data: IndexedSeq[(Int, Double)], 
                            func: IndexedSeq[Int] => Double = Gini): (Int, (Double, Double)) = {
    assert(L != null && data.length > 0)
    var leftBins = collection.mutable.IndexedSeq[Int](0,numOfLabels)
    var rightBins = collection.mutable.IndexedSeq[Int](0,numOfLabels)
    var Label = data.map(x => L(x._1))
    val n = Label.length

    for (i<- 0 until n) {
      rightBins(Label(i)) = rightBins(Label(i)) + 1
    }
    val nosplitScore = func(rightBins)
    
    val possibleSplits = (for (i<- 1 until n) yield {
      leftBins(Label(i-1)) = leftBins(Label(i-1)) + 1
      rightBins(Label(i-1)) = rightBins(Label(i-1)) - 1
      (i, nosplitScore - (func(leftBins) * (i) + func(rightBins) * (n-i)) / n.toDouble)
    }).filter(x => data(x._1)._2 > data(x._1 -1)._2 )
    if (possibleSplits.length > 0) {
      val theSplit = possibleSplits.maxBy(_._2)
      (theSplit._1, (theSplit._2, data(theSplit._1-1)._2))
    } else {
      (1, (0, data(0)._2))
    }    
  }
  
  private def splitDouble(I: IndexedSeq[Int], index: Int): ((Double /*score of split*/, Double /*cutoff*/), 
      (IndexedSeq[Int], IndexedSeq[Int])/*splitted index*/) = {
    assert(I.length > 0)
    val data = I.map(i=> i -> D(index)(i)).sortBy(_._2)
    val n = data.length
    
    val theSplit = impuritySplit(data, Gini)
    val cutoffIndex = theSplit._1
    (theSplit._2, data.map(_._1).splitAt(cutoffIndex))
  }
  
  // Stopping Criterion
  private def stop(assignment: IndexedSeq[Int]): Boolean = {
    val threshold = 0.01
    val mLabel = majorityVote(assignment.map(L(_)))
    assignment.filter(mLabel == L(_)).length > (1-threshold)*assignment.length
  }

  private def split(assignment: IndexedSeq[Int], depth: Int): Tree = {
    assert(assignment.length > 0)
    if (assignment.length == 1 || depth >= maxDepth || stop(assignment)) {    
      Leaf(majorityVote(assignment))
    } else {
      // find the best splits among all possible features
      val selectedSplit = (0 until D.length).map(featIndex 
          => (featIndex, splitDouble(assignment, featIndex))).maxBy(_._2._1._1)
      val goodness = selectedSplit._2._1._1
      
      if (goodness == 0) { // goodness of split does not improve
        Leaf(majorityVote(assignment))
      } else {
        val index = selectedSplit._1;       
        val cutoff = selectedSplit._2._1._2
        val leftIndex = selectedSplit._2._2._1;
        val rightIndex= selectedSplit._2._2._2;
      
        // recursion
        val leftTree  = split(leftIndex, depth+1)
        val rightTree = split(rightIndex, depth+1)
      
        Branch(leftTree, rightTree)(index, cutoff)        
      }
    }
  } 
  
  ////////////////////////////////////////////
  // training and testing facilities
  def train(data: List[IndexedSeq[Double]], label:IndexedSeq[Int], maxDepth: Int = 20): Unit = {
	assert (data.length > 0 && data(0).length == label.length)
    D = data
    L = label
    numOfLabels = L.max+1 //label starts from 0
    val featureDim = D.length  
    val sampleSize = D(0).length
    
    cTree = split(0 until sampleSize, 0)
    println("Total number of leaf nodes: " + cTree.numOfLeaves)
  }
  
  def test(data: List[IndexedSeq[Double]], label: IndexedSeq[Int]): Unit = {
    assert (data.length > 0 && data(0).length == label.length)
    assert (cTree != null)
    println("The Accuracy on Test Set: " + ((0 until label.length).map(i => 
      cTree.classify(data.map(feat => feat(i))) == label(i))
      .filter(identity).length) / label.length.toDouble) // print accuracy
  }
}