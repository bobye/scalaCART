package cart

/********************************************************************
 * 
 * Classification and Regression Tree (CART) 
 * Jianbo Ye <jxy198@ist.psu.edu> (c) 2014
 * 
 ********************************************************************/


class CARTx {
  var maxDepth = 10 // maximum length of cTree
  var D : List[IndexedSeq[Double]] = null
  var L : IndexedSeq[Int] = null
  var numOfLabels : Int = 0
  var featureDim: Int = 0
  var sampleSize: Int = 0
  var cTree : Tree = null
  ////////////////////////////////////////////
  // Data Structure Of Classification Tree
  //
  // Note: To allow efficient pruning, the tree structure is mutable
  trait Tree {
    var numOfLeaves: Int
    val label: Int
    var r: Double // resubstitution of node
    var R: Double // resubstitution of tree
    val parent: Branch
    def clear(): Unit
    def classify(x:List[Double]): Int
    def weakestLink(): (Double, Tree)
    def resubstitution(l: IndexedSeq[Int]): Unit
    
    var counts: Int = 0
    def dotGraph(): String
    def digraph(data: List[IndexedSeq[Double]], label: IndexedSeq[Int]): String = {
      D = data; L = label; sampleSize = L.length
      cTree.clear(); accuracy(data, label); resubstitution(0 until sampleSize);
      "digraph G {\n" + dotGraph() + "}\n"
    }
    override def hashCode(): Int = (super.hashCode()+numOfLeaves+label).abs.toInt
  }

  case class Leaf(val parent: Branch)(val label: Int, var r: Double) extends Tree {
    var numOfLeaves = 1
    var R = r
    def clear() = {counts = 0}
    def classify(x:List[Double]) = {counts = counts +1; label}
    def weakestLink() = (1E10, this)
    def dotGraph() = "node"+hashCode() + " [label=\""+label+"(" + counts + ")\", shape=box, style=filled, fillcolor=\"0.0 " + (r) + " " + (1-r) + "\"]\n"
    def resubstitution(l: IndexedSeq[Int]) = {
      r = reSubstitution(l); R = r;
    }
  }
  
  case class Branch (val parent: Branch)(var left: Tree, var right: Tree)(val label: Int, var r: Double, 
      val index:Int, val cutoff: Double) extends Tree  {
    var numOfLeaves = 1
    var R = r
    def renew() = {
      numOfLeaves = left.numOfLeaves + right.numOfLeaves
      R = left.R + right.R
    }
    def clear() = {counts = 0; left.clear(); right.clear();}
    def classify(x:List[Double]) = {
      counts = counts + 1
      if (x(index) <= cutoff) left.classify(x) else right.classify(x)
    }
    def weakestLink() = List(left.weakestLink(), right.weakestLink(), 
        ((r - R)/(numOfLeaves -1), this)).minBy(_._1)
    def dotGraph(): String = {
      left.dotGraph() + right.dotGraph() +
      "node"+hashCode() + " [label=\"x"+(index+1)+" <= " + cutoff + "?(" + counts +")\", style=filled, fillcolor=\"0.0 " + (r) + " " + (1-r) + "\"]\n" +
      "node"+hashCode()+ " -> node" + left.hashCode() + " [label=\"yes\"]\n node"+ hashCode() + " -> node" + right.hashCode() + "[label=\"no\"]\n"
    }
    def resubstitution(l: IndexedSeq[Int]) = {
      left.resubstitution(l.filter(D(index)(_) <= cutoff))
      right.resubstitution(l.filter(D(index)(_) > cutoff))
      r = reSubstitution(l); R = left.R + right.R;
    }    
  }
  ////////////////////////////////////////////
  // Impurity Functions
  private def Gini[C<: IndexedSeq[Int]](x:C): Double = {
    val counts = x.sum
    1 - (x.map(x => x*x).sum / (counts * counts).toDouble)
  }
  ////////////////////////////////////////////
  // Splitting Method
  private def reSubstitution(I: IndexedSeq[Int]): Double = {
    (I.length - (0 until numOfLabels).map(i=>I.filter(L(_) == i).length).max) / sampleSize.toDouble 
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

  private def split(assignment: IndexedSeq[Int], depth: Int, parent: Branch): Tree = {
    assert(assignment.length > 0)
    if (assignment.length == 1 || depth >= maxDepth || stop(assignment)) {    
      Leaf(parent)(majorityVote(assignment), reSubstitution(assignment))
    } else {
      // find the best splits among all possible features
      val selectedSplit = (0 until D.length).map(featIndex 
          => (featIndex, splitDouble(assignment, featIndex))).maxBy(_._2._1._1)
      val goodness = selectedSplit._2._1._1
      
      if (goodness == 0) { // goodness of split does not improve
        Leaf(parent)(majorityVote(assignment), reSubstitution(assignment))
      } else {
        val index = selectedSplit._1;       
        val cutoff = selectedSplit._2._1._2
        val leftIndex = selectedSplit._2._2._1;
        val rightIndex= selectedSplit._2._2._2;
      
        // recursion

      
        val b = Branch(parent)(null, null)(
            majorityVote(assignment), reSubstitution(assignment), 
            index, cutoff)   
        b.left  = split(leftIndex, depth+1,b)
        b.right = split(rightIndex, depth+1,b)  
        b.renew()
        b
      }
    }
  } 
  ////////////////////////////////////////////
  // Pruning
  private def pruneTree(t: Tree, threshold: Double = 1E-10): Double = {
    val (link, remove) = t.weakestLink()

    if (link <= threshold || threshold < 0) {      
      val parentTree = remove.parent
      assert(parentTree != null)
      if (remove eq parentTree.left) {
        parentTree.left = Leaf(parentTree)(remove.label, remove.r)
      } else if (remove eq parentTree.right){
        parentTree.right = Leaf(parentTree)(remove.label, remove.r) 
      } else {
        assert(false)
      }
      //println("Remove " + (remove.numOfLeaves -1) + " Terminal Nodes: " + link )
    
      var pathSearch = parentTree    
      while (pathSearch != null) {
        pathSearch.renew()
        pathSearch = pathSearch.parent
      }
    }
    link
  }
  
  def accuracy(data: List[IndexedSeq[Double]], label:IndexedSeq[Int]): Double = 
    ((0 until label.length).map(i => 
      cTree.classify(data.map(feat => feat(i))) == label(i))
      .filter(identity).length) / label.length.toDouble
      
  def prune(data: List[IndexedSeq[Double]], label:IndexedSeq[Int]): Unit = {
    while (pruneTree(cTree) <= 1E-10) {} //
    var acc1 = accuracy(data, label)
    var acc2 = acc1
    while(acc1 - acc2 <= 0.01 && cTree.numOfLeaves>2) {
      val alpha = pruneTree(cTree, -1)
      acc2 = accuracy(data, label)      
      if (acc2 > acc1) acc1 = acc2
    }
  }
  
  ////////////////////////////////////////////
  // training and testing facilities
  def train(data: List[IndexedSeq[Double]], label:IndexedSeq[Int], maxDepth: Int = 20): Unit = {
	assert (data.length > 0 && data(0).length == label.length)
    D = data
    L = label
    numOfLabels = L.max+1 //label starts from 0
    featureDim = D.length  
    sampleSize = D(0).length
    
    cTree = split(0 until sampleSize, 0, null)    
    
  }
  
  def test(data: List[IndexedSeq[Double]], label: IndexedSeq[Int]): Double = {
    assert (data.length > 0 && data(0).length == label.length)
    assert (cTree != null)
    println("The Accuracy on Test Set Before Pruning: " + accuracy(data, label)) // print accuracy
    prune(data, label)   
    println("The Accuracy on Test Set After Pruning:  " + accuracy(data, label)) // print accuracy    
    println("Total number of leaf nodes: " + cTree.numOfLeaves)
    println("ReSubstitution of the Tree: " + cTree.R)
    accuracy(data, label)
  }
}