import scala.collection.mutable
import scala.util.Random

object Chapter4 {

  class GraphNode[T](val id: T, val children: mutable.Buffer[GraphNode[T]] = mutable.Buffer())

  class MyGraph[T](val root: GraphNode[T])

  class BinaryTree(val root: BinaryTreeNode)

  class BinaryTreeNode(val value: Int, val left: Option[BinaryTreeNode] = None, val right: Option[BinaryTreeNode] = None) {
    var parent: Option[BinaryTreeNode] = None
  }

  def bfs[T](root: GraphNode[T], searchPredicate: GraphNode[T] => Boolean): Boolean = {
    val searchQueue: mutable.Queue[GraphNode[T]] = mutable.Queue[GraphNode[T]]()
    val visitedNodes: mutable.Set[T] = mutable.Set[T]()
    searchQueue.append(root)
    while (searchQueue.nonEmpty) {
      val currentNode: GraphNode[T] = searchQueue.dequeue()
      if (!visitedNodes.contains(currentNode.id)) {
        val found: Boolean = searchPredicate(currentNode)
        if (found) {
          return true
        } else {
          visitedNodes.add(currentNode.id)
          searchQueue.enqueueAll(currentNode.children)
        }
      }
    }
    false
  }

  def dfs[T](root: GraphNode[T], searchPredicate: GraphNode[T] => Boolean): Boolean = {
    val searchStack: mutable.Stack[GraphNode[T]] = mutable.Stack[GraphNode[T]]()
    val visitedNodes: mutable.Set[T] = mutable.Set[T]()
    searchStack.append(root)
    while (searchStack.nonEmpty) {
      val currentNode: GraphNode[T] = searchStack.pop()
      if (!visitedNodes.contains(currentNode.id)) {
        val found: Boolean = searchPredicate(currentNode)
        if (found) {
          return true
        } else {
          visitedNodes.add(currentNode.id)
          searchStack.pushAll(currentNode.children)
        }
      }
    }
    false
  }

  def dfsRecursive[T](currentNode: GraphNode[T], searchPredicate: GraphNode[T] => Boolean, visitedNodes: mutable.Set[T] = mutable.Set[T]()): Boolean = {
    if (visitedNodes.contains(currentNode.id)) {
      false
    } else if (searchPredicate(currentNode)) {
      true
    } else {
      visitedNodes.add(currentNode.id)
      currentNode.children.map(x => dfsRecursive(x, searchPredicate, visitedNodes))
        .foldLeft(false)(_ || _)
    }
  }

  //4.1 route between nodes
  def routeBetweenNodes[T](node1: GraphNode[T], node2: GraphNode[T]): Boolean = {
    bfs[T](node1, _.id == node2.id) || bfs[T](node2, _.id == node1.id)
  }

  //4.2 minimal tree from array
  def minimalTree(arr: Array[Int]): BinaryTree = {
    def buildSubTree(start: Int, end: Int): BinaryTreeNode = {
      if (start == end - 1) {
        new BinaryTreeNode(arr(start))
      } else if (start == end - 2) {
        new BinaryTreeNode(arr(start + 1), left = Some(new BinaryTreeNode(arr(start))))
      } else {
        val mid = (start + end) / 2
        new BinaryTreeNode(arr(mid), left = Some(buildSubTree(start, mid)), right = Some(buildSubTree(mid + 1, end)))
      }
    }

    new BinaryTree(buildSubTree(0, arr.length))
  }

  //4.3 list of depths
  def listOfDepths(tree: BinaryTree): Seq[mutable.Buffer[BinaryTreeNode]] = {
    val depthToListOfNodes: mutable.Map[Int, mutable.Buffer[BinaryTreeNode]] = mutable.Map()

    def innerListOfDepths(currentNode: BinaryTreeNode, currentDepth: Int = 0): Unit = {
      val listOfCurrentDepth: mutable.Buffer[BinaryTreeNode] = depthToListOfNodes.getOrElseUpdate(currentDepth, mutable.Buffer[BinaryTreeNode]())
      listOfCurrentDepth.append(currentNode)

      currentNode.left.foreach(left => innerListOfDepths(left, currentDepth + 1))
      currentNode.right.foreach(right => innerListOfDepths(right, currentDepth + 1))
    }

    innerListOfDepths(tree.root)
    depthToListOfNodes.keys.toSeq.sorted.map(depth => depthToListOfNodes(depth))
  }

  //4.4 check if binary tree is balanced
  def checkBalanced(tree: BinaryTree): Boolean = {
    case class SubTreeInfo(height: Int, maxHeightDifference: Int)

    def getSubTreeInfo(node: BinaryTreeNode): SubTreeInfo = {
      val leftInfo: SubTreeInfo = node.left.map(getSubTreeInfo).getOrElse(SubTreeInfo(0, 0))
      val rightInfo: SubTreeInfo = node.right.map(getSubTreeInfo).getOrElse(SubTreeInfo(0, 0))

      val currentHeight: Int = math.max(leftInfo.height, rightInfo.height) + 1
      val newMaxHeightDifference: Int = math.max(math.max(leftInfo.maxHeightDifference, rightInfo.maxHeightDifference), math.abs(leftInfo.height - rightInfo.height))

      SubTreeInfo(height = currentHeight, maxHeightDifference = newMaxHeightDifference)
    }

    val rootInfo = getSubTreeInfo(tree.root)
    rootInfo.maxHeightDifference <= 1
  }

  //4.5 check if a binary tree is a binary search tree
  def validateBST(tree: BinaryTree): Boolean = {
    def validateBSTForNode(node: BinaryTreeNode, lowerLimit: Int = Int.MinValue, upperLimit: Int = Int.MaxValue): Boolean = {
      if (node.value < lowerLimit || node.value > upperLimit) {
        false
      } else {
        val leftIsValid: Boolean = node.left.forall(left => validateBSTForNode(left, lowerLimit = lowerLimit, upperLimit = math.min(upperLimit, node.value)))
        val rightIsValid: Boolean = node.right.forall(right => validateBSTForNode(right, lowerLimit = math.max(lowerLimit, node.value), upperLimit = upperLimit))

        leftIsValid && rightIsValid
      }
    }

    validateBSTForNode(tree.root)
  }

  //4.6 find next node in BST
  def findSuccessor(node: BinaryTreeNode): Option[BinaryTreeNode] = {
    if (node.right.nonEmpty) {
      //if node has right subtree, the successor is the leftmost node in it
      var successorCandidate: BinaryTreeNode = node.right.get
      while (successorCandidate.left.nonEmpty) {
        successorCandidate = successorCandidate.left.get
      }
      Some(successorCandidate)
    } else {
      //successor is the first parent for which we are in a left subtree
      var parent = node.parent
      var child = node
      var found = false
      while (!found) {
        parent match {
          case Some(candidate) =>
            if (candidate.left.map(_.value).contains(child.value)) {
              //node is on the left subtree of the parent, so the parent is the next node
              found = true
            } else {
              child = candidate
              parent = child.parent
            }
          case None =>
            //no right subtree and no parent, means current node has no successor
            found = true
        }
      }

      parent
    }
  }

  //4.7 build order
  //  def buildOrder(projects: Seq[String], dependencies: Seq[(String, String)]): Option[Seq[String]] = {
  //    val projectNameToGraphNode: Map[String, GraphNode[String]] = projects.map(x => x -> new GraphNode[String](x)).toMap
  //
  //    dependencies.foreach({
  //      case (dependency, dependant) =>
  //        val dependantNode = projectNameToGraphNode(dependant)
  //        val dependencyNode = projectNameToGraphNode(dependency)
  //        dependantNode.children.append(dependencyNode)
  //    })

  //4.8 find common ancestor of two nodes
  def findCommonAncestor(root: BinaryTreeNode,
                         p: BinaryTreeNode,
                         q: BinaryTreeNode): BinaryTreeNode = {
    def isNodeInSubtree(sourceNode: BinaryTreeNode,
                        targetNode: BinaryTreeNode): Boolean = {
      if (sourceNode == targetNode) {
        true
      } else {
        sourceNode.left.exists(x => isNodeInSubtree(x, targetNode)) ||
          sourceNode.right.exists(x => isNodeInSubtree(x, targetNode))
      }
    }

    lazy val isPInLeft: Boolean = root.left.exists(x => isNodeInSubtree(x, p))
    lazy val isQInLeft: Boolean = root.left.exists(x => isNodeInSubtree(x, q))
    lazy val isPInRight: Boolean = root.right.exists(x => isNodeInSubtree(x, p))
    lazy val isQInRight: Boolean = root.right.exists(x => isNodeInSubtree(x, q))

    if (isPInLeft && isQInLeft) {
      findCommonAncestor(root.left.get, p, q)
    } else if (isPInRight && isQInRight) {
      findCommonAncestor(root.right.get, p, q)
    } else {
      root
    }
  }

  //4.9 BST sequences
  def getBSTSequences(root: BinaryTreeNode): mutable.Buffer[mutable.Buffer[Int]] = {
    val sequencesOfLeft: mutable.Buffer[mutable.Buffer[Int]] = root.left.map(getBSTSequences)
      .getOrElse(mutable.Buffer[mutable.Buffer[Int]]())
    val sequencesOfRight: mutable.Buffer[mutable.Buffer[Int]] = root.right.map(getBSTSequences)
      .getOrElse(mutable.Buffer[mutable.Buffer[Int]]())

    (sequencesOfLeft.isEmpty, sequencesOfRight.isEmpty) match {
      case (true, true) => mutable.Buffer[mutable.Buffer[Int]](mutable.Buffer[Int](root.value))
      case (false, true) => sequencesOfLeft.foreach(_.prepend(root.value))
        sequencesOfLeft
      case (true, false) => sequencesOfRight.foreach(_.prepend(root.value))
        sequencesOfRight
      case (false, false) =>
        (for{
          sequenceOfLeft <- sequencesOfLeft
          sequenceOfRight <- sequencesOfRight
        }yield {
          val withLeftFirst: mutable.Buffer[Int] = mutable.Buffer[Int](root.value)
          withLeftFirst.appendAll(sequenceOfLeft)
          withLeftFirst.appendAll(sequenceOfRight)

          val withRightFirst: mutable.Buffer[Int] = mutable.Buffer[Int](root.value)
          withRightFirst.appendAll(sequenceOfRight)
          withRightFirst.appendAll(sequenceOfLeft)
          val combined = mutable.Buffer[mutable.Buffer[Int]](withLeftFirst, withRightFirst)
          combined
        }).flatten
    }
  }

  def compareTrees(node1: BinaryTreeNode, node2: BinaryTreeNode): Boolean = {
    lazy val rootsMatch: Boolean = node1.value == node2.value
    lazy val leftSubTreesMatch: Boolean = (node1.left, node2.left) match {
      case (None, None) => true
      case (Some(l1), Some(l2)) => compareTrees(l1, l2)
      case _ => false
    }
    lazy val rightSubTreesMatch: Boolean = (node1.right, node2.right) match {
      case (None, None) => true
      case (Some(r1), Some(r2)) => compareTrees(r1, r2)
      case _ => false
    }
    rootsMatch && leftSubTreesMatch && rightSubTreesMatch
  }

  //4.10 check subtree
  def checkSubtree(t1Root: BinaryTreeNode, t2Root: BinaryTreeNode): Boolean = {
    lazy val isSameTree: Boolean = compareTrees(t1Root, t2Root)
    lazy val isSubTreeOfLeft: Boolean = t1Root.left.exists(left => checkSubtree(left, t2Root))
    lazy val isSubTreeOfRight: Boolean = t1Root.right.exists(right => checkSubtree(right, t2Root))

    isSameTree || isSubTreeOfLeft || isSubTreeOfRight
  }

  class RandomAccessBinaryTree{
    var root: Option[RandomAccessBinaryTreeNode] = None

    def insert(value: Int): Unit = {
      if(root.isEmpty){
        root = Some(new RandomAccessBinaryTreeNode(value))
      }else{
        root.get.insert(value)
      }
    }

    def find(v: Int): Option[RandomAccessBinaryTreeNode] = {
      root.flatMap(_.find(v))
    }

    def getRandomNode: Option[RandomAccessBinaryTreeNode] = {
      root.map(actualRoot => {
        val totalNumberOfNodes: Int = actualRoot.treeSize
        val randomNodeIdx: Int = Random.nextInt(totalNumberOfNodes)
        actualRoot.getNodeAtIdx(randomNodeIdx)
      })
    }
  }

  class RandomAccessBinaryTreeNode(val value: Int,
                                   var treeSize: Int = 1,
                                   var left: Option[RandomAccessBinaryTreeNode] = None,
                                   var right: Option[RandomAccessBinaryTreeNode] = None,
                                   var parent: Option[RandomAccessBinaryTreeNode] = None){

    def insert(newValue: Int): Unit = {
      this.treeSize += 1
      if(newValue <= this.value){
        //insert on left
        left match {
          case None => this.left = Some(new RandomAccessBinaryTreeNode(newValue))
          case Some(l) => l.insert(newValue)
        }
      }else{
        //insert on right
        right match {
          case None => this.right = Some(new RandomAccessBinaryTreeNode(newValue))
          case Some(r) => r.insert(newValue)
        }
      }
    }

    def find(target: Int): Option[RandomAccessBinaryTreeNode] = {
      if(target == this.value){
        Some(this)
      }else if(target < this.value){
        left.flatMap(_.find(target))
      }else {
        right.flatMap(_.find(target))
      }
    }

    def getNodeAtIdx(idx: Int): RandomAccessBinaryTreeNode = {
      (idx - left.map(_.treeSize).getOrElse(0)) match {
        case _ < 0 => left.get.getNodeAtIdx(idx)
        case 0 => this
        case _ > 0 => right.get.getNodeAtIdx(idx - left.map(_.treeSize).getOrElse(0) - 1)
      }
    }
  }


  def main(args: Array[String]): Unit = {
    val arr = Array(1, 2, 3, 4, 5, 6)
    val res = minimalTree(arr)
    println(res)
  }

}
