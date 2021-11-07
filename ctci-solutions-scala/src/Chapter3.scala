import scala.collection.mutable

object Chapter3 {

  class MyStack {
    var size: Int = 0

    class StackNode(val data: Int) {
      var next: Option[StackNode] = None
      var min: Int = data
    }

    var top: Option[StackNode] = None

    def pop(): Option[Int] = {
      top match {
        case None => None
        case Some(node) =>
          top = node.next
          size -= 1
          Some(node.data)
      }
    }

    def peek: Option[Int] = top.map(_.data)

    def isEmpty: Boolean = top.isEmpty

    def min: Option[Int] = top.map(_.min)

    def push(item: Int): Unit = {
      val newNode = new StackNode(item)
      newNode.next = top
      size += 1
      newNode.min = newNode.next.map(x => {
        if (x.data.compareTo(newNode.data) < 0) {
          x.data
        } else {
          newNode.data
        }
      }).getOrElse(newNode.data)
      top = Some(newNode)
    }
  }

  class SetOfStacks(sizeLimit: Int) {
    var stackList: mutable.Buffer[MyStack] = mutable.Buffer()

    def pop(): Option[Int] = {
      stackList.headOption match {
        case None => None
        case Some(currentStack) =>
          val poppedValue: Option[Int] = currentStack.pop()
          if (currentStack.isEmpty) {
            stackList.remove(0)
          }
          poppedValue
      }
    }

    def peek: Option[Int] = stackList.headOption.flatMap(_.peek)

    def isEmpty: Boolean = peek.isEmpty

    def push(item: Int): Unit = {
      if (stackList.headOption.exists(_.size < sizeLimit)) {
        stackList.head.push(item)
      } else {
        val newStack = new MyStack()
        newStack.push(item)
        stackList.addOne(newStack)
      }
    }
  }

  class MyQueue {
    private val mainStack: MyStack = new MyStack()
    private val reverseStack: MyStack = new MyStack()

    def push(item: Int): Unit = {
      if(!areItemsInMainStack){
        moveStackItems(reverseStack, mainStack)
      }
      mainStack.push(item)
    }

    def pop(): Option[Int] = {
      if(areItemsInMainStack){
        moveStackItems(mainStack, reverseStack)
      }
      reverseStack.pop()
    }

    private def moveStackItems(source: Chapter3.MyStack, target: Chapter3.MyStack): Unit = {
      while(!source.isEmpty){
        target.push(source.pop().get)
      }
    }

    private def areItemsInMainStack: Boolean = reverseStack.isEmpty

    def isEmpty: Boolean = if(areItemsInMainStack) mainStack.isEmpty else reverseStack.isEmpty
  }


  //3.5 sort stack using only 1 additional stack
  def sortStack(stack: MyStack): Unit = {
    val tempStack: MyStack = new MyStack()
    while(!stack.isEmpty){
      val currentItem: Int = stack.pop().get
      var itemsToMoveBack: Int = 0
      while(tempStack.peek.exists(_ > currentItem)){
        stack.push(tempStack.pop().get)
        itemsToMoveBack += 1
      }
      tempStack.push(currentItem)
      (0 until itemsToMoveBack).foreach(_ => tempStack.push(stack.pop().get))
    }
    while(!tempStack.isEmpty){
      stack.push(tempStack.pop().get)
    }
  }

  sealed trait Animal
  case class Cat() extends Animal
  case class Dog() extends Animal

  case class AnimalInShelter(animal: Animal,
                             timeOfArrivalInShelter: Long = System.currentTimeMillis())
  //3.6
  class AnimalShelter {
    private val catsQueue: mutable.Buffer[AnimalInShelter] = mutable.Buffer[AnimalInShelter]()
    private val dogsQueue: mutable.Buffer[AnimalInShelter] = mutable.Buffer[AnimalInShelter]()

    def enqueue(animal: Animal): Unit = animal match {
      case cat: Cat => catsQueue.append(AnimalInShelter(cat))
      case dog: Dog => dogsQueue.append(AnimalInShelter(dog))
    }

    def dequeueDog(): Option[Dog] = {
      if(dogsQueue.isEmpty){
        None
      }else{
        dogsQueue.remove(0) match {
          case AnimalInShelter(dog: Dog, _) => Some(dog)
          case _ => throw new Exception("Invalid state of animal shelter")
        }
      }
    }

    def dequeueCat(): Option[Cat] = {
      if(catsQueue.isEmpty){
        None
      }else{
        catsQueue.remove(0) match {
          case AnimalInShelter(cat: Cat, _) => Some(cat)
          case _ => throw new Exception("Invalid state of animal shelter")
        }
      }
    }

    def dequeueAny(): Option[Animal] = {
      (catsQueue.headOption, dogsQueue.headOption) match {
        case (None, None) => None
        case (Some(_), None) => dequeueCat()
        case (None, Some(_)) => dequeueDog()
        case (Some(AnimalInShelter(_, timeOfCatArrival)), Some(AnimalInShelter(_, timeOfDogArrival))) =>
          if(timeOfCatArrival < timeOfDogArrival){
            dequeueCat()
          }else{
            dequeueDog()
          }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val stk = new MyStack()

    stk.push(6)
    stk.push(2)
    stk.push(5)
    stk.push(3)
    stk.push(1)

    sortStack(stk)
    while (stk.peek.nonEmpty) {
      println(stk.peek)
      stk.pop()
    }
  }
}
