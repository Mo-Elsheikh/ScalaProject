import scala.collection.mutable.ArrayBuffer
import scala.{+:, ::}

/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Description: Prg 02 - Functional Programming BST
 * Student(s) Name(s): Mohamed Elsheikh
 */

class BinTree[T <: Comparable[T]]() {

  private var value: T = _
  private var left:  BinTree[T] = _
  private var right: BinTree[T] = _

  def this(value: T) {
    this();
    this.value = value
  }

  def add(value: T): Unit = {

    def add(current: BinTree[T]): BinTree[T] = {
      if (current == null)
        new BinTree[T](value)
      else {
        if (value.compareTo(current.value) < 0)
          current.left = add(current.left)
        else if (value.compareTo(current.value) > 0)
          current.right = add(current.right)
        current
      }
    }

    if (this.value == null)
      this.value = value
    else
      add(this)
  }

  // TODO #1: returns a string representation of the tree using tabs to show its structure
  def mkString(): String = {
      var s:String =""
      if(this.value != null){
          s = s.concat(this.value.toString)
          if(this.left != null){
              s = s.concat("\t").concat(this.left.mkString())
          }
          if(this.right != null){
              s = s.concat("\t").concat(this.right.mkString())
          }
      }
      s
  }

  // TODO #2: returns a new tree (from the callee) with only the values that passed the test denoted by function f
  def filter(f: T => Boolean): BinTree[T] = {
      def filter(f:T=>Boolean,temptree:BinTree[T]):ArrayBuffer[T]={
          val temparray:ArrayBuffer[T] = new ArrayBuffer() 
          if(temptree.value!=null){
              if(f(temptree.value)){
                  temparray.append(temptree.value)
              }
              if(temptree.left!=null){
                  temparray.appendAll(filter(f,temptree.left))
              }
              if(temptree.right!=null){
                  temparray.appendAll(filter(f,temptree.right))
              }
          }
          temparray
      }
      val resultarray:ArrayBuffer[T] = filter(f,this)
      val resulttree:BinTree[T] = new BinTree()
      for(i<-resultarray){
          resulttree.add(i)
      }
      resulttree
  }

  // TODO #3: returns a new tree (from the callee) by applying function f to each of the callee's elements
  def map(f: T => T): BinTree[T] = {
      val temptree:BinTree[T] = new BinTree()
      if(this.value != null){
          temptree.value = f(this.value)
          if(this.left != null){
              temptree.left = this.left.map(f)
          }
          if(this.right != null){
              temptree.right = this.right.map(f)
          }
      }
      temptree
  }

  // TODO #4: applies function f to each of the tree's elements
  def foreach(f: T => T): Unit = {
      if(this.value != null){
          this.value = f(this.value)
          if(this.left != null){
              this.left.foreach(f)
          }
          if(this.right != null){
              this.right.foreach(f)
          }
      }
  }

  // TODO #5: similar to foldLeft for collections
  def foldLeft(value: T)(f: (T, T) => T ): T = {
      def getAll(temptree:BinTree[T]):ArrayBuffer[T]={
          val temparray:ArrayBuffer[T] = new ArrayBuffer() 
          if(temptree.value!=null){
              temparray.append(temptree.value)
              if(temptree.left!=null){
                  temparray.appendAll(getAll(temptree.left))
              }
              if(temptree.right!=null){
                  temparray.appendAll(getAll(temptree.right))
              }
          }
          temparray
      }
      var ans:T = value
      val resultarray:ArrayBuffer[T] = getAll(this)
      for(i<-resultarray){
          ans = f(ans,i)
      }
      ans
  }

  // TODO #6: similar to foldRight for collections
  def foldRight(value: T)(f: (T, T) => T ): T = {
      foldLeft(value)((x,y)=>f(y,x))
  }

  // TODO #7: similar to foldLeft
  def fold(value: T)(f: (T, T) => T ): T = {
      foldLeft(value)(f)
  }

  // TODO #8: returns the height of the tree
  def height(): Int = {
      if(this.value==null){
          return 0
      }
      else{
          var lh:Int = 0
          var rh:Int = 0
          if(this.left!=null){
              lh = this.left.height()
          }
          if(this.right!=null){
              rh = this.right.height()
          }
          return (lh.max(rh) +1)
      }
  }

  // TODO #9: returns the number of elements of the tree
  def size(): Int = {
      var s:Int = 0
      if(this.value != null){
          s+=1
          if(this.left != null){
              s+=this.left.size()
          }
          if(this.right != null){
              s+=this.right.size()
          }
      }
      s
  }

  // TODO #10: returns true/false depending whether value is/is not found in the tree
  def search(value: T): Boolean = {
      if(this.value != null){
          if(this.value == value){
              return true
          }
          else if(value.compareTo(this.value)<0){
              if(this.left!=null){
                  return this.left.search(value)
              }
              else{
                  return false
              }
          }
          else{
              if(this.right!=null){
                  return this.right.search(value)
              }
              else{
                  return false
              }
          }
      }
      else{
          return false
      }
  }
}

object BinTree {

  def main(args: Array[String]): Unit = {

    // tree 1 (a BST of integers)
    var tree1 = new BinTree[Integer]()

    // TODO #11a: use array's foreach to add the following elements: 2, 1, 7, 3, 9, 10; display the tree next
	Array(2, 1, 7, 3, 9, 10).foreach(x=>tree1.add(x))
	println(tree1.mkString())
    // TODO #11b: use foreach to multiply all of its elements by 2; display the tree next
	tree1.foreach(x=>x*2)
	println(tree1.mkString())
    // TODO #11c: use filter to obtain another tree whose elements are > 5; display the resulting tree next
	println(tree1.filter(x=> x>5).mkString())
    // TODO #11d: use map to subtract 2 from each element of the original tree (modified by TODO #11b); display the resulting tree next
	val modifiedby11d = tree1.map(x=> x-2)
	println(modifiedby11d.mkString())
    // TODO #11e: use fold to compute the sum of all of the elements of the (original) tree (modified by TODO #11d)
	println(modifiedby11d.fold(0)((x,y)=>x+y))
    // TODO #11f: use fold and size to compute the average of the elements of the (original) tree (modified by TODO #11d)
	println(modifiedby11d.fold(0)((x,y)=>x+y).toDouble/modifiedby11d.size().toDouble)
    // TODO #11g: show the height of the tree
	println(tree1.height())
    // TODO #11h: search for elements 4 and 17 in the (original) tree (modified by TODO #11d)
	if(modifiedby11d.search(4)){
    		println("4 was found!")
	}
	else{
    		println("4 was not found!")
	}
	if(modifiedby11d.search(17)){
    		println("17 was found!")
	}
	else{
    		println("17 was not found!")
	}
    // tree 2 (a BST of strings)
    var tree2 = new BinTree[String]()

    // TODO #12a: use array's foreach to add the following elements: "perry rhodan", "icho tolot", "denetree", "deshan apian", "levian paronn", "roder roderich", "gressko gurrad"; display the tree next
	Array("perry rhodan", "icho tolot", "denetree", "deshan apian", "levian paronn", "roder roderich", "gressko gurrad").foreach(x=>tree2.add(x))
	println(tree2.mkString())
    // TODO #12b: use foreach to capitalize the first letter of each word of each tree node; display the tree next
	tree2.foreach(x=>x.split(" ").map(x=>x.capitalize).mkString(" "))
	println(tree2.mkString())
    // TODO #12c: use filter to obtain another tree whose names are made of at least 2 words; display the resulting tree next
	println(tree2.filter(x=>x.split(" ").length >=2).mkString())
    // TODO #12d: use map to capitalize all of the letters from each of the names in the (original) tree; display the resulting tree next
	val modifiedtree2 = tree2.map(x=> x.toUpperCase)
	println(modifiedtree2.mkString())
    // TODO #12e: use fold to compute the sum of all of the letters from each of the names in the tree
	println(tree2.fold("0")((x,y)=>(x.toInt+y.length).toString))
    // TODO #12f: use fold and size to compute the average number of characters of the names in the tree
	println(tree2.fold("0")((x,y)=>(x.toInt+y.length).toString).toDouble/tree2.size().toDouble)
    // TODO #12g: show the height of the tree
	println(tree2.height())
    // TODO #12h: search for elements "PERRY RHODAN" and "ATLAN"
	if(modifiedtree2.search("PERRY RHODAN")){
    		println("PERRY RHODAN was found!")
	}
	else{
    		println("PERRY RHODAN was not found!")
	}
	if(modifiedtree2.search("ATLAN")){
    		println("ATLAN was found!")
	}
	else{
    		println("ATLAN was not found!")
	}
  }
}