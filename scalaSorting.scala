object hwk8{

  def merge(l: List[Int], r: List[Int]): List[Int] =
    (l, r) match {
      case (l, Nil) => l
      case (Nil, r) => r
      case (a :: b, c :: d) =>
        if (a < c) a :: merge(b, r)
        else c :: merge(l, d)
    }

  def merge_sort(lst: List[Int]): List[Int] = {
    val a = lst.length / 2
    if (a == 0) lst // i.e. if list is empty or single value, no sorting needed
    else {
      val (l, r) = lst.splitAt(a)
      merge(merge_sort(l), merge_sort(r))
    }
  }

  def selection_sort(lst: List[Int]): List[Int] = {

    def select(a: Int, b: List[Int], c: List[Int]): List[Int] =
      b match {
        case Nil => a::selection_sort(c)
        case x::y => if(x<a){select(x,y, a::c)}else{select(a,y,x::c)}
      }

    lst match {
      case Nil => Nil
      case a::b => select(a, b, Nil)
    }

  }

  def insert(i:Int, lst:List[Int]): List[Int] =
    lst match {
      case Nil => i::lst
      case a :: b => if (a < i) {a :: insert(i, b)} else {i::lst}
    }

  def insertion_sort(lst: List[Int]): List[Int] =
    lst match {
      case Nil => lst
      case a::b => insert(a, insertion_sort(b))
    }

  def main(args: Array[String]) {
    val lst = List(5,4,11,2,3,1,0,9)
    println(merge_sort(lst))
    println(selection_sort(lst))
    println(insertion_sort(lst))
  }
}