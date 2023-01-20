object hwk9 {
  def main(args: Array[String]) {
    val t1 = Let("y", Num(10),
      Let("f", Fn("x", Plus(Var("x"), Var("y"))),
        Let("y", Num(20),
          App(Var("f"), Num(5)))))

    val t2 = Let("y", Num(10),
      Let("f", Fn("x", Plus(Var("x"), Var("z"))),
        Let("y", Num(10),
          App(Var("f"), Num(5)))))

    val t3 = Div(Num(10), Num(0))

    val t4 = Plus(Num(10), Minus (Num(10), Fn("x", Num(5))))

    val t5 = App(Num(10), Num(10))

    val t6 = Let("f", Num(10), App(Var("f"), Num(20)))

    val t7 = Let("x", Plus(Num(10), Fn("x", Var("x"))), Plus(Num(0), Num(20)))


    println(t1.eval)
    println(t2.eval)
    println(t3.eval)
    println(t4.eval)
    println(t5.eval)
    println(t6.eval)
    println(t7.eval)
  }
}

abstract class Exp {
  // return the string representation of this expression
  override def toString: String = this match {
    // TOD0
    case Num(x) => x.toString()
    case Plus(x, y) => "(" + x.toString + " + " + y.toString + ")"
    case Minus(x, y) => "(" + x.toString + " - " + y.toString + ")"
    case Times(x, y) => "(" + x.toString + " * " + y.toString + ")"
    case Div(x, y) => "(" + x.toString + " / " + y.toString + ")"
    case Var(x) => x
    case Let(x, y, z) => "let val " + x + "=" + y.toString + " in " + z.toString + " end"
    case Fn(x, e) => "(fn " + x + " => " + e.toString + ")"
    case App(x, y) => "(" + x.toString + " " + y.toString + ")"
  }

  // lookup 'y' in 'ctx'
  def lookup (ctx: List[(String, Value)], y: String): Value = ctx match {
    case Nil => Error ("variable not found")
    case (a,b)::c => if(a==y){b}else{lookup(c, y)}
  }

  def eval: Value = this eval List()

  // evaluate 'this' expression with the context 'ctx'
  def eval (ctx: List[(String, Value)]): Value = this match {
    case Num(x) => CVal(x)
    case Plus(e1, e2) =>
      val a = e1.eval(ctx)
      val b = e2.eval(ctx)
      (a,b) match {
        case (FnVal(x,e,ctx), _) => Error("fn is not a number")
        case (_, FnVal(x,e,ctx)) => Error("fn is not a number")
        case (CVal (a), CVal (b)) => CVal(a+b)
        case (Error(e), _) => Error(e)
        case (_, e) => e
      }
    case Minus(e1, e2) =>
      val a = e1.eval(ctx)
      val b = e2.eval(ctx)
      (a,b) match {
        case (FnVal(x,e,ctx), _) => Error("fn is not a number")
        case (_, FnVal(x,e,ctx)) => Error("fn is not a number")
        case (CVal (a), CVal (b)) => CVal(a-b)
        case (Error(e), _) => Error(e)
        case (_, e) => e
      }
    case Times(e1, e2) =>
      val a = e1.eval(ctx)
      val b = e2.eval(ctx)
      (a,b) match {
        case (FnVal(x,e,ctx), _) => Error("fn is not a number")
        case (_, FnVal(x,e,ctx)) => Error("fn is not a number")
        case (CVal (a), CVal (b)) => CVal(a*b)
        case (Error(e), _) => Error(e)
        case (_, e) => e
      }
    case Div(e1, e2) =>
      val a = e1.eval(ctx)
      val b = e2.eval(ctx)
      (a,b) match {
        case (FnVal(x,e,ctx), _) => Error("fn is not a number")
        case (_, FnVal(x,e,ctx)) => Error("fn is not a number")
        case (CVal (a), CVal (0)) => Error("cannot divide by 0")
        case (CVal (a), CVal (b)) => CVal(a / b)
        case (Error(e), _) => Error(e)
        case (_, e) => e
      }
    case Var(x) => lookup(ctx, x)
    case Let(x, y, z) =>
      if (y.eval(ctx) == Error("variable not found"))
      {
        Error("variable not found")
      }
      else if (y.eval(ctx) == Error("cannot divide by 0"))
      {
        Error("cannot divide by 0")
      }
      else if (y.eval(ctx) == Error("this is a cval not a fnval"))
      {
        Error("this is a cval not a fnval")
      }
      else if (y.eval(ctx) == Error("fn is not a number"))
      {
        Error("fn is not a number")
      }
      else
      {
        z.eval((x, y.eval(ctx))::ctx)
      }
    case Fn(x, e) => FnVal(x, e, ctx)
    case App(e1, e2) =>
      val a = e1.eval(ctx)
      a match {
        case CVal(x) => Error("this is a cval not a fnval")
        case FnVal(x, e, ctx) =>
          val v = e2.eval(ctx)
          e.eval((x,v)::ctx)
        case Error(m) => Error(m)
      }
  }
}

case class Num(x: Int) extends Exp
case class Plus(e1: Exp, e2: Exp) extends Exp
case class Minus(e1: Exp, e2: Exp) extends Exp
case class Times(e1: Exp, e2: Exp) extends Exp
case class Div(e1: Exp, e2: Exp) extends Exp
case class Var(x: String) extends Exp
case class Let(x: String, e1: Exp, e2: Exp) extends Exp
case class App(e1: Exp, e2: Exp) extends Exp
case class Fn(x: String, e: Exp) extends Exp


abstract class Value {
  // return the string representation of this value
  override def toString = this match {
    // TODO
    case CVal(x) => Num(x).toString
    case FnVal(x, e, ctx) => Fn(x, e).toString
    case Error(m) => Var(m).toString
  }
}
case class CVal(x: Int) extends Value
case class FnVal(x: String, e: Exp, ctx: List[(String, Value)]) extends Value
case class Error(m: String) extends Value