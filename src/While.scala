//scala.util.parsing.combinator.JavaTokenParsers

class NotImplementedException(s: String) extends RuntimeException(s)
class StepException(s: String) extends RuntimeException(s)
class DeriveException(s: String) extends RuntimeException(s)

// ---skip for now--- somehow convert string to BExpr, still looking into that
//  def read(text: String): BExpr = {
//}

//BExpr, for Basic Syntax.
sealed abstract class BExpr
case class BNum(n: Int) extends BExpr
case class BStr(s:String) extends BExpr
case class BList(list: List[BExpr]) extends BExpr

//DExpr, for Derived Syntax.
sealed abstract class DExpr

//expressions
case class DTrue() extends DExpr //literal
case class DFalse() extends DExpr //literal
case class DNum(n: Int) extends DExpr //literal
case class DPlus(l: DExpr, r: DExpr) extends DExpr
case class DLeq(l: DExpr, r: DExpr) extends DExpr
case class DId(c: String) extends DExpr

sealed trait Command extends DExpr // not sure of this part yet
case class DSeq(c1:Command, c2:Command) extends Command
case class DAssign(id:DId, e:DExpr) extends Command
case class DPrint(e: DExpr) extends Command
case class DWhile(e:DExpr, c:Command) extends Command
case class DDone() extends Command


object DExpr {
  val keywords = Set("print", "while", "do", "od", "done", ";", ":=", "+", "<=")
}

object Step {
  //  def step(str: String): DExpr = parse(read(str))

  def step(bexpr: BExpr): DExpr = bexpr match {
    case BNum(n) => DNum(n) // assuming dat parser elk getal in een BNum omzet
    case BStr(s) => s match {
      case "true" => DTrue()
      case "false" => DFalse()
      case x if DExpr.keywords.contains(x) => throw new StepException(x + " cannot be used as an identifier")
      case x => DId(x) // free identifier
    }
    case BList(list) => list match {
      case l :: BStr("+") :: r :: Nil => DPlus(step(l), step(r))
      case l :: BStr("<=") :: r :: Nil => DLeq(step(l), step(r))
      case x => stepCommand(BList(x))
    }
    case _ => stepCommand(bexpr)
  }

  def stepCommand(bexpr: BExpr): Command = bexpr match {
    case BStr("done") => DDone()
    case BList(list) => list match {
      case l :: BStr(";") :: r :: Nil => DSeq(stepCommand(l), stepCommand(r))
      case BStr("print") :: expr :: Nil => DPrint(step(expr))
      case BStr("while") :: expr :: BStr("do") :: comm :: BStr("od") :: Nil => DWhile(step(expr), stepCommand(comm))
      case l :: BStr(":=") :: r :: Nil => l match {
        case BStr(x) if DExpr.keywords.contains(x) => throw new StepException(x + " cannot be used as an identifier")
        case BStr(x) => DAssign(DId(x), step(r))
        case _ => throw new StepException("identifier should be a string, but was " + l)
      }
      case _ => throw new StepException("I don't know what to do with list" + list)
    }
    case _ => throw new StepException("I don't know what to do with " + bexpr)
  }
}

object Derive {
  case class Assign(id:String, value:DExpr)
  type Store = List[Assign]

  def derive(e: DExpr, st: Store): Any = e match {
    case DTrue() => true
    case DFalse() => false
    case DNum(n) => n
    case DPlus(l, r) => (derive(l, st), derive(r, st)) match {
      case (DNum(left), DNum(right)) => left+right
      case x => throw new DeriveException("DPlus failed " + x)
    }
    case DLeq(l, r) => (derive(l, st), derive(r, st)) match {
      case (DNum(left), DNum(right)) => left<=right
      case x => throw new DeriveException("DLeq failed " + x)
    }
    case DId(id) => lookup(id, st);
    case DSeq(c1, c2) => ???
    case DAssign(id, e) => ???
    case DPrint(e) => System.out.print(derive(e, st))
    case DWhile(e, c) => ???
    case DDone() => ???
  }

  def lookup(x: String, st: Store): Any = st match {
    case Nil => throw new DeriveException("could not find identifier " + x + " in store")
    case Assign(id, value) :: st1 => if (x == id) derive(value, st1) else lookup (x, st1)
  }
}

