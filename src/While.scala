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

//DExpr, for Derived Expressions.
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

// final result, L*
sealed abstract class Output
case class NumO(n:Int) extends Output
case class BoolO(b:Boolean) extends Output
case class NoneO() extends Output

object DExpr {
  val keywords = Set("print", "while", "do", "od", "done", ";", ":=", "+", "<=")
}

object Step {
  //  def step(str: String): DExpr = parse(read(str))

  def stepCommand(bexpr: BExpr): Command = bexpr match {
    case BStr("done") => DDone()
    case BList(list) => list match {
      case l :: BStr(";") :: r :: Nil => DSeq(stepCommand(l), stepCommand(r))
      case BStr("print") :: expr :: Nil => DPrint(step(expr))
      case BStr("while") :: expr :: BStr("do") :: comm :: BStr("od") :: Nil => DWhile(step(expr), stepCommand(comm))
      case l :: BStr(":=") :: r :: Nil => l match {
        case BStr(x) if !DExpr.keywords.contains(x) => DAssign(DId(x), step(r))
        case BStr(x) => throw new StepException(x + " cannot be used as an identifier")
        case _ => throw new StepException("identifier should be a string, but was " + l)
      }
      case _ => throw new StepException("I don't know what to do with list" + list)
    }
    case _ => throw new StepException("I don't know what to do with " + bexpr)
  }

  def step(bexpr: BExpr): DExpr = bexpr match {
    case BNum(n) => DNum(n)
    case BStr(s) => s match {
      case "true" => DTrue()
      case "false" => DFalse()
      case x if DExpr.keywords.contains(x) => throw new StepException(x + " cannot be used as an identifier")
      case x => DId(x)
    }
    case BList(list) => list match {
      case l :: BStr("+") :: r :: Nil => DPlus(step(l), step(r))
      case l :: BStr("<=") :: r :: Nil => DLeq(step(l), step(r))
      case _ => stepCommand(BList(list))
    }
    case _ => stepCommand(bexpr)
  }
}

object Derive {
  case class Assign(id:String, value:DExpr)
  type Store = List[Assign]

  // not sure how to implement command yet
  def derive(e: DExpr, st: Store): (Command, Output, Store) = e match {
        case DSeq(c1, c2) => derive(c1, st) match {
          case (DDone(), out, st1) => derive(c2, st1)
        }
        case DAssign(DId(id), e) => {
          val st1 = st ::: Assign(id, e) :: Nil
          (DDone(), deriveExpr(e, st1)._1, st1)
        }
        case DPrint(e) => {
          (DDone(), deriveExpr(e, st)._1, st)
        }
        case DWhile(e, c) => deriveExpr(e, st) match {
          case (BoolO(true), st1) => derive (c, st1)
          case (BoolO(false), st1) => (DDone(), NoneO(), st1) // not sure about this one
        }
        case DDone() => (DDone(), NoneO(), st) // not sure about this one either
        case x => throw new DeriveException(x + " command invalid")
  }

  def deriveExpr(e: DExpr, st: Store): (Output, Store) = e match {
    case DTrue() => (BoolO(true), st)
    case DFalse() => (BoolO(false), st)
    case DNum(n) => (NumO(n), st)
    case DPlus(l, r) =>
      val (NumO(left), st1) = deriveExpr(l, st)
      val (NumO(right), st2) = deriveExpr(r, st1)
      (NumO(left + right), st2)
    case DLeq(l, r) =>
      val (NumO(left), st1) = deriveExpr(l, st)
      val (NumO(right), st2) = deriveExpr(r, st1)
      (BoolO(left <= right), st2)
    case DId(id) => (lookup(id, st), st);
    case x => throw new DeriveException(x + " expr invalid")
  }

  def lookup(x: String, st: Store): Output = st match {
    case Nil => throw new DeriveException("could not find identifier " + x + " in store")
    case Assign(id, value) :: st1 => if (x == id) deriveExpr(value, st)._1 else lookup (x, st1)
  }

  def deriveExpr(e: DExpr): (Output, Store) = deriveExpr(e, Nil)
  def derive(e: DExpr): (Command, Output, Store) = derive(e, Nil)
}

