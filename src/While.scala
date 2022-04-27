//scala.util.parsing.combinator.JavaTokenParsers

case class NotImplementedException(s: String) extends RuntimeException(s)
case class StepException(s: String) extends RuntimeException(s)
case class DeriveException(s: String) extends RuntimeException(s)

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

sealed trait Command extends DExpr // should be trait
case class DSeq(c1:Command, c2:Command) extends Command
case class DAssign(id:DId, e:DExpr) extends Command
case class DPrint(e: DExpr) extends Command
case class DWhile(e:DExpr, c:Command) extends Command // change according to fixing command
case class DDone() extends Command

object Step {
//  def step(str: String): DExpr = parse(read(str))

  def step(bexpr: BExpr): DExpr = bexpr match {
    case BNum(n) => DNum(n) // assuming dat parser elk getal in een BNum omzet
    case BStr(s) => s match {
      case "true" => DTrue()
      case "false" => DFalse()
      case "done" => DDone()
      case x => DId(x) // free identifier
    }
    case BList(list) => list match {
      case l :: BStr("+") :: r :: Nil => DPlus(step(l), step(r))
      case l :: BStr("<=") :: r :: Nil => DLeq(step(l), step(r))
      case l :: BStr(":=") :: r :: Nil => l match {
        case BStr(x) => DAssign(DId(x), step(r));
      }
      case BStr("print") :: expr :: Nil => DPrint(step(expr))
      case BStr("while") :: expr :: BStr("do") :: comm :: BStr("od") :: Nil => DWhile(step(expr), stepCommand(comm))
      case l :: BStr(";") :: r :: Nil => (l, r) match {
        case (BStr("done"), BStr("done")) => DSeq(DDone(), DDone())
        case (BStr("done"), BList(r)) => DSeq(DDone(), stepCommand(BList(r)))
        case (BStr("done"), BStr("done")) => DSeq(DDone(), DDone())
        case (BList(l), BStr("done")) => DSeq(stepCommand(BList(l)), DDone())
        case _ => throw new StepException("not a valid command sequence")
    }
      case l :: BStr("") :: r :: Nil => DLeq(step(l), step(r))
      case x => throw new StepException("not yet implemented")
    }
  }

  def stepCommand(bexpr: BExpr): Command = ???

}

object Derive {
  def Derive(e: DExpr): Any = e match {
    case _ => throw new DeriveException("not yet implemented")
  }

  def lookup(x: String): DExpr = ???
}

