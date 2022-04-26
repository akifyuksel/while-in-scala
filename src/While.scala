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
case class DTrue() extends DExpr
case class DFalse() extends DExpr
case class DNum(n: Int) extends DExpr
case class DPlus(l: DExpr, r: DExpr) extends DExpr
case class DLeq(l: DExpr, r: DExpr) extends DExpr
case class DId(c: String) extends DExpr

case class DCommand(l:DExpr, r:DExpr) extends DExpr // should be trait
case class DAssign(id:DId, e:DExpr) extends DExpr
case class DPrint(e: DExpr) extends DExpr
case class DWhile(e:DExpr, c:DCommand) extends DExpr // change according to fixing command

case class DDone() extends DExpr

object Step {
//  def step(str: String): DExpr = parse(read(str))

  def step(bexpr: BExpr): DExpr = bexpr match {
    case BNum(n) => DNum(n) // assuming dat parser elk getal in een BNum omzet
    case BStr(s) => s match {
      case "true" => DTrue()
      case "false" => DFalse()
      case x => DId(x) // free identifier
    }
    case BList(list) => list match {
      case l :: BStr("+") :: r :: Nil => DPlus(step(l), step(r))
      case l :: BStr("<=") :: r :: Nil => DLeq(step(l), step(r))
      case x => throw new StepException("not yet implemented")
    }
  }
}

object Derive {
  def Derive(e: DExpr): Any = e match {
    case _ => throw new DeriveException("not yet implemented")
  }
}

