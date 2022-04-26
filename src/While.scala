import scala.util.{Failure, Success}

case class NotImplementedException(s: String) extends RuntimeException(s)
case class ParseException(s: String) extends RuntimeException(s)
case class DeriveException(s: String) extends RuntimeException(s)

object Reader extends JavaTokenParsers {

  def read(text: String): BExpr = {

  }

  
}
//BExpr, or Basic Expression.
sealed abstract class BExpr
case class BNum(n: Int) extends BExpr
case class BStr(s:String) extends BExpr
case class BList(list: List[BExpr]) extends BExpr

//DExpr, or Derived Expression.
sealed abstract class DExpr
case class DTrue() extends DExpr
case class DFalse() extends DExpr
case class DNum(n: Int) extends DExpr
case class DPlus(l: DExpr, r: DExpr) extends DExpr
case class DLeq(l: DExpr, r: DExpr) extends DExpr
case class DId(c: String) extends DExpr

case class DCommand(l:DExpr, r:DExpr) extends DExpr //???
case class DAssign(id:DId, e:DExpr) extends DExpr
case class DPrint(e: DExpr) extends DExpr
case class DWhile(e:DExpr, c:DCommand) extends DExpr//???

case class DDone() extends DExpr

object Parser {
  def parse(str: String): DExpr = parse(Reader.read(str))

  def parse(bexpr: BExpr): DExpr = bexpr match {

  }
}

object Desugar {
  def desugar(e: DExpr): DExpr = e match {

  }
}

object Interp {
  def interp(e: DExpr): Any = e match {

  }
}

