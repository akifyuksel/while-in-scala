//test: Test

import Derive.{Assign, derive, deriveExpr, deriveSmall}
import Step.step
import org.scalatest.FunSuite

class Test extends FunSuite {
  // 5
  test("derive(small) 5 fail") {
    intercept[StepException] {
//      derive(step(BNum(5)))
      deriveSmall(step(BNum(5)))
    }
  }

  // (x := 27)
  test("derive(small) assign") {
    assertResult(DDone(), NumO(27), List(Assign("x", NumO(27)))) {
//      derive(step(BList(List(BStr("x"), BStr(":="), BNum(27)))))
      deriveSmall(step(BList(List(BStr("x"), BStr(":="), BNum(27)))))
    }
  }

  // (+ := 1)
  test("parse assign keyword identifier") {
    intercept[StepException] {
      step(BList(List(BStr("+"), BStr(":="), BNum(1))))
    }
  }

  // ((x := 7) ; (print (5 + x)))
  test("deriveSmall seq") {
    assertResult(DSeq(DDone(), DPrint(DPlus(DNum(5), DId("x"))))) {
      val res = deriveSmall(step(BList(List(BList(List(BStr("x"), BStr(":="), BNum(7))), BStr(";"), BList(List(BStr("print"), BList(List(BNum(5), BStr("+"), BStr("x")))))))))
      (res._1)
    }
  }
  test("derive seq") {
    assertResult(DDone(), NumO(12), List(Assign("x", NumO(7)))) {
      derive(step(BList(List(BList(List(BStr("x"), BStr(":="), BNum(7))), BStr(";"), BList(List(BStr("print"), BList(List(BNum(5), BStr("+"), BStr("x")))))))))
    }
  }

  // (((x := 0) ; (while (x <= 10) do (x := (x + 1)) od)) ; (print x))
  test("deriveSmall while") {
    assertResult(DSeq(DSeq(DDone(),DWhile(DLeq(DId("x"),DNum(10)),DAssign(DId("x"),DPlus(DId("x"),DNum(1))))),DPrint(DId("x")))) {
      deriveSmall(step(
        BList(List(BList(List(BList(List(BStr("x"), BStr(":="), BNum(0))), BStr(";"),
          BList(List(BStr("while"), BList(List(BStr("x"), BStr("<="), BNum(10))), BStr("do"),
            BList(List(BStr("x"), BStr(":="), BList(List(BStr("x"), BStr("+"), BNum(1))))), BStr("od"))))), BStr(";"),
          BList(List(BStr("print"), BStr("x")))))
      ))._1
    }
  }
  test("derive while") {
  assertResult(NumO(11)) {
    derive(step(
      BList(List(BList(List(BList(List(BStr("x"), BStr(":="), BNum(0))), BStr(";"),
        BList(List(BStr("while"), BList(List(BStr("x"), BStr("<="), BNum(10))), BStr("do"),
          BList(List(BStr("x"), BStr(":="), BList(List(BStr("x"), BStr("+"), BNum(1))))), BStr("od"))))), BStr(";"),
        BList(List(BStr("print"), BStr("x")))))
      ))._2
    }
  }
}
