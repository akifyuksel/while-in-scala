//test: Test

import Derive.{Assign, derive, deriveExpr}
import Step.step
import org.scalatest.FunSuite

class Test extends FunSuite {
  // 5
  test("derive 5 fail") {
    intercept[StepException] {
      derive(step(BNum(5)))
    }
  }

  // (x := 27)
  test("derive assign") {
    assertResult(DDone(), NumO(27), List(Assign("x", NumO(27)))) {
      derive(step(BList(List(BStr("x"), BStr(":="), BNum(27)))))
    }
  }

  // (+ := 1)
  test("derive assign keyword identifier") {
    intercept[StepException] {
      step(BList(List(BStr("+"), BStr(":="), BNum(1))))
    }
  }

  // ((x := 7) ; (print (5 + x)))
  test("derive seq") {
    assertResult(DDone(), NumO(12), List(Assign("x", NumO(7)))) {
      derive(step(BList(List(BList(List(BStr("x"), BStr(":="), BNum(7))), BStr(";"), BList(List(BStr("print"), BList(List(BNum(5), BStr("+"), BStr("x")))))))))
    }
  }

  // (((x := 0) ; (while (x <= 10) do (x := (x + 1)) od)) ; (print x))
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
