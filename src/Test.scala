//test: Test

import Derive.{Assign, derive, deriveExpr}
import Step.step
import org.scalatest.FunSuite

class Test extends FunSuite {
  test("derive 5") {
    assertResult(NumO(5)) {
      deriveExpr(step(BNum(5)))._1
    }
  }

  test("derive assign") {
    assertResult(DDone(), NumO(27), List(Assign("x", DNum(27)))) {
      derive(step(BList(List(BStr("x"), BStr(":="), BNum(27)))))
    }
  }

  test("derive assign keyword identifier") {
    intercept[StepException] {
      step(BList(List(BStr("+"), BStr(":="), BNum(1))))
    }
  }

  test("derive seq") {
    assertResult(DDone(), NumO(12), List(Assign("x", DNum(7)))) {
      derive(step(BList(List(BList(List(BStr("x"), BStr(":="), BNum(7))), BStr(";"), BList(List(BStr("print"), BList(List(BNum(5), BStr("+"), BStr("x")))))))))
    }
  }
}
