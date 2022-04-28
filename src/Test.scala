//test: Test

import Derive.deriveExpr
import Step.step
import org.scalatest.FunSuite

class Test extends FunSuite {
  test("derive 5") {
    assertResult(NumO(5)) {
      deriveExpr(step(BNum(5)))
    }
  }
}
