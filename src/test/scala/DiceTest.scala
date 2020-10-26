package dnd_dice

import scalaprob.probability._
import spire.math.Rational

class DiscreteTest extends org.scalatest.funsuite.AnyFunSuite {

  test("D20 Test") {
    assert(dice.advantage.getProb(20) == 0.0975)
    assert(1 - dice.advantage.distribution(10) == 0.75) // P(adv >= 11)
    assert(dice.disadvantage.getProb(1) == 0.0975)
  }

  test("Attack") {
    val att = Attacker(
      att_mod = 2,
      dam_dice = List(6),
      dam_mod = 2,
      crits = List(20),
      dc = 20
    )
    val target = Target(10, 5)
    val dam_dice = att.dam_dice
    val d20Dist = dice.advantage

    val attack = dice.attack(att, target, dam_dice, d20Dist)
    val attack2 = dice.attackN(2, att,target,dam_dice,d20Dist)

    assert(attack.checkDensity)
    assert(attack2.checkDensity)
    assert(attack2.getProb(1) == 0)
  }

}
