package dnd_dice

import scalaprob.probability._
import spire.math.Rational

class DiscreteTest extends org.scalatest.funsuite.AnyFunSuite {

  val att = Attacker(
    att_mod = 2,
    dam_dice = List(6),
    dam_mod = 2,
    crits = List(20),
    dc = 16
  )
  val target = Target(10, 5)
  val d20Dist = dice.advantage

  test("D20 Test") {
    assert(dice.advantage.getProb(20) == 0.0975)
    assert(1 - dice.advantage.distribution(10) == 0.75) // P(adv >= 11)
    assert(dice.disadvantage.getProb(1) == 0.0975)
  }

  test("Attack") {
    val dam_dice = att.dam_dice
    val attack = dice.attack(att, target, dam_dice, d20Dist)
    val attack2 = dice.attackN(2, att,target,dam_dice,d20Dist)
    val res_attack = dice.attack(att,target,dam_dice,d20Dist, Rational(1,2))
    val imm_attack = dice.attack(att,target,dam_dice,d20Dist, Rational.zero)

    assert(attack.checkDensity)
    assert(attack2.checkDensity)
    assert(res_attack.checkDensity)
    assert(attack2.getProb(1) == 0)
    assert(res_attack.getProb(attack.density.keySet.map(_/2)) == 1)
    assert{
      res_attack
        .density
        .keySet
        .toSeq
        .map { k => 
          attack.getProb(2*k) + attack.getProb(2*k+1) 
        }
        .equals(res_attack.density.values.toSeq)
    }
    assert(imm_attack.checkDensity)
    assert(imm_attack.getProb(0) == 1)
  }

  test("save") {
    val dam_dice = List(6,6,6)
    val save = dice.save(att, target, dam_dice, 0, true, d20Dist)
    val analytic = dice.save(att.copy(dc = 10), target.copy(save_mod = 0), List(6), 0, true, dice.d20)

    assert(save.checkDensity)
    assert(save.getProb(Set(0,1)) == save.getProb(1))
    assert(save.getProb(18) < save.getProb(17))
    assert(analytic.checkDensity)
    assert(analytic.getProb(Set(0,1,2,3)) == 3*analytic.getProb(Set(4,5,6))) // P(save) = 0.5, P(D < 3 | save) = 1, P(D > 3 | fail) = 0.5
  }

}
