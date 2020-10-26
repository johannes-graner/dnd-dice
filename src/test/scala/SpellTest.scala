package dnd_dice

import scalaprob.probability._
import spire.math.Rational

class SpellTest extends org.scalatest.funsuite.AnyFunSuite {

  val strongAtt = Attacker(
    att_mod = 2,
    dam_dice = List(6),
    dam_mod = 2,
    crits = List(20),
    dc = 20
  )
  val weakAtt = strongAtt.copy(dam_mod = -2)

  val target = Target(10, 5)
  val dam_dice = strongAtt.dam_dice

  test("Weapon Attack") {
    val spell = spells.weapon_attack(strongAtt, target, dice.d20)

    print(spell)
    assert(spell.checkDensity)
    assert(
      spell.density.keySet == 
        (0 to strongAtt.dam_dice.sum * 2 + strongAtt.dam_mod).toSet
        -- (1 to strongAtt.dam_dice.length + strongAtt.dam_mod - 1)
    )
  }

  test("Swiftness") {
    val spell = spells.swiftness(strongAtt,target,dice.d20)
    val dam_dice = strongAtt.dam_dice
    val dam_mod = strongAtt.dam_mod
    val max_dam = 8*dam_dice.sum + 4*dam_mod

    assert(spell.checkDensity)
    assert((1 to dam_dice.length + dam_mod - 1).filter(spell.getProb(_) != 0).isEmpty)
    assert(spell.getProb(max_dam) < spell.getProb(max_dam - 1))
  }

  test("Steel Wind Strike") {
    val spell = spells.steel_wind_strike(strongAtt,target,dice.d20)

    assert(spell.checkDensity)
    assert(spell.getProb(100) > 0)
    assert(spell.getProb(101) == 0)
    assert(spell.getProb(5) > 0)
    assert(spell.getProb(4) == 0)
  }

  test("Sharpened Blade") {
    val spell2 = spells.sharpened_blade(2)(strongAtt,target,dice.d20)
    val spell3 = spells.sharpened_blade(3)(strongAtt,target,dice.d20)

    assert(spell2.checkDensity)
    assert(spell3.checkDensity)
    assert(spell2.density.keys.max == spell3.density.keys.max - 2*10)
  }

}
