package dnd_dice

import scalaprob.probability._

object spells {

  def swiftness(
      attacker: Attacker,
      target: Target,
      adv: DiscreteProb
  ): DiscreteProb = {
    dice.attackN(
      4,
      attacker,
      target,
      attacker.dam_dice,
      adv
    )
  }

  def weapon_attack(
      attacker: Attacker,
      target: Target,
      adv: DiscreteProb
  ): DiscreteProb = {
    dice.attack(
      attacker,
      target,
      attacker.dam_dice,
      adv
    )
  }

  def sharpened_blade(level: Int)(
      attacker: Attacker,
      target: Target,
      adv: DiscreteProb
  ): DiscreteProb = {
    dice.attack(
      attacker,
      target,
      attacker.dam_dice ++ List.fill(1 + math.max(level, 2))(10),
      adv
    )
  }

  def steel_wind_strike(
      attacker: Attacker,
      target: Target,
      adv: DiscreteProb
  ): DiscreteProb = {
    dice.attack(
      attacker.copy(dam_mod = 0),
      target,
      List.fill(5)(10),
      adv
    )
  }

  def fireball(level: Int = 3)(
    attacker: Attacker,
    target: Target,
    adv: DiscreteProb
  ): DiscreteProb = {
    dice.save(
      attacker,
      target,
      List.fill(5 + math.max(level,3))(6),
      0,
      true,
      adv
    )
  }
}
