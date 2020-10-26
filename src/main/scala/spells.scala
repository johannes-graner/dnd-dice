/*package dnd_dice

import scalaprob.probability._

object spells {
    def swiftness(
        attacker: Attacker,
        target: Target,
        adv: String
    ): List[Double] = {
        probability.attackn_density(
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
        adv: String
    ): List[Double] = {
        probability.attack_density(
            attacker,
            target,
            attacker.dam_dice,
            adv
        )
    }

    def sharpened_blade(
        level: Int)(
        attacker: Attacker,
        target: Target,
        adv: String
    ): List[Double] = {
        if (level < 2)
            sharpened_blade(2)(attacker,target,adv)
        else
            probability.attack_density(
                attacker,
                target,
                attacker.dam_dice ++ List.fill(level + 1)(10),
                adv
            )
    }

    def steel_wind_strike(
        attacker: Attacker,
        target: Target,
        adv: String
    ): List[Double] = {
        probability.attack_density(
            attacker.copy(dam_mod = 0),
            target,
            List.fill(5)(10),
            adv
        )
    }
}*/