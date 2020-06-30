package dnd_dice

object dice {
    import util.Random
    import character._
    val sharpened = false
    
    def dam_calc(dam_dice: List[Int]): Int = {
        dam_dice.map(d => 1 + Random.nextInt(d)).sum
    }

    def d20(): Int = {
        1 + Random.nextInt(20)
    }

    def attack (
        attacker: Attacker,
        target: Target,
        dam_dice: List[Int],
        adv: String
    ): Int = {
        val roll = adv match {
            case "adv" => List.fill(2)(d20()).max
            case "tri" => List.fill(3)(d20()).max
            case "dis" => List.fill(2)(d20()).min
            case _ => d20()
        }
        if (attacker.crits.contains(roll))
            if (sharpened)
                attacker.dam_mod +
                    List.fill(2)(dam_calc(dam_dice)).sum +
                    dam_calc(List(dam_dice.head))
            else
                attacker.dam_mod + List.fill(2)(dam_calc(dam_dice)).sum
        else if (roll == 1 || roll + attacker.att_mod < target.ac)
            0
        else
            attacker.dam_mod + dam_calc(dam_dice)
    }

    def attackn (
        n: Int,
        attacker: Attacker,
        target: Target,
        dam_dice: List[Int],
        adv: String
    ): Int = {
        List.fill(n)(attack(attacker, target, dam_dice, adv)).sum
    }

    def save (
        attacker: Attacker,
        target: Target,
        dam_dice: List[Int],
        dam_mod: Int,
        half: Boolean,
        adv: String
    ): Int = {
        val roll = adv match {
            case "adv" => List.fill(2)(d20()).max
            case "dis" => List.fill(2)(d20()).min
            case _ => d20()
        }
        if (roll + target.save_mod < attacker.dc)
            dam_calc(dam_dice) + dam_mod
        else if (half)
            (dam_calc(dam_dice) + dam_mod)/2
        else
            0
    }
}