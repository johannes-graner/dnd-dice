package dnd_dice

object presentation {
    import character._

    def listToString(lst: List[AnyVal], delim: String): String = {
        def pad(str: String): String = {
            if (str.length() < 4)
                str ++ " " * (4 - str.length())
            else
                str
        }
        if (lst.length == 1)
            pad(lst.head.toString())
        else
            pad(lst.head.toString()) ++ 
            delim ++
            listToString(lst.tail, delim)
    }

    def roundAt(p: Int)(n: Double): Double = {
            val s = math pow (10, p)
            (math round n * s) / s
    }

    def roundAt1(n: Double): Double = {
        roundAt(1)(n)
    }

    def run(): Unit = {
        

        val cha = 5
        val prof = 3
        val raym = Attacker(cha + prof, List(8), cha, List(20), 8 + cha + prof, "Raym")
        val raym_hbc = raym.copy(dam_mod = raym.dam_mod + prof, crits = List(19,20), name = "Raym with Hexblade's Curse")
        val raym_hex = raym.copy(dam_dice = raym.dam_dice ++ List(6), name = "Raym with Hex")
        val raym_hex_hbc = raym_hbc.copy(dam_dice = raym_hbc.dam_dice ++ List(6), name = "Raym with Hex and Hexblade's Curse")
        
        val min_ac = 12
        val max_ac = 22
        val curr = raym

        val n = 100000
        def eval(ac: Int, adv: String, spell: (Attacker, Target, String) => List[Double]): Double = {
            probability.expect(spell(curr, Target(ac,0), adv))
            // spell(curr,Target(ac,0),adv)
            // sim.expect(n, spell, curr, Target(ac, 0), adv)
            // probability.expect(probability.attack_density(curr, Target(ac,0), curr.dam_dice, adv))
            // probability.expect(spells.swiftness_dens(curr, Target(ac,0), adv))
        }

        val acs = (min_ac to max_ac).toList
        val header = "adv \\ ac | " + listToString(acs, " ")
        val columns = header.length

        def damages(adv: String, spell: (Attacker, Target, String) => List[Double]): String = {
            listToString(
                acs.map(
                    ac => roundAt1(
                        eval(
                            ac,
                            adv,
                            spell
                        )
                    )
                ),
                " "
            )
        }

        def row(adv: String) = {
            println("-" * columns)
            // println(adv + " swift| " + damages(adv, spells.swiftness))
            println(adv + " sharp| " + damages(adv, spells.sharpened_blade(4)))
            // println(   "   attack| " + damages(adv, spells.weapon_attack))
        }

        println()
        println(curr.name)
        println(header)
        row("dis")
        row("no ")
        row("adv")
        row("tri")
        println()
    }
}
