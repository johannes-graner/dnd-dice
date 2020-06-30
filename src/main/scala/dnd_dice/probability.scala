package dnd_dice

object probability {
    import character._

    def d20_hit_prob(ac: Int, adv: String): Double = {
        if (adv == "dis") 
            (math pow ((21 - ac)/(20: Double), 2))
        else {
            val exp = adv match {
                case "tri" => 3
                case "adv" => 2
                case _     => 1
            }
            1 - (math pow(((ac - 1):Double)/20, exp))
        }
    }

    def d20_distribution(adv: String): List[Double] = {
        List.range(1,21).map(1 - d20_hit_prob(_, adv))
    }

    def dam_distribution(dam_dice: List[Int]): List[Double] = {
        val offset: Int = dam_dice.length - 1
        List.fill(offset)(0.0) ++ convolution_dam_dice(dam_dice).scanLeft(0:Double)(_+_).tail
    }

    def dam_density(dam_dice: List[Int]): List[Double] = {
        val offset: Int = dam_dice.length - 1
        List.fill(offset)(0.0) ++ convolution_dam_dice(dam_dice)
    }

    def convolution_dam_dice(dam_dice: List[Int]): List[Double] = {
        val first = List.fill( dam_dice.head )( 1/(dam_dice.head:Double) )
        if (dam_dice.length == 1)
            first
        else {
            val recurse = convolution_dam_dice(dam_dice.tail)
            
            List.range[Int](0,first.length + recurse.length - 1)
                .map(conv(_, first, recurse))
        }
    }

    def convolution(densities: List[List[Double]]): List[Double] = {
        val first = densities.head
        if (densities.length == 1)
            first
        else {
            val recurse = convolution(densities.tail)
            List.range[Int](0,densities.head.length + recurse.length - 1)
                .map(conv(_, first, recurse))
        }
    }

    def conv(x: Int, L1: List[Double], L2: List[Double]): Double = {
        var res: Double = 0
        if (L1.length < L2.length) {
            conv(x,L2,L1)
        } else {
            val paddedL2 = L2 ++ List.fill(L1.length - L2.length)(0:Double)
            for (i <- 0 until L1.length) {
                if (x-i < L2.length && x-i >= 0)
                    res += L1(i)*L2(x-i)
            }
            res
        }
    }

    def distrToDensity(distr: List[Double]): List[Double] = {
        List(distr.head) ++ distr.tail.indices.map(i => distr(i+1) - distr(i))
    }

    def attack_density(
        attacker: Attacker,
        target: Target,
        dam_dice: List[Int],
        adv: String
    ): List[Double] = {
        val adjusted_ac: Int = target.ac - attacker.att_mod
        val d20_distr: List[Double] = d20_distribution(adv)

        val crit_prob: Double = 1 - d20_distr(20 - attacker.crits.length)
        val crit_dam: List[Double] = List.fill(attacker.dam_mod)(0.0) ++ 
                                        dam_density(dam_dice ++ dam_dice)

        val hit_prob: Double =  1 - d20_distr(adjusted_ac - 1) - crit_prob
        val hit_dam: List[Double] = List.fill(attacker.dam_mod)(0.0) ++ 
                                        dam_density(dam_dice) ++
                                        List.fill(dam_dice.sum)(0.0)

        d20_distr(adjusted_ac - 1) :: hit_dam
                                        .map(_*hit_prob)
                                        .zip(crit_dam.map(_*crit_prob))
                                        .map {case (x,y) => x + y}
    }

    def attackn_density(
        nattacks: Int,
        attacker: Attacker,
        target: Target,
        dam_dice: List[Int],
        adv: String
    ): List[Double] = {
        convolution(List.fill(nattacks)(attack_density(attacker,target,dam_dice,adv)))
    }

    def save_density(
        attacker: Attacker,
        target: Target,
        dam_dice: List[Int],
        dam_mod: Int,
        half: Boolean,
        adv: String
    ): List[Double] = {
        val adjusted_dc = attacker.dc - target.save_mod
        val d20_distr: List[Double] = d20_distribution(adv)

        val save_prob: Double = 1 - d20_distr(adjusted_dc - 1)
        val save_dam: List[Double] = {
            if (half) {
                val rolled = List.fill(dam_mod)(0.0) ++ dam_density(dam_dice)
                ((rolled.head ::
                    List.range(0,rolled.length/2 - 1)
                        .map(i => rolled.tail(2*i) + rolled.tail(2*i+1))) :+
                    rolled(rolled.length-1)) ++
                    List.fill((dam_dice.sum + dam_mod)/2)(0.0)
            } else
                1 :: List.fill(dam_dice.sum + dam_mod)(0.0)
        }

        val fail_prob: Double = 1 - save_prob
        val fail_dam: List[Double] = 0.0 :: dam_density(dam_dice)

        save_dam.map(_*save_prob)
                .zip(fail_dam.map(_*fail_prob))
                .map{ case (x,y) => x + y}
    }

    def expect(density: List[Double]): Double = {
        // Assumes distribution on {0,1,..,density.length-1}
        density.indices.zip(density).map{case (x,y) => x*y}.sum
    }

    def percentile(perc: Double, density: List[Double]): Int = {
        // Same assumption as above
        val distr = density.scanLeft(0.0)(_+_).tail.map(presentation.roundAt(5))
        distr.foldLeft(0){case (acc, x) => if (x < perc) acc + 1 else acc}
    }
}