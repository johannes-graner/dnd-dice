package dnd_dice

import scala.annotation.switch

object probability {
    import character._

    case class Distribution(
        private val density: List[Double],
        start: Int = 0
    ) {
        def getDensity: List[Double] = {
            // If start < 0, list is padded with -start zeros on the right.
            if (start < 0) 
                density ++ List.fill(-start)(0.0)
            else
                List.fill(start)(0.0) ++ density
        }

        def addStart(mod: Int): Distribution = copy(start = start + mod)

        def checkDensity: Boolean = density.sum == 1.0 && density.map(_ >= 0).fold(true)(_ & _)
        def distribution: List[Double] = density.scanLeft(0: Double)(_+_).tail
        def moment(n: Int): Double = density.indices.map(k => math.pow(k + start,n)).zip(density).map{case (x,y) => x*y}.sum
        def mean: Double = moment(1)
        def variance: Double = moment(2) - math.pow(mean,2)


        def getProb(k: Int): Double = {
            val adj_k = k - start
            if (adj_k < 0 || adj_k >= density.length)
                0
            else
                density(adj_k)
        }

        def convolution(other: Distribution): Distribution = {
            var this_dens: List[Double] = List.empty
            var other_dens: List[Double] = List.empty
            if (start >= 0) {
                if (other.start >= 0) {
                    this_dens = getDensity
                    other_dens = other.getDensity
                } else {
                    this_dens = List.fill(-other.start)(0.0) ++ getDensity
                    other_dens = other.density
                }
            } else {
                if (other.start >= 0) {
                    this_dens = density
                    other_dens = List.fill(-start)(0.0) ++ density
                } else {
                    this_dens = List.fill(-other.start)(0.0) ++ getDensity
                    other_dens = List.fill(-start)(0.0) ++ density
                }
            }

            Distribution(
                List.range[Int](0,this_dens.length + other_dens.length - 1)
                    .map(conv(_, this_dens, other_dens)).drop(start + other.start),
                start + other.start
            )
        }
    }

    case class Die(size: Int) {
        def distribution: Distribution = Distribution(List.fill(size)(1/(size: Double)), 1)
    }

    private def conv(x: Int, L1: List[Double], L2: List[Double]): Double = {
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

    private def d20_hit_prob(ac: Int, adv: String): Double = {
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

    private def d20_distribution(adv: String): List[Double] = {
        List.range(1,21).map(1 - d20_hit_prob(_, adv))
    }

    def dam_distribution(dam_dice: List[Int]): Distribution = {
        /* val start: Int = dam_dice.length - 1
        List.fill(offset)(0.0) ++ convolution_dam_dice(dam_dice).scanLeft(0:Double)(_+_).tail */
        val dice: List[Distribution] = dam_dice.map(Die(_).distribution)
        dice.tail.fold(dice.head)(_.convolution(_))
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
        // convolution(dam_dice.map(d => List.fill(d)(1/(d: Double))))
    }

    /* def convolution(densities: List[List[Double]]): List[Double] = {
        val first = densities.head
        if (densities.length == 1)
            first
        else {
            val recurse = convolution(densities.tail)
            List.range[Int](0,densities.head.length + recurse.length - 1)
                .map(conv(_, first, recurse))
        }
    } */
    def convolution(densities: List[Distribution]): Distribution = {
        densities.tail.fold(densities.head)(_.convolution(_))
    }


    def distrToDensity(distr: List[Double]): List[Double] = {
        distr.head :: distr.tail.indices.toList.map(i => distr(i+1) - distr(i))
    }

    def attack_density(
        attacker: Attacker,
        target: Target,
        dam_dice: List[Int],
        adv: String
    ): Distribution = {
        val adjusted_ac: Int = target.ac - attacker.att_mod
        val d20_distr: List[Double] = d20_distribution(adv)

        val crit_prob: Double = 1 - d20_distr(20 - attacker.crits.length)
        val crit_dam: List[Double] = dam_distribution(dam_dice ++ dam_dice)
                                        .addStart(attacker.dam_mod - 1)
                                        .getDensity
                                    /* List.fill(attacker.dam_mod)(0.0) ++ 
                                        dam_density(dam_dice ++ dam_dice) */ 

        val hit_prob: Double =  1 - d20_distr(adjusted_ac - 1) - crit_prob
        val hit_dam: List[Double] = dam_distribution(dam_dice)
                                        .addStart(attacker.dam_mod - 1)
                                        .getDensity ++ List.fill(dam_dice.sum)(0.0)
                                    /* List.fill(attacker.dam_mod)(0.0) ++ 
                                         dam_density(dam_dice) ++
                                         List.fill(dam_dice.sum)(0.0)  */

        Distribution(d20_distr(adjusted_ac - 1) :: hit_dam
                                        .map(_*hit_prob)
                                        .zip(crit_dam.map(_*crit_prob))
                                        .map {case (x,y) => x + y}
        )
    }

    def attackn_density(
        nattacks: Int,
        attacker: Attacker,
        target: Target,
        dam_dice: List[Int],
        adv: String
    ): Distribution = {
        convolution(List.fill(nattacks)(attack_density(attacker,target,dam_dice,adv)))
    }

    def save_density(
        attacker: Attacker,
        target: Target,
        dam_dice: List[Int],
        dam_mod: Int,
        half: Boolean,
        adv: String
    ): Distribution = {
        val adjusted_dc = attacker.dc - target.save_mod
        val d20_distr: List[Double] = d20_distribution(adv)

        val save_prob: Double = 1 - d20_distr(adjusted_dc - 1)
        val save_dam: List[Double] = {
            if (half) {
                val rolled = dam_distribution(dam_dice).addStart(dam_mod).getDensity
                ((rolled.head ::
                    List.range(0,rolled.length/2 - 1)
                        .map(i => rolled.tail(2*i) + rolled.tail(2*i+1))) :+
                    rolled(rolled.length-1)) ++
                    List.fill((dam_dice.sum + dam_mod)/2)(0.0)
            } else
                1 :: List.fill(dam_dice.sum + dam_mod)(0.0)
        }

        val fail_prob: Double = 1 - save_prob
        val fail_dam: List[Double] = dam_distribution(dam_dice).getDensity

        val raw_dens = save_dam.map(_*save_prob)
                        .zip(fail_dam.map(_*fail_prob))
                        .map{ case (x,y) => x + y}
        val start = raw_dens.filter(_ == 0.0).length

        Distribution(raw_dens.drop(start), start)
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
