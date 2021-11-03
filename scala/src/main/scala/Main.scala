package dnd_dice

import scalaprob.probability._
import spire.math.Rational

object Main extends App {
  //euler.problem1.test()

  val cha = 5
  val prof = 4
  val wpn_mod = 3
  val hexblade = true

  val att_mod = cha + prof + wpn_mod
  val dam_mod = if (hexblade) cha + prof + wpn_mod else cha + wpn_mod

  val raym =
    Attacker(att_mod, List(8), dam_mod, List(20), 8 + cha + prof, "Raym")
  val orc = Target(14, 3)

  val attack = spells.swiftness(raym, orc, dice.disadvantage)

  val alpha = 0.1
  val attack_CI = (attack.percentile(Rational(alpha/2)), attack.percentile(Rational(1 - alpha/2)))

  //val attack_density = probability.attack_density(raym,orc,raym.dam_dice,"no")

  // presentation.run()
  val d6 = DeMoivre(6)

  //println(attack.distribution.mapValues(_.doubleValue).toSeq.sortBy(_._1))

  println(attack.distribution.get(65).get.doubleValue)

  println(
    // testDist.getDensity.indices.map(k => testDist.getProb(k + testDist.start))
    //save_density(raym, orc, List(6,6), 0, true, "no").getDensity.map(presentation.roundAt(4))
    // attack_density.map(presentation.roundAt(5))
    // convolution(List(Die(6).distribution, Die(6).distribution))
    attack_CI
    // List(1,2,3,4)
    // probability.percentile(0.95, probability.distrToDensity(probability.d20_distribution("no")))
    // probability.save_density(raym,orc,List(6,6),0,true,"no")
    // probability.d20_distribution("adv").map(presentation.roundAt(5)(_))
    // probability.distrToDensity(probability.d20_distribution("adv")).map(presentation.roundAt(5)(_))
    // probability.dam_density(List(6,6)).map(presentation.roundAt(5)(_))
    // probability.expect(attack_density)
    // probability.attack_distribution(raym,orc,raym.dam_dice,"adv").map(presentation.roundAt(4)(_))
    // probability.convolution_dam_dice(List(6,6)).map(presentation.roundAt(5))
  )
}
