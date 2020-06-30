object Main extends App {
  //euler.problem1.test()
  import dnd_dice._
  import dnd_dice.character._

  val cha = 5
  val prof = 3
  val raym = Attacker(cha + prof, List(8), cha, List(20), 8 + cha + prof, "Raym")
  val orc = Target(12,3)

  val attack_density = probability.attack_density(raym,orc,raym.dam_dice,"no")

  presentation.run()

  println(
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
