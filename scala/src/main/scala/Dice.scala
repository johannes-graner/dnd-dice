package dnd_dice

import scalaprob.probability._
import spire.math._

object dice {

  implicit val num = spire.compat.numeric[Rational]

  private val d20DeMoivre = DeMoivre(20)

  private def nDk(n: Int, k: Int): DiscreteProb = {
    List
      .fill(n - 1)(DeMoivre(k))
      .foldLeft(DeMoivre(k).toDiscreteProb)(_.convolution(_))
  }

  private def takeHighest(
      die: DeMoivre,
      noDice: Int
  ): DiscreteProb = {
    val dieSize = die.density.size
    val density = (1 to dieSize)
      .map(k =>
        (
          k,
          Rational(k, dieSize).pow(noDice) - Rational(k - 1, dieSize)
            .pow(noDice)
        )
      )
      .toMap
    DiscreteProb(density)
  }

  private def takeLowest(die: DeMoivre, noDice: Int): DiscreteProb = {
    val revDens = takeHighest(die, noDice).density.toSeq.sortBy(_._1)
    DiscreteProb(revDens.map(_._1).zip(revDens.map(_._2).reverse).toMap)
  }

  val d20 = d20DeMoivre.toDiscreteProb
  val advantage = takeHighest(d20DeMoivre, 2)
  val elven_acc = takeHighest(d20DeMoivre, 3)

  val disadvantage = takeLowest(d20DeMoivre, 2)

  def attack(
      attacker: Attacker,
      target: Target,
      dam_dice: List[Int],
      d20Dist: DiscreteProb,
      dam_mult: Rational = 1
  ): DiscreteProb = {

    if (dam_mult == Rational.zero) {
      return OnePoint(0).toDiscreteProb
    }

    val deMoivreDice = dam_dice.map(DeMoivre(_).toDiscreteProb)
    val effectiveAC = target.ac - attacker.att_mod

    val critProb: Rational =
      d20Dist.getProb(
        attacker.crits.toSet
      ) // attacker.crits.map(d20Dist.getProb(_)).sum
    val critDam: DiscreteProb = {
      val dice = deMoivreDice ++ deMoivreDice
      dice
        .reduce(_.convolution(_))
        .shift(attacker.dam_mod)
    }
    val critDens: Map[Int, Rational] =
      critDam.density.mapValues(_ * critProb).toMap

    val missProb: Rational = d20Dist.distribution(max(1, effectiveAC))
    val missDens: Map[Int, Rational] = Map((0, missProb))

    val hitProb: Rational = 1 - critProb - missProb
    val hitDam: DiscreteProb = deMoivreDice
      .reduce(_.convolution(_))
      .shift(attacker.dam_mod)
    val hitDens: Map[Int, Rational] =
      hitDam.density.mapValues(_ * hitProb).toMap

    val densities = List(missDens, hitDens, critDens)
    val keys = densities.foldLeft(Set.empty[Int]) { (set, dens) =>
      set.union(dens.keySet)
    }

    val finalDensity =
      keys
        .map(k => (k, densities.map(_.getOrElse(k, Rational.zero)).sum))
        .map(pk => (if (pk._1 < 0) (0, pk._2) else pk))
        .groupBy(_._1)
        .mapValues(_.map(_._2).sum)
        .toMap
        .groupMapReduce(pair => (pair._1 * dam_mult).intValue)(_._2)(_ + _)

    DiscreteProb(finalDensity)
  }

  def attackN(
      n: Int,
      attacker: Attacker,
      target: Target,
      dam_dice: List[Int],
      d20Dist: DiscreteProb,
      dam_mult: Rational = 1
  ): DiscreteProb = {
    val attacks =
      List.fill(n)(attack(attacker, target, dam_dice, d20Dist, dam_mult))
    attacks.reduce(
      _.convolution(_)
    ) //tail.foldLeft(attacks.head)(_.convolution(_))
  }

  def save(
      attacker: Attacker,
      target: Target,
      dam_dice: List[Int],
      dam_mod: Int,
      half: Boolean,
      d20Dist: DiscreteProb,
      dam_mult: Rational = 1
  ): DiscreteProb = {

    if (dam_mult == Rational.zero) return OnePoint(0).toDiscreteProb

    val effectiveDC = attacker.dc - target.save_mod

    val save_dam_mult = if (half) Rational(1, 2) else Rational.zero

    val failProb = d20Dist.getProb((1 to effectiveDC).toSet)
    val fail_dam_dens =
      dam_dice
        .map(DeMoivre(_).toDiscreteProb)
        .reduce(_.convolution(_))
        .shift(dam_mod)
        .density

    val saveProb = 1 - failProb
    val save_dam_dens =
      if (save_dam_mult == 0) {
        Map(0 -> Rational.one)
      } else {
        fail_dam_dens
          .groupMapReduce(pair => (pair._1 * save_dam_mult).intValue)(_._2)(_ + _)
      }

    val final_dam_dens = fail_dam_dens.keySet
      .union(save_dam_dens.keySet)
      .map { k =>
        (
          k,
          fail_dam_dens.getOrElse(k, Rational.zero) * failProb +
            save_dam_dens.getOrElse(k, Rational.zero) * saveProb
        )
      }
      .toMap
      .groupMapReduce(pair => (pair._1 * dam_mult).intValue)(_._2)(_ + _)
    DiscreteProb(final_dam_dens)
  }

  /*
  import util.Random
  val sharpened = false

  def dam_calc(dam_dice: List[Int]): Int = {
    dam_dice.map(d => 1 + Random.nextInt(d)).sum
  }

  def d20(): Int = {
    1 + Random.nextInt(20)
  }

  def attack(
      attacker: Attacker,
      target: Target,
      dam_dice: List[Int],
      adv: String
  ): Int = {
    val roll = adv match {
      case "adv" => List.fill(2)(d20()).max
      case "tri" => List.fill(3)(d20()).max
      case "dis" => List.fill(2)(d20()).min
      case _     => d20()
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

  def attackn(
      n: Int,
      attacker: Attacker,
      target: Target,
      dam_dice: List[Int],
      adv: String
  ): Int = {
    List.fill(n)(attack(attacker, target, dam_dice, adv)).sum
  }

  def save(
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
      case _     => d20()
    }
    if (roll + target.save_mod < attacker.dc)
      dam_calc(dam_dice) + dam_mod
    else if (half)
      (dam_calc(dam_dice) + dam_mod) / 2
    else
      0
  }
   */
}
