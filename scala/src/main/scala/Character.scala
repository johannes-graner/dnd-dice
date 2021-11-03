package dnd_dice

case class Attacker(
  att_mod: Int, 
  dam_dice: List[Int], 
  dam_mod: Int, 
  crits: List[Int], 
  dc: Int, 
  name: String = "N/A"
)

case class Target(ac: Int, save_mod: Int)
