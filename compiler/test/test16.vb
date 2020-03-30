On Player1.UnitCount(Terran.Marine, "TestLocation") = 0
  Player1.Units(Terran.Marine).Remove(1)
  Player1.CreateUnit(1, Terran.Marine, "TestLocation") With {.Invincible = True}
End On

