On Player1.UnitCount(Unit.TerranMarine, "TestLocation") = 0
  Player1.Units(1, Unit.TerranMarine).Remove()
  Player1.CreateUnit(1, Unit.TerranMarine, "TestLocation") With {.Invincible = True}
End On

