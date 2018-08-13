When Player1.UnitCount(TerranMarine, "TestLocation") = 0
  Player1.Unit(1, TerranMarine).Remove()
  Player1.CreateUnit(1, TerranMarine, "TestLocation") With {.Invincible = True}
End When

