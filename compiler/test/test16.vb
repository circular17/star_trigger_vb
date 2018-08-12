When Player1.UnitCount("Terran Marine", "TestLocation") = 0
  Player1.Unit(1, "Terran Marine").Remove()
  Player1.CreateUnit(1, "Terran Marine", "TestLocation") With {.Invincible = True}
End When

