Const Loc = "TestLocation"
Dim MarineProp As New UnitProperties With {.Life = 50}

Class Player1

  Sub Main()

      CreateUnit(1, Protoss.Probe, Loc)
      Units(Protoss.Probe, Loc).Take(1).Shield = 0
      Units(Protoss.Probe, Loc).Take(1).Life = 50

      Units(AnyUnit, Loc).Take(1).Resource = 53
      Units(Protoss.HighTemplar, Loc).Take(1).Energy = 100
      Units(Protoss.Reaver, Loc).Take(1).HangarCount = 3

      CreateUnit(1, Terran.Marine, Loc, MarineProp)
      Units(Terran.Marine, Loc).Take(1).Life = 100
      Units(Terran.Marine, Loc).Take(1).Properties = MarineProp
      Units(Terran.Marine, Loc).Invincible = True

  End Sub

End Class
