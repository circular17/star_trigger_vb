Const Loc = "TestLocation"
Const MarineProp As UnitProperties = {.Life = 50}

Class Player1

  Sub Main()

      CreateUnit(1, Protoss.Probe, Loc)
      Units(1, Protoss.Probe, Loc).Shield = 0
      Units(1, Protoss.Probe, Loc).Life = 50

      Units(1, AnyUnit, Loc).Resource = 53
      Units(1, Protoss.HighTemplar, Loc).Energy = 100
      Units(1, Protoss.Reaver, Loc).HangarCount = 3

      CreateUnit(1, Terran.Marine, Loc, MarineProp)
      Units(1, Terran.Marine, Loc).Life = 100
      Units(1, Terran.Marine, Loc).Properties = MarineProp
      Units(All, Terran.Marine, Loc).Invincible = True

  End Sub

End Class
