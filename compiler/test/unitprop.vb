Const Loc = "TestLocation"
Const MarineProp As UnitProperties = {.Life = 50}

Sub Main()

    Player1.CreateUnit(1, Protoss.Probe, Loc)
    Player1.Units(1, Protoss.Probe, Loc).Shield = 0
    Player1.Units(1, Protoss.Probe, Loc).Life = 50

    Player1.Units(1, AnyUnit, Loc).Resource = 53
    Player1.Units(1, Protoss.HighTemplar, Loc).Energy = 100
    Player1.Units(1, Protoss.Reaver, Loc).HangarCount = 3

    Player1.CreateUnit(1, Terran.Marine, Loc, MarineProp)
    Player1.Units(1, Terran.Marine, Loc).Life = 100
    Player1.Units(1, Terran.Marine, Loc).Properties = MarineProp
    Player1.Units(All, Terran.Marine, Loc).Invincible = True

End Sub
