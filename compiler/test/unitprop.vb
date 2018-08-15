Const Loc = "TestLocation"
Const MarineProp As UnitProperties = {.Life = 50}

Sub New()

    Player1.CreateUnit(1, Unit.ProtossProbe, Loc)
    Player1.Units(1, Unit.ProtossProbe, Loc).Shield = 0
    Player1.Units(1, Unit.ProtossProbe, Loc).Life = 50

    Player1.Units(1, Unit.AnyUnit, Loc).Resource = 53
    Player1.Units(1, Unit.ProtossHighTemplar, Loc).Energy = 100
    Player1.Units(1, Unit.ProtossReaver, Loc).HangarCount = 3

    Player1.CreateUnit(1, Unit.TerranMarine, Loc, MarineProp)
    Player1.Units(1, Unit.TerranMarine, Loc).Life = 100
    Player1.Units(1, Unit.TerranMarine, Loc).Properties = MarineProp
    Player1.Units(All, Unit.TerranMarine, Loc).Invincible = True

End Sub
