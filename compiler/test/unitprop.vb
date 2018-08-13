Const Loc = "TestLocation"
Const MarineProp As UnitProperties = {.Life = 50}

Player1.CreateUnit(1, ProtossProbe, Loc)
Player1.GetUnit(1, ProtossProbe, Loc).Shield = 0
Player1.GetUnit(1, ProtossProbe, Loc).Life = 50

Player1.GetUnit(1, AnyUnit, Loc).Resource = 53
Player1.GetUnit(1, ProtossHighTemplar, Loc).Energy = 100
Player1.GetUnit(1, ProtossReaver, Loc).HangarCount = 3

Player1.CreateUnit(1, TerranMarine, Loc, Properties)
Player1.GetUnit(1, TerranMarine, Loc).Life = 100
Player1.GetUnit(1, TerranMarine, Loc).Properties = MarineProp
Player1.GetUnit(All, TerranMarine, Loc).Invincible = True
