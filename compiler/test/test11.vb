Const LurkerProp1 As UnitProperties = {.Life = 10, .Burrowed = True}

Sub New()
    Player1.CreateUnit(1, "Zerg Lurker", "TestLocation") With LurkerProp1
End Sub

