Const LurkerProp1 As UnitProperties = {.Life = 10, .Burrowed = True}

Sub New()
    Player1.CreateUnit(1, Unit.ZergLurker, "TestLocation") With LurkerProp1
End Sub

