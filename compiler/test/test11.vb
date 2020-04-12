Dim LurkerProp1 As New UnitProperties With {.Life = 10, .Burrowed = True}

Sub Main()
    Player1.CreateUnit(1, Zerg.Lurker, "TestLocation") With LurkerProp1
End Sub

