Sub New()
    Do As {Player1,Player2}
        MissionObjectives = "Hello world!" & vbCrLf & 
                            "This is a multi-line string"
        CreateUnit(1, Unit.TerranMarine, "TestLocation")
        Alliance({Player1, Player2}) = Alliance.Ally
    End Do
End Sub

