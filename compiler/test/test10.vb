Sub Main()
    Do As {Player1,Player2}
        MissionObjectives = "Hello world!" & vbCrLf & 
                            "This is a multi-line string"
        CreateUnit(1, Terran.Marine, "TestLocation")
        Alliance.Ally({Player1, Player2})
    End Do
End Sub

