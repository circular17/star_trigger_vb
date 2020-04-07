Option Hyper Off

Class Player8

    Sub Main()
        CreateMarines(10, True)
    End Sub

    Sub CreateMarines(Count As Byte, Other As Boolean)
        If Other Then
            CreateUnit(Count, Terran.Firebat)
        Else
            CreateUnit(Count, Terran.Marine)
        End If
    End Sub

End Class
