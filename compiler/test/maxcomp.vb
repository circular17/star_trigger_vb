Option Hyper Off

Dim HasMostKills(8) As Boolean
Dim TestNot As Boolean

Class Player8

    Sub Main()
        TestNot = Not TestNot
    End Sub

    On True
        For I = 1 To 8
            If HasMostKills(I) Then
                HasMostKills(I) = False
                AllPlayers.Print("Player " & I & " has most kills")
            End If
        Next
    End On

End Class

As AllPlayers
On KillCount(Men) = Max
    HasMostKills(Me) = True
End On
