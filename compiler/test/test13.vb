Sub Main()

    AllPlayers.Print("Hello world")

    If Player1.Present Then
        AllPlayers.Print("Player 1 is here.")
    End If
    
    If Player6.Present Then
        AllPlayers.Print("Player 6 is here.")
    End If

    If Present(1) And Present(2) Then
        AllPlayers.Print("Player 1 and 2 are here.")
    End If

    Player1.Gas = CountIf(Present, True)
End Sub
