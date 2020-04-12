Sub Main()

    AllPlayers.Print("Hello world")

    If Present(1) Then
        AllPlayers.Print("Player 1 is here.")
    End If
    
    If Present(6) Then
        AllPlayers.Print("Player 6 is here.")
    End If

    If Present(1) And Present(2) Then
        AllPlayers.Print("Player 1 and 2 are here.")
    End If

    Gas(1) = CountIf(Present, True)
End Sub
