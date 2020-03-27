Dim foo As Byte = 6

As AllPlayers
On True

    If foo >= 5 Then
      Print("6 >= 5")
    End If
    
    If foo > 5 Then
      Print("6 > 5")
    End If
    
    If foo < 2 Then
      Print("6 < 2")
    End If
    
    If foo < 12 Then
      Print("6 < 12")
    End If
    
    If foo <= 7 Then
      Print("6 <= 7")
    End If 
    
    If foo <= 4 Then
      Print("6 <= 4")
    End If
    
    If foo >= 13 Then
      Print("6 >= 13")
    End If
    
    If foo >= 2 Then
      Print("6 >= 2")
    End If

    Wait(4000)

End On

