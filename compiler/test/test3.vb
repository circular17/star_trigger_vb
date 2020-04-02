Sub Main()
  Player1.Minerals += 50
End Sub

Class AllPlayers

  On Me.Minerals >= 25
    Print("We have the minerals!")
  End On

  On ElapsedTime >= 5
    Print("5 seconds have elapsed!")
  End On

End Class


