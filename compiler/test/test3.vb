Sub Main()
  Minerals(1) += 50
End Sub

Class AllPlayers

  On Minerals(Me) >= 25
    Print("We have the minerals!")
  End On

  On ElapsedTime >= 5
    Print("5 seconds have elapsed!")
  End On

End Class


