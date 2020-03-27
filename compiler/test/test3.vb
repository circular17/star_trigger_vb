Sub New()
  Player1.Minerals += 50
End Sub

As AllPlayers
On Me.Minerals >= 25
  Print("We have the minerals!")
End On

As AllPlayers
On ElapsedTime >= 5
  Print("5 seconds have elapsed!")
End On

