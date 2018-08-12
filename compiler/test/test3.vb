Sub New()
  Player1.Minerals += 50
End Sub

As AllPlayers
When Me.Minerals >= 25
  Print("We have the minerals!")
End When

As AllPlayers
When ElapsedTime >= 5
  Print("5 seconds have elapsed!")
End When

