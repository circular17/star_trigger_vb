Dim PlayerScore(8) As Byte

When Player2.DeathCount(Unit.TerranMarine) > 0
  Player2.DeathCount(Unit.TerranMarine) -= 1
  PlayerScore(1) += 1
  PlayerScore(2) += 1
End When

Sub New()
  Dim foo(4) As Byte
  foo(1) = 15
  foo(2) = 42

  If foo(1) = 15 Then
    AllPlayers.Print("foo(1) = 15")
  EndIf

  If foo(2)*2 = 84 Then
    AllPlayers.Print("foo(2)*2 = 84")
  EndIf
End Sub
