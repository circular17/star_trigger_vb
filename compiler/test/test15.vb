Dim PlayerScore(8) As Byte

On Player2.DeathCount(Terran.Marine) > 0
  Player2.DeathCount(Terran.Marine) -= 1
  PlayerScore(1) += 1
  PlayerScore(2) += 1
End On

Sub Main()
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
