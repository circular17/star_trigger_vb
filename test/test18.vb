Dim Count as Byte
Dim Distraction as Byte

When Count = 1
  AllPlayers.Print("Finished countdown!")
End When

Sub New()
  Count = 50  
End Sub

When True
  Count -= 1
  Distraction -=1
End When

