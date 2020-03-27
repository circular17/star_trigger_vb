Dim Count as Byte
Dim Distraction as Byte

On Count = 1
  AllPlayers.Print("Finished countdown!")
End On

Sub New()
  Count = 50  
End Sub

On True
  Count -= 1
  Distraction -=1
End On

