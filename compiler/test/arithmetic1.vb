Option Hyper On

As Player1
Sub Main()

  Dim a As Byte = 250
  dim b As Byte = 20
  dim c As UInt16
  a += b
  if a = 255 Then
    Print("OK")
  Else
    Print("Error")
  End If

  a = 250
  b = 20
  c = a + b - 30
  if c = 240 Then
    Print("OK")
  Else
    Print("Error")
  End If

  c = 16384
  a = c
  if a = 255 Then
    Print("OK")
  Else
    Print("Error")
  End If

  c = 120
  a = c + 10
  if a = 130 Then
    Print("OK")
  Else
    Print("Error")
  End If
  
  c = 40
  b = 200
  a = b - c
  if a = 160 Then
    Print("OK")
  Else
    Print("Error")
  End If
  Me.CustomScore = a
  Leaderboard.Show(CustomScore)
  
  c = 40
  a = 200 - 2*c
  if a = 120 Then
    Print("OK")
  Else
    Print("Error")
  End If
  Me.Minerals = a

  c = 40
  a = 200 - c*2
  if a = 120 Then
    Print("OK")
  Else
    Print("Error")
  End If
  Me.Gas = a

End Sub


