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
End Sub


