Option Hyper On

Const CONSTANT_A = 3
Const CONSTANT_B = 2
Const CONSTANT_C = 8

As Player1
Sub Main()
  Print("hello world")
  
  Dim q As Byte = 0
  If q * 5 = 0 Then
    Print("0. correct result")
  Else
    Print("0. incorrect result")
  EndIf
  
  Dim x As Byte = CONSTANT_A
  Dim y As Byte = x * CONSTANT_B
  
  If y = CONSTANT_A * CONSTANT_B Then
    Print("1. correct result")
  Else
    Print("1. incorrect result :(")
  EndIf
  
  Dim z As Byte = y * CONSTANT_C
  
  If z = CONSTANT_A * CONSTANT_B * CONSTANT_C Then
    Print("2. correct result")
  Else
    Print("2. incorrect result :(")
  EndIf

End Sub
