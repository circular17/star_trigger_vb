Option Hyper On

Const CONSTANT_A = 3
const CONSTANT_B = 2
Const CONSTANT_C = 8

As Player1
Sub Main()
  
  Print("hello world")

  Dim q As Byte = 0
  if q + 5 = 5 Then
    Print("0. correct result")
  Else
    Print("0. incorrect result")
  End If

  if q + 4 < 5 Then
    Print("0b. correct result")
  Else
    Print("0b. incorrect result")
  End If

  if q + 6 > 5 Then
    Print("0c. correct result")
  Else
    Print("0c. incorrect result")
  End If

  Dim x As Byte = CONSTANT_A
  Dim y As Byte = x + CONSTANT_B
  
  if y = CONSTANT_A + CONSTANT_B Then
    Print("1. correct result")
  Else
    Print("1. incorrect result :(")
  End If
 
  Dim z As Byte = y + CONSTANT_C
 
  if z = CONSTANT_A + CONSTANT_B + CONSTANT_C Then
    print("2. correct result")
  Else
    print("2. incorrect result :(")
  End If

  x = 3
  y = 6
  While x <> 4
    x += 1
  End While

  if x = 4 Then
    print("3. correct result")
  Else
    print("3. incorrect result :(")
  End If

  While x+y > 0
    x-= 1
    y-= 1
    Minerals(1) += 1
  End While
  
  if Minerals(1) = 6 Then
    print("4. correct result")
  Else
    print("4. incorrect result :(")
  End If


End Sub

