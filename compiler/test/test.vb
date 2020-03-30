Option Hyper On

Const MAX_SWAPS = 3

Dim AllowedSwaps As Byte = MAX_SWAPS

As Player1
Sub New()
    CreateUnit(1, Terran.Marine, "TestLocation")
End Sub

As Player1
On UnitCount(Terran.Marine, "TestLocation2") > 0
  If AllowedSwaps = 0 Then
    Print("Sorry, you have no more swaps left.")
    Return
  End If

  AllowedSwaps -= 1
  Units(Terran.Marine, "TestLocation2").Kill(1)
  CreateUnit(1, Protoss.Zealot, "TestLocation")
  
  Print("Here is your zealot.")
End On

As Player1
On UnitCount(Protoss.Zealot, "TestLocation2") > 0
  If AllowedSwaps = 0 Then
    Print("Sorry, you have no more swaps left.") 
    Return
  End If

  AllowedSwaps -= 1
  Units(Protoss.Zealot, "TestLocation2").Kill(1)
  CreateUnit(1, Terran.Marine, "TestLocation")
  
  Print("Here is your marine.")
End On


