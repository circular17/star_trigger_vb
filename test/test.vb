Option Hyper On

Const MAX_SWAPS = 3

Dim AllowedSwaps As Byte = MAX_SWAPS

As Player1
Sub New()
    CreateUnit(1, "Terran Marine", "TestLocation")
End Sub

As Player1
When UnitCount("Terran Marine", "TestLocation2") > 0
  If AllowedSwaps = 0 Then
    Print("Sorry, you have no more swaps left.")
    Return
  End If

  AllowedSwaps -= 1
  KillUnit(1, "Terran Marine", "TestLocation2")
  CreateUnit(1, "Protoss Zealot", "TestLocation")
  
  Print("Here is your zealot.")
End When

As Player1
When UnitCount("Protoss Zealot", "TestLocation2") > 0
  If AllowedSwaps = 0 Then
    Print("Sorry, you have no more swaps left.") 
    Return
  End If

  AllowedSwaps -= 1
  KillUnit(1, "Protoss Zealot", "TestLocation2")
  CreateUnit(1, "Terran Marine", "TestLocation")
  
  Print("Here is your marine.")
End When


