Option Hyper On

Const MAX_SWAPS = 3

Dim AllowedSwaps As Byte = MAX_SWAPS

As Player1
Sub New()
    CreateUnit(1, Unit.TerranMarine, "TestLocation")
End Sub

As Player1
When UnitCount(Unit.TerranMarine, "TestLocation2") > 0
  If AllowedSwaps = 0 Then
    Print("Sorry, you have no more swaps left.")
    Return
  End If

  AllowedSwaps -= 1
  KillUnit(1, Unit.TerranMarine, "TestLocation2")
  CreateUnit(1, Unit.ProtossZealot, "TestLocation")
  
  Print("Here is your zealot.")
End When

As Player1
When UnitCount(Unit.ProtossZealot, "TestLocation2") > 0
  If AllowedSwaps = 0 Then
    Print("Sorry, you have no more swaps left.") 
    Return
  End If

  AllowedSwaps -= 1
  KillUnit(1, Unit.ProtossZealot, "TestLocation2")
  CreateUnit(1, Unit.TerranMarine, "TestLocation")
  
  Print("Here is your marine.")
End When


