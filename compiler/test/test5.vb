As Player1
Sub New()
  TalkingPortrait(Unit.TerranMarine, 2000)
  CreateUnit(1, Unit.TerranMarine, "TestLocation")
  Units(Unit.TerranMarine, "TestLocation").Life = 50
  Units(Unit.TerranMarine, "TestLocation").MoveOrder("TestLocation2")
End Sub
