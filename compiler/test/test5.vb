As Player1
Sub New()
  TalkingPortrait(TerranMarine, 2000)
  CreateUnit(1, TerranMarine, "TestLocation")
  Unit(TerranMarine, "TestLocation").Life = 50
  Unit(TerranMarine, "TestLocation").MoveOrder("TestLocation2")
End Sub
