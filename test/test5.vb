As Player1
Sub New()
  TalkingPortrait("Terran Marine", 2000)
  CreateUnit(1, "Terran Marine", "TestLocation")
  Unit("Terran Marine", "TestLocation").Life = 50
  Unit("Terran Marine", "TestLocation").MoveOrder("TestLocation2")
End Sub
