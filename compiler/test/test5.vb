As Player1
Sub New()
  TalkingPortrait(Terran.Marine, 2000)
  CreateUnit(1, Terran.Marine, "TestLocation")
  Units(Terran.Marine, "TestLocation").Life = 50
  Units(Terran.Marine, "TestLocation").MoveOrder("TestLocation2")
End Sub
