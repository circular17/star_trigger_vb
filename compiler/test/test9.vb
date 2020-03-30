As Player2
Sub Main()
  CreateUnit(1, Terran.Marine, "TestLocation")
  CreateUnit(1, Terran.Dropship, "TestLocation")
  RunAIScript(AI.EnterTransport, "TestLocation")
End Sub
