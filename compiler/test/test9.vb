As Player2
Sub New()
  CreateUnit(1, "Terran Marine", "TestLocation")
  CreateUnit(1, "Terran Dropship", "TestLocation")
  RunAIScript(AI.EnterTransport, "TestLocation")
End Sub
