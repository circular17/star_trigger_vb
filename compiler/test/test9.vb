As Player2
Sub New()
  CreateUnit(1, TerranMarine, "TestLocation")
  CreateUnit(1, TerranDropship, "TestLocation")
  RunAIScript(AI.EnterTransport, "TestLocation")
End Sub
