As Player2
Sub New()
  CreateUnit(1, Unit.TerranMarine, "TestLocation")
  CreateUnit(1, Unit.TerranDropship, "TestLocation")
  RunAIScript(AI.EnterTransport, "TestLocation")
End Sub
