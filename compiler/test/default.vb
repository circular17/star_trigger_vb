Class AllPlayers

  On True
      Me.Minerals = 50
      Stop
  End On

  On Me.UnitCount(Unit.Buildings) <= 0
      Defeat()
      Stop
  End On

  On NonAlliedVictoryPlayers.UnitCount(Unit.Buildings) <= 0
      Victory()
      Stop
  End On

End Class
