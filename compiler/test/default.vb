Class AllPlayers

  On True
      Minerals(Me) = 50
      Stop
  End On

  On UnitCount(Unit.Buildings) <= 0
      Defeat()
      Stop
  End On

  On NonAlliedVictoryPlayers.UnitCount(Unit.Buildings) <= 0
      Victory()
      Stop
  End On

End Class
