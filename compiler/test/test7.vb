Option Hyper On

Const LUCKY_NUMBER = 100

As Player1
When Countdown = 0
  Countdown = 2 + Rnd()*4
  If Rnd()*256 >= LUCKY_NUMBER Then
    Print("Lucky you")
    CreateUnit(1 + Rnd()*4, TerranMarine, "TestLocation")
    Unit(TerranMarine, "TestLocation").AttackOrder("TestLocation2")
  Else
    Print("Try again next time")
    Player2.CreateUnit(1 + Rnd()*4, TerranMarine, "TestLocation2")
    Player2.Unit(TerranMarine, "TestLocation2").AttackOrder("TestLocation")
  EndIf
End When

