Function SpawnUnits() As Byte
    Print("hello")
    Return 5
End Function

As Player1
Sub Main()
    Dim foo as Byte, bar as Byte
    foo = SpawnUnits()
    bar = 5 + foo
    CreateUnit(bar, Terran.Marine)
End Sub



