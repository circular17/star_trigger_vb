Option Hyper Off

Function GiveMeTrue() As Boolean
    Print("GiveMeTrue")
    Return True
End Function

Class Player8

  Sub Main()
    Print("Main")
    dim b as Boolean
    b = GiveMeTrue()
  End Sub

End Class
