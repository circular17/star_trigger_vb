Option Hyper Off

Dim t As Timer = 1000

Class Player8

    Sub Main()
        t = 5000
        If t > 2000 Then
           Print("Big time")
        End if
    End Sub

    On t.Elapsed
        Print("Timer has elapsed")
    End On

End Class

Class Two = {Player1, Player2}

    Dim u As Timer

    On u.Elapsed
        u = 1000
    End On

End Class
