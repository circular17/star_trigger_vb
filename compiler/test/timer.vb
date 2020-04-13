Option Hyper Off

Dim t As Timer

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
