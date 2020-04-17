Option Hyper Off

Class Player1
    Enum State
        None = 0
        Other = 2
        One
    End Enum
    Dim MyState As Player1.State = State.Other
End Class

Class Player8

    Sub Main()
        If Player1.MyState = Player1.State.One Then
            Print("this is one")
        End If
    End Sub

End Class
