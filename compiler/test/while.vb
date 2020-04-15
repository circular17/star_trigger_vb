Option Hyper Off

Class Player8

    Sub Main()
        Dim b As Byte = 5
        While b > 0
            b -= 1
            If b = 2 Then
                Continue While
            End If
            Print("More")
        End While
    End Sub

End Class
