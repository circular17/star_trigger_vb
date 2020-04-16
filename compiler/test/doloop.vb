Option Hyper Off

Class Player8

    Sub Main()
        Dim b As Byte = 5
        Do
            b -= 1
            If b = 2 Then
                Exit Do
                Print("Never do")
            End if
            Print("More")
        Loop Until b = 0
        Print("End sub")
    End Sub

End Class
