Option Hyper Off

Const Ints(4) As Byte = {1,2,3,4}
Const Bools(4) As Boolean = {False,True,False,True}

Class Player8

    Sub Main()
        For Each i In Ints
            Print(i)
        Next
        For Each b As Boolean In Bools
            Print(b)
        Next
    End Sub

End Class
