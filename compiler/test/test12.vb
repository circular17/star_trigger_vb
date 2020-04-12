Dim TestWav As New Sound With {.Filename = "staredit\wav\test.wav",
    .Duration = 2719}

As Player1
Sub Main()
    Print("Playing test.wav")
    TestWav.Play()
End Sub
