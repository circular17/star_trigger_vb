Const TestWav As Sound = {.Filename = "staredit\wav\test.wav", .Duration = 2719}

As Player1
Sub New()
    Print("Playing test.wav")
    TestWav.Play()
End Sub
