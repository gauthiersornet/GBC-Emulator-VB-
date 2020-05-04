Option Explicit On
Imports SharpDX.DirectSound
Imports SharpDX.Multimedia

Module Sound

    Public Tes As Boolean = True 'Test emule Sound -> ça permet de savoir si on émule le son
    Public VSound As Integer = -3000 'Volume Du Son

    'Private SE As Byte ' sound emulation ? Tes suffis :)
    Private Cl1(3) As Long 'il s'agit des channels c'est à dire
    Private En1(3) As Byte 'des cannaux sonor de la gameboy
    Private Cl2(3) As Long
    Private En2(3) As Byte
    Private Cl3(3) As Long
    Private En3(3) As Byte
    Private Cl4(3) As Long
    Private En4(3) As Byte

    Private tlol As Byte 'temp lol :)
    Private sqrW(7, 3) As Byte
    Private tmp2(7) As Byte ' un tableau temporaire qui sert aux données des divers cannaux
    Private tmp3(31) As Byte
    Private Tsnd As Long 'Temp Sound

    REM==========================================================DxEngine===============
    'Private ObjDirectX8 As DirectX8
    Private ObjDirectSound As DirectSound
    Private Chanel1 As SecondarySoundBuffer
    Private Chanel2 As SecondarySoundBuffer
    Private Chanel3 As SecondarySoundBuffer
    Private Chanel4 As SecondarySoundBuffer

    Private Null1_2_3(31) As Byte 'C'est un signale sonor null et donc Channel1Set Null1_2_3, 200 coupe le cannaux 1
#Const hi_q = 0 ' en cas de débogage je crois

    Public Sub InitDxSound(hwnd As Long)
        Dim DirectSsoundBufferDescription As SoundBufferDescription
        DirectSsoundBufferDescription = New SoundBufferDescription()
        ObjDirectSound = New DirectSound()

        '
        'ObjDirectX8 = New DirectX8
        'ObjDirectSound = ObjDirectX8.DirectSoundCreate(vbNullString)
        ObjDirectSound.SetCooperativeLevel(hwnd, CooperativeLevel.Normal)

        Dim wf As WaveFormat
        wf = New WaveFormat(44100, 8, 1)
        DirectSsoundBufferDescription.Format = wf
        'DirectSsoundBufferDescription.fxFormat.nFormatTag = WAVE_FORMAT_PCM
        'DirectSsoundBufferDescription.fxFormat.nChannels = 1
        'DirectSsoundBufferDescription.fxFormat.lSamplesPerSec = 44100
        'DirectSsoundBufferDescription.fxFormat.nBitsPerSample = 8
        'DirectSsoundBufferDescription.fxFormat.nBlockAlign = 1
        'DirectSsoundBufferDescription.fxFormat.lAvgBytesPerSec = DirectSsoundBufferDescription.fxFormat.lSamplesPerSec * DirectSsoundBufferDescription.fxFormat.nBlockAlign
        DirectSsoundBufferDescription.Flags = BufferFlags.ControlFrequency Or BufferFlags.ControlVolume
        DirectSsoundBufferDescription.BufferBytes = 8

        Chanel1 = New SecondarySoundBuffer(ObjDirectSound, DirectSsoundBufferDescription)
        Chanel2 = New SecondarySoundBuffer(ObjDirectSound, DirectSsoundBufferDescription)
        DirectSsoundBufferDescription.BufferBytes = 32
        Chanel3 = New SecondarySoundBuffer(ObjDirectSound, DirectSsoundBufferDescription)
        Chanel4 = New SecondarySoundBuffer(ObjDirectSound, DirectSsoundBufferDescription)

        Chanel1.Volume = VSound
        Chanel2.Volume = VSound
        Chanel3.Volume = VSound
        Chanel4.Volume = VSound
    End Sub

    Public Sub RemoveDxSound()
        '    Set Chanel4 = Nothing
        'Chanel3 = Nothing
        'Chanel2 = Nothing
        'Chanel1 = Nothing
        '
        'ObjDirectSound = Nothing
        'ObjDirectX8 = Nothing
    End Sub

    Private Sub Channel(ch As Long, ByVal Stat As Integer)
        If Stat = 0 Then
            If ch = 1 Then Chanel1.Volume = -10000
            If ch = 2 Then Chanel2.Volume = -10000
            If ch = 3 Then Chanel3.Volume = -10000
            If ch = 4 Then Chanel4.Volume = -10000
        Else
            If ch = 1 Then Chanel1.Volume = VSound
            If ch = 2 Then Chanel2.Volume = VSound
            If ch = 3 Then Chanel3.Volume = VSound
            If ch = 4 Then Chanel4.Volume = VSound
        End If
    End Sub

    Private Sub Channel1Stop()
        Channel1Set(Null1_2_3, 200)
    End Sub

    Private Sub Channel2Stop()
        Channel2Set(Null1_2_3, 200)
    End Sub
    Private Sub Channel3Stop()
        Channel3Set(Null1_2_3, 200)
    End Sub
    Private Sub Channel4Stop()
        Channel4Set(Null1_2_3, 200)
    End Sub

    Private Sub Channel1Set(tmp() As Byte, freq As Double)
        If freq < 200 Then freq = 200
        If freq > 88200 Then freq = 88200
#If hi_q Then
        For hqi = 0 To 7
            If tmp(hqi) <> ch1(hqi) Then GoTo ch:
        Next
        GoTo of:
ch:
        CopyMemory ch1(0), tmp(0), 8
#End If
        'Chanel1.WriteBuffer(0, 8, tmp(0), DSBLOCK_DEFAULT)
        'Chanel1.SetFrequency(freq)
        'If (Chanel1.GetStatus = 0) Then Chanel1.Play(DSBPLAY_LOOPING)
        Chanel1.Write(Of Byte)(tmp, 0, LockFlags.EntireBuffer)
        Chanel1.Frequency = freq
        If (Chanel1.Status = 0) Then Chanel1.Play(0, PlayFlags.Looping)
    End Sub

    Private Sub Channel2Set(tmp() As Byte, freq As Double)
        If freq < 200 Then freq = 200
        If freq > 88200 Then freq = 88200
#If hi_q Then
        For hqi = 0 To 7
            If tmp(hqi) <> ch2(hqi) Then GoTo ch:
        Next
        GoTo of:
ch:
        CopyMemory ch2(0), tmp(0), 8
#End If

        'Chanel2.WriteBuffer(0, 8, tmp(0), DSBLOCK_DEFAULT)
        'Chanel2.SetFrequency(freq)
        'If (Chanel2.GetStatus = 0) Then Chanel2.Play(DSBPLAY_LOOPING)
        Chanel2.Write(Of Byte)(tmp, 0, LockFlags.EntireBuffer)
        Chanel2.Frequency = freq
        If (Chanel2.Status = 0) Then Chanel2.Play(0, PlayFlags.Looping)
    End Sub

    Private Sub Channel3Set(tmp() As Byte, ByRef freq As Double)
        If freq < 200 Then freq = 200
        If freq > 88200 Then freq = 88200
#If hi_q Then
        For hqi = 0 To 31
            If tmp(hqi) <> ch3(hqi) Then GoTo ch:
        Next
        GoTo of:
ch:
        CopyMemory ch3(0), tmp(0), 32
#End If
        'Chanel3.WriteBuffer(0, 32, tmp(0), DSBLOCK_DEFAULT)
        'Chanel3.SetFrequency(freq)
        'If (Chanel3.GetStatus = 0) Then Chanel3.Play DSBPLAY_LOOPING
        Chanel3.Write(Of Byte)(tmp, 0, LockFlags.EntireBuffer)
        Chanel3.Frequency = freq
        If (Chanel3.Status = 0) Then Chanel3.Play(0, PlayFlags.Looping)
    End Sub

    Private Sub Channel4Set(tmp() As Byte, ByRef freq As Double)
        If freq < 200 Then freq = 200
        If freq > 88200 Then freq = 88200
#If hi_q Then
            For hqi = 0 To 31
                If tmp(hqi) <> ch4(hqi) Then GoTo ch:
            Next
            GoTo of:
    ch:
            CopyMemory ch4(0), tmp(0), 32
#End If
        'Chanel4.WriteBuffer 0, 32, tmp(0), DSBLOCK_DEFAULT
        'Chanel4.SetFrequency freq
        'If Chanel4.GetStatus = 0 Then Chanel4.Play DSBPLAY_LOOPING
        Chanel4.Write(Of Byte)(tmp, 0, LockFlags.EntireBuffer)
        Chanel4.Frequency = freq
        If (Chanel4.Status = 0) Then Chanel4.Play(0, PlayFlags.Looping)
    End Sub
    REM==========================================================DxEngine===============

    Public Sub UpDateSnd(clc As Long) 'Update Sound
        If Tes Then
            If En1(0) Then Cl1(0) = Cl1(0) - clc

            If Cl1(0) < 1 And En1(0) Then 'stop
                Channel1Stop()
                En1(0) = 0
            End If

            If En1(1) Then Cl1(1) = Cl1(1) - clc

            If Cl1(1) < 1 And En1(1) Then 'Volume envelop
                Tsnd = (RAM(65298, 0) And 240) \ 16
                If RAM(65298, 0) And 8 Then
                    Tsnd = Tsnd + 1
                    If Tsnd > 15 Then En1(1) = 0 : Tsnd = 15
                Else
                    Tsnd = Tsnd - 1
                    If Tsnd < 0 Then En1(1) = 0 : Tsnd = 0
                End If
                RAM(65298, 0) = (RAM(65298, 0) And 15) + Tsnd * 16
                Write1()
                Cl1(1) = (En1(1) / 64) * 4194304
            End If

            If En1(2) Then Cl1(2) = Cl1(2) - clc

            If Cl1(2) < 1 And En1(2) Then 'Sweep envelop
                Tsnd = (RAM(65300, 0) And 7) * 256 + RAM(65299, 0)
                If RAM(65296, 0) And 8 Then
                    Tsnd = Tsnd - Tsnd / 2 ^ (RAM(65296, 0) And 7)
                Else
                    Tsnd = Tsnd + Tsnd / 2 ^ (RAM(65296, 0) And 7)
                End If
                RAM(65300, 0) = (RAM(65300, 0) And 248) Or Tsnd \ 256
                RAM(65299, 0) = Tsnd And 255
                Write1()
                Cl1(2) = ((En1(2) \ 16) / 128) * 4194304
            End If

            If En2(0) Then Cl2(0) = Cl2(0) - clc

            If Cl2(0) < 1 And En2(0) Then 'stop
                Channel2Stop()
                En2(0) = 0
            End If

            If En2(1) Then Cl2(1) = Cl2(1) - clc

            If Cl2(1) < 1 And En2(1) Then 'Volume envelop
                Tsnd = (RAM(65303, 0) And 240) \ 16
                If RAM(65303, 0) And 8 Then
                    Tsnd = Tsnd + 1
                    If Tsnd > 15 Then Tsnd = 15
                Else
                    Tsnd = Tsnd - 1
                    If Tsnd < 0 Then Tsnd = 0
                End If
                RAM(65303, 0) = (RAM(65303, 0) And 15) + Tsnd * 16
                Write2()
                Cl2(1) = (En2(1) / 64) * 4194304
            End If

            If En3(0) Then Cl3(0) = Cl3(0) - clc

            If Cl3(0) < 1 And En3(0) Then 'stop
                Channel3Stop()
                En3(0) = 0
            End If
        End If
    End Sub

    Public Sub InitWave()
        sqrW(0, 0) = 0 : sqrW(1, 0) = 0 : sqrW(2, 0) = 255 : sqrW(3, 0) = 255 : sqrW(4, 0) = 255 : sqrW(5, 0) = 255 : sqrW(6, 0) = 255 : sqrW(7, 0) = 255
        sqrW(0, 1) = 0 : sqrW(1, 1) = 0 : sqrW(2, 1) = 0 : sqrW(3, 1) = 255 : sqrW(4, 1) = 255 : sqrW(5, 1) = 255 : sqrW(6, 1) = 255 : sqrW(7, 1) = 255
        sqrW(0, 2) = 0 : sqrW(1, 2) = 0 : sqrW(2, 2) = 0 : sqrW(3, 2) = 0 : sqrW(4, 2) = 255 : sqrW(5, 2) = 255 : sqrW(6, 2) = 255 : sqrW(7, 2) = 255
        sqrW(0, 3) = 0 : sqrW(1, 3) = 0 : sqrW(2, 3) = 0 : sqrW(3, 3) = 0 : sqrW(4, 3) = 0 : sqrW(5, 3) = 0 : sqrW(6, 3) = 255 : sqrW(7, 3) = 255
        Channel(1, 0)
        Channel(2, 0)
        Channel(3, 0)
        Channel(4, 0)

        Call UpDateVolumSound()
    End Sub

    Public Sub UpDateVolumSound()
        Channel(1, Tes)
        Channel(2, Tes)
        Channel(3, Tes)
        Channel(4, Tes)
    End Sub

    'Register Writes
    Public Sub setNR10(val As Long)
        En1(2) = val And 112
        If En1(2) Then
            Cl1(2) = ((En1(2) \ 16) / 128) * 4194304
        End If
    End Sub

    Public Sub setNR11(val As Long)
        Cl1(0) = (64 - (val And 63)) / 256
        En1(0) = RAM(65300, 0) And 64
    End Sub

    Public Sub setNR12(val As Long)
        En1(1) = val And 7
        Cl1(1) = En1(1) * (1 / 64) * 4194304
        If Tes Then Write1()
    End Sub

    Public Sub setNR13(val As Long)

    End Sub

    Public Sub setNR14(val As Long)
        If RAM(65300, 0) And 128 Then Write1()
        En1(0) = val And 64
        Cl1(0) = (64 - (RAM(65297, 0) And 63)) / 256 * 4194304
    End Sub

    Public Sub setNR21(val As Long)
        Cl2(0) = (64 - (val And 63)) / 256
        En2(0) = RAM(65305, 0) And 64
    End Sub

    Public Sub setNR22(val As Long)
        En2(1) = val And 7
        Cl2(1) = En2(1) * (1 / 64) * 4194304
        If Tes Then Write2()
    End Sub

    Public Sub setNR23(val As Long)

    End Sub

    Public Sub setNR24(val As Long)
        If RAM(65305, 0) And 128 Then Write2()
        En2(0) = val And 64
        Cl2(0) = (64 - (RAM(65302, 0) And 63)) / 256 * 4194304
    End Sub

    Public Sub setNR30(val As Long)

    End Sub

    Sub setNR31(val As Long)

    End Sub

    Sub setNR32(val As Long)
        'En3(1) = val And 7
        'Cl3(1) = En3(1) * (1 / 64) * 4194304
        'If Tes Then Write3()
    End Sub

    Sub setNR33(val As Long)

    End Sub

    Public Sub setNR34(val As Long)
        If Tes Then If val And 128 Then Write3()
        En3(0) = val And 64
        Cl3(0) = (256 - (RAM(65307, 0))) / 256 * 4194304
    End Sub

    Public Sub setNR41(val As Long)

    End Sub

    Public Sub setNR42(val As Long)

    End Sub

    Public Sub setNR43(val As Long)

    End Sub

    Public Sub setNR44(val As Long)

    End Sub

    Public Sub setNR50(val As Long)

    End Sub

    Public Sub setNR51(val As Long)

    End Sub

    Public Sub setNR52(val As Long)

    End Sub

    Sub Write1() 'Write Ch1 data to dsound buffer
        Dim tv As Double
        tlol = (RAM(65297, 0) And 192) \ 63
        tv = ((RAM(65298, 0) And 224) \ 16) / 15
        tmp2(0) = sqrW(0, tlol) * tv
        tmp2(1) = sqrW(1, tlol) * tv
        tmp2(2) = sqrW(2, tlol) * tv
        tmp2(3) = sqrW(3, tlol) * tv
        tmp2(4) = sqrW(4, tlol) * tv
        tmp2(5) = sqrW(5, tlol) * tv
        tmp2(6) = sqrW(6, tlol) * tv
        tmp2(7) = sqrW(7, tlol) * tv
        Channel1Set(tmp2, GetTone((RAM(65300, 0) And 7) * 256 + RAM(65299, 0)))
    End Sub

    Sub Write2() 'Write Ch2 data to dsound buffer
        Dim tv As Double
        tlol = (RAM(65302, 0) And 192) \ 63
        tv = ((RAM(65303, 0) And 224) \ 16) / 15
        tmp2(0) = sqrW(0, tlol) * tv
        tmp2(1) = sqrW(1, tlol) * tv
        tmp2(2) = sqrW(2, tlol) * tv
        tmp2(3) = sqrW(3, tlol) * tv
        tmp2(4) = sqrW(4, tlol) * tv
        tmp2(5) = sqrW(5, tlol) * tv
        tmp2(6) = sqrW(6, tlol) * tv
        tmp2(7) = sqrW(7, tlol) * tv
        Channel2Set(tmp2, GetTone((RAM(65305, 0) And 7) * 256 + RAM(65304, 0)))
    End Sub

    Sub Write3() 'Write Ch3 data to dsound buffer
        Dim tv As Double
        If (RAM(65306, 0) And 128) = 0 Then Exit Sub 'Channel3Stop:
        tv = (RAM(65308, 0) And 96) \ 32
        If tv = 2 Then
            tv = 0.5
        ElseIf tv = 3 Then
            tv = 0.25
        End If
        tv = tv * 16
        Dim I As Byte
        For I = 0 To 15
            tmp3(I * 2) = (RAM(65328 + I, 0) \ 16) * tv
            tmp3(I * 2 + 1) = (RAM(65328 + I, 0) And 15) * tv
        Next I
        Channel3Set(tmp3, GetTone3((RAM(65310, 0) And 7) * 256 + RAM(65309, 0)))
    End Sub

    'Sub Write4() 'Apparament y a pas de cannau 4 o_O
    '
    'End Sub

    Public Function GetTone(ByVal freq As Double) As Double 'addaptation du son GBC à celui du PC
        GetTone = 131072 / (2048 - freq) * 8
    End Function

    Function GetTone3(tone As Long) 'addaptation du son GBC à celui du PC
        GetTone3 = (65536 / (2048 - tone)) * 32
    End Function

End Module
