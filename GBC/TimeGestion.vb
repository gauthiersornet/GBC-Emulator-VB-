Option Explicit On

Public Class TimeGestion

    'Well yes this is bad writen
    'But i'm getting bored with Gameboy..
    'It's a 5 minutes work :)

    Public Act As Byte

    Dim Secs As Long, Halt As Long, Dccb As Long
    Dim Dat As Date ', Tim As Date

    Public Property S() As Integer
        Get
            Call Update()
            S = Secs Mod 60
        End Get
        Set(ByVal value As Integer)
            Secs = Secs - S + value
        End Set
    End Property


    Public Property M() As Integer
        Get
            Call Update()
            M = (Secs \ 60) Mod 60
        End Get
        Set(ByVal value As Integer)
            Secs = Secs - M * 60 + value * 60
        End Set
    End Property



    Public Property H() As Integer
        Get
            Call Update()
            H = (Secs \ 3600) Mod 24
        End Get
        Set(ByVal value As Integer)
            Secs = Secs - H * 3600 + value * 3600
        End Set
    End Property

    Public Property DL() As Integer
        Get
            Call Update()
            DL = (Secs / 86400) And 255
        End Get
        Set(ByVal value As Integer)
            Secs = Secs - DL * 86400 + value * 86400
        End Set
    End Property

    Public Property DH() As Integer
        Get
            Call Update()
            DH = (Secs / 22118400) And 1 + ((Secs / 22118400) And 2) * 32 + Halt * 32
        End Get
        Set(ByVal value As Integer)
            Secs = Secs - ((S / 22118400) And 1) * 22118400 + (value And 1) * 22118400
            If (value And 64) = 0 Then Secs = Secs - ((S / 22118400) And 2) * 22118400
        End Set
    End Property


    Public Sub Save(FileName As String)
        'Dim FreeF As Long
        'FreeF = FreeFile()
        'Open FileName & ".rtc" For Binary As #FreeF
        '    Put #FreeF, , Secs
        '    Put #FreeF, , Halt
        '    Put #FreeF, , Dat
        '    Put #FreeF, , Tim
        'Close #FreeF
    End Sub

    Public Sub Load(FileName As String)
        'Dim FreeF As Long
        'FreeF = FreeFile()
        'Open FileName & ".rtc" For Binary As #FreeF
        '    If LOF(FreeF) = 0 Then Exit Sub
        '    Get #FreeF, , Secs
        '    Get #FreeF, , Halt
        '    Get #FreeF, , Dat
        '    Get #FreeF, , Tim
        'Close #FreeF
        'Call Update()
    End Sub

    Private Sub Class_Initialize()
        Dat = Date.Now
        'Tim = Date.Now
    End Sub

    Private Sub Update()
        Dim ssc As Long
        Dim dnw As Date = Date.Now
        ssc = (dnw - Dat).TotalSeconds
        Secs = Secs + ssc
        Dat = Date.Now
        'Tim = Time
    End Sub

    Public Function ReadReg() As Long
        Select Case Act
            Case &H8
                ReadReg = S
            Case &H9
                ReadReg = M
            Case &HA
                ReadReg = H
            Case &HB
                ReadReg = DL
            Case &HC
                ReadReg = DH
            Case Else
                ReadReg = 0
        End Select
    End Function

    Public Sub WriteReg(val As Long)
        Select Case Act
            Case &H8
                S = val
            Case &H9
                M = val
            Case &HA
                H = val
            Case &HB
                DL = val
            Case &HC
                DH = val
        End Select
    End Sub




End Class
