Option Explicit On

Module z80

    'Global variable
    Public GameBoyMode As Byte
    Public Clm0 As Long, Clm3 As Long, Cllc As Long, Cldr As Long, Drw As Byte, CpuS As Byte 'permet le contrôle des fréquences du CPU
    Public Cpc(&HFF) As Long 'Vitesse du CPU
    Public Tvm As Long ' ça veux pt dire Time Virtual Mem en tout cas c'est un indicateur rien de plus
    Public Smp As Byte
    Public SETT(0 To 56) As Byte ' équivaut à SETT(x*8)=2^x
    Public BITT(0 To 56) As Byte ' équivaut à BITT(x*8)=255-2^x
    Public cm As Byte = 1 'Core Mode c'est cette variable qui permet de savoir quel cycle proc on va utiliser
    Public Mips As Long 'Nombre d'instruction par seconde
    Public Mhz As Long 'Fréquence


    'Public variable
    Public A As Long, F As Long 'Registre AF flag -> poid fort la 1er lettre
    Public B As Long, C As Long 'Registre BC
    Public D As Long, E As Long ' Registre DE
    Public H As Long, L As Long 'Registre HL
    Public PC As Long 'Registre PC
    Public SP As Long 'Registre SP pile de donnée
    Public IME As Boolean
    Public LimitFps As Boolean = True
    Public Clcount As Long 'Nombre de de cycle ou qqchose en lien car Clcount=Clcount+Cpc(Opcode)

    'Private variable
    Private brkAddr As Long ' Break Adress
    Private Hline As Integer
    Private Temp As Byte
    Private I As Long
    Private PbMemVal As Long 'Variable qui prend les instructions :)
    Private MemPtr As Long 'Pointeur qui aide à l'execution d'une instruction
    Private ime_stat As Byte

    'FF07 - TAC - Timer Control (R/W)
    '  Bit 2    - Timer Stop  (0=Stop, 1=Start)
    '  Bits 1-0 - Input Clock Select
    '             00:   4096 Hz    (~4194 Hz SGB)
    '             01: 262144 Hz  (~268400 Hz SGB)
    '             10:  65536 Hz   (~67110 Hz SGB)
    '             11:  16384 Hz   (~16780 Hz SGB)
    Private TimerC As Integer


    REM============Drapeaux de AF===========
    'Set Flags

    'Bit  Name  Set Clr  Expl.
    Public Sub SetZ(ByVal Act As Boolean)
        ' 7   zf    Z   NZ   Zero Flag
        'The Zero Flag (Z)
        'This bit becomes set (1) if the result of an
        'operation has been zero (0). Used for conditional jumps.
        If Act Then F = F Or 128 Else F = F And 127
    End Sub

    'The BCD Flags (N, H)
    'These flags are (rarely) used for the DAA instruction only, N Indicates whether the previous instruction has been an addition or subtraction,
    'and H indicates carry for lower 4bits of the result, also for DAA, the C flag must indicate carry for upper 8bits.
    'After adding/subtracting two BCD numbers, DAA is intended to convert the result into BCD format; BCD numbers are ranged from 00h to 99h rather than 00h to FFh.
    'Because C and H flags must contain carry-outs for each digit, DAA cannot be used for 16bit operations (which have 4 digits), or for INC/DEC(operations )(which do not affect C-flag).
    Public Sub SetN(ByVal Act As Boolean)
        ' 6   n     -   -    Add/Sub-Flag (BCD)
        If Act Then F = F Or 64 Else F = F And 191
    End Sub
    Public Sub SetH(ByVal Act As Boolean)
        ' 5   h     -   -    Half Carry Flag (BCD)
        If Act Then F = F Or 32 Else F = F And 223
    End Sub

    Sub SetC(ByVal Act As Boolean)
        ' 4    cy    C   NC   Carry Flag
        'The Carry Flag (C, or Cy)
        'Becomes set when the result of an addition became bigger than FFh (8bit) or FFFFh (16bit). Or when the result of a subtraction or comparision became less than zero (much as for Z80 and 80x86 CPUs, but unlike as for 65XX and ARM CPUs). Also the flag becomes set when a rotate/shift operation has shifted-out a "1"-bit.
        'Used for conditional jumps, and for instructions such like ADC, SBC, RL, RLA, etc.
        If Act Then F = F Or 16 Else F = F And 239
    End Sub

    'Get Flags
    Public Function GetZ() As Byte
        GetZ = F \ 128 ' And 1 ' not needed cos it's the last bit
    End Function
    Public Function GetN() As Byte
        GetN = F \ 64 And 1
    End Function
    Public Function GetH() As Byte
        GetH = F \ 32 And 1
    End Function
    Public Function GetC() As Byte
        GetC = F \ 16 And 1
    End Function
    REM============FinDrapeaux==============

    Public Sub Reset() 'Reset the z80
        Call QueryPerformanceFrequency(CurFreq)  'Get the timer frequency
        CurFreq = CurFreq / 1000 ' in ms
        Hline = -1

        Smp = 0
        IME = True
        If GameBoyMode = 0 Then A = &H1 Else A = &H11
        '- GB \ SGB , &hFF-GBP, &h11-GBC
        F = &HB0
        B = 0
        C = &H13
        D = 0
        E = &HD8
        H = 1
        L = &H4D
        PC = &H100
        SP = 65534

        Cldr = 255
        Clm0 = 251
        Clm3 = 79
        Cllc = 455
        CpuS = 0

        WriteM(65285, &H0)   ' TIMA
        WriteM(65286, &H0)   ' TMA
        WriteM(65287, &H0)   ' TAC
        WriteM(65296, &H80)  ' NR10
        WriteM(65297, &HBF)  ' NR11
        WriteM(65298, &HF3)  ' NR12
        WriteM(65300, &HBF)  ' NR14
        WriteM(65302, &H3F)   ' NR21
        WriteM(65303, &H0)   ' NR22
        WriteM(65305, &HBF)  ' NR24
        WriteM(65306, &H7F)  ' NR30
        WriteM(65307, &HFF)  ' NR31
        WriteM(65308, &H9F)  ' NR32
        WriteM(65310, &HBF)  ' NR33
        WriteM(65312, &HFF)  ' NR41
        WriteM(65313, &H0)   ' NR42
        WriteM(65314, &H0)   ' NR43
        WriteM(65315, &HBF)  ' NR30
        WriteM(65316, &H77)  ' NR50
        WriteM(65317, &HF3)  ' NR51
        WriteM(65318, &HF1)  '- GB, &HF0 - SGB ' NR52
        WriteM(65344, &H91)  ' LCDC
        WriteM(65346, &H0)   ' SCY
        WriteM(65347, &H0)   ' SCX
        WriteM(65349, &H0)   ' LYC
        WriteM(65351, &HE4)  ' BGP
        WriteM(65352, &HE4)  ' OBP0
        WriteM(65353, &HE4)  ' OBP1
        WriteM(65354, &H0)   ' WY
        WriteM(65355, &H0)   ' WX
        WriteM(65535, &H0)   ' IE

        If LimitFps Then
            Call QueryPerformanceCounter(Graph.CurStart)  'Get the start time
        End If
        brkAddr = -1
        'run @ 4mhz(4194304 hrz)
        '~60Fps(59.7)(~70224 hrz per frame(~4194304 hrz))
        Cldr = 255
        Cllc = 455
        Clm0 = Cllc - 204
        Clm3 = Cllc - 376
        'GBCEmul.BNotPause = True
    End Sub

    Public Sub InitCPU()

        'Cpc gère la vitesse en tout point du CPU
        'Il cela à un rapport avec le temps horloge que mes chaques instructions :)
        Cpc(&H0) = 1 : Cpc(&H1) = 3 : Cpc(&H2) = 2 : Cpc(&H3) = 2
        Cpc(&H4) = 1 : Cpc(&H5) = 1 : Cpc(&H6) = 2 : Cpc(&H7) = 1
        Cpc(&H8) = 5 : Cpc(&H9) = 2 : Cpc(&HA) = 2 : Cpc(&HB) = 2
        Cpc(&HC) = 1 : Cpc(&HD) = 1 : Cpc(&HE) = 2 : Cpc(&HF) = 1

        Cpc(&H10) = 1 : Cpc(&H11) = 3 : Cpc(&H12) = 2 : Cpc(&H13) = 2
        Cpc(&H14) = 1 : Cpc(&H15) = 1 : Cpc(&H16) = 2 : Cpc(&H17) = 1
        Cpc(&H18) = 2 : Cpc(&H19) = 2 : Cpc(&H1A) = 2 : Cpc(&H1B) = 2
        Cpc(&H1C) = 1 : Cpc(&H1D) = 1 : Cpc(&H1E) = 2 : Cpc(&H1F) = 1

        Cpc(&H20) = 2 : Cpc(&H21) = 3 : Cpc(&H22) = 2 : Cpc(&H23) = 2
        Cpc(&H24) = 1 : Cpc(&H25) = 1 : Cpc(&H26) = 2 : Cpc(&H27) = 1
        Cpc(&H28) = 2 : Cpc(&H29) = 2 : Cpc(&H2A) = 2 : Cpc(&H2B) = 2
        Cpc(&H2C) = 1 : Cpc(&H2D) = 1 : Cpc(&H2E) = 2 : Cpc(&H2F) = 1

        Cpc(&H30) = 2 : Cpc(&H31) = 3 : Cpc(&H32) = 2 : Cpc(&H33) = 2
        Cpc(&H34) = 3 : Cpc(&H35) = 3 : Cpc(&H36) = 3 : Cpc(&H37) = 1
        Cpc(&H38) = 2 : Cpc(&H39) = 2 : Cpc(&H3A) = 2 : Cpc(&H3B) = 2
        Cpc(&H3C) = 1 : Cpc(&H3D) = 1 : Cpc(&H3E) = 2 : Cpc(&H3F) = 1

        Cpc(&H40) = 1 : Cpc(&H41) = 1 : Cpc(&H42) = 1 : Cpc(&H43) = 1
        Cpc(&H44) = 1 : Cpc(&H45) = 1 : Cpc(&H46) = 2 : Cpc(&H47) = 1
        Cpc(&H48) = 1 : Cpc(&H49) = 1 : Cpc(&H4A) = 1 : Cpc(&H4B) = 1
        Cpc(&H4C) = 1 : Cpc(&H4D) = 1 : Cpc(&H4E) = 2 : Cpc(&H4F) = 1

        Cpc(&H50) = 1 : Cpc(&H51) = 1 : Cpc(&H52) = 1 : Cpc(&H53) = 1
        Cpc(&H54) = 1 : Cpc(&H55) = 1 : Cpc(&H56) = 2 : Cpc(&H57) = 1
        Cpc(&H58) = 1 : Cpc(&H59) = 1 : Cpc(&H5A) = 1 : Cpc(&H5B) = 1
        Cpc(&H5C) = 1 : Cpc(&H5D) = 1 : Cpc(&H5E) = 2 : Cpc(&H5F) = 1

        Cpc(&H60) = 1 : Cpc(&H61) = 1 : Cpc(&H62) = 1 : Cpc(&H63) = 1
        Cpc(&H64) = 1 : Cpc(&H65) = 1 : Cpc(&H66) = 2 : Cpc(&H67) = 1
        Cpc(&H68) = 1 : Cpc(&H69) = 1 : Cpc(&H6A) = 1 : Cpc(&H6B) = 1
        Cpc(&H6C) = 1 : Cpc(&H6D) = 1 : Cpc(&H6E) = 2 : Cpc(&H6F) = 1

        Cpc(&H70) = 2 : Cpc(&H71) = 2 : Cpc(&H72) = 2 : Cpc(&H73) = 2
        Cpc(&H74) = 2 : Cpc(&H75) = 2 : Cpc(&H76) = 1 : Cpc(&H77) = 2
        Cpc(&H78) = 1 : Cpc(&H79) = 1 : Cpc(&H7A) = 1 : Cpc(&H7B) = 1
        Cpc(&H7C) = 1 : Cpc(&H7D) = 1 : Cpc(&H7E) = 2 : Cpc(&H7F) = 1

        Cpc(&H80) = 1 : Cpc(&H81) = 1 : Cpc(&H82) = 1 : Cpc(&H83) = 1
        Cpc(&H84) = 1 : Cpc(&H85) = 1 : Cpc(&H86) = 2 : Cpc(&H87) = 1
        Cpc(&H88) = 1 : Cpc(&H89) = 1 : Cpc(&H8A) = 1 : Cpc(&H8B) = 1
        Cpc(&H8C) = 1 : Cpc(&H8D) = 1 : Cpc(&H8E) = 2 : Cpc(&H8F) = 1

        Cpc(&H90) = 1 : Cpc(&H91) = 1 : Cpc(&H92) = 1 : Cpc(&H93) = 1
        Cpc(&H94) = 1 : Cpc(&H95) = 1 : Cpc(&H96) = 2 : Cpc(&H97) = 1
        Cpc(&H98) = 1 : Cpc(&H99) = 1 : Cpc(&H9A) = 1 : Cpc(&H9B) = 1
        Cpc(&H9C) = 1 : Cpc(&H9D) = 1 : Cpc(&H9E) = 2 : Cpc(&H9F) = 1

        Cpc(&HA0) = 1 : Cpc(&HA1) = 1 : Cpc(&HA2) = 1 : Cpc(&HA3) = 1
        Cpc(&HA4) = 1 : Cpc(&HA5) = 1 : Cpc(&HA6) = 2 : Cpc(&HA7) = 1
        Cpc(&HA8) = 1 : Cpc(&HA9) = 1 : Cpc(&HAA) = 1 : Cpc(&HAB) = 1
        Cpc(&HAC) = 1 : Cpc(&HAD) = 1 : Cpc(&HAE) = 2 : Cpc(&HAF) = 1

        Cpc(&HB0) = 1 : Cpc(&HB1) = 1 : Cpc(&HB2) = 1 : Cpc(&HB3) = 1
        Cpc(&HB4) = 1 : Cpc(&HB5) = 1 : Cpc(&HB6) = 2 : Cpc(&HB7) = 1
        Cpc(&HB8) = 1 : Cpc(&HB9) = 1 : Cpc(&HBA) = 1 : Cpc(&HBB) = 1
        Cpc(&HBC) = 1 : Cpc(&HBD) = 1 : Cpc(&HBE) = 2 : Cpc(&HBF) = 1

        Cpc(&HC0) = 2 : Cpc(&HC1) = 3 : Cpc(&HC2) = 3 : Cpc(&HC3) = 4
        Cpc(&HC4) = 3 : Cpc(&HC5) = 4 : Cpc(&HC6) = 2 : Cpc(&HC7) = 4
        Cpc(&HC8) = 2 : Cpc(&HC9) = 4 : Cpc(&HCA) = 3 : Cpc(&HCB) = 2 'plus MRW(+8)
        Cpc(&HCC) = 3 : Cpc(&HCD) = 6 : Cpc(&HCE) = 2 : Cpc(&HCF) = 4

        Cpc(&HD0) = 2 : Cpc(&HD1) = 3 : Cpc(&HD2) = 3 : Cpc(&HD3) = 3
        Cpc(&HD4) = 3 : Cpc(&HD5) = 4 : Cpc(&HD6) = 2 : Cpc(&HD7) = 4
        Cpc(&HD8) = 2 : Cpc(&HD9) = 4 : Cpc(&HDA) = 3 : Cpc(&HDB) = 0
        Cpc(&HDC) = 3 : Cpc(&HDD) = 0 : Cpc(&HDE) = 2 : Cpc(&HDF) = 4

        Cpc(&HE0) = 3 : Cpc(&HE1) = 3 : Cpc(&HE2) = 2 : Cpc(&HE3) = 0
        Cpc(&HE4) = 0 : Cpc(&HE5) = 4 : Cpc(&HE6) = 2 : Cpc(&HE7) = 4
        Cpc(&HE8) = 4 : Cpc(&HE9) = 1 : Cpc(&HEA) = 4 : Cpc(&HEB) = 0
        Cpc(&HEC) = 0 : Cpc(&HED) = 0 : Cpc(&HEE) = 2 : Cpc(&HEF) = 4

        Cpc(&HF0) = 2 : Cpc(&HF1) = 3 : Cpc(&HF2) = 2 : Cpc(&HF3) = 1
        Cpc(&HF4) = 0 : Cpc(&HF5) = 4 : Cpc(&HF6) = 2 : Cpc(&HF7) = 4
        Cpc(&HF8) = 2 : Cpc(&HF9) = 2 : Cpc(&HFA) = 4 : Cpc(&HFB) = 1
        Cpc(&HFC) = 0 : Cpc(&HFD) = 0 : Cpc(&HFE) = 2 : Cpc(&HFF) = 4
        Dim I As Integer
        For I = 0 To 255
            Cpc(I) = Cpc(I) * 4 'Si vous changez le 4 ici ça va changer la vitesse du jeux émulé
        Next I
    End Sub

    Public Sub RunCpu()
        'While (frmPrincipale.BNotPause) ' keep cpu running
        'If LinkState Then Check
        '#If Debuger Then
        'DebuggerDiss PC
        'frmDebugger.execcommand
        '#End If
        If (cm = 1) Then
            Call RunCycles1()
        Else 'If (cm = 2) Then
            Call RunCycles2()
            'Else
            'Call RunCyclesGoSub()
        End If
        Mips = Mips + 1

        'Do interups
        Temp = RAM(65535, 0) And RAM(65295, 0) ' AND IE, IF
        If ((IME = True) And (Temp > 0)) Then  'If no Interrupt occured exit
            'Process Interrput
            'Push(pc)
            SP = SP - 1
            WriteM(SP, PC \ 256)
            SP = SP - 1
            WriteM(SP, PC And 255)
            IME = False
            If (Temp And 1) = 1 Then        'V-Blank ?
                PC = 64
                RAM(65295, 0) = RAM(65295, 0) And 254
            ElseIf (Temp And 2) = 2 Then    'LCDC ?
                PC = 72
                RAM(65295, 0) = RAM(65295, 0) And 253
            ElseIf (Temp And 4) = 4 Then    'Timer ?
                PC = 80
                RAM(65295, 0) = RAM(65295, 0) And 251
            ElseIf (Temp And 8) = 8 Then    'Serial ?
                PC = 88
                RAM(65295, 0) = RAM(65295, 0) And 247
            ElseIf (Temp And 16) = 16 Then  'Joypad ?
                PC = 96
                RAM(65295, 0) = RAM(65295, 0) And 239
            End If
        End If

        If (RAM(65344, 0) And 128) Then
            If Clcount > Clm3 And RAM(65348, 0) < 144 Then
                '80
                Clm3 = Clm3 + 456 + 456 * CpuS
                'set stat mode 3
                If (Not SkipFrame) Then If (GameBoyMode = 1) Then Call DrawLine() Else Call DrawLineOldGameBoy()
                RAM(65345, 0) = (RAM(65345, 0) And 252) Or 3
            ElseIf ((Clcount > Clm0) And (RAM(65348, 0) < 144)) Then
                '252
                Clm0 = Clm0 + 456 + 456 * CpuS
                'set h-blank

                Hline = RAM(65348, 0)
                RAM(65345, 0) = RAM(65345, 0) And 252
                If Hdma = True Then
                    For I = HdmaS To HdmaS + 15
                        RAM(HdmaD, vRamB) = ReadM(I)
                        HdmaD = HdmaD + 1
                    Next I
                    HdmaS = HdmaS + 16
                    Hdmal = Hdmal - 1
                    If Hdmal = -1 Then Hdma = False : RAM(65365, 0) = 255 Else RAM(65365, 0) = Hdmal
                End If

                'stat h-blank int
                If (RAM(65345, 0) And 8) Then RAM(65295, 0) = (RAM(65295, 0) Or 2)

            End If

            If Clcount > Cllc Then
                '456
                Cllc = Cllc + 456 + 456 * CpuS
                ' Increment Line Counter
                RAM(65348, 0) = (RAM(65348, 0) + 1) Mod 154

                If RAM(65348, 0) = 145 Then
                    If RAM(65344, 0) And 128 Then
                        Call DrawScreen()
                        If LimitFps Then Call QueryPerformanceCounter(CurStart) 'Get the start time
                    End If
                End If
                'ly=lyc
                If (RAM(65348, 0) = RAM(65349, 0)) Then
                    'ly=lyc int
                    If RAM(65345, 0) And 64 Then RAM(65295, 0) = RAM(65295, 0) Or 2
                    'set ly=lyc
                    RAM(65345, 0) = RAM(65345, 0) Or 4
                Else
                    'reset ly=lyc
                    RAM(65345, 0) = RAM(65345, 0) And 251
                End If

                'check h-blank,v-blank ,ect
                If (RAM(65348, 0) < 144) Then
                    'set mode 2
                    RAM(65345, 0) = (RAM(65345, 0) And 252) Or 2
                    'stat mode 2 int
                    If RAM(65345, 0) And 32 Then RAM(65295, 0) = RAM(65295, 0) Or 2
                ElseIf (RAM(65348, 0) = 144) Then
                    'set v-blank (mode 1)
                    RAM(65345, 0) = (RAM(65345, 0) And 252) Or 1
                    'v-blank int
                    RAM(65295, 0) = RAM(65295, 0) Or 1
                    'stat mode 1 int too
                    If RAM(65345, 0) And 16 Then RAM(65295, 0) = RAM(65295, 0) Or 2
                End If 'hck

            End If 'mod456
        Else
            If Clcount > Cllc Then Cllc = Cllc + 456 + 456 * CpuS
#If 1 Then 'fix
            If (Hdma) Then
                For I = HdmaS To HdmaS + 15
                    RAM(HdmaD, vRamB) = ReadM(I)
                    HdmaD = HdmaD + 1
                Next I
                HdmaS = HdmaS + 16
                Hdmal = Hdmal - 1
                If Hdmal = -1 Then Hdma = False : RAM(65365, 0) = 255 Else RAM(65365, 0) = Hdmal
            End If
#End If
        End If

        If (Clcount > Cldr) Then
            '256
            Cldr = Cldr + 456
            'Inc(divreg)
            RAM(65284, 0) = (RAM(65284, 0) + 1) And 255
        End If

        If (Clcount > 70223) Then
            Mhz = Mhz + 1
            'Call FenetreTriche.UpDate
            Clcount = Clcount - 70224
            'draw screen
            Cldr = Cldr - 70224
            Cllc = Cllc - 70224
            Clm0 = Cllc - 204 - 204 * CpuS
            Clm3 = Cllc - 376 - 376 * CpuS

            Call JoyPad.UpDateJoyPad()
            'DoEvents
        End If

extif:
        'End While
    End Sub

    Sub UTimer(cycles As Long) 'Update the timer
        If RAM(65287, 0) And 4 Then  'tac bit 2
            TimerC = TimerC + cycles
            Select Case RAM(65287, 0) And 3
                Case 0
                    If TimerC > 1023 Then
                        TimerC = TimerC - 1024
                        If RAM(65285, 0) = 255 Then
                            RAM(65285, 0) = RAM(65286, 0)
                            RAM(65295, 0) = RAM(65295, 0) Or 4 'IF
                        Else
                            RAM(65285, 0) = RAM(65285, 0) + 1
                        End If
                    End If
                Case 1
                    If TimerC > 15 Then
                        TimerC = TimerC - 16
                        If RAM(65285, 0) = 255 Then
                            RAM(65285, 0) = RAM(65286, 0)
                            RAM(65295, 0) = RAM(65295, 0) Or 4 'IF
                        Else
                            RAM(65285, 0) = RAM(65285, 0) + 1
                        End If
                    End If
                Case 2
                    If TimerC > 63 Then
                        TimerC = TimerC - 64
                        If RAM(65285, 0) = 255 Then
                            RAM(65285, 0) = RAM(65286, 0)
                            RAM(65295, 0) = RAM(65295, 0) Or 4 'IF
                        Else
                            RAM(65285, 0) = RAM(65285, 0) + 1
                        End If
                    End If
                Case 3
                    If TimerC > 255 Then
                        TimerC = TimerC - 256
                        If RAM(65285, 0) = 255 Then
                            RAM(65285, 0) = RAM(65286, 0)
                            RAM(65295, 0) = RAM(65295, 0) Or 4 'IF
                        Else
                            RAM(65285, 0) = RAM(65285, 0) + 1
                        End If
                    End If
            End Select
        End If
    End Sub

    Sub RunCycles1()
        PbMemVal = Pb()
        Clcount = Clcount + Cpc(PbMemVal)
        UTimer(Cpc(PbMemVal))
        'If utu Then UTimer(Cpc(PbMemVal))
        If Tes Then
            If CpuS = 0 Then
                UpDateSnd(Cpc(PbMemVal))
            Else
                UpDateSnd(Cpc(PbMemVal) / 2)
            End If
        End If
        If ime_stat = 3 Then IME = True : ime_stat = 0
        If ime_stat = 4 Then IME = False : ime_stat = 0
        If ime_stat = 1 Then ime_stat = 3
        If ime_stat = 2 Then ime_stat = 4

        Select Case PbMemVal
        'moved up for speed
            Case 118   ' HALT
                Halt()
            Case 0 '     ' NOP
        'Do nothing
            Case &H28     ' JR(z, disp)
                Jr(Pb, GetZ)
            Case &HFA     ' LD     'A,(nnnn)     '  ---- special (old jp(m),nnnn)
                A = ReadM(Pw)
            Case &HEA     ' LD     '(nnnn),A     '  ---- special (old jp(pe),nnnn)
                WriteM(Pw, A)
            Case &HF0     ' LD     'A,($FF00+nn) ---- special (old ret(p))
                A = ReadM(65280 + Pb())
            Case &HA7     ' AND  A
                Zand(A)
            Case 1     '  'LD BC, nnnn
                C = Pb()
                B = Pb()
            Case 2     '  LD (BC), a
                WriteM(B * 256 + C, A)
            Case 3     '  INC(BC)
                Inc16(B, C)
            Case 4     '  INC(b)
                Inc(B)
            Case 5     '  DEC(b)
                Dec(B)
            Case 6     '  LD b, nn
                B = Pb()
            Case 7     '  RLCA
                Rlca()
            Case 8      ' LD     '(nnnn),SP     ' ---- special (old ex af,af)
                MemPtr = Pw()
                WriteM(MemPtr, SP And 255)
                WriteM(MemPtr + 1, SP \ 256)
            Case 9     '  Add(HL), BC
                AddHL(B, C)
            Case &HA     ' LD     'A,(BC)
                A = ReadM(B * 256 + C)
            Case &HB     ' DEC(BC)
                Dec16(B, C)
            Case &HC    ' INC(C)
                Inc(C)
            Case &HD      ' DEC(C)
                Dec(C)
            Case &HE      ' LD     'C,nn
                C = Pb()
            Case &HF  'RRCA
                Rrca()
            Case &H10 '00 STOP     '     '     '     '  ---- special ??? (old djnz disp)
                If Smp = 0 Then
                    If Mem.JoyVal1 = 0 And Mem.JoyVal2 = 0 Then PC = PC - 1 Else PC = PC + 1
                Else
                    If CpuS = 0 Then CpuS = 1 Else CpuS = 0
                    RAM(65357, 0) = CpuS * 128
                    Smp = 0
                End If
            Case &H11     ' LD DE, nnnn
                E = Pb()
                D = Pb()
            Case &H12     ' LD (DE), a
                WriteM(D * 256 + E, A)
            Case &H13     ' INC(DE)
                Inc16(D, E)
            Case &H14     ' INC(d)
                Inc(D)
            Case &H15     ' DEC(d)
                Dec(D)
            Case &H16     ' LD d, nn
                D = Pb()
            Case &H17     ' RLA
                Rla()
            Case &H18     ' JR disp
                Jr(Pb)
            Case &H19     ' Add(HL), DE
                AddHL(D, E)
            Case &H1A    ' LD     'A,(DE)
                A = ReadM(D * 256 + E)
            Case &H1B    ' DEC(DE)
                Dec16(D, E)
            Case &H1C    ' INC(E)
                Inc(E)
            Case &H1D     ' DEC(E)
                Dec(E)
            Case &H1E     ' LD     'E,nn
                E = Pb()
            Case &H1F     'RRA
                Rra()
            Case &H20     ' JR(nz, disp)
                Jr(Pb, 1 - GetZ())
            Case &H21      ' LD HL, nnnn
                L = Pb()
                H = Pb()
            Case &H22     ' LDI  (HL),A     '     ' ---- special (old ld (nnnn),hl)
                WriteM(H * 256 + L, A)
                Inc16(H, L)
            Case &H23     ' INC(HL)
                Inc16(H, L)
            Case &H24     ' INC(H)
                Inc(H)
            Case &H25     ' DEC(H)
                Dec(H)
            Case &H26     ' LD H, nn
                H = Pb()
            Case &H27     ' DAA
                Daa()
            Case &H29     ' Add(HL), HL
                AddHL(H, L)
            Case &H2A    ' LDI  A,(HL)     '     ' ---- special (old ld hl,(nnnn))
                A = ReadM(H * 256 + L)
                Inc16(H, L)
            Case &H2B    ' DEC(HL)
                Dec16(H, L)
            Case &H2C    ' INC(L)
                Inc(L)
            Case &H2D     ' DEC(L)
                Dec(L)
            Case &H2E     ' LD     'L,nn
                L = Pb()
            Case &H2F 'CPL
                Cpl()
            Case &H30     ' JR(NC, disp)
                Jr(Pb, 1 - GetC())
            Case &H31     ' LD sp, nnnn
                SP = Pw()
            Case &H32     ' LDD  (HL),A     '     ' ---- special (old remapped ld (nnnn),a)
                WriteM(H * 256 + L, A)
                Dec16(H, L)
            Case &H33     ' INC(sp)
                SP = SP + 1
                SP = SP And 65535
            Case &H34     ' INC (HL)
                MemPtr = ReadM(H * 256 + L)
                Inc(MemPtr)
                WriteM(H * 256 + L, MemPtr)
            Case &H35     ' DEC (HL)
                MemPtr = ReadM(H * 256 + L)
                Dec(MemPtr)
                WriteM(H * 256 + L, MemPtr)
            Case &H36     ' LD (HL), nn
                WriteM(H * 256 + L, Pb)
            Case &H37     ' SCF
                SetC(1)
                SetN(0)
                SetH(0)
            Case &H38     ' JR(c, disp)
                Jr(Pb, GetC)
            Case &H39      ' Add(HL), sp
                AddHL(SP \ 256, SP And 256)
            Case &H3A    ' LDD  A,(HL)     '     ' ---- special (old remapped ld a,(nnnn))
                A = ReadM(H * 256 + L)
                Dec16(H, L)
            Case &H3B    ' DEC(SP)
                SP = SP - 1
                If SP = -1 Then SP = 65535
            Case &H3C    ' INC(A)
                Inc(A)
            Case &H3D     ' DEC(A)
                Dec(A)
            Case &H3E     ' LD     'A,nn
                A = Pb()
            Case &H3F 'CCF
                SetC(1 - GetC())
                SetN(0)
                SetH(0)
            Case &H40     ' LD     'B,B     '     '     '     '     '     '     '     '
        'Stop 'nop
            Case &H60     ' LD     'H,B
                H = B
            Case &H41     ' LD     'B,C     '     '     '     '     '     '     '     '
                B = C
            Case &H61     ' LD     'H,C
                H = C
            Case &H42     ' LD     'B,D     '     '     '     '     '     '     '     '
                B = D
            Case &H62     ' LD     'H,D
                H = D
            Case &H43     ' LD     'B,E     '     '     '     '     '     '     '     '
                B = E
            Case &H63     ' LD     'H,E
                H = E
            Case &H44     ' LD     'B,H     '     '     '     '     '     '     '     '
                B = H
            Case &H64     ' LD     'H,H
        'Stop 'nop
            Case &H45     ' LD     'B,L     '     '     '     '     '     '     '     '
                B = L
            Case &H65     ' LD     'H,L
                H = L
            Case &H46     ' LD     'B,(HL)     '     '     '     '     '     '     '
                B = ReadM(H * 256 + L)
            Case &H66     ' LD     'H,(HL)
                H = ReadM(H * 256 + L)
            Case &H47     ' LD     'B,A     '     '     '     '     '     '     '     '
                B = A
            Case &H67     ' LD     'H,A
                H = A
            Case &H48     ' LD     'C,B     '     '     '     '     '     '     '     '
                C = B
            Case &H68     ' LD     'L,B
                L = B
            Case &H49     ' LD     'C,C     '     '     '     '     '     '     '     '
        'Stop 'nop
            Case &H69     ' LD     'L,C
                L = C
            Case &H4A    ' LD     'C,D     '     '     '     '     '     '     '     '
                C = D
            Case &H6A     ' LD     'L,D
                L = D
            Case &H4B    ' LD     'C,E     '     '     '     '     '     '     '     '
                C = E
            Case &H6B     ' LD     'L,E
                L = E
            Case &H4C    ' LD     'C,H     '     '     '     '     '     '     '     '
                C = H
            Case &H6C     ' LD     'L,H
                L = H
            Case &H4D     ' LD     'C,L     '     '     '     '     '     '     '     '
                C = L
            Case &H6D     ' LD     'L,L
        'Stop 'nop
            Case &H4E     ' LD     'C,(HL)     '     '     '     '     '     '     '
                C = ReadM(H * 256 + L)
            Case &H6E     ' LD     'L,(HL)
                L = ReadM(H * 256 + L)
            Case &H4F    ' LD     'C,A     '     '     '     '     '     '     '     '
                C = A
            Case &H6F     ' LD     'L,A
                L = A
            Case &H50     ' LD     'D,B     '     '     '     '     '     '     '     '
                D = B
            Case &H70     ' LD     '(HL),B
                WriteM(H * 256 + L, B)
            Case &H51     ' LD     'D,C     '     '     '     '     '     '     '     '
                D = C
            Case &H71     ' LD     '(HL),C
                WriteM(H * 256 + L, C)
            Case &H52     ' LD     'D,D     '     '     '     '     '     '     '     '
        'Stop 'nop
            Case &H72     ' LD     '(HL),D
                WriteM(H * 256 + L, D)
            Case &H53     ' LD     'D,E     '     '     '     '     '     '     '     '
                D = E
            Case &H73     ' LD     '(HL),E
                WriteM(H * 256 + L, E)
            Case &H54     ' LD     'D,H     '     '     '     '     '     '     '     '
                D = H
            Case &H74     ' LD     '(HL),H
                WriteM(H * 256 + L, H)
            Case &H55     ' LD     'D,L     '     '     '     '     '     '     '     '
                D = L
            Case &H75     ' LD     '(HL),L
                WriteM(H * 256 + L, L)
            Case &H56     ' LD     'D,(HL)     '     '     '     '     '     '     '
                D = ReadM(H * 256 + L)
            Case &H57     ' LD     'D,A     '     '     '     '     '     '     '     '
                D = A
            Case &H77     ' LD     '(HL),A
                WriteM(H * 256 + L, A)
            Case &H58     ' LD     'E,B     '     '     '     '     '     '     '     '
                E = B
            Case &H78     ' LD     'A,B
                A = B
            Case &H59     ' LD     'E,C     '     '     '     '     '     '     '     '
                E = C
            Case &H79     ' LD     'A,C
                A = C
            Case &H5A    ' LD     'E,D     '     '     '     '     '     '     '     '
                E = D
            Case &H7A     ' LD     'A,D
                A = D
            Case &H5B    ' LD     'E,E     '     '     '     '     '     '     '     '
        'Stop 'nop
            Case &H7B     ' LD     'A,E
                A = E
            Case &H5C    ' LD     'E,H     '     '     '     '     '     '     '     '
                E = H
            Case &H7C     ' LD     'A,H
                A = H
            Case &H5D     ' LD     'E,L     '     '     '     '     '     '     '     '
                E = L
            Case &H7D     ' LD     'A,L
                A = L
            Case &H5E     ' LD     'E,(HL)     '     '     '     '     '     '     '
                E = ReadM(H * 256 + L)
            Case &H7E     ' LD     'A,(HL)
                A = ReadM(H * 256 + L)
            Case &H5F    ' LD     'E,A     '     '     '     '     '     '     '     '
                E = A
            Case &H7F     ' LD     'A,A
        'Stop 'nop
            Case &H80     ' ADD(A),B     '     '     '     '     '     '     '     '
                Add(B)
            Case &HA0     ' AND  B
                Zand(B)
            Case &H81     ' ADD(A),C     '     '     '     '     '     '     '     '
                Add(C)
            Case &HA1     ' AND  C
                Zand(C)
            Case &H82     ' ADD(A),D     '     '     '     '     '     '     '     '
                Add(D)
            Case &HA2     ' AND  D
                Zand(D)
            Case &H83     ' ADD(A),E     '     '     '     '     '     '     '     '
                Add(E)
            Case &HA3     ' AND  E
                Zand(E)
            Case &H84     ' ADD(A),H     '     '     '     '     '     '     '     '
                Add(H)
            Case &HA4     ' AND  H
                Zand(H)
            Case &H85     ' ADD(A),L     '     '     '     '     '     '     '     '
                Add(L)
            Case &HA5     ' AND  L
                Zand(L)
            Case &H86     ' ADD(A),(HL)     '     '     '     '     '     '     '
                Add(ReadM(H * 256 + L))
            Case &HA6     ' AND  (HL)
                Zand(ReadM(H * 256 + L))
            Case &H87     ' ADD(A),A     '     '     '     '     '     '     '     '
                Add(A)
            Case &H88     ' ADC(A),B     '     '     '     '     '     '     '     '
                Adc(B)
            Case &HA8     ' XOR  B
                Zxor(B)
            Case &H89     ' ADC(A),C     '     '     '     '     '     '     '     '
                Adc(C)
            Case &HA9     ' XOR  C
                Zxor(C)
            Case &H8A    ' ADC(A),D     '     '     '     '     '     '     '     '
                Adc(D)
            Case &HAA     ' XOR  D
                Zxor(D)
            Case &H8B    ' ADC(A),E     '     '     '     '     '     '     '     '
                Adc(E)
            Case &HAB     ' XOR  E
                Zxor(E)
            Case &H8C    ' ADC(A),H     '     '     '     '     '     '     '     '
                Adc(H)
            Case &HAC     ' XOR  H
                Zxor(H)
            Case &H8D     ' ADC(A),L     '     '     '     '     '     '     '     '
                Adc(L)
            Case &HAD     ' XOR  L
                Zxor(L)
            Case &H8E     ' ADC(A),(HL)     '     '     '     '     '     '     '
                Adc(ReadM(H * 256 + L))
            Case &HAE     ' XOR  (HL)
                Zxor(ReadM(H * 256 + L))
            Case &H8F    ' ADC(A),A     '     '     '     '     '     '     '     '
                Adc(A)
            Case &HAF     ' XOR  A
                Zxor(A)
            Case &H90     ' SUB  B     '     '     '     '     '     '     '     '     '
                Zsub(B)
            Case &HB0     ' OR     'B
                Zor(B)
            Case &H91     ' SUB  C     '     '     '     '     '     '     '     '     '
                Zsub(C)
            Case &HB1     ' OR     'C
                Zor(C)
            Case &H92     ' SUB  D     '     '     '     '     '     '     '     '     '
                Zsub(D)
            Case &HB2     ' OR     'D
                Zor(D)
            Case &H93     ' SUB  E     '     '     '     '     '     '     '     '     '
                Zsub(E)
            Case &HB3     ' OR     'E
                Zor(E)
            Case &H94     ' SUB  H     '     '     '     '     '     '     '     '     '
                Zsub(H)
            Case &HB4     ' OR     'H
                Zor(H)
            Case &H95     ' SUB  L     '     '     '     '     '     '     '     '     '
                Zsub(L)
            Case &HB5     ' OR     'L
                Zor(L)
            Case &H96     ' SUB  (HL)     '     '     '     '     '     '     '     '
                Zsub(ReadM(H * 256 + L))
            Case &HB6     ' OR     '(HL)
                Zor(ReadM(H * 256 + L))
            Case &H97     ' SUB  A     '     '     '     '     '     '     '     '     '
                Zsub(A)
            Case &HB7     ' OR     'A
                Zor(A)
            Case &H98     ' SBC(A),B     '     '     '     '     '     '     '     '
                Sbc(B)
            Case &HB8     ' CP( )'B
                Cp(B)
            Case &H99     ' SBC(A),C     '     '     '     '     '     '     '     '
                Sbc(C)
            Case &HB9     ' CP( )'C
                Cp(C)
            Case &H9A    ' SBC(A),D     '     '     '     '     '     '     '     '
                Sbc(D)
            Case &HBA     ' CP( )'D
                Cp(D)
            Case &H9B    ' SBC(A),E     '     '     '     '     '     '     '     '
                Sbc(E)
            Case &HBB     ' CP( )'E
                Cp(E)
            Case &H9C    ' SBC(A),H     '     '     '     '     '     '     '     '
                Sbc(H)
            Case &HBC     ' CP( )'H
                Cp(H)
            Case &H9D     ' SBC(A),L     '     '     '     '     '     '     '     '
                Sbc(L)
            Case &HBD     ' CP( )'L
                Cp(L)
            Case &H9E     ' SBC(A),(HL)     '     '     '     '     '     '     '
                Sbc(ReadM(H * 256 + L))
            Case &HBE     ' CP( )'(HL)
                Cp(ReadM(H * 256 + L))
            Case &H9F    ' SBC(A),A     '     '     '     '     '     '     '     '
                Sbc(A)
            Case &HBF     ' CP( )'A
                Cp(A)
            Case &HC0     ' RET(NZ
                Ret(1 - GetZ())
            Case &HC1     ' POP(BC)
                Pop(C)
                Pop(B)
            Case &HC2     ' JP( )'NZ,nnnn
                Jp(Pw, 1 - GetZ())
            Case &HC3     ' JP( )'nnnn
                PC = Pw()
            Case &HC4     ' CALL NZ,nnnn
                Zcall(Pw, 1 - GetZ())
            Case &HC5     ' PUSH(BC)
                Push(B)
                Push(C)
            Case &HC6     ' ADD(A),nn
                Add(Pb)
            Case &HC7     ' RST(00H)
                Rst(0)
            Case &HC8     ' RET(Z)
                Ret(GetZ)
            Case &HC9 'RET
                Ret()
            Case &HCA     ' JP( )'Z,nnnn
                Jp(Pw, GetZ)
            Case &HCB 'nn ---(see beyond)---
                PbMemVal = Pb()

                Select Case PbMemVal
                    Case &H0   'RLC(B)
                        Rlc(B)
                    Case &H1   'RLC(C)
                        Rlc(C)
                    Case &H2   'RLC(D)
                        Rlc(D)
                    Case &H3   'RLC(E)
                        Rlc(E)
                    Case &H4   'RLC(H)
                        Rlc(H)
                    Case &H5   'RLC(L)
                        Rlc(L)
                    Case &H6   'RLC( )(HL)
                        Clcount = Clcount + 8
                        MemPtr = ReadM(H * 256 + L)
                        Rlc(MemPtr)
                        WriteM(H * 256 + L, MemPtr)
                    Case &H7   'RLC(A)
                        Rlc(A)
                    Case &H8   'RRC(B)
                        Rrc(B)
                    Case &H9   'RRC(C)
                        Rrc(C)
                    Case &HA   'RRC(D)
                        Rrc(D)
                    Case &HB   'RRC(E)
                        Rrc(E)
                    Case &HC   'RRC(H)
                        Rrc(H)
                    Case &HD   'RRC(L)
                        Rrc(L)
                    Case &HE   'RRC( )(HL)
                        Clcount = Clcount + 8
                        MemPtr = ReadM(H * 256 + L)
                        Rrc(MemPtr)
                        WriteM(H * 256 + L, MemPtr)
                    Case &HF   'RRC(A)
                        Rrc(A)
                    Case &H10  'RL( )'B
                        Rl(B)
                    Case &H11  'RL( )'C
                        Rl(C)
                    Case &H12  'RL( )'D
                        Rl(D)
                    Case &H13  'RL( )'E
                        Rl(E)
                    Case &H14  'RL( )'H
                        Rl(H)
                    Case &H15  'RL( )'L
                        Rl(L)
                    Case &H16  'RL( )'(HL)
                        Clcount = Clcount + 8
                        MemPtr = ReadM(H * 256 + L)
                        Rl(MemPtr)
                        WriteM(H * 256 + L, MemPtr)
                    Case &H17  'RL( )'A
                        Rl(A)
                    Case &H18  'RR( )'B
                        Rr(B)
                    Case &H19  'RR( )'C
                        Rr(C)
                    Case &H1A  'RR( )'D
                        Rr(D)
                    Case &H1B  'RR( )'E
                        Rr(E)
                    Case &H1C  'RR( )'H
                        Rr(H)
                    Case &H1D  'RR( )'L
                        Rr(L)
                    Case &H1E  'RR( )'(HL)
                        Clcount = Clcount + 8
                        PbMemVal = ReadM(H * 256 + L)
                        Rr(PbMemVal)
                        WriteM(H * 256 + L, PbMemVal)
                    Case &H1F  'RR( )'A
                        Rr(A)
                    Case &H20  'SLA(B)
                        Sla(B)
                    Case &H21  'SLA(C)
                        Sla(C)
                    Case &H22  'SLA(D)
                        Sla(D)
                    Case &H23  'SLA(E)
                        Sla(E)
                    Case &H24        'SLA(H)
                        Sla(H)
                    Case &H25  'SLA(L)
                        Sla(L)
                    Case &H26  'SLA( )(HL)
                        Clcount = Clcount + 8
                        PbMemVal = ReadM(H * 256 + L)
                        Sla(PbMemVal)
                        WriteM(H * 256 + L, PbMemVal)
                    Case &H27  'SLA(A)
                        Sla(A)
                    Case &H28  'SRA(B)
                        Sra(B)
                    Case &H29  'SRA(C)
                        Sra(C)
                    Case &H2A  'SRA(D)
                        Sra(D)
                    Case &H2B  'SRA(E)
                        Sra(E)
                    Case &H2C  'SRA(H)
                        Sra(H)
                    Case &H2D  'SRA(L)
                        Sra(L)
                    Case &H2E  'SRA( )(HL)
                        Clcount = Clcount + 8
                        PbMemVal = ReadM(H * 256 + L)
                        Sra(PbMemVal)
                        WriteM(H * 256 + L, PbMemVal)
                    Case &H2F  'SRA(A)
                        Sra(A)
                    Case &H30  'SWAP(B     )'     '     '  ---- special (old sll)
                        Swap(B)
                    Case &H31  'SWAP(C     )'     '     '  ---- special ""
                        Swap(C)
                    Case &H32  'SWAP(D     )'     '     '  ---- special ""
                        Swap(D)
                    Case &H33  'SWAP(E     )'     '     '  ---- special ""
                        Swap(E)
                    Case &H34  'SWAP(H     )'     '     '  ---- special ""
                        Swap(H)
                    Case &H35  'SWAP(L     )'     '     '  ---- special ""
                        Swap(L)
                    Case &H36  'SWAP (HL)     '     '  ---- special ""
                        Clcount = Clcount + 8
                        PbMemVal = ReadM(H * 256 + L)
                        Swap(PbMemVal)
                        WriteM(H * 256 + L, PbMemVal)
                    Case &H37  'SWAP(A     )'     '     '  ---- special ""
                        Swap(A)
                    Case &H38  'SRL(B)
                        Srl(B)
                    Case &H39  'SRL(C)
                        Srl(C)
                    Case &H3A  'SRL(D)
                        Srl(D)
                    Case &H3B  'SRL(E)
                        Srl(E)
                    Case &H3C  'SRL(H)
                        Srl(H)
                    Case &H3D  'SRL(L)
                        Srl(L)
                    Case &H3E  'SRL( )(HL)
                        Clcount = Clcount + 8
                        PbMemVal = ReadM(H * 256 + L)
                        Srl(PbMemVal)
                        WriteM(H * 256 + L, PbMemVal)
                    Case &H3F  'SRL(A)
                        Srl(A)
                    Case Else
                        Select Case PbMemVal And 199
                            Case &H40 '+n*38  BIT(n, B)
                                Bit(B, (BITT(PbMemVal And 56)))
                            Case &H41 '+n*38  BIT(n, C)
                                Bit(C, (BITT(PbMemVal And 56)))
                            Case &H42 '+n*38  BIT(n, D)
                                Bit(D, (BITT(PbMemVal And 56)))
                            Case &H43 '+n*38  BIT(n, E)
                                Bit(E, (BITT(PbMemVal And 56)))
                            Case &H44 '+n*38  BIT(n, H)
                                Bit(H, (BITT(PbMemVal And 56)))
                            Case &H45 '+n*38  BIT(n, L)
                                Bit(L, (BITT(PbMemVal And 56)))
                            Case &H46 '+n*38  BIT(n, (HL))
                                Clcount = Clcount + 8
                                MemPtr = ReadM(H * 256 + L)
                                Bit(MemPtr, (BITT(PbMemVal And 56)))
                            Case &H47 '+n*38  BIT(n, A)
                                Bit(A, (BITT(PbMemVal And 56)))
                            Case &H80 '+ n * 38 'RES(n, B)
                                Res(B, (SETT(PbMemVal And 56)))
                            Case &H81 '+ n * 38 'RES(n, C)
                                Res(C, (SETT(PbMemVal And 56)))
                            Case &H82 '+ n * 38 'RES(n, D)
                                Res(D, (SETT(PbMemVal And 56)))
                            Case &H83 '+ n * 38 'RES(n, E)
                                Res(E, (SETT(PbMemVal And 56)))
                            Case &H84 '+ n * 38 'RES(n, H)
                                Res(H, (SETT(PbMemVal And 56)))
                            Case &H85 '+ n * 38 'RES(n, L)
                                Res(L, (SETT(PbMemVal And 56)))
                            Case &H86 '+ n * 38 'RES(n, (HL))
                                Clcount = Clcount + 8
                                MemPtr = ReadM(H * 256 + L)
                                Res(MemPtr, (SETT(PbMemVal And 56)))
                                WriteM(H * 256 + L, MemPtr)
                            Case &H87 '+ n * 38 'RES(n, A)
                                Res(A, (SETT(PbMemVal And 56)))
                            Case &HC0 '+ n * 38 'SET  n,B
                                Zset(B, (BITT(PbMemVal And 56)))
                            Case &HC1 '+ n * 38 'SET  n,C
                                Zset(C, (BITT(PbMemVal And 56)))
                            Case &HC2 '+ n * 38 'SET  n,D
                                Zset(D, (BITT(PbMemVal And 56)))
                            Case &HC3 '+ n * 38 'SET  n,E
                                Zset(E, (BITT(PbMemVal And 56)))
                            Case &HC4 '+ n * 38 'SET  n,H
                                Zset(H, (BITT(PbMemVal And 56)))
                            Case &HC5 '+ n * 38 'SET  n,L
                                Zset(L, (BITT(PbMemVal And 56)))
                            Case &HC6 '+ n * 38 'SET  n,(HL)
                                Clcount = Clcount + 8
                                MemPtr = ReadM(H * 256 + L)
                                Zset(MemPtr, (BITT(PbMemVal And 56)))
                                WriteM(H * 256 + L, MemPtr)
                            Case &HC7 '+ n * 38 'SET n,A
                                Zset(A, (BITT(PbMemVal And 56)))
                        End Select
                End Select
            Case &HCC     ' CALL Z,nnnn
                Zcall(Pw, GetZ)
            Case &HCD     ' CALL nnnn
                Zcall(Pw)
            Case &HCE     ' ADC(A),nn
                Adc(Pb)
            Case &HCF     ' RST(8)
                Rst(8)
            Case &HD0     ' RET(NC
                Ret(1 - GetC())
            Case &HD1     ' POP(DE)
                Pop(E)
                Pop(D)
            Case &HD2     ' JP( )'NC,nnnn
                Jp(Pw, 1 - GetC())
            Case &HD3     ' -     '     '     '     '     '  ---- ??? (old out (nn),a)
        'Stop
            Case &HD4     ' CALL NC,nnnn
                Zcall(Pw, 1 - GetC())
            Case &HD5     ' PUSH(DE)
                Push(D)
                Push(E)
            Case &HD6     ' SUB  nn
                Zsub(Pb)
            Case &HD7     ' RST(10H)
                Rst(16)
            Case &HD8     ' RET(C)
                Ret(GetC)
            Case &HD9     ' RETI     '     '     '     '  ---- remapped (old exx)
                Reti()
            Case &HDA     ' JP( )'C,nnnn
                Jp(Pw, GetC)
            Case &HDB     ' -     '     '     '     '     '  ---- ??? (old in a,(nn))
        'Stop
            Case &HDC     ' CALL C,nnnn
                Zcall(Pw, GetC)
            Case &HDD     ' -     '     '     '     '     '  ---- ??? (old ix-commands)
        'Stop
            Case &HDE     ' SBC(A),nn     '  (nocash added, this opcode does existed, e.g. used by kwirk)
                Sbc(Pb)
            Case &HDF     ' RST(18H)
                Rst(24)
            Case &HE0     ' LD     '($FF00+nn),A ---- special (old ret(po))
                WriteM(65280 + Pb(), A)
            Case &HE1     ' POP(HL)
                Pop(L)
                Pop(H)
            Case &HE2     ' LD     '($FF00+C),A  ---- special (old jp(po),nnnn)
                WriteM(65280 + C, A)
            Case &HE3     ' -     '     '     '     '     '  ---- ??? (old ex (sp),hl)
        'Stop
            Case &HE4     ' -     '     '     '     '     '  ---- ??? (old call po,nnnn)
        'Stop
            Case &HE5     ' PUSH(HL)
                Push(H)
                Push(L)
            Case &HE6     ' AND  nn
                Zand(Pb)
            Case &HE7     ' RST(20H)
                Rst(32)
            Case &HE8     ' ADD(SP),dd     '     '  ---- special (old ret(pe)) (nocash extended as shortint)
                AddSP(Pb)
            Case &HE9 'JP(HL)
                Jp(H * 256 + L)
            Case &HEB     ' -     '     '     '     '     '  ---- ??? (old ex de,hl)
        'Stop
            Case &HEC     ' -     '     '     '     '     '  ---- ??? (old call pe,nnnn)
        'Stop
            Case &HED     ' -     '     '     '     '     '  ---- ??? (old ed-commands)
        'Stop
            Case &HEE     ' XOR  nn
                Zxor(Pb)
            Case &HEF     ' RST(28H)
                Rst(40)
            Case &HF1     ' POP(AF)
                Pop(F)
                Pop(A)
            Case &HF2     ' LD     'A,(C)     '     '  ---- special (old jp(p),nnnn)
                A = ReadM(65280 + C)
            Case &HF3 'DI
                ime_stat = 2
            Case &HF4     ' -     '     '     '     '     '  ---- ??? (old call p,nnnn)
        'Stop
            Case &HF5     ' PUSH(AF)
                Push(A)
                Push(F)
            Case &HF6     ' OR     'nn
                Zor(Pb)
            Case &HF7     ' RST(30H)
                Rst(48)
            Case &HF8     ' LD     'HL,SP+dd     '  ---- special (old ret(m)) (nocash corrected)
                MemPtr = Pb()
                If MemPtr > 127 Then MemPtr = MemPtr - 256
                PbMemVal = (SP + MemPtr) And 65535
                If MemPtr >= 0 Then
                    SetC(SP > PbMemVal)
                    SetH(((SP Xor MemPtr Xor PbMemVal) And 4096) > 0)
                    H = PbMemVal \ 256
                    L = PbMemVal And 255
                Else
                    SetC(SP > PbMemVal)
                    SetH(((SP Xor MemPtr Xor PbMemVal) And 4096) > 0)
                    H = PbMemVal \ 256
                    L = PbMemVal And 255
                End If
                SetZ(0)
                SetN(0)
            Case &HF9     ' LD     'SP,HL
                SP = H * 256 + L
            Case &HFB 'EI
                ime_stat = 1
            Case &HFC     ' -     '     '     '     '     '  ---- ??? (old call m,nnnn)
        'Stop
            Case &HFD     ' -     '     '     '     '     '  ---- ??? (old iy-commands)
        'Stop
            Case &HFE     ' CP( )'nn
                Cp(Pb)
            Case &HFF     ' RST(38H)
                Rst(56)
        End Select

    End Sub

    Sub RunCycles2()
        PbMemVal = Pb()
        Clcount = Clcount + Cpc(PbMemVal)
        UTimer(Cpc(PbMemVal))
        'If utu Then UTimer(Cpc(PbMemVal))
        If Tes Then
            If CpuS = 0 Then
                UpDateSnd(Cpc(PbMemVal))
            Else
                UpDateSnd(Cpc(PbMemVal) / 2)
            End If
        End If
        If ime_stat = 3 Then IME = True : ime_stat = 0
        If ime_stat = 4 Then IME = False : ime_stat = 0
        If ime_stat = 1 Then ime_stat = 3
        If ime_stat = 2 Then ime_stat = 4

        Select Case PbMemVal
            Case 0 '     ' NOP

            Case 1     '  'LD BC, nnnn
                C = Pb()
                B = Pb()
            Case 2     '  LD (BC), a
                WriteM(B * 256 + C, A)
            Case 3     '  INC(BC)
                Inc16(B, C)
            Case 4     '  INC(b)
                Inc(B)
            Case 5     '  DEC(b)
                Dec(B)
            Case 6     '  LD b, nn
                B = Pb()
            Case 7     '  RLCA
                Rlca()
            Case 8      ' LD     '(nnnn),SP     ' ---- special (old ex af,af)
                MemPtr = Pw()
                WriteM(MemPtr, SP And 255)
                WriteM(MemPtr + 1, SP \ 256)
            Case 9     '  Add(HL), BC
                AddHL(B, C)
            Case &HA     ' LD     'A,(BC)
                A = ReadM(B * 256 + C)
            Case &HB     ' DEC(BC)
                Dec16(B, C)
            Case &HC    ' INC(C)
                Inc(C)
            Case &HD      ' DEC(C)
                Dec(C)
            Case &HE      ' LD     'C,nn
                C = Pb()
            Case &HF  'RRCA
                Rrca()
            Case &H10 '00 STOP     '     '     '     '  ---- special ??? (old djnz disp)
                If Smp = 0 Then
                    If Mem.JoyVal1 = 0 And Mem.JoyVal2 = 0 Then PC = PC - 1 Else PC = PC + 1
                Else
                    If CpuS = 0 Then CpuS = 1 Else CpuS = 0
                    RAM(65357, 0) = CpuS * 128
                    Smp = 0
                End If
            Case &H11     ' LD DE, nnnn
                E = Pb()
                D = Pb()
            Case &H12     ' LD (DE), a
                WriteM(D * 256 + E, A)
            Case &H13     ' INC(DE)
                Inc16(D, E)
            Case &H14     ' INC(d)
                Inc(D)
            Case &H15     ' DEC(d)
                Dec(D)
            Case &H16     ' LD d, nn
                D = Pb()
            Case &H17     ' RLA
                Rla()
            Case &H18     ' JR disp
                Jr(Pb)
            Case &H19     ' Add(HL), DE
                AddHL(D, E)
            Case &H1A    ' LD     'A,(DE)
                A = ReadM(D * 256 + E)
            Case &H1B    ' DEC(DE)
                Dec16(D, E)
            Case &H1C    ' INC(E)
                Inc(E)
            Case &H1D     ' DEC(E)
                Dec(E)
            Case &H1E     ' LD     'E,nn
                E = Pb()
            Case &H1F     'RRA
                Rra()
            Case &H20     ' JR(nz, disp)
                Jr(Pb, 1 - GetZ())
            Case &H21      ' LD HL, nnnn
                L = Pb()
                H = Pb()
            Case &H22     ' LDI  (HL),A     '     ' ---- special (old ld (nnnn),hl)
                WriteM(H * 256 + L, A)
                Inc16(H, L)
            Case &H23     ' INC(HL)
                Inc16(H, L)
            Case &H24     ' INC(H)
                Inc(H)
            Case &H25     ' DEC(H)
                Dec(H)
            Case &H26     ' LD H, nn
                H = Pb()
            Case &H27     ' DAA
                Daa()
            Case &H28     ' JR(z, disp)
                Jr(Pb, GetZ)
            Case &H29     ' Add(HL), HL
                AddHL(H, L)
            Case &H2A    ' LDI  A,(HL)     '     ' ---- special (old ld hl,(nnnn))
                A = ReadM(H * 256 + L)
                Inc16(H, L)
            Case &H2B    ' DEC(HL)
                Dec16(H, L)
            Case &H2C    ' INC(L)
                Inc(L)
            Case &H2D     ' DEC(L)
                Dec(L)
            Case &H2E     ' LD     'L,nn
                L = Pb()
            Case &H2F 'CPL
                Cpl()
            Case &H30     ' JR(NC, disp)
                Jr(Pb, 1 - GetC())
            Case &H31     ' LD sp, nnnn
                SP = Pw()
            Case &H32     ' LDD  (HL),A     '     ' ---- special (old remapped ld (nnnn),a)
                WriteM(H * 256 + L, A)
                Dec16(H, L)
            Case &H33     ' INC(sp)
                SP = SP + 1
                SP = SP And 65535
            Case &H34     ' INC (HL)
                MemPtr = ReadM(H * 256 + L)
                Inc(MemPtr)
                WriteM(H * 256 + L, MemPtr)
            Case &H35     ' DEC (HL)
                MemPtr = ReadM(H * 256 + L)
                Dec(MemPtr)
                WriteM(H * 256 + L, MemPtr)
            Case &H36     ' LD (HL), nn
                WriteM(H * 256 + L, Pb)
            Case &H37     ' SCF
                SetC(1)
                SetN(0)
                SetH(0)
            Case &H38     ' JR(c, disp)
                Jr(Pb, GetC)
            Case &H39      ' Add(HL), sp
                AddHL(SP \ 256, SP And 256)
            Case &H3A    ' LDD  A,(HL)     '     ' ---- special (old remapped ld a,(nnnn))
                A = ReadM(H * 256 + L)
                Dec16(H, L)
            Case &H3B    ' DEC(SP)
                SP = SP - 1
                If SP = -1 Then SP = 65535
            Case &H3C    ' INC(A)
                Inc(A)
            Case &H3D     ' DEC(A)
                Dec(A)
            Case &H3E     ' LD     'A,nn
                A = Pb()
            Case &H3F 'CCF
                SetC(1 - GetC())
                SetN(0)
                SetH(0)
            Case &H40     ' LD     'B,B     '     '     '     '     '     '     '     '
        'Stop 'nop
            Case &H60     ' LD     'H,B
                H = B
            Case &H41     ' LD     'B,C     '     '     '     '     '     '     '     '
                B = C
            Case &H61     ' LD     'H,C
                H = C
            Case &H42     ' LD     'B,D     '     '     '     '     '     '     '     '
                B = D
            Case &H62     ' LD     'H,D
                H = D
            Case &H43     ' LD     'B,E     '     '     '     '     '     '     '     '
                B = E
            Case &H63     ' LD     'H,E
                H = E
            Case &H44     ' LD     'B,H     '     '     '     '     '     '     '     '
                B = H
            Case &H64     ' LD     'H,H
        'Stop 'nop
            Case &H45     ' LD     'B,L     '     '     '     '     '     '     '     '
                B = L
            Case &H65     ' LD     'H,L
                H = L
            Case &H46     ' LD     'B,(HL)     '     '     '     '     '     '     '
                B = ReadM(H * 256 + L)
            Case &H66     ' LD     'H,(HL)
                H = ReadM(H * 256 + L)
            Case &H47     ' LD     'B,A     '     '     '     '     '     '     '     '
                B = A
            Case &H67     ' LD     'H,A
                H = A
            Case &H48     ' LD     'C,B     '     '     '     '     '     '     '     '
                C = B
            Case &H68     ' LD     'L,B
                L = B
            Case &H49     ' LD     'C,C     '     '     '     '     '     '     '     '
        'Stop 'nop
            Case &H69     ' LD     'L,C
                L = C
            Case &H4A    ' LD     'C,D     '     '     '     '     '     '     '     '
                C = D
            Case &H6A     ' LD     'L,D
                L = D
            Case &H4B    ' LD     'C,E     '     '     '     '     '     '     '     '
                C = E
            Case &H6B     ' LD     'L,E
                L = E
            Case &H4C    ' LD     'C,H     '     '     '     '     '     '     '     '
                C = H
            Case &H6C     ' LD     'L,H
                L = H
            Case &H4D     ' LD     'C,L     '     '     '     '     '     '     '     '
                C = L
            Case &H6D     ' LD     'L,L
        'Stop 'nop
            Case &H4E     ' LD     'C,(HL)     '     '     '     '     '     '     '
                C = ReadM(H * 256 + L)
            Case &H6E     ' LD     'L,(HL)
                L = ReadM(H * 256 + L)
            Case &H4F    ' LD     'C,A     '     '     '     '     '     '     '     '
                C = A
            Case &H6F     ' LD     'L,A
                L = A
            Case &H50     ' LD     'D,B     '     '     '     '     '     '     '     '
                D = B
            Case &H70     ' LD     '(HL),B
                WriteM(H * 256 + L, B)
            Case &H51     ' LD     'D,C     '     '     '     '     '     '     '     '
                D = C
            Case &H71     ' LD     '(HL),C
                WriteM(H * 256 + L, C)
            Case &H52     ' LD     'D,D     '     '     '     '     '     '     '     '
        'Stop 'nop
            Case &H72     ' LD     '(HL),D
                WriteM(H * 256 + L, D)
            Case &H53     ' LD     'D,E     '     '     '     '     '     '     '     '
                D = E
            Case &H73     ' LD     '(HL),E
                WriteM(H * 256 + L, E)
            Case &H54     ' LD     'D,H     '     '     '     '     '     '     '     '
                D = H
            Case &H74     ' LD     '(HL),H
                WriteM(H * 256 + L, H)
            Case &H55     ' LD     'D,L     '     '     '     '     '     '     '     '
                D = L
            Case &H75     ' LD     '(HL),L
                WriteM(H * 256 + L, L)
            Case &H56     ' LD     'D,(HL)     '     '     '     '     '     '     '
                D = ReadM(H * 256 + L)
            Case &H76     ' HALT
                Halt()
            Case &H57     ' LD     'D,A     '     '     '     '     '     '     '     '
                D = A
            Case &H77     ' LD     '(HL),A
                WriteM(H * 256 + L, A)
            Case &H58     ' LD     'E,B     '     '     '     '     '     '     '     '
                E = B
            Case &H78     ' LD     'A,B
                A = B
            Case &H59     ' LD     'E,C     '     '     '     '     '     '     '     '
                E = C
            Case &H79     ' LD     'A,C
                A = C
            Case &H5A    ' LD     'E,D     '     '     '     '     '     '     '     '
                E = D
            Case &H7A     ' LD     'A,D
                A = D
            Case &H5B    ' LD     'E,E     '     '     '     '     '     '     '     '
        'Stop 'nop
            Case &H7B     ' LD     'A,E
                A = E
            Case &H5C    ' LD     'E,H     '     '     '     '     '     '     '     '
                E = H
            Case &H7C     ' LD     'A,H
                A = H
            Case &H5D     ' LD     'E,L     '     '     '     '     '     '     '     '
                E = L
            Case &H7D     ' LD     'A,L
                A = L
            Case &H5E     ' LD     'E,(HL)     '     '     '     '     '     '     '
                E = ReadM(H * 256 + L)
            Case &H7E     ' LD     'A,(HL)
                A = ReadM(H * 256 + L)
            Case &H5F    ' LD     'E,A     '     '     '     '     '     '     '     '
                E = A
            Case &H7F     ' LD     'A,A
        'Stop 'nop
            Case &H80     ' ADD(A),B     '     '     '     '     '     '     '     '
                Add(B)
            Case &HA0     ' AND  B
                Zand(B)
            Case &H81     ' ADD(A),C     '     '     '     '     '     '     '     '
                Add(C)
            Case &HA1     ' AND  C
                Zand(C)
            Case &H82     ' ADD(A),D     '     '     '     '     '     '     '     '
                Add(D)
            Case &HA2     ' AND  D
                Zand(D)
            Case &H83     ' ADD(A),E     '     '     '     '     '     '     '     '
                Add(E)
            Case &HA3     ' AND  E
                Zand(E)
            Case &H84     ' ADD(A),H     '     '     '     '     '     '     '     '
                Add(H)
            Case &HA4     ' AND  H
                Zand(H)
            Case &H85     ' ADD(A),L     '     '     '     '     '     '     '     '
                Add(L)
            Case &HA5     ' AND  L
                Zand(L)
            Case &H86     ' ADD(A),(HL)     '     '     '     '     '     '     '
                Add(ReadM(H * 256 + L))
            Case &HA6     ' AND  (HL)
                Zand(ReadM(H * 256 + L))
            Case &H87     ' ADD(A),A     '     '     '     '     '     '     '     '
                Add(A)
            Case &HA7     ' AND  A
                Zand(A)
            Case &H88     ' ADC(A),B     '     '     '     '     '     '     '     '
                Adc(B)
            Case &HA8     ' XOR  B
                Zxor(B)
            Case &H89     ' ADC(A),C     '     '     '     '     '     '     '     '
                Adc(C)
            Case &HA9     ' XOR  C
                Zxor(C)
            Case &H8A    ' ADC(A),D     '     '     '     '     '     '     '     '
                Adc(D)
            Case &HAA     ' XOR  D
                Zxor(D)
            Case &H8B    ' ADC(A),E     '     '     '     '     '     '     '     '
                Adc(E)
            Case &HAB     ' XOR  E
                Zxor(E)
            Case &H8C    ' ADC(A),H     '     '     '     '     '     '     '     '
                Adc(H)
            Case &HAC     ' XOR  H
                Zxor(H)
            Case &H8D     ' ADC(A),L     '     '     '     '     '     '     '     '
                Adc(L)
            Case &HAD     ' XOR  L
                Zxor(L)
            Case &H8E     ' ADC(A),(HL)     '     '     '     '     '     '     '
                Adc(ReadM(H * 256 + L))
            Case &HAE     ' XOR  (HL)
                Zxor(ReadM(H * 256 + L))
            Case &H8F    ' ADC(A),A     '     '     '     '     '     '     '     '
                Adc(A)
            Case &HAF     ' XOR  A
                Zxor(A)
            Case &H90     ' SUB  B     '     '     '     '     '     '     '     '     '
                Zsub(B)
            Case &HB0     ' OR     'B
                Zor(B)
            Case &H91     ' SUB  C     '     '     '     '     '     '     '     '     '
                Zsub(C)
            Case &HB1     ' OR     'C
                Zor(C)
            Case &H92     ' SUB  D     '     '     '     '     '     '     '     '     '
                Zsub(D)
            Case &HB2     ' OR     'D
                Zor(D)
            Case &H93     ' SUB  E     '     '     '     '     '     '     '     '     '
                Zsub(E)
            Case &HB3     ' OR     'E
                Zor(E)
            Case &H94     ' SUB  H     '     '     '     '     '     '     '     '     '
                Zsub(H)
            Case &HB4     ' OR     'H
                Zor(H)
            Case &H95     ' SUB  L     '     '     '     '     '     '     '     '     '
                Zsub(L)
            Case &HB5     ' OR     'L
                Zor(L)
            Case &H96     ' SUB  (HL)     '     '     '     '     '     '     '     '
                Zsub(ReadM(H * 256 + L))
            Case &HB6     ' OR     '(HL)
                Zor(ReadM(H * 256 + L))
            Case &H97     ' SUB  A     '     '     '     '     '     '     '     '     '
                Zsub(A)
            Case &HB7     ' OR     'A
                Zor(A)
            Case &H98     ' SBC(A),B     '     '     '     '     '     '     '     '
                Sbc(B)
            Case &HB8     ' CP( )'B
                Cp(B)
            Case &H99     ' SBC(A),C     '     '     '     '     '     '     '     '
                Sbc(C)
            Case &HB9     ' CP( )'C
                Cp(C)
            Case &H9A    ' SBC(A),D     '     '     '     '     '     '     '     '
                Sbc(D)
            Case &HBA     ' CP( )'D
                Cp(D)
            Case &H9B    ' SBC(A),E     '     '     '     '     '     '     '     '
                Sbc(E)
            Case &HBB     ' CP( )'E
                Cp(E)
            Case &H9C    ' SBC(A),H     '     '     '     '     '     '     '     '
                Sbc(H)
            Case &HBC     ' CP( )'H
                Cp(H)
            Case &H9D     ' SBC(A),L     '     '     '     '     '     '     '     '
                Sbc(L)
            Case &HBD     ' CP( )'L
                Cp(L)
            Case &H9E     ' SBC(A),(HL)     '     '     '     '     '     '     '
                Sbc(ReadM(H * 256 + L))
            Case &HBE     ' CP( )'(HL)
                Cp(ReadM(H * 256 + L))
            Case &H9F    ' SBC(A),A     '     '     '     '     '     '     '     '
                Sbc(A)
            Case &HBF     ' CP( )'A
                Cp(A)
            Case &HC0     ' RET(NZ
                Ret(1 - GetZ())
            Case &HC1     ' POP(BC)
                Pop(C)
                Pop(B)
            Case &HC2     ' JP( )'NZ,nnnn
                Jp(Pw, 1 - GetZ())
            Case &HC3     ' JP( )'nnnn
                PC = Pw()
            Case &HC4     ' CALL NZ,nnnn
                Zcall(Pw, 1 - GetZ())
            Case &HC5     ' PUSH(BC)
                Push(B)
                Push(C)
            Case &HC6     ' ADD(A),nn
                Add(Pb)
            Case &HC7     ' RST(00H)
                Rst(0)
            Case &HC8     ' RET(Z)
                Ret(GetZ)
            Case &HC9 'RET
                Ret()
            Case &HCA     ' JP( )'Z,nnnn
                Jp(Pw, GetZ)
            Case &HCB 'nn ---(see beyond)---
                PbMemVal = Pb()

                Select Case PbMemVal
                    Case &H0   'RLC(B)
                        Rlc(B)
                    Case &H1   'RLC(C)
                        Rlc(C)
                    Case &H2   'RLC(D)
                        Rlc(D)
                    Case &H3   'RLC(E)
                        Rlc(E)
                    Case &H4   'RLC(H)
                        Rlc(H)
                    Case &H5   'RLC(L)
                        Rlc(L)
                    Case &H6   'RLC( )(HL)
                        Clcount = Clcount + 8
                        MemPtr = ReadM(H * 256 + L)
                        Rlc(MemPtr)
                        WriteM(H * 256 + L, MemPtr)
                    Case &H7   'RLC(A)
                        Rlc(A)
                    Case &H8   'RRC(B)
                        Rrc(B)
                    Case &H9   'RRC(C)
                        Rrc(C)
                    Case &HA   'RRC(D)
                        Rrc(D)
                    Case &HB   'RRC(E)
                        Rrc(E)
                    Case &HC   'RRC(H)
                        Rrc(H)
                    Case &HD   'RRC(L)
                        Rrc(L)
                    Case &HE   'RRC( )(HL)
                        Clcount = Clcount + 8
                        MemPtr = ReadM(H * 256 + L)
                        Rrc(MemPtr)
                        WriteM(H * 256 + L, MemPtr)
                    Case &HF   'RRC(A)
                        Rrc(A)
                    Case &H10  'RL( )'B
                        Rl(B)
                    Case &H11  'RL( )'C
                        Rl(C)
                    Case &H12  'RL( )'D
                        Rl(D)
                    Case &H13  'RL( )'E
                        Rl(E)
                    Case &H14  'RL( )'H
                        Rl(H)
                    Case &H15  'RL( )'L
                        Rl(L)
                    Case &H16  'RL( )'(HL)
                        Clcount = Clcount + 8
                        MemPtr = ReadM(H * 256 + L)
                        Rl(MemPtr)
                        WriteM(H * 256 + L, MemPtr)
                    Case &H17  'RL( )'A
                        Rl(A)
                    Case &H18  'RR( )'B
                        Rr(B)
                    Case &H19  'RR( )'C
                        Rr(C)
                    Case &H1A  'RR( )'D
                        Rr(D)
                    Case &H1B  'RR( )'E
                        Rr(E)
                    Case &H1C  'RR( )'H
                        Rr(H)
                    Case &H1D  'RR( )'L
                        Rr(L)
                    Case &H1E  'RR( )'(HL)
                        Clcount = Clcount + 8
                        PbMemVal = ReadM(H * 256 + L)
                        Rr(PbMemVal)
                        WriteM(H * 256 + L, PbMemVal)
                    Case &H1F  'RR( )'A
                        Rr(A)
                    Case &H20  'SLA(B)
                        Sla(B)
                    Case &H21  'SLA(C)
                        Sla(C)
                    Case &H22  'SLA(D)
                        Sla(D)
                    Case &H23  'SLA(E)
                        Sla(E)
                    Case &H24        'SLA(H)
                        Sla(H)
                    Case &H25  'SLA(L)
                        Sla(L)
                    Case &H26  'SLA( )(HL)
                        Clcount = Clcount + 8
                        PbMemVal = ReadM(H * 256 + L)
                        Sla(PbMemVal)
                        WriteM(H * 256 + L, PbMemVal)
                    Case &H27  'SLA(A)
                        Sla(A)
                    Case &H28  'SRA(B)
                        Sra(B)
                    Case &H29  'SRA(C)
                        Sra(C)
                    Case &H2A  'SRA(D)
                        Sra(D)
                    Case &H2B  'SRA(E)
                        Sra(E)
                    Case &H2C  'SRA(H)
                        Sra(H)
                    Case &H2D  'SRA(L)
                        Sra(L)
                    Case &H2E  'SRA( )(HL)
                        Clcount = Clcount + 8
                        PbMemVal = ReadM(H * 256 + L)
                        Sra(PbMemVal)
                        WriteM(H * 256 + L, PbMemVal)
                    Case &H2F  'SRA(A)
                        Sra(A)
                    Case &H30  'SWAP(B     )'     '     '  ---- special (old sll)
                        Swap(B)
                    Case &H31  'SWAP(C     )'     '     '  ---- special ""
                        Swap(C)
                    Case &H32  'SWAP(D     )'     '     '  ---- special ""
                        Swap(D)
                    Case &H33  'SWAP(E     )'     '     '  ---- special ""
                        Swap(E)
                    Case &H34  'SWAP(H     )'     '     '  ---- special ""
                        Swap(H)
                    Case &H35  'SWAP(L     )'     '     '  ---- special ""
                        Swap(L)
                    Case &H36  'SWAP (HL)     '     '  ---- special ""
                        Clcount = Clcount + 8
                        PbMemVal = ReadM(H * 256 + L)
                        Swap(PbMemVal)
                        WriteM(H * 256 + L, PbMemVal)
                    Case &H37  'SWAP(A     )'     '     '  ---- special ""
                        Swap(A)
                    Case &H38  'SRL(B)
                        Srl(B)
                    Case &H39  'SRL(C)
                        Srl(C)
                    Case &H3A  'SRL(D)
                        Srl(D)
                    Case &H3B  'SRL(E)
                        Srl(E)
                    Case &H3C  'SRL(H)
                        Srl(H)
                    Case &H3D  'SRL(L)
                        Srl(L)
                    Case &H3E  'SRL( )(HL)
                        Clcount = Clcount + 8
                        PbMemVal = ReadM(H * 256 + L)
                        Srl(PbMemVal)
                        WriteM(H * 256 + L, PbMemVal)
                    Case &H3F  'SRL(A)
                        Srl(A)
                    Case Else
                        Select Case PbMemVal And 199
                            Case &H40 '+n*38  BIT(n, B)
                                Bit(B, (BITT(PbMemVal And 56)))
                            Case &H41 '+n*38  BIT(n, C)
                                Bit(C, (BITT(PbMemVal And 56)))
                            Case &H42 '+n*38  BIT(n, D)
                                Bit(D, (BITT(PbMemVal And 56)))
                            Case &H43 '+n*38  BIT(n, E)
                                Bit(E, (BITT(PbMemVal And 56)))
                            Case &H44 '+n*38  BIT(n, H)
                                Bit(H, (BITT(PbMemVal And 56)))
                            Case &H45 '+n*38  BIT(n, L)
                                Bit(L, (BITT(PbMemVal And 56)))
                            Case &H46 '+n*38  BIT(n, (HL))
                                Clcount = Clcount + 8
                                MemPtr = ReadM(H * 256 + L)
                                Bit(MemPtr, (BITT(PbMemVal And 56)))
                            Case &H47 '+n*38  BIT(n, A)
                                Bit(A, (BITT(PbMemVal And 56)))
                            Case &H80 '+ n * 38 'RES(n, B)
                                Res(B, (SETT(PbMemVal And 56)))
                            Case &H81 '+ n * 38 'RES(n, C)
                                Res(C, (SETT(PbMemVal And 56)))
                            Case &H82 '+ n * 38 'RES(n, D)
                                Res(D, (SETT(PbMemVal And 56)))
                            Case &H83 '+ n * 38 'RES(n, E)
                                Res(E, (SETT(PbMemVal And 56)))
                            Case &H84 '+ n * 38 'RES(n, H)
                                Res(H, (SETT(PbMemVal And 56)))
                            Case &H85 '+ n * 38 'RES(n, L)
                                Res(L, (SETT(PbMemVal And 56)))
                            Case &H86 '+ n * 38 'RES(n, (HL))
                                Clcount = Clcount + 8
                                MemPtr = ReadM(H * 256 + L)
                                Res(MemPtr, (SETT(PbMemVal And 56)))
                                WriteM(H * 256 + L, MemPtr)
                            Case &H87 '+ n * 38 'RES(n, A)
                                Res(A, (SETT(PbMemVal And 56)))
                            Case &HC0 '+ n * 38 'SET  n,B
                                Zset(B, (BITT(PbMemVal And 56)))
                            Case &HC1 '+ n * 38 'SET  n,C
                                Zset(C, (BITT(PbMemVal And 56)))
                            Case &HC2 '+ n * 38 'SET  n,D
                                Zset(D, (BITT(PbMemVal And 56)))
                            Case &HC3 '+ n * 38 'SET  n,E
                                Zset(E, (BITT(PbMemVal And 56)))
                            Case &HC4 '+ n * 38 'SET  n,H
                                Zset(H, (BITT(PbMemVal And 56)))
                            Case &HC5 '+ n * 38 'SET  n,L
                                Zset(L, (BITT(PbMemVal And 56)))
                            Case &HC6 '+ n * 38 'SET  n,(HL)
                                Clcount = Clcount + 8
                                MemPtr = ReadM(H * 256 + L)
                                Zset(MemPtr, (BITT(PbMemVal And 56)))
                                WriteM(H * 256 + L, MemPtr)
                            Case &HC7 '+ n * 38 'SET n,A
                                Zset(A, (BITT(PbMemVal And 56)))
                        End Select
                End Select
            Case &HCC     ' CALL Z,nnnn
                Zcall(Pw, GetZ)
            Case &HCD     ' CALL nnnn
                Zcall(Pw)
            Case &HCE     ' ADC(A),nn
                Adc(Pb)
            Case &HCF     ' RST(8)
                Rst(8)
            Case &HD0     ' RET(NC
                Ret(1 - GetC())
            Case &HD1     ' POP(DE)
                Pop(E)
                Pop(D)
            Case &HD2     ' JP( )'NC,nnnn
                Jp(Pw, 1 - GetC())
            Case &HD3     ' -     '     '     '     '     '  ---- ??? (old out (nn),a)
        'Stop
            Case &HD4     ' CALL NC,nnnn
                Zcall(Pw, 1 - GetC())
            Case &HD5     ' PUSH(DE)
                Push(D)
                Push(E)
            Case &HD6     ' SUB  nn
                Zsub(Pb)
            Case &HD7     ' RST(10H)
                Rst(16)
            Case &HD8     ' RET(C)
                Ret(GetC)
            Case &HD9     ' RETI     '     '     '     '  ---- remapped (old exx)
                Reti()
            Case &HDA     ' JP( )'C,nnnn
                Jp(Pw, GetC)
            Case &HDB     ' -     '     '     '     '     '  ---- ??? (old in a,(nn))
        'Stop
            Case &HDC     ' CALL C,nnnn
                Zcall(Pw, GetC)
            Case &HDD     ' -     '     '     '     '     '  ---- ??? (old ix-commands)
        'Stop
            Case &HDE     ' SBC(A),nn     '  (nocash added, this opcode does existed, e.g. used by kwirk)
                Sbc(Pb)
            Case &HDF     ' RST(18H)
                Rst(24)
            Case &HE0     ' LD     '($FF00+nn),A ---- special (old ret(po))
                WriteM(65280 + Pb(), A)
            Case &HE1     ' POP(HL)
                Pop(L)
                Pop(H)
            Case &HE2     ' LD     '($FF00+C),A  ---- special (old jp(po),nnnn)
                WriteM(65280 + C, A)
            Case &HE3     ' -     '     '     '     '     '  ---- ??? (old ex (sp),hl)
        'Stop
            Case &HE4     ' -     '     '     '     '     '  ---- ??? (old call po,nnnn)
        'Stop
            Case &HE5     ' PUSH(HL)
                Push(H)
                Push(L)
            Case &HE6     ' AND  nn
                Zand(Pb)
            Case &HE7     ' RST(20H)
                Rst(32)
            Case &HE8     ' ADD(SP),dd     '     '  ---- special (old ret(pe)) (nocash extended as shortint)
                AddSP(Pb)
            Case &HE9 'JP(HL)
                Jp(H * 256 + L)
            Case &HEA     ' LD     '(nnnn),A     '  ---- special (old jp(pe),nnnn)
                WriteM(Pw, A)
            Case &HEB     ' -     '     '     '     '     '  ---- ??? (old ex de,hl)
        'Stop
            Case &HEC     ' -     '     '     '     '     '  ---- ??? (old call pe,nnnn)
        'Stop
            Case &HED     ' -     '     '     '     '     '  ---- ??? (old ed-commands)
        'Stop
            Case &HEE     ' XOR  nn
                Zxor(Pb)
            Case &HEF     ' RST(28H)
                Rst(40)
            Case &HF0     ' LD     'A,($FF00+nn) ---- special (old ret(p))
                A = ReadM(65280 + Pb())
            Case &HF1     ' POP(AF)
                Pop(F)
                Pop(A)
            Case &HF2     ' LD     'A,(C)     '     '  ---- special (old jp(p),nnnn)
                A = ReadM(65280 + C)
            Case &HF3 'DI
                ime_stat = 2
            Case &HF4     ' -     '     '     '     '     '  ---- ??? (old call p,nnnn)
    'Stop
            Case &HF5     ' PUSH(AF)
                Push(A)
                Push(F)
            Case &HF6     ' OR     'nn
                Zor(Pb)
            Case &HF7     ' RST(30H)
                Rst(48)
            Case &HF8     ' LD     'HL,SP+dd     '  ---- special (old ret(m)) (nocash corrected)
                MemPtr = Pb()
                If MemPtr > 127 Then MemPtr = MemPtr - 256
                PbMemVal = (SP + MemPtr) And 65535
                If MemPtr >= 0 Then
                    SetC(SP > PbMemVal)
                    SetH((SP Xor MemPtr Xor PbMemVal) And 4096 > 0)
                    H = PbMemVal \ 256
                    L = PbMemVal And 255
                Else
                    SetC(SP > PbMemVal)
                    SetH((SP Xor MemPtr Xor PbMemVal) And 4096 > 0)
                    H = PbMemVal \ 256
                    L = PbMemVal And 255
                End If
                SetZ(0)
                SetN(0)
            Case &HF9     ' LD     'SP,HL
                SP = H * 256 + L
            Case &HFA     ' LD     'A,(nnnn)     '  ---- special (old jp(m),nnnn)
                A = ReadM(Pw)
            Case &HFB 'EI
                ime_stat = 1
            Case &HFC     ' -     '     '     '     '     '  ---- ??? (old call m,nnnn)
        'Stop
            Case &HFD     ' -     '     '     '     '     '  ---- ??? (old iy-commands)
        'Stop
            Case &HFE     ' CP( )'nn
                Cp(Pb)
            Case &HFF     ' RST(38H)
                Rst(56)
        End Select
    End Sub


End Module
