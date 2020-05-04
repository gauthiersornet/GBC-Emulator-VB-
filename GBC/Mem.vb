Option Explicit On
Imports System.Text

Module Mem

    Public Structure CartIinfo
        Public NGraph As Byte() '47
        Public Title As String
        Public TitleB As Byte() '15
        Public GBC As Byte
        Public LCode As Byte() '1
        Public IsSGB As Byte
        Public CCType As Byte
        Public RomSize As Byte
        Public RamSize As Byte
        Public DestCode As Byte
        Public LcodeOld As Byte
        Public MaskRomV As Byte
        Public CCheck As Byte
        Public Checksum As Byte() '1

        Public RomSizeT As String() '54
        Public RomSizeN As Byte() '54

        Public RamSizeT As String() '4
        Public RamSizeN As Byte() '4

        Public Sub CartIinfo()
            ReDim NGraph(47)
            ReDim TitleB(15)
            ReDim LCode(1)
            ReDim Checksum(1)
            ReDim RomSizeT(54)
            ReDim RomSizeN(54)
            ReDim RamSizeT(4)
            ReDim RamSizeN(4)
        End Sub

        'Public Sub New(i As Integer)
        'End Sub
    End Structure

    'Global variable
    Public RomInfo As CartIinfo

    Public ROM(,) As Byte '16383, 128
    Public RAM(65535, 7) As Byte

    Public BridgeRam(8191, 15) As Byte

    Public Objp(7, 3) As Integer, Bgp(7, 3) As Integer

    Public CurROMBank As Integer, CurRAMBank As Integer
    Public CartType(&HFF) As String 'Type de carte

    Public JoyVal1 As Byte, JoyVal2 As Byte 'ça c pas compliqué c'est les touches de la GameBoy

    Public HdmaS As Long, HdmaD As Long ' qqchose Source et qqchose Destination
    Public Hdma As Boolean, Hdmal As Long, tHdmal As Long, nwr As Boolean
    Public vRamB As Byte ' Mémoire virtuelle blank

    'Private variable
    Private RamBridgeSaveFileName As String
    Private Mbc3rtc As TimeGestion, Mbcrtc As Byte, MbcrtcE As Byte 'Le timing
    Private wRamB As Byte ' écrir Ram Blank
    Private Bgpi As Long '-> pal index
    Private bgai As Byte '-> A index
    Private I As Long, J As Long
    Private bgtmp As Long, bgtmp2 As Long, bgtmp3 As Long 'Variable Temporaire de travaille
    Private Objpi As Byte, Objai As Boolean  'OBJ palette index
    Private Mbc1mode As Byte 'MBC's Controll mode

    Public Sub InitMem()
        If (Mbc3rtc Is Nothing) Then Mbc3rtc = New TimeGestion
        RomInfo.CartIinfo()

        Dim I As Long, J As Byte
        For J = 0 To 7
            For I = 32768 To 65535
                RAM(I, J) = 0
            Next I
        Next J

        For J = 0 To 15
            For I = 0 To 8191
                BridgeRam(I, J) = 0
            Next I
        Next J

        For I = 0 To 7
            Bgp(I, 0) = 32767 : Objp(I, 0) = 32767
            Bgp(I, 1) = 32767 : Objp(I, 1) = 32767
            Bgp(I, 2) = 32767 : Objp(I, 2) = 32767
            Bgp(I, 3) = 32767 : Objp(I, 3) = 32767
        Next I

        CurROMBank = 1
        CartType(0) = "Rom Only" : CartType(&H12) = "Rom+MBC3+Ram"
        CartType(1) = "Rom+MBC1" : CartType(&H13) = "Rom+MBC3+Ram+Batt"
        CartType(2) = "Rom+MBC1+Ram" : CartType(&H19) = "Rom+MBC5"
        CartType(3) = "Rom+MBC1+Ram+Batt" : CartType(&H1A) = "Rom+MBC5+Ram"
        CartType(5) = "Rom+MBC2" : CartType(&H1B) = "Rom+MBC5+Ram+Batt"
        CartType(6) = "Rom+MBC2+Batt" : CartType(&H1C) = "Rom+MBC5+Rumble"
        CartType(8) = "Rom+Ram" : CartType(&H1D) = "Rom+MBC5+Rumble+Sram"
        CartType(9) = "Rom+Ram+Batt" : CartType(&H1E) = "Rom+MBC5+Rumble+Sram+Batt"
        CartType(&HB) = "Rom+MMO1" : CartType(&H1F) = "Pocket Camera"
        CartType(&HC) = "Rom+MMO1+Sram" : CartType(&HFD) = "Bandai TAMA5"
        CartType(&HD) = "Rom+MMO1+Sram+Batt" : CartType(&HFE) = "Hudson HuC-3"
        CartType(&HF) = "Rom+MBC3+Timer+Batt" : CartType(&HFF) = "Hudson HuC-1"
        CartType(&H10) = "Rom+MBC3+Timer+Ram+Batt"
        CartType(&H11) = "Rom+MBC3"
        RomInfo.CCType = ROM(&H147, 0)
        For I = &H134 To &H142
            RomInfo.TitleB(I - &H134) = ROM(I, 0)
        Next I

        If ((RomInfo.CCType = &HF) Or (RomInfo.CCType = &H10)) Then
            Mbcrtc = 1
        Else
            Mbcrtc = 0
        End If

        RomInfo.Title = Encoding.Default.GetString(RomInfo.TitleB) 'StrConv(RomInfo.TitleB, VbStrConv.None)
        RomInfo.Title.Replace("\0", "")
        RomInfo.RomSize = ROM(&H148, 0)
        RomInfo.RamSize = ROM(&H149, 0)

        RomInfo.RomSizeT(0) = "32 Kbyte" : RomInfo.RomSizeN(0) = 2
        RomInfo.RomSizeT(1) = "64 Kbyte" : RomInfo.RomSizeN(1) = 4
        RomInfo.RomSizeT(2) = "128 Kbyte" : RomInfo.RomSizeN(2) = 8
        RomInfo.RomSizeT(3) = "256 Kbyte" : RomInfo.RomSizeN(3) = 16
        RomInfo.RomSizeT(4) = "512 Kbyte" : RomInfo.RomSizeN(4) = 32
        RomInfo.RomSizeT(5) = "1 Mbyte" : RomInfo.RomSizeN(5) = 64
        RomInfo.RomSizeT(6) = "2 Mbyte" : RomInfo.RomSizeN(6) = 128
        RomInfo.RomSizeT(52) = "1.1 Mbyte" : RomInfo.RomSizeN(52) = 72
        RomInfo.RomSizeT(53) = "1.2 Mbyte" : RomInfo.RomSizeN(53) = 80
        RomInfo.RomSizeT(54) = "1.5 Mbyte" : RomInfo.RomSizeN(54) = 96

        RomInfo.RamSizeT(0) = "None" : RomInfo.RamSizeN(0) = 0
        RomInfo.RamSizeT(1) = "2 Kbyte" : RomInfo.RamSizeN(1) = 1
        RomInfo.RamSizeT(2) = "8 Kbyte" : RomInfo.RamSizeN(2) = 1
        RomInfo.RamSizeT(3) = "32 Kbyte" : RomInfo.RamSizeN(3) = 4
        RomInfo.RamSizeT(4) = "128 Kbyte" : RomInfo.RamSizeN(4) = 16
    End Sub

    Public Sub ReadRam() 'Li la Ram depuis un fichier de sauvegarde
        'Dim TempRam() As Byte
        'Dim FreeF As Integer
        'FreeF = FreeFile()
        'On Error GoTo SansSauvegarde
        'If Len(RamBridgeSaveFileName) > 0 Then Call WriteRam()
        'RamBridgeSaveFileName = Cmd.FileName & ".sav"
        'If Mbcrtc Then Mbc3rtc.Load RamBridgeSaveFileName 'c'est pour sauvegarder le timing
        'ReDim TempRam(CLng(RomInfo.RamSizeN(RomInfo.RamSize)) * 8192 - 1) As Byte
        '
        'Open RamBridgeSaveFileName For Binary As #FreeF
        '    Get #FreeF, , TempRam()
        'Close #FreeF
        'CopyMemory BridgeRam(0, 0), TempRam(0), UBound(TempRam) + 1 ' car ça commence à 0
        'SansSauvegarde:
        'Close #FreeF
    End Sub

    Public Sub WriteRam() 'écrit la Ram vers un fichier de sauvegarde
        'If Len(RamBridgeSaveFileName) <= 0 Then Exit Sub
        'Dim TempRam() As Byte
        'Dim FreeF As Integer
        'FreeF = FreeFile()
        'On Error GoTo Erreur
        'If Mbcrtc Then Mbc3rtc.Save RamBridgeSaveFileName 'c'est pour sauvegarder le timing
        'ReDim TempRam(CLng(RomInfo.RamSizeN(RomInfo.RamSize)) * 8192 - 1) As Byte
        '    CopyMemory TempRam(0), BridgeRam(0, 0), UBound(TempRam) + 1 ' car ça commence à 0
        '
        'Open RamBridgeSaveFileName For Binary As #FreeF
        '        Put #FreeF, , TempRam
        '    Close #FreeF
        'Erreur:
        'Close #FreeF
        'RamBridgeSaveFileName = ""
    End Sub

    Public Function ReadM(MemPtr As Long) As Byte
        If GameBoyMode = 0 Then
            If MemPtr < 16384 Then
                ReadM = ROM(MemPtr, 0)      ' Read from ROM
            ElseIf MemPtr < 32768 Then
                ReadM = ROM(MemPtr - 16384, CurROMBank)      ' Read from ROM
            Else
                If MemPtr > 40959 And MemPtr < 49152 Then
                    If MbcrtcE = 0 Then
                        ReadM = BridgeRam(MemPtr - 40960, CurRAMBank)    ' Read from sRAM
                    Else
                        ReadM = Mbc3rtc.ReadReg
                    End If
                Else
                    ReadM = RAM(MemPtr, 0)      ' Read from RAM
                End If
            End If
        Else
            If MemPtr < 16384 Then
                ReadM = ROM(MemPtr, 0)      ' Read from ROM
            ElseIf MemPtr < 32768 Then
                ReadM = ROM(MemPtr - 16384, CurROMBank)      ' Read from ROM
            ElseIf MemPtr < 40960 Then 'read Vram
                ReadM = RAM(MemPtr, vRamB)
            ElseIf MemPtr < 49152 Then 'read sRam
                If MbcrtcE = 0 Then
                    ReadM = BridgeRam(MemPtr - 40960, CurRAMBank)
                Else
                    ReadM = Mbc3rtc.ReadReg
                End If
            ElseIf MemPtr < 53248 Then 'read wRam(0)
                ReadM = RAM(MemPtr, 0)
            ElseIf MemPtr < 57344 Then 'read wRam(1-7)
                ReadM = RAM(MemPtr, wRamB)
            Else 'read ram
                ReadM = RAM(MemPtr, 0)      ' Read from RAM
            End If
        End If
    End Function

    Public Sub WriteM(MemPtr As Long, ByVal Value As Long)
        If MemPtr > 32767 Then 'ram/mmio
            'ram
            If GameBoyMode = 0 Then 'Old gameboy
                If MemPtr > 40959 And MemPtr < 49152 Then    ' write to sRAM
                    If MbcrtcE = 0 Then
                        BridgeRam(MemPtr - 40960, CurRAMBank) = Value
                        Exit Sub
                    Else
                        Mbc3rtc.WriteReg(Value)
                    End If
                Else
                    RAM(MemPtr, 0) = Value    ' write to RAM
                    If MemPtr > &HE000 And MemPtr < &HFE00 Then ' echo
                        RAM(MemPtr - 8192, 0) = Value
                    ElseIf MemPtr > &HC000 And MemPtr < &HDE00 Then ' echo
                        RAM(MemPtr + 8192, 0) = Value
                    End If
                End If
            Else
                Select Case MemPtr 'GameBoy collor
                    Case Is < 40960 'write Vram
                        RAM(MemPtr, vRamB) = Value
                        Exit Sub
                    Case Is < 49152 'write sRam
                        If MbcrtcE = 0 Then
                            BridgeRam(MemPtr - 40960, CurRAMBank) = Value
                        Else
                            Mbc3rtc.WriteReg(Value)
                        End If
                        Exit Sub
                    Case Is < 53248 'write wRam(0)
                        RAM(MemPtr, 0) = Value
                        Exit Sub
                    Case Is < 57344 'write wRam(1-7)
                        RAM(MemPtr, wRamB) = Value
                        Exit Sub
                    Case Else 'write ram
                        RAM(MemPtr, 0) = Value
                End Select
            End If

            'Memory Maped Registers
            If MemPtr > 65279 Then
                Select Case MemPtr
                    Case Is = 65280     ' Joypad
                        If (Value And 32) = 32 Then         'Directional
                            RAM(65280, 0) = 223 And (255 - JoyVal1)
                        ElseIf (Value And 16) = 16 Then     ' Buttons
                            RAM(65280, 0) = 239 And (255 - JoyVal2)
                        Else
                            RAM(65280, 0) = 255
                        End If
                    Case Is = 65350     ' DMA Xfer
                        RAM(65350, 0) = Value
                        J = Value * 256
                        For I = 65024 To 65183
                            RAM(I, 0) = ReadM(J)
                            J = J + 1
                        Next I
                    Case 65351, 65352, 65353 'Old gameboy palets
                        CColid2(Value, MemPtr - 65351)
                    Case 65287 'Timer
                        Select Case Value And 3
                            Case 0
                                z80.Tvm = 1024
                            Case 1
                                z80.Tvm = 65536
                            Case 2
                                z80.Tvm = 16384
                            Case 3
                                z80.Tvm = 4096
                        End Select

                'Sound regs
                    Case 65296 ' NR10
                        If Tes Then setNR10(Value)
                    Case 65297 ' NR11
                        If Tes Then setNR11(Value)
                    Case 65298 ' NR12
                        If Tes Then setNR12(Value)
                    Case 65299 ' NR13
                        If Tes Then setNR13(Value)
                    Case 65300 ' NR14
                        If Tes Then setNR14(Value)
                    Case 65302 ' NR21
                        If Tes Then setNR21(Value)
                    Case 65303 ' NR22
                        If Tes Then setNR22(Value)
                    Case 65304 ' NR23
                        If Tes Then setNR23(Value)
                    Case 65305 ' NR24
                        If Tes Then setNR24(Value)
                    Case 65306 ' NR30
                        If Tes Then setNR30(Value)
                    Case 65307 ' NR31
                        If Tes Then setNR31(Value)
                    Case 65308 ' NR32
                        If Tes Then setNR32(Value)
                    Case 65309 ' NR33
                        If Tes Then setNR33(Value)
                    Case 65310 ' NR34
                        If Tes Then setNR34(Value)
                    Case 65312 ' NR41
                        If Tes Then setNR41(Value)
                    Case 65313 ' NR42
                        If Tes Then setNR42(Value)
                    Case 65314 ' NR43
                        If Tes Then setNR43(Value)
                    Case 65315 ' NR44
                        If Tes Then setNR44(Value)
                    Case 65316 ' NR50
                        If Tes Then setNR50(Value)
                    Case 65317 ' NR51
                        If Tes Then setNR51(Value)
                    Case 65318 ' NR52
                        If Tes Then setNR52(Value)
                End Select
                'Gameboy Color Olny
                If GameBoyMode = 1 Then
                    Select Case MemPtr
                        Case 65357  'Speed SW
                            Smp = Value And 1
                            RAM(65357, 0) = CpuS * 128 + Smp
                        Case 65359  'Vram Bank
                            vRamB = Value And 1
                            RAM(65359, 0) = vRamB
                        Case 65361  'HDMA1 sh
                            HdmaS = (HdmaS And 255) + Value * 256
                        Case 65362 'HDMA2 sl
                            HdmaS = (HdmaS And 65280) + Value
                        Case 65363  'HDMA3 dh
                            HdmaD = (HdmaD And 255) + Value * 256
                        Case 65364 'HDMA4 dl
                            HdmaD = (HdmaD And 65280) + Value
                        Case 65365 'HDMA5 lms
                            HdmaD = (HdmaD And 8176) + 32768
                            HdmaS = HdmaS And 65520
                            'If nwr Then
                            If Hdma Then If (Value And 128) = 0 Then Hdma = False : RAM(65365, 0) = 128 + 70 : Exit Sub Else Exit Sub
                            If Value And 128 Then Hdma = True : Hdmal = Value And 127 : tHdmal = Value And 127 : RAM(65365, 0) = Hdmal : Exit Sub
                            'End If
                            J = HdmaD
                            For I = HdmaS To HdmaS + (Value And 127) * 16 + 15
                                RAM(J, vRamB) = ReadM(I)
                                J = J + 1
                            Next I
                            RAM(65365, 0) = 255
                        Case 65366  'Rp
                'InfraRed

                        Case 65384  'BG pal indx
                            Bgpi = Value And 63
                            bgai = Value And 128
                            If Bgpi Mod 2 Then RAM(65385, 0) = Bgp(Bgpi \ 8, (Bgpi \ 2) Mod 4) \ 256 Else RAM(65385, 0) = Bgp(Bgpi \ 8, (Bgpi \ 2) Mod 4) And 255

                        Case 65385 'BG Pal Val
                            I = Bgpi Mod 2
                            bgtmp = Bgpi \ 8
                            bgtmp2 = (Bgpi \ 2) Mod 4
                            If I = 0 Then ' 1st byte
                                Bgp(bgtmp, bgtmp2) = (Bgp(bgtmp, bgtmp2) And 65280) + Value
                                bgtmp3 = Bgp(bgtmp, bgtmp2)
                                bgpCC(bgtmp, bgtmp2) = IntToRGB(bgtmp3) '(bgtmp3 And 31744) \ 1024 + (bgtmp3 And 992) + (bgtmp3 And 31) * 1024
                            Else '2nd byte
                                Bgp(bgtmp, bgtmp2) = ((Bgp(bgtmp, bgtmp2) And 255) + Value * 256) And 32767
                                bgtmp3 = Bgp(bgtmp, bgtmp2)
                                bgpCC(bgtmp, bgtmp2) = IntToRGB(bgtmp3) '(bgtmp3 And 31744) \ 1024 + (bgtmp3 And 992) + (bgtmp3 And 31) * 1024
                            End If
                            If bgai Then Bgpi = Bgpi + 1
                            WriteM(65384, (RAM(65384, 0) And 128) Or (Bgpi And 63))

                        Case 65386  'OBJ pal indx
                            Objpi = Value And 63
                            Objai = Value And 128
                            If Objpi Mod 2 Then RAM(65387, 0) = Objp(Objpi \ 8, (Objpi \ 2) Mod 4) \ 256 Else RAM(65387, 0) = Objp(Objpi \ 8, (Objpi \ 2) Mod 4) And 255

                        Case 65387 'OBJ Pal Val

                            I = Objpi Mod 2
                            bgtmp = Objpi \ 8
                            bgtmp2 = (Objpi \ 2) Mod 4
                            If I = 0 Then ' 1st byte
                                Objp(bgtmp, bgtmp2) = (Objp(bgtmp, bgtmp2) And 65280) + Value
                                bgtmp3 = Objp(bgtmp, bgtmp2)
                                objpCC(bgtmp, bgtmp2) = IntToRGB(bgtmp3) '(bgtmp3 And 31744) \ 1024 + (bgtmp3 And 992) + (bgtmp3 And 31) * 1024
                            Else '2nd byte
                                Objp(bgtmp, bgtmp2) = ((Objp(bgtmp, bgtmp2) And 255) + Value * 256) And 32767
                                bgtmp3 = Objp(bgtmp, bgtmp2)
                                objpCC(bgtmp, bgtmp2) = IntToRGB(bgtmp3) '(bgtmp3 And 31744) \ 1024 + (bgtmp3 And 992) + (bgtmp3 And 31) * 1024
                            End If


                            If Objai Then Objpi = Objpi + 1
                            WriteM(65386, (RAM(65386, 0) And 128) Or (Objpi And 63))

                        Case 65392  'SVBK
                            wRamB = Value And 7
                            If wRamB < 1 Then wRamB = 1
                            RAM(65392, 0) = wRamB
                    End Select
                End If

            End If
        Else 'MBC's Controll
            Select Case RomInfo.CCType

                Case 1, 2, 3 ' mbc1
                    If ((MemPtr > &H1FFF) And (MemPtr < &H4000)) Then 'std rom banks
                        Value = Value And 31 'XXXBBBBB->00011111->31
                        If (Value = 0) Then Value = 1
                        CurROMBank = Value
                    ElseIf ((MemPtr > &H3FFF) And (MemPtr < &H6000)) Then 'ram/extended rom
                        Value = Value And 2
                        If (Mbc1mode = 1) Then 'ram
                            CurRAMBank = Value
                        Else 'rom
                            CurROMBank = Value * 32 + (CurROMBank And 31) 'value << 5 + last 5 from crb
                        End If
                    ElseIf ((MemPtr > &H5FFF) And (MemPtr < &H8000)) Then 'Model selection
                        Mbc1mode = Value And 1
                    End If
                Case &HF, &H10, &H11, &H12, &H13 'mbc3
                    If ((MemPtr > &H1FFF) And (MemPtr < &H4000)) Then 'rom
                        Value = Value And 127 'olny 7 lower
                        If (Value = 0) Then Value = 1
                        CurROMBank = Value
                    ElseIf ((MemPtr > &H3FFF) And (MemPtr < &H6000)) Then 'ram
                        If (Value < RomInfo.RamSize) Then
                            CurRAMBank = Value : MbcrtcE = 0
                        Else
                            If Mbcrtc = 1 Then Mbc3rtc.Act = Value : MbcrtcE = 1
                        End If
                    End If

            'Case &H19, &H1A, &H1B, &HC, &H1D, &H1E 'mbc5
            '    If ((MemPtr > &H1FFF) And (MemPtr < &H3000)) Then
            'If (Value = 286) Then
            'DoEvents
            'End If
            '        CurROMBank = Value + (CurROMBank And 256)
            '    ElseIf (MemPtr < &H4000) Then
            '        Value = Value And 1
            'If (((CurROMBank And 255) + Value * 256) = 286) Then
            'DoEvents
            'End If
            '        CurROMBank = (CurROMBank And 255) + Value * 256
            '        If (CurROMBank > 10) Then
            '        DoEvents
            '        End If
            '    ElseIf (MemPtr < &H6000) Then
            '        CurRAMBank = Value And 16
            '    End If

                Case &H19, &H1A, &H1B, &HC, &H1D, &H1E 'mbc5
                    If MemPtr > &H1FFF And MemPtr < &H3000 Then
                        CurROMBank = Value + (CurROMBank And 256)
                    ElseIf MemPtr < &H4000 Then
                        Value = Value And 1
                        CurROMBank = (CurROMBank And 255) + Value * 256
                    ElseIf MemPtr < &H6000 Then
                        CurRAMBank = Value And 16
                    End If

            End Select
        End If
    End Sub

    Public Function Pb() As Byte 'read a byte at pc ,increase pc
        'If (PC = 17385) Then
        '    DoEvents
        'End If
        If GameBoyMode = 0 Then
            Select Case PC
                Case Is < 16384
                    Pb = ROM(PC, 0)      ' Read from ROM
                Case Is < 32768
                    Pb = ROM(PC - 16384, CurROMBank)      ' Read from ROM
                Case Else
                    If PC > 40959 And PC < 49152 Then
                        Pb = BridgeRam(PC - 40960, CurRAMBank)    ' Read from sRAM
                    Else
                        Pb = RAM(PC, 0)      ' Read from RAM
                    End If
            End Select
        Else
            Select Case PC
                Case Is < 16384
                    Pb = ROM(PC, 0)      ' Read from ROM
                Case Is < 32768
                    Pb = ROM(PC - 16384, CurROMBank)      ' Read from ROM
                Case Is < 40960 'read Vram
                    Pb = RAM(PC, vRamB)
                Case Is < 49152 'read sRam
                    Pb = BridgeRam(PC - 40960, CurRAMBank)
                Case Is < 53248 'read wRam(0)
                    Pb = RAM(PC, 0)
                Case Is < 57344 'read wRam(1-7)
                    Pb = RAM(PC, wRamB)
                Case Else 'read ram
                    Pb = RAM(PC, 0) ' Read from RAM
            End Select
        End If
        PC = PC + 1
    End Function

    Public Function Pw() As Long 'read a word at pc ,increase pc
        If GameBoyMode = 0 Then
            Select Case PC
                Case Is < 16384
                    Pw = ROM(PC, 0)      ' Read from ROM
                Case Is < 32768
                    Pw = ROM(PC - 16384, CurROMBank)      ' Read from ROM
                Case Else
                    If PC > 40959 And PC < 49152 Then
                        Pw = BridgeRam(PC - 40960, CurRAMBank)    ' Read from sRAM
                    Else
                        Pw = RAM(PC, 0)      ' Read from RAM
                    End If
            End Select
            PC = PC + 1
            Select Case PC
                Case Is < 16384
                    Pw = Pw + ROM(PC, 0) * 256  ' Read from ROM
                Case Is < 32768
                    Pw = Pw + ROM(PC - 16384, CurROMBank) * 256  ' Read from ROM
                Case Else
                    If PC > 40959 And PC < 49152 Then
                        Pw = Pw + BridgeRam(PC - 40960, CurRAMBank) * 256 ' Read from sRAM
                    Else
                        Pw = Pw + RAM(PC, 0) * 256  ' Read from RAM
                    End If
            End Select
            PC = PC + 1
        Else
            Select Case PC
                Case Is < 16384
                    Pw = ROM(PC, 0)      ' Read from ROM
                Case Is < 32768
                    Pw = ROM(PC - 16384, CurROMBank)      ' Read from ROM
                Case Is < 40960 'read Vram
                    Pw = RAM(PC, vRamB)
                Case Is < 49152 'read sRam
                    Pw = BridgeRam(PC - 40960, CurRAMBank)
                Case Is < 53248 'read wRam(0)
                    Pw = RAM(PC, 0)
                Case Is < 57344 'read wRam(1-7)
                    Pw = RAM(PC, wRamB)
                Case Else 'read ram
                    Pw = RAM(PC, 0)      ' Read from RAM
            End Select
            PC = PC + 1
            Select Case PC
                Case Is < 16384
                    Pw = Pw + CLng(ROM(PC, 0)) * 256  ' Read from ROM
                Case Is < 32768
                    Pw = Pw + CLng(ROM(PC - 16384, CurROMBank)) * 256  ' Read from ROM
                Case Is < 40960 'read Vram
                    Pw = Pw + RAM(PC, vRamB) * 256
                Case Is < 49152 'read sRam
                    Pw = Pw + CLng(BridgeRam(PC - 40960, CurRAMBank)) * 256
                Case Is < 53248 'read wRam(0)
                    Pw = Pw + CLng(RAM(PC, 0)) * 256
                Case Is < 57344 'read wRam(1-7)
                    Pw = Pw + CLng(RAM(PC, wRamB)) * 256
                Case Else 'read ram
                    Pw = Pw + CLng(RAM(PC, 0)) * 256  ' Read from RAM
            End Select
            PC = PC + 1
        End If
    End Function





End Module
