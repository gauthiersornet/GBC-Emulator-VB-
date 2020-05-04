Option Explicit On

Module z80CommandesV2

    Private Temp_Var As Long, Temp_Var2 As Long

    Public Sub Rl(ByRef reg8 As Long) 'Rotate left thru carry
        Temp_Var = reg8 \ 128
        reg8 = ((reg8 * 2) Or GetC()) And 255
        SetZ(reg8 = 0)
        SetC(Temp_Var)
        SetH(0)
        SetN(0)
    End Sub

    Public Sub Rla() 'Rotate left thru carry register a
        Temp_Var = A \ 128
        A = ((A * 2) Or GetC()) And 255
        SetZ(A = 0)
        SetC(Temp_Var)
        SetH(0)
        SetN(0)
    End Sub

    Public Sub Rlc(ByRef reg8 As Long) 'rotate left
        SetC(reg8 And 128)
        reg8 = (reg8 * 2) And 255 Or GetC()
        SetZ(reg8 = 0)
        SetH(0)
        SetN(0)
    End Sub

    Public Sub Rlca() 'rotate left register A
        SetC(A And 128)
        A = (A * 2) And 255 Or GetC()
        SetZ(A = 0)
        SetH(0)
        SetN(0)
    End Sub

    Public Sub Rr(ByRef reg8 As Long) 'Rotate right thru carry
        Temp_Var = reg8 And 1
        reg8 = (reg8 \ 2) Or (128 * GetC())
        SetZ(reg8 = 0)
        SetC(Temp_Var)
        SetH(0)
        SetN(0)
    End Sub

    Public Sub Rra() 'Rotate right thru carry Register A
        Temp_Var = A And 1
        A = (A \ 2) Or (128 * GetC())
        SetZ(A = 0)
        SetC(Temp_Var)
        SetH(0)
        SetN(0)
    End Sub

    Public Sub Rrc(ByRef reg8 As Long) 'Rotate right
        SetC(reg8 And 1)
        reg8 = (reg8 \ 2) Or (128 * GetC())
        SetZ(reg8 = 0)
        SetH(0)
        SetN(0)
    End Sub

    Public Sub Rrca() 'Rotate right register A
        SetC(A And 1)
        A = (A \ 2) Or (128 * GetC())
        SetZ(A = 0)
        SetH(0)
        SetN(0)
    End Sub

    Public Sub Sla(ByRef reg8 As Long) 'Shift Left
        SetC(reg8 And 128)
        reg8 = (reg8 * 2) And 255
        SetZ(reg8 = 0)
        SetH(0)
        SetN(0)
    End Sub

    Public Sub Sra(ByRef reg8 As Long) 'Shift Right arithmetic
        SetC(reg8 And 1)
        reg8 = (reg8 \ 2) Or (reg8 And 128)
        SetZ(reg8 = 0)
        SetH(0)
        SetN(0)
    End Sub

    Public Sub Srl(ByRef reg8 As Long) 'Shift Right logical
        SetC(reg8 And 1)
        reg8 = reg8 \ 2
        SetZ(reg8 = 0)
        SetH(0)
        SetN(0)
    End Sub

    Public Sub Zsub(ByRef reg8 As Long) 'Substract from a
        Temp_Var = A - reg8
        Temp_Var2 = Temp_Var And 255
        SetZ(Temp_Var2 = 0)
        SetC(A < Temp_Var2)
        SetH((A And 15) < (Temp_Var2 And 15))
        SetN(1)
        A = Temp_Var2
    End Sub

    Public Sub Sbc(ByRef reg8 As Long) 'Substract from a - carry
        Temp_Var2 = GetC()
        Temp_Var = (A - reg8 - Temp_Var2) And 255
        SetZ(Temp_Var = 0)
        SetC(A < Temp_Var)
        SetH((A And &HF&) < (Temp_Var And 15))
        SetN(1)
        A = Temp_Var
    End Sub

    Public Sub Add(ByRef reg8 As Long) 'add to a
        Temp_Var = (A + reg8) And 255
        SetZ(Temp_Var = 0)
        SetC((A + reg8) And 256)
        SetH(((A And 15) + (reg8 And 15)) And 16)
        SetN(0)
        A = Temp_Var
    End Sub

    Public Sub Adc(ByRef reg8 As Long) 'add to a + carry
        Temp_Var = GetC()
        Temp_Var2 = (A + reg8 + Temp_Var) And 255
        SetZ(Temp_Var2 = 0)
        SetC((A + reg8 + Temp_Var) And 256)
        SetH(((A And 15) + (reg8 And 15) + Temp_Var) And 16)
        SetN(0)
        A = Temp_Var2
    End Sub

    Public Sub Push(ByRef reg8 As Long) 'Push to the stack
        SP = SP - 1
        WriteM(SP, reg8)
    End Sub

    Public Sub Pop(ByRef reg8 As Long) 'pop from the stack
        reg8 = ReadM(SP)
        SP = SP + 1
    End Sub

    Public Sub Zand(ByRef val As Long) 'Logocal and
        A = A And val
        SetZ(A = 0)
        SetN(0)
        SetC(0)
        SetH(1)
    End Sub

    Public Sub Zor(ByRef val As Long) 'Logocal or
        A = A Or val
        SetZ(A = 0)
        SetN(0)
        SetH(0)
        SetC(0)
    End Sub

    Public Sub Zxor(ByRef val As Long) 'Logocal xor
        A = A Xor val
        SetZ(A = 0)
        SetN(0)
        SetH(0)
        SetC(0)
    End Sub

    Public Sub Cp(ByRef val As Long) 'Compare with A
        SetC(A < val)
        SetH((A And 15) < (val And 15))
        SetZ(A = val)
        SetN(1)
    End Sub

    Public Sub Dec(ByRef reg8 As Long) 'decrease
        Temp_Var = reg8
        reg8 = reg8 - 1
        reg8 = reg8 And 255
        SetH((reg8 And 15) < (Temp_Var And 15))
        SetZ(reg8 = 0)
        SetN(1)
    End Sub

    Public Sub Dec16(ByRef reg81 As Long, ByRef reg82 As Long) 'same but for 16bit
        reg82 = reg82 - 1
        reg81 = reg81 + (reg82 < 0)
        reg81 = reg81 And 255
        reg82 = reg82 And 255
    End Sub

    Public Sub Inc(ByRef reg8 As Long) 'increase
        reg8 = reg8 + 1
        reg8 = reg8 And 255
        SetH((reg8 And 15) = 0)
        SetZ(reg8 = 0)
        SetN(0)
    End Sub

    Public Sub Inc16(ByRef reg81 As Long, ByRef reg82 As Long) 'same but for 16bit
        reg82 = reg82 + 1
        reg81 = reg81 + (reg82 \ 256)
        reg81 = reg81 And 255 : reg82 = reg82 And 255
    End Sub

    Public Sub AddHL(ByRef r1h As Long, ByRef r1l As Long) 'add to hl
        SetC(((H * 256 + L) + (r1h * 256 + r1l)) > 65535)
        SetH((((H * 256 + L) And 4095) + ((r1h * 256 + r1l) And 4095)) > 4095)
        L = L + r1l
        H = H + r1h
        If L > 255 Then H = H + 1
        L = L And 255 : H = H And 255
        SetN(0)
    End Sub

    Public Sub AddSP(ByVal Value As Long) 'add to sp(StackPointer)
        If Value > 127 Then Value = Value - 256
        Temp_Var = SP + Value
        Temp_Var = Temp_Var And 65535
        If Value > 0 Then
            SetC(SP > Temp_Var)
            SetH((SP Xor Value Xor Temp_Var) And 4096)
            SP = Temp_Var
        Else
            SetC(SP < Temp_Var)
            SetH((SP Xor Value Xor Temp_Var) And 4096)
            SP = Temp_Var
        End If
        SetZ(0)
        SetN(0)
    End Sub

    Public Sub Swap(ByRef reg8 As Long) 'Swap nibles
        reg8 = (reg8 \ 16) Or ((reg8 And 15) * 16)
        SetZ(reg8 = 0)
        SetN(0)
        SetH(0)
        SetC(0)
    End Sub

    Public Sub Daa() 'Demical adjust register A
        If GetN() Then
            If ((A And 15) >= 10 Or GetH()) Then A = A - 6
            If ((A And 240) >= 160 Or GetC()) Then A = A - 96 : SetC(1)
        Else
            If ((A And 15) >= 10 Or GetH()) Then A = A + 6
            If ((A And 240) >= 160 Or GetC()) Then A = A + 96 : SetC(1)
        End If
        A = A And 255
        SetZ(A = 0)
        SetH(0)
    End Sub

    Public Sub Cpl() 'logical not
        A = 255 - A
        SetH(1)
        SetN(1)
    End Sub

    Public Sub Halt() 'wait interupt
        If IME = False Then Exit Sub
        Temp_Var = RAM(65535, 0) And RAM(65295, 0)    ' AND IE, IF
        If Temp_Var = 0 Then PC = PC - 1 : Exit Sub                  'If no Interrupt occured exit
        'Process Interrput
        'Push pc
        SP = SP - 1
        WriteM(SP, PC \ 256)
        SP = SP - 1
        WriteM(SP, PC And 255)
        IME = False
        If (Temp_Var And 1) = 1 Then        'V-Blank ?
            PC = 64
            RAM(65295, 0) = RAM(65295, 0) And 254
        ElseIf (Temp_Var And 2) = 2 Then    'LCDC ?
            PC = 72
            RAM(65295, 0) = RAM(65295, 0) And 253
        ElseIf (Temp_Var And 4) = 4 Then    'Timer ?
            PC = 80
            RAM(65295, 0) = RAM(65295, 0) And 251
        ElseIf (Temp_Var And 8) = 8 Then    'Serial ?
            PC = 88
            RAM(65295, 0) = RAM(65295, 0) And 247
        ElseIf (Temp_Var And 16) = 16 Then  'Joypad ?
            PC = 96
            RAM(65295, 0) = RAM(65295, 0) And 239
        End If
    End Sub

    Public Sub Bit(ByRef reg8 As Long, B As Long) 'test bit
        SetZ((reg8 And B) = 0)
        SetN(0)
        SetH(1)
    End Sub

    Public Sub Zset(ByRef reg8 As Long, B As Long) ' set bit
        reg8 = reg8 Or B
    End Sub

    Public Sub Res(ByRef reg8 As Long, B As Long) 'reset bit
        reg8 = reg8 And B
    End Sub

    Public Sub Jp(Adr As Long, Optional cc As Byte = 1) 'jump to
        If cc Then PC = Adr
    End Sub

    Public Sub Jr(ByVal val As Long, Optional cc As Byte = 1) 'jump local to
        If val > 127 Then val = val - 256
        If cc Then PC = PC + val
    End Sub

    Public Sub Zcall(Adr As Long, Optional cc As Byte = 1) 'call subroutine
        If cc Then
            SP = SP - 1
            WriteM(SP, PC \ 256)
            SP = SP - 1
            WriteM(SP, PC And 255)
            PC = Adr
        End If
    End Sub

    Public Sub Rst(Value As Long) 'restart at
        SP = SP - 1
        WriteM(SP, PC \ 256)
        SP = SP - 1
        WriteM(SP, PC And 255)
        PC = Value
    End Sub

    Public Sub Ret(Optional cc As Byte = 1) 'return from subroutine
        If cc Then PC = ReadM(SP) + ReadM(SP + 1) * 256 : SP = SP + 2
    End Sub

    Public Sub Reti() 'return from subroutine ,enable interups
        PC = ReadM(SP) + ReadM(SP + 1) * 256
        SP = SP + 2
        IME = True
    End Sub


End Module
