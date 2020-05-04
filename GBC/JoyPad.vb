Option Explicit On

Module JoyPad

    'Public JRight As Keys = Keys.Right
    'Public JLeft As Keys = Keys.Left
    'Public JUp As Keys = Keys.Up
    'Public JDown As Keys = Keys.Down
    '
    'Public JA As Keys = Keys.A
    'Public JB As Keys = Keys.Z
    'Public JSelect As Keys = Keys.Return
    'Public JStart As Keys = Keys.Enter

    Public JRight As Byte = 39
    Public JLeft As Byte = 37
    Public JUp As Byte = 38
    Public JDown As Byte = 40

    Public JA As Byte = Keys.A
    Public JB As Byte = Keys.Z
    Public JSelect As Byte = 8
    Public JStart As Byte = 13

    'Public Sub DefautKeyJ()
    '    JRight = Keys.Right
    '    JLeft = Keys.Left
    '    JUp = Keys.Up
    '    JDown = Keys.Down
    '
    '    JA = Keys.A
    '    JB = Keys.Z
    '    JSelect = Keys.Return
    '    JStart = Keys.Enter
    '
    '    'Call ConfigTouches.UpDateCodeKey()
    'End Sub

    Public Sub UpDateJoyPad()
        'GetKeyState(KeyCode)>=0 ça veux dire que la touche qui correspond au KeyCode n'est pas appuyée :) ce qui est le cas au 3/4 du temps ;)
        'Mem.JoyVal1
        'Mem.JoyVal2
        'JoyVal1 c'est les touches directionelles Matrice des drapeaux:
        '11111111 Max val -> Byte 8 bits
        '00000001 Right => Droite 'Le seul bit montre le drapeau corespondant à la touche
        '00000010 Left => Gauche
        '00000100 Up => Haut
        '00001000 Down => Bas
        '
        'JoyVal2 c'est les touches Select, Start, A et B Matrice des drapeaux:
        '00000001 'Z - A Button
        '00000010 'X - B button
        '00000100 ' <Space> - Select
        '00001000 ' <Enter> - Start
        Dim JOld As Byte ' On sauvegarde l'état du JoyPad pour savoir si ont fait une intéruption comme quoi le JoyPad à changer
        JOld = Mem.JoyVal1 * 16 + Mem.JoyVal2

        If (GetKeyState(JRight) >= 0) Then Mem.JoyVal1 = Mem.JoyVal1 And 254 Else Mem.JoyVal1 = Mem.JoyVal1 Or 1
        If (GetKeyState(JLeft) >= 0) Then Mem.JoyVal1 = Mem.JoyVal1 And 253 Else Mem.JoyVal1 = Mem.JoyVal1 Or 2
        If (GetKeyState(JUp) >= 0) Then Mem.JoyVal1 = Mem.JoyVal1 And 251 Else Mem.JoyVal1 = Mem.JoyVal1 Or 4
        If (GetKeyState(JDown) >= 0) Then Mem.JoyVal1 = Mem.JoyVal1 And 247 Else Mem.JoyVal1 = Mem.JoyVal1 Or 8

        If (GetKeyState(JA) >= 0) Then Mem.JoyVal2 = Mem.JoyVal2 And 254 Else Mem.JoyVal2 = Mem.JoyVal2 Or 1
        If (GetKeyState(JB) >= 0) Then Mem.JoyVal2 = Mem.JoyVal2 And 253 Else Mem.JoyVal2 = Mem.JoyVal2 Or 2
        If (GetKeyState(JSelect) >= 0) Then Mem.JoyVal2 = Mem.JoyVal2 And 251 Else Mem.JoyVal2 = Mem.JoyVal2 Or 4
        If (GetKeyState(JStart) >= 0) Then Mem.JoyVal2 = Mem.JoyVal2 And 247 Else Mem.JoyVal2 = Mem.JoyVal2 Or 8

        If (JOld <> (Mem.JoyVal1 * 16 + Mem.JoyVal2)) Then Mem.RAM(65295, 0) = Mem.RAM(65295, 0) Or 16 'UpDate JoyPas Reg
    End Sub

End Module
