Option Explicit On
Imports System.Drawing.Imaging
Imports System.Runtime.InteropServices

Module Graph

    Public Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (pDst As IntPtr, pSrc As IntPtr, ByVal ByteLen As Long)
    Public Declare Function VarPtrArray Lib "msvbvm50.dll" Alias "VarPtr" (Ptr() As IntPtr) As Long
    Private Declare Function GetObjectAPI Lib "gdi32" Alias "GetObjectA" (ByVal hObject As Long, ByVal nCount As Long, lpObject As IntPtr) As Long

    Public Structure SAFEARRAYBOUND
        Public cElements As Long
        Public lLbound As Long
    End Structure

    Public Structure SAFEARRAY2D
        Public cDims As Integer
        Public fFeatures As Integer
        Public cbElements As Long
        Public cLocks As Long
        Public pvData As Long
        Public Bounds As SAFEARRAYBOUND()
        Public Sub SAFEARRAY2D()
            ReDim Bounds(1)
        End Sub
    End Structure

    'Public Structure BITMAP
    '    Public bmType As Long
    '    Public bmWidth As Long
    '    Public bmHeight As Long
    '    Public bmWidthBytes As Long
    '    Public bmPlanes As Integer
    '    Public bmBitsPixel As Integer
    '    Public bmBits As Long
    'End Structure

    Public Structure RGBByte
        Public B As Byte
        Public G As Byte
        Public R As Byte
    End Structure

    'Public Viewport() As RGBByte

    'Variable Globale
    Public bgpCC(7, 3) As RGBByte, objpCC(7, 3) As RGBByte 'C'est en rapport avec la palette de couleur
    Public SkipFrame As Boolean = False
    Public Fmode As Byte = 0 'Mode de saut d'image
    Public Fskip As Byte = 1 'Saut d'image
    Public Skipf As Boolean = False 'Activation du saut d'image si il doit en ignorer
    Public BgV As Boolean = True 'le BackGround est-il visible ?
    Public WinV As Boolean = True 'la fenêtre est-elle visible ?
    Public Objv As Boolean = True 'les objets sont-ils visibles ?

    'Variable Public
    Public Const MAX_WIDTH As Byte = 160
    Public Const MAX_HEIGHT As Byte = 144
    Public Const MAX_AREA As Integer = 23040
    Public Const MWIDTH As Byte = MAX_WIDTH - 1
    Public Const MHEIGHT As Byte = MAX_HEIGHT - 1

    Public sa As SAFEARRAY2D
    Public bmp As Bitmap
    Public CurFreq As Long, CurStart As Long, CurEnd As Long 'ça permet de limiter la vitesse
    'Public FpsT As Long
    Public FPS As Long

    'Variable Private
    Private btMap As Bitmap
    Private Vram(160, 144) As RGBByte 'C'est la mémoire de l'écran
    Private Colid2(2, 128, 128) As RGBByte ' Image ou sprite
    Private Colid(128, 128) As RGBByte ' Image ou sprite
    Private ColiDx(128, 128) As Byte ' Image ou sprite
    Private TCol As Byte 'échantillonage de couleur "un test"
    Private dblResult As Single
    Private Mir(255) As Byte, Mv1 As Byte, Mv2 As Byte
    Private LastLine As Byte 'Pour dessiner -> Pointeur de la dernière ligne utilisée ou modifiée
    Private CurLine As Integer, CurLineFlip As Integer 'Pour dessiner -> Pointeur de la dernière ligne utilisée ou modifiée
    Private Tcls(19) As RGBByte
    Private Xoffset As Integer, Yoffset As Integer, TileData As Long, TileEnd As Long, TileMap As Long, TileTmp As Long, TilePtr As Long ' Les titles ces des petites images mit bou à bou
    Private SpriteX As Integer, SpriteY As Integer
    Private Tms As Long
    Private X As Integer, Y As Integer, xs As Integer, ys As Integer 'dans la fonction DrawLine et DrawLineOlgGameBoy
    Private Bgat As Byte, Ccp As Byte, Vrm As Byte, Xflip As Byte, Yflip As Byte, Ccid(128, 128) As RGBByte
    Private Cid2 As RGBByte, Spat As Byte 'heu Spit Spat Spat = Cracher lol ^^ ' C'est pour la OldGameBoy ça
    Private MemPtr As Long
    Private Vf As Boolean
    Private I As Byte, j As Byte, K As Integer, L As Integer 'Pour le Flip de la VRam->Viewport
    Private TimeFps As Long, TimeCalcul As Long
    Private CoefWait As Single = 16.6 'Régulation automatisée ;)

    Sub PictArrayInit2D(xPicture As Image)
        CoefWait = 16.6
        Call PictArrayKill()

        'xPicture.Picture = LoadPicture(App.Path & "\Blank.bmp")
        'Recherche des infos sur l'image :
        'GetObjectAPI xPicture.Picture, Len(bmp), bmp 'dest
        'On sort si ce n'est pas une image supportée :
        'If ((bmp.bmPlanes <> 1) Or (bmp.bmBitsPixel <> 24) Or (bmp.bmWidth <> 160) Or (bmp.bmHeight <> 144)) Then
        '    Call MsgBox("Blank.bmp -> 24-Bit,160*144, single bitplane bitmaps Only!", vbCritical, "Error!")
        '    End
        'End If
        '
        'With sa
        '    .cbElements = 3
        '    .cDims = 2
        '    .Bounds(0).lLbound = 0
        '    .Bounds(0).cElements = bmp.bmHeight
        '    .Bounds(1).lLbound = 0
        '    .Bounds(1).cElements = bmp.bmWidth
        '    .pvData = bmp.bmBits
        'End With

        'Copie des données du bitmap dans un tableau en byte :
        'CopyMemory ByVal VarPtrArray(Vram), VarPtr(sa), 4
        'Call xPicture.Refresh
        'DoEvents
    End Sub

    Public Sub PictArrayKill()
        'CopyMemory ByVal VarPtrArray(Vram), 0&, 4
    End Sub

    Public Sub ClearViewport()
        Dim I As Integer, j As Integer
        For j = 0 To MHEIGHT
            For I = 0 To MWIDTH
                'Vram(I, j).R = 0
                'Vram(I, j).G = 0
                'Vram(I, j).B = 0
            Next I
        Next j
    End Sub

    'Colid2 => Color id 2 => Couleur indice version 2
    Public Sub CColid2(ByVal Col As Byte, Target As Long)
        Dim tm1 As Long, tm2 As Long
        Colid2(Target, 0, 0) = Colid(Col And 2, Col And 1)
        For tm2 = 1 To 128 Step 0
            Colid2(Target, 0, tm2) = Colid(Col And 8, Col And 4)
            tm2 = tm2 * 2
        Next tm2
        For tm1 = 1 To 128 Step 0
            Colid2(Target, tm1, 0) = Colid(Col And 32, Col And 16)
            tm1 = tm1 * 2
        Next tm1
        For tm1 = 1 To 128 Step 0
            For tm2 = 1 To 128 Step 0
                Colid2(Target, tm1, tm2) = Colid(Col And 128, Col And 64)
                tm2 = tm2 * 2
            Next tm2
            tm1 = tm1 * 2
        Next tm1
    End Sub

    Function RGB24(Red As Byte, Green As Byte, Blue As Byte) As RGBByte
        RGB24.R = Red
        RGB24.G = Green
        RGB24.B = Blue
    End Function

    Public Sub DrawLine() 'Using Vram()
        CurLine = RAM(65348, 0)
        If CurLine = LastLine Then Exit Sub
        LastLine = CurLine
        CurLineFlip = MHEIGHT - CurLine
        ' Draw Background
        ' Get BG & window Tile Pattern Data Address
        If RAM(65344, 0) And 16 Then
            TileData = 32768
        Else
            TileData = 34816
        End If
        If BgV Then
            ' Get BG Tile Table Address
            If RAM(65344, 0) And 8 Then
                TileMap = 39936
                Tms = 39936
            Else
                TileMap = 38912
                Tms = 38912
            End If
            TileEnd = TileMap + 1023
            Xoffset = RAM(65347, 0)
            Yoffset = RAM(65346, 0) + CurLine
            xs = Xoffset \ 8
            ys = Yoffset \ 8
            Yoffset = Yoffset And 7
            Xoffset = -(Xoffset And 7)

            For X = Xoffset To 159 Step 8
                TileTmp = TileMap + ys * 32 + xs
                If TileTmp > TileEnd Then TileTmp = TileTmp - 1024
                If TileData = 32768 Then           ' Tile Data @ &H8800-&h97FF is 128ed
                    TilePtr = RAM(TileTmp, 0) * 16             'Get pointer to tile
                Else
                    TilePtr = (RAM(TileTmp, 0) Xor 128) * 16
                End If
                Bgat = RAM(TileTmp, 1)
                Ccp = Bgat And 7
                Tcls(X \ 8) = bgpCC(Ccp, 0)
                Vrm = (Bgat And 8) \ 8
                Xflip = (Bgat And 32) \ 32 : Yflip = (Bgat And 64) \ 64
                Ccid(0, 0) = bgpCC(Ccp, 0)
                Ccid(0, 1) = bgpCC(Ccp, 1)
                Ccid(1, 0) = bgpCC(Ccp, 2)
                Ccid(1, 1) = bgpCC(Ccp, 3)
                If Yflip Then MemPtr = TileData + TilePtr + 14 - (Yoffset And 7) * 2 Else MemPtr = TileData + TilePtr + (Yoffset And 7) * 2
                If Xflip Then Mv1 = Mir(RAM(MemPtr + 1, Vrm)) : Mv2 = Mir(RAM(MemPtr, Vrm)) Else Mv1 = RAM(MemPtr + 1, Vrm) : Mv2 = RAM(MemPtr, Vrm)
                xs = (xs + 1) Mod 32

                If X > -1 And X < 153 Then
                    Vram(X + 7, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 6, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 5, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 4, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 3, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 2, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 1, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                Else
                    If X < 153 Then Vram(X + 7, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -7 And X < 154 Then Vram(X + 6, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -6 And X < 155 Then Vram(X + 5, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -5 And X < 156 Then Vram(X + 4, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -4 And X < 157 Then Vram(X + 3, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -3 And X < 158 Then Vram(X + 2, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -2 And X < 159 Then Vram(X + 1, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -1 And X < 160 Then Vram(X, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                End If

            Next X
        End If


        'Draw Window

        If ((RAM(65344, 0) And 32) = 32) And WinV And (CurLine >= RAM(65354, 0)) And (RAM(65355, 0) < 167) Then
            ' Get window Tile Table Address
            If RAM(65344, 0) And 64 Then
                TileMap = 39936
            Else
                TileMap = 38912
            End If
            Yoffset = CurLine - RAM(65354, 0)
            TileMap = TileMap + (Yoffset \ 8) * 32
            Yoffset = Yoffset And 7
            For X = RAM(65355, 0) - 7 To 159 Step 8
                If TileData = 32768 Then           ' Tile Data @ &H8800-&h97FF is 128ed
                    TilePtr = RAM(TileMap, 0) * 16             'Get pointer to tile
                Else
                    TilePtr = (RAM(TileMap, 0) Xor 128) * 16
                End If
                Bgat = RAM(TileMap, 1)
                Ccp = Bgat And 7
                Vrm = (Bgat And 8) \ 8
                Xflip = (Bgat And 32) \ 32 : Yflip = (Bgat And 64) \ 64
                Ccid(0, 0) = bgpCC(Ccp, 0)
                Ccid(0, 1) = bgpCC(Ccp, 1)
                Ccid(1, 0) = bgpCC(Ccp, 2)
                Ccid(1, 1) = bgpCC(Ccp, 3)
                If Yflip Then MemPtr = TileData + TilePtr + 14 - Yoffset * 2 Else MemPtr = TileData + TilePtr + Yoffset * 2
                If Xflip Then Mv1 = Mir(RAM(MemPtr + 1, Vrm)) : Mv2 = Mir(RAM(MemPtr, Vrm)) Else Mv1 = RAM(MemPtr + 1, Vrm) : Mv2 = RAM(MemPtr, Vrm)

                If X > -1 And X < 153 Then
                    Vram(X + 7, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 6, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 5, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 4, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 3, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 2, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 1, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                Else
                    If X < 153 Then Vram(X + 7, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -7 And X < 154 Then Vram(X + 6, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -6 And X < 155 Then Vram(X + 5, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -5 And X < 156 Then Vram(X + 4, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -4 And X < 157 Then Vram(X + 3, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -3 And X < 158 Then Vram(X + 2, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -2 And X < 159 Then Vram(X + 1, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -1 And X < 160 Then Vram(X, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                End If



                TileMap = TileMap + 1
            Next X
        End If

        'Draw Sprites
        If (RAM(65344, 0) And 2) And Objv Then
            If (RAM(65344, 0) And 4) = 0 Then
                SpriteY = 7
            Else
                SpriteY = 15
            End If
            For TileMap = 65180 To 65024 Step -4
                Y = RAM(TileMap, 0) - 16
                X = RAM(TileMap + 1, 0) - 8
                If Y <= CurLine And Y + SpriteY >= CurLine And X > -8 And X < 160 Then

                    If SpriteY = 7 Then TilePtr = ReadM(TileMap + 2) * 16 Else TilePtr = (RAM(TileMap + 2, 0) And 254) * 16 'Get pointer to tile
                    TCol = -((RAM(TileMap + 3, 0) And 128) > 0)
                    Vf = RAM(TileMap + 3, 0) And 32
                    Vrm = (RAM(TileMap + 3, 0) And 8) \ 8
                    Ccp = RAM(TileMap + 3, 0) And 7
                    'init palete
                    Ccid(0, 0) = objpCC(Ccp, 0)
                    Ccid(0, 1) = objpCC(Ccp, 1)
                    Ccid(1, 0) = objpCC(Ccp, 2)
                    Ccid(1, 1) = objpCC(Ccp, 3)
                    MemPtr = 32768 + TilePtr

                    If (RAM(TileMap + 3, 0) And 64) Then MemPtr = MemPtr + SpriteY * 2 - (CurLine - Y) * 2 Else MemPtr = MemPtr + (CurLine - Y) * 2
                    If Vf Then Mv1 = Mir(RAM(MemPtr + 1, Vrm)) : Mv2 = Mir(RAM(MemPtr, Vrm)) Else Mv1 = RAM(MemPtr + 1, Vrm) : Mv2 = RAM(MemPtr, Vrm)

                    If TCol = 0 Then
                        If X < 153 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 7, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -7 And X < 154 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 6, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -6 And X < 155 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 5, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -5 And X < 156 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 4, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -4 And X < 157 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 3, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -3 And X < 158 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 2, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -2 And X < 159 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 1, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -1 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)

                    Else
                        If X < 153 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then If Cmp2RGBByte(Vram(X + 7, CurLineFlip), Tcls((X + 7) \ 8)) Then Vram(X + 7, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -7 And X < 154 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then If Cmp2RGBByte(Vram(X + 6, CurLineFlip), Tcls((X + 6) \ 8)) Then Vram(X + 6, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -6 And X < 155 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then If Cmp2RGBByte(Vram(X + 5, CurLineFlip), Tcls((X + 5) \ 8)) Then Vram(X + 5, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -5 And X < 156 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then If Cmp2RGBByte(Vram(X + 4, CurLineFlip), Tcls((X + 4) \ 8)) Then Vram(X + 4, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -4 And X < 157 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then If Cmp2RGBByte(Vram(X + 3, CurLineFlip), Tcls((X + 3) \ 8)) Then Vram(X + 3, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -3 And X < 158 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then If Cmp2RGBByte(Vram(X + 2, CurLineFlip), Tcls((X + 2) \ 8)) Then Vram(X + 2, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -2 And X < 159 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then If Cmp2RGBByte(Vram(X + 1, CurLineFlip), Tcls((X + 1) \ 8)) Then Vram(X + 1, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -1 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then If Cmp2RGBByte(Vram(X, CurLineFlip), Tcls(X \ 8)) Then Vram(X, CurLineFlip) = Ccid(Mv1 And 1, Mv2 And 1)

                    End If
                End If
            Next TileMap
        End If
    End Sub

    Public Sub DrawLineOldGameBoy() 'Using VRam()
        CurLine = RAM(65348, 0)
        If CurLine = LastLine Then Exit Sub
        LastLine = CurLine
        CurLineFlip = MHEIGHT - CurLine

        ' Draw Background
        ' Get BG & window Tile Pattern Data Address
        If RAM(65344, 0) And 16 Then
            TileData = 32768
        Else
            TileData = 34816
        End If
        If BgV Then
            ' Get BG Tile Table Address
            If RAM(65344, 0) And 8 Then
                TileMap = 39936
                Tms = 39936
            Else
                TileMap = 38912
                Tms = 38912
            End If
            TileEnd = TileMap + 1023
            Xoffset = RAM(65347, 0)
            Yoffset = RAM(65346, 0) + CurLine
            xs = Xoffset \ 8
            ys = Yoffset \ 8
            Yoffset = Yoffset And 7
            Xoffset = -(Xoffset And 7)
            For X = Xoffset To 159 Step 8
                TileTmp = TileMap + ys * 32 + xs
                If TileTmp > TileEnd Then TileTmp = TileTmp - 1024
                If TileData = 32768 Then           ' Tile Data @ &H8800-&h97FF is 128ed
                    TilePtr = RAM(TileTmp, 0) * 16             'Get pointer to tile
                Else
                    TilePtr = (RAM(TileTmp, 0) Xor 128) * 16
                End If

                xs = (xs + 1) Mod 32

                MemPtr = TileData + TilePtr + (Yoffset And 7) * 2
                Mv1 = RAM(MemPtr + 1, 0) : Mv2 = RAM(MemPtr, 0)
                If X > -1 And X < 153 Then
                    Vram(X + 7, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1) : Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 6, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1) : Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 5, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1) : Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 4, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1) : Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 3, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1) : Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 2, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1) : Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 1, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1) : Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1)
                Else
                    If X < 153 Then Vram(X + 7, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -7 And X < 154 Then Vram(X + 6, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -6 And X < 155 Then Vram(X + 5, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -5 And X < 156 Then Vram(X + 4, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -4 And X < 157 Then Vram(X + 3, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -3 And X < 158 Then Vram(X + 2, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -2 And X < 159 Then Vram(X + 1, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -1 And X < 160 Then Vram(X, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1)
                End If
            Next X
        End If


        'Draw Window

        If ((RAM(65344, 0) And 32) = 32) And WinV And (CurLine >= RAM(65354, 0)) And (RAM(65355, 0) < 167) Then
            ' Get window Tile Table Address
            If RAM(65344, 0) And 64 Then
                TileMap = 39936
            Else
                TileMap = 38912
            End If
            Yoffset = CurLine - RAM(65354, 0)
            TileMap = TileMap + (Yoffset \ 8) * 32
            Yoffset = Yoffset And 7
            For X = RAM(65355, 0) - 7 To 159 Step 8
                If TileData = 32768 Then           ' Tile Data @ &H8800-&h97FF is 128ed
                    TilePtr = RAM(TileMap, 0) * 16             'Get pointer to tile
                Else
                    TilePtr = (RAM(TileMap, 0) Xor 128) * 16
                End If
                MemPtr = TileData + TilePtr + (Yoffset And 7) * 2
                Mv1 = RAM(MemPtr + 1, 0) : Mv2 = RAM(MemPtr, 0)

                If X > -1 And X < 153 Then
                    Vram(X + 7, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1) : Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 6, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1) : Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 5, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1) : Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 4, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1) : Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 3, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1) : Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 2, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1) : Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X + 1, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1) : Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    Vram(X, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1)
                    TileMap = TileMap + 1
                Else
                    If X < 153 Then Vram(X + 7, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -7 And X < 154 Then Vram(X + 6, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -6 And X < 155 Then Vram(X + 5, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -5 And X < 156 Then Vram(X + 4, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -4 And X < 157 Then Vram(X + 3, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -3 And X < 158 Then Vram(X + 2, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -2 And X < 159 Then Vram(X + 1, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1)
                    Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                    If X > -1 And X < 160 Then Vram(X, CurLineFlip) = Colid2(0, Mv1 And 1, Mv2 And 1)
                    TileMap = TileMap + 1
                End If

            Next X
        End If

        'Draw Sprites
        If (RAM(65344, 0) And 2) And Objv Then
            If (RAM(65344, 0) And 4) = 0 Then
                SpriteY = 7
            Else
                SpriteY = 15
            End If
            For TileMap = 65180 To 65024 Step -4
                Y = RAM(TileMap, 0) - 16
                X = RAM(TileMap + 1, 0) - 8
                If Y <= CurLine And Y + SpriteY >= CurLine And X > -8 And X < 160 Then

                    If SpriteY = 7 Then TilePtr = ReadM(TileMap + 2) * 16 Else TilePtr = (RAM(TileMap + 2, 0) And 254) * 16 'Get pointer to tile
                    TCol = -((RAM(TileMap + 3, 0) And 128) > 0)
                    Spat = -((ReadM(TileMap + 3) And 16) > 0) + 1
                    Vf = RAM(TileMap + 3, 0) And 32
                    'init palete
                    MemPtr = 32768 + TilePtr

                    If (RAM(TileMap + 3, 0) And 64) Then MemPtr = MemPtr + SpriteY * 2 - (CurLine - Y) * 2 Else MemPtr = MemPtr + (CurLine - Y) * 2
                    If Vf Then Mv1 = Mir(RAM(MemPtr + 1, Vrm)) : Mv2 = Mir(RAM(MemPtr, Vrm)) Else Mv1 = RAM(MemPtr + 1, Vrm) : Mv2 = RAM(MemPtr, Vrm)

                    If TCol = 0 Then

                        If X < 153 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 7, CurLineFlip) = Colid2(Spat, Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -7 And X < 154 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 6, CurLineFlip) = Colid2(Spat, Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -6 And X < 155 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 5, CurLineFlip) = Colid2(Spat, Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -5 And X < 156 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 4, CurLineFlip) = Colid2(Spat, Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -4 And X < 157 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 3, CurLineFlip) = Colid2(Spat, Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -3 And X < 158 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 2, CurLineFlip) = Colid2(Spat, Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -2 And X < 159 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 1, CurLineFlip) = Colid2(Spat, Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -1 And X < 160 Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X, CurLineFlip) = Colid2(Spat, Mv1 And 1, Mv2 And 1)

                    Else
                        Cid2 = Colid2(0, 0, 0)
                        If X < 153 Then If Cmp2RGBByte(Vram(X + 7, CurLineFlip), Cid2) Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 7, CurLineFlip) = Colid2(Spat, Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -7 And X < 154 Then If Cmp2RGBByte(Vram(X + 6, CurLineFlip), Cid2) Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 6, CurLineFlip) = Colid2(Spat, Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -6 And X < 155 Then If Cmp2RGBByte(Vram(X + 5, CurLineFlip), Cid2) Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 5, CurLineFlip) = Colid2(Spat, Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -5 And X < 156 Then If Cmp2RGBByte(Vram(X + 4, CurLineFlip), Cid2) Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 4, CurLineFlip) = Colid2(Spat, Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -4 And X < 157 Then If Cmp2RGBByte(Vram(X + 3, CurLineFlip), Cid2) Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 3, CurLineFlip) = Colid2(Spat, Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -3 And X < 158 Then If Cmp2RGBByte(Vram(X + 2, CurLineFlip), Cid2) Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 2, CurLineFlip) = Colid2(Spat, Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -2 And X < 159 Then If Cmp2RGBByte(Vram(X + 1, CurLineFlip), Cid2) Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X + 1, CurLineFlip) = Colid2(Spat, Mv1 And 1, Mv2 And 1)
                        Mv1 = Mv1 \ 2 : Mv2 = Mv2 \ 2
                        If X > -1 And X < 160 Then If Cmp2RGBByte(Vram(X, CurLineFlip), Cid2) Then If ColiDx(Mv1 And 1, Mv2 And 1) Then Vram(X, CurLineFlip) = Colid2(Spat, Mv1 And 1, Mv2 And 1)

                    End If
                End If
            Next TileMap
        End If

        'Vram(10, 10).R = 255
    End Sub

    Public Sub DrawScreen() 'Using Viewport()
        FPS = FPS + 1

        If Fmode = 0 Then 'frame skip mode 1(act skip(x1(1),x2(2),x3(3),x4(4),x5(5),x6(6))
            If FPS Mod Fskip > 0 Then
                Skipf = True
                If LimitFps Then
                    Do
                        QueryPerformanceCounter(CurEnd)
                        dblResult = (CurEnd - CurStart) / CurFreq
                        'DoEvents
                    Loop While dblResult < CoefWait '16.6
                End If
                Exit Sub
            End If
        Else 'frame skip mode 2(act skip(x1.20(6),x1.25(5),x1,3(4),x1.5(3))
            If FPS Mod Fskip = 0 Then
                Skipf = True
                If LimitFps Then
                    Do
                        QueryPerformanceCounter(CurEnd)
                        dblResult = (CurEnd - CurStart) / CurFreq
                        'DoEvents
                    Loop While dblResult < CoefWait '16.6
                End If
                Exit Sub
            End If
        End If

        'WinApi
        'StretchDIBits desthdc, 0, 0, Dw, DH, 0, 0, 160, 144, Vram(0, 0), BB, 0, vbSrcCopy
        'For J = 0 To MHEIGHT
        '    K = MHEIGHT - J
        '    K = K * MAX_WIDTH
        '    For I = 0 To MWIDTH
        '        L = I + K
        '        Viewport(L).B = Vram(I, J).B
        '        Viewport(L).G = Vram(I, J).G
        '        Viewport(L).R = Vram(I, J).R
        '    Next I
        'Next J
        'Call CopyMemory(Viewport(0).B, Vram(0, 0).B, 69120)

        'Dim g As Graphics = frmPrincipale.pbScreen.CreateGraphics()
        'Dim p As Pen = New Pen(Color.Red)
        'For j = 0 To MHEIGHT
        '    K = MHEIGHT - j
        '    K = K * MAX_WIDTH
        '    For I = 0 To MWIDTH
        '        L = I + K
        '        'Viewport(L).B = Vram(I, j).B
        '        'Viewport(L).G = Vram(I, j).G
        '        'Viewport(L).R = Vram(I, j).R
        '        p.Color = Color.FromArgb(Vram(I, j).R, Vram(I, j).G, Vram(I, j).B)
        '        g.DrawLine(p, I, j, I + 1, j)
        '    Next I
        'Next j

        Dim btMapData As BitmapData
        btMapData = btMap.LockBits(New Rectangle(0, 0, btMap.Width, btMap.Height), Imaging.ImageLockMode.WriteOnly, Imaging.PixelFormat.Format24bppRgb)
        Dim Viewport As IntPtr = btMapData.Scan0
        'Dim rgb As RGBByte
        'rgb.R = 255
        For j = 0 To MHEIGHT
            K = MHEIGHT - j
            K = K * MAX_WIDTH
            For I = 0 To MWIDTH
                L = I + K
                Marshal.StructureToPtr(Vram(I, j), Viewport + 3 * L, False)
            Next I
        Next j
        btMap.UnlockBits(btMapData)

        Skipf = False
        If LimitFps Then
            Do
                QueryPerformanceCounter(CurEnd)
                dblResult = (CurEnd - CurStart) / CurFreq
            Loop While dblResult < CoefWait '16.6
            'DoEvents
        End If

        TimeCalcul = GetTickCount() - TimeFps
        If (TimeCalcul >= 1000) Then
            Dim Tmp1 As Single, tmp2 As Single, Mipsd As Single
            Tmp1 = TimeCalcul / 1000
            tmp2 = Tmp1
            Mipsd = ((Mips / tmp2) / 1024) / 1024
            Tmp1 = FPS / Tmp1
            tmp2 = (z80.Mhz * 70224 + z80.Clcount) / tmp2
            tmp2 = ((tmp2 / 1024)) / 1024
            tmp2 = Format$(tmp2, "000.000")
            frmPrincipale.sb.Text = "Cpu " & Str(tmp2) & " Hhz(" & Format$(Mipsd, "0.000") & " Mips),Fps " & Str(CLng(Tmp1)) & " (" & Format$(Tmp1 / 60, "00.0%") & ")"
            TimeFps = GetTickCount()
            Mips = 0
            z80.Mhz = 0
            FPS = 0
            If (LimitFps) Then
                Tmp1 = (Tmp1 / 10) - 6 '(Fps-60)/10 pour réguler automatiquement le CoefWait
                If (Math.Abs(Tmp1) > 2) Then If (Tmp1 > 0) Then Tmp1 = 2 Else Tmp1 = -2 'On peut pas modifier plus de 2 unités ;) sinon la régulation risque de s'enbaler
                CoefWait = CoefWait + Tmp1
                If (CoefWait < 0) Then
                    CoefWait = 0
                ElseIf (CoefWait > 200) Then
                    CoefWait = 200
                End If
            End If
        End If

        Call frmPrincipale.pbScreen.Refresh()
        'DoEvents
    End Sub

    Private Sub InitMir()
        Dim I As Integer
        For I = 0 To 255
            Mir(I) = (I And 128) \ 128 + (I And 64) \ 32 + (I And 32) \ 8 + (I And 16) \ 2 +
             (I And 8) * 2 + (I And 4) * 8 + (I And 2) * 32 + (I And 1) * 128
        Next I
    End Sub

    Public Sub InitCol()
        Dim Coef As Integer = 8
        Dim I As Byte
        For I = 0 To 7
            objpCC(I, 0) = RGB24(31 * Coef, 31 * Coef, 31 * Coef)
            bgpCC(I, 0) = RGB24(31 * Coef, 31 * Coef, 31 * Coef)
            objpCC(I, 1) = RGB24(21 * Coef, 21 * Coef, 21 * Coef)
            bgpCC(I, 1) = RGB24(21 * Coef, 21 * Coef, 21 * Coef)
            objpCC(I, 2) = RGB24(10 * Coef, 10 * Coef, 10 * Coef)
            bgpCC(I, 2) = RGB24(10 * Coef, 10 * Coef, 10 * Coef)
            objpCC(I, 3) = RGB24(0, 0, 0)
            bgpCC(I, 3) = RGB24(0, 0, 0)
        Next I
        Call InitMir()
        Dim tm2 As Long, tm1 As Long
        If frmPrincipale.EmulerGBC Then
            Colid(0, 0) = RGB24(31 * Coef, 31 * Coef, 31 * Coef)
            For tm2 = 1 To 128
                Colid(0, tm2) = RGB24(21 * Coef, 21 * Coef, 21 * Coef)
            Next tm2

            For tm1 = 1 To 128
                Colid(tm1, 0) = RGB24(10 * Coef, 10 * Coef, 10 * Coef)
            Next tm1

            For tm1 = 1 To 128
                For tm2 = 1 To 128
                    Colid(tm1, tm2) = RGB24(0, 0, 0)
                Next tm2
            Next tm1
        Else
            Colid(0, 0) = RGB24(31 * Coef, 31 * Coef, 31 * Coef)
            For tm2 = 1 To 128
                Colid(0, tm2) = RGB24(21 * Coef, 21 * Coef, 21 * Coef)
            Next tm2

            For tm1 = 1 To 128
                Colid(tm1, 0) = RGB24(10 * Coef, 10 * Coef, 10 * Coef)
            Next tm1

            For tm1 = 1 To 128
                For tm2 = 1 To 128
                    Colid(tm1, tm2) = RGB24(0, 0, 0)
                Next tm2
            Next tm1
        End If
        ColiDx(0, 0) = 0

        For tm2 = 1 To 128
            ColiDx(0, tm2) = 2
        Next tm2

        For tm1 = 1 To 128
            ColiDx(tm1, 0) = 1
        Next tm1

        For tm1 = 1 To 128
            For tm2 = 1 To 128
                ColiDx(tm1, tm2) = 3
            Next tm2
        Next tm1

        Colid2(0, 0, 0) = Colid(0, 0)
        For tm2 = 1 To 128
            Colid2(0, 0, tm2) = Colid(0, 1)
        Next tm2
        For tm1 = 1 To 128
            Colid2(0, tm1, 0) = Colid(1, 0)
        Next tm1
        For tm1 = 1 To 128
            For tm2 = 1 To 128
                Colid2(0, tm1, tm2) = Colid(1, 1)
            Next tm2
        Next tm1
        Colid2(1, 0, 0) = Colid(0, 0)
        For tm2 = 1 To 128
            Colid2(1, 0, tm2) = Colid(0, 1)
        Next tm2 : For tm1 = 1 To 128
            Colid2(1, tm1, 0) = Colid(1, 0)
        Next tm1 : For tm1 = 1 To 128
            For tm2 = 1 To 128
                Colid2(1, tm1, tm2) = Colid(1, 1)
            Next tm2
        Next tm1
        Colid2(2, 0, 0) = Colid(0, 0)
        For tm2 = 1 To 128
            Colid2(2, 0, tm2) = Colid(0, 1)
        Next tm2
        For tm1 = 1 To 128
            Colid2(2, tm1, 0) = Colid(1, 0)
        Next tm1 : For tm1 = 1 To 128
            For tm2 = 1 To 128
                Colid2(2, tm1, tm2) = Colid(1, 1)
            Next tm2 : Next tm1

        btMap = CType(frmPrincipale.pbScreen.Image, Bitmap)
    End Sub

    Public Function IntToRGB(Color As Long) As RGBByte
        IntToRGB.B = Int(((Color And 31744) / 31744) * 255)
        IntToRGB.G = Int(((Color And 992) / 992) * 255)
        IntToRGB.R = Int(((Color And 31) / 31) * 255)
    End Function

    Function Cmp2RGBByte(A As RGBByte, B As RGBByte) As Boolean
        Cmp2RGBByte = False
        If (A.R <> B.R) Then Exit Function
        If (A.G <> B.G) Then Exit Function
        If (A.B <> B.B) Then Exit Function
        Cmp2RGBByte = True
    End Function




End Module
