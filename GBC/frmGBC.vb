Option Explicit On

Public Class frmPrincipale
    Public BNotPause As Boolean
    Public EmulerGBC As Boolean = True

    Private LoadRom As Boolean

    Private ModeEcran As Byte '1x 2x 3x 4x FullScreen ;)

    Private Sub QuitterToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles QuitterToolStripMenuItem.Click
        Application.Exit()
    End Sub

    Private Sub PlayGame()
        If (LoadRom) Then
            TimClockCore.Enabled = True
        End If
    End Sub

    Private Sub OuvrirToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles OuvrirToolStripMenuItem.Click
        Dim ODF As OpenFileDialog = New OpenFileDialog()
        ODF.Title = "Ouvrir..."
        ODF.Filter = "GameBoy Roms (*.gb;*.gbc;*.cgb)|*.gb;*.gbc;*.cgb"
        ODF.InitialDirectory = Application.ExecutablePath

        If ODF.ShowDialog <> Windows.Forms.DialogResult.Cancel Then
            'Call Graph.PictArrayInit2D(pbScreen.Image)
            Call Graph.InitCol()
            Call z80.InitCPU()
            Dim I2 As Integer
            For I2 = 0 To 7 ' For Set,Bit and Res
                z80.BITT(I2 * 8) = 2 ^ I2
                z80.SETT(I2 * 8) = 255 - 2 ^ I2
            Next I2
            Call Sound.InitWave()

            Dim bytes As Byte() = My.Computer.FileSystem.ReadAllBytes(ODF.FileName)

            If (bytes.Length <= 0) Then Exit Sub
            Dim RomBank As Integer
            RomBank = (bytes.Length \ 16384)
            Dim RomRest As Integer
            RomRest = bytes.Length Mod 16384
            If (RomBank = -1) Then RomBank = 0
            Dim NbRomBank As Integer
            NbRomBank = RomBank + If(RomRest <> 0, 1, 0)
            ReDim ROM(16383, NbRomBank - 1)

            For I = 0 To (NbRomBank - 1)
                Dim Len As Integer
                If (RomRest > 0 And I = RomBank) Then Len = RomRest Else Len = 16384
                'Array.Copy(bytes, I * 16384, ROM(I, 0), 0, Len)
                For J = 0 To Len - 1
                    ROM(J, I) = bytes(I * 16384 + J)
                Next
            Next

            '_rom = New Byte[RomBank+(rest>0?1:0)][];
            '        For (Int() i = 0; i < _rom.Length; ++i)
            '        {
            '            _rom[I] = New Byte[16384];
            '            Int Len;
            '            If (rest > 0 && I == RomBank) Then len = rest;
            '            Else Len = 16384;
            '            Array.Copy(bFil, I * 16384, _rom[i], 0, Len);
            '        }

            BNotPause = True
            LoadRom = True
            Pause.Checked = False
            Graph.ClearViewport()
            pbScreen.Refresh()

            Call Mem.InitMem()

            Dim Extension As String

            If EmulerGBC Then
                If ROM(&H143, 0) = 192 Then Extension = "(GBC) " : GameBoyMode = 1 Else If ROM(&H143, 0) <> 0 Then Extension = "(GB/GBC) " : GameBoyMode = 1 Else Extension = "(GB) " : GameBoyMode = 0
            Else
                If ROM(&H143, 0) = 192 Then Extension = "(GBC) " Else If ROM(&H143, 0) <> 0 Then Extension = "(GB/GBC) " Else Extension = "(GB) "
                GameBoyMode = 0
            End If

            'Resize
            Call z80.Reset()

            Dim Titre As String
            Titre = ""
            For I = 0 To 15
                If Mem.RomInfo.TitleB(I) = 0 Then
                    Exit For
                Else
                    Titre = Titre & Chr(Mem.RomInfo.TitleB(I))
                End If
            Next I
            RomInfo.Title = Titre
            'If tls = "WORMS" Then nwr = False Else nwr = True

            Me.Text = "Emulateur GBC - " & Titre & " " & Extension

            Call Sound.UpDateVolumSound()
            Call Mem.ReadRam()
            BNotPause = True

            Call PlayGame()
            Exit Sub

        End If
    End Sub

    Private NbClock As Integer = 0
    Private Sub TimClockCore_Tick(sender As Object, e As EventArgs) Handles TimClockCore.Tick
        Dim Dv As Integer
        Dv = 1000 / TimClockCore.Interval
        NbClock += 4194304 * 2
        Dim nbc As Integer
        nbc = NbClock / Dv
        NbClock = NbClock Mod Dv
        Dim ii As Integer
        For ii = 1 To nbc '83870
            Call z80.RunCpu()
        Next
    End Sub

    Private Sub frmPrincipale_KeyDown(sender As Object, e As KeyEventArgs)
    End Sub

    Private Sub frmPrincipale_KeyUp(sender As Object, e As KeyEventArgs)
    End Sub

    Private Sub frmPrincipale_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Call Sound.InitDxSound(Me.Handle)
    End Sub
End Class
