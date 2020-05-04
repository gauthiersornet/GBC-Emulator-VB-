<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmPrincipale
    Inherits System.Windows.Forms.Form

    'Form remplace la méthode Dispose pour nettoyer la liste des composants.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Requise par le Concepteur Windows Form
    Private components As System.ComponentModel.IContainer

    'REMARQUE : la procédure suivante est requise par le Concepteur Windows Form
    'Elle peut être modifiée à l'aide du Concepteur Windows Form.  
    'Ne la modifiez pas à l'aide de l'éditeur de code.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.FichierToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.OuvrirToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem1 = New System.Windows.Forms.ToolStripSeparator()
        Me.QuitterToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.pbScreen = New System.Windows.Forms.PictureBox()
        Me.pbGameBoyColor = New System.Windows.Forms.PictureBox()
        Me.sb = New System.Windows.Forms.Label()
        Me.Pause = New System.Windows.Forms.CheckBox()
        Me.TimClockCore = New System.Windows.Forms.Timer(Me.components)
        Me.MenuStrip1.SuspendLayout()
        CType(Me.pbScreen, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.pbGameBoyColor, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'MenuStrip1
        '
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.FichierToolStripMenuItem})
        Me.MenuStrip1.Location = New System.Drawing.Point(0, 0)
        Me.MenuStrip1.Name = "MenuStrip1"
        Me.MenuStrip1.Size = New System.Drawing.Size(479, 24)
        Me.MenuStrip1.TabIndex = 1
        Me.MenuStrip1.Text = "MenuStrip1"
        '
        'FichierToolStripMenuItem
        '
        Me.FichierToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.OuvrirToolStripMenuItem, Me.ToolStripMenuItem1, Me.QuitterToolStripMenuItem})
        Me.FichierToolStripMenuItem.Name = "FichierToolStripMenuItem"
        Me.FichierToolStripMenuItem.Size = New System.Drawing.Size(54, 20)
        Me.FichierToolStripMenuItem.Text = "Fichier"
        '
        'OuvrirToolStripMenuItem
        '
        Me.OuvrirToolStripMenuItem.Name = "OuvrirToolStripMenuItem"
        Me.OuvrirToolStripMenuItem.Size = New System.Drawing.Size(180, 22)
        Me.OuvrirToolStripMenuItem.Text = "Ouvrir"
        '
        'ToolStripMenuItem1
        '
        Me.ToolStripMenuItem1.Name = "ToolStripMenuItem1"
        Me.ToolStripMenuItem1.Size = New System.Drawing.Size(177, 6)
        '
        'QuitterToolStripMenuItem
        '
        Me.QuitterToolStripMenuItem.Name = "QuitterToolStripMenuItem"
        Me.QuitterToolStripMenuItem.Size = New System.Drawing.Size(180, 22)
        Me.QuitterToolStripMenuItem.Text = "Quitter"
        '
        'pbScreen
        '
        Me.pbScreen.Image = Global.GBC.My.Resources.Resources.Blank
        Me.pbScreen.Location = New System.Drawing.Point(121, 118)
        Me.pbScreen.Name = "pbScreen"
        Me.pbScreen.Size = New System.Drawing.Size(240, 216)
        Me.pbScreen.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage
        Me.pbScreen.TabIndex = 2
        Me.pbScreen.TabStop = False
        '
        'pbGameBoyColor
        '
        Me.pbGameBoyColor.Image = Global.GBC.My.Resources.Resources.Game_Boy_Color__green_
        Me.pbGameBoyColor.Location = New System.Drawing.Point(12, 27)
        Me.pbGameBoyColor.Name = "pbGameBoyColor"
        Me.pbGameBoyColor.Size = New System.Drawing.Size(456, 800)
        Me.pbGameBoyColor.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage
        Me.pbGameBoyColor.TabIndex = 0
        Me.pbGameBoyColor.TabStop = False
        '
        'sb
        '
        Me.sb.AutoSize = True
        Me.sb.Location = New System.Drawing.Point(114, 49)
        Me.sb.Name = "sb"
        Me.sb.Size = New System.Drawing.Size(64, 13)
        Me.sb.TabIndex = 3
        Me.sb.Text = "                   "
        Me.sb.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'Pause
        '
        Me.Pause.AutoSize = True
        Me.Pause.Enabled = False
        Me.Pause.Location = New System.Drawing.Point(242, 719)
        Me.Pause.Name = "Pause"
        Me.Pause.Size = New System.Drawing.Size(56, 17)
        Me.Pause.TabIndex = 4
        Me.Pause.Text = "Pause"
        Me.Pause.UseVisualStyleBackColor = True
        '
        'TimClockCore
        '
        Me.TimClockCore.Interval = 10
        '
        'frmPrincipale
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(479, 813)
        Me.Controls.Add(Me.Pause)
        Me.Controls.Add(Me.sb)
        Me.Controls.Add(Me.pbScreen)
        Me.Controls.Add(Me.pbGameBoyColor)
        Me.Controls.Add(Me.MenuStrip1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.MainMenuStrip = Me.MenuStrip1
        Me.MaximizeBox = False
        Me.Name = "frmPrincipale"
        Me.Text = "Emulateur GBC"
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        CType(Me.pbScreen, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.pbGameBoyColor, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents pbGameBoyColor As PictureBox
    Friend WithEvents MenuStrip1 As MenuStrip
    Friend WithEvents FichierToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents OuvrirToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem1 As ToolStripSeparator
    Friend WithEvents QuitterToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents pbScreen As PictureBox
    Friend WithEvents sb As Label
    Friend WithEvents Pause As CheckBox
    Friend WithEvents TimClockCore As Timer
End Class
