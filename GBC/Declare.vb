Option Explicit On

Module Declaration

    'Global Cmd As CommonDialog

    Public Declare Function QueryPerformanceFrequency _
        Lib "kernel32" (
        ByRef lpFrequency As Long
    ) As Long

    Public Declare Function QueryPerformanceCounter _
        Lib "kernel32" (
        ByRef lpPerformanceCount As Long
    ) As Long

    Public Declare Function GetKeyState Lib "user32" (ByVal nVirtKey As Integer) As Short
    Public Declare Function GetTickCount Lib "kernel32" () As Integer

End Module
