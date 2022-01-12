'
'      ===  Demo LCDSmartie Plugin for vb.net  ===
'
' dot net plugins are supported in LCD Smartie 5.3 beta 3 and above.
'

' There must be a public Class that's named LCDSmartie
Public Class LCDSmartie

    ' This function is used in LCDSmartie by using the dll command as follows:
    '    $dll(vbdotnetplugin,1,hello,there)
    ' Smartie will then display on the LCD: function called with (hello, there)
    Public Function function1(ByVal param1 As String, ByVal param2 As String) As String

        Return "function called with (" & param1 & ", " & param2 & ")"

    End Function

    ' This function is used in LCDSmartie by using the dll command as follows:
    '    $dll(vbdotnetplugin,2,hello,there)
    ' Smartie will then display on the LCD: VB.net
    Public Function function2(ByVal param1 As String, ByVal param2 As String) As String

        Return "VB.net"

    End Function


    ' You may provide/use upto 20 functions (function1 to function20).


    '
    ' Define the minimum interval that a screen should get fresh data from our plugin.
    ' The actual value used by Smartie will be the higher of this value and of the "dll check interval" setting
    ' on the Misc tab.  [This function is optional, Smartie will assume 300ms if it is not provided.]
    '
    Public Function GetMinRefreshInterval() As Integer

        Return 300 ' 300 ms (around 3 times a second)

    End Function


End Class
