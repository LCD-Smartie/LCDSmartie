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

    '
    '
    ' From 5.6 LCD Smartie support embedded info retreval for plugins to be displayed under the plugins tab on Config screen a help users to use plugins withoug searching for documentation 
    ' The follwoing two functions are examples for the additional info 
    Public Function SmartieDemo()
        Dim demolist As New StringBuilder()

        demolist.AppendLine("plugin info and description to be displayed here plugin for LCD Smartie")
        demolist.AppendLine("Replace the text on the lines and add more to generate the emebdded info")
        demolist.AppendLine("------ Function1 ------")
        demolist.AppendLine(">>>  Description  <<<")
        demolist.AppendLine("YSome additional info ")
        demolist.AppendLine("The following example can be added by double clicking:  $dll(plugin,1,param1,param2)")
        demolist.AppendLine("------ Function20 ------")
        demolist.AppendLine(">>>  Plugin info  <<<")
        demolist.AppendLine("example:  $dll(plugin,20, , )")
        demolist.AppendLine("------------------------------------------------------------------------------------------------------------------")
        demolist.AppendLine(" *** Visit ***")
        demolist.AppendLine("> Home page")
        demolist.AppendLine("https://lcdsmartie.sourceforge.net")
        demolist.AppendLine("> Forums")
        demolist.AppendLine("https://www.lcdsmartie.org")
        demolist.AppendLine("> Active development branch (latest version)")
        demolist.AppendLine("https://github.com/LCD-Smartie/LCDSmartie/")

        Dim result As String = demolist.ToString()
        Return result
    End Function

    Public Function SmartieInfo()
        Return "Developer: Dev Name" & vbNewLine & "Version: 0.0 "
    End Function

    
    Public Function GetMinRefreshInterval() As Integer

        Return 300 ' 300 ms (around 3 times a second)

    End Function


End Class
