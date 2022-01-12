//
//      ===  Demo LCDSmartie Plugin for c#  ===
//
// dot net plugins are supported in LCD Smartie 5.3 beta 3 and above.
//

// You may provide/use upto 20 functions (function1 to function20).


using System;

namespace csharpplugin
{
	/// <summary>
	/// Summary description for Class1.
	/// </summary>
	public class LCDSmartie
	{
		public LCDSmartie()
		{
			//
			// TODO: Add constructor logic here
			//
		}


		// This function is used in LCDSmartie by using the dll command as follows:
		//    $dll(vbdotnetplugin,1,hello,there)
		// Smartie will then display on the LCD: function called with (hello, there)
		public string function1(string param1, string param2)
		{
			return "function called with (" + param1 + ", " + param2 + ")";
		}

		// This function is used in LCDSmartie by using the dll command as follows:
		//    $dll(vbdotnetplugin,2,hello,there)
		// Smartie will then display on the LCD: c#
		public string function2(string param1, string param2)
		{
			return "c#";
		}

		//
		// Define the minimum interval that a screen should get fresh data from our plugin.
		// The actual value used by Smartie will be the higher of this value and of the "dll check interval" setting
		// on the Misc tab.  [This function is optional, Smartie will assume 300ms if it is not provided.]
		// 
		public int GetMinRefreshInterval()
		{
			return 300; // 300 ms (around 3 times a second)
		}

	}





}
