//
//      ===  Demo LCDSmartie Plugin for vc++.net  ===
//
// dot net plugins are supported in LCD Smartie 5.3 beta 3 and above.
//

// You may provide/use upto 20 functions (function1 to function20).


#include "stdafx.h"

#include "vcppdotnetplugin.h"

using namespace vcppdotnetplugin;

LCDSmartie::LCDSmartie()
{
	// Initialization in here
}

LCDSmartie::~LCDSmartie()
{
	// Clean up in here
}


// This function is used in LCDSmartie by using the dll command as follows:
//    $dll(vbdotnetplugin,1,hello,there)
// Smartie will then display on the LCD: function called with (hello, there)
String __gc *LCDSmartie::function1(String *param1, String *param2)
{
	return String::Concat(S"function called with (",
							param1,
							S", ", 
							param2, 
							S")");
}


// This function is used in LCDSmartie by using the dll command as follows:
//    $dll(vbdotnetplugin,2,hello,there)
// Smartie will then display on the LCD: c++.net
String __gc *LCDSmartie::function2(String *param1, String *param2)
{
	return new String("c++.net");
}


//
// Define the minimum interval that a screen should get fresh data from our plugin.
// The actual value used by Smartie will be the higher of this value and of the "dll check interval" setting
// on the Misc tab.  [This function is optional, Smartie will assume 300ms if it is not provided.]
// 
int LCDSmartie::GetMinRefreshInterval()
{
	return 300; // 300 ms (around 3 times a second)
}

