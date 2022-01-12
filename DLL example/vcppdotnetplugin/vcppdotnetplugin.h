// vcppdotnetplugin.h

#pragma once

using namespace System;

namespace vcppdotnetplugin
{
	// There must be a public Class that's named LCDSmartie	(within your namespace)
	public __gc class LCDSmartie
	{
	public:
		LCDSmartie();
		~LCDSmartie();
		String __gc *function1(String *param1, String *param2);
		String __gc *function2(String *param1, String *param2);
		int GetMinRefreshInterval();
	};

}
