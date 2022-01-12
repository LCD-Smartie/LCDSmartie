#include "stdafx.h"
#include "DNBridge.h"

#include <windows.h>
#include <string.h>
#include <_vcclrit.h>

#using <mscorlib.dll>

using namespace System;
using namespace System::Reflection;
using namespace System::Text;

const int maxPlugins = 100;

__gc struct plugin
{
	Assembly __gc *myAssembly;
	Type __gc *myType;
	Object __gc *myObject;
	MethodInfo __gc *myMethods[];
};

__gc class  globals {
public:
	 static plugin *plugins __gc [];
	 static int numPlugins = -1;
};


extern "C"
{

__declspec(dllexport) void __stdcall SmartieInit(void) 
{
    __crt_dll_initialize();
	if (globals::numPlugins == -1)
	{
		globals::plugins = new plugin *[maxPlugins];
		globals::numPlugins = 0;
	}
}

__declspec(dllexport) char * __stdcall BridgeInit(const char *param1, int *id, int *minRefreshInterval) 
{
	String __gc *result = new String("");

	*id = -1;
	*minRefreshInterval = 0;

	if (globals::numPlugins >= 100)
		result = "Too many dot net plugins.";
	else 
	{
		try
		{
			plugin __gc *current = new plugin();
		
			String __gc *managed_param1 = new String(param1);    
			String __gc *loadPath = String::Concat(S"plugins\\", managed_param1);

			String __gc *classtype = managed_param1;
			if (classtype->LastIndexOf(S".") != -1)
				classtype = managed_param1->Substring(0, classtype->LastIndexOf(S"."));
			classtype = classtype->Concat(classtype, S".LCDSmartie");

			current->myAssembly = Assembly::LoadFrom( loadPath );
			if (current->myAssembly)
			{
				current->myType = current->myAssembly->GetType(classtype);
				if (current->myType)
				{
					current->myObject = Activator::CreateInstance(current->myType);

					if (current->myObject)
					{
						current->myMethods = new MethodInfo*[20];
						bool found = false;

						for (int iFunc=1; iFunc<=20; iFunc++) 
						{
							String __gc *function = String::Concat(S"function", iFunc.ToString());
							current->myMethods[iFunc-1] = current->myType->GetMethod(function);
							if (current->myMethods[iFunc-1])
								found = true;
						}
						
						if (!found)
							result = S"Class contains no Smartie methods!";
						else
						{
							MethodInfo __gc *minRefresh = current->myType->GetMethod("GetMinRefreshInterval");
							if (minRefresh)
							{
								Object __gc *noargs[] = new Object*[0];
								Object __gc *o = minRefresh->Invoke(current->myObject, noargs);
								*minRefreshInterval = *dynamic_cast<__box Int32*>(o);

							}

							globals::plugins[globals::numPlugins++]=current;
							*id = globals::numPlugins;
						}
					}
					else result = S"Failed to create an instance";
				}
				else result = String::Concat(S"Plugin does not contain a type of ", classtype);
			}
			else result = String::Concat(S"Failed to load ", loadPath);
		} catch (Exception *e) {
			result = String::Concat("[Exception: ", e->Message);
			if (e->InnerException) 
				result = String::Concat(result, S": ", e->InnerException->Message);
			result = String::Concat(result, S"]");
		}
	}
	

	static char buffer[1024];
	buffer[0] = 0;
	if (result != "")
	{
		char tmp __gc[] = System::Text::Encoding::UTF8->GetBytes(result->ToCharArray());
		char __pin *value = &tmp[0];
			
		strncpy(buffer, value, 1024);
	}
	return buffer;
}

char *ConvertUtf8ToAnsi(char *input)
{
	unsigned int n, nn, nLength;
	unsigned char *inputString = (unsigned char *)input;

	nLength = strlen(input);
	nn = 0;
	for (n=0; n<nLength; n++)
	{
		if (inputString[n] > 127 && (n+1) < nLength)
		{
			if (inputString[n] == 194 || inputString[n] == 195)
			{
				inputString[nn] = 64 * (inputString[n] - 194)  + inputString[n+1];
			}
			n++;
		}
		else
		{
			inputString[nn] = inputString[n];
		}
		nn++;
	}
	if (nLength>0)
		inputString[nn] = 0;
	return input;
}

char *ConvertAnsiToUtf8(const char *input)
{
	static unsigned char buffer[2048];
	unsigned int n, nn, nLength;
	unsigned const char *inputString = (unsigned const char *)input;

	nLength = strlen(input);
	nn = 0;
	for (n=0; n<nLength && inputString[n]!=0; n++)
	{
		if (inputString[n] > 127)
		{
			buffer[nn++] = 192 + inputString[n]/64;
			buffer[nn++] = 128 + inputString[n]%64;
		}
		else
		{
			buffer[nn++] = inputString[n];
		}
	}
	buffer[nn] = '\0';
	return (char *) buffer;
}

__declspec(dllexport) char * __stdcall BridgeFunc(int iBridgeId, int iFunc, const char *param1, const char *param2)
{
	char *param;
	UTF8Encoding* encoder = new UTF8Encoding();
	param = ConvertAnsiToUtf8(param1);
	String __gc *managed_param1 = new String(param, 0, strlen(param), encoder);
	param = ConvertAnsiToUtf8(param2);
	String __gc *managed_param2 = new String(param, 0, strlen(param), encoder);
	String __gc *result = new String("");
	plugin __gc *current = globals::plugins[iBridgeId - 1];

	// 0 based array
	iFunc--;

	try
	{
		if (!current->myMethods || !current->myMethods[iFunc])
			result = "[Dll: function not found]";
		else
		{
			String __gc *args[] = new String*[2];
			args[0] = managed_param1;
			args[1] = managed_param2;

			result = static_cast<String*>(current->myMethods[iFunc]->Invoke(current->myObject, args));
		}

	} catch (Exception *e) {
		result = String::Concat("[Exception: ", e->Message);
		if (e->InnerException) 
			result = String::Concat(result, S": ", e->InnerException->Message);
		result = String::Concat(result, S"]");
	}

	static char buffer[1024];
	buffer[0] = 0;
	if (result != "")
	{
		char tmp __gc[] = encoder->GetBytes(result->ToCharArray());
		char __pin *value = &tmp[0];
	
		strncpy(buffer, value, 1024);
		ConvertUtf8ToAnsi(buffer);
	}
	return buffer;
}

}
