#include "stdafx.h"
#include <msclr\auto_gcroot.h>
#include <msclr\marshal_cppstd.h>

using namespace System;
using namespace System::Reflection;
using namespace System::Text;
using namespace System::Runtime::InteropServices;
using namespace System::Collections::Generic;

const int maxPlugins = 100;

 ref struct plugin
{
	static  Assembly^ myAssembly;
	static Type^ myType;
	Object^ myObject;
	array <MethodInfo^>^ myMethods;
};

ref class  globals {
public:
	static array <plugin^>^ plugins = gcnew array<plugin^>(maxPlugins); ;
	static int numPlugins = 0 ;
};


extern "C"
{

__declspec(dllexport) void __stdcall SmartieInit(void) 
{
// nothing to init
}

__declspec(dllexport) char * __stdcall BridgeInit(const char *param1, int *id, int *minRefreshInterval) 
{
	String^ result = gcnew String("");

	*id = -1;
	*minRefreshInterval = 0;

	if (globals::numPlugins >= maxPlugins)
		result = "Too many dot net plugins.";
	else 
	{
		try
		{
			plugin^ current = gcnew plugin();
		
			String^ managed_param1 = gcnew String(param1);    
			String^ loadPath = String::Concat("plugins\\", managed_param1);

			String^ classtype = managed_param1;
			if (classtype->LastIndexOf(".") != -1)
				classtype = managed_param1->Substring(0, classtype->LastIndexOf("."));
			classtype = classtype->Concat(classtype, ".LCDSmartie");

			current->myAssembly = Assembly::LoadFrom( loadPath );
			if (current->myAssembly)
			{
				current->myType = current->myAssembly->GetType(classtype);
				if (current->myType)
				{
					current->myObject = Activator::CreateInstance(current->myType);

					if (current->myObject)
					{
						current->myMethods = gcnew array<MethodInfo^>(20) ;
						bool found = false;

						for (int iFunc=1; iFunc<=20; iFunc++) 
						{
							String^ function = String::Concat("function", iFunc.ToString());
							current->myMethods[iFunc-1] = current->myType->GetMethod(function);
							if (current->myMethods[iFunc-1])
								found = true;
						}
						
						if (!found)
							result = "Class contains no Smartie methods!";
						else
						{
							MethodInfo^ minRefresh = current->myType->GetMethod("GetMinRefreshInterval");
							if (minRefresh)
							{
								array <Object^>^ noargs = gcnew array<Object^>(0);
								Object^ o = minRefresh->Invoke(current->myObject, noargs);
								*minRefreshInterval = safe_cast<int>(o);
							}

							globals::plugins[globals::numPlugins++]=current;
							*id = globals::numPlugins;
						}
					}
					else result = "Failed to create an instance";
				}
				else result = String::Concat("Plugin does not contain a type of ", classtype);
			}
			else result = String::Concat("Failed to load ", loadPath);
		} catch (Exception^ e) {
			result = String::Concat("[Exception: ", e->Message);
			if (e->InnerException) 
				result = String::Concat(result, ": ", e->InnerException->Message);
			result = String::Concat(result, "]");
		}
	}
	

	static char buffer[1024];
	buffer[0] = 0;
	if (result != "")
	{
		std::string value = msclr::interop::marshal_as<std::string>(result);
		strcpy_s(buffer, value.c_str());		
	}
	return buffer;
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
	UTF8Encoding^ encoder = gcnew UTF8Encoding();
	param = ConvertAnsiToUtf8(param1);
	String^ managed_param1 = gcnew String(param, 0, strlen(param), encoder);
	param = ConvertAnsiToUtf8(param2);
	String^ managed_param2 = gcnew String(param, 0, strlen(param), encoder);
	String^ result = gcnew String("");
	plugin^ current = globals::plugins[iBridgeId - 1];

	// 0 based array
	iFunc--;

	try
	{
		if (!current->myMethods || !current->myMethods[iFunc])
			result = "[Dll: function not found]";
		else
		{
			array<String^>^ args = gcnew array<String^>(2);
			args[0] = managed_param1;
			args[1] = managed_param2;

			result = reinterpret_cast<String^>(current->myMethods[iFunc]->Invoke(current->myObject, args));
		}

	} catch (Exception^ e) {
		result = String::Concat("[Exception: ", e->Message);
		if (e->InnerException) 
			result = String::Concat(result, ": ", e->InnerException->Message);
		result = String::Concat(result, "]");
	}

	static char buffer[1024];
	buffer[0] = 0;
	if (result != "")
	{
		std::string value = msclr::interop::marshal_as<std::string>(result);
		strcpy_s(buffer, value.c_str());
	}
	return buffer;
}

}
