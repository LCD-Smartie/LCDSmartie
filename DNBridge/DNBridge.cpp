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
	static Assembly^ myAssembly;
	static Type^ myType;
	Object^ myObject;
	array <MethodInfo^>^ myMethods;
	MethodInfo^ InfoFunc;
	MethodInfo^ DemoFunc;

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

	__declspec(dllexport) char* __stdcall BridgeInit(const char* param1, int* id, int* minRefreshInterval)
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

				current->myAssembly = Assembly::LoadFrom(loadPath);
				if (current->myAssembly)
				{
					current->myType = current->myAssembly->GetType(classtype);
					if (current->myType)
					{
						current->myObject = Activator::CreateInstance(current->myType);

						if (current->myObject)
						{
							current->myMethods = gcnew array<MethodInfo^>(20);
							bool found = false;

							for (int iFunc = 1; iFunc <= 20; iFunc++)
							{
								String^ function = String::Concat("function", iFunc.ToString());
								current->myMethods[iFunc - 1] = current->myType->GetMethod(function);
								if (current->myMethods[iFunc - 1])
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
								current->InfoFunc = current->myType->GetMethod("SmartieInfo");
								current->DemoFunc = current->myType->GetMethod("SmartieDemo");

								globals::plugins[globals::numPlugins++] = current;
								*id = globals::numPlugins;
							}
						}
						else result = "Failed to create an instance";
					}
					else result = String::Concat("Plugin does not contain a type of ", classtype);
				}
				else result = String::Concat("Failed to load ", loadPath);
			}
			catch (Exception^ e) {
				result = String::Concat("[Exception: ", e->Message);
				if (e->InnerException)
					result = String::Concat(result, ": ", e->InnerException->Message);
				result = String::Concat(result, "]");
			}
		}

		return (char*)(void*)Marshal::StringToHGlobalAnsi(result);		
	}

	__declspec(dllexport) char* __stdcall BridgeFunc(int iBridgeId, int iFunc, const char* param1, const char* param2)
	{
		int wchars_num = MultiByteToWideChar(CP_UTF8, 0, param1, -1, NULL, 0);
		wchar_t* wstr = new wchar_t[wchars_num];
		MultiByteToWideChar(CP_UTF8, 0, param1, -1, wstr, wchars_num);
		String^ managed_param1 = gcnew String(wstr);

		wchars_num = MultiByteToWideChar(CP_UTF8, 0, param2, -1, NULL, 0);
		wstr = new wchar_t[wchars_num];
		MultiByteToWideChar(CP_UTF8, 0, param2, -1, wstr, wchars_num);
		String^ managed_param2 = gcnew String(wstr);

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

		}
		catch (Exception^ e) {
			result = String::Concat("[Exception: ", e->Message);
			if (e->InnerException)
				result = String::Concat(result, ": ", e->InnerException->Message);
			result = String::Concat(result, "]");
		}

		static char buffer[1024];
		buffer[0] = 0;
		if (result != "")
		{
			pin_ptr<const wchar_t> wch = PtrToStringChars(result);

			size_t  sizeInBytes = ((result->Length + 1) * 2);
			char* ch = (char*)malloc(sizeInBytes);

			WideCharToMultiByte(CP_UTF8, 0, wch, sizeInBytes, ch, sizeInBytes, NULL, NULL);

			strcpy_s(buffer, ch);
		}
		return buffer;
	}

	__declspec(dllexport) char* __stdcall BridgeInfoFunc(int iBridgeId)
	{
		plugin^ current = globals::plugins[iBridgeId - 1];
		if (!current->InfoFunc)
			return "";

		array <Object^>^ noargs = gcnew array<Object^>(0);

		return (char*)(void*)Marshal::StringToHGlobalAnsi(reinterpret_cast<String^>(current->InfoFunc->Invoke(current->myObject, noargs)));
	}

	__declspec(dllexport) char* __stdcall BridgeDemoFunc(int iBridgeId)
	{
		plugin^ current = globals::plugins[iBridgeId - 1];
		if (!current->DemoFunc)
			return "";

		array <Object^>^ noargs = gcnew array<Object^>(0);

		return (char*)(void*)Marshal::StringToHGlobalAnsi(reinterpret_cast<String^>(current->DemoFunc->Invoke(current->myObject, noargs)));
	}
}
