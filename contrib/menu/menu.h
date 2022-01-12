// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the MENU_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// MENU_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.
#ifdef MENU_EXPORTS
#define MENU_API __declspec(dllexport)
#else
#define MENU_API __declspec(dllimport)
#endif

// This class is exported from the menu.dll
class MENU_API Cmenu {
public:
	Cmenu(void);
	// TODO: add your methods here.
};

extern MENU_API int nmenu;

MENU_API int fnmenu(void);
