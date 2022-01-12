{********************************************************************************************************}
{                                                                                                        }
{ Carbonsoft cxCpu Processor Detection Toolkit Release 4                                                 }
{                                                                                                        }
{ Copyright © 1995-2004 Kev French, Carbonsoft. All rights reserved.                                     }
{ http://www.carbonsoft.com/cxcpu/                                                                       }
{                                                                                                        }
{ IMPORTANT INFORMATION                                                                                  }
{ This work is licensed under the Creative Commons Attribution-ShareAlike License. To view a copy        }
{ of this license, visit http://creativecommons.org/licenses/by-sa/1.0/ or send a letter to Creative     }
{ Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.                                       }
{                                                                                                        }
{ You are free:                                                                                          }
{   - to copy, distribute, display, and perform the work                                                 }
{   - to make derivative works                                                                           }
{   - to make commercial use of the work                                                                 }
{                                                                                                        }
{ Under the following conditions:                                                                        }
{   - Attribution. You must give the original author credit                                              }
{   - Share Alike. If you alter, transform, or build upon this work, you may distribute the              }
{                  resulting work only under a license identical to this one                             }
{                                                                                                        }
{ - For any reuse or distribution, you must make clear to others the license terms of this work          }
{ - Any of these conditions can be waived if you get permission from the author.                         }
{                                                                                                        }
{ For more information please contact licensing@carbonsoft.com                                           }
{                                                                                                        }
{********************************************************************************************************}
{                                                                                                        }
{ History:                                                                                               }
{ 27-Aug-2004       4.3.01       Update Release                                                          }
{ 24-Aug-2004       4.3.00       Update Release                                                          }
{ 10-Aug-2004       4.2.00       Update Release                                                          }
{ 14-Feb-2004       4.1.00       First Release                                                           }
{ 18-Sept-2003      4.0.17b      Beta 1 interim release (Public)                                         }
{ 23-Aug-2003       4.0.16b      Beta 1 release (Public)                                                 }
{ 21-May-2003       4.0.15b      Beta 0 release                                                          }
{ 25-April-2003     4.0.14b      Initial private beta release                                            }
{                                                                                                        }
{********************************************************************************************************}
unit cxCpu40;

interface

// Enables support for Alexy Dynnikov's adCpuUsage unit (Windows only)
{$DEFINE USE_ADCPUUSAGE}

// Enables alternate speed detection (may not operate correctly on Intel SpeedStep parts)
{$DEFINE USE_SLEEP}

// adCpuUsage not available under Linux
{$IFDEF LINUX}
  {$UNDEF USE_ADCPUUSAGE}
{$ENDIF LINUX}

// MSWINDOWS define not present pre Delphi 6
{$IFDEF WIN32}
  {$IFNDEF MSWINDOWS}
    {$DEFINE MSWINDOWS}
  {$ENDIF MSWINDOWS}
{$ENDIF WIN32}

uses
  Classes,
  {$IFDEF MSWINDOWS}      Windows,      {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}          Libc,         {$ENDIF LINUX}
  {$IFDEF USE_ADCPUUSAGE} AdCpuUsage,   {$ENDIF USE_ADCPUUSAGE}
  SysUtils;

//--------------------------------------------------------------------------------------------------------
// Enhanced integer & boolean types
//--------------------------------------------------------------------------------------------------------

resourcestring
  RsCpu_Format_True     = 'True';
  RsCpu_Format_False    = 'False';
  RsCpu_Format_Yes      = 'Yes';
  RsCpu_Format_No       = 'No';
  RsCpu_Format_On       = 'On';
  RsCpu_Format_Off      = 'Off';

  RsCpu_Format_Integer  = '%d';

  RsCpu_Format_Hex      = '$%1.8x';

  RsCpu_Format_Bytes    = '%d bytes';
  RsCpu_Format_KBytes   = '%1.1n Kb';
  RsCpu_Format_MBytes   = '%1.1n Mb';
  RsCpu_Format_GBytes   = '%1.1n Gb';

  RsCpu_Format_Mhz      = '%d Mhz';
  RsCpu_Format_Ghz      = '%1.1n Ghz';

  RsCpu_Format_Version  = '%d.%d.%2.2d';

type
  ICpuBoolean = interface
    ['{D34AD555-2CF5-4E92-B386-686008F03F41}']
    function AsBoolean: Boolean;

    function FormatYesNo: String;
    function FormatTrueFalse: String;
    function FormatOnOff: String;
  end;
  TCpuBoolean = class(TInterfacedObject, ICpuBoolean)
  private
    fValue: Boolean;
  public
    constructor Create(Value: Boolean);
    destructor Destroy; override;

    function AsBoolean: Boolean;

    function FormatYesNo: String;
    function FormatTrueFalse: String;
    function FormatOnOff: String;
  end;

  ICpuWord = interface
    ['{DE266303-9F21-44F8-A839-2096DB26CD9E}']
    function HiByte: Byte;
    function LoByte: Byte;

    function Value: Word;
  end;
  TCpuWord = class(TInterfacedObject, ICpuWord)
  private
    fWord: WORD;
  public
    constructor Create(Value: Word);
    destructor Destroy; override;

    function HiByte: Byte;
    function LoByte: Byte;
    function Value: Word;
  end;

  ICpuNumber = interface
    ['{A1013670-1034-4493-97F4-1E421855EB94}']
    function AsNumber: Longword;
    function AsString: String;

    function HiWord: ICpuWord;
    function LoWord: ICpuWord;

    function CountBitsOn: Byte;
    function IsBitActive(Bit: Byte): ICpuBoolean;

    function FormatBytes: String;
    function FormatHex: String;
    function FormatMhz: String;
    function FormatString: String;
    function FormatVersion: String;
  end;
  TCpuNumber = class(TInterfacedObject, ICpuNumber)
  private
    fValue: Longword;
  public
    constructor Create(Value: Longword);
    destructor Destroy; override;

    function AsNumber: Longword;
    function AsString: String;

    function HiWord: ICpuWord;
    function LoWord: ICpuWord;

    function CountBitsOn: Byte;
    function IsBitActive(Bit: Byte): ICpuBoolean;

    function FormatBytes: String;
    function FormatHex: String;
    function FormatMhz: String;
    function FormatString: String;
    function FormatVersion: String;
  end;

  TCpuNumberFormat = (cnfBytes, cnfHex, cnfMhz, cnfString, cnfVersion);

  TCpuBooleanFormat = (cbfYesNo, cbfTrueFalse, cbfOnOff);

  function CpuFormatNumber(Value: Longword; Format: TCpuNumberFormat): String;

  function CpuFormatBoolean(Value: Boolean; Format: TCpuBooleanFormat): String;

//--------------------------------------------------------------------------------------------------------
// Processor affinity functions
//--------------------------------------------------------------------------------------------------------

resourcestring
  RsCpu_Library_Kernel    = 'KERNEL32.DLL';
  RsCpu_Library_Function  = 'SetProcessAffinityMask';

type
  ICpuAffinity = interface
    ['{51581B4D-BF14-46F3-BD0A-40C19E425B9A}']
  end;
  TCpuAffinity = class(TInterfacedObject, ICpuAffinity)
  {$IFDEF MSWINDOWS}
  private
    fProcess: Longword;
    fAffinity: Longword;
    fOldAffinity: Longword;
  {$ENDIF MSWINDOWS}
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;
  end;

  {$IFDEF MSWINDOWS}
    type TcxSetProcessAffinityMask = function(hProcess: THandle; dwProcessAffinityMask: DWORD): BOOL; stdcall;
  {$ENDIF MSWINDOWS}

  function CpuSetAffinity(Cpu: Byte): ICpuAffinity;

//--------------------------------------------------------------------------------------------------------
// Cpuid execution routines
//--------------------------------------------------------------------------------------------------------

const
  cReg_Eax              = 0;
  cReg_Ebx              = 1;
  cReg_Ecx              = 2;
  cReg_Edx              = 3;

  cCpuid_EFlags         = $00200000;
  cCpuid_OpCode         = $0000A20F;

  cStd_MaximumLevel     = $00000000;
  cStd_VendorSignature  = $00000000;
  cStd_Signature        = $00000001;
  cStd_FeatureSet       = $00000001;
  cStd_CacheTlbs        = $00000002;
  cStd_SerialNumber     = $00000003;

  cExt_MaximumLevel     = $80000000;
  cExt_Signature        = $80000001;
  cExt_FeatureSet       = $80000001;
  cExt_MarketingName1   = $80000002;
  cExt_MarketingName2   = $80000003;
  cExt_MarketingName3   = $80000004;
  cExt_Level1Cache      = $80000005;
  cExt_Level2Cache      = $80000006;
  cExt_PowerManagement  = $80000007;
  cExt_AA64Information  = $80000008;
  cExt_Unsupported      = $80000099;  // Dummy command for unsuported features

  cTmx_MaximumLevel     = $80860000;
  cTmx_Signature        = $80860001;
  cTmx_SoftwareVersion  = $80860002;
  cTmx_MarketingName1   = $80860003;
  cTmx_MarketingName2   = $80860004;
  cTmx_MarketingName3   = $80860005;
  cTmx_MarketingName4   = $80860006;
  cTmx_Operation        = $80860007;

  cCmd_Standard0        = 0;

type
  TCpuidExecutionLevel = (celStandard, celExtended, celTransmeta);

  TCpuidResult = record
    Eax: Longword;
    Ebx: Longword;
    Ecx: Longword;
    Edx: Longword;
  end;

  ICpuid = interface
    ['{CA8D4D57-6BF4-4608-960D-670376601885}']
    function Cpu: Byte;
    function Command: Longword;
    function Iterations: Integer;

    function AsString: String;

    function Supported: ICpuBoolean;

    function Eax: ICpuNumber;
    function Ebx: ICpuNumber;
    function Ecx: ICpuNumber;
    function Edx: ICpuNumber;
  end;
  TCpuid = class(TInterfacedObject, ICpuid)
  private
    fCpu: Byte;
    fCommand: Longword;
    fIterations: Integer;
    fResult: TCpuidResult;
  public
    constructor Create(Cpu: Byte; Command: Longword; Iterations: Integer = 1); overload;
    constructor Create(Cpu: Byte; Command: Longword; Iterations: Integer; Result: TCpuidResult); overload;
    destructor Destroy; override;

    function Cpu: Byte;
    function Command: Longword;
    function Iterations: Integer;

    function AsString: String;

    function Supported: ICpuBoolean;

    function Eax: ICpuNumber;
    function Ebx: ICpuNumber;
    function Ecx: ICpuNumber;
    function Edx: ICpuNumber;
  end;

  function CpuIsCpuidSupported: Boolean; overload;
  function CpuIsCpuidSupported(Cpu: Byte): Boolean; overload;

  function CpuGetCommandLevel(Command: Longword): TCpuidExecutionLevel;

  function CpuGetMaximumCommand(Level: TCpuidExecutionLevel): Longword; overload;
  function CpuGetMaximumCommand(Cpu: Byte; Level: TCpuidExecutionLevel): Longword; overload;

  function CpuIsCpuidCommandSupported(Command: Longword): Boolean; overload;
  function CpuIsCpuidCommandSupported(Cpu: Byte; Command: Longword): Boolean; overload;

  function CpuGetCpuidResult: TCpuidResult; assembler; register;

  function CpuExecuteCpuid(Command: Longword; Iterations: Integer): TCpuidResult; overload;
  function CpuExecuteCpuid(Cpu: Byte; Command: Longword; Iterations: Integer): TCpuidResult; overload;

//--------------------------------------------------------------------------------------------------------
// CPUID Information Functions
//--------------------------------------------------------------------------------------------------------

type
  ICpuidInformation = interface
    ['{5E5E930B-9FCD-424C-8894-A80ED26F9C73}']
    function Available: ICpuBoolean;
    function MaximumCommand: ICpuNumber;
  end;
  TCpuidInformation = class(TInterfacedObject, ICpuidInformation)
  private
    fCpu: Byte;
    fExecutionLevel: TCpuidExecutionLevel;

    fCpuid: ICpuid;
  public
    constructor Create(Cpu: Byte; Level: TCpuidExecutionLevel);
    destructor Destroy; override;

    function Available: ICpuBoolean;
    function MaximumCommand: ICpuNumber;
  end;

  IProcessorCpuid = interface
    ['{8106B2AF-AF31-4774-9F97-1D19C1FE2CE3}']
    function Standard: ICpuidInformation;
    function Extended: ICpuidInformation;
    function Transmeta: ICpuidInformation;
  end;
  TProcessorCpuid = class(TInterfacedObject, IProcessorCpuid)
  private
    fCpu: Byte;

  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;

    function Standard: ICpuidInformation;
    function Extended: ICpuidInformation;
    function Transmeta: ICpuidInformation;
  end;

  function CpuIsCpuidAvailable(Cpu: Byte; CpuidExecutionLevel: TCpuidExecutionLevel): Boolean;
  function CpuGetCpuidMaximumCommand(Cpu: Byte; CpuidExecutionLevel: TCpuidExecutionLevel): Longword;

//--------------------------------------------------------------------------------------------------------
// Processor signature functions
//--------------------------------------------------------------------------------------------------------

const
  cType_OemRetail = 0;
  cType_OverDrive = 1;

  cFamily_486     = 4;
  cFamily_P5      = 5;
  cFamily_P6      = 6;
  cFamily_P7      = 7;

  cBrand_Unknown      = 0;
  cBrand_Celeron      = 1;
  cBrand_P3           = 2;
  cBrand_P3Xeon       = 3;  // If signature = $000006B1 then 'Intel Celeron® processor'
  cBrand_P3Alt        = 4;
  cBrand_P3Mobile     = 6;
  cBrand_CeleronM     = 7;
  cBrand_P4           = 8;  // If signature > $00000F13 then 'Intel® Geniune processor'
  cBrand_P4Alt        = 9;
  cBrand_CeleronA     = 10;
  cBrand_Xeon         = 11; // If signature < $00000F13 then 'Intel® Xeon™ processor MP'
  cBrand_XeonMP       = 12;
  cBrand_P4Mobile     = 14;
  cBrand_CeleronMAlt  = 15;

  cBrand_OpteronUP    = 3;
  cBrand_Opteron2P    = 4;
  cBrand_OpteronMP    = 5;

  cExtend_Fields      = 15;

type
  TCpuSignatureField = (csfType, csfFamily, csfModel, csfStepping, csfBrand);

  ICpuSignatureInformation = interface
    ['{89610B21-D70A-4548-BB7D-19B67849FD2B}']
    function Value: ICpuNumber;
    function Extended: ICpuBoolean;
  end;
  TCpuSignatureInformation = class(TInterfacedObject, ICpuSignatureInformation)
  private
    fValue: ICpuNumber;
    fField: TCpuSignatureField;
  public
    constructor Create(Value: Longword; Field: TCpuSignatureField);
    destructor Destroy; override;

    function Value: ICpuNumber;
    function Extended: ICpuBoolean;
  end;

  IProcessorSignature = interface
    ['{89644EBE-DFB3-4BEE-9CBD-8F7F7F73F682}']
    function CpuType: ICpuSignatureInformation;
    function Family: ICpuSignatureInformation;
    function Model: ICpuSignatureInformation;
    function Stepping: ICpuSignatureInformation;
    function Brand: ICpuSignatureInformation;
    function Generic: String;
    function Value: ICpuNumber;
  end;
  TProcessorSignature = class(TInterfacedObject, IProcessorSignature)
  private
    fCpu: Byte;

    fCpuid: ICpuid;
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;
    function CpuType: ICpuSignatureInformation;
    function Family: ICpuSignatureInformation;
    function Model: ICpuSignatureInformation;
    function Stepping: ICpuSignatureInformation;
    function Brand: ICpuSignatureInformation;
    function Generic: String;
    function Value: ICpuNumber;
  end;

  function CpuGetSignatureType(Cpu: Byte): Integer;
  function CpuGetSignatureFamily(Cpu: Byte): Integer;
  function CpuGetSignatureModel(Cpu: Byte): Integer;
  function CpuGetSignatureStepping(Cpu: Byte): Integer;
  function CpuGetSignatureBrand(Cpu: Byte): Integer;

//--------------------------------------------------------------------------------------------------------
// Processor Vendor functions
//--------------------------------------------------------------------------------------------------------

type
  TCpuVendor = (cvNone, cvUnknown, cvIntel, cvAmd, cvCyrix, cvIDT, cvNexGen, cvUMC, cvRise, cvTransmeta);

  TFeatureAvailability = (faCommon, faIntel, faAmd, faCyrix);
  TVendorCacheDetect = (vcdStandard, vcdExtended, vcdCombined);

  IProcessorVendor = interface
    ['{BDF60067-73A7-4EF6-A95F-41005D921E54}']
    function Signature: String;
    function Name: String;
    function FeatureStyle: TFeatureAvailability;
    function CacheDetect: TVendorCacheDetect;
    function VendorType: TCpuVendor;
  end;
  TProcessorVendor = class(TInterfacedObject, IProcessorVendor)
  private
    fCpu: Byte;
    fVendor: TCpuVendor;

    procedure GetVendor;
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;

    function Signature: String;
    function Name: String;
    function FeatureStyle: TFeatureAvailability;
    function CacheDetect: TVendorCacheDetect;
    function VendorType: TCpuVendor;
  end;

  TCpuVendorInfo = record
    Signature: String;
    Prefix: String;
    Name: String;
    FeatureAvailability: TFeatureAvailability;
    CacheDetect: TVendorCacheDetect;
  end;

const
  cVendorNames: array[cvUnknown..cvTransmeta] of TCpuVendorInfo = (
    (Signature: 'BadCpuVendor'; Prefix: 'Unknown ';   Name: 'Unknown Vendor';
      FeatureAvailability: faCommon; CacheDetect: vcdStandard),
    (Signature: 'GenuineIntel'; Prefix: 'Intel ';     Name: 'Intel Corporation';
      FeatureAvailability: faIntel;  CacheDetect: vcdStandard),
    (Signature: 'AuthenticAMD'; Prefix: 'AMD ';       Name: 'Advanced Micro Devices';
      FeatureAvailability: faAmd;    CacheDetect: vcdExtended),
    (Signature: 'CyrixInstead'; Prefix: 'Cyrix ';     Name: 'Via Technologies Inc';
      FeatureAvailability: faCyrix;  CacheDetect: vcdCombined),
    (Signature: 'CentaurHauls'; Prefix: 'Via ';       Name: 'Via Technologies Inc';
      FeatureAvailability: faCommon; CacheDetect: vcdExtended),
    (Signature: 'NexGenDriven'; Prefix: 'NexGen ';    Name: 'NexGen Inc';
      FeatureAvailability: faCommon; CacheDetect: vcdStandard),
    (Signature: 'UMC UMC UMC '; Prefix: 'UMC ';       Name: 'United Microelectronics Corp';
      FeatureAvailability: faCommon; CacheDetect: vcdStandard),
    (Signature: 'RiseRiseRise'; Prefix: 'Rise ';      Name: 'Rise Technology';
      FeatureAvailability: faCommon; CacheDetect: vcdStandard),
    (Signature: 'GenuineTMx86'; Prefix: 'Transmeta '; Name: 'Transmeta';
      FeatureAvailability: faAmd;    CacheDetect: vcdExtended)
  );

  function CpuGetVendor(Cpu: Byte): TCpuVendor;
  function CpuGetVendorSignature(Cpu: Byte): String;
  function CpuGetVendorName(Cpu: Byte): String;
  function CpuGetVendorFeatureStyle(Cpu: Byte): TFeatureAvailability;
  function CpuGetVendorCacheDetect(Cpu: Byte): TVendorCacheDetect;

//--------------------------------------------------------------------------------------------------------
// Processor Feature Set functions
//--------------------------------------------------------------------------------------------------------

resourcestring
  RsCpu_Feature_ResMnemonic       = 'RES';
  RsCpu_Feature_Res               = 'Reserved or unused';

  RsCpu_Feature_FPU               = 'Floating point unit';
  RsCpu_Feature_VME               = 'Virtual mode extension';
  RsCpu_Feature_DE                = 'Debugging extensions';
  RsCpu_Feature_PSE               = 'Page size extension';
  RsCpu_Feature_TSC               = 'Time stamp counter';
  RsCpu_Feature_MSR               = 'Machine specific registers';
  RsCpu_Feature_PAE               = 'Physical address exception';
  RsCpu_Feature_MCE               = 'Machine check exception';
  RsCpu_Feature_CX8               = 'CMPXCHG8 instrucion support';
  RsCpu_Feature_APIC              = 'APIC hardware support';
  RsCpu_Feature_SEP               = 'Fast system call';
  RsCpu_Feature_MTRR              = 'Memory type range registers';
  RsCpu_Feature_PGE               = 'Page global enable';
  RsCpu_Feature_MCA               = 'Machine check architecture';
  RsCpu_Feature_CMOV              = 'Conditional move support';
  RsCpu_Feature_PAT               = 'Page attribute table';
  RsCpu_Feature_PSE36             = '36-bit page size extension';
  RsCpu_Feature_PSN               = 'Processor serial number';
  RsCpu_Feature_CLFSH             = 'CLFLUSH instruction support';
  RsCpu_Feature_DS                = 'Debug store';
  RsCpu_Feature_ACPI              = 'Thermal monitor and software controlled clock';
  RsCpu_Feature_MMX               = 'MMX architecture support';
  RsCpu_Feature_MMXPLUS           = 'Extended MMX architecture';
  RsCpu_Feature_FXSR              = 'Fast floating point save';
  RsCpu_Feature_SSE               = 'Streaming SIMD instruction support';
  RsCpu_Feature_SSE2              = 'Streaming SIMD extensions 2';
  RsCpu_Feature_SS                = 'Self snoop';
  RsCpu_Feature_HTT               = 'Hyper-Threading technology';
  RsCpu_Feature_TM                = 'Thermal monitor support';
  RsCpu_Feature_3DNOW             = '3DNow! extensions';
  RsCpu_Feature_3DNOWPLUS         = 'Extended 3DNow! extensions';
  RsCpu_Feature_MP                = 'Multi processor enabled';
  RsCpu_Feature_DTES              = 'Debug trace and EMON store MSR';
  RsCpu_Feature_LM                = 'AA-64 Long mode';
  RsCpu_Feature_IA64              = 'IA-64';
  RsCpu_Feature_SBF               = 'Signal break on FERR';
  RsCpu_Feature_NX                = 'No Execute Page Protections';
  RsCpu_Feature_TS                = 'Temperature Sensor';
  RsCpu_Feature_FID               = 'Frequency Id Control';
  RsCpu_Feature_VID               = 'Voltage Id Control';
  RsCpu_Feature_TTP               = 'Thermal Trip';
  RsCpu_Feature_STC               = 'Software Thermal Control';
  RsCpu_Feature_TM2               = 'Thermal Monitor 2';
  RsCpu_Feature_EST               = 'Enhanced SpeedStep Technology';
  RsCpu_Feature_CID               = 'Context Id';

type
  TFeatureSet = (fsUnknown, fsStandard, fsStandardEx, fsExtended, fsPowerManagement);

  TFeatureDetail = record
    Index: Byte;
    Mnemonic: String;
    Name: String;
    Info: TFeatureAvailability;
    Level: set of TFeatureSet;
  end;

const
  cFeatureRegisterContent = 50;
  cMaxFeatures            = 48;
  cFeatureDetails: array[0..cMaxFeatures - 1] of TFeatureDetail = (
    (Index: 0;  Mnemonic: 'FPU';    Name: RsCpu_Feature_FPU;        Info: faCommon;   Level: [fsStandard, fsExtended]),
    (Index: 0;  Mnemonic: 'TS';     Name: RsCpu_Feature_TS;         Info: faAmd;      Level: [fsPowerManagement]),
    (Index: 1;  Mnemonic: 'VME';    Name: RsCpu_Feature_VME;        Info: faCommon;   Level: [fsStandard, fsExtended]),
    (Index: 1;  Mnemonic: 'FID';    Name: RsCpu_Feature_FID;        Info: faAmd;      Level: [fsPowerManagement]),
    (Index: 2;  Mnemonic: 'DE';     Name: RsCpu_Feature_DE;         Info: faCommon;   Level: [fsStandard, fsExtended]),
    (Index: 2;  Mnemonic: 'VID';    Name: RsCpu_Feature_VID;        Info: faAmd;      Level: [fsPowerManagement]),
    (Index: 3;  Mnemonic: 'PSE';    Name: RsCpu_Feature_PSE;        Info: faCommon;   Level: [fsStandard, fsExtended]),
    (Index: 3;  Mnemonic: 'TPP';    Name: RsCpu_Feature_TTP;        Info: faAmd;      Level: [fsPowerManagement]),
    (Index: 4;  Mnemonic: 'TSC';    Name: RsCpu_Feature_TSC;        Info: faCommon;   Level: [fsStandard, fsExtended]),
    (Index: 4;  Mnemonic: 'TM';     Name: RsCpu_Feature_TM;         Info: faAmd;      Level: [fsPowerManagement]),
    (Index: 5;  Mnemonic: 'MSR';    Name: RsCpu_Feature_MSR;        Info: faCommon;   Level: [fsStandard, fsExtended]),
    (Index: 5;  Mnemonic: 'STC';    Name: RsCpu_Feature_STC;        Info: faAmd;      Level: [fsPowerManagement]),
    (Index: 6;  Mnemonic: 'PAE';    Name: RsCpu_Feature_PAE;        Info: faCommon;   Level: [fsStandard, fsExtended]),
    (Index: 7;  Mnemonic: 'MCE';    Name: RsCpu_Feature_MCE;        Info: faCommon;   Level: [fsStandard, fsExtended]),
    (Index: 7;  Mnemonic: 'TM2';    Name: RsCpu_Feature_TM2;        Info: faCommon;   Level: [fsStandardEx]),
    (Index: 8;  Mnemonic: 'CX8';    Name: RsCpu_Feature_CX8;        Info: faCommon;   Level: [fsStandard, fsExtended]),
    (Index: 8;  Mnemonic: 'EST';    Name: RsCpu_Feature_EST;        Info: faCommon;   Level: [fsStandardEx]),
    (Index: 9;  Mnemonic: 'APIC';   Name: RsCpu_Feature_APIC;       Info: faCommon;   Level: [fsStandard, fsExtended]),
    (Index: 10; Mnemonic: 'CID';    Name: RsCpu_Feature_CID;        Info: faCommon;   Level: [fsStandardEx]),
    (Index: 11; Mnemonic: 'SEP';    Name: RsCpu_Feature_SEP;        Info: faCommon;   Level: [fsStandard, fsExtended]),
    (Index: 12; Mnemonic: 'MTRR';   Name: RsCpu_Feature_MTRR;       Info: faCommon;   Level: [fsStandard, fsExtended]),
    (Index: 13; Mnemonic: 'PGE';    Name: RsCpu_Feature_PGE;        Info: faCommon;   Level: [fsStandard, fsExtended]),
    (Index: 14; Mnemonic: 'MCA';    Name: RsCpu_Feature_MCA;        Info: faCommon;   Level: [fsStandard, fsExtended]),
    (Index: 15; Mnemonic: 'CMOV';   Name: RsCpu_Feature_CMOV;       Info: faCommon;   Level: [fsStandard, fsExtended]),
    (Index: 16; Mnemonic: 'PAT';    Name: RsCpu_Feature_PAT;        Info: faCommon;   Level: [fsStandard, fsExtended]),
    (Index: 17; Mnemonic: 'PSE36';  Name: RsCpu_Feature_PSE36;      Info: faCommon;   Level: [fsStandard]),
    (Index: 18; Mnemonic: 'PSN';    Name: RsCpu_Feature_PSN;        Info: faCommon;   Level: [fsStandard]),
    (Index: 19; Mnemonic: 'CLFSH';  Name: RsCpu_Feature_CLFSH;      Info: faCommon;   Level: [fsStandard, fsExtended]),
    (Index: 19; Mnemonic: 'MP';     Name: RsCpu_Feature_MP;         Info: faAmd;      Level: [fsExtended]),
    (Index: 20; Mnemonic: 'NX';     Name: RsCpu_Feature_NX;         Info: faAmd;      Level: [fsExtended]),
    (Index: 20; Mnemonic: 'NX';     Name: RsCpu_Feature_NX;         Info: faCyrix;    Level: [fsExtended]),
    (Index: 21; Mnemonic: 'DTES';   Name: RsCpu_Feature_DTES;       Info: faCommon;   Level: [fsStandard]),
    (Index: 22; Mnemonic: 'ACPI';   Name: RsCpu_Feature_ACPI;       Info: faCommon;   Level: [fsStandard]),
    (Index: 22; Mnemonic: 'MMX+';   Name: RsCpu_Feature_MMXPLUS;    Info: faAmd;      Level: [fsExtended]),
    (Index: 23; Mnemonic: 'MMX';    Name: RsCpu_Feature_MMX;        Info: faCommon;   Level: [fsStandard, fsExtended]),
    (Index: 24; Mnemonic: 'FXSR';   Name: RsCpu_Feature_FXSR;       Info: faCommon;   Level: [fsStandard, fsExtended]),
    (Index: 24; Mnemonic: 'MMX+';   Name: RsCpu_Feature_MMXPLUS;    Info: faCyrix;    Level: [fsExtended]),
    (Index: 25; Mnemonic: 'SSE';    Name: RsCpu_Feature_SSE;        Info: faCommon;   Level: [fsStandard]),
    (Index: 26; Mnemonic: 'SSE2';   Name: RsCpu_Feature_SSE2;       Info: faCommon;   Level: [fsStandard]),
    (Index: 27; Mnemonic: 'SS';     Name: RsCpu_Feature_SS;         Info: faCommon;   Level: [fsStandard]),
    (Index: 28; Mnemonic: 'HTT';    Name: RsCpu_Feature_HTT;        Info: faCommon;   Level: [fsStandard]),
    (Index: 29; Mnemonic: 'TM';     Name: RsCpu_Feature_TM;         Info: faCommon;   Level: [fsStandard]),
    (Index: 29; Mnemonic: 'LM';     Name: RsCpu_Feature_LM;         Info: faAmd;      Level: [fsExtended]),
    (Index: 29; Mnemonic: 'LM';     Name: RsCpu_Feature_LM;         Info: faCyrix;    Level: [fsExtended]),
    (Index: 30; Mnemonic: 'IA-64';  Name: RsCpu_Feature_IA64;       Info: faCommon;   Level: [fsStandard]),
    (Index: 30; Mnemonic: '3DNOW+'; Name: RsCpu_Feature_3DNOWPLUS;  Info: faAmd;      Level: [fsExtended]),
    (Index: 31; Mnemonic: 'SBF';    Name: RsCpu_Feature_SBF;        Info: faCommon;   Level: [fsStandard]),
    (Index: 31; Mnemonic: '3DNOW';  Name: RsCpu_Feature_3DNOW;      Info: faAmd;      Level: [fsExtended])
  );

  cFeature_Ts     = 0;
  cFeature_Fpu    = 0;
  cFeature_Vme    = 1;
  cFeature_Fid    = 1;
  cFeature_De     = 2;
  cFeature_Vid    = 2;
  cFeature_Pse    = 3;
  cFeature_Ttp    = 3;
  cFeature_Tsc    = 4;
  cFeature_TmAmd  = 4;
  cFeature_Msr    = 5;
  cFeature_Stc    = 5;
  cFeature_Pae    = 6;
  cFeature_Mce    = 7;
  cFeature_Cx8    = 8;
  cFeature_Apic   = 9;
  cFeature_SepK6  = 10;
  cFeature_Sep    = 11;
  cFeature_Mtrr   = 12;
  cFeature_Pge    = 13;
  cFeature_Mca    = 14;
  cFeature_Cmov   = 15;
  cFeature_Pat    = 16;
  cFeature_Pse36  = 17;
  cFeature_Psn    = 18;
  cFeature_Mobile = 18;
  cFeature_Mp     = 19;   // AMD Multi-processor
  cFeature_Clfsh  = 19;
  cFeature_Nx     = 20;
  cFeature_Ds     = 21;
  cFeature_Acpi   = 22;
  cFeature_MmxAmd = 22;   // AMD MMX extensions
  cFeature_Mmx    = 23;
  cFeature_Fxsr   = 24;
  cFeature_MmxVia = 24;   // Cyrix/Via MMX extensions
  cFeature_Sse    = 25;
  cFeature_Sse2   = 26;
  cFeature_Ss     = 27;
  cFeature_Htt    = 28;
  cFeature_Tm     = 29;
  cFeature_Lm     = 29;
  cFeature_Ia64   = 30;
  cFeature_3dNowX = 30;
  cFeature_3dNow  = 31;
  cFeature_Pbe    = 31;
  cFeature_Tm2    = 57;
  cFeature_Est    = 58;
  cFeature_Cid    = 60;
  cFeature_Res    = 255;

type
  IFeatureSetDefinition = interface
    ['{0B144673-3BB4-4A10-84C2-969305D9F1FD}']
    function MnemonicToBitIndex(Mnemonic: String): Byte;
    function FeatureCount: ICpuNumber;
  end;
  TFeatureSetDefinition = class(TInterfacedObject, IFeatureSetDefinition)
  private
    fCpu: Byte;
    fFeatureLevel: TFeatureSet;
    fFeatureStyle: TFeatureAvailability;
    fAvailableFeatures: array[0..31] of TFeatureDetail;
    fFeatureCount: Integer;
  protected
    procedure GetAvailableFeatures;
  public
    constructor Create(Cpu: Byte; Level: TFeatureSet);
    destructor Destroy; override;

    function MnemonicToBitIndex(Mnemonic: String): Byte;
    function BitIndexToResource(BitIndex: Byte): Byte;
    function FeatureCount: ICpuNumber;
  end;

  ICpuFeature = interface
    ['{E9839DDC-FD8D-4D79-BF89-C85FE5F54EB2}']
    function BitIndex: Byte;
    function Mnemonic: String;
    function Name: String;
    function Available: ICpuBoolean;
  end;
  TUnknownFeature = class(TInterfacedObject, ICpuFeature)
  public
    constructor Create;
    destructor Destroy; override;

    function BitIndex: Byte;
    function Mnemonic: String;
    function Name: String;
    function Available: ICpuBoolean;
  end;
  TCpuFeature = class(TInterfacedObject, ICpuFeature)
  private
    fCpu: Byte;

    fIndex: Byte;
    fFeatureSet: TFeatureSet;
  public
    constructor Create(Cpu: Byte; Index: Integer; FeatureSet: TFeatureSet);
    destructor Destroy; override;

    function BitIndex: Byte;
    function Mnemonic: String;
    function Name: String;
    function Available: ICpuBoolean;
  end;

  ICpuFeatureSet = interface
    ['{BE12A2FD-F96D-4754-BE17-90E0640E24AD}']
    function Available: ICpuBoolean;
    function Count: ICpuNumber;
    function ByIndex(Feature: Byte): ICpuFeature;
    function ByName(Mnemonic: String): ICpuFeature;
  end;
  TCpuFeatureSet = class(TInterfacedObject, ICpuFeatureSet)
  private
    fCpu: Byte;

    fSet: TFeatureSet;
    fDefinition: TFeatureSetDefinition;
  public
    constructor Create(Cpu: Byte; FeatureSet: TFeatureSet);
    destructor Destroy; override;

    function Available: ICpuBoolean;
    function Count: ICpuNumber;
    function ByIndex(Feature: Byte): ICpuFeature;
    function ByName(Mnemonic: String): ICpuFeature;
  end;

  IProcessorFeatures = interface
    ['{9984FB6A-0598-4535-A5DB-0B3E07C3F179}']
    function Standard: ICpuFeatureSet;
    function StandardEx: ICpuFeatureSet;
    function Extended: ICpuFeatureSet;
    function Power: ICpuFeatureSet;

    function ByName(Mnemonic: String): ICpuFeature;
  end;
  TProcessorFeatures = class(TInterfacedObject, IProcessorFeatures)
  private
    fCpu: Byte;

  protected
    function FeatureLevel(Mnemonic: String): TFeatureSet;
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;

    function Standard: ICpuFeatureSet;
    function StandardEx: ICpuFeatureSet;
    function Extended: ICpuFeatureSet;
    function Power: ICpuFeatureSet;

    function ByName(Mnemonic: String): ICpuFeature;
  end;

  function CpuIsFeatureSupported(Cpu: Byte; Level: TFeatureSet; Feature: Byte): Boolean; overload;
  function CpuIsFeatureSupported(Cpu: Byte; Level: TFeatureSet; Mnemonic: String): Boolean; overload;

  function CpuGetFeatureSupport(Cpu: Byte; Mnemonic: String): Boolean;
  function CpuGetFeatureIndex(Mnemonic: String): Integer;
  function CpuGetFeatureMnemonic(Index: Byte): String;
  function CpuGetFeatureName(Index: Byte): String;

//--------------------------------------------------------------------------------------------------------
// Processor serial number functions
//--------------------------------------------------------------------------------------------------------

resourcestring
  RsCpu_Serial_Format   = '%1.8x';

const
  // Pentium III Model number supporting the PSN feature
  cModel_MinPsn         = 7;

type
  IProcessorSerial = interface
    ['{32A2653B-6B5D-4337-A4DA-50ABDA435450}']
    function Available: ICpuBoolean;
    function Formatted: String;
    function Value: String;
  end;
  TProcessorSerial = class(TInterfacedObject, IProcessorSerial)
  private
    fCpu: Byte;

    fCpuid1: ICpuid;
    fCpuid2: ICpuid;

    function Nibble(Value: Longword): String;
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;

    function Available: ICpuBoolean;
    function Formatted: String;
    function Value: String;
  end;

  function CpuIsSerialSupported(Cpu: Byte): Boolean;

  function CpuGetSerialNumber(Cpu: Byte): String;
  function CpuGetSerialFormat(Cpu: Byte): String;

//--------------------------------------------------------------------------------------------------------
// Processor usage
//--------------------------------------------------------------------------------------------------------

type
  IProcessorUsage = interface
    ['{F19A23D5-5C2F-4510-830E-9D6339110F81}']
    function Value: ICpuNumber;
  end;
  TProcessorUsage = class(TInterfacedObject, IProcessorUsage)
  private
    fCpu: Byte;
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;

    function Value: ICpuNumber;
  end;

  function CpuGetUsage(Cpu: Byte): Integer;

//--------------------------------------------------------------------------------------------------------
// Processor errata
//--------------------------------------------------------------------------------------------------------

type
  IProcessorErrata = interface
    ['{AB5C614B-4D0F-4177-94FD-37930BE39048}']
    function FDivError: ICpuBoolean;
    function DuronCache: ICpuBoolean;
  end;
  TProcessorErrata = class(TInterfacedObject, IProcessorErrata)
  private
    fCpu: Byte;
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;

    function FDivError: ICpuBoolean;
    function DuronCache: ICpuBoolean;
  end;

  function CpuIsFDivBugPresent(Cpu: Byte): Boolean;
  function CpuIsDuronCacheBugPresent(Cpu: Byte): Boolean;

//--------------------------------------------------------------------------------------------------------
// Processor name class
//--------------------------------------------------------------------------------------------------------

resourcestring
  // Generic part names
  RsCpu_Generic_Name         = 'x86 Family %d Model %d Stepping %d';
  // Intel part names
  RsCpu_Intel_Generic        = 'Intel Geniune Processor';
  RsCpu_Intel_486DX          = 'Intel 80486DX';
  RsCpu_Intel_486SX          = 'Intel 80486SX';
  RsCpu_Intel_486DX2         = 'Intel 80486DX2';
  RsCpu_Intel_486DX4         = 'Intel 80486DX4';
  RsCpu_Intel_P5             = 'Intel Pentium';
  RsCpu_Intel_P5MMX          = 'Intel Pentium MMX';
  RsCpu_Intel_P5OverDrive    = 'Intel Pentium Overdrive';
  RsCpu_Intel_P5LV           = 'Intel Mobile Pentium';
  RsCpu_Intel_PPro           = 'Intel Pentium Pro';
  RsCpu_Intel_PII            = 'Intel Pentium II';
  RsCpu_Intel_PIIOverDrive   = 'Intel Pentium II Overdrive';
  RsCpu_Intel_PIIXeon        = 'Intel Pentium II Xeon';
  RsCpu_Intel_PIIMobile      = 'Intel Mobile Pentium II';
  RsCpu_Intel_Celeron        = 'Intel Celeron';
  RsCpu_Intel_MobileCeleron  = 'Intel Mobile Celeron';
  RsCpu_Intel_PIII           = 'Intel Pentium III';
  RsCpu_Intel_PIIIXeon       = 'Intel Pentium III Xeon';
  RsCpu_Intel_PIIIMobile     = 'Intel Mobile Pentium III';
  RsCpu_Intel_P4             = 'Intel Pentium 4';
  RsCpu_Intel_P4Mobile       = 'Intel Mobile Pentium 4';
  RsCpu_Intel_Xeon           = 'Intel Xeon';
  RsCpu_Intel_XeonMP         = 'Intel Xeon MP';
  // AMD part names
  RsCpu_Amd_486DX2           = 'AMD AM486DX2';
  RsCpu_Amd_486DX4           = 'AMD AM486DX4';
  RsCpu_Amd_5X86             = 'AMD AM5x86';
  RsCpu_Amd_K5               = 'AMD K5';
  RsCpu_Amd_K6               = 'AMD K6';
  RsCpu_Amd_K62              = 'AMD K6 2';
  RsCpu_Amd_K63              = 'AMD K6 III';
  // Cyrix/Via part names
  RsCpu_Cyrix_MediaGX        = 'Cyrix Media GX';
  RsCpu_Cyrix_Cx5x86         = 'Cyrix Cx5x86';
  RsCpu_Cyrix_6x86           = 'Cyrix 6x86';
  RsCpu_Cyrix_GXm            = 'Cyrix GXm';
  RsCpu_Cyrix_6x86MX         = 'Cyrix 6x86MX';
  RsCpu_Cyrix_M2             = 'Cyrix M2';
  RsCpu_Cyrix_WinChip        = 'Via Cyrix III';
  // IDT part names
  RsCpu_Idt_WinChip          = 'IDT WinChip';
  RsCpu_Idt_WinChip2         = 'IDT WinChip 2';
  RsCpu_Idt_WinChip3         = 'IDT WinChip 3';
  // NexGen part names
  RsCpu_NexGen_Nx586         = 'NexGen Nx586';
  // UMC part names
  RsCpu_Umc_U5D              = 'UMC U5D';
  RsCpu_Umc_U5S              = 'UMC U5S';
  // Rise part names
  RsCpu_Rise_mP6             = 'Rise iDragon';
  RsCpu_Rise_mP6II           = 'Rise iDragon II';
  // Transmeta part names
  RsCpu_Transmeta_Crusoe     = 'Transmeta Crusoe';

const
  cCache_Celeron  = 131072;   // 128 * 1024   (128Kb)
  cCache_Xeon     = 1048576;  // 1024 * 1024  (1Mb)

  cBrand_XeonCel  = 1713;    // $000006B1
  cBrand_IntelMp  = 3859;    // $00000F13

type
  ICpuName = interface
    ['{1DB31043-BF40-469B-8FB0-5A31975A2B1C}']
    function Name: String;
  end;
  TProcessorNameDetect = class(TInterfacedObject, ICpuName)
  private
    fCpu: Byte;
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;

    function Name: String;
  end;
  TUnknownProcessorLookup = class(TInterfacedObject, ICpuName)
  private
    fCpu: Byte;
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;

    function Name: String;
  end;
  TIntelProcessorLookup = class(TInterfacedObject, ICpuName)
  private
    fCpu: Byte;
    function GetBrandName: String;
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;

    function Name: String;
  end;
  TAmdProcessorLookup = class(TInterfacedObject, ICpuName)
  private
    fCpu: Byte;
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;

    function Name: String;
  end;
  TCyrixProcessorLookup = class(TInterfacedObject, ICpuName)
  private
    fCpu: Byte;
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;

    function Name: String;
  end;
  TIdtProcessorLookup = class(TInterfacedObject, ICpuName)
  private
    fCpu: Byte;
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;

    function Name: String;
  end;
  TNexGenProcessorLookup = class(TInterfacedObject, ICpuName)
  private
    fCpu: Byte;
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;

    function Name: String;
  end;
  TUmcProcessorLookup = class(TInterfacedObject, ICpuName)
  private
    fCpu: Byte;
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;

    function Name: String;
  end;
  TRiseProcessorLookup = class(TInterfacedObject, ICpuName)
  private
    fCpu: Byte;
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;

    function Name: String;
  end;
  TTransmetaProcessorLookup = class(TInterfacedObject, ICpuName)
  private
    fCpu: Byte;
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;

    function Name: String;
  end;

  IProcessorName = interface
    ['{56301CB6-0B6B-4E31-BFF8-0B0E0029EB5A}']
    function AsString: String;
    function FromLookup: ICpuBoolean;
  end;
  TProcessorName = class(TInterfacedObject, IProcessorName)
  private
    fCpu: Byte;
    fUseLookup: Boolean;
    fTransmetaCpu: Boolean;
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;

    function AsString: String;
    function FromLookup: ICpuBoolean;
  end;

  function CpuGetFullName(Cpu: Byte): String;
  function CpuIsNameFromLookup(Cpu: Byte): Boolean;


//--------------------------------------------------------------------------------------------------------
// Processor cache information
//--------------------------------------------------------------------------------------------------------

type
  TCacheLevel = (clLevel1Code, clLevel1Data, clLevel1Unified, clLevel2, clLevel3, clTrace);

  TCacheAssociativity = (caNone, caDirect, ca2Way, ca4Way, ca8Way, ca16Way, caFull);

  TCacheDescriptorInfo = record
    Descriptor: Byte;
    Level: TCacheLevel;
    Associativity: TCacheAssociativity;
    Size: Integer;
    LineSize: Integer;
  end;

const
  cAssociativityInfo: array[caNone..caFull] of Byte = (0, 1, 2, 4, 6, 8, 15);
  cAssociativityDescription: array[caNone..caFull] of String = ('None', 'Direct', '2-Way', '4-Way',
                                                                '8-Way', '16-Way', 'Full');

  cMaxDescriptors = 36;
  cDescriptorInfo: array[0..cMaxDescriptors - 1] of TCacheDescriptorInfo = (
    (Descriptor: $06; Level: clLevel1Code;  Associativity: ca4Way;   Size: 8;    LineSize: 32),
    (Descriptor: $08; Level: clLevel1Code;  Associativity: ca4Way;   Size: 16;   LineSize: 32),
    (Descriptor: $30; Level: clLevel1Code;  Associativity: ca8Way;   Size: 32;   LineSize: 64),

    (Descriptor: $0A; Level: clLevel1Data;  Associativity: ca2Way;   Size: 8;    LineSize: 32),
    (Descriptor: $0C; Level: clLevel1Data;  Associativity: ca4Way;   Size: 16;   LineSize: 32),
    (Descriptor: $2C; Level: clLevel1Data;  Associativity: ca8Way;   Size: 32;   LineSize: 64),
    (Descriptor: $66; Level: clLevel1Data;  Associativity: ca4Way;   Size: 8;    LineSize: 64),
    (Descriptor: $67; Level: clLevel1Data;  Associativity: ca4Way;   Size: 16;   LineSize: 64),
    (Descriptor: $68; Level: clLevel1Data;  Associativity: ca4Way;   Size: 32;   LineSize: 64),

    (Descriptor: $39; Level: clLevel2;      Associativity: ca4Way;   Size: 128;  LineSize: 64),
    (Descriptor: $3B; Level: clLevel2;      Associativity: ca2Way;   Size: 128;  LineSize: 64),
    (Descriptor: $3C; Level: clLevel2;      Associativity: ca4Way;   Size: 256;  LineSize: 64),
    (Descriptor: $40; Level: clLevel2;      Associativity: caNone;   Size: 0;    LineSize: 0),
    (Descriptor: $41; Level: clLevel2;      Associativity: ca4Way;   Size: 128;  LineSize: 32),
    (Descriptor: $42; Level: clLevel2;      Associativity: ca4Way;   Size: 256;  LineSize: 32),
    (Descriptor: $43; Level: clLevel2;      Associativity: ca4Way;   Size: 512;  LineSize: 32),
    (Descriptor: $44; Level: clLevel2;      Associativity: ca4Way;   Size: 1024; LineSize: 32),
    (Descriptor: $45; Level: clLevel2;      Associativity: ca4Way;   Size: 2048; LineSize: 32),
    (Descriptor: $79; Level: clLevel2;      Associativity: ca8Way;   Size: 128;  LineSize: 64),
    (Descriptor: $7A; Level: clLevel2;      Associativity: ca8Way;   Size: 256;  LineSize: 64),
    (Descriptor: $7B; Level: clLevel2;      Associativity: ca8Way;   Size: 512;  LineSize: 64),
    (Descriptor: $7C; Level: clLevel2;      Associativity: ca8Way;   Size: 1024; LineSize: 64),
    (Descriptor: $82; Level: clLevel2;      Associativity: ca8Way;   Size: 256;  LineSize: 32),
    (Descriptor: $83; Level: clLevel2;      Associativity: ca8Way;   Size: 512;  LineSize: 32),
    (Descriptor: $84; Level: clLevel2;      Associativity: ca8Way;   Size: 1024; LineSize: 32),
    (Descriptor: $85; Level: clLevel2;      Associativity: ca8Way;   Size: 2048; LineSize: 32),
    (Descriptor: $86; Level: clLevel2;      Associativity: ca4Way;   Size: 512;  LineSize: 64),
    (Descriptor: $87; Level: clLevel2;      Associativity: ca8Way;   Size: 1024; LineSize: 64),

    (Descriptor: $22; Level: clLevel3;      Associativity: ca4Way;   Size: 512;  LineSize: 64),
    (Descriptor: $23; Level: clLevel3;      Associativity: ca8Way;   Size: 1024; LineSize: 64),
    (Descriptor: $25; Level: clLevel3;      Associativity: ca8Way;   Size: 2048; LineSize: 64),
    (Descriptor: $29; Level: clLevel3;      Associativity: ca8Way;   Size: 4096; LineSize: 64),
    (Descriptor: $40; Level: clLevel3;      Associativity: caNone;   Size: 0;    LineSize: 0),

    (Descriptor: $70; Level: clTrace;       Associativity: ca8Way;   Size: 12;   LineSize: 0),
    (Descriptor: $71; Level: clTrace;       Associativity: ca8Way;   Size: 16;   LineSize: 0),
    (Descriptor: $72; Level: clTrace;       Associativity: ca8Way;   Size: 32;   LineSize: 0)
  );

type
  TCacheDescriptors = array[1..16] of Longword;

  TCacheSegment = (csCode, csData, csUnified);

  ICpuCacheDescriptors = interface
    ['{A582BF81-9597-43F9-90EE-35173C8476AE}']
    function ByIndex(Index: Integer): Longword;
    function ValueExists(Value: Longword): ICpuBoolean;
  end;
  TCpuCacheDescriptors = class(TInterfacedObject, ICpuCacheDescriptors)
  private
    fCpu: Byte;
    fLevel: TCacheLevel;
    fValues: TCacheDescriptors;
    function ValidDescriptor(Value: Longword): Boolean;
  protected
    procedure DecodeDescriptor(Value: ICpuNumber; Index: Integer);
    procedure SetDescriptors;
  public
    constructor Create(Cpu: Byte; Level: TCacheLevel);
    destructor Destroy; override;

    function ByIndex(Index: Integer): Longword;
    function ValueExists(Value: Longword): ICpuBoolean;
  end;

  ICpuCacheAssociativity = interface
    ['{61F2FE7E-95AC-4EE5-82CA-1BD0374EBE76}']
    function Value: ICpuNumber;
    function Name: String;
  end;
  TCpuCacheAssociativity = class(TInterfacedObject, ICpuCacheAssociativity)
  private
    fValue: TCacheAssociativity;
  public
    constructor Create(Value: TCacheAssociativity);
    destructor Destroy; override;

    function Value: ICpuNumber;
    function Name: String;
  end;

  ICpuCache = interface
    ['{D1AF660F-7BD9-4DBC-9259-DBC56E506912}']
    function Associativity: ICpuCacheAssociativity;
    function Available: ICpuBoolean;
    function LineSize: ICpuNumber;
    function Size: ICpuNumber;
  end;
  TCacheByDescriptor = class(TInterfacedObject, ICpuCache)
  private
    fCpu: Byte;
    fLevel: TCacheLevel;
    fDescriptors: ICpuCacheDescriptors;
  public
    constructor Create(Cpu: Integer; Level: TCacheLevel);
    destructor Destroy; override;

    function Associativity: ICpuCacheAssociativity;
    function Available: ICpuBoolean;
    function LineSize: ICpuNumber;
    function Size: ICpuNumber;
  end;
  TCacheByExtendedCpuid = class(TInterfacedObject, ICpuCache)
  private
    fCpu: Byte;
    fLevel: TCacheLevel;
    fCacheValue: ICpuid;
  public
    constructor Create(Cpu: Byte; Level: TCacheLevel);
    destructor Destroy; override;

    function Associativity: ICpuCacheAssociativity;
    function Available: ICpuBoolean;
    function LineSize: ICpuNumber;
    function Size: ICpuNumber;
  end;

  ICpuSegmentedCache = interface
    ['{0B7B1571-B49E-4819-BD4F-0AC858D6FA4A}']
    function Code: ICpuCache;
    function Data: ICpuCache;
    function Unified: ICpuCache;
  end;
  TCpuSegmentedCache = class(TInterfacedObject, ICpuSegmentedCache)
  private
    fCpu: Byte;
    fExtended: Boolean;
  public
    constructor Create(Cpu: Byte; Extended: Boolean);
    destructor Destroy; override;

    function Code: ICpuCache;
    function Data: ICpuCache;
    function Unified: ICpuCache;
  end;

  IProcessorCache = interface
    ['{7E0E0625-0F0B-46EA-A933-2FEAC4838D91}']
    function Level1: ICpuSegmentedCache;
    function Level2: ICpuCache;
    function Level3: ICpuCache;
    function Trace: ICpuCache;
  end;
  TProcessorCache = class(TInterfacedObject, IProcessorCache)
  private
    fCpu: Byte;
    fExtended: Boolean;
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;

    function Level1: ICpuSegmentedCache;
    function Level2: ICpuCache;
    function Level3: ICpuCache;
    function Trace: ICpuCache;
  end;

  function CpuIsCacheAvailable(Cpu: Byte; CacheLevel: TCacheLevel): Boolean;
  function CpuGetCacheAssociativity(Cpu: Byte; CacheLevel: TCacheLevel): TCacheAssociativity;
  function CpuGetCacheLineSize(Cpu: Byte; CacheLevel: TCacheLevel): Integer;
  function CpuGetCacheSize(Cpu: Byte; CacheLevel: TCacheLevel): Integer;


//--------------------------------------------------------------------------------------------------------
// Processor speed information
//--------------------------------------------------------------------------------------------------------

resourcestring
  RsCpu_Mapping_1100        = '1100+';
  RsCpu_Mapping_1200        = '1200+';
  RsCpu_Mapping_1300        = '1300+';
  RsCpu_Mapping_1400        = '1400+';
  RsCpu_Mapping_1500        = '1500+';
  RsCpu_Mapping_1600        = '1600+';
  RsCpu_Mapping_1700        = '1700+';
  RsCpu_Mapping_1800        = '1800+';
  RsCpu_Mapping_1900        = '1900+';
  RsCpu_Mapping_2000        = '2000+';
  RsCpu_Mapping_2100        = '2100+';
  RsCpu_Mapping_2200        = '2200+';
  RsCpu_Mapping_2400        = '2400+';
  RsCpu_Mapping_2500        = '2500+';
  RsCpu_Mapping_2600        = '2600+';
  RsCpu_Mapping_2700        = '2700+';
  RsCpu_Mapping_2800        = '2800+';
  RsCpu_Mapping_3000        = '3000+';
  RsCpu_Mapping_3200        = '3200+';

type
  TMappingInfo = record
    Frequency: Integer;
    Mapping: String;
  end;

const
  {$IFDEF LINUX}
    cBenchmarkDelay = 250;
  {$ENDIF LINUX}

  cMaxFrequencies   = 12;
  cCpuFrequencies: array[0..cMaxFrequencies - 1] of Integer =
                   (0, 17, 20, 33, 50, 60, 66, 67, 80, 83, 90, 100);

  cMax256Mappings = 18;
  cAmd256Mappings: array[0..cMax256Mappings-1] of TMappingInfo = (
    (Frequency: 950;  Mapping: RsCpu_Mapping_1100),
    (Frequency: 1000; Mapping: RsCpu_Mapping_1200),
    (Frequency: 1100; Mapping: RsCpu_Mapping_1300),
    (Frequency: 1200; Mapping: RsCpu_Mapping_1400),
    (Frequency: 1300; Mapping: RsCpu_Mapping_1500),
    (Frequency: 1333; Mapping: RsCpu_Mapping_1500),
    (Frequency: 1400; Mapping: RsCpu_Mapping_1600),
    (Frequency: 1467; Mapping: RsCpu_Mapping_1700),
    (Frequency: 1500; Mapping: RsCpu_Mapping_1800),
    (Frequency: 1533; Mapping: RsCpu_Mapping_1800),
    (Frequency: 1600; Mapping: RsCpu_Mapping_1900),
    (Frequency: 1667; Mapping: RsCpu_Mapping_2000),
    (Frequency: 1733; Mapping: RsCpu_Mapping_2100),
    (Frequency: 1800; Mapping: RsCpu_Mapping_2200),
    (Frequency: 2000; Mapping: RsCpu_Mapping_2400),
    (Frequency: 2083; Mapping: RsCpu_Mapping_2600),
    (Frequency: 2167; Mapping: RsCpu_Mapping_2700),
    (Frequency: 2250; Mapping: RsCpu_Mapping_2800)
  );

  cMax512Mappings = 16;
  cAmd512Mappings: array[0..cMax512Mappings-1] of TMappingInfo = (
    (Frequency: 1300; Mapping: RsCpu_Mapping_1700),
    (Frequency: 1400; Mapping: RsCpu_Mapping_1800),
    (Frequency: 1467; Mapping: RsCpu_Mapping_1900),
    (Frequency: 1533; Mapping: RsCpu_Mapping_2000),
    (Frequency: 1600; Mapping: RsCpu_Mapping_2100),
    (Frequency: 1667; Mapping: RsCpu_Mapping_2200),
    (Frequency: 1800; Mapping: RsCpu_Mapping_2400),
    (Frequency: 1833; Mapping: RsCpu_Mapping_2500),
    (Frequency: 1867; Mapping: RsCpu_Mapping_2500),
    (Frequency: 1917; Mapping: RsCpu_Mapping_2600),
    (Frequency: 2000; Mapping: RsCpu_Mapping_2600),
    (Frequency: 2083; Mapping: RsCpu_Mapping_2800),
    (Frequency: 2100; Mapping: RsCpu_Mapping_3000),
    (Frequency: 2133; Mapping: RsCpu_Mapping_2800),
    (Frequency: 2167; Mapping: RsCpu_Mapping_3000),
    (Frequency: 2200; Mapping: RsCpu_Mapping_3200)
  );

type
  TCpuSpeedThread = class(TThread)
  private
    fCpuSpeed: Double;
    function GetCpuSpeed: Longword;
    {$IFDEF MSWINDOWS}
      function RdTsc: Int64;
    {$ENDIF MSWINDOWS}
  protected
    function GetProcessorSpeed: Double;
    procedure Execute; override;
  public
    constructor Create;
    property CpuSpeed: Longword read GetCpuSpeed;
  end;

  IProcessorSpeed = interface
    ['{E7579AF4-7BC0-434F-BC3F-E521D8A28D79}']
    function RawSpeed: ICpuNumber;
    function Normalised: ICpuNumber;
    function Mapping: String;
  end;
  TProcessorSpeedInformation = class(TInterfacedObject, IProcessorSpeed)
  private
    fCpu: Byte;
    fRawSpeed: Longword;
    fNormalisedSpeed: Longword;
    procedure GetCpuSpeed;
    function NormaliseSpeed(Value: Longword): Longword;
  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;
    function RawSpeed: ICpuNumber;
    function Normalised: ICpuNumber;
    function Mapping: String;
  end;

  function CpuGetRawSpeed(Cpu: Byte): Integer;
  function CpuGetNormalisedSpeed(Cpu: Byte): Integer;
  function CpuGetAmdMapping(Cpu: Byte): String;

//--------------------------------------------------------------------------------------------------------
// Processor count
//--------------------------------------------------------------------------------------------------------

const
  cHyperthread_Logical  = $00FF0000;

type
  IProcessorCount = interface
    ['{91847D4E-35C0-4F76-B116-E908E692780B}']
    function Available: ICpuNumber;
    function Logical: ICpuNumber;
  end;
  TProcessorCount = class(TInterfacedObject, IProcessorCount)
  private
    function SupportsHyperThreading(Cpu: Byte): Boolean;
    function LogicalProcessorCount(Cpu: Byte): Byte;
  public
    constructor Create;
    destructor Destroy; override;
    function Available: ICpuNumber;
    function Logical: ICpuNumber;
  end;

  function CpuGetProcessorCount: Byte;
  function CpuGetVirtualCount: Byte;

//--------------------------------------------------------------------------------------------------------
// Processor information
//--------------------------------------------------------------------------------------------------------

type
  IProcessorInformation = interface
    ['{4BCBA360-5FE4-4EDC-A027-ED0FDED6C598}']
    function Cache: IProcessorCache;
    function Cpuid: IProcessorCpuid;
    function Errata: IProcessorErrata;
    function Features: IProcessorFeatures;
    function Name: IProcessorName;
    function Serial: IProcessorSerial;
    function Signature: IProcessorSignature;
    function Speed: IProcessorSpeed;
    function Usage: IProcessorUsage;
    function Vendor: IProcessorVendor;
  end;
  TProcessorInformation = class(TInterfacedObject, IProcessorInformation)
  private
    fCpu: Byte;

  public
    constructor Create(Cpu: Byte);
    destructor Destroy; override;

    function Cache: IProcessorCache;
    function Cpuid: IProcessorCpuid;
    function Errata: IProcessorErrata;
    function Features: IProcessorFeatures;
    function Name: IProcessorName;
    function Serial: IProcessorSerial;
    function Signature: IProcessorSignature;
    function Speed: IProcessorSpeed;
    function Usage: IProcessorUsage;
    function Vendor: IProcessorVendor;

    property Cpu: Byte read fCpu;
  end;

//--------------------------------------------------------------------------------------------------------
// cxCpu base class
//--------------------------------------------------------------------------------------------------------

type
  TCachedCpuidResult = record
    Iterations: Integer;
    Result: TCpuidResult;
  end;

  TcxCpu = class
  private
    fSmp: Boolean;

    function GetProcessor(Value: Integer): IProcessorInformation;
  public
    constructor Create;
    destructor Destroy; override;

    function ProcessorCount: IProcessorCount;
    function Version: ICpuNumber;

    // Result cache (4.2)

    property Processors[Index: Integer]: IProcessorInformation read GetProcessor; default;
    property SmpActive: Boolean read fSmp;
  end;

  procedure SetDynSetProcessAffinity;
  function CpuGetToolkitVersion: String;

const
  cToolkit_Version = 4301;

var
  {$IFDEF MSWINDOWS}
    DynSetProcessAffinity: TcxSetProcessAffinityMask;
  {$ENDIF MSWINDOWS}

  CpuidCommand: Longword;
  fcxCpu: TcxCpu;

// Singleton function (r4.2)
function cxCpu: TcxCpu;

implementation

{ TCpuWord }

constructor TCpuWord.Create(Value: Word);
begin
  inherited Create;

  fWord := Value;
end;

destructor TCpuWord.Destroy;
begin
  inherited;
end;

function TCpuWord.HiByte: Byte;
begin
  Result := fWord shr 8;
end;

function TCpuWord.LoByte: Byte;
begin
  Result := Byte(fWord);
end;

function TCpuWord.Value: Word;
begin
  Result := fWord;
end;

{ TCpuNumber }

function TCpuNumber.AsNumber: Longword;
begin
  Result := fValue;
end;

function TCpuNumber.AsString: String;
begin
  Result := IntToStr(fValue);
end;

function TCpuNumber.CountBitsOn: Byte;
var
  i: Integer;
  c: Byte;
begin
  c := 0;
  for i := 0 to 31 do
    if IsBitActive(i).AsBoolean then
      inc(c);
  Result := c;
end;

constructor TCpuNumber.Create(Value: Longword);
begin
  inherited Create;

  fValue := Value;
end;

destructor TCpuNumber.Destroy;
begin
  inherited;
end;

function TCpuNumber.FormatBytes: String;
const
  Kbyte = 1024;
  MByte = 1048576;
  GByte = 1073741824;
begin
  case fValue of
    0..KByte - 1:     Result := Format(RsCpu_Format_Bytes, [fValue]);
    KByte..MByte - 1: Result := Format(RsCpu_Format_KBytes, [fValue / KByte]);
    MByte..GByte - 1: Result := Format(RsCpu_Format_MBytes, [fValue / MByte]);
  else
    Result := Format(RsCpu_Format_GBytes, [fValue / GByte]);
  end;
end;

function TCpuNumber.FormatHex: String;
begin
  Result := Format(RsCpu_Format_Hex, [fValue]);
end;

function TCpuNumber.FormatMhz: String;
const
  Mhz   = 1000;
begin
  case fValue of
    0..Mhz - 1:   Result := Format(RsCpu_Format_Mhz, [fValue]);
  else
    Result := Format(RsCpu_Format_GHz, [fValue / Mhz]);
  end;
end;

function TCpuNumber.FormatString: String;
begin
  Result := Char(LoWord.LoByte) + Char(LoWord.HiByte) + Char(HiWord.LoByte) + Char(HiWord.HiByte);
end;

function TCpuNumber.FormatVersion: String;
begin
  Result := Format(RsCpu_Format_Version, [(fValue div 1000),                              // Major
                                          ((fValue - ((fValue div 1000)*1000)) div 100) , // Minor
                                          (fValue mod 100)]);                             // Build
end;

function TCpuNumber.HiWord: ICpuWord;
begin
  Result := TCpuWord.Create(fValue shr 16);
end;

function TCpuNumber.IsBitActive(Bit: Byte): ICpuBoolean;
begin
  Result := TCpuBoolean.Create((fValue and (1 shl Bit)) <> 0);
end;

function TCpuNumber.LoWord: ICpuWord;
begin
  Result := TCpuWord.Create(Word(fValue));
end;

{ TCpuBoolean }

function TCpuBoolean.AsBoolean: Boolean;
begin
  Result := fValue;
end;

constructor TCpuBoolean.Create(Value: Boolean);
begin
  inherited Create;

  fValue := Value;
end;

destructor TCpuBoolean.Destroy;
begin
  inherited;
end;

function TCpuBoolean.FormatTrueFalse: String;
const
  cBoolean_Results: array[False..True] of String = (RsCpu_Format_False, RsCpu_Format_True);
begin
  Result := cBoolean_Results[fValue];
end;

function TCpuBoolean.FormatYesNo: String;
const
  cBoolean_Results: array[False..True] of String = (RsCpu_Format_No, RsCpu_Format_Yes);
begin
  Result := cBoolean_Results[fValue];
end;

function TCpuBoolean.FormatOnOff: String;
const
  cBoolean_Results: array[False..True] of String = (RsCpu_Format_Off, RsCpu_Format_On);
begin
  Result := cBoolean_Results[fValue];
end;

function CpuFormatNumber(Value: Longword; Format: TCpuNumberFormat): String;
var
  CpuNumber: ICpuNumber;
begin
  CpuNumber := TCpuNumber.Create(Value);
  case Format of
    cnfBytes:     Result := CpuNumber.FormatBytes;
    cnfHex:       Result := CpuNumber.FormatHex;
    cnfMhz:       Result := CpuNumber.FormatMhz;
    cnfString:    Result := CpuNumber.FormatString;
    cnfVersion:   Result := CpuNumber.FormatVersion;
  end;
end;

function CpuFormatBoolean(Value: Boolean; Format: TCpuBooleanFormat): String;
var
  CpuBoolean: ICpuBoolean;
begin
  CpuBoolean := TCpuBoolean.Create(Value);
  case Format of
    cbfYesNo:     Result := CpuBoolean.FormatYesNo;
    cbfTrueFalse: Result := CpuBoolean.FormatTrueFalse;
    cbfOnOff:     Result := CpuBoolean.FormatOnOff;
  end;
end;

{ TProcessorAffinity }

constructor TCpuAffinity.Create(Cpu: Byte);
{$IFDEF MSWINDOWS}
  var
    SystemAffinity: Longword;
{$ENDIF MSWINDOWS}
begin
  inherited Create;

  {$IFDEF MSWINDOWS}
    if cxCpu.SmpActive then
      if Cpu in [0..31] then
      begin
        fProcess := GetCurrentProcess;
        GetProcessAffinityMask(fProcess, fOldAffinity, SystemAffinity);

        if Cpu > cxCpu.ProcessorCount.Available.AsNumber - 1 then
          Cpu := 0;

        fAffinity := 1 shl Cpu;
        if Assigned(DynSetProcessAffinity) then
          DynSetProcessAffinity(fProcess, fAffinity);
        Sleep(0);
      end;
  {$ENDIF MSWINDOWS}
end;

destructor TCpuAffinity.Destroy;
begin
  {$IFDEF MSWINDOWS}
    if Assigned(DynSetProcessAffinity) then
      DynSetProcessAffinity(fProcess, fOldAffinity);
  {$ENDIF}
  inherited;
end;

function CpuSetAffinity(Cpu: Byte): ICpuAffinity;
begin
  Result := TCpuAffinity.Create(Cpu);
end;

{ TCpuid }

function TCpuid.AsString: String;
begin
  Result := Eax.FormatString + Ebx.FormatString + Ecx.FormatString + Edx.FormatString;
end;

function TCpuid.Command: Longword;
begin
  Result := fCommand;
end;

function TCpuid.Cpu: Byte;
begin
  Result := fCpu;
end;

constructor TCpuid.Create(Cpu: Byte; Command: Longword; Iterations: Integer);
var
  Affinity: ICpuAffinity;
begin
  // Add cache controller code here
  
  inherited Create;

  fCpu := Cpu;
  fCommand := Command;
  fIterations := Iterations;

  if cxCpu.SmpActive then
    Affinity := TCpuAffinity.Create(Cpu);
  if Supported.AsBoolean then
    fResult := CpuExecuteCpuid(fCommand, Iterations)
  else
  begin
    fResult.Eax := 0;
    fResult.Ebx := 0;
    fResult.Ecx := 0;
    fResult.Edx := 0;
  end;
end;

constructor TCpuid.Create(Cpu: Byte; Command: Longword;
  Iterations: Integer; Result: TCpuidResult);
begin
  inherited Create;

  fCpu := Cpu;
  fCommand := Command;
  fIterations := Iterations;
  fResult := Result;
end;

destructor TCpuid.Destroy;
begin
  inherited;
end;

function TCpuid.Eax: ICpuNumber;
begin
  Result := TCpuNumber.Create(fResult.Eax);
end;

function TCpuid.Ebx: ICpuNumber;
begin
  Result := TCpuNumber.Create(fResult.Ebx);
end;

function TCpuid.Ecx: ICpuNumber;
begin
  Result := TCpuNumber.Create(fResult.Ecx);
end;

function TCpuid.Edx: ICpuNumber;
begin
  Result := TCpuNumber.Create(fResult.Edx);
end;

function TCpuid.Iterations: Integer;
begin
  Result := fIterations;
end;

function TCpuid.Supported: ICpuBoolean;
begin
  Result := TCpuBoolean.Create(CpuIsCpuidCommandSupported(fCpu, fCommand));
end;

function CpuIsCpuidSupported: Boolean; overload;
asm
    PUSHFD
    POP       EAX
    MOV       EDX, EAX
    XOR       EAX, cCpuid_EFlags
    PUSH      EAX
    POPFD
    PUSHFD
    POP       EAX
    XOR       EAX, EDX
    JZ        @exit
    MOV       AL, TRUE
   @exit:
end;

function CpuIsCpuidSupported(Cpu: Byte): Boolean; overload;
var
  Affinity: ICpuAffinity;
begin
  if cxCpu.SmpActive then
    Affinity := TCpuAffinity.Create(Cpu);
  Result := CpuIsCpuidSupported;
end;

function CpuGetCommandLevel(Command: Longword): TCpuidExecutionLevel;
begin
  case Command of
    cStd_MaximumLevel..cStd_SerialNumber    : Result := celStandard;
    cExt_MaximumLevel..cExt_AA64Information : Result := celExtended;
    cTmx_MaximumLevel..cTmx_Operation       : Result := celTransmeta;
  else
    Result := celStandard;
  end;
end;

function CpuGetMaximumCommand(Level: TCpuidExecutionLevel): Longword; overload;
begin
  case Level of
    celStandard:  Result := CpuExecuteCpuid(cStd_MaximumLevel, 1).Eax;
    celExtended:  Result := CpuExecuteCpuid(cExt_MaximumLevel, 1).Eax;
    celTransmeta: Result := CpuExecuteCpuid(cTmx_MaximumLevel, 1).Eax;
  else
    Result := 0;
  end;
end;

function CpuGetMaximumCommand(Cpu: Byte; Level: TCpuidExecutionLevel): Longword; overload;
var
  Affinity: ICpuAffinity;
begin
  if cxCpu.SmpActive then
    Affinity := TCpuAffinity.Create(Cpu);
  Result := CpuGetMaximumCommand(Level);
end;

function CpuIsCpuidCommandSupported(Command: Longword): Boolean; overload;
begin
  Result := Command <= CpuGetMaximumCommand(CpuGetCommandLevel(Command));
end;

function CpuIsCpuidCommandSupported(Cpu: Byte; Command: Longword): Boolean; overload;
var
  Affinity: ICpuAffinity;
begin
  if cxCpu.SmpActive then
    Affinity := TCpuAffinity.Create(Cpu);
  Result := CpuIsCpuidCommandSupported(Command);
end;

function CpuGetCpuidResult: TCpuidResult; assembler; register;
asm
    PUSH      EBX
    PUSH      EDI
    MOV       EDI, EAX
    MOV       EAX, CpuidCommand
    DW        cCpuid_OpCode
    STOSD
    MOV       EAX, EBX
    STOSD
    MOV       EAX, ECX
    STOSD
    MOV       EAX, EDX
    STOSD
    POP       EDI
    POP       EBX
end;

function CpuExecuteCpuid(Command: Longword; Iterations: Integer): TCpuidResult; overload;
var
  i: Integer;
begin
  CpuidCommand := Command;
  for i := 0 to Iterations - 1 do
    Result := CpuGetCpuidResult;
end;

function CpuExecuteCpuid(Cpu: Byte; Command: Longword; Iterations: Integer): TCpuidResult; overload;
var
  Affinity: ICpuAffinity;
begin
  if cxCpu.SmpActive then
    Affinity := TCpuAffinity.Create(Cpu);
  Result := CpuExecuteCpuid(Command, Iterations);
end;

{ TCpuidInformation }

function TCpuidInformation.Available: ICpuBoolean;
begin
  Result := TCpuBoolean.Create(fCpuid.Supported.AsBoolean);
end;

constructor TCpuidInformation.Create(Cpu: Byte;
  Level: TCpuidExecutionLevel);
begin
  inherited Create;

  fCpu := Cpu;
  fExecutionLevel := Level;

  case fExecutionLevel of
    celStandard:  fCpuid := TCpuid.Create(fCpu, cStd_MaximumLevel);
    celExtended:  fCpuid := TCpuid.Create(fCpu, cExt_MaximumLevel);
    celTransmeta: fCpuid := TCpuid.Create(fCpu, cTmx_MaximumLevel);
  else
    fCpuid := TCpuid.Create(fCpu, cStd_MaximumLevel);
  end;
end;

destructor TCpuidInformation.Destroy;
begin
  inherited;
end;

function TCpuidInformation.MaximumCommand: ICpuNumber;
begin
  Result := TCpuNumber.Create(fCpuid.Eax.AsNumber);
end;

{ TProcessorCpuidInformation }

constructor TProcessorCpuid.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;
end;

destructor TProcessorCpuid.Destroy;
begin
  inherited;
end;

function TProcessorCpuid.Extended: ICpuidInformation;
begin
  Result := TCpuidInformation.Create(fCpu, celExtended);
end;

function TProcessorCpuid.Standard: ICpuidInformation;
begin
  Result := TCpuidInformation.Create(fCpu, celStandard);
end;

function TProcessorCpuid.Transmeta: ICpuidInformation;
begin
  Result := TCpuidInformation.Create(fCpu, celTransmeta);
end;

function CpuIsCpuidAvailable(Cpu: Byte; CpuidExecutionLevel: TCpuidExecutionLevel): Boolean;
var
  CpuidInformation: ICpuidInformation;
begin
  CpuidInformation := TCpuidInformation.Create(Cpu, CpuidExecutionLevel);
  Result := CpuidInformation.Available.AsBoolean;
end;

function CpuGetCpuidMaximumCommand(Cpu: Byte; CpuidExecutionLevel: TCpuidExecutionLevel): Longword;
var
  CpuidInformation: ICpuidInformation;
begin
  CpuidInformation := TCpuidInformation.Create(Cpu, CpuidExecutionLevel);
  Result := CpuidInformation.MaximumCommand.AsNumber;
end;

{ TCpuSignatureInformation }

constructor TCpuSignatureInformation.Create(Value: Longword; Field: TCpuSignatureField);
begin
  inherited Create;

  fValue := TCpuNumber.Create(Value);
  fField := Field;
end;

destructor TCpuSignatureInformation.Destroy;
begin
  inherited;
end;

function TCpuSignatureInformation.Extended: ICpuBoolean;
begin
  case fField of
    csfFamily:    Result := TCpuBoolean.Create((fValue.AsNumber shr 8 and $F) >= $F);
    csfModel:     Result := TCpuBoolean.Create((fValue.AsNumber shr 4 and $F) >= $F);
  else
    Result := TCpuBoolean.Create(False);
  end;
end;

function TCpuSignatureInformation.Value: ICpuNumber;
begin
  case fField of
    csfType:      Result := TCpuNumber.Create(fValue.AsNumber shr 12 and 3);
    csfFamily:    if Extended.AsBoolean then
                    Result := TCpuNumber.Create((fValue.AsNumber shr 20 and $FF) + (fValue.AsNumber shr 8 and $F))
                  else
                    Result := TCpuNumber.Create(fValue.AsNumber shr 8 and $F);
    csfModel:     if Extended.AsBoolean then
                    Result := TCpuNumber.Create((fValue.AsNumber shr 16 and $F) + (fValue.AsNumber shr 4 and $F))
                  else
                    Result := TCpuNumber.Create(fValue.AsNumber shr 4 and $F);
    csfStepping:  Result := TCpuNumber.Create(fValue.AsNumber and $F);
    csfBrand:     Result := TCpuNumber.Create(fValue.AsNumber);
  else
    Result := TCpuNumber.Create(0);
  end;
end;

{ TProcessorSignature }

function TProcessorSignature.Brand: ICpuSignatureInformation;

  function IsAmdOpteron: Boolean;
  begin
    Result := (cxCpu[fCpu].Vendor.VendorType = cvAmd) and
              (Family.Value.AsNumber = 15) and
              (Model.Value.AsNumber > 4);
  end;

var
  OpteronCpuid: ICpuid;
begin
  if IsAmdOpteron then
  begin
    if fCpuid.Ebx.AsNumber = 0 then
    begin
      OpteronCpuid := TCpuid.Create(fCpu, cExt_Signature);
      Result := TCpuSignatureInformation.Create(OpteronCpuid.Ebx.AsNumber, csfBrand);
    end
    else
      Result := TCpuSignatureInformation.Create(fCpuid.Ebx.Loword.LoByte, csfBrand);
  end
  else
    Result := TCpuSignatureInformation.Create(fCpuid.Ebx.Loword.LoByte, csfBrand);
end;

function TProcessorSignature.CpuType: ICpuSignatureInformation;
begin
  Result := TCpuSignatureInformation.Create(fCpuid.Eax.AsNumber, csfType);
end;

constructor TProcessorSignature.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;
  fCpuid := TCpuid.Create(fCpu, cStd_Signature);
end;

destructor TProcessorSignature.Destroy;
begin
  inherited;
end;

function TProcessorSignature.Family: ICpuSignatureInformation;
begin
  Result := TCpuSignatureInformation.Create(fCpuid.Eax.AsNumber, csfFamily);
end;

function TProcessorSignature.Generic: String;
begin
  Result := Format(RsCpu_Generic_Name, [Family.Value.AsNumber, Model.Value.AsNumber, Stepping.Value.AsNumber]);
end;

function TProcessorSignature.Model: ICpuSignatureInformation;
begin
  Result := TCpuSignatureInformation.Create(fCpuid.Eax.AsNumber, csfModel);
end;

function TProcessorSignature.Stepping: ICpuSignatureInformation;
begin
  Result := TCpuSignatureInformation.Create(fCpuid.Eax.AsNumber, csfStepping);
end;

function TProcessorSignature.Value: ICpuNumber;
begin
  Result := TCpuNumber.Create(fCpuid.Eax.AsNumber);
end;

function CpuGetSignatureType(Cpu: Byte): Integer;
begin
  Result := cxCpu[Cpu].Signature.CpuType.Value.AsNumber;
end;

function CpuGetSignatureFamily(Cpu: Byte): Integer;
begin
  Result := cxCpu[Cpu].Signature.Family.Value.AsNumber;
end;

function CpuGetSignatureModel(Cpu: Byte): Integer;
begin
  Result := cxCpu[Cpu].Signature.Model.Value.AsNumber;
end;

function CpuGetSignatureStepping(Cpu: Byte): Integer;
begin
  Result := cxCpu[Cpu].Signature.Stepping.Value.AsNumber;
end;

function CpuGetSignatureBrand(Cpu: Byte): Integer;
begin
  Result := cxCpu[Cpu].Signature.Brand.Value.AsNumber;
end;

{ TProcessorVendor }

function TProcessorVendor.CacheDetect: TVendorCacheDetect;
begin
  Result := cVendorNames[fVendor].CacheDetect;
end;

constructor TProcessorVendor.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;
  GetVendor;
end;

destructor TProcessorVendor.Destroy;
begin
  inherited;
end;

function TProcessorVendor.FeatureStyle: TFeatureAvailability;
begin
  Result := cVendorNames[fVendor].FeatureAvailability;
end;

procedure TProcessorVendor.GetVendor;
var
  i: TCpuVendor;
  Cpuid: ICpuid;
  Vid: String;
begin
  Cpuid := TCpuid.Create(fCpu, cStd_VendorSignature);
  Vid := Cpuid.Ebx.FormatString + Cpuid.Edx.FormatString + Cpuid.Ecx.FormatString;

  fVendor := cvUnknown;
  for i := cvUnknown to cvTransmeta do
    if cVendorNames[i].Signature = Vid then
    begin
      fVendor := i;
      Exit;
    end;
end;

function TProcessorVendor.Name: String;
begin
  Result := cVendorNames[fVendor].Name;
end;

function TProcessorVendor.Signature: String;
begin
  Result := cVendorNames[fVendor].Signature;
end;

function TProcessorVendor.VendorType: TCpuVendor;
begin
  Result := fVendor;
end;

function CpuGetVendor(Cpu: Byte): TCpuVendor;
begin
  Result := cxCpu[Cpu].Vendor.VendorType;
end;

function CpuGetVendorSignature(Cpu: Byte): String;
begin
  Result := cxCpu[Cpu].Vendor.Signature;
end;

function CpuGetVendorName(Cpu: Byte): String;
begin
  Result := cxCpu[Cpu].Vendor.Name;
end;

function CpuGetVendorFeatureStyle(Cpu: Byte): TFeatureAvailability;
begin
  Result := cxCpu[Cpu].Vendor.FeatureStyle;
end;

function CpuGetVendorCacheDetect(Cpu: Byte): TVendorCacheDetect;
begin
  Result := cxCpu[Cpu].Vendor.CacheDetect;
end;

{ TFeatureSetDefinition }

function TFeatureSetDefinition.BitIndexToResource(BitIndex: Byte): Byte;
begin
  Result := fAvailableFeatures[BitIndex].Index;
end;

constructor TFeatureSetDefinition.Create(Cpu: Byte; Level: TFeatureSet);
begin
  inherited Create;

  fCpu := Cpu;
  fFeatureLevel := Level;
  fFeatureStyle := cxCpu[fCpu].Vendor.FeatureStyle;
  fFeatureCount := 0;
  GetAvailableFeatures;
end;

destructor TFeatureSetDefinition.Destroy;
begin
  inherited;
end;

function TFeatureSetDefinition.FeatureCount: ICpuNumber;
begin
  Result := TCpuNumber.Create(fFeatureCount);
end;

procedure TFeatureSetDefinition.GetAvailableFeatures;
var
  i: Byte;
begin
  // Predefine available features
  for i := Low(fAvailableFeatures) to High(fAvailableFeatures) do
    fAvailableFeatures[i].Index := 255;

  // Get common features
  for i := 0 to cMaxFeatures - 1 do
    if (cFeatureDetails[i].Info = faCommon) and (fFeatureLevel in cFeatureDetails[i].Level) then
    begin
      fAvailableFeatures[cFeatureDetails[i].Index] := cFeatureDetails[i];
      fAvailableFeatures[cFeatureDetails[i].Index].Index := i;
    end;

  // Overwrite vendor specific features
  if fFeatureStyle <> faCommon then
    for i := 0 to cMaxFeatures - 1 do
      if (cFeatureDetails[i].Info = fFeatureStyle) and (fFeatureLevel in cFeatureDetails[i].Level) then
      begin
        fAvailableFeatures[cFeatureDetails[i].Index] := cFeatureDetails[i];
        fAvailableFeatures[cFeatureDetails[i].Index].Index := i;
      end;

  // Count available features
  for i := Low(fAvailableFeatures) to High(fAvailableFeatures) do
    if fAvailableFeatures[i].Index < 255 then
      inc(fFeatureCount);
end;

function TFeatureSetDefinition.MnemonicToBitIndex(Mnemonic: String): Byte;
var
  i: Byte;
  Pref: TFeatureAvailability;
begin
  Result := 255;
  Pref := cxCpu[fCpu].Vendor.FeatureStyle;

  if Pref in [faAmd, faCyrix] then
  begin
    // Amd and Cyrix/Via use different bits for extended MMX
    if UpperCase(Mnemonic) = 'MMX+' then
    begin
      if cxCpu[fCpu].Vendor.VendorType = cvAmd then
        Result := cFeature_MmxAmd;
      if cxCpu[fCpu].Vendor.VendorType = cvCyrix then
        Result := cFeature_MmxVia;
    end;

    // Amd K5 (model 0) use bit 9 (APIC) to report PGE support
    if UpperCase(Mnemonic) = 'PGE' then
      if (cxCpu[fCpu].Vendor.VendorType = cvAmd) and
         (cxCpu[fCpu].Signature.Family.Value.AsNumber = 5) and
         (cxCpu[fCpu].Signature.Model.Value.AsNumber = 0) then
        Result := cFeature_Apic;

    // Amd K6 use reserved bit 10 to report SEP support
    if UpperCase(Mnemonic) = 'SEP' then
      if (cxCpu[fCpu].Vendor.VendorType = cvAmd) and (cxCpu[fCpu].Signature.Family.Value.AsNumber = 6) then
        Result := cFeature_SepK6;
  end;

  if Result > cMaxFeatures then
    for i := Low(fAvailableFeatures) to High(fAvailableFeatures) do
      if fAvailableFeatures[i].Mnemonic = UpperCase(Mnemonic) then
      begin
        Result := i;//fAvailableFeatures[i].Index;
        Break;
      end;
end;

{ TUnknownFeature }

function TUnknownFeature.Available: ICpuBoolean;
begin
  Result := TCpuBoolean.Create(False);
end;

function TUnknownFeature.BitIndex: Byte;
begin
  Result := cFeature_Res;
end;

constructor TUnknownFeature.Create;
begin
  inherited Create;
end;

destructor TUnknownFeature.Destroy;
begin
  inherited;
end;

function TUnknownFeature.Mnemonic: String;
begin
  Result := RsCpu_Feature_ResMnemonic;
end;

function TUnknownFeature.Name: String;
begin
  Result := RsCpu_Feature_Res;
end;

{ TCpuFeature }

function TCpuFeature.Available: ICpuBoolean;
var
  Cpuid: ICpuid;
begin
  case fFeatureSet of
    fsExtended:
      Cpuid := TCpuid.Create(fCpu, cExt_FeatureSet);
    fsPowerManagement:
      Cpuid := TCpuid.Create(fCpu, cExt_PowerManagement);
  else
    Cpuid := TCpuid.Create(fCpu, cStd_FeatureSet);
  end;

  if Cpuid.Supported.AsBoolean then
  begin
    if fFeatureSet <> fsStandardEx then
      Result := TCpuBoolean.Create((Cpuid.Edx.AsNumber and (1 shl BitIndex)) <> 0)
    else
      Result := TCpuBoolean.Create((Cpuid.Ecx.AsNumber and (1 shl BitIndex)) <> 0);
  end
  else
    Result := TCpuBoolean.Create(False);
end;

function TCpuFeature.BitIndex: Byte;
begin
  Result := cFeatureDetails[fIndex].Index
end;

constructor TCpuFeature.Create(Cpu: Byte; Index: Integer; FeatureSet: TFeatureSet);
begin
  inherited Create;

  fCpu := Cpu;
  fFeatureSet := FeatureSet;
  fIndex := Index;
end;

destructor TCpuFeature.Destroy;
begin
  inherited;
end;

function TCpuFeature.Mnemonic: String;
begin
  Result := cFeatureDetails[fIndex].Mnemonic
end;

function TCpuFeature.Name: String;
begin
    Result := cFeatureDetails[fIndex].Name
end;

{ TCpuFeatureSet }

function TCpuFeatureSet.Available: ICpuBoolean;
begin
  case fSet of
    fsStandard, fsStandardEx:
      Result := TCpuBoolean.Create(cxCpu[fCpu].Cpuid.Standard.MaximumCommand.AsNumber >= cStd_FeatureSet);
    fsExtended:
      Result := TCpuBoolean.Create(cxCpu[fCpu].Cpuid.Extended.MaximumCommand.AsNumber >= cExt_FeatureSet);
    fsPowerManagement:
      Result := TCpuBoolean.Create(cxCpu[fCpu].Cpuid.Extended.MaximumCommand.AsNumber >= cExt_PowerManagement);
  else
    Result := TCpuBoolean.Create(False);
  end;
end;

function TCpuFeatureSet.ByIndex(Feature: Byte): ICpuFeature;
begin
  if fDefinition.fAvailableFeatures[Feature].Index < cFeature_Res then
    Result := TCpuFeature.Create(fCpu, fDefinition.BitIndexToResource(Feature), fSet)
  else
    Result := TUnknownFeature.Create;
end;

function TCpuFeatureSet.ByName(Mnemonic: String): ICpuFeature;
{ Fixed range check error in Delphi 4 - F. Quointeau (8 Jan 2004) }
var
  Index: Byte;
begin
  Index := fDefinition.MnemonicToBitIndex(Mnemonic);
  if Index < 255 then
    Result := ByIndex(Index)
  else
    Result := TUnknownFeature.Create;
end;

function TCpuFeatureSet.Count: ICpuNumber;
begin
  if Available.AsBoolean then
    Result := fDefinition.FeatureCount
  else
    Result := TCpuNumber.Create(0);
end;

constructor TCpuFeatureSet.Create(Cpu: Byte; FeatureSet: TFeatureSet);
begin
  inherited Create;

  fCpu := Cpu;
  fSet := FeatureSet;
  fDefinition := TFeatureSetDefinition.Create(fCpu, fSet);
end;

destructor TCpuFeatureSet.Destroy;
begin
  fDefinition.Free;
  inherited;
end;

{ TProcessorFeatures }

function TProcessorFeatures.ByName(Mnemonic: String): ICpuFeature;
begin
  Result := (TCpuFeatureSet.Create(fCpu, FeatureLevel(Mnemonic)) as ICpuFeatureSet).ByName(Mnemonic);
end;

constructor TProcessorFeatures.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;
end;

destructor TProcessorFeatures.Destroy;
begin
  inherited;
end;

function TProcessorFeatures.Extended: ICpuFeatureSet;
begin
  Result := TCpuFeatureSet.Create(fCpu, fsExtended);
end;

function TProcessorFeatures.FeatureLevel(Mnemonic: String): TFeatureSet;
var
  i: Integer;
  fi: Integer;
begin
  fi := -1;
  Result := fsUnknown;
  for i := 0 to cMaxFeatures - 1 do
    if cFeatureDetails[i].Mnemonic = UpperCase(Mnemonic) then
    begin
      fi := i;
      Break;
    end;

  if fi >= 0 then
    for i := Ord(fsStandard) to Ord(fsPowerManagement) do
      if TFeatureSet(i) in cFeatureDetails[fi].Level then
      begin
        Result := TFeatureSet(i);
        Break;
      end;
end;

function TProcessorFeatures.Power: ICpuFeatureSet;
begin
  Result := TCpuFeatureSet.Create(fCpu, fsPowerManagement);
end;

function TProcessorFeatures.Standard: ICpuFeatureSet;
begin
  Result := TCpuFeatureSet.Create(fCpu, fsStandard);
end;

function TProcessorFeatures.StandardEx: ICpuFeatureSet;
begin
  Result := TCpuFeatureSet.Create(fCpu, fsStandardEx);
end;

function CpuIsFeatureSupported(Cpu: Byte; Level: TFeatureSet; Feature: Byte): Boolean; overload;
var
  FeatureSet: ICpuFeatureSet;
begin
  FeatureSet := TCpuFeatureSet.Create(Cpu, Level);
  Result := FeatureSet.ByIndex(Feature).Available.AsBoolean;
end;

function CpuIsFeatureSupported(Cpu: Byte; Level: TFeatureSet; Mnemonic: String): Boolean; overload;
var
  FeatureSet: ICpuFeatureSet;
begin
  FeatureSet := TCpuFeatureSet.Create(Cpu, Level);
  Result := FeatureSet.ByName(Mnemonic).Available.AsBoolean;
end;

function CpuGetFeatureSupport(Cpu: Byte; Mnemonic: String): Boolean;
begin
  Result := cxCpu[Cpu].Features.ByName(Mnemonic).Available.AsBoolean;
end;

function CpuGetFeatureIndex(Mnemonic: String): Integer;
var
  i: Integer;
begin
  Result := 255;
  if Result > cMaxFeatures then
    for i := Low(cFeatureDetails) to High(cFeatureDetails) do
      if cFeatureDetails[i].Mnemonic = UpperCase(Mnemonic) then
      begin
        Result := i;
        Break;
      end;
end;

function CpuGetFeatureMnemonic(Index: Byte): String;
begin
  Result := cFeatureDetails[Index].Mnemonic;
end;

function CpuGetFeatureName(Index: Byte): String;
begin
  Result := cFeatureDetails[Index].Name;
end;

{ TProcessorSerial }

function TProcessorSerial.Available: ICpuBoolean;
begin
  Result := TCpuBoolean.Create(cxCpu[fCpu].Features.Standard.ByIndex(cFeature_Psn).Available.AsBoolean);
end;

constructor TProcessorSerial.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;

  fCpuid1 := TCpuid.Create(fCpu, cStd_SerialNumber);
  fCpuid2 := TCpuid.Create(fCpu, cStd_SerialNumber);
end;

destructor TProcessorSerial.Destroy;
begin
  inherited;
end;

function TProcessorSerial.Formatted: String;
begin
  if cxCpu[fCpu].Vendor.VendorType <> cvTransmeta then
    Result := Nibble(fCpuid1.Eax.AsNumber) + '-' +
              Nibble(fCpuid2.Edx.AsNumber) + '-' +
              Nibble(fCpuid2.Ecx.AsNumber)
  else
    Result := Nibble(fCpuid1.Ebx.AsNumber);
end;

function TProcessorSerial.Nibble(Value: Longword): String;
var
  HexValue: String;
begin
  HexValue := Format(RsCpu_Serial_Format, [Value]);
  Result := Copy(HexValue, 0, 4) + '-' + Copy(HexValue, 5, 4);
end;

function TProcessorSerial.Value: String;
begin
  if cxCpu[fCpu].Vendor.VendorType <> cvTransmeta then
    Result := Format(RsCpu_Serial_Format, [fCpuid1.Eax.AsNumber]) +
              Format(RsCpu_Serial_Format, [fCpuid2.Edx.AsNumber]) +
              Format(RsCpu_Serial_Format, [fCpuid2.Ecx.AsNumber])
  else
    Result := Format(RsCpu_Serial_Format, [fCpuid1.Ebx.AsNumber]);
end;

function CpuIsSerialSupported(Cpu: Byte): Boolean;
begin
  Result := cxCpu[Cpu].Serial.Available.AsBoolean;
end;

function CpuGetSerialNumber(Cpu: Byte): String;
begin
  Result := cxCpu[Cpu].Serial.Value;
end;

function CpuGetSerialFormat(Cpu: Byte): String;
begin
  Result := cxCpu[Cpu].Serial.Formatted;
end;

{ TProcessorUsage }

constructor TProcessorUsage.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;
end;

destructor TProcessorUsage.Destroy;
begin
  inherited;
end;

function CpuGetUsage(Cpu: Byte): Integer;
begin
  Result := cxCpu[Cpu].Usage.Value.AsNumber;
end;

function TProcessorUsage.Value: ICpuNumber;
begin
  {$IFDEF USE_ADCPUUSAGE}
    CollectCPUData;
    Result := TCpuNumber.Create(Trunc(GetCPUUsage(fCpu) * 100));
  {$ELSE}
    Result := TCpuNumber.Create(0);
  {$ENDIF USE_ADCPUUSAGE}
end;

{ TProcessorErrata }

constructor TProcessorErrata.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;
end;

destructor TProcessorErrata.Destroy;
begin
  inherited;
end;

function TProcessorErrata.DuronCache: ICpuBoolean;
begin
  Result := TCpuBoolean.Create((cxCpu[fCpu].Vendor.VendorType = cvAmd) and
                               (cxCpu[fCpu].Signature.Family.Value.AsNumber = 6) and
                               (cxCpu[fCpu].Signature.Model.Value.AsNumber = 3) and
                               (cxCpu[fCpu].Signature.Stepping.Value.AsNumber = 0));
end;

function TProcessorErrata.FDivError: ICpuBoolean;
const
  N1: Real = 4195835.0;
  N2: Real = 3145727.0;
begin
  Result := TCpuBoolean.Create((((N1 / N2) * N2) - N1) <> 0.0);
end;

function CpuIsFDivBugPresent(Cpu: Byte): Boolean;
begin
  Result := cxCpu[Cpu].Errata.FDivError.AsBoolean;
end;

function CpuIsDuronCacheBugPresent(Cpu: Byte): Boolean;
begin
  Result := cxCpu[Cpu].Errata.DuronCache.AsBoolean;
end;

{ TProcessorNameDetect }

constructor TProcessorNameDetect.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;
end;

destructor TProcessorNameDetect.Destroy;
begin
  inherited;
end;

function TProcessorNameDetect.Name: String;
var
  LoCommand: Longword;
  HiCommand: Longword;
  Command: Longword;
begin
  if cxCpu[fCpu].Vendor.VendorType <> cvTransmeta then
  begin
    LoCommand := cExt_MarketingName1;
    HiCommand := cExt_MarketingName3;
  end
  else
  begin
    LoCommand := cTmx_MarketingName1;
    HiCommand := cTmx_MarketingName4;
  end;

  Result := '';
  for Command := LoCommand to HiCommand do
    Result := Result + (TCpuid.Create(fCpu, Command) as ICpuid).AsString;
end;

{ TUnknownProcessorLookup }

constructor TUnknownProcessorLookup.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu
end;

destructor TUnknownProcessorLookup.Destroy;
begin
  inherited;
end;

function TUnknownProcessorLookup.Name: String;
begin
  Result := cVendorNames[cvUnknown].Prefix +
            Format(RsCpu_Generic_Name, [cxCpu[fCpu].Signature.Family.Value.AsNumber,
                                        cxCpu[fCpu].Signature.Model.Value.AsNumber,
                                        cxCpu[fCpu].Signature.Stepping.Value.AsNumber]);
end;

{ TIntelProcessorLookup }

constructor TIntelProcessorLookup.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;
end;

destructor TIntelProcessorLookup.Destroy;
begin
  inherited;
end;

function TIntelProcessorLookup.GetBrandName: String;
var
  Signature: Longword;
  BrandIdentifier: Longword;
begin
  Result := '';

  BrandIdentifier := cxCpu[fCpu].Signature.Brand.Value.AsNumber;
  Signature := cxCpu[fCpu].Signature.Value.AsNumber;

  if BrandIdentifier <> cBrand_Unknown then
  begin
    case BrandIdentifier of
      cBrand_Celeron, cBrand_CeleronA:
        Result := RsCpu_Intel_Celeron;

      cBrand_P3, cBrand_P3Alt:
        Result := RsCpu_Intel_PIII;

      cBrand_P3Xeon:
        if Signature <> cBrand_XeonCel then
          Result := RsCpu_Intel_PIIIXeon
        else
          Result := RsCpu_Intel_Celeron;

      cBrand_P3Mobile:
        Result := RsCpu_Intel_PIIIMobile;

      cBrand_CeleronM, cBrand_CeleronMAlt:
        Result := RsCpu_Intel_MobileCeleron;

      cBrand_P4, cBrand_P4Alt:
        if Signature >= cBrand_IntelMp then
          Result := RsCpu_Intel_Generic
        else
          Result := RsCpu_Intel_P4;

      cBrand_P4Mobile:
        if Signature < cBrand_IntelMp then
          Result := RsCpu_Intel_Xeon
        else
          Result := RsCpu_Intel_P4Mobile;

      cBrand_Xeon:
        if Signature < cBrand_IntelMp then
          Result := RsCpu_Intel_XeonMP
        else
          Result := RsCpu_Intel_Xeon;

      cBrand_XeonMP:
        Result := RsCpu_Intel_XeonMP;

    end;
  end
  else
    Result := '';
end;

function TIntelProcessorLookup.Name: String;
var
  Family: Longword;
  Model: Longword;
  Stepping: Longword;
  L2Cache: Longword;
begin
  Result := GetBrandName;
  if Length(Result) > 0 then
    Exit;

  Family := cxCpu[fCpu].Signature.Family.Value.AsNumber;
  Model := cxCpu[fCpu].Signature.Model.Value.AsNumber;
  Stepping := cxCpu[fCpu].Signature.Stepping.Value.AsNumber;

  if Family = cFamily_486 then
    case Model of
      4:  Result := RsCpu_Intel_486SX;
      7:  Result := RsCpu_Intel_486DX2;
      8:  Result := RsCpu_Intel_486DX4;
    else
      Result := cVendorNames[cvIntel].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);
    end;

  if Family = cFamily_P5 then
    case Model of
      0, 1, 2, 7:
        Result := RsCpu_Intel_P5;
      3: Result := RsCpu_Intel_P5OverDrive;
      4: Result := RsCpu_Intel_P5MMX;
    else
      Result := cVendorNames[cvIntel].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);
    end;

  if Family = cFamily_P6 then
  begin
    L2Cache := cxCpu[fCpu].Cache.Level2.Size.AsNumber;

    case Model of
      0, 1:
        Result := RsCpu_Intel_PPro;
      3:  if cxCpu[fCpu].Signature.CpuType.Value.AsNumber = cType_OverDrive then
            Result := RsCpu_Intel_PIIOverDrive
          else
            Result := RsCpu_Intel_PII;
      5:  if L2Cache < cCache_Celeron then
            Result := RsCpu_Intel_Celeron
          else
            if L2Cache < cCache_Xeon then
              Result := RsCpu_Intel_PII
            else
              Result := RsCpu_Intel_PIIXeon;
      6:  if L2Cache < cCache_Celeron then
          begin
            if cxCpu[fCpu].Features.Standard.ByIndex(cFeature_Psn).Available.AsBoolean then
              Result := RsCpu_Intel_MobileCeleron
            else
              Result := RsCpu_Intel_Celeron;
          end
          else
            Result := RsCpu_Intel_PIIMobile;
      7:  if L2Cache < cCache_Xeon then
            Result := RsCpu_Intel_PIII
          else
            Result := RsCpu_Intel_PIIIXeon;
    else
      Result := cVendorNames[cvIntel].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);
    end;
  end;
  if Length(Result) = 0 then
    Result := cVendorNames[cvIntel].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);
end;

{ TAmdProcessorLookup }

constructor TAmdProcessorLookup.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;
end;

destructor TAmdProcessorLookup.Destroy;
begin
  inherited;
end;

function TAmdProcessorLookup.Name: String;
var
  Family: Longword;
  Model: Longword;
  Stepping: Longword;
begin
  Result := '';
  Family := cxCpu[fCpu].Signature.Family.Value.AsNumber;
  Model := cxCpu[fCpu].Signature.Model.Value.AsNumber;
  Stepping := cxCpu[fCpu].Signature.Stepping.Value.AsNumber;

  if Family = cFamily_486 then
    case Model of
      3, 7, 8, 9:
        Result := RsCpu_Amd_486DX2;
      14, 15:
        Result := RsCpu_Amd_5X86;
    else
      Result := cVendorNames[cvAmd].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);
    end;

  if Family = cFamily_P5 then
    case Model of
      0..3:   Result := RsCpu_Amd_K5;
      6, 7:   Result := RsCpu_Amd_K6;
      8:      Result := RsCpu_Amd_K62;
      9, $D:  Result := RsCpu_Amd_K63;
    else
      Result := cVendorNames[cvAmd].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);
    end;

  if Length(Result) = 0 then
    Result := cVendorNames[cvAmd].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);

end;

{ TCyrixProcessorLookup }

constructor TCyrixProcessorLookup.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;
end;

destructor TCyrixProcessorLookup.Destroy;
begin
  inherited;
end;

function TCyrixProcessorLookup.Name: String;
var
  Family: Longword;
  Model: Longword;
  Stepping: Longword;
begin
  Result := '';
  Family := cxCpu[fCpu].Signature.Family.Value.AsNumber;
  Model := cxCpu[fCpu].Signature.Model.Value.AsNumber;
  Stepping := cxCpu[fCpu].Signature.Stepping.Value.AsNumber;

  if Family = cFamily_486 then
    case Model of
      4:  Result := RsCpu_Cyrix_MediaGX;
      9:  Result := RsCpu_Cyrix_Cx5x86;
    else
      Result := cVendorNames[cvCyrix].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);
    end;

  if Family = cFamily_P5 then
    case Model of
      2:  Result := RsCpu_Cyrix_6x86;
      4:  Result := RsCpu_Cyrix_GXm;
    else
      Result := cVendorNames[cvCyrix].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);
    end;

  if Family = cFamily_P6 then
    case Model of
      0:    Result := RsCpu_Cyrix_6x86MX;
      5:    Result := RsCpu_Cyrix_M2;
      6..8: Result := RsCpu_Cyrix_WinChip;
    else
      Result := cVendorNames[cvCyrix].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);
    end;

  if Length(Result) = 0 then
    Result := cVendorNames[cvCyrix].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);
end;

{ TIdtProcessorLookup }

constructor TIdtProcessorLookup.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;
end;

destructor TIdtProcessorLookup.Destroy;
begin
  inherited;
end;

function TIdtProcessorLookup.Name: String;
var
  Family: Longword;
  Model: Longword;
  Stepping: Longword;
begin
  Result := '';
  Family := cxCpu[fCpu].Signature.Family.Value.AsNumber;
  Model := cxCpu[fCpu].Signature.Model.Value.AsNumber;
  Stepping := cxCpu[fCpu].Signature.Stepping.Value.AsNumber;

  if Family = cFamily_P5 then
    case Model of
      4: Result := RsCpu_Idt_WinChip;
      8: Result := RsCpu_Idt_WinChip2;
      9: Result := RsCpu_Idt_WinChip3;
    else
      Result := cVendorNames[cvIDT].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);
    end;

  if Length(Result) = 0 then
    Result := cVendorNames[cvIDT].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);
end;

{ TNexGenProcessorLookup }

constructor TNexGenProcessorLookup.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;
end;

destructor TNexGenProcessorLookup.Destroy;
begin
  inherited;
end;

function TNexGenProcessorLookup.Name: String;
var
  Family: Longword;
  Model: Longword;
  Stepping: Longword;
begin
  Result := '';
  Family := cxCpu[fCpu].Signature.Family.Value.AsNumber;
  Model := cxCpu[fCpu].Signature.Model.Value.AsNumber;
  Stepping := cxCpu[fCpu].Signature.Stepping.Value.AsNumber;

  if Family = cFamily_P5 then
    case Model of
      0: Result := RsCpu_NexGen_Nx586;
    else
      Result := cVendorNames[cvNexGen].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);
    end;

  if Length(Result) = 0 then
    Result := cVendorNames[cvNexGen].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);
end;

{ TUmcProcessorLookup }

constructor TUmcProcessorLookup.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;
end;

destructor TUmcProcessorLookup.Destroy;
begin
  inherited;
end;

function TUmcProcessorLookup.Name: String;
var
  Family: Longword;
  Model: Longword;
  Stepping: Longword;
begin
  Result := '';
  Family := cxCpu[fCpu].Signature.Family.Value.AsNumber;
  Model := cxCpu[fCpu].Signature.Model.Value.AsNumber;
  Stepping := cxCpu[fCpu].Signature.Stepping.Value.AsNumber;

  if Family = cFamily_486 then
    case Model of
      1: Result := RsCpu_Umc_U5D;
      2: Result := RsCpu_Umc_U5D;
    else
      Result := cVendorNames[cvUMC].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);
    end;

  if Length(Result) = 0 then
    Result := cVendorNames[cvUMC].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);
end;

{ TRiseProcessorLookup }

constructor TRiseProcessorLookup.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;
end;

destructor TRiseProcessorLookup.Destroy;
begin
  inherited;
end;

function TRiseProcessorLookup.Name: String;
var
  Family: Longword;
  Model: Longword;
  Stepping: Longword;
begin
  Result := '';
  Family := cxCpu[fCpu].Signature.Family.Value.AsNumber;
  Model := cxCpu[fCpu].Signature.Model.Value.AsNumber;
  Stepping := cxCpu[fCpu].Signature.Stepping.Value.AsNumber;

  if Family = cFamily_P5 then
    case Model of
      0: Result := RsCpu_Rise_mP6;
      2: Result := RsCpu_Rise_mP6II;
    else
      Result := cVendorNames[cvRise].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);
    end;

  if Length(Result) = 0 then
    Result := cVendorNames[cvRise].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);
end;

{ TTransmetaProcessorLookup }

constructor TTransmetaProcessorLookup.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;
end;

destructor TTransmetaProcessorLookup.Destroy;
begin
  inherited;
end;

function TTransmetaProcessorLookup.Name: String;
var
  Family: Longword;
  Model: Longword;
  Stepping: Longword;
begin
  Family := cxCpu[fCpu].Signature.Family.Value.AsNumber;
  Model := cxCpu[fCpu].Signature.Model.Value.AsNumber;
  Stepping := cxCpu[fCpu].Signature.Stepping.Value.AsNumber;

  Result := cVendorNames[cvTransmeta].Prefix + Format(RsCpu_Generic_Name, [Family, Model, Stepping]);
end;

{ TProcessorName }

function TProcessorName.AsString: String;
begin
  if not fUseLookup then
    Result := Trim((TProcessorNameDetect.Create(fCpu) as ICpuName).Name)
  else
    case cxCpu[fCpu].Vendor.VendorType of
      cvIntel:      Result := (TIntelProcessorLookup.Create(fCpu) as ICpuName).Name;
      cvAmd:        Result := (TAmdProcessorLookup.Create(fCpu) as ICpuName).Name;
      cvCyrix:      Result := (TCyrixProcessorLookup.Create(fCpu) as ICpuName).Name;
      cvIDT:        Result := (TIdtProcessorLookup.Create(fCpu) as ICpuName).Name;
      cvNexGen:     Result := (TNexGenProcessorlookup.Create(fCpu) as ICpuName).Name;
      cvUMC:        Result := (TUmcProcessorLookup.Create(fCpu) as ICpuName).Name;
      cvRise:       Result := (TRiseProcessorLookup.Create(fCpu) as ICpuName).Name;
      cvTransmeta:  Result := (TTransmetaProcessorLookup.Create(fCpu) as ICpuName).Name;
    else
      Result := (TUnknownProcessorLookup.Create(fCpu) as ICpuName).Name;
    end;
end;

constructor TProcessorName.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;

  fTransmetaCpu := cxCpu[fCpu].Vendor.VendorType = cvTransmeta;
  if not fTransmetaCpu then
  begin
    fUseLookup := cxCpu[fCpu].Cpuid.Extended.MaximumCommand.AsNumber < cExt_MarketingName3;
  end
  else
    fUseLookup := cxCpu[fCpu].Cpuid.Extended.MaximumCommand.AsNumber < cTmx_MarketingName4;
end;

destructor TProcessorName.Destroy;
begin
  inherited;
end;

function TProcessorName.FromLookup: ICpuBoolean;
begin
  Result := TCpuBoolean.Create(fUseLookup);
end;

function CpuGetFullName(Cpu: Byte): String;
begin
  Result := cxCpu[Cpu].Name.AsString;
end;

function CpuIsNameFromLookup(Cpu: Byte): Boolean;
begin
  Result := cxCpu[Cpu].Name.FromLookup.AsBoolean;
end;

{ TCpuCacheDescriptors }

function TCpuCacheDescriptors.ByIndex(Index: Integer): Longword;
begin
  Result := fValues[Index];
end;

constructor TCpuCacheDescriptors.Create(Cpu: Byte; Level: TCacheLevel);
begin

  fCpu := Cpu;
  fLevel := Level;

  SetDescriptors;
end;

procedure TCpuCacheDescriptors.DecodeDescriptor(Value: ICpuNumber;
  Index: Integer);
begin
  if ValidDescriptor(Value.AsNumber) then
  begin
    fValues[Index * 4 - 3] := Value.LoWord.LoByte;
    fValues[Index * 4 - 2] := Value.LoWord.HiByte;
    fValues[Index * 4 - 1] := Value.HiWord.LoByte;
    fValues[Index * 4]     := Value.HiWord.HiByte;
  end;
end;

destructor TCpuCacheDescriptors.Destroy;
begin
  inherited;
end;

procedure TCpuCacheDescriptors.SetDescriptors;
var
  CacheValue: ICpuid;
begin
  if cxCpu[fCpu].Vendor.VendorType = cvIntel then
    CacheValue := TCpuid.Create(fCpu, cStd_CacheTlbs, (TCpuid.Create(fCpu, cStd_CacheTlbs) as ICpuid).Eax.LoWord.LoByte)
  else
    if fLevel in [clLevel1Code..clLevel1Unified, clTrace] then
      CacheValue := TCpuid.Create(fCpu, cExt_Level1Cache)
    else
      CacheValue := TCpuid.Create(fCpu, cExt_Level2Cache);

  DecodeDescriptor(CacheValue.Eax, 1);
  DecodeDescriptor(CacheValue.Ebx, 2);
  DecodeDescriptor(CacheValue.Ecx, 3);
  DecodeDescriptor(CacheValue.Edx, 4);
end;

function TCpuCacheDescriptors.ValidDescriptor(Value: Longword): Boolean;
begin
  Result := (Value and (1 shl 31)) = 0;
end;

function TCpuCacheDescriptors.ValueExists(Value: Longword): ICpuBoolean;
var
  i: Integer;
  f: Boolean;
begin
  f := False;
  for i := Low(fValues) to High(fValues) do
    if not f then
      f := fValues[i] = Value;

  Result := TCpuBoolean.Create(f);
end;

{ TCpuCacheAssociativity }

constructor TCpuCacheAssociativity.Create(Value: TCacheAssociativity);
begin
  inherited Create;

  fValue := Value;
end;

destructor TCpuCacheAssociativity.Destroy;
begin
  inherited;
end;

function TCpuCacheAssociativity.Name: String;
begin
  Result := cAssociativityDescription[fValue];
end;

function TCpuCacheAssociativity.Value: ICpuNumber;
begin
  Result := TCpuNumber.Create(cAssociativityInfo[fValue]);
end;

{ TCacheByDescriptor }

function TCacheByDescriptor.Associativity: ICpuCacheAssociativity;
var
  i: Integer;
begin
  if not Available.AsBoolean then
  begin
    Result := TCpuCacheAssociativity.Create(caNone);
    Exit;
  end;

  Result := TCpuCacheAssociativity.Create(caNone);
  for i := Low(cDescriptorInfo) to High(cDescriptorInfo) do
    if cDescriptorInfo[i].Level = fLevel then
    begin
      Result := TCpuCacheAssociativity.Create(cDescriptorInfo[i].Associativity);
      Break;
    end;
end;

function TCacheByDescriptor.Available: ICpuBoolean;
begin
  Result := TCpuBoolean.Create(Size.AsNumber > 0);
end;

constructor TCacheByDescriptor.Create(Cpu: Integer; Level: TCacheLevel);
begin
  inherited Create;

  fCpu := Cpu;
  fLevel := Level;
  fDescriptors := TCpuCacheDescriptors.Create(fCpu, fLevel);
end;

destructor TCacheByDescriptor.Destroy;
begin
  inherited;
end;

function TCacheByDescriptor.LineSize: ICpuNumber;
var
  i: Integer;
  s: Integer;
begin
  if not Available.AsBoolean then
  begin
    Result := TCpuNumber.Create(0);
    Exit;
  end;

  s := 0;
  for i := Low(cDescriptorInfo) to High(cDescriptorInfo) do
    if (cDescriptorInfo[i].Level = fLevel) and (fDescriptors.ValueExists(cDescriptorInfo[i].Descriptor).AsBoolean) then
      s := cDescriptorInfo[i].LineSize;
  Result := TCpuNumber.Create(s);
end;

function TCacheByDescriptor.Size: ICpuNumber;
var
  i: Integer;
  s: Integer;
begin
  s := 0;
  for i := Low(cDescriptorInfo) to High(cDescriptorInfo) do
    if (cDescriptorInfo[i].Level = fLevel) and (fDescriptors.ValueExists(cDescriptorInfo[i].Descriptor).AsBoolean) then
      s := cDescriptorInfo[i].Size * 1024;
  Result := TCpuNumber.Create(s);
end;

{ TCacheByExtendedCpuid }

function TCacheByExtendedCpuid.Associativity: ICpuCacheAssociativity;
var
  i: TCacheAssociativity;
  AssociativityValue: Byte;
begin
  if not Available.AsBoolean then
  begin
    Result := TCpuCacheAssociativity.Create(caNone);
    Exit;
  end;

  case fLevel of
    clLevel1Code: AssociativityValue := fCacheValue.Edx.HiWord.LoByte;
    clLevel1Data: AssociativityValue := fCacheValue.Ecx.HiWord.LoByte;
    clLevel2:
    begin
      if (cxCpu[fCpu].Vendor.VendorType = cvAmd) and (cxCpu[fCpu].Signature.Family.Value.AsNumber = 6) then
        AssociativityValue := cAssociativityInfo[ca16Way]
      else
        AssociativityValue := fCacheValue.Ecx.HiWord.LoByte; // ????
    end;
  else
    AssociativityValue := 0;
  end;
  for i := caNone to caFull do
    if AssociativityValue = cAssociativityInfo[i] then
      Result := TCpuCacheAssociativity.Create(TCacheAssociativity(i));
end;

function TCacheByExtendedCpuid.Available: ICpuBoolean;
begin
  Result := TCpuBoolean.Create(Size.AsNumber > 0);
end;

constructor TCacheByExtendedCpuid.Create(Cpu: Byte; Level: TCacheLevel);
begin
  inherited Create;

  fCpu := Cpu;
  fLevel := Level;

  case Level of
    clLevel1Code, clLevel1Data: fCacheValue := TCpuid.Create(fCpu, cExt_Level1Cache);
    clLevel2: fCacheValue := TCpuid.Create(fCpu, cExt_Level2Cache);
  else
    fCacheValue := TCpuid.Create(Cpu, cExt_Unsupported);
  end;
end;

destructor TCacheByExtendedCpuid.Destroy;
begin
  inherited;
end;

function TCacheByExtendedCpuid.LineSize: ICpuNumber;
begin
  if not Available.AsBoolean then
  begin
    Result := TCpuNumber.Create(0);
    Exit;
  end;

  case fLevel of
    clLevel1Code:
      Result := TCpuNumber.Create(fCacheValue.Edx.LoWord.LoByte);
    clLevel1Data, clLevel2:
      Result := TCpuNumber.Create(fCacheValue.Ecx.LoWord.LoByte);
  else
    Result := TCpuNumber.Create(0);
  end;
end;

function TCacheByExtendedCpuid.Size: ICpuNumber;
begin
  case fLevel of
    clLevel1Code:
      Result := TCpuNumber.Create(fCacheValue.Edx.HiWord.HiByte * 1024);
    clLevel1Data:
      Result := TCpuNumber.Create(fCacheValue.Ecx.HiWord.HiByte * 1024);
    clLevel2:
      if cxCpu[fCpu].Vendor.VendorType = cvIDT then
        Result := TCpuNumber.Create(fCacheValue.Ecx.HiWord.HiByte * 1024)
      else
        Result := TCpuNumber.Create(fCacheValue.Ecx.HiWord.Value * 1024);
  else
    Result := TCpuNumber.Create(0);
  end;
end;

{ TCpuSegmentedCache }

function TCpuSegmentedCache.Code: ICpuCache;
begin
  if not fExtended then
    Result := TCacheByDescriptor.Create(fCpu, clLevel1Code)
  else
    Result := TCacheByExtendedCpuid.Create(fCpu, clLevel1Code);
end;

constructor TCpuSegmentedCache.Create(Cpu: Byte; Extended: Boolean);
begin
  inherited Create;

  fCpu := Cpu;
  fExtended := Extended;
end;

function TCpuSegmentedCache.Data: ICpuCache;
begin
  if not fExtended then
    Result := TCacheByDescriptor.Create(fCpu, clLevel1Data)
  else
    Result := TCacheByExtendedCpuid.Create(fCpu, clLevel1Data);
end;

destructor TCpuSegmentedCache.Destroy;
begin
  inherited;
end;

function TCpuSegmentedCache.Unified: ICpuCache;
begin
  if not fExtended then
    Result := TCacheByDescriptor.Create(fCpu, clLevel1Unified)
  else
    Result := TCacheByExtendedCpuid.Create(fCpu, clLevel1Unified);
end;

{ TProcessorCache }

constructor TProcessorCache.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;
  fExtended := cxCpu[fCpu].Vendor.CacheDetect = vcdExtended;
end;

destructor TProcessorCache.Destroy;
begin
  inherited;
end;

function TProcessorCache.Level1: ICpuSegmentedCache;
begin
  Result := TCpuSegmentedCache.Create(fCpu, fExtended);
end;

function TProcessorCache.Level2: ICpuCache;
begin
  if not fExtended then
    Result := TCacheByDescriptor.Create(fCpu, clLevel2)
  else
    Result := TCacheByExtendedCpuid.Create(fCpu, clLevel2);
end;

function TProcessorCache.Level3: ICpuCache;
begin
  if not fExtended then
    Result := TCacheByDescriptor.Create(fCpu, clLevel3)
  else
    Result := TCacheByExtendedCpuid.Create(fCpu, clLevel3);
end;

function TProcessorCache.Trace: ICpuCache;
begin
  if not fExtended then
    Result := TCacheByDescriptor.Create(fCpu, clTrace)
  else
    Result := TCacheByExtendedCpuid.Create(fCpu, clTrace);
end;

function CpuIsCacheAvailable(Cpu: Byte; CacheLevel: TCacheLevel): Boolean;
var
  Cache: ICpuCache;
begin
  if cxCpu[Cpu].Vendor.CacheDetect <> vcdExtended then
    Cache := TCacheByDescriptor.Create(Cpu, CacheLevel)
  else
    Cache := TCacheByExtendedCpuid.Create(Cpu, CacheLevel);

  Result := Cache.Available.AsBoolean;
end;

function CpuGetCacheAssociativity(Cpu: Byte; CacheLevel: TCacheLevel): TCacheAssociativity;
var
  Cache: ICpuCache;
begin
  if cxCpu[Cpu].Vendor.CacheDetect <> vcdExtended then
    Cache := TCacheByDescriptor.Create(Cpu, CacheLevel)
  else
    Cache := TCacheByExtendedCpuid.Create(Cpu, CacheLevel);

  Result := TCacheAssociativity(Cache.Associativity.Value.AsNumber);
end;

function CpuGetCacheLineSize(Cpu: Byte; CacheLevel: TCacheLevel): Integer;
var
  Cache: ICpuCache;
begin
  if cxCpu[Cpu].Vendor.CacheDetect <> vcdExtended then
    Cache := TCacheByDescriptor.Create(Cpu, CacheLevel)
  else
    Cache := TCacheByExtendedCpuid.Create(Cpu, CacheLevel);

  Result := Cache.LineSize.AsNumber;
end;

function CpuGetCacheSize(Cpu: Byte; CacheLevel: TCacheLevel): Integer;
var
  Cache: ICpuCache;
begin
  if cxCpu[Cpu].Vendor.CacheDetect <> vcdExtended then
    Cache := TCacheByDescriptor.Create(Cpu, CacheLevel)
  else
    Cache := TCacheByExtendedCpuid.Create(Cpu, CacheLevel);

  Result := Cache.Size.AsNumber;
end;

{ TCpuSpeedThread }

constructor TCpuSpeedThread.Create;
begin
  inherited Create(False);
  {$IFDEF MSWINDOWS}
  Priority := tpTimeCritical;
  {$ENDIF MSWINDOWS}
end;

procedure TCpuSpeedThread.Execute;
begin
  fCpuSpeed := GetProcessorSpeed;
end;

function TCpuSpeedThread.GetCpuSpeed: Longword;
begin
  Result := Trunc(fCpuSpeed);
end;

{$IFDEF MSWINDOWS}
  function TCpuSpeedThread.GetProcessorSpeed: Double;
  var
    C1, T1, C2, T2, F: Int64;
    Tc: DWORD;
  begin
    {$O-}
    C1 := RdTsc;
    QueryPerformanceCounter(T1);
    Tc := GetTickCount;
    {$IFDEF USE_SLEEP}
      Sleep(200);
    {$ENDIF USE_SLEEP}
    while GetTickCount - Tc < 5 do;
    C2 := Rdtsc;
    QueryPerformanceCounter(T2);
    QueryPerformanceFrequency(F);
    {$O+}
    Result := (C2 - C1) / ((T2 - T1) / F) / 1E6;
  end;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  function TCpuSpeedThread.GetProcessorSpeed: Double;
  var
    t: Cardinal;
    mhi, mlo, nhi, nlo: LongInt;
    t0, t1, chi, clo, shr32: Comp;

    function Ticks: Cardinal;
    var
      Time: TimeVal;
    begin
      GetTimeOfDay(Time, nil);
      Result := Int64(Time.tv_sec) * 1000 + Time.tv_usec div 1000;
    end;

  begin
    shr32 := 65536;
    shr32 := shr32 * 65536;
    t := Ticks;
    while (t = Ticks) do
    begin
    end;

    asm
      DB 	  0FH
      DB 	  031H
      mov   mhi, EDX
      mov   mlo, EAX
    end;

    while (Ticks < (t + cBenchmarkDelay)) do
    begin
    end;

    asm
      DB 	  0FH
      DB 	  031H
      mov   nhi, EDX
      mov   nlo, EAX
    end;

    chi := mhi;
    if (mhi < 0) then
      chi := chi + shr32;
    clo := mlo;
    if (mlo < 0) then
      clo := clo + shr32;
    t0 := chi * shr32 + clo;
    chi := nhi;
    if (nhi < 0) then
      chi := chi + shr32;
    clo := nlo;
    if (nlo < 0) then
      clo := clo + shr32;
    t1 := chi * shr32 + clo;

    Result := ((t1 - t0) / (1E6 / (1000/cBenchmarkDelay)));
  end;
{$ENDIF LINUX}

{$IFDEF MSWINDOWS}
  function TCpuSpeedThread.RdTsc: Int64;
  asm
    DB 0FH, 31H
  end;
{$ENDIF MSWINDOWS}

{ TProcessorSpeedInformation }

constructor TProcessorSpeedInformation.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;
  GetCpuSpeed;
  fNormalisedSpeed := NormaliseSpeed(fRawSpeed);
end;

destructor TProcessorSpeedInformation.Destroy;
begin
  inherited;
end;

procedure TProcessorSpeedInformation.GetCpuSpeed;
begin
  with TCpuSpeedThread.Create do
  try
    WaitFor;
    fRawSpeed := CpuSpeed;
  finally
    Free;
  end;
end;

function TProcessorSpeedInformation.Mapping: String;
var
  i: Integer;
  ns: Longword;
  l: Integer;
begin
  Result := '';
  if cxCpu[fCpu].Vendor.VendorType <> cvAmd then
    Exit;

  ns := Normalised.AsNumber;

  // Mapping group determined by size of L2 cache
  l := cxCpu[fCpu].Cache.Level2.Size.AsNumber div 1024;

  case l of
    256:  for i := 0 to cMax256Mappings - 1 do
            if cAmd256Mappings[i].Frequency = Integer(ns) then
            begin
              Result := cAmd256Mappings[i].Mapping;
              Break;
            end;
    512:  for i := 0 to cMax512Mappings - 1 do
            if cAmd512Mappings[i].Frequency = Integer(ns) then
            begin
              Result := cAmd512Mappings[i].Mapping;
              Break;
            end
  else
    Result := '';
  end;
end;

function TProcessorSpeedInformation.Normalised: ICpuNumber;
begin
  Result := TCpuNumber.Create(fNormalisedSpeed);
end;

function TProcessorSpeedInformation.NormaliseSpeed(
  Value: Longword): Longword;
var
  Freq, RF: Integer;
  i: Byte;
  Hi, Lo: Byte;
begin
  RF := 0;
  Freq := Value mod 100;
  for i := 0 to cMaxFrequencies - 1 do
  begin
    if Freq < cCpuFrequencies[i] then
    begin
      Hi := i;
      Lo := i - 1;
      if (cCpuFrequencies[Hi] - Freq) > (Freq - cCpuFrequencies[Lo]) then
        RF := cCpuFrequencies[Lo] - Freq
      else
        RF := cCpuFrequencies[Hi] - Freq;
      Break;
    end;
  end;
  Result := Integer(Value) + RF;
end;

function TProcessorSpeedInformation.RawSpeed: ICpuNumber;
begin
  Result := TCpuNumber.Create(fRawSpeed);
end;

function CpuGetRawSpeed(Cpu: Byte): Integer;
begin
  Result := cxCpu[Cpu].Speed.RawSpeed.AsNumber;
end;

function CpuGetNormalisedSpeed(Cpu: Byte): Integer;
begin
  Result := cxCpu[Cpu].Speed.Normalised.AsNumber;
end;

function CpuGetAmdMapping(Cpu: Byte): String;
begin
  Result := cxCpu[Cpu].Speed.Mapping;
end;

{ TProcessorCount }

{$IFDEF MSWINDOWS}
  function TProcessorCount.Available: ICpuNumber;
  var
    lpSysInfo: TSystemInfo;
  begin
    GetSystemInfo(lpSysInfo);
    Result := TCpuNumber.Create(lpSysInfo.dwNumberOfProcessors);
  end;
{$ENDIF MSWINDOWS}

{$IFDEF LINUX}
  function TProcessorCount.Available: ICpuNumber;
  begin
    Result := TCpuNumber.Create(get_nprocs_conf);
  end;
{$ENDIF LINUX}

constructor TProcessorCount.Create;
begin
  inherited Create;
end;

destructor TProcessorCount.Destroy;
begin
  inherited;
end;

function TProcessorCount.Logical: ICpuNumber;
var
  i: Byte;
  r: Integer;
begin
  r := 0;
  for i := 1 to Available.AsNumber do
  begin
    r := r + LogicalProcessorCount(i);
  end;
  Result := TCpuNumber.Create(r);
end;

function TProcessorCount.LogicalProcessorCount(Cpu: Byte): Byte;
var
  LogicalResult: ICpuid;
begin
  if SupportsHyperThreading(Cpu) then
  begin
    LogicalResult := TCpuid.Create(Cpu, cStd_Signature);
    Result := LogicalResult.Ebx.HiWord.LoByte;
  end
  else
    Result := 1;
end;

function TProcessorCount.SupportsHyperThreading(Cpu: Byte): Boolean;
begin
  Result := cxCpu[Cpu].Features.Standard.ByIndex(cFeature_Htt).Available.AsBoolean;
end;

function CpuGetProcessorCount: Byte;
begin
  Result := cxCpu.ProcessorCount.Available.AsNumber;
end;

function CpuGetVirtualCount: Byte;
begin
  Result := cxCpu.ProcessorCount.Logical.AsNumber;
end;

{ TProcessorInformation }

function TProcessorInformation.Cache: IProcessorCache;
begin
  Result := TProcessorCache.Create(fCpu);
end;

function TProcessorInformation.Cpuid: IProcessorCpuid;
begin
  Result := TProcessorCpuid.Create(fCpu);
end;

constructor TProcessorInformation.Create(Cpu: Byte);
begin
  inherited Create;

  fCpu := Cpu;
end;

destructor TProcessorInformation.Destroy;
begin
  inherited;
end;

function TProcessorInformation.Errata: IProcessorErrata;
begin
  Result := TProcessorErrata.Create(fCpu);
end;

function TProcessorInformation.Features: IProcessorFeatures;
begin
  Result := TProcessorFeatures.Create(fCpu);
end;

function TProcessorInformation.Name: IProcessorName;
begin
  Result := TProcessorName.Create(fCpu);
end;

function TProcessorInformation.Serial: IProcessorSerial;
begin
  Result := TProcessorSerial.Create(fCpu);
end;

function TProcessorInformation.Signature: IProcessorSignature;
begin
  Result := TProcessorSignature.Create(fCpu);
end;

function TProcessorInformation.Speed: IProcessorSpeed;
begin
  Result := TProcessorSpeedInformation.Create(fCpu);
end;

function TProcessorInformation.Usage: IProcessorUsage;
begin
  Result := TProcessorUsage.Create(fCpu);
end;

function TProcessorInformation.Vendor: IProcessorVendor;
begin
  Result := TProcessorVendor.Create(fCpu);
end;

{ TcxCpu }

constructor TcxCpu.Create;
begin
  inherited Create;

  fSmp := ProcessorCount.Available.AsNumber > 1;
end;

destructor TcxCpu.Destroy;
begin
  inherited;
end;

function TcxCpu.GetProcessor(Value: Integer): IProcessorInformation;
begin
  Result := TProcessorInformation.Create(Value);
end;

function TcxCpu.ProcessorCount: IProcessorCount;
begin
  Result := TProcessorCount.Create;
end;

function TcxCpu.Version: ICpuNumber;
begin
  Result := TCpuNumber.Create(cToolkit_Version);
end;

{$IFDEF MSWINDOWS}
  procedure SetDynSetProcessAffinity;
  var
    Kernel32: THandle;
  begin
    Kernel32 := LoadLibrary(PChar(RsCpu_Library_Kernel));
    if Kernel32 <> 0 then
    begin
      DynSetProcessAffinity := GetProcAddress(Kernel32, PChar(RsCpu_Library_Function));
    end;
  end;
{$ENDIF MSWINDOWS}


function CpuGetToolkitVersion: String;
begin
  Result := (TCpuNumber.Create(cToolkit_Version) as ICpuNumber).FormatVersion;
end;

function cxCpu: TcxCpu;
begin
  if fcxCpu = nil then
    fcxCpu := TcxCpu.Create;

  Result := fcxCpu;
end;

initialization
  {$IFDEF MSWINDOWS}
    SetDynSetProcessAffinity;
  {$ENDIF MSWINDOWS}

finalization
  fcxCpu.Free;

end.
