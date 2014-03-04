unit unitVersionHelpers;
{
/******************************************************************
*                                                                 *
*  VersionHelpers.h -- This module defines helper functions to    *
*                      promote version check with proper          *
*                      comparisons.                               *
*                                                                 *
*  Copyright (c) Microsoft Corp.  All rights reserved.            *
*                                                                 *
******************************************************************/

Header translation copyright (c) 2014 - J. Peter Mugaas

Written: Feb 19, 2014 - J. Peter Mugaas <jpmugaas@suddenlink.net>

Copyright
Portions of this software are Copyright (c) 2014, J. Peter Mugaas - http://www.jpetermugaas.com

License terms described in LICENSE.TXT
}
interface
{$ifdef COMPILER10_UP}
  {$define USE_INLINE}
{$endif}
{$ALIGN ON}
{$MINENUMSIZE 4}
uses Windows;

type
  {$EXTERNALSYM _SYSTEM_INFO}
  _SYSTEM_INFO = record
    case Integer of
    0 : (dwOemId : DWORD);  // Obsolete field...do not use
    1 : (wProcessorArchitecture : WORD;
         wReserved : WORD;
         dwPageSize : DWORD;
         lpMinimumApplicationAddress : LPVOID;
         lpMaximumApplicationAddress : LPVOID;
         dwActiveProcessorMask : DWORD_PTR;
         dwNumberOfProcessors : DWORD;
         dwProcessorType : DWORD;
         dwAllocationGranularity : DWORD;
         wProcessorLevel : WORD;
         wProcessorRevision : WORD);
  end;
  {$EXTERNALSYM SYSTEM_INFO}
  SYSTEM_INFO = _SYSTEM_INFO;
  {$EXTERNALSYM LPSYSTEM_INFO}
  LPSYSTEM_INFO = ^SYSTEM_INFO;

type
  {$EXTERNALSYM LPFN_GetProductInfo}
  LPFN_GetProductInfo = function(const dwOSMajorVersion : DWORD;
    const dwOSMinorVersion : DWORD;
    const dwSpMajorVersion : DWORD;
    const dwSpMinorVersion : DWORD;
    var pdwReturnedProductType : DWORD) : BOOL stdcall;
  {$EXTERNALSYM LPFN_GetSystemInfo}
  LPFN_GetSystemInfo = procedure(out lpSystemInfo :  _SYSTEM_INFO) stdcall;
  {$EXTERNALSYM LPFN_GetNativeSystemInfo }
  LPFN_GetNativeSystemInfo = procedure(out lpSystemInfo :  _SYSTEM_INFO) stdcall;
  {$EXTERNALSYM LPFN_VerifyVersionInfoW}
  LPFN_VerifyVersionInfoW = function(var VersionInformation : OSVERSIONINFOEXW;
    const dwTypeMask : DWORD;
    const dwlConditionMask : DWORDLONG) : BOOL stdcall;
  {$EXTERNALSYM LPFN_VerSetConditionMask}
  LPFN_VerSetConditionMask = function(const ConditionMask : ULONGLONG;
   const TypeMask : DWORD;
   const Condition : BYTE ) : ULONGLONG stdcall;
  {$EXTERNALSYM LPFN_IsWow64Process}
  LPFN_IsWow64Process = function(hProcess : THANDLE; out Wow64Process : BOOL) : BOOL stdcall;

var
  {$EXTERNALSYM GetSystemInfo}
  GetSystemInfo : LPFN_GetSystemInfo = nil;
  {$EXTERNALSYM GetNativeSystemInfo}
  GetNativeSystemInfo : LPFN_GetNativeSystemInfo = nil;
  {$EXTERNALSYM IsWow64Process}
  IsWow64Process : LPFN_IsWow64Process = nil;
  {$EXTERNALSYM LPFN_VerifyVersionInfoW}
  VerifyVersionInfoW : LPFN_VerifyVersionInfoW = nil;
  {$EXTERNALSYM VerSetConditionMask}
  VerSetConditionMask : LPFN_VerSetConditionMask = nil;
  {$EXTERNALSYM GetProductInfo}
  GetProductInfo : LPFN_GetProductInfo = nil;

{$EXTERNALSYM IsWindowsVersionOrGreater}
function IsWindowsVersionOrGreater(const wMajorVersion, wMinorVersion, wServicePackMajor : Word) : Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IsWindowsXPOrGreater}
function IsWindowsXPOrGreater : Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IsWindowsXPSP1OrGreater}
function IsWindowsXPSP1OrGreater : Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IsWindowsXPSP2OrGreater}
function IsWindowsXPSP2OrGreater : Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IsWindowsXPSP3OrGreater}
function IsWindowsXPSP3OrGreater : Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IsWindowsVistaOrGreater}
function IsWindowsVistaOrGreater : Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IsWindowsVistaSP1OrGreater}
function IsWindowsVistaSP1OrGreater : Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IsWindowsVistaSP2OrGreater}
function IsWindowsVistaSP2OrGreater : Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IsWindows7OrGreater}
function IsWindows7OrGreater : Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IsWindows7SP1OrGreater}
function IsWindows7SP1OrGreater : Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IsWindows8OrGreater}
function IsWindows8OrGreater: Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IsWindows8Point1OrGreater}
function IsWindows8Point1OrGreater: Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IsWindowsServer}
function IsWindowsServer : Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}

const
//from winnt.h
  {$EXTERNALSYM VER_EQUAL}
  VER_EQUAL                       = 1;
  {$EXTERNALSYM VER_GREATER}
  VER_GREATER                     = 2;
  {$EXTERNALSYM VER_GREATER_EQUAL}
  VER_GREATER_EQUAL               = 3;
  {$EXTERNALSYM VER_LESS}
  VER_LESS                        = 4;
  {$EXTERNALSYM VER_LESS_EQUAL}
  VER_LESS_EQUAL                  = 5;
  {$EXTERNALSYM VER_AND}
  VER_AND                         = 6;
  {$EXTERNALSYM VER_OR}
  VER_OR                          = 7;

  {$EXTERNALSYM VER_CONDITION_MASK}
  VER_CONDITION_MASK              = 7;
  {$EXTERNALSYM VER_NUM_BITS_PER_CONDITION_MASK}
  VER_NUM_BITS_PER_CONDITION_MASK = 3;

  {$EXTERNALSYM  VER_NT_WORKSTATION}
  VER_NT_WORKSTATION              = $0000001;
  {$EXTERNALSYM  VER_NT_DOMAIN_CONTROLLER}
  VER_NT_DOMAIN_CONTROLLER        = $0000002;
  {$EXTERNALSYM VER_NT_SERVER}
  VER_NT_SERVER                   = $0000003;

  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_INTEL}
  PROCESSOR_ARCHITECTURE_INTEL           = 0;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_MIPS}
  PROCESSOR_ARCHITECTURE_MIPS            = 1;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_ALPHA}
  PROCESSOR_ARCHITECTURE_ALPHA           = 2;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_PPC}
  PROCESSOR_ARCHITECTURE_PPC             = 3;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_SHX}
  PROCESSOR_ARCHITECTURE_SHX             = 4;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_ARM}
  PROCESSOR_ARCHITECTURE_ARM             = 5;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_IA64}
  PROCESSOR_ARCHITECTURE_IA64            = 6;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_ALPHA64}
  PROCESSOR_ARCHITECTURE_ALPHA64         = 7;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_MSIL}
  PROCESSOR_ARCHITECTURE_MSIL            = 8;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_AMD64}
  PROCESSOR_ARCHITECTURE_AMD64           = 9;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_IA32_ON_WIN64}
  PROCESSOR_ARCHITECTURE_IA32_ON_WIN64   = 10;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_UNKNOWN}
  PROCESSOR_ARCHITECTURE_UNKNOWN         = $FFFF;

  //ntdef.h
  {$EXTERNALSYM VER_SERVER_NT}
  VER_SERVER_NT                       = $80000000;
  {$EXTERNALSYM VER_WORKSTATION_NT}
  VER_WORKSTATION_NT                  = $40000000;
  {$EXTERNALSYM VER_SUITE_SMALLBUSINESS}
  VER_SUITE_SMALLBUSINESS             = $00000001;
  {$EXTERNALSYM VER_SUITE_ENTERPRISE}
  VER_SUITE_ENTERPRISE                = $00000002;
  {$EXTERNALSYM VER_SUITE_BACKOFFICE}
  VER_SUITE_BACKOFFICE                = $00000004;
  {$EXTERNALSYM VER_SUITE_COMMUNICATIONS}
  VER_SUITE_COMMUNICATIONS            = $00000008;
  {$EXTERNALSYM VER_SUITE_TERMINAL}
  VER_SUITE_TERMINAL                  = $00000010;
  {$EXTERNALSYM VER_SUITE_SMALLBUSINESS_RESTRICTED}
  VER_SUITE_SMALLBUSINESS_RESTRICTED  = $00000020;
  {$EXTERNALSYM VER_SUITE_EMBEDDEDNT}
  VER_SUITE_EMBEDDEDNT                = $00000040;
  {$EXTERNALSYM VER_SUITE_DATACENTER}
  VER_SUITE_DATACENTER                = $00000080;
  {$EXTERNALSYM VER_SUITE_SINGLEUSERTS}
  VER_SUITE_SINGLEUSERTS              = $00000100;
  {$EXTERNALSYM VER_SUITE_PERSONAL}
  VER_SUITE_PERSONAL                  = $00000200;
  {$EXTERNALSYM  VER_SUITE_BLADE}
  VER_SUITE_BLADE                     = $00000400;
  {$EXTERNALSYM VER_SUITE_EMBEDDED_RESTRICTED}
  VER_SUITE_EMBEDDED_RESTRICTED       = $00000800;
  {$EXTERNALSYM VER_SUITE_SECURITY_APPLIANCE}
  VER_SUITE_SECURITY_APPLIANCE        = $00001000;
  {$EXTERNALSYM VER_SUITE_STORAGE_SERVER}
  VER_SUITE_STORAGE_SERVER            = $00002000;
  {$EXTERNALSYM VER_SUITE_COMPUTE_SERVER}
  VER_SUITE_COMPUTE_SERVER            = $00004000;
  {$EXTERNALSYM VER_SUITE_WH_SERVER}
  VER_SUITE_WH_SERVER                 = $00008000;

  //from sdkddkver.h
  {$EXTERNALSYM _WIN32_WINNT_NT4}
  _WIN32_WINNT_NT4                 = $0400;
  {$EXTERNALSYM _WIN32_WINNT_WIN2K}
  _WIN32_WINNT_WIN2K               = $0500;
  {$EXTERNALSYM _WIN32_WINNT_WINXP}
  _WIN32_WINNT_WINXP               = $0501;
  {$EXTERNALSYM _WIN32_WINNT_WS03}
  _WIN32_WINNT_WS03                = $0502;
  {$EXTERNALSYM _WIN32_WINNT_WIN6}
  _WIN32_WINNT_WIN6                = $0600;
  {$EXTERNALSYM _WIN32_WINNT_VISTA}
  _WIN32_WINNT_VISTA               = $0600;
  {$EXTERNALSYM _WIN32_WINNT_WS08}
  _WIN32_WINNT_WS08                = $0600;
  {$EXTERNALSYM _WIN32_WINNT_LONGHORN}
  _WIN32_WINNT_LONGHORN            = $0600;
  {$EXTERNALSYM _WIN32_WINNT_WIN7}
  _WIN32_WINNT_WIN7                = $0601;
  {$EXTERNALSYM _WIN32_WINNT_WIN8}
  _WIN32_WINNT_WIN8                = $0602;
  {$EXTERNALSYM _WIN32_WINNT_WINBLUE}
  _WIN32_WINNT_WINBLUE             = $0603;
   //WinUser.h
  {$EXTERNALSYM SM_TABLETPC}
  SM_TABLETPC             = 86;
  {$EXTERNALSYM SM_MEDIACENTER}
  SM_MEDIACENTER          = 87;
  {$EXTERNALSYM SM_STARTER}
  SM_STARTER              = 88;
  {$EXTERNALSYM SM_SERVERR2}
  SM_SERVERR2             = 89;
  //ntdef.h


//
// Product types
// This list grows with each OS release.
//
// There is no ordering of values to ensure callers
// do an equality test i.e. greater-than and less-than
// comparisons are not useful.
//
// NOTE: Values in this list should never be deleted.
//       When a product-type 'X' gets dropped from a
//       OS release onwards, the value of 'X' continues
//       to be used in the mapping table of GetProductInfo.
//
  {$EXTERNALSYM PRODUCT_UNDEFINED}
  PRODUCT_UNDEFINED                          = $00000000;

  {$EXTERNALSYM PRODUCT_ULTIMATE}
  PRODUCT_ULTIMATE                           = $00000001;
  {$EXTERNALSYM PRODUCT_HOME_BASIC}
  PRODUCT_HOME_BASIC                         = $00000002;
  {$EXTERNALSYM PRODUCT_HOME_PREMIUM}
  PRODUCT_HOME_PREMIUM                       = $00000003;
  {$EXTERNALSYM PRODUCT_ENTERPRISE}
  PRODUCT_ENTERPRISE                         = $00000004;
  {$EXTERNALSYM PRODUCT_HOME_BASIC_N}
  PRODUCT_HOME_BASIC_N                       = $00000005;
  {$EXTERNALSYM PRODUCT_BUSINESS}
  PRODUCT_BUSINESS                           = $00000006;
  {$EXTERNALSYM  PRODUCT_STANDARD_SERVER}
  PRODUCT_STANDARD_SERVER                    = $00000007;
  {$EXTERNALSYM PRODUCT_DATACENTER_SERVER}
  PRODUCT_DATACENTER_SERVER                  = $00000008;
  {$EXTERNALSYM PRODUCT_SMALLBUSINESS_SERVER}
  PRODUCT_SMALLBUSINESS_SERVER               = $00000009;
  {$EXTERNALSYM PRODUCT_ENTERPRISE_SERVER}
  PRODUCT_ENTERPRISE_SERVER                  = $0000000A;
  {$EXTERNALSYM PRODUCT_STARTER}
  PRODUCT_STARTER                            = $0000000B;
  {$EXTERNALSYM PRODUCT_DATACENTER_SERVER_CORE }
  PRODUCT_DATACENTER_SERVER_CORE             = $0000000C;
  {$EXTERNALSYM PRODUCT_STANDARD_SERVER_CORE}
  PRODUCT_STANDARD_SERVER_CORE               = $0000000D;
  {$EXTERNALSYM PRODUCT_ENTERPRISE_SERVER_CORE}
  PRODUCT_ENTERPRISE_SERVER_CORE             = $0000000E;
  {$EXTERNALSYM PRODUCT_ENTERPRISE_SERVER_IA64}
  PRODUCT_ENTERPRISE_SERVER_IA64             = $0000000F;
  {$EXTERNALSYM PRODUCT_BUSINESS_N}
  PRODUCT_BUSINESS_N                         = $00000010;
  {$EXTERNALSYM PRODUCT_WEB_SERVER}
  PRODUCT_WEB_SERVER                         = $00000011;
  {$EXTERNALSYM PRODUCT_CLUSTER_SERVER}
  PRODUCT_CLUSTER_SERVER                     = $00000012;
  {$EXTERNALSYM PRODUCT_HOME_SERVER}
  PRODUCT_HOME_SERVER                        = $00000013;
  {$EXTERNALSYM PRODUCT_STORAGE_EXPRESS_SERVER}
  PRODUCT_STORAGE_EXPRESS_SERVER             = $00000014;
  {$EXTERNALSYM PRODUCT_STORAGE_STANDARD_SERVER}
  PRODUCT_STORAGE_STANDARD_SERVER            = $00000015;
  {$EXTERNALSYM PRODUCT_STORAGE_WORKGROUP_SERVER}
  PRODUCT_STORAGE_WORKGROUP_SERVER           = $00000016;
  {$EXTERNALSYM PRODUCT_STORAGE_ENTERPRISE_SERVER}
  PRODUCT_STORAGE_ENTERPRISE_SERVER          = $00000017;
  {$EXTERNALSYM PRODUCT_SERVER_FOR_SMALLBUSINESS}
  PRODUCT_SERVER_FOR_SMALLBUSINESS           = $00000018;
  {$EXTERNALSYM PRODUCT_SMALLBUSINESS_SERVER_PREMIUM}
  PRODUCT_SMALLBUSINESS_SERVER_PREMIUM       = $00000019;
  {$EXTERNALSYM PRODUCT_HOME_PREMIUM_N}
  PRODUCT_HOME_PREMIUM_N                     = $0000001A;
  {$EXTERNALSYM PRODUCT_ENTERPRISE_N}
  PRODUCT_ENTERPRISE_N                       = $0000001B;
  {$EXTERNALSYM PRODUCT_ULTIMATE_N}
  PRODUCT_ULTIMATE_N                         = $0000001C;
  {$EXTERNALSYM PRODUCT_WEB_SERVER_CORE}
  PRODUCT_WEB_SERVER_CORE                    = $0000001D;
  {$EXTERNALSYM PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT}
  PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT   = $0000001E;
  {$EXTERNALSYM PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY}
  PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY     = $0000001F;
  {$EXTERNALSYM PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING }
  PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING    = $00000020;
  {$EXTERNALSYM PRODUCT_SERVER_FOUNDATION}
  PRODUCT_SERVER_FOUNDATION                  = $00000021;
  {$EXTERNALSYM PRODUCT_HOME_PREMIUM_SERVER}
  PRODUCT_HOME_PREMIUM_SERVER                = $00000022;
  {$EXTERNALSYM PRODUCT_SERVER_FOR_SMALLBUSINESS_V}
  PRODUCT_SERVER_FOR_SMALLBUSINESS_V         = $00000023;
  {$EXTERNALSYM PRODUCT_STANDARD_SERVER_V}
  PRODUCT_STANDARD_SERVER_V                  = $00000024;
  {$EXTERNALSYM PRODUCT_DATACENTER_SERVER_V}
  PRODUCT_DATACENTER_SERVER_V                = $00000025;
  {$EXTERNALSYM PRODUCT_ENTERPRISE_SERVER_V}
  PRODUCT_ENTERPRISE_SERVER_V                = $00000026;
  {$EXTERNALSYM PRODUCT_DATACENTER_SERVER_CORE_V}
  PRODUCT_DATACENTER_SERVER_CORE_V           = $00000027;
  {$EXTERNALSYM PRODUCT_STANDARD_SERVER_CORE_V}
  PRODUCT_STANDARD_SERVER_CORE_V             = $00000028;
  {$EXTERNALSYM PRODUCT_ENTERPRISE_SERVER_CORE_V}
  PRODUCT_ENTERPRISE_SERVER_CORE_V           = $00000029;
  {$EXTERNALSYM PRODUCT_HYPERV}
  PRODUCT_HYPERV                             = $0000002A;
  {$EXTERNALSYM PRODUCT_STORAGE_EXPRESS_SERVER_CORE}
  PRODUCT_STORAGE_EXPRESS_SERVER_CORE        = $0000002B;
  {$EXTERNALSYM PRODUCT_STORAGE_STANDARD_SERVER_CORE}
  PRODUCT_STORAGE_STANDARD_SERVER_CORE       = $0000002C;
  {$EXTERNALSYM PRODUCT_STORAGE_WORKGROUP_SERVER_CORE}
  PRODUCT_STORAGE_WORKGROUP_SERVER_CORE      = $0000002D;
  {$EXTERNALSYM PRODUCT_STORAGE_ENTERPRISE_SERVER_CORE}
  PRODUCT_STORAGE_ENTERPRISE_SERVER_CORE     = $0000002E;
  {$EXTERNALSYM PRODUCT_STARTER_N}
  PRODUCT_STARTER_N                          = $0000002F;
  {$EXTERNALSYM PRODUCT_PROFESSIONAL}
  PRODUCT_PROFESSIONAL                       = $00000030;
  {$EXTERNALSYM PRODUCT_PROFESSIONAL_N}
  PRODUCT_PROFESSIONAL_N                     = $00000031;
  {$EXTERNALSYM PRODUCT_SB_SOLUTION_SERVER}
  PRODUCT_SB_SOLUTION_SERVER                 = $00000032;
  {$EXTERNALSYM PRODUCT_SERVER_FOR_SB_SOLUTIONS}
  PRODUCT_SERVER_FOR_SB_SOLUTIONS            = $00000033;
  {$EXTERNALSYM PRODUCT_STANDARD_SERVER_SOLUTIONS}
  PRODUCT_STANDARD_SERVER_SOLUTIONS          = $00000034;
  {$EXTERNALSYM PRODUCT_STANDARD_SERVER_SOLUTIONS_CORE}
  PRODUCT_STANDARD_SERVER_SOLUTIONS_CORE     = $00000035;
  {$EXTERNALSYM PRODUCT_SB_SOLUTION_SERVER_EM}
  PRODUCT_SB_SOLUTION_SERVER_EM              = $00000036;
  {$EXTERNALSYM PRODUCT_SERVER_FOR_SB_SOLUTIONS_EM}
  PRODUCT_SERVER_FOR_SB_SOLUTIONS_EM         = $00000037;
  {$EXTERNALSYM PRODUCT_SOLUTION_EMBEDDEDSERVER}
  PRODUCT_SOLUTION_EMBEDDEDSERVER            = $00000038;
  {$EXTERNALSYM PRODUCT_SOLUTION_EMBEDDEDSERVER_CORE}
  PRODUCT_SOLUTION_EMBEDDEDSERVER_CORE       = $00000039;
  {$EXTERNALSYM PRODUCT_PROFESSIONAL_EMBEDDED}
  PRODUCT_PROFESSIONAL_EMBEDDED              = $0000003A;
  {$EXTERNALSYM PRODUCT_ESSENTIALBUSINESS_SERVER_MGMT}
  PRODUCT_ESSENTIALBUSINESS_SERVER_MGMT      = $0000003B;
  {$EXTERNALSYM PRODUCT_ESSENTIALBUSINESS_SERVER_ADDL}
  PRODUCT_ESSENTIALBUSINESS_SERVER_ADDL      = $0000003C;
  {$EXTERNALSYM PRODUCT_ESSENTIALBUSINESS_SERVER_MGMTSVC}
  PRODUCT_ESSENTIALBUSINESS_SERVER_MGMTSVC   = $0000003D;
  {$EXTERNALSYM PRODUCT_ESSENTIALBUSINESS_SERVER_ADDLSVC}
  PRODUCT_ESSENTIALBUSINESS_SERVER_ADDLSVC   = $0000003E;
  {$EXTERNALSYM PRODUCT_SMALLBUSINESS_SERVER_PREMIUM_CORE}
  PRODUCT_SMALLBUSINESS_SERVER_PREMIUM_CORE  = $0000003F;
  {$EXTERNALSYM PRODUCT_CLUSTER_SERVER_V}
  PRODUCT_CLUSTER_SERVER_V                   = $00000040;
  {$EXTERNALSYM PRODUCT_EMBEDDED}
  PRODUCT_EMBEDDED                           = $00000041;
  {$EXTERNALSYM PRODUCT_STARTER_E}
  PRODUCT_STARTER_E                          = $00000042;
  {$EXTERNALSYM PRODUCT_HOME_BASIC_E}
  PRODUCT_HOME_BASIC_E                       = $00000043;
  {$EXTERNALSYM PRODUCT_HOME_PREMIUM_E}
  PRODUCT_HOME_PREMIUM_E                     = $00000044;
  {$EXTERNALSYM PRODUCT_PROFESSIONAL_E}
  PRODUCT_PROFESSIONAL_E                     = $00000045;
  {$EXTERNALSYM PRODUCT_ENTERPRISE_E}
  PRODUCT_ENTERPRISE_E                       = $00000046;
  {$EXTERNALSYM PRODUCT_ULTIMATE_E}
  PRODUCT_ULTIMATE_E                         = $00000047;
  {$EXTERNALSYM PRODUCT_ENTERPRISE_EVALUATION}
  PRODUCT_ENTERPRISE_EVALUATION              = $00000048;
  {$EXTERNALSYM PRODUCT_MULTIPOINT_STANDARD_SERVER}
  PRODUCT_MULTIPOINT_STANDARD_SERVER         = $0000004C;
  {$EXTERNALSYM PRODUCT_MULTIPOINT_PREMIUM_SERVER}
  PRODUCT_MULTIPOINT_PREMIUM_SERVER          = $0000004D;
  {$EXTERNALSYM PRODUCT_STANDARD_EVALUATION_SERVER}
  PRODUCT_STANDARD_EVALUATION_SERVER         = $0000004F;
  {$EXTERNALSYM PRODUCT_DATACENTER_EVALUATION_SERVER}
  PRODUCT_DATACENTER_EVALUATION_SERVER       = $00000050;
  {$EXTERNALSYM PRODUCT_ENTERPRISE_N_EVALUATION}
  PRODUCT_ENTERPRISE_N_EVALUATION            = $00000054;
  {$EXTERNALSYM PRODUCT_EMBEDDED_AUTOMOTIVE}
  PRODUCT_EMBEDDED_AUTOMOTIVE                = $00000055;
  {$EXTERNALSYM PRODUCT_EMBEDDED_INDUSTRY_A}
  PRODUCT_EMBEDDED_INDUSTRY_A                = $00000056;
  {$EXTERNALSYM PRODUCT_THINPC}
  PRODUCT_THINPC                             = $00000057;
  {$EXTERNALSYM PRODUCT_EMBEDDED_A}
  PRODUCT_EMBEDDED_A                         = $00000058;
  {$EXTERNALSYM PRODUCT_EMBEDDED_INDUSTRY}
  PRODUCT_EMBEDDED_INDUSTRY                  = $00000059;
  {$EXTERNALSYM PRODUCT_EMBEDDED_E}
  PRODUCT_EMBEDDED_E                         = $0000005A;
  {$EXTERNALSYM PRODUCT_EMBEDDED_INDUSTRY_E}
  PRODUCT_EMBEDDED_INDUSTRY_E                = $0000005B;
  {$EXTERNALSYM PRODUCT_EMBEDDED_INDUSTRY_A_E}
  PRODUCT_EMBEDDED_INDUSTRY_A_E              = $0000005C;
  {$EXTERNALSYM PRODUCT_STORAGE_WORKGROUP_EVALUATION_SERVER}
  PRODUCT_STORAGE_WORKGROUP_EVALUATION_SERVER= $0000005F;
  {$EXTERNALSYM PRODUCT_STORAGE_STANDARD_EVALUATION_SERVER}
  PRODUCT_STORAGE_STANDARD_EVALUATION_SERVER = $00000060;
  {$EXTERNALSYM PRODUCT_CORE_ARM}
  PRODUCT_CORE_ARM                           = $00000061;
  {$EXTERNALSYM PRODUCT_CORE_N}
  PRODUCT_CORE_N                             = $00000062;
  {$EXTERNALSYM PRODUCT_CORE_COUNTRYSPECIFIC}
  PRODUCT_CORE_COUNTRYSPECIFIC               = $00000063;
  {$EXTERNALSYM PRODUCT_CORE_SINGLELANGUAGE}
  PRODUCT_CORE_SINGLELANGUAGE                = $00000064;
  {$EXTERNALSYM PRODUCT_CORE}
  PRODUCT_CORE                               = $00000065;
  {$EXTERNALSYM PRODUCT_PROFESSIONAL_WMC}
  PRODUCT_PROFESSIONAL_WMC                   = $00000067;
  {$EXTERNALSYM PRODUCT_MOBILE_CORE}
  PRODUCT_MOBILE_CORE                        = $00000068;
  {$EXTERNALSYM PRODUCT_EMBEDDED_INDUSTRY_EVAL}
  PRODUCT_EMBEDDED_INDUSTRY_EVAL             = $00000069;
  {$EXTERNALSYM PRODUCT_EMBEDDED_INDUSTRY_E_EVAL}
  PRODUCT_EMBEDDED_INDUSTRY_E_EVAL           = $0000006A;
  {$EXTERNALSYM PRODUCT_EMBEDDED_EVAL}
  PRODUCT_EMBEDDED_EVAL                      = $0000006B;
  {$EXTERNALSYM PRODUCT_EMBEDDED_E_EVAL}
  PRODUCT_EMBEDDED_E_EVAL                    = $0000006C;
  {$EXTERNALSYM PRODUCT_CORE_SERVER}
  PRODUCT_CORE_SERVER                        = $0000006D;
  {$EXTERNALSYM PRODUCT_CLOUD_STORAGE_SERVER}
  PRODUCT_CLOUD_STORAGE_SERVER               = $0000006E;

  {$EXTERNALSYM PRODUCT_UNLICENSED}
  PRODUCT_UNLICENSED                         = $ABCDABCD;

implementation
var KernelDLL : THandle = 0;

procedure GetKernelHandle; {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if KernelDLL = 0 then
    KernelDLL := GetModuleHandle('Kernel32.dll')
end;

function FixupStub(hDll: THandle; const AName:{$IFDEF WINCE}TIdUnicodeString{$ELSE}string{$ENDIF}): Pointer;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := Windows.GetProcAddress(hDll, {$IFDEF WINCE}PWideChar{$ELSE}PChar{$ENDIF}(AName));
end;

//stub section
function stub_GetProductInfo(const dwOSMajorVersion : DWORD;
    const dwOSMinorVersion : DWORD;
    const dwSpMajorVersion : DWORD;
    const dwSpMinorVersion : DWORD;
    var pdwReturnedProductType : DWORD) : BOOL stdcall;
begin
  GetKernelHandle;
  GetProductInfo:= FixupStub( KernelDLL, 'GetProductInfo');
  if Assigned(GetProductInfo) then begin
    Result := GetProductInfo(dwOSMajorVersion,dwOSMinorVersion, dwSpMajorVersion, dwSpMinorVersion, pdwReturnedProductType);
  end else begin
    GetProductInfo := stub_GetProductInfo;
    SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
    Result := False;
  end;
end;

procedure stub_GetSystemInfo(out lpSystemInfo :  _SYSTEM_INFO) stdcall;
begin
  GetKernelHandle;
  GetNativeSystemInfo := FixupStub( KernelDLL, 'GetSystemInfo');

  if Assigned(GetNativeSystemInfo) then begin
    GetNativeSystemInfo(lpSystemInfo);
  end else begin
    GetNativeSystemInfo := stub_GetSystemInfo;
    SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
  end;
end;

procedure stub_GetNativeSystemInfo(out lpSystemInfo :  _SYSTEM_INFO) stdcall;
begin
  GetKernelHandle;
  GetNativeSystemInfo := FixupStub( KernelDLL, 'GetNativeSystemInfo');

  if Assigned(GetNativeSystemInfo) then begin
    GetNativeSystemInfo(lpSystemInfo);
  end else begin
    GetNativeSystemInfo := stub_GetNativeSystemInfo;
    SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
  end;
end;

function stub_VerifyVersionInfoW(var VersionInformation : OSVERSIONINFOEXW;
    const dwTypeMask : DWORD;
    const dwlConditionMask : DWORDLONG) : BOOL stdcall;
begin
  GetKernelHandle;
  VerifyVersionInfoW := FixupStub( KernelDLL, 'VerifyVersionInfoW');
  if Assigned(VerifyVersionInfoW) then begin
    Result := VerifyVersionInfoW(VersionInformation,dwTypeMask,dwlConditionMask);
  end else begin
    VerifyVersionInfoW := stub_VerifyVersionInfoW;
    SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
    Result := False;
  end;
end;

function stub_VerSetConditionMask(const ConditionMask : ULONGLONG;
   const TypeMask : DWORD;
   const Condition : BYTE ) : ULONGLONG stdcall;
begin
  GetKernelHandle;
  @VerSetConditionMask :=  FixupStub( KernelDLL, 'VerSetConditionMask');
  if Assigned(VerSetConditionMask) then begin
    Result := VerSetConditionMask(ConditionMask,TypeMask, Condition);
  end else begin
    VerSetConditionMask := stub_VerSetConditionMask;

// For documentation sakes here's the old version of the macro that got
// changed to call an API
//         VER_SET_CONDITION(_m_,_t_,_c_)  _m_=(_m_|(_c_<<(1<<_t_)))
//
    Result :=  ConditionMask or (Condition shl (1 shl TypeMask));
  end;
end;

function stub_IsWow64Process(hProcess : THANDLE; out Wow64Process : BOOL) : BOOL stdcall;
begin
  GetKernelHandle;
  @IsWow64Process :=  FixupStub( KernelDLL, 'IsWow64Process');
  if Assigned(IsWow64Process) then begin
    Result :=  IsWow64Process(hProcess,Wow64Process);
  end else begin
    IsWow64Process := stub_IsWow64Process;
    SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
    Result := False;
  end;
end;
// end stubs

//initialize stubs
procedure InitStuds;
begin
  GetSystemInfo := stub_GetSystemInfo;
  GetNativeSystemInfo := stub_GetNativeSystemInfo;
  VerifyVersionInfoW := stub_VerifyVersionInfoW;
  VerSetConditionMask := stub_VerSetConditionMask;
  IsWow64Process := stub_IsWow64Process;
  GetProductInfo := stub_GetProductInfo;
end;

function IsWindowsVersionOrGreater(const wMajorVersion, wMinorVersion, wServicePackMajor : Word) : Boolean;
var
  osvi : TOSVERSIONINFOEXW;
  dwlConditionMask : DWORDLONG;
begin
  FillChar(osvi,SizeOf(osvi),0);
  osvi.dwOSVersionInfoSize := SizeOf(osvi);
  dwlConditionMask :=
    VerSetConditionMask(
      VerSetConditionMask(
        VerSetConditionMask(
            0, VER_MAJORVERSION, VER_GREATER_EQUAL),
      VER_MINORVERSION, VER_GREATER_EQUAL),
    VER_SERVICEPACKMAJOR, VER_GREATER_EQUAL);
  osvi.dwMajorVersion := wMajorVersion;
  osvi.dwMinorVersion := wMinorVersion;
  osvi.wServicePackMajor := wServicePackMajor;
  Result := VerifyVersionInfoW(osvi, VER_MAJORVERSION or VER_MINORVERSION or VER_SERVICEPACKMAJOR, dwlConditionMask) <> False;
end;

function IsWindowsXPOrGreater : Boolean;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_WINXP), LOBYTE(_WIN32_WINNT_WINXP),0);
end;

function IsWindowsXPSP1OrGreater : Boolean;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_WINXP), LOBYTE(_WIN32_WINNT_WINXP), 1);
end;

function IsWindowsXPSP2OrGreater : Boolean;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_WINXP), LOBYTE(_WIN32_WINNT_WINXP), 2);
end;

function IsWindowsXPSP3OrGreater : Boolean;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_WINXP), LOBYTE(_WIN32_WINNT_WINXP), 3);
end;

function IsWindowsVistaOrGreater : Boolean;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_VISTA), LOBYTE(_WIN32_WINNT_VISTA), 0);
end;

function IsWindowsVistaSP1OrGreater : Boolean;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_VISTA), LOBYTE(_WIN32_WINNT_VISTA), 1);
end;

function IsWindowsVistaSP2OrGreater : Boolean;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_VISTA), LOBYTE(_WIN32_WINNT_VISTA), 2);
end;

function IsWindows7OrGreater : Boolean;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_WIN7), LOBYTE(_WIN32_WINNT_WIN7), 0);
end;

function IsWindows7SP1OrGreater : Boolean;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_WIN7), LOBYTE(_WIN32_WINNT_WIN7), 1);
end;

function IsWindows8OrGreater: Boolean;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_WIN8), LOBYTE(_WIN32_WINNT_WIN8), 0);
end;

function IsWindows8Point1OrGreater: Boolean;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_WINBLUE), LOBYTE(_WIN32_WINNT_WINBLUE), 0);
end;

function IsWindowsServer : Boolean;
var
  osvi : OSVERSIONINFOEXW;
  dwlConditionMask : DWORDLONG;
begin
  FillChar(osvi,SizeOf(osvi),0);
  osvi.dwOSVersionInfoSize := SizeOf(osvi);
  osvi.wProductType := VER_NT_WORKSTATION;
  dwlConditionMask := VerSetConditionMask( 0, VER_PRODUCT_TYPE, VER_EQUAL );
  Result := not VerifyVersionInfoW(&osvi, VER_PRODUCT_TYPE, dwlConditionMask);
end;

initialization
  InitStuds;
end.
