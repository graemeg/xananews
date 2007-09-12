unit NewsReader3_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 15/05/2003 15:58:25 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Development\delphi7\NewsReader3\NewsReader3.tlb (1)
// LIBID: {E1CB5E8C-DFBE-4847-B6EF-4717023AEB57}
// LCID: 0
// Helpfile: 
// HelpString: NewsReader3 Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  NewsReader3MajorVersion = 1;
  NewsReader3MinorVersion = 0;

  LIBID_NewsReader3: TGUID = '{E1CB5E8C-DFBE-4847-B6EF-4717023AEB57}';

  IID_IInternetProtocol: TGUID = '{0AC15E7E-67EC-48C7-9681-C8C26B589E4E}';
  CLASS_CIDMIMEHandler: TGUID = '{C2AED258-7AB6-4E10-91F1-AA17019DA2A4}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IInternetProtocol = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  CIDMIMEHandler = IInternetProtocol;


// *********************************************************************//
// Interface: IInternetProtocol
// Flags:     (256) OleAutomation
// GUID:      {0AC15E7E-67EC-48C7-9681-C8C26B589E4E}
// *********************************************************************//
  IInternetProtocol = interface(IUnknown)
    ['{0AC15E7E-67EC-48C7-9681-C8C26B589E4E}']
  end;

// *********************************************************************//
// The Class CoCIDMIMEHandler provides a Create and CreateRemote method to          
// create instances of the default interface IInternetProtocol exposed by              
// the CoClass CIDMIMEHandler. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCIDMIMEHandler = class
    class function Create: IInternetProtocol;
    class function CreateRemote(const MachineName: string): IInternetProtocol;
  end;

implementation

uses ComObj;

class function CoCIDMIMEHandler.Create: IInternetProtocol;
begin
  Result := CreateComObject(CLASS_CIDMIMEHandler) as IInternetProtocol;
end;

class function CoCIDMIMEHandler.CreateRemote(const MachineName: string): IInternetProtocol;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CIDMIMEHandler) as IInternetProtocol;
end;

end.
