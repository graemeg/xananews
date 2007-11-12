{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     ASN1 Unit                                         }
{
  LICENSE

  Copyright (c) 2004, Henrick Wibell Hellström, StreamSec
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the name of StreamSec nor the names of its contributors may be
      used to endorse or promote products derived from this software without
      specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
{*******************************************************}
{$I ver.inc}
unit Asn1;

interface

uses
  Classes, SysUtils, SyncObjs,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc, DateUtils,
  {$ENDIF}
  {$IFDEF D6UP}
  Variants,
  {$ENDIF}
  SecUtils, MpArithTypes, HashList;

const
  V_ASN1_UNIVERSAL                    = $00;
  V_ASN1_APPLICATION                  = $40;
  V_ASN1_CONTEXT_SPECIFIC             = $80;
  V_ASN1_PRIVATE                      = $c0;

  V_ASN1_CONSTRUCTED                  = $20;
  V_ASN1_PRIMITIVE_TAG                = $1f;

  V_ASN1_UNDEF                        = -1;
  V_ASN1_EOC                          = 0;
  V_ASN1_BOOLEAN                      = 1;
  V_ASN1_INTEGER                      = 2;
  V_ASN1_BIT_STRING                   = 3;
  V_ASN1_OCTET_STRING                 = 4;
  V_ASN1_NULL                         = 5;
  V_ASN1_OBJECT                       = 6;
  V_ASN1_OBJECT_DESCRIPTOR            = 7;
  V_ASN1_EXTERNAL                     = 8;
  V_ASN1_REAL                         = 9;
  V_ASN1_ENUMERATED                   = 10;
  V_ASN1_UTF8STRING                   = 12;
  V_ASN1_SEQUENCE                     = 16;
  V_ASN1_SET                          = 17;
  V_ASN1_NUMERICSTRING                = 18;
  V_ASN1_PRINTABLESTRING              = 19;
  V_ASN1_T61STRING                    = 20;
  V_ASN1_TELETEXSTRING                = 20;    (* alias *)
  V_ASN1_VIDEOTEXSTRING               = 21;
  V_ASN1_IA5STRING                    = 22;
  V_ASN1_UTCTIME                      = 23;
  V_ASN1_GENERALIZEDTIME              = 24;
  V_ASN1_GRAPHICSTRING                = 25;
  V_ASN1_ISO64STRING                  = 26;
  V_ASN1_VISIBLESTRING                = 26;    (* alias *)
  V_ASN1_GENERALSTRING                = 27;
  V_ASN1_UNIVERSALSTRING              = 28;
  V_ASN1_BMPSTRING                    = 30;

  UniversalTypes: array [0..30] of string = (
    'EOC','BOOLEAN','INTEGER','BIT STRING','OCTET STRING','NULL','OBJECT',
    'OBJECT-DESCRIPTOR','EXTERNAL','REAL','ENUMERATED','[UNIVERSAL 11]',
    'UTF8STRING','[UNIVERSAL 13]','[UNIVERSAL 14]','[UNIVERSAL 15]',
    'SEQUENCE','SET','NumericString','PrintableString','TeletexString',
    'VideotexString','IA5String','UTCTime','GeneralizedTime','GraphicString',
    'VisibleString','GeneralString','UniversalString','[UNIVERSAL 29]',
    'BMPString');

  // MappedUnivTypes is used by the ASN2Pascal expert to determine the OP type
  // of properties corresponding to primitive ASN.1 types.
  MappedUnivTypes: array [0..30] of string = (
    '','Boolean','TIntegerWrapper','TBitString','TOctetString','','ObjectIdentifier',
    'ObjectDescriptor','','Extended','Integer','',
    'WideString','','','',
    '','','NumericString','PrintableString','TeletexString',
    'VideotexString','IA5String','TUTCTime','TGeneralizedTime','GraphicString',
    'VisibleString','GeneralString','UniversalString','',
    'WideString');
  // WrapperUnivTypes is used by the ASN2Pascal expert to select the
  // internal wrapper class.
  WrapperUnivTypes: array [0..30] of string = (
    '','TBooleanWrapper','TIntegerWrapper','TBitString','TOctetString','','TStringWrapper',
    'TStringWrapper','','TRealWrapper','TEnumeratedWrapper','',
    'TWideStringWrapper','','','',
    '','','TStringWrapper','TStringWrapper','TStringWrapper',
    'TStringWrapper','TStringWrapper','TUTCTime','TGeneralizedTime','TStringWrapper',
    'TStringWrapper','TStringWrapper','TStringWrapper','',
    'TWideStringWrapper');

  fmtDER = 1;
  fmtASN1 = 2;
  fmtDOM = 3;

type
  {$IFNDEF BCB}
  ObjectIdentifier = SecUtils.ObjectIdentifier;
  {$ENDIF}
  ObjectDescriptor = string;
  NumericString = string;
  PrintableString = string;
  TeletexString = string;
  VideotexString = string;
  IA5String = string;
  GraphicString = string;
  VisibleString = string;
  GeneralString = string;
  UniversalString = string;
  TimeString = string;
  IntegerString = string;

  TReadResumeStream = class(TStream)
  private
    FDataStream: TStream;
    FBufferStream: TStream;
    procedure SetDataStream(const Value: TStream);
  public
    constructor Create;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property DataStream: TStream read FDataStream write SetDataStream;
  end;

  TChangeType = (ctCnt,ctBrowse,ctNewRec,ctDelRec,ctDecl);
  TChangeTypes = set of TChangeType;

  TECP2OSFormat = (cCompressed,cUncompressed,cHybrid);
  
  {$IFNDEF BCB}
  OctetString = SecUtils.OctetString;
  {$ENDIF}

  TASN1Struct = class;

  TASN1FieldEvent = procedure (Sender: TObject; Field: TASN1Struct) of object;
  TASN1CaptureEvent = procedure (Sender: TObject;
                                 Field: TASN1Struct;
                                 Stream: TStream;
                                 Length: Integer;
                                 var Count: Integer;
                                 var Done: Boolean) of object;
  TASN1ChangeEvent = procedure (Sender: TObject;
                                Field: TASN1Struct;
                                ChangeTypes: TChangeTypes) of object;


  PASN1Structs = ^TASN1Structs;
  PASN1Struct = ^TASN1Struct;
  TASN1Structs = packed record
    ItemCount: Integer;
    Items: packed array [0..MaxListSize] of TASN1Struct;
  end;

  EInvalidTag = class(Exception);

  {$IFNDEF MSWINDOWS}
  TSystemTime = record
    wYear: Word;
    wMonth: Word;
    wDayOfWeek: Word;
    wDay: Word;
    wHour: Word;
    wMinute: Word;
    wSecond: Word;
    wMilliseconds: Word;
  end;
  {$ENDIF}

  TASN1Struct = class
  private
    FCls: Byte;
    FTag: Cardinal;
    FLength: Integer;
    FCapacity: Integer;
    FTypeName: string;
    FVarName: string;
    FConstructed: Boolean;
    Cnt: Pointer;
    FChoices: PASN1Structs;
    FDefault: OctetString;
    FOptional: Boolean;
    FHasDefaultValue: Boolean;
    FImplicit: Boolean;
    FPersistent: Boolean;
    FTemplate: TASN1Struct;
    FEncapsulated: TASN1Struct;
    FRefCount: Integer;
    FIdenAndLen: packed array [0..10] of Byte;
    FIdenAndLenCountLoad: Integer;
    FIdenAndLenCountSave: Integer;
    FContentCountLoad: Integer;
    FContentCountSave: Integer;
    FOwner: TASN1Struct;
    FCaptureContent: Boolean;
    FOnCaptureLoad: TASN1CaptureEvent;
    FOnCaptureSave: TASN1CaptureEvent;
    FBeforeSave: TASN1FieldEvent;
    FBeforeLoad: TASN1FieldEvent;
    FAfterSave: TASN1FieldEvent;
    FAfterLoad: TASN1FieldEvent;
    FDoneLoading: Boolean;
    FDoneSaving: Boolean;
    FOnSave: TASN1FieldEvent;
    FOnLoad: TASN1FieldEvent;
    FAllowResume: Boolean;
    FResumeStream: TReadResumeStream;
    FImplicitTag: Cardinal;
    FImplicitTypeName: string;
    FChoiceTypeName: string;
    FChoiceVarName: string;
    FChoiceIndex: Integer;
    FAliasTypeName: string;
    FOnLoadStructure: TASN1FieldEvent;
    FNoSave: Boolean;
    FIDField: string;
    FIDFieldStruct: TASN1Struct;
    FBERIdentifiedBy: string;
    FIdentifies: TStrings;
    FTypeIdentified: Boolean;
    FUpdateCount: Integer;
    FChanges: TChangeTypes;
    FOnChange: TASN1ChangeEvent;
    FCurrentRecNo: Integer;
    FCurrentRecord: TASN1Struct;
    FInserting: Boolean;
    FEditing: Boolean;
    FModuleName: string;
    FDestroying: Boolean;
    FNoChange: Boolean;
    FReadOnly: Boolean;
    FReadOnlyCnt: Pointer;
    FReadOnlyCntLen: Integer;
    FReadOnlyCntSlave: Pointer;
    FReadOnlyCntLenSlave: Integer;
    FModifiedChild: Boolean;
    FUnambiguousField: Integer;
    FUFTested: Integer;
    FEstTotalCount: Integer;
    FStrictTagging: Boolean;
    FForgetOptionalDefault: Boolean;
    function GetItemCount: Integer;
    function GetItems(index: Integer): PASN1Struct;
    procedure SetItems(index: Integer; const Value: PASN1Struct);
    procedure SetCls(const Value: Byte);
    procedure SetConstructed(const Value: Boolean);
    procedure SetTag(const Value: Cardinal);
    procedure SetTypeName(const Value: string);
    procedure SetLength(const Value: Integer);
    function GetTemplate: TASN1Struct;      
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCaptureContent(const Value: Boolean);
    procedure SetAfterLoad(const Value: TASN1FieldEvent);
    procedure SetAfterSave(const Value: TASN1FieldEvent);
    procedure SetBeforeLoad(const Value: TASN1FieldEvent);
    procedure SetBeforeSave(const Value: TASN1FieldEvent);
    procedure SetOnCaptureLoad(const Value: TASN1CaptureEvent);
    procedure SetOnCaptureSave(const Value: TASN1CaptureEvent);
    procedure SetDoneLoading(const Value: Boolean);
    procedure SetDoneSaving(const Value: Boolean);
    procedure SetOnLoad(const Value: TASN1FieldEvent);
    procedure SetOnSave(const Value: TASN1FieldEvent);
    procedure SetAllowResume(const Value: Boolean);
    procedure SetImplicitTag(const Value: Cardinal);
    procedure SetImplicitTypeName(const Value: string);
    function GetChoiceCount: Integer;
    function GetChoices(index: Integer): PASN1Struct;
    procedure SetChoices(index: Integer; const Value: PASN1Struct);
    procedure SetChoiceCount(const Value: Integer);
    procedure SetImplicit(const Value: Boolean);
    procedure SetOnLoadStructure(const Value: TASN1FieldEvent);
    function GetIdenAndLen: string;
    procedure SetNoSave(const Value: Boolean);
    procedure SetIdenAndLen(const Value: string);
    procedure SetIDField(const Value: string);
    procedure SetOnChange(const Value: TASN1ChangeEvent);
    function GetCurrentRecord: PASN1Struct;
    function GetCurrentRecNo: Integer;
    function GetIdentifiedBy: string;
    procedure SetIdentifiedBy(const Value: string);
    procedure SetModuleName(const Value: string);
    procedure SetNoChange(const Value: Boolean);
    procedure SetReadOnly(const Value: Boolean);
    function GetActualTag: Cardinal;
    function GetFields(const FieldName: string): PASN1Struct;
    procedure SetFields(const FieldName: string; const Value: PASN1Struct);
    function GetEncapsulated: TASN1Struct;
    function GetTypeIdentified: Boolean;
    function GetIDField: string;
    procedure SetForgetOptionalDefault(const Value: Boolean);
    procedure SetChoiceCountEx(const Value: Integer; Garbage: THashList);
  protected
    procedure Change(ChangeType: TChangeType);
    procedure CopyTypeInfoAsField(Struct: TASN1Struct);
    procedure Dispose(Garbage: THashList);
    procedure DoBeforeLoad(Field: TASN1Struct); {$IFDEF THIN_TASN1STRUCT}virtual;{$ENDIF}
    procedure DoAfterLoad(Field: TASN1Struct); {$IFDEF THIN_TASN1STRUCT}virtual;{$ENDIF}
    procedure DoBeforeSave(Field: TASN1Struct); {$IFDEF THIN_TASN1STRUCT}virtual;{$ENDIF}
    procedure DoAfterSave(Field: TASN1Struct); {$IFDEF THIN_TASN1STRUCT}virtual;{$ENDIF}
    function DoCaptureLoad(Field: TASN1Struct;
                           Stream: TStream;
                           var Done: Boolean): Integer; {$IFDEF THIN_TASN1STRUCT}virtual;{$ENDIF}
    function DoCaptureSave(Field: TASN1Struct;
                           Stream: TStream;
                           var Done: Boolean): Integer; {$IFDEF THIN_TASN1STRUCT}virtual;{$ENDIF}
    procedure DoChange; {$IFDEF THIN_TASN1STRUCT}virtual;{$ENDIF}
    procedure DoLoad(Field: TASN1Struct); {$IFDEF THIN_TASN1STRUCT}virtual;{$ENDIF}
    procedure DoSave(Field: TASN1Struct); {$IFDEF THIN_TASN1STRUCT}virtual;{$ENDIF}
    procedure DoLoadStructure(Field: TASN1Struct); {$IFDEF THIN_TASN1STRUCT}virtual;{$ENDIF}
    procedure DoModifiedChild;
    function FindIDField: PASN1Struct;
    procedure InsertItemEx(Item: TASN1Struct; Index: Integer);
    procedure InternalBrowse(ARecNo: Integer; Silent: Boolean = False);
    function IsChoiceTemplate(Struct: TASN1Struct): Boolean;
    procedure ItemChanged(Field: TASN1Struct; ChangeTypes: TChangeTypes);
    procedure ParentBrowseHereSilently;
    procedure PropagateChoices(Template: TASN1Struct);
    procedure RemoveItem(Item: TASN1Struct; Garbage: THashList = nil);
    procedure RemoveItemRecurse(Item: TASN1Struct);
    procedure SetToDefault;
    function TotalCount: Integer;
    function UnambiguousField(ItemIndex: Integer): Boolean;
    property IdenAndLenCountSave: Integer read  FIdenAndLenCountSave
                                          write FIdenAndLenCountSave;
    property IdenAndLenCountLoad: Integer read FIdenAndLenCountLoad;
    property BERIdentifiedBy: string read FBERIdentifiedBy;
    property Identifies: TStrings read FIdentifies;
    property Editing: Boolean read FEditing;
    property Inserting: Boolean read FInserting;
    property AliasTypeName: string read FAliasTypeName;
  public
    constructor Create;
    destructor Destroy; override;
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
    function _AddRef: Integer;
    function _Release(Garbage: THashList = nil): Integer;
    function AddChoice(const AVarName, ATypeName: string): PASN1Struct;
    function AddField: PASN1Struct; overload;
    function AddField(ChoiceIndex: Integer): PASN1Struct; overload;
    function AddField(const AVarName: string;
                      const ATypeName: string;
                      AValue: TASN1Struct): PASN1Struct; overload;
    function AddField(const AVarName: string;
                      const ATypeName: string;
                      const AValue: WideString;
                      APersistent: Boolean): PASN1Struct; overload;
    function AddBooleanField(const AVarName: string;
                             AValue: Boolean;
                             APersistent: Boolean): PASN1Struct;
    function AddGeneralizedTimeField(const AVarName: string;
                                     AValue: TDateTime;
                                     APersistent: Boolean): PASN1Struct;
    function AddIntegerField(const AVarName: string;
                             AValue: Int64;
                             StoreAsUnsigned: Boolean;
                             APersistent: Boolean): PASN1Struct; overload;
    function AddIntegerField(const AVarName: string;
                             AValue: PMPInteger;
                             StoreAsUnsigned: Boolean;
                             APersistent: Boolean): PASN1Struct; overload;
    function AddRealField(const AVarName: string;
                          AValue: Extended;
                          APersistent: Boolean): PASN1Struct;
    function AddUTCTimeField(const AVarName: string;
                             AValue: TDateTime;
                             APersistent: Boolean): PASN1Struct;
    procedure AllocContents(ItemCount: Cardinal);
    procedure Assign(Source: TASN1Struct);
    function BeginUpdate: Integer;

    procedure BrowseFirst;
    procedure BrowseLast;
    procedure BrowseNext;
    procedure BrowsePrior;

    function CalculateLength: Integer;

    procedure CancelItem;

    procedure Clear;
    function Content: PChar;
    procedure ContentAsASN1Struct(var Struct: TASN1Struct);
    procedure ContentAsMPInt(var X: PMPInteger);
    procedure ContentAsUMPInt(var X: PMPInteger);
    function ContentAsECPoint(var P: TECPoint; const C: TECurve): Boolean;
    function ContentAsBoolean: Boolean;
    function ContentAsInteger: Integer;
    function ContentAsCardinal: Cardinal;
    function ContentAsInt64: Int64;
    function ContentAsUInt64: Int64;
    procedure ContentAsOctetString(var OS: ISecretKey); overload;
    function ContentAsOctetString: string; overload;
    function ContentAsOID: string;
    function ContentAsReal: Extended;
    function ContentAsString: WideString;
    function ContentAsDateTime: TDateTime;
    function ContentAsSystemTime(var T: TSystemTime): Boolean;
    function ContentAsVariant: Variant;
    function Contents: PASN1Structs;
    procedure CopyTypeInfo(const Source: TASN1Struct; Flat: Boolean = False);
    procedure CreateEncapsulated;
    procedure CreateOFTemplate;
    procedure DeleteChoice(Index: Integer);
    procedure DeleteItem; overload;
    procedure DeleteItem(Index: Integer; Garbage: THashList = nil); overload;
    function DisplayContent: WideString;
    procedure DisposeContent(Garbage: THashList = nil);
    procedure EditContent(const AValue: WideString); overload;
    procedure EditContent(const AValue: Boolean); overload;
    procedure EditContent(const AValue: Integer); overload;
    procedure EditContent(const AValue: Cardinal); overload;
    procedure EditContent(const AValue: Int64); overload;
    procedure EditContent(const AValue: Int64; StoreAsUnsigned: Boolean); overload;
    procedure EditContent(const AValue: PMPInteger; StoreAsUnsigned: Boolean); overload;
    procedure EditContent(const AValue: TECPoint;
                          const C: TECurve; Format: TECP2OSFormat); overload;
    procedure EditContent(const AValue: TDateTime); overload;
    procedure EditContent(const AValue: TSystemTime); overload;
    procedure EditContent(const AValue: Extended); overload;
    procedure EditContent(const AValue: TASN1Struct); overload;   
    procedure EditContent(const AValue: TASN1Struct; PadAlign: Byte); overload;
    function EditField(AVarName: string; const AValue: WideString): Boolean; overload;
    function EditField(AVarName: string; const AValue: Boolean): Boolean; overload;
    function EditField(AVarName: string; const AValue: Integer): Boolean; overload;
    function EditField(AVarName: string; const AValue: Cardinal): Boolean; overload;
    function EditField(AVarName: string; const AValue: Int64): Boolean; overload;
    function EditField(AVarName: string; const AValue: Int64; StoreAsUnsigned: Boolean): Boolean; overload;
    function EditField(AVarName: string; const AValue: PMPInteger; StoreAsUnsigned: Boolean): Boolean; overload;
    function EditField(AVarName: string; const AValue: TECPoint;
                       const C: TECurve; Format: TECP2OSFormat): Boolean; overload;
    function EditField(AVarName: string; const AValue: TDateTime): Boolean; overload;
    function EditField(AVarName: string; const AValue: Extended): Boolean; overload;
    function EditField(AVarName: string; const AValue: TASN1Struct): Boolean; overload;

    procedure EditItem;

    function EndUpdate: Integer;
    function FindField(AVarName: string): PASN1Struct;
    function FindBrowseableParent: TASN1Struct;
    function GetNamePath(BrowsePath: Boolean = False): string;

    procedure ImposeTypeInfo(const Source: TASN1Struct; Flat: Boolean = False);

    function IndexOf(AItem: TASN1Struct): Integer;

    procedure InsertItem;

    function IsEmpty: Boolean;
    function IsParent(const Struct: TASN1Struct): Boolean;
    function IsSubItem(const Struct: TASN1Struct): Boolean;

    function ListBrowseableFields(List: TStrings;
                                  BrowsePath, FullPath, Recurse: Boolean): Integer;
    function ListConstructedFields(List: TStrings; BrowsePath, FullPath, Recurse: Boolean): Integer;
    function ListFields(List: TStrings; BrowsePath, FullPath, Recurse: Boolean): Integer;
    function ListObjectFields(List: TStrings;
                              MaxIndex: Integer;
                              BrowsePath, FullPath: Boolean): Integer;
    function ListPrimitiveFields(List: TStrings; BrowsePath, FullPath, Recurse: Boolean): Integer;
    function ListTypedFields(List: TStrings;
                             UniversalTag: Cardinal;
                             BrowsePath, FullPath, Recurse: Boolean): Integer; overload;
    function ListTypedFields(List: TStrings;
                             TypeName: string;
                             BrowsePath, FullPath, Recurse: Boolean): Integer; overload;
                                                                     
    function LoadFromFile(AFileName: string; Format: Byte): Integer;
    function LoadFromStream(AStream: TStream; Format: Byte): Integer;
    procedure LoadFromStrings(AStrings: TStrings;
                              DeclaredTypes: TStrings = nil;
                              DeclIndex: TStrings = nil);

    procedure ParentBrowseHere;

    procedure PostItem;

    procedure ResetStreaming;
    function SaveToFile(AFileName: string; Format: Byte;
                        DeclaredTypes: TStrings = nil): Integer;
    function SaveToStream(AStream: TStream; Format: Byte;
                          DeclaredTypes: TStrings = nil): Integer;
    procedure SelectChoice(ChoiceIndex: Integer); overload;
    procedure SelectChoice(ChoiceTempl: PASN1Struct); overload;
    procedure SetAsDefault;
    procedure SetContent(const Buf; Count: Integer);
    procedure SetTypeNameNoCheck(const ATypeName: string);
    procedure SortSET;
    function TrySelectChoice(ACls: Byte; AConstructed: Boolean; ATag: Cardinal): Boolean;
    procedure TypeIdentify; overload;
    procedure TypeIdentify(var Template: PASN1Struct); overload;
    procedure TypeIdentify(var Template: PASN1Struct;
                           var Index: Integer;
                           Gently: Boolean = False); overload;
    property Cls: Byte read FCls write SetCls;
    property Tag: Cardinal read FTag write SetTag;
    property Length: Integer read FLength write SetLength;
    property TypeName: string read FTypeName write SetTypeName;
    property VarName: string read FVarName write FVarName;
    property Constructed: Boolean read FConstructed write SetConstructed;
    property ItemCount: Integer read GetItemCount;
    property Items[index: Integer]: PASN1Struct read GetItems write SetItems;
    property Fields[const FieldName: string]: PASN1Struct read GetFields write SetFields; default;
    property ChoiceCount: Integer read GetChoiceCount write SetChoiceCount;
    property Choices[index: Integer]: PASN1Struct read GetChoices write SetChoices;
    property ChoiceTypeName: string read FChoiceTypeName;
    property ChoiceVarName: string read FChoiceVarName;
    property CurrentRecNo: Integer read GetCurrentRecNo;
    property CurrentRecord: PASN1Struct read GetCurrentRecord;
    property HasDefaultValue: Boolean read FHasDefaultValue write FHasDefaultValue;
    property Default: OctetString read FDefault write FDefault;
    property Encapsulated: TASN1Struct read GetEncapsulated;
    property IdenAndLen: string read GetIdenAndLen write SetIdenAndLen;
    property IdentifiedBy: string read GetIdentifiedBy write SetIdentifiedBy;
    property IDField: string read GetIDField write SetIDField;
    property Implicit: Boolean read FImplicit write SetImplicit;
    property ImplicitTag: Cardinal read FImplicitTag write SetImplicitTag;
    property ActualTag: Cardinal read GetActualTag;
    property ImplicitTypeName: string read FImplicitTypeName write SetImplicitTypeName;
    property ModuleName: string read FModuleName write SetModuleName;
    property Optional: Boolean read FOptional write FOptional;
    property Persistent: Boolean read FPersistent write FPersistent;
    property Template: TASN1Struct read GetTemplate;
    property TypeIdentified: Boolean read GetTypeIdentified write FTypeIdentified;
    property Owner: TASN1Struct read FOwner;
    property CaptureContent: Boolean read FCaptureContent write SetCaptureContent;
    property ContentCountLoad: Integer read FContentCountLoad;
    property ContentCountSave: Integer read FContentCountSave;
    property AllowResume: Boolean read FAllowResume write SetAllowResume;
    property DoneLoading: Boolean read FDoneLoading write SetDoneLoading;
    property DoneSaving: Boolean read FDoneSaving write SetDoneSaving;
    property NoSave: Boolean read FNoSave write SetNoSave;
    property NoChangeEvent: Boolean read FNoChange write SetNoChange;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property StrictTagging: Boolean read FStrictTagging write FStrictTagging;
    property ForgetOptionalDefault: Boolean read FForgetOptionalDefault write SetForgetOptionalDefault;
    property AfterLoad: TASN1FieldEvent read FAfterLoad write SetAfterLoad;
    property BeforeLoad: TASN1FieldEvent read FBeforeLoad write SetBeforeLoad;
    property AfterSave: TASN1FieldEvent read FAfterSave write SetAfterSave;
    property BeforeSave: TASN1FieldEvent read FBeforeSave write SetBeforeSave;
    property OnCaptureLoad: TASN1CaptureEvent read FOnCaptureLoad write SetOnCaptureLoad;
    property OnCaptureSave: TASN1CaptureEvent read FOnCaptureSave write SetOnCaptureSave;
    property OnChange: TASN1ChangeEvent read FOnChange write SetOnChange;
    property OnLoad: TASN1FieldEvent read FOnLoad write SetOnLoad;
    property OnSave: TASN1FieldEvent read FOnSave write SetOnSave;
    property OnLoadStructure: TASN1FieldEvent read FOnLoadStructure write SetOnLoadStructure;
    property RefCount: Integer read FRefCount;
  end;

  TASNCustomFieldWrapper = class;
  TASNWrapperClass = class of TASNCustomFieldWrapper;

  IASNClassFactory = interface
  ['{C36E255D-49B1-4FDB-A2D2-5BB458B95FDA}']
    function GetChoiceCount: Integer;
    function GetChoiceClass(Index: Integer): TASNWrapperClass;   
    function GetIndexOfChoice(AClass: TASNWrapperClass): Integer;
    function GetDataTemplate: TASN1Struct;
    function GetItemCount: Integer;
    function GetItemClass(Index: Integer): TASNWrapperClass;
    function GetTargetClass: TASNWrapperClass;
  end;

  IASNClassFactories = interface
  ['{BC752B32-BD2E-4784-AE80-2E345DC89B5B}']
    function GetClassFactory(AClass: TASNWrapperClass): IASNClassFactory;
    procedure Lock;
    procedure RegisterChoiceClass(AChoiceClass: TASNWrapperClass;
                                  ADataTemplate: TASN1Struct;
                                  AChoices: array of TASNWrapperClass);
    procedure RegisterConstructedClass(AOwnerClass: TASNWrapperClass;
                                       ADataTemplate: TASN1Struct;
                                       AItemClasses: array of TASNWrapperClass);
    procedure Unlock;
  end;

  TASNClassFactory = class(TInterfacedObject,IASNClassFactory)
  private
    FTargetClass: TASNWrapperClass;
    FDataTemplate: TASN1Struct;
    FItemClasses: TList;
    FChoiceClasses: TList;
  public
    destructor Destroy; override;
    function GetChoiceCount: Integer;
    function GetChoiceClass(Index: Integer): TASNWrapperClass;
    function GetIndexOfChoice(AClass: TASNWrapperClass): Integer;
    function GetDataTemplate: TASN1Struct;
    function GetItemCount: Integer;
    function GetItemClass(Index: Integer): TASNWrapperClass;
    function GetTargetClass: TASNWrapperClass;
  end;

  TASNChoiceClassFactory = class(TASNClassFactory)
  public
    constructor Create(AChoiceClass: TASNWrapperClass;
                       ADataTemplate: TASN1Struct;
                       AChoices: array of TASNWrapperClass);
  end;

  TASNConstructedClassFactory = class(TASNClassFactory)
  public
    constructor Create(AOwnerClass: TASNWrapperClass;
                       ADataTemplate: TASN1Struct;
                       AItemClasses: array of TASNWrapperClass);
  end;

  TASNClassFactories = class(TInterfacedObject,IASNClassFactories)
  private
    FLock: TCriticalSection;
    FClasses: THashList;
    FFactories: TList;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
    function GetClassFactory(AClass: TASNWrapperClass): IASNClassFactory;
    procedure Lock;
    procedure RegisterChoiceClass(AChoiceClass: TASNWrapperClass;      
                                  ADataTemplate: TASN1Struct;
                                  AChoices: array of TASNWrapperClass);
    procedure RegisterConstructedClass(AOwnerClass: TASNWrapperClass;
                                       ADataTemplate: TASN1Struct;
                                       AItemClasses: array of TASNWrapperClass);
    procedure Unlock;
  end;

  TASNCustomWrapper = class(TPersistent)
  private
    FUpdateCount: Integer;
    FUserData: Pointer;
    function GetVarName: string;
    function GetWrapperOwner: TASNCustomWrapper;
    procedure SetOwnerEncapsulated(const Value: Boolean);
  protected
    FData: TASN1Struct;
    FTemplate: TASN1Struct;
    FItems: TList;
    FOwner: TPersistent;
    FOwnerEncapsulated: Boolean;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Clear; dynamic;
    procedure DestroyItems;
    procedure FieldFactory(AWrapperClass: TASNWrapperClass;
                           ATemplate: TASN1Struct;
                           AStruct: TASN1Struct;
                           var AReference);
    function GetOwner: TPersistent; override;
    procedure Update; virtual;
    procedure UpdateContents; dynamic;
    property UpdateCount: Integer read FUpdateCount;
    property OwnerEncapsulated: Boolean read FOwnerEncapsulated write SetOwnerEncapsulated;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); virtual;
    destructor Destroy; override;
    procedure AssignStruct(Struct: TASN1Struct);
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetNamePath: string; override;
    procedure LoadFromFile(AFileName: string);
    procedure LoadFromStream(AStream: TStream); dynamic;
    procedure SaveToFile(AFileName: string);
    procedure SaveToStream(AStream: TStream); dynamic;
    property Data: TASN1Struct read FData;
    property Owner: TASNCustomWrapper read GetWrapperOwner;
    property VarName: string read GetVarName;
    property UserData: Pointer read FUserData write FUserData;
  end;

  TASNCustomFieldWrapper = class(TASNCustomWrapper)
  protected
    class function GetClassFactories: IASNClassFactories; virtual;
    class function GetClassFactory: IASNClassFactory; virtual;
    function IsTemplateType: Boolean;
    function DataLength: Integer;
    procedure Update; override;
  end;

  TASNConstructedWrapper = class(TASNCustomFieldWrapper)
  private
    function GetItems(Index: Integer): TASNCustomFieldWrapper;
  protected
    class function GetItemClass(Index: Integer): TASNWrapperClass;
    class function GetItemClassCount: Integer;
    function GetCount: Integer;
    function InternalGetItems(Index: Integer): TASNCustomFieldWrapper;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    function ItemByName(const VarName: string): TASNCustomFieldWrapper;
    property ItemCount: Integer read GetCount;
    property Items[Index: Integer]: TASNCustomFieldWrapper read GetItems;
  end;

  TOctetString = class;

  TASNChoiceWrapper = class(TASNConstructedWrapper)
  private
    function GetSelected: TASNCustomFieldWrapper;
    procedure SetSelected(const Value: TASNCustomFieldWrapper);
    function GetValue: WideString;
    procedure SetValue(const Value: WideString);
    function GetGeneric: TOctetString;
    procedure SetGeneric(const Value: TOctetString);
  protected
    FChoiceList: TList;
    FSelected: TASNCustomFieldWrapper;        
    function GetChoiceClass(Index: Integer): TASNWrapperClass;
    function GetChoiceClassCount: Integer;
    function GetIndexOfChoice(AClass: TClass): Integer;
    function InternalGetChoice: Integer; dynamic;
    function InternalIsChoice(Value: Integer): Boolean;
    procedure InternalSelectChoice(Value: Integer); dynamic;
    procedure SelectGeneric; dynamic;
    procedure Update; override;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property Generic: TOctetString read GetGeneric write SetGeneric;
  published
    property Selected: TASNCustomFieldWrapper read GetSelected write SetSelected stored False;
    property Value: WideString read GetValue write SetValue stored False;
  end;

  TASNCustomOFFieldWrapper = class(TASNConstructedWrapper)
  protected
    FFieldType: TASNWrapperClass;
    FCreating: Boolean;
    function InternalAdd: TASNCustomFieldWrapper; dynamic;
    procedure Update; override;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function IndexOf(Item: TASNCustomWrapper): Integer;
    procedure LoadFromStream(AStream: TStream); override;
    procedure Move(CurIndex, NewIndex: Integer);
    property Count: Integer read GetCount;
  end;

  TOctetString = class(TASNCustomFieldWrapper)
  private
    function GetAsHexString: string;
    function GetAsString: string;
    function GetLength: Integer;
    procedure SetAsHexString(const Value: string);
    procedure SetAsString(const Value: string);
  public
    procedure GetAsStruct(var Struct: TASN1Struct);
    function GetBinary(var Buf; Count: Integer): Integer;
    procedure SetAsStruct(Struct: TASN1Struct);
    procedure SetBinary(const Buf; Count: Integer);
    property AsHexString: string read GetAsHexString write SetAsHexString;
    property AsString: string read GetAsString write SetAsString;
    property Length: Integer read GetLength;
  published
    property Value: string read GetAsHexString write SetAsHexString;
  end;

  TBitString = class(TOctetString)
  private
    function GetBitCount: Integer;
    function GetBits(Index: Integer): Boolean;
    procedure SetBitCount(const Value: Integer);
    procedure SetBits(Index: Integer; const Value: Boolean);
  public
    property BitCount: Integer read GetBitCount write SetBitCount;
    property Bits[Index: Integer]: Boolean read GetBits write SetBits; default;
  end;

  TStringWrapper = class(TASNCustomFieldWrapper)
  private
    function GetString: string;
    procedure SetString(const Value: string);
  published
    property Value: string read GetString write SetString stored False;
  end;

  TWideStringWrapper = class(TASNCustomFieldWrapper)
  private
    function GetWideString: WideString;
    procedure SetWideString(const Value: WideString);
  published
    property Value: WideString read GetWideString write SetWideString stored False;
  end;

  TBooleanWrapper = class(TASNCustomFieldWrapper)
  private
    function GetBoolean: Boolean;
    procedure SetBoolean(const Value: Boolean);
  published
    property Value: Boolean read GetBoolean write SetBoolean stored False;
  end;

  TIntegerWrapper = class(TOctetString)
  private
    function GetAsCardinal: Cardinal;
    function GetAsInt64: Integer;
    procedure SetAsCardinal(const Value: Cardinal);
    procedure SetAsInt64(const Value: Integer);
    function GetIntegerString: string;
    procedure SetIntegerString(const Value: string);
  protected
    function GetAsInteger: Integer;
    procedure SetAsInteger(const Value: Integer);
  public
    procedure GetMPInt(var Value: PMPInteger);
    procedure SetMPInt(Value: PMPInteger);
    property AsCardinal: Cardinal read GetAsCardinal write SetAsCardinal;
    property AsInt64: Integer read GetAsInt64 write SetAsInt64;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
  published
    property Value: string read GetIntegerString write SetIntegerString stored False;
  end;

  TRealWrapper = class(TASNCustomFieldWrapper)
  private
    function GetExtended: Extended;
    procedure SetExtended(const Value: Extended);
  published
    property Value: Extended read GetExtended write SetExtended stored False;
  end;

  TUTCTime = class(TASNCustomFieldWrapper)
  private
    function GetAsDateTime: TDateTime;
    function GetDay: Byte;
    function GetHour: Byte;
    function GetMinutes: Byte;
    function GetMonth: Byte;
    function GetSeconds: Byte;
    function GetYear: Word;
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetDay(const Value: Byte);
    procedure SetHour(const Value: Byte);
    procedure SetMinutes(const Value: Byte);
    procedure SetMonth(const Value: Byte);
    procedure SetSeconds(const Value: Byte);
    procedure SetYear(const Value: Word);
    procedure SetTimeString(const Value: TimeString);
  protected
    function GetTimeString: TimeString;
  public
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
  published
    property Day: Byte read GetDay write SetDay;
    property Hour: Byte read GetHour write SetHour;
    property Minutes: Byte read GetMinutes write SetMinutes;
    property Month: Byte read GetMonth write SetMonth;
    property Seconds: Byte read GetSeconds write SetSeconds;
    property Year: Word read GetYear write SetYear;
    property Value: TimeString read GetTimeString write SetTimeString stored False;
  end;

  TGeneralizedTime = class(TUTCTime)
  private
    procedure SetMilliseconds(const Value: Word);
    function GetMilliseconds: Word;
    procedure SetGeneralizedTime(const Value: TimeString);
  published
    property Milliseconds: Word read GetMilliseconds write SetMilliseconds;
    property Value: TimeString read GetTimeString write SetGeneralizedTime stored False;
  end;

  TEnumeratedWrapper = class(TIntegerWrapper)
  published
    property Value: Integer read GetAsInteger write SetAsInteger stored False;
  end;

procedure RenderAsText(Source: TASN1Struct;
                       List: TStrings;
                       ShowVarName, ShowTypeName, ShowContent: Boolean;
                       Indent: Integer = 0;
                       Width: Integer = 80;
                       HexDump: Boolean = False);

function ASN1StructAssigned(const Struct: TASN1Struct): Boolean;
function ASN1StructCompare(const Struct0, Struct1: TASN1Struct): Boolean;

procedure NewComposeASN1Struct(var Struct: TASN1Struct; Cls: Byte; Constructed: Boolean; Tag: Cardinal);

procedure DisposeASN1Struct(var Struct: TASN1Struct);
procedure ReleaseASN1Struct(var Struct: TASN1Struct; Owner: TASN1Struct = nil;
  Garbage: THashList = nil);

function ASN1IdenAndLenFromStream(AStrm: TStream;
                                  var Struct: TASN1Struct;
                                  var Done: Boolean): Integer;
function ASN1IdenAndLenFromBuf(var Buf: PChar; var BufLen: Integer;
                               var Struct: TASN1Struct;
                               var Done: Boolean): Integer;
function ASN1IdenAndLenToStream(const Struct: TASN1Struct;
                                AStrm: TStream;
                                var Done: Boolean): Integer;

function ASN1FromStream(AStrm: TStream; var Struct: TASN1Struct): Integer;
function ASN1ToStream(const Struct: TASN1Struct; AStrm: TStream): Integer;

procedure CopyASN1Struct(var Dest: TASN1Struct; const Source: TASN1Struct);
procedure AddASN1Field(var Dest: TASN1Struct; const Source: TASN1Struct);
function AddComposeASN1Field(var Dest: TASN1Struct; Cls: Byte;
  Constructed: Boolean; Tag: Cardinal): PASN1Struct;

function InterpretBERToOID(const Value: string): string; overload;
function InterpretBERToOID(Str: PChar; Len: Integer): string; overload;
function ExtractSubIden(const S: string; var StartPos: Integer): Int64;
function InterpretOIDToBER(const OID: string): string;

function InterpretBERtoReal(const Value: string): Extended;
function InterpretRealtoBER(Value: Extended): string;

function TrimPrintable(Str: string): string;
function Utf8ToUnicode(Str: string): WideString;
function UnicodeToUtf8(Str: WideString): string;
function BMPToUnicode(Str: PChar; Len: Integer): WideString;
function UnicodeToBMP(const Str: WideString): string;

function UTCTimeToDateTime(Str: PChar; Len: Integer; var Time: TDateTime): Boolean;
function GeneralizedTimeToDateTime(Str: PChar; Len: Integer; var Time: TDateTime): Boolean;
function X509GeneralizedTimeToDateTime(Str: PChar; Len: Integer; var Time: TDateTime): Boolean;
procedure DateTimeToASN1(Time: TDateTime; var Struct: TASN1Struct);  
function DateTimeToUTCTime(Time: TDateTime): string;
function DateTimeToX509GeneralizedTime(Time: TDateTime): string;
function DateTimeToGeneralizedTime(Time: TDateTime): string;
                                                        
{$IFDEF LINUX}
procedure DateTimeToSystemTime(const DateTime: TDateTime; var T: TSystemTime);
function SystemTimeToDateTime(const T: TSystemTime): TDateTime;
{$ENDIF}

function OSToHex(const OSStr: OctetString; NoSpaces: Boolean = False): string;
function HexToOS(const HexStr: string): OctetString;

procedure RegistredModules(List: TStrings);

procedure LockInstanceList;
procedure UnlockInstanceList;
procedure PreAllocInstances(Count: Integer);

{$IFNDEF INIT_SECTIONS}
procedure InitASN;
{$ENDIF}
{$IFNDEF FINI_SECTIONS}
procedure CollectGarbage;
{$ENDIF}

implementation

uses
  MpArith, ReadStrm, SsBase64, Pkix, SecStr, MpECArith;

var
  GarbageList: TThreadHashList;
  FinalGarbageList: THashList;
  CollectingGarbage: Boolean = False;
  RegistredTypes: TStrings;
  RTLock: TCriticalSection;
  InstancePool: TList;
  MemoryBlocks: TList;

{$IFNDEF D5UP}
function SameText(S0,S1: string): Boolean;
begin
  Result := CompareText(S0,S1) = 0;
end;
{$ENDIF}

function AllocateMemoryBlock(Size: Integer): Pointer;
begin
  GetMem(Result,Size);
  FillChar(Result^,Size,0);
  MemoryBlocks.Add(Result);
end;

var
  InstanceListLockedThread: Cardinal = 0;
  ILLCount: Cardinal;
  InstanceCount: Integer;
//threadvar
//  CurrTID: Cardinal;

procedure LockInstanceList;
var
  CurrTID: Cardinal;
begin
  Assert(Assigned(InstancePool),'ASN1 not initialized: Call StrSecInit.InitASN1 to correct.');
  CurrTID := GetCurrentThreadID;
  if InstanceListLockedThread = 0 then begin
    FinalGarbageList := GarbageList.LockList;
    ILLCount := 1;
    InstanceListLockedThread := CurrTID;
  end else begin
    if InstanceListLockedThread = CurrTID then
      Inc(ILLCount)
    else begin
      FinalGarbageList := GarbageList.LockList;
      ILLCount := 1;
      InstanceListLockedThread := CurrTID;
    end;
  end;
end;

procedure UnlockInstanceList;
begin
  Dec(ILLCount);
  if ILLCount = 0 then begin
    FinalGarbageList := nil;
    InstanceListLockedThread := 0;
    GarbageList.UnlockList;
  end;
end;

procedure PreAllocInstances(Count: Integer);
var
  I, NewCount: Integer;
  P: Pointer;
begin
  if Count <= 1024 then Exit;
  if CollectingGarbage then Abort;
  LockInstanceList;
  try
    NewCount := Count - InstancePool.Count;
    if NewCount > 0 then begin
      P := AllocateMemoryBlock(NewCount*TASN1Struct.InstanceSize);
      if InstancePool.Capacity < Count then
        InstancePool.Capacity := Count;
      for I := 1 to NewCount do begin
        PInteger(P)^ := Integer(TASN1Struct);
        InstancePool.Add(P);
        P := Ptr(LongInt(P) + TASN1Struct.InstanceSize);
      end;
      FinalGarbageList.Capacity := FinalGarbageList.Capacity + NewCount;
    end;
  finally
    UnlockInstanceList;
  end;
end;

function AllocInstance: Pointer;
var
  GL: THashList;
  I: Integer;
  P: Pointer;
begin
//  Result := nil;
  if CollectingGarbage then Abort;
  LockInstanceList;
  try
    GL := FinalGarbageList;
    I := InstancePool.Count - 1;
    if I < 0 then begin
      P := AllocateMemoryBlock(1024*TASN1Struct.InstanceSize);
      if InstancePool.Capacity < 1024 then
        InstancePool.Capacity := 1024;
      for I := 1 to 1023 do begin
        PInteger(P)^ := Integer(TASN1Struct);
        InstancePool.Add(P);
        P := Ptr(LongInt(P) + TASN1Struct.InstanceSize);
      end;
      PInteger(P)^ := Integer(TASN1Struct);
      Result := P;
      GL.Capacity := GL.Capacity + 1024;
    end else begin
      Result := InstancePool[I];
      InstancePool.Delete(I);
    end;
    Inc(InstanceCount);
    GL.Add(Result);
  finally
    UnlockInstanceList;
  end;
end;

function SortCompare(Item1, Item2: Pointer): Integer;
begin
  Result := LongInt(Item2) - LongInt(Item1);
end;

procedure CollectGarbageNow;
var
  I, J: Integer;
  Item, Prev, Mem: Pointer;
begin
  InstancePool.Sort(SortCompare);
  if InstancePool.Count > 1024 then begin
    Prev := nil;
    Mem := nil;
    J := 0;
    for I := 0 to InstancePool.Count - 1 do begin
      Item := InstancePool[I];
      if Assigned(Prev) then begin
        if LongInt(Item) - LongInt(Prev) <> TASN1Struct.InstanceSize then begin
          Prev := nil;
          J := 0;
          Mem := nil;
        end else begin
          Prev := Item;
          Inc(J);
          if J = 1024 then begin
            MemoryBlocks.Remove(Mem);
            for J := I downto I-1023 do
              InstancePool.Delete(J);
            FreeMem(Mem);
            Exit;
          end;
        end;
      end else if MemoryBlocks.IndexOf(Item) >= 0 then begin
        J := 1;
        Prev := Item;
        Mem := Item;
      end else if InstancePool.Count - I < 1024 then
        Exit;
    end;
  end;
end;

procedure DeallocInstance(Instance: Pointer);
begin
  if not CollectingGarbage then begin
    GarbageList.LockList;
    try
      FillChar(Instance^,TASN1Struct.InstanceSize,0);
      PInteger(Instance)^ := Integer(TASN1Struct);
      if InstancePool.IndexOf(Instance) < 0 then
        InstancePool.Add(Instance)
      else
        raise Exception.Create('Already dealloced');

      if InstancePool.Count mod 128 = 0 then
        CollectGarbageNow;

      Dec(InstanceCount);
    finally
      GarbageList.UnlockList;
    end;
  end;
end;

procedure DestroyingRegistration(Struct: TASN1Struct); forward;

procedure DeallocInstances(Garbage: THashList);
var
  I: Integer;
  P: TASN1Struct;
  GL: THashList;
begin
  if Garbage.Count = 0 then Exit;
  if CollectingGarbage then begin
    for I := 0 to Garbage.Count - 1 do begin
      P := Garbage[I];
      TASN1Struct(P).CleanupInstance;
      if Assigned(FinalGarbageList) then
        while FinalGarbageList.Remove(P) >= 0 do;
    end;
  end else begin
    GL := GarbageList.LockList;
    try
      for I := 0 to Garbage.Count - 1 do
        GL.Remove(Garbage[I]);
      if InstancePool.Capacity < InstancePool.Count + Garbage.Count then
        InstancePool.Capacity := InstancePool.Count + Garbage.Count;
      for I := 0 to Garbage.Count - 1 do begin
        P := Garbage[I];
        if P.FRefCount > 0 then
          DestroyingRegistration(P);
        P.CleanupInstance;
        FillChar(Pointer(P)^,TASN1Struct.InstanceSize,0);
        PInteger(P)^ := Integer(TASN1Struct);
        InstancePool.Add(P);
        Dec(InstanceCount);
      end;
    finally
      GarbageList.UnlockList;
    end;
  end;
end;

procedure CollectGarbage;
var
  GL: THashList;
  I: Integer;

  procedure DisposeRegistredTypes;
  var
    I, J: Integer;
    SL: TStrings;
    List: TList;
    RL: TStrings;
  begin
    RTLock.Acquire;
    try
      RL := RegistredTypes;
      RegistredTypes := nil;
      if RL <> nil then begin
        List := TList.Create;
        try
          for I := 0 to RL.Count - 1 do begin
            SL := TStrings(RL.Objects[I]);
            List.Capacity := SL.Count;
            for J := 0 to SL.Count - 1 do
              List.Add(SL.Objects[J]);
            for J := 0 to List.Count - 1 do
              if GL.IndexOf(List[J]) >= 0 then
                TASN1Struct(List[J])._Release;
            List.Clear;
            SL.Free;
          end;
        finally
          List.Free;
        end;
      end;
      RL.Free;
    finally
      RTLock.Release;
    end;
    RTLock.Free;
  end;

  function ReleaseOwner(List: THashList; Struct: TASN1Struct): Boolean;
  begin
    if List.IndexOf(Struct.Owner) >= 0 then
      Result := ReleaseOwner(List,Struct.Owner)
    else if List.IndexOf(Struct) >= 0 then begin
      Result := True;
      if Struct.FRefCount <= 0 then
        Struct.Free
      else begin
        Result := Struct.FRefCount = 1;
        Struct._Release;
      end;
    end else
      Result := False;
  end;

begin
  if GarbageList = nil then
    Exit;

  try
    GL := GarbageList.LockList;
    try
      try
        FinalGarbageList := GL;
        CollectingGarbage := True;
// *** This code segment may be superfluous:
        DisposeRegistredTypes;
        while (GL.Count > 0) and ReleaseOwner(GL,GL.First) do;
        while GL.Count > 0 do begin
          TASN1Struct(GL.First).FOwner := nil;
          TASN1Struct(GL.First).FOnChange := nil;
          TASN1Struct(GL.First).Free;
        end;
// ***
        FinalGarbageList := nil;
      finally
        for I := 0 to MemoryBlocks.Count - 1 do
          FreeMem(MemoryBlocks[I]);
        MemoryBlocks.Free;
        InstancePool.Free;
      end;
    finally
      GarbageList.UnlockList;
    end;
  finally
    GarbageList.Free;
    GarbageList := nil;
  end;
end;

function ExtractModuleName(Module: TStrings): string;
var
  J: Integer;
  S: string;
  P, PD: Integer;
begin
  J := 0;
  Result := '';
  while J < Module.Count do begin
    S := Trim(Module[J]);
    if S <> '' then begin
      Result := Result + S + ' ';
      S := UpperCase(Result);
      P := Pos(' BEGIN ',S);
      if P > 0 then begin
        PD := Pos(' ::= BEGIN ',S);
        if PD <> P - 4 then
          Break;
        PD := Pos(' DEFINITIONS ',S);
        if (PD > 0) and (PD < P) then begin
          Result := Copy(Result,1,PD-1);
          Exit;
        end;
        Break;
      end;
    end;
    Inc(J);
  end;
  Result := '';
end;

procedure RegistredModules(List: TStrings);
begin
  if Assigned(RegistredTypes) then
    List.AddStrings(RegistredTypes);
end;

procedure RegisterModule(ModuleName: string; DeclaredTypes: TStrings);
var
  I, J, K: Integer;
  SL: TStrings;
  F, NewF: TASN1Struct;
begin
//  Exit;
  RTLock.Acquire;
  try
    if RegistredTypes = nil then
      RegistredTypes := TStringList.Create;
    if ModuleName = '' then
      ModuleName := '(Default)';

    I := RegistredTypes.IndexOf(ModuleName);
    if I >= 0 then begin
      SL := TStrings(RegistredTypes.Objects[I]);
      if ModuleName <> '(Default)' then begin
        for J := 0 to SL.Count - 1 do
          TASN1Struct(SL.Objects[J])._Release;
        SL.Free;
        RegistredTypes.Delete(I);
        SL := TStringList.Create;
        I := -1;
      end;
    end else
      SL := TStringList.Create;

    for J := 0 to DeclaredTypes.Count - 1 do begin
      F := TASN1Struct(DeclaredTypes.Objects[J]);
      if F = nil then Continue;
      if F.Tag < High(UniversalTypes) then
        if F.FTypeName = UniversalTypes[F.Tag] then
          Continue;
      if (F.ModuleName <> '') and (F.ModuleName <> ModuleName) then
        Continue;
      if (F.Tag <= Cardinal(MaxInt)) or (F.ChoiceCount > 0) then begin
        NewF := F;
        NewF._AddRef;
        F.FModuleName := ModuleName;
        NewF.FModuleName := ModuleName;
        K := SL.IndexOf(DeclaredTypes[J]);
        if K < 0 then
          SL.AddObject(DeclaredTypes[J],NewF)
        else if SL.Objects[K] <> NewF then begin
          TASN1Struct(SL.Objects[K])._Release;
          SL.Objects[K] := NewF;
        end else
          NewF._Release;
      end;
    end;
    if I < 0 then
      RegistredTypes.AddObject(ModuleName,SL);
  finally
    RTLock.Release;
  end;
end;

procedure ExplicitImport(Module, DeclaredTypes: TStrings);
var
  I, J, K, P: Integer;
  S: string;
  TN, MN: PChar;
  SL: TStrings;
  More: Boolean;
begin
  if RegistredTypes = nil then
    Exit;
  RTLock.Acquire;
  try
    S := '';
    for I := 0 to Module.Count - 1 do
      if Module[I] <> '' then
        S := S + Module[I] + ' ';
    S := S + #0;
    I := 1;
    while I <= Length(S) do begin
      if StrLIComp(' IMPORTS ',PChar(@S[I]),9) = 0 then begin
        TN := @S[I+9];
        J := 0;
        I := I+9;
        while not (TN[J] in [#0,';']) do begin
          if TN[J] in ['f','F'] then
            if StrLIComp(' FROM ',PChar(@TN[J-1]),6) = 0 then begin
              TN[J-1] := #0;
              MN := @TN[J + 5];
              I := I + J + 5;
              J := 0;
              while not (MN[J] in [#0,';']) do
                Inc(J);
              I := I + J;
              MN[J] := #0;
              P := RegistredTypes.IndexOf(MN);
              if P >= 0 then begin
                SL := TStrings(RegistredTypes.Objects[P]);
                repeat
                  K := 0;
                  while not (TN[K] in [#0,',']) do
                    Inc(K);
                  More := TN[K] = ',';
                  TN[K] := #0;
                  P := SL.IndexOf(TN);
                  if P >= 0 then
                    DeclaredTypes.AddObject(TN,SL.Objects[P]);
                  if More then begin
                    Inc(K);
                    while TN[K] = ' ' do
                      Inc(K);
                    TN := @TN[K];
                  end;
                until not More;
              end;
              Break;
            end;
          Inc(J)
        end;
      end else
        Inc(I);
    end;
  finally
    RTLock.Release;
  end;
end;

procedure DestroyingRegistration(Struct: TASN1Struct);
var
  I, J: Integer;
  SL: TStrings;
  F: TASN1Struct;
begin
  if RegistredTypes = nil then Exit;
  RTLock.Acquire;
  try
    for I := 0 to RegistredTypes.Count - 1 do begin
      SL := TStrings(RegistredTypes.Objects[I]);
      for J := 0 to SL.Count - 1 do
        if SL.Objects[J] = Struct then begin
          F := TASN1Struct.Create;
          F._AddRef;
          F.Assign(Struct);
          SL.Objects[J] := F;
          Exit;
        end;
    end;
  finally
    RTLock.Release;
  end;
end;

procedure ImplicitImport(TypeName: string; Struct: TASN1Struct);
var
  I, J: Integer;
  SL: TStrings;
begin
  if RegistredTypes = nil then Exit;
  RTLock.Acquire;
  try
    if ((Struct.Tag <> Cardinal(V_ASN1_UNDEF)) and not Struct.Implicit) or
       ((Struct.ImplicitTag <> Cardinal(V_ASN1_UNDEF)) and Struct.Implicit) then
      Exit;
    for I := 0 to RegistredTypes.Count - 1 do begin
      SL := TStrings(RegistredTypes.Objects[I]);
      for J := 0 to SL.Count - 1 do
        if SameText(SL[J],TypeName) then begin
          Struct.CopyTypeInfoAsField(TASN1Struct(SL.Objects[J]));
          Exit;
        end;
    end;
  finally
    RTLock.Release;
  end;
end;    

function GarbageFreeNotification(Struct: TASN1Struct): Boolean;
var
  GL: THashList;
begin
  if CollectingGarbage then begin
    if Assigned(FinalGarbageList) then
      while FinalGarbageList.Remove(Struct) >= 0 do;
    Result := True;
  end else begin
    if Struct.FRefCount > 0 then
      DestroyingRegistration(Struct);
    GL := GarbageList.LockList;
    try
      if not CollectingGarbage then begin
        Result := True;
        while GL.Remove(Struct) >= 0 do;
      end else
        Result := False;
    finally
      GarbageList.UnlockList;
    end;
  end;
end;

function OSToHex(const OSStr: OctetString; NoSpaces: Boolean): string;
const
  Digits = '0123456789ABCDEF';
var
  I: Integer;
begin
  if NoSpaces then begin
    SetLength(Result,Length(OSStr)*2);
    for I := 1 to Length(OSStr) do begin
      Result[2*I - 1] := Digits[(Byte(OSStr[I]) shr 4) + 1];
      Result[2*I] := Digits[(Byte(OSStr[I]) and $F) + 1];
    end;
  end else begin
    Result := StringOfChar(' ',Length(OSStr)*3 - 1);
    for I := 1 to Length(OSStr) do begin
      Result[3*I - 2] := Digits[(Byte(OSStr[I]) shr 4) + 1];
      Result[3*I - 1] := Digits[(Byte(OSStr[I]) and $F) + 1];
    end;
  end;
end;

function HexToOS(const HexStr: string): OctetString;
const
  Digits = '0123456789ABCDEF';
var
  I, J, K, P: Integer;
  B: Byte;
  U: string;
begin
  SetLength(Result,Length(HexStr) shr 1);
  J := 1;
  B := 0;
  K := 0;
  U := UpperCase(HexStr);
  for I := 1 to Length(U) do begin
    P := Pos(U[I],Digits);
    if P > 0 then begin
      B := (B shl 4) + P - 1;
      Inc(K);
    end;
    if K = 2 then begin
      Result[J] := Char(B);
      Inc(J);
      B := 0;
      K := 0;
    end;
  end;
  SetLength(Result,J-1);
end;

procedure RenderAsText(Source: TASN1Struct;
                       List: TStrings;
                       ShowVarName, ShowTypeName, ShowContent: Boolean;
                       Indent: Integer = 0;
                       Width: Integer = 80;
                       HexDump: Boolean = False);
var
  I, P: Integer;
  Str, S, T: string;
begin                  
  Str := '';
  if HexDump then begin
    for I := 0 to Source.FIdenAndLenCountLoad - 1 do
      Str := Str + IntToHex(Source.FIdenAndLen[I],2) + ' ';
    for I := Source.FIdenAndLenCountLoad to 10 do
      Str := Str + '   ';
  end;
  Str := Str + StringOfChar(' ',Indent);
  if ShowVarName then begin
    if Source.ChoiceVarName = '' then
      Str := Str + Source.VarName
    else
      Str := Str + Source.ChoiceVarName;
    if ShowTypeName or ShowContent then
      Str := Str + ': ';
  end;
  if ShowTypeName then begin
    if Source.ChoiceTypeName = '' then
      Str := Str + Source.TypeName
    else
      Str := Str + Source.ChoiceTypeName;
  end;
  if (Source.Encapsulated = nil) and not Source.Constructed then begin
    if ShowContent then begin
      if HexDump then begin
        P := 0;
        while P < Source.Length do begin
          List.Add(Str);
          Str := '';
          for I := P to P + 10 do begin
            if I >= Source.Length then Break;
            Str := Str + IntToHex(Byte(Source.Content[I]),2) + ' ';
          end;
          Inc(P,11);
        end;
      end else begin
        if ShowVarName or ShowContent then
          Str := Str + ' = ';
        try
          S := Source.DisplayContent;
        except
          on E: Exception do
            S := '(Error: ' + E.Message + ')';
        end;
        T := '';
        P := Width - Length(Str) - Indent;
        if P < 2 then P := 2;
        if Length(S) > P then repeat
          if S[P-1] = ' ' then
            Dec(P);
          if S[1] = ' ' then
            T := T + Copy(S,2,P) + #13#10 + StringOfChar(' ',4 + Indent)
          else
            T := T + Copy(S,1,P) + #13#10 + StringOfChar(' ',4 + Indent);
          Delete(S,1,P);
          P := Width - 4 - Indent;
        until Length(S) <= P;
        Str := Str + T + S;
      end;
    end;
    List.Add(Str);
  end else begin
    List.Add(Str);
    if Assigned(Source.Encapsulated) then begin
      Source.ContentAsASN1Struct(Source.FEncapsulated);
      RenderAsText(Source.Encapsulated,List,ShowVarName,ShowTypeName,ShowContent,Indent + 3,Width,HexDump)
    end else
      for I := 0 to Source.ItemCount -1 do
        RenderAsText(Source.Items[I]^,List,ShowVarName,ShowTypeName,ShowContent,Indent + 3,Width,HexDump);
  end;
end;

function ASN1StructAssigned(const Struct: TASN1Struct): Boolean;
begin
  Result := Assigned(Struct);
end;

function ASN1StructCompare(const Struct0, Struct1: TASN1Struct): Boolean;
var
  MS0, MS1: TSecureMemoryStream;
  I: Integer;
  Cnts0, Cnts1: PASN1Structs;

  function TagLengthCompare(const S0, S1: TASN1Struct): Boolean;
  begin
    Result := (S0.Cls = S1.Cls) and
              (S0.Tag = S1.Tag) and
              (S0.Constructed = S1.Constructed) and
              (((S0.Length <= 0) and (S1.Length <= 0)) or
               (S0.Length = S1.Length));
  end;

begin
  if Assigned(Struct0.FReadOnlyCntSlave) and Assigned(Struct1.FReadOnlyCntSlave) and
     not (Struct0.FModifiedChild or Struct1.FModifiedChild) then
    Result := (Struct0.FReadOnlyCntLenSlave = Struct1.FReadOnlyCntLenSlave) and
              CompareMem(Struct0.FReadOnlyCntSlave,
                         Struct1.FReadOnlyCntSlave,
                         Struct0.FReadOnlyCntLenSlave)
  else if (Struct0.ModuleName <> Struct1.ModuleName) or
     (Struct0.TypeName <> Struct1.TypeName) then begin
    Struct0.CalculateLength;
    Struct1.CalculateLength;
    MS0 := TSecureMemoryStream.Create;
    try
      MS1 := TSecureMemoryStream.Create;
      try
        MS0.Capacity := Struct0.Length + 4;
        Struct0.SaveToStream(MS0,fmtDER);
        MS1.Capacity := Struct1.Length + 4;
        Struct1.SaveToStream(MS1,fmtDER);
        Result := (MS0.Size = MS1.Size) and
                  CompareMem(MS0.Memory,MS1.Memory,MS0.Size);
      finally
        MS1.Free;
      end;
    finally
      MS0.Free;
    end;
  end else begin
    Result := TagLengthCompare(Struct0,Struct1);
    if Result then begin
      if not Struct0.Constructed then
        Result := CompareMem(Struct0.Cnt,Struct1.Cnt,Struct0.Length)
      else begin
        Cnts0 := Struct0.Contents;
        Cnts1 := Struct1.Contents;
        Result := (Cnts0 <> nil) and (Cnts1 <> nil);
        if Result then begin
          Result := Cnts0^.ItemCount = Cnts1^.ItemCount;
          if Result then begin
            for I := 0 to Cnts0^.ItemCount - 1 do begin
              Result := TagLengthCompare(Cnts0^.Items[I],
                                         Cnts1^.Items[I]);
              if not Result then Break;
            end;
            if Result then
              for I := 0 to Cnts0^.ItemCount - 1 do begin
                Result := ASN1StructCompare(Cnts0^.Items[I],
                                            Cnts1^.Items[I]);
                if not Result then Break;
              end;
          end;
        end;
      end;
    end;
  end;
end;

procedure NewComposeASN1Struct(var Struct: TASN1Struct; Cls: Byte; Constructed: Boolean; Tag: Cardinal);
begin
  if Assigned(Struct) then begin
    if Struct.FRefCount <= 0 then
      Struct.Free
    else
      Struct._Release;
  end;
  Struct := TASN1Struct.Create;
  Struct.Cls := Cls;
  Struct.Constructed := Constructed;
  Struct.Tag := Tag;
  if Constructed then
    Struct.AllocContents(0);
end;

procedure DisposeASN1Struct(var Struct: TASN1Struct);
var
  F: TASN1Struct;
begin
  F := Struct;
  Struct := nil;
  F.Free;
end;

procedure ReleaseASN1Struct(var Struct: TASN1Struct; Owner: TASN1Struct = nil;
  Garbage: THashList = nil);
var
  F: TASN1Struct;
  InList: Boolean;
  GL: THashList;
begin
  if Assigned(Garbage) then begin
    if Assigned(Owner) then begin
      if Struct.FOwner = Owner then
        Struct.FOwner := nil;
      Struct.RemoveItem(Owner,Garbage);
    end;
    F := Struct;
    Struct := nil;
    F._Release(Garbage);
  end else if not CollectingGarbage then begin
    GL := GarbageList.LockList;
    try
      InList := GL.IndexOf(Struct) >= 0;
    finally
      GarbageList.UnlockList;
    end;
    if InList then begin
      if Assigned(Owner) then begin
        if Struct.FOwner = Owner then
          Struct.FOwner := nil;
        Struct.RemoveItem(Owner);
      end;
      F := Struct;
      Struct := nil;
      F._Release;
    end;
  end;
end;

function TrimPrintable(Str: string): string;
var
  I: Integer;
begin
  Result := Trim(Str);
  for I := Length(Result) - 1 downto 2 do
    if (Result[I] = ' ') and (Result[I+1] = ' ') then
      Delete(Result,I,1);
end;

function ASN1IdenAndLenFromStream(AStrm: TStream;
                                  var Struct: TASN1Struct;
                                  var Done: Boolean): Integer;
var
  B: Byte;
  R: Integer;
  L: LongInt;
begin
  if Struct = nil then
    Struct := TASN1Struct.Create;

  Struct.DoBeforeLoad(Struct);

  Struct.FIdenAndLenCountLoad := 0;

  Result := AStrm.Read(B,1);
  Done := Result = 1;
  if not Done then Exit;
  Struct.FIdenAndLen[0] := B;
  Struct.FIdenAndLenCountLoad := Result;

  Struct.Cls := B and $C0;
  Struct.Constructed := (B and V_ASN1_CONSTRUCTED) > 0;
  if B and $1F = $1F then begin
    Struct.Tag := 0;
    repeat
      R := AStrm.Read(B,1);
      Done := R = 1;
      if not Done then Exit;
      Struct.FIdenAndLen[Result] := B;
      Struct.FIdenAndLenCountLoad := Result + R;
      Result := Result + R;
      Struct.Tag := Struct.Tag shl 7 or (B and $7F);
    until B and $80 = 0;
  end else
    Struct.Tag := B and $1F;

  R := AStrm.Read(B,1);
  Done := R = 1;
  if not Done then Exit;
  Struct.FIdenAndLen[Result] := B;
  Result := Result + R;
  Struct.FIdenAndLenCountLoad := Result;

  if B and $80 = 0 then
    Struct.FLength := B
  else begin
    B := B and $7F;
    if B = 0 then
      Struct.FLength := -1
    else begin
      if B > 4 then
        raise EAbort.Create(Format('Too large field: ~2^%d octets',[B*8]));
      Struct.FLength := 0;
      L := 0;
      R := AStrm.Read(L,B);
      Done := B = R;
      Move(L,Struct.FIdenAndLen[Result],R);
      Result := Result + R;
      Struct.FIdenAndLenCountLoad := Result;
      for B := 1 to R do begin
        Struct.FLength := Struct.FLength shl 8 + (L and $FF);
        L := L shr 8;
      end;
    end;
  end;
  Done := True;
end;

function ASN1IdenAndLenFromBuf(var Buf: PChar; var BufLen: Integer;
                               var Struct: TASN1Struct;
                               var Done: Boolean): Integer;
var
  B: Byte;
  R: Integer;
  L, C: LongWord;
begin
  if Struct = nil then
    Struct := TASN1Struct.Create;

  Struct.DoBeforeLoad(Struct);

  Struct.FIdenAndLenCountLoad := 0;

  Done := BufLen > 0;
  if Done then begin
    B := Byte(Buf[0]);
    Inc(Buf);
    Dec(BufLen);
    Result := 1;
  end else begin
    Result := 0;
    Exit;
  end;
  Struct.FIdenAndLen[0] := B;
  Struct.FIdenAndLenCountLoad := Result;

  if Struct.Persistent and (Struct.Tag <> Cardinal(V_ASN1_UNDEF)) then begin
    if (Struct.Cls <> B and $C0) or
       (Struct.Constructed xor ((B and V_ASN1_CONSTRUCTED) > 0)) then
      raise EInvalidTag.Create('Wrong format: ' + Struct.GetNamePath);
  end else begin
    Struct.Cls := B and $C0;
    Struct.Constructed := (B and V_ASN1_CONSTRUCTED) > 0;
  end;
  if B and $1F = $1F then begin
    L := 0;
    repeat
      Done := BufLen > 0;
      if Done then begin
        B := Byte(Buf[0]);
        Inc(Buf);
        Dec(BufLen);
        Inc(Result);
      end;
      if not Done then Exit;
      Struct.FIdenAndLen[Result - 1] := B;
      Struct.FIdenAndLenCountLoad := Result;
      L := (L shl 7) or (B and $7F);
    until B and $80 = 0;
    if Struct.Persistent and (Struct.Tag <> Cardinal(V_ASN1_UNDEF)) and
       (L <> Struct.Tag) then
      raise EInvalidTag.Create(Format('Wrong format: %s'#13#10'Tag: %d - %d expected',
                                      [Struct.GetNamePath,B and $1F,Struct.Tag]));
    Struct.Tag := L;
  end else if Struct.Persistent and (Struct.Tag <> Cardinal(V_ASN1_UNDEF)) then begin
    if Struct.Tag <> B and $1F then
      raise EInvalidTag.Create(Format('Wrong format: %s'#13#10'Tag: %d - %d expected',
                                      [Struct.GetNamePath,B and $1F,Struct.Tag]));
  end else
    Struct.Tag := B and $1F;

  Done := BufLen > 0;
  if Done then begin
    B := Byte(Buf[0]);
    Inc(Buf);
    Dec(BufLen);
    Inc(Result);
  end;
  if not Done then Exit;
  Struct.FIdenAndLen[Result - 1] := B;
  Struct.FIdenAndLenCountLoad := Result;

  if B and $80 = 0 then
    Struct.FLength := B
  else begin
    B := B and $7F;
    if B = 0 then
      Struct.FLength := -1
    else begin
      Struct.FLength := 0;
      R := B;
      if R > 4 then
        raise EAbort.Create(Format('Too large field: %s'#13#10'~2^%d octets',[Struct.GetNamePath,R*8]));
      if R > BufLen then R := BufLen;
      L := 0;
      Move(Buf[0],L,R);
      Inc(Buf,R);
      Dec(BufLen,R);
      Move(L,Struct.FIdenAndLen[Result],R);
      Result := Result + R;
      Struct.FIdenAndLenCountLoad := Result;
      Done := B = R;
      if not Done then Exit;
      C := (L and $FF);
      for B := 2 to R do begin
        L := L shr 8;
        C := C shl 8 + (L and $FF);
      end;
      Struct.FLength := C;
    end;
  end;
  Done := True;
end;

function ASN1FromBuf(var Buf: PChar; var BufLen: Integer;
                     var Struct: TASN1Struct): Integer; forward;

function ASN1CntFromBuf(var Buf: PChar; var BufLen: Integer;
                        var Struct: TASN1Struct): Integer;
var
  Count, Id: Integer;
  V: TASN1Struct;
  RS: TReadStream;
  Done, SaveReadOnly: Boolean;
  Fld, Tmpl: PASN1Struct;
  SaveBuf: PChar;
  SaveBufLen: Integer;
  HasNull: Boolean;

  procedure ImplicitToExplicit;
  begin
    V._AddRef;
    Count := Count + ASN1IdenAndLenFromBuf(Buf,BufLen,V,Done);
    V.FConstructed := True;
    V.FVarName := Fld^.VarName;
    V.FOptional := Fld^.FOptional;
    V.FDefault := Fld^.FDefault;
    V.FHasDefaultValue := Fld^.FHasDefaultValue;
    V.FPersistent := True;
    V.FChoiceVarName := Fld^.FChoiceVarName;
    V.FChoiceTypeName := Fld^.FChoiceTypeName;
    V.FTypeName := StringReplace(Fld^.FTypeName,' IMPLICIT','',[rfIgnoreCase]);
    V.AllocContents(1);
    Fld^.FTag := Fld^.FImplicitTag;
    Fld^.FCls := V_ASN1_UNIVERSAL;
    Fld^.FTypeName := Fld^.ImplicitTypeName;
    Fld^.FImplicit := False;
    V.Items[0]^.Assign(Fld^);
    V.Items[0]^.ChoiceCount := 0;
    Fld^._Release;
    Struct.Contents^.Items[Id] := V;
    Count := Count + ASN1FromBuf(Buf,BufLen,V.Items[0]^);
  end;

begin
  Result := 0;
  Struct.DoLoad(Struct);

  if Struct.Constructed then begin
    SaveReadOnly := Struct.FReadOnly;
    Struct.FReadOnly := False;
    if Struct.Persistent then begin
      Count := 0;
      Id := 0;
      HasNull := False;
      if Assigned(Struct.FTemplate) then begin
        while (Struct.Length < 0) or (Count < Struct.Length) do begin
          if Id < Struct.ItemCount then
            Fld := Struct.Items[Id]
          else
            Fld := Struct.AddField;
          Fld^.FReadOnly := SaveReadOnly;
          Count := Count + ASN1FromBuf(Buf,BufLen,Fld^);
          Struct.DoneLoading := Fld^.DoneLoading;
          if Struct.AllowResume and
             not Fld^.DoneLoading then
            Break
          else if (Struct.Length < 0) and
                  (Fld^.Cls = 0) and
                  (Fld^.Constructed = False) and
                  (Fld^.Tag = 0) and
                  (Fld^.Length = 0) then begin
//            HasNull := True;
            Break;
          end;
          Inc(Id);
        end;
        Struct.AllocContents(Id);
      end else begin
        while Struct.UnambiguousField(Id) and
              ((Struct.Length < 0) or (Count < Struct.Length)) do begin
          if Id >= Struct.ItemCount then
            Struct.AllocContents(Id + 1);
          Fld := Struct.Items[Id];
          Fld^.FReadOnly := SaveReadOnly;
          if ((Fld^.Cls <> V_ASN1_UNIVERSAL) and Fld^.Implicit) or
             (Fld^.ChoiceCount > 0) then begin
            SaveBuf := Buf;
            SaveBufLen := BufLen;
            Done := False;
            try
              Count := Count + ASN1FromBuf(Buf,BufLen,Fld^);
              Done := True;
            except
              on E:EInvalidTag do begin
                if (Fld^.Cls = V_ASN1_UNIVERSAL) or not Fld^.Implicit or
                   Fld^.FStrictTagging then
                  raise;
              end else
                raise;
            end;
            if not Done then begin
              Buf := SaveBuf;
              BufLen := SaveBufLen;
              // EXPLICIT / IMPLICIT Confusion ?
              V := TASN1Struct.Create;
              ImplicitToExplicit;
              V.DoneLoading := V.Items[0]^.DoneLoading;
            end;
          end else
            Count := Count + ASN1FromBuf(Buf,BufLen,Fld^);
          Struct.DoneLoading := Fld^.DoneLoading;
          if Fld^.AllowResume and
             not Fld^.DoneLoading then
            Break
          else if (Struct.Length < 0) and
                  (Fld^.Cls = 0) and
                  (Fld^.Constructed = False) and
                  (Fld^.Tag = 0) and
                  (Fld^.Length = 0) then begin
            HasNull := True;
            Struct.DeleteItem(Id);
            Break;
          end;
          Inc(Id);
        end;
        if ((Struct.Length < 0) and not HasNull) or (Count < Struct.Length) then begin
          V := TASN1Struct.Create;
          try
            V.AllowResume := True;
            while (Struct.Length < 0) or (Count < Struct.Length) do begin
              if Struct.Length >= 0 then begin
                if Id >= Struct.ItemCount then
                  raise Exception.Create('Invalid format: ' + Struct.GetNamePath);
                if V.DoneLoading then begin
                  V.DisposeContent;
                  V.FLength := 0;
                  V.ResetStreaming;
                end;
                Count := Count + ASN1IdenAndLenFromBuf(Buf,BufLen,V,Done);
                Struct.DoneLoading := Done;
                if not Done then
                  Break;
              end else begin
                if V.DoneLoading then begin
                  V.DisposeContent;
                  V.FLength := 0;
                  V.ResetStreaming;
                end;
                Count := Count + ASN1IdenAndLenFromBuf(Buf,BufLen,V,Done);
                Struct.DoneLoading := Done;
                if not Done then
                  Break
                else if (V.Cls = 0) and
                        (V.Constructed = False) and
                        (V.Tag = 0) and
                        (V.Length = 0) then begin
                  HasNull := True;
                  Break;
                end;
                if Id >= Struct.ItemCount then
                  raise Exception.Create('Invalid format: ' + Struct.GetNamePath);
              end;
              while (Id < Struct.ItemCount) and not HasNull do begin
                Fld := Struct.Items[Id];
                if Fld^.TypeIdentified then
                  Fld^.TypeIdentify(Tmpl)
                else
                  Tmpl := Fld;
                if (Fld^.ChoiceCount > 0) and
                   ((Tmpl = nil) or
                    Tmpl^.TrySelectChoice(V.Cls,V.Constructed,V.Tag)) then begin
                  if Assigned(Tmpl) and (Tmpl <> Fld) then
                    Fld^.SelectChoice(Tmpl)
                  else begin
                    Fld^.FTag := V.Tag;
                    Fld^.FCls := V.Cls;
                    Fld^.FConstructed := V.Constructed;
                  end;
                  Fld^.FLength := V.Length;
                  if (V.Cls <> V_ASN1_UNIVERSAL) and Fld^.Implicit then begin
                    SaveBuf := Buf;
                    SaveBufLen := BufLen;
                    try
                      Count := Count + ASN1CntFromBuf(Buf,BufLen,Fld^);
                    except
                      on E:EInvalidTag do begin
                        if Fld^.FStrictTagging then
                          raise;
                        Buf := SaveBuf;
                        BufLen := SaveBufLen;
                        // EXPLICIT / IMPLICIT Confusion ?
                        ImplicitToExplicit;
                        V := TASN1Struct.Create;
                        Break;
                      end;
                      on E: Exception do
                        raise;
                    end;
                  end else
                    Count := Count + ASN1CntFromBuf(Buf,BufLen,Fld^);
                  if Struct.FForgetOptionalDefault then begin
                    Fld^.FOptional := False;
                    Fld^.FDefault := '';
                    Fld^.FHasDefaultValue := False;
                  end;
                  Break;
                end else if (V.Cls = Struct.Items[Id].Cls) and
                            (((V.Tag = Struct.Items[Id].Tag) and
                              (V.Constructed = Struct.Items[Id].Constructed)) or
                            ((Fld^.Tag = Cardinal(V_ASN1_UNDEF)) and
                             (Id = Struct.ItemCount - 1))) then begin
                  if Fld^.Tag = Cardinal(V_ASN1_UNDEF) then begin
                    Fld^.Constructed := V.Constructed;
                    Fld^.Tag := V.Tag;
                  end;
                  Fld^.FLength := V.Length;
                  if (V.Cls <> V_ASN1_UNIVERSAL) and Fld^.Implicit then begin
                    SaveBuf := Buf;
                    SaveBufLen := BufLen;
                    try
                      Count := Count + ASN1CntFromBuf(Buf,BufLen,Fld^);
                    except
                      on E:EInvalidTag do begin
                        if Fld^.FStrictTagging then
                          raise;
                        Buf := SaveBuf;
                        BufLen := SaveBufLen;
                        // EXPLICIT / IMPLICIT Confusion ?
                        ImplicitToExplicit;
                        V := TASN1Struct.Create;
                        Break;
                      end;
                      on E: Exception do
                        raise;
                    end;
                  end else
                    Count := Count + ASN1CntFromBuf(Buf,BufLen,Fld^);
                  if Struct.FForgetOptionalDefault then begin
                    Fld^.FOptional := False;
                    Fld^.FDefault := '';
                    Fld^.FHasDefaultValue := False;
                  end;
                  Break;
                end else if (V.Cls = Struct.Items[Id].Cls) and
                            (V.Cls <> V_ASN1_UNIVERSAL) and
                            (V.Tag = Struct.Items[Id].Tag) and
                            (V.Constructed xor Struct.Items[Id].Constructed) and
                            V.Constructed and
                            not Struct.FStrictTagging then begin
                // EXPLICIT / IMPLICIT Confusion
                  ImplicitToExplicit;
                  if Struct.FForgetOptionalDefault then begin
                    V.FOptional := False;
                    V.FDefault := '';
                    V.FHasDefaultValue := False;
                  end;
                  V := TASN1Struct.Create;
                  Break;
                end else if Fld^.HasDefaultValue then
                  Fld^.SetToDefault
                else if Fld^.Optional then
                  Fld^.Clear
                else
                  raise Exception.Create(Format('Syntax error:'#13#10 +
                                                'Tag %d - %d expected'#13#10 +
                                                'Cls %d - %d expected',
                                                [V.Tag,Fld^.Tag,V.Cls,Fld^.Cls]));
                Inc(Id);
              end;
              Inc(Id);
            end;
          finally
            V.Free;
          end;
        end;
        while (Id < Struct.ItemCount) and not HasNull do begin
          Fld := Struct.Items[Id];
          if Fld^.HasDefaultValue then
            Fld^.SetToDefault
          else if not Fld^.Optional then
            raise Exception.Create(Format('Syntax error:'#13#10 +
                                          'Tag %d expected'#13#10 +
                                          'Cls %d expected',
                                        [Fld^.Tag,Fld^.Cls]));
          Inc(Id);
        end;
      end;
    end else begin
      Struct.AllocContents(0);
      Count := 0;
      Id := 0;
      while (Struct.Length < 0) or (Count < Struct.Length) do begin
        Struct.AllocContents(Id + 1);
        Fld := Struct.Items[Id];
        Fld^.FReadOnly := SaveReadOnly;
        Count := Count + ASN1FromBuf(Buf,BufLen,Fld^);
        Struct.DoneLoading := Fld^.DoneLoading;
        if Struct.AllowResume and not Fld^.DoneLoading then
          Break
        else if (Struct.Length < 0) and
                (Fld^.Cls = 0) and
                (Fld^.Constructed = False) and
                (Fld^.Tag = 0) and
                (Fld^.Length = 0) then
          Break;
        Inc(Id);
      end;
      if Id > 0 then
        Struct.Contents^.ItemCount := Id;
      Struct.DoneLoading := (Struct.Length < 0) or (Count = Struct.Length);
    end;
    Struct.FReadOnly := SaveReadOnly;
  end else if Struct.CaptureContent then begin
    Done := False;
    RS := TReadStream.Create(Buf^,BufLen);
    try
      Count := Struct.DoCaptureLoad(Struct,
                                    RS,
                                    Done) +
               Struct.FContentCountLoad;
    finally
      RS.Free;
    end;
    Buf := Buf + Count;
    BufLen := BufLen - Count;
    Struct.FContentCountLoad := Count;
    Struct.DoneLoading := Done;
//    Assert((Struct.Length < 0) or (Count = Struct.Length) or not Done);
  end else begin
    if Struct.ReadOnly then begin
      Assert(BufLen >= Struct.Length);
      Count := Struct.Length;
      Struct.Cnt := Buf;
      Struct.DoneLoading := True;
    end else begin
      ReallocMem(Struct.Cnt,Struct.Length + 1);
      if BufLen < Struct.Length then begin
        Count := BufLen;
        Move(Buf^,Struct.Content^,Count);
        Struct.DoneLoading := False;
      end else begin
        Count := Struct.Length;
        if Count < 0 then
          raise EInvalidTag.Create('Not supported');
        Move(Buf^,Struct.Content^,Count);
        Struct.DoneLoading := True;
      end;
      Struct.Content[Struct.Length] := #0;
    end;
    Buf := Buf + Count;
    BufLen := BufLen - Count;
  end;
  Result := Result + Count;
  if (Count > Struct.Length) and (Struct.Length > -1) then begin
    Dec(Buf,Count - Struct.Length);
    Dec(Result,Count - Struct.Length);
    Inc(BufLen,Count - Struct.Length);
  end else if Count < Struct.Length then begin
    if Struct.AllowResume then
      Exit;
    raise EAbort.Create('Unable to load field: ' + Struct.FVarName);
  end;
  Struct.DoAfterLoad(Struct);
end;

function ASN1FromBuf(var Buf: PChar; var BufLen: Integer;
                     var Struct: TASN1Struct): Integer;
var
  Id: Integer;
  Done: Boolean;
  P: PASN1Struct;
  IC: Integer;
begin
  P := nil;
  if Assigned(Struct) and Struct.TypeIdentified then begin
    Struct.TypeIdentify(P);
    if (P = nil) or (P^.ChoiceCount > 0) then
      Struct.FTag := Cardinal(-1);
  end else if Assigned(Struct) and (Struct.ChoiceCount > 0) then
    Struct.FTag := Cardinal(-1);

  if Struct.ReadOnly then
    Struct.FReadOnlyCntSlave := Buf
  else
    Struct.FReadOnlyCntSlave := nil;

  Result := ASN1IdenAndLenFromBuf(Buf, BufLen, Struct, Done);
  if not Done then begin
    Struct.DoneLoading := False;
    if Struct.AllowResume then
      Exit;
    raise EAbort.Create('Unable to load field: ' + Struct.FVarName);
  end;
  Struct.FDoneLoading := False;

  if (Struct.ChoiceCount > 0) or Struct.TypeIdentified then begin
    if Struct.TypeIdentified then begin
      if Assigned(P) and (P^.ChoiceCount > 0) then
        if P^.TrySelectChoice(Struct.Cls,Struct.Constructed,Struct.Tag) then
          Struct.SelectChoice(P);
    end else begin
      Done := Integer(Struct.Tag) = -1;
      if not Done then
        for Id := 0 to Struct.ChoiceCount - 1 do
          if Struct.Choices[Id]^.TrySelectChoice(Struct.Cls,Struct.Constructed,Struct.Tag) then begin
            Struct.SelectChoice(Id);
            Done := True;
            Break;
          end;
      if not Done then
        raise EInvalidTag.Create(Format('Choice not registred for type %s:'#13#10 +
                                        'Tag: %d'#13#10'Cls: %d',
                                        [Struct.TypeName,Struct.Tag,Struct.Cls]));
    end;
  end;

  Struct.FUnambiguousField := -1;
  Struct.FUFTested := -1;

  if Struct.Length <> 0 then begin
    IC := InstanceCount;
    Result := Result + ASN1CntFromBuf(Buf,BufLen,Struct);
    Struct.FEstTotalCount := Struct.FEstTotalCount + InstanceCount - IC;
  end;

  if Struct.ReadOnly then
    Struct.FReadOnlyCntLenSlave := Result
  else
    Struct.FReadOnlyCntLenSlave := 0;
//  Struct.DoAfterLoad(Field);
end;

function ASN1CntFromStream(AStrm: TStream; var Struct: TASN1Struct): Integer;
var
  Count, Id: Integer;
  V: TASN1Struct;
  Done: Boolean;
begin
  Result := 0;
  Struct.DoLoad(Struct);

  if Struct.Constructed then begin
    if Struct.Persistent then begin
      Count := 0;
      Id := 0;
      V := TASN1Struct.Create;
      try
        V.AllowResume := True;
        if Assigned(Struct.FTemplate) then
          Struct.AllocContents(0);
        while (Struct.Length < 0) or (Count < Struct.Length) do begin
          if Assigned(Struct.FTemplate) then begin
            Struct.AddField;
            Struct.Contents^.ItemCount := Id + 1;
            if Struct.Length < 0 then begin
              if V.DoneLoading then begin
                V.DisposeContent;
                V.FLength := 0;
                V.ResetStreaming;
              end;
              Count := Count + ASN1IdenAndLenFromStream(AStrm,V,Done);
              Struct.DoneLoading := Done;
              if not Done then
                Break
              else if (Struct.Length < 0) and
                      (V.Cls = 0) and
                      (V.Constructed = False) and
                      (V.Tag = 0) and
                      (V.Length = 0) then begin
                Struct.Items[Id].Assign(V);
                Inc(Id);
                Break;
              end;
              if Struct.Items[Id].ChoiceCount > 0 then
                Struct.Items[Id].TrySelectChoice(V.Cls,V.Constructed,V.Tag)
              else if (Struct.Items[Id].Tag <> V.Tag) or
                      (Struct.Items[Id].Constructed xor V.Constructed) or
                      (Struct.Items[Id].Cls <> V.Cls) then
                raise Exception.Create('Syntax error');
              Struct.Items[Id].FLength := V.Length;
              Count := Count + ASN1CntFromStream(AStrm,Struct.Items[Id]^);
            end else
              Count := Count + ASN1FromStream(AStrm,Struct.Items[Id]^);
            Struct.DoneLoading := Struct.Items[Id].DoneLoading;
            if Struct.AllowResume and
               not Struct.Items[Id].DoneLoading then
              Break
            else if (Struct.Length < 0) and
                    (Struct.Items[Id]^.Cls = 0) and
                    (Struct.Items[Id]^.Constructed = False) and
                    (Struct.Items[Id]^.Tag = 0) and
                    (Struct.Items[Id]^.Length = 0) then
              Break;
          end else if Struct.UnambiguousField(Id) then begin
            if Id >= Struct.ItemCount then
              Struct.AllocContents(Id + 1);
            Count := Count + ASN1FromStream(AStrm,Struct.Items[Id]^);
            Struct.DoneLoading := Struct.Items[Id].DoneLoading;
            if Struct.Items[Id].AllowResume and
               not Struct.Items[Id].DoneLoading then
              Break
            else if (Struct.Length < 0) and
                    (Struct.Items[Id].Cls = 0) and
                    (Struct.Items[Id].Constructed = False) and
                    (Struct.Items[Id].Tag = 0) and
                    (Struct.Items[Id].Length = 0) then begin
              Struct.DeleteItem(Id);
              Break;
            end;
          end else begin
            if V.DoneLoading then begin
              V.DisposeContent;
              V.FLength := 0;
              V.ResetStreaming;
            end;
            Count := Count + ASN1IdenAndLenFromStream(AStrm,V,Done);
            Struct.DoneLoading := Done;
            if not Done then
              Break
            else if (Struct.Length < 0) and
                    (V.Cls = 0) and
                    (V.Constructed = False) and
                    (V.Tag = 0) and
                    (V.Length = 0) then
              Break;
            while Id < Struct.ItemCount do begin
              if (Struct.Items[Id]^.ChoiceCount > 0) and
                 not Struct.Items[Id]^.TypeIdentified and
                 Struct.Items[Id]^.TrySelectChoice(V.Cls,V.Constructed,V.Tag) then begin
                Struct.Items[Id]^.ResetStreaming;
                Struct.Items[Id]^.FLength := V.Length;
                Count := Count + ASN1CntFromStream(AStrm,Struct.Items[Id]^);
                Break;
              end else if (V.Cls = Struct.Items[Id].Cls) and
                          (((V.Tag = Struct.Items[Id].Tag) and
                            (V.Constructed or not Struct.Items[Id].Constructed)) or
                           (Struct.Items[Id].Tag = Cardinal(V_ASN1_UNDEF))) then begin
                Struct.Items[Id]^.ResetStreaming;
                if Struct.Items[Id].Tag = Cardinal(V_ASN1_UNDEF) then begin
                  Struct.Items[Id].Constructed := V.Constructed;
                  Struct.Items[Id].Tag := V.Tag;
                end else if (V.Constructed xor Struct.Items[Id].Constructed) then begin
                  Struct.Items[Id].Constructed := True;
                  Struct.Items[Id].CreateOFTemplate;
                  if Struct.Items[Id].Implicit then
                    Struct.Items[Id].Template.TypeName := Struct.Items[Id].ImplicitTypeName
                  else
                    Struct.Items[Id].Template.TypeName := Struct.Items[Id].TypeName;
                end;
                Struct.Items[Id]^.FLength := V.Length;
                Count := Count + ASN1CntFromStream(AStrm,Struct.Items[Id]^);
                Break;
              end else if Struct.Items[Id].HasDefaultValue then
                Struct.Items[Id]^.SetToDefault
              else if Struct.Items[Id].Optional then
                Struct.Items[Id].Clear
              else
                raise Exception.Create('Syntax error');
              Inc(Id);
            end;
          end;
          Inc(Id);
        end;
        while (Id < Struct.ItemCount) and not Struct.AllowResume do begin
          if Struct.Items[Id].Optional or Struct.Items[Id].HasDefaultValue then
            Struct.Items[Id].SetToDefault
          else
            raise Exception.Create(Format('Syntax error:'#13#10 +
                                          'Tag %d expected'#13#10 +
                                          'Cls %d expected',
                                          [Struct.Items[Id].Tag,Struct.Items[Id].Cls]));
          Inc(Id);
        end;
      finally
        V.Free;
      end;
    end else begin
      Struct.AllocContents(0);
      Count := 0;
      Id := 0;
      while (Struct.Length < 0) or (Count < Struct.Length) do begin
        Struct.AllocContents(Id + 1);
        Count := Count + ASN1FromStream(AStrm,Struct.Contents^.Items[Id]);
        Struct.DoneLoading := Struct.Items[Id].DoneLoading;
        if Struct.AllowResume and not Struct.Items[Id].DoneLoading then
          Break
        else if (Struct.Length < 0) and
                (Struct.Contents^.Items[Id].Cls = 0) and
                (Struct.Contents^.Items[Id].Constructed = False) and
                (Struct.Contents^.Items[Id].Tag = 0) and
                (Struct.Contents^.Items[Id].Length = 0) then
          Break;
        Inc(Id);
      end;
      if Id > 0 then
        Struct.Contents^.ItemCount := Id;
    end;
  end else if Struct.CaptureContent then begin
    Done := False;
    Count := Struct.DoCaptureLoad(Struct,
                                  TReadResumeStream(AStrm).DataStream,
                                  Done) +
             Struct.FContentCountLoad;
    Struct.FContentCountLoad := Count;
    Struct.DoneLoading := Done;
//    Assert((Struct.Length < 0) or (Count = Struct.Length) or not Done);
  end else begin
    if Struct.Length < 0 then begin
      Count := AStrm.Size - AStrm.Position;
      ReallocMem(Struct.Cnt,Count);
      Id := 0;
      repeat
        repeat
          Inc(Id,AStrm.Read(Struct.Content[Id],1));
        until (Id = Count) or (Struct.Content[Id-1] = #0);
        Inc(Id,AStrm.Read(Struct.Content[Id],1));
      until (Id = Count) or (Struct.Content[Id-1] = #0);
      Count := Id;
      Struct.DoneLoading := Struct.Content[Id-1] = #0;
    end else begin
      ReallocMem(Struct.Cnt,Struct.Length + 1);
      Count := AStrm.Read(Struct.Content^,Struct.Length);
      Struct.DoneLoading := Count = Struct.Length;
      Struct.Content[Struct.Length] := #0;
    end;
  end;
  if (Count <> Struct.Length) and (Struct.Length > -1) then begin
    if Struct.AllowResume then
      Exit;
    raise Exception.Create('Unable to load field: ' + Struct.FVarName);
  end;
  Result := Result + Count;
  Struct.DoAfterLoad(Struct);
end;

function ASN1FromStream(AStrm: TStream; var Struct: TASN1Struct): Integer;
var
  Id: Integer;
  Done: Boolean;
  Buf: PChar;
  PLen, BufLen: Integer;
  P: PASN1Struct;
  IC: Integer;
begin
  P := nil;
  if Assigned(Struct) and Struct.TypeIdentified then
    Struct.TypeIdentify(P);

  Result := ASN1IdenAndLenFromStream(AStrm, Struct, Done);

  if not Done then begin
    Struct.DoneLoading := False;
    if Struct.AllowResume then
      Exit;
    raise Exception.Create('Unable to load field: ' + Struct.FVarName);
  end;
  Struct.FDoneLoading := False;

  if (Struct.Length > 0) and not Struct.AllowResume then begin
    PLen := Struct.Length + Struct.IdenAndLenCountLoad;
    Struct.FReadOnlyCntLen := PLen;
    ReallocMem(Struct.FReadOnlyCnt,PLen + 1);
    Buf := Struct.FReadOnlyCnt;
    Buf[PLen] := #0;
    Move(Struct.FIdenAndLen,Buf^,Struct.FIdenAndLenCountLoad);
    BufLen := Struct.FIdenAndLenCountLoad +
              AStrm.Read(Buf[Struct.FIdenAndLenCountLoad],Struct.Length);
    Result := ASN1FromBuf(Buf,BufLen,Struct);
    Exit;
  end;

  if (Struct.ChoiceCount > 0) or Struct.TypeIdentified then begin
    if Struct.TypeIdentified then begin
      if Assigned(P) then
        if P^.TrySelectChoice(Struct.Cls,Struct.Constructed,Struct.Tag) then
          Struct.SelectChoice(P);
    end else
      for Id := 0 to Struct.ChoiceCount - 1 do
        if Struct.Choices[Id]^.TrySelectChoice(Struct.Cls,Struct.Constructed,Struct.Tag) then begin
          Struct.SelectChoice(Id);
          Break;
        end;
  end;

  Struct.FUnambiguousField := -1;
  Struct.FUFTested := -1;

  if Struct.Length <> 0 then begin
    IC := InstanceCount;
    Result := Result + ASN1CntFromStream(AStrm,Struct);
    Struct.FEstTotalCount := InstanceCount - IC;
  end else
    Struct.FDoneLoading := True;
  Struct.DoAfterLoad(Struct);
end;

function CalcASN1IdenAndLen(const Struct: TASN1Struct): Integer;
var
  B: Byte;
  Id, R: Integer;
begin
  // Quick bug fix:
  if Struct.ChoiceCount > 0 then
    if (Struct.ChoiceTypeName = '') and
       (Struct.Length = 0) and (Struct.Tag < 1) and (Struct.Cls = 0) then
      Struct.SelectChoice(0);

  Struct.FIdenAndLenCountSave := 0;

  if Struct.Tag < $1F then begin
    B := Struct.Cls or Struct.Tag;
    if Struct.Constructed then
      B := B or V_ASN1_CONSTRUCTED;
    Struct.FIdenAndLen[0] := B;
    R := 1;
  end else begin
    B := Struct.Cls or $1F;
    if Struct.Constructed then
      B := B or V_ASN1_CONSTRUCTED;
    Struct.FIdenAndLen[0] := B;
    R := 1;
    if Struct.Tag > $FFFFFFF then begin
      B := Struct.Tag shr 28 or $80;
      Struct.FIdenAndLen[R] := B;
      Inc(R);
    end;
    if Struct.Tag > $1FFFFF then begin
      B := Byte(Struct.Tag shr 21) or $80;
      Struct.FIdenAndLen[R] := B;
      Inc(R);
    end;
    if Struct.Tag > $3FFF then begin
      B := Byte(Struct.Tag shr 14) or $80;
      Struct.FIdenAndLen[R] := B;
      Inc(R);
    end;
    if Struct.Tag > $7F then begin
      B := Byte(Struct.Tag shr 7) or $80;
      Struct.FIdenAndLen[R] := B;
      Inc(R);
    end;
    B := Byte(Struct.Tag) and $7F;
    Struct.FIdenAndLen[R] := B;
    Inc(R);
  end;
  if Struct.Length < 0 then begin
    B := $80;
    Struct.FIdenAndLen[R] := B;
    Inc(R);
  end else if Struct.Length < $80 then begin
    B := Struct.Length;
    Struct.FIdenAndLen[R] := B;
    Inc(R);
  end else begin
    if Struct.Length > $FFFFFF then
      B := $84
    else if Struct.Length > $FFFF then
      B := $83
    else if Struct.Length > $FF then
      B := $82
    else
      B := $81;
    Struct.FIdenAndLen[R] := B;
    Inc(R);
    for Id := (B and $1F) - 1 downto 0 do begin
      B := Byte(Struct.Length shr (Id*8));
      Struct.FIdenAndLen[R] := B;
      Inc(R);
    end;
  end;
  Result := R;
  Struct.FIdenAndLenCountLoad := 0;
end;

function ASN1IdenAndLenToBuf(const Struct: TASN1Struct;
                             var Buf: PChar; var BufLen: Integer;
                             var Done: Boolean): Integer;
var
  R: Integer;
begin
  Struct.DoBeforeSave(Struct);

  // Might be changed by event handler:
  if Struct.FNoSave then begin
    Result := Struct.FIdenAndLenCountSave + Struct.Length;
    Struct.FDoneSaving := True;
    Exit;
  end;

  R := CalcASN1IdenAndLen(Struct);

  if Struct.FIdenAndLenCountSave < R then begin
    Result := R - Struct.FIdenAndLenCountSave;
    if Result > BufLen then Result := BufLen;
    Move(Struct.FIdenAndLen[Struct.FIdenAndLenCountSave],Buf^,Result);
    Inc(Buf,Result);
    Dec(BufLen,Result);
    Result := Result + Struct.FIdenAndLenCountSave;
  end else
    Result := R;    
  if Struct.AllowResume then
    Struct.FIdenAndLenCountSave := Result;
  Done := Result = R;
  if Done then
    Struct.FIdenAndLenCountLoad := 0;
end;

function ASN1ToBuf(const Struct: TASN1Struct; var Buf: PChar; var BufLen: Integer): Integer;
var
  B: Byte;
  Id, Count, Len: Integer;
  MS: TSecureMemoryStream;
  Done: Boolean;

  function WriteToBuf(const Src; SrcLen: Integer;
                      var Buf: PChar; var BufLen: Integer): Integer;
  begin
    Result := BufLen;
    if Result > SrcLen then
      Result := SrcLen;
    Move(Src,Buf^,Result);
    Inc(Buf,Result);
    Dec(BufLen,Result);
  end;

begin
  Result := 0;
  if Struct.HasDefaultValue then begin
    MS := TSecureMemoryStream.Create;
    try
      Struct.FHasDefaultValue := False;
      ASN1ToStream(Struct,MS);
      Struct.FHasDefaultValue := True;
      if (MS.Size <> Length(Struct.Default)) or
         not CompareMem(MS.Memory,Pointer(Struct.Default),MS.Size) then
        Result := Result + WriteToBuf(MS.Memory^,MS.Size,Buf,BufLen);
    finally
      MS.Free;
    end;
  end else begin
    Result := ASN1IdenAndLenToBuf(Struct,Buf,BufLen,Struct.FDoneSaving);
    if Struct.FNoSave or not Struct.FDoneSaving then Exit;
    Struct.FDoneSaving := False;

    Struct.DoSave(Struct);

    if Struct.Constructed then begin
      if Struct.Cnt = nil then
        Struct.AllocContents(0);
      Done := True;
      for Id := 0 to Struct.ItemCount - 1 do begin
        if Struct.Items[Id]^.HasDefaultValue then begin
          MS := TSecureMemoryStream.Create;
          try
            ASN1ToStream(Struct.Contents^.Items[Id],MS);
            if (MS.Size <> Length(Struct.Contents^.Items[Id].Default)) or
               not CompareMem(MS.Memory,Pointer(Struct.Contents^.Items[Id].Default),MS.Size) then
              Result := Result + WriteToBuf(MS.Memory^,MS.Size,Buf,BufLen);
          finally
            MS.Free;
          end;
        end else if not (Struct.Contents^.Items[Id].Optional and
                         Struct.Contents^.Items[Id].IsEmpty) then
          Result := Result + ASN1ToBuf(Struct.Items[Id]^,Buf,BufLen)
        else
          Struct.Items[Id].DoneSaving := True;
        Done := Done and Struct.Items[Id].DoneSaving;
      end;
      if Struct.Length < 0 then begin
        B := 0;
        if BufLen > 1 then begin
          Buf[0] := #0;
          Buf[1] := #0;
          Inc(Buf,2);
          Dec(BufLen,2);
          Result := Result + 2;
        end else if BufLen = 1 then begin
          Buf[0] := #0;
          Inc(Buf);
          Dec(BufLen);
          Result := Result + 1;
          Done := False;
        end else
          Done := False;
      end;
      Struct.DoneSaving := Done;
    end else if Struct.CaptureContent then begin
      Done := False;
      MS := TSecureMemoryStream.Create;
      try
        Count := Struct.DoCaptureSave(Struct,MS,Done);
        WriteToBuf(MS.Memory^,Count,Buf,BufLen);
      finally
        MS.Free;
      end;
      Assert((Count = Struct.Length) or (Struct.Length < 0) or not Done);
      Struct.DoneSaving := Done;
      Result := Result + Count;
    end else begin
      if (Struct.Length < 0) and Assigned(Struct.Cnt) then begin
        Len := StrLen(Struct.Content);
        if (Len > Struct.Length) then
          Len := Struct.Length;
      end else
        Len := Struct.Length;
      Count := WriteToBuf(Struct.Content^,Len,Buf,BufLen);
      Result := Result + Count;
      Done := Count = Len;
      if Done and (Struct.Length < 0) then begin
        B := 0;
        Done := WriteToBuf(B,1,Buf,BufLen) = 1;
        if Done then begin
          Inc(Result);
          Done := WriteToBuf(B,1,Buf,BufLen) = 1;
          if Done then
            Inc(Result);
        end;
      end;
      Struct.DoneSaving := Done;
    end;
  end;
  Struct.DoAfterSave(Struct);
end;

function ASN1IdenAndLenToStream(const Struct: TASN1Struct;
                                AStrm: TStream;
                                var Done: Boolean): Integer;
var
  R: Integer;
begin
  Struct.DoBeforeSave(Struct);

  // Might be changed by event handler:
  if Struct.FNoSave then begin
    Result := Struct.FIdenAndLenCountSave + Struct.Length;
    Struct.FDoneSaving := True;
    Exit;
  end;

  R := CalcASN1IdenAndLen(Struct);

  if Struct.FIdenAndLenCountSave < R then
    Result := AStrm.Write(Struct.FIdenAndLen[Struct.FIdenAndLenCountSave],
                          R - Struct.FIdenAndLenCountSave) +
              Struct.FIdenAndLenCountSave
  else
    Result := R;    
  if Struct.AllowResume then
    Struct.FIdenAndLenCountSave := Result;
  Done := Result = R;
  if Done then
    Struct.FIdenAndLenCountLoad := 0;
end;

function ASN1ToStream(const Struct: TASN1Struct; AStrm: TStream): Integer;
var
  B: Byte;
  Id, Count, Len: Integer;
  MS: TSecureMemoryStream;
  Done: Boolean;
begin
  Assert(Assigned(Struct));
  if (Assigned(Struct.FReadOnlyCnt) or Assigned(Struct.FReadOnlyCntSlave)) and
     Struct.ReadOnly and not Struct.FModifiedChild then begin
    if Assigned(Struct.FReadOnlyCnt) then
      Result := AStrm.Write(Struct.FReadOnlyCnt^,Struct.FReadOnlyCntLen)
    else
      Result := AStrm.Write(Struct.FReadOnlyCntSlave^,Struct.FReadOnlyCntLenSlave);
    Exit;
  end;
  try
  Result := 0;
  if Struct.HasDefaultValue then begin
    MS := TSecureMemoryStream.Create;
    try
      Struct.FHasDefaultValue := False;
      ASN1ToStream(Struct,MS);
      Struct.FHasDefaultValue := True;
      if (MS.Size <> Length(Struct.Default)) or
         not CompareMem(MS.Memory,Pointer(Struct.Default),MS.Size) then
        Result := Result + AStrm.CopyFrom(MS,0);
    finally
      MS.Free;
    end;
  end else begin
    Result := ASN1IdenAndLenToStream(Struct,AStrm,Struct.FDoneSaving);
    if Struct.FNoSave or not Struct.FDoneSaving then Exit;
    Struct.FDoneSaving := False;

    Struct.DoSave(Struct);

    if Struct.Constructed then begin
      if Struct.Cnt = nil then
        Struct.AllocContents(0);
      Done := True;
      for Id := 0 to Struct.ItemCount - 1 do begin
        if Struct.Items[Id]^.HasDefaultValue then begin
          MS := TSecureMemoryStream.Create;
          try
            ASN1ToStream(Struct.Contents^.Items[Id],MS);
            if (MS.Size <> Length(Struct.Contents^.Items[Id].Default)) or
               not CompareMem(MS.Memory,Pointer(Struct.Contents^.Items[Id].Default),MS.Size) then
              Result := Result + AStrm.CopyFrom(MS,0);
          finally
            MS.Free;
          end;
        end else if not (Struct.Contents^.Items[Id].Optional and
                         Struct.Contents^.Items[Id].IsEmpty) then
          Result := Result + ASN1ToStream(Struct.Items[Id]^,AStrm)
        else
          Struct.Items[Id].DoneSaving := True;
        Done := Done and Struct.Items[Id].DoneSaving;
      end;
      if Struct.Length < 0 then begin
        B := 0;
        if AStrm.Write(B,1) < 1 then
          Done := False
        else
          Inc(Result);
        if AStrm.Write(B,1) < 1 then
          Done := False
        else
          Inc(Result);
      end;
      Struct.DoneSaving := Done;
    end else if Struct.CaptureContent then begin
      Done := False;
      Count := Struct.DoCaptureSave(Struct,AStrm,Done);
      Assert((Count = Struct.Length) or (Struct.Length < 0) or not Done);
      Struct.DoneSaving := Done;
      Result := Result + Count;
    end else begin
      if (Struct.Length < 0) and Assigned(Struct.Cnt) then begin
        Len := StrLen(Struct.Content);
        if (Len > Struct.Length) then
          Len := Struct.Length;
      end else
        Len := Struct.Length;
      Count := AStrm.Write(Struct.Content^,Len);
      Result := Result + Count;
      Done := Count = Len;
      if Done and (Struct.Length < 0) then begin
        B := 0;
        Done := AStrm.Write(B,1) = 1;
        if Done then begin
          Inc(Result);
          Done := AStrm.Write(B,1) = 1;
          if Done then
            Inc(Result);
        end;
      end;
      Struct.DoneSaving := Done;
    end;
  end;
  Struct.DoAfterSave(Struct);
  except
    on E: Exception do
      raise Exception.Create('ASN1ToStream: Could not save field ' + Struct.VarName + #13#10 +
                             E.ClassName + ': ' + E.Message);
  end;
end;

procedure CopyASN1Struct(var Dest: TASN1Struct; const Source: TASN1Struct);
var
  MS: TSecureMemoryStream;
begin
  MS := TSecureMemoryStream.Create;
  try
    Source.ResetStreaming;
    ASN1ToStream(Source,MS);
    MS.Position := 0;
    if Assigned(Dest) then
      Dest.ResetStreaming;
    ASN1FromStream(MS,Dest);
    FillChar(MS.Memory^,MS.Size,0);
  finally
    MS.Free;
  end;
end;

procedure AddASN1Field(var Dest: TASN1Struct; const Source: TASN1Struct);
var
  Id: Integer;
begin
  if not Dest.Constructed then
    raise Exception.Create('Destination is not a constructed type');
  if Dest.Contents = nil then begin
    Dest.AllocContents(1);
    Id := 0;
  end else begin
    Id := Dest.Contents^.ItemCount;
    Dest.AllocContents(Id + 1);
  end;
  Dest.Contents^.Items[Id].Assign(Source);
  Dest.Contents^.ItemCount := Id + 1;
  Dest.CalculateLength;
end;

function AddComposeASN1Field(var Dest: TASN1Struct; Cls: Byte;
  Constructed: Boolean; Tag: Cardinal): PASN1Struct;
var
  Struct: TASN1Struct;
begin
  Assert(((Tag <> V_ASN1_SEQUENCE) and (Tag <> V_ASN1_SET)) or Constructed or
         (Cls <> V_ASN1_UNIVERSAL));
  FillChar(Struct,SizeOf(Struct),0);
  NewComposeASN1Struct(Struct,Cls,Constructed,Tag);
  if Constructed then
    Struct.AllocContents(0);
  AddASN1Field(Dest,Struct);
  Result := @Dest.Contents^.Items[Dest.Contents^.ItemCount - 1];
  DisposeASN1Struct(Struct);
end;

function InterpretBERToOID(const Value: string): string;
begin
  Result := InterpretBERToOID(Pchar(Value),Length(Value));
end;                     

function InterpretBERToOID(Str: PChar; Len: Integer): string;
var
  I, J, X, Y: Cardinal;
  T: Byte;
  OldOIDLen: Integer;
  OID, N: string;
  O: Pointer;
begin
  if (Str = nil) or (Len = 0) then begin
    Result := '';
    Exit;
  end;
  J := 0;
  X := 0;
  for I := 0 to Len-1 do begin
    T := Byte(Str[I]);
    X := X shl 7;
    X := X + (T and $7F);
    if T and $80 = 0 then begin
      if J = 0 then begin
        Y := X div 40;
        X := X mod 40;
        OID := IntToStr(Y);
        Inc(J);
      end;
      N := IntToStr(X);
      OldOIDLen := Length(OID);
      SetLength(OID,OldOIDLen + 1 + Length(N));
      PChar(OID)[OldOIDLen] := '.';
      O := Pointer(OID);
      Move(Pointer(N)^,Ptr(LongInt(O) + OldOIDLen + 1)^,Length(N));
      X := 0;
      Inc(J);
    end;
  end;
  if X <> 0 then begin
    Result := '';
//    Assert(False,'Illegal format');
    Exit;
  end;
  Result := OID;
end;

function ExtractSubIden(const S: string; var StartPos: Integer): Int64;
var
  V: string;
begin
  Result := 0;
  if StartPos > Length(S) then begin
    Assert(False,'ExtractSubIden: StartPos out of range'#13#10 +
           S + #13#10 +
           'Pos: ' + IntToStr(StartPos));
    Exit;
  end;        
  V := '';
  repeat
    V := V + S[StartPos];
    Inc(StartPos);
  until (StartPos > Length(S)) or (S[StartPos] = '.');
  if StartPos < Length(S) then Inc(StartPos);
  Result := StrToInt64(V);
end;

function InterpretOIDToBER(const OID: string): string;
var
  I: Integer;
  X, Y: Int64;

  function EncodeSubIden(Value: Int64): string;
  var
    T: Byte;
  begin
    T := Value and $7F;
    Result := Char(T);
    Value := Value shr 7;
    while Value > 0 do begin
      T := (Value and $7F) or $80;
      Result := Char(T) + Result;
      Value := Value shr 7;
    end;
  end;

begin
  I := 1;
  Y := ExtractSubIden(OID,I);
  if I >= Length(OID) then begin
    // "OID" appears to be a decimal integer value.
    Result := '';
    while Y > 0 do begin
      Result := Char(Y and $FF) + Result;
      Y := Y shr 8;
    end;
    if Result = '' then
      Result := #0
    else if Byte(Result[1]) and $80 > 0 then
      Result := #0 + Result;
  end else begin
    X := ExtractSubIden(OID,I);
    X := X + Y*40;
    Result := '';
    repeat
      Result := Result + EncodeSubIden(X);
      X := ExtractSubIden(OID,I);
    until I > Length(OID);
    Result := Result + EncodeSubIden(X);
  end;
end;
                                                           
type
  TExtRec = packed record
    E: Word;
  case Byte of
    0: (F: Int64);
    1: (A: array [0..7] of Byte);
  end;

function InterpretBERtoReal(const Value: string): Extended;
var
  B, Base, F: Byte;
  E, S: LongWord;
  ECount, EStart, NStart: Integer;
  ES: SmallInt;
  I: Integer;
  X: Extended;
begin
  Assert(Value <> '');
  FillChar(X,SizeOf(X),0);
  B := Byte(Value[1]);                   
  Result := 0;
  if B and $C0 = 0 then begin
    Assert(False,'Format not supported');
    Exit;
  end;
  if B and $C0 = $40 then begin
    if B = $40 then begin
      TExtRec(X).E := $7FFF;
      TExtRec(X).F := 0;
    end else if B = $41 then begin
      TExtRec(X).E := $FFFF;
      TExtRec(X).F := 0;
    end else begin
      Assert(False,'Syntax error');
      Exit;
    end;
  end else begin
    if B and $40 = 0 then
      S := 0
    else
      S := $8000;
    case B and $30 of
      $00: Base := 1;
      $10: Base := 3;
      $20: Base := 4;
    else
      Assert(False,'Format not supported');
      Exit;
    end;
    F := (B shr 2) and $03;
    if F >= Base then begin
      Assert(False,'Syntax error');
      Exit;
    end;
    EStart := 1;
    case B and $03 of
      $00: ECount := 1;
      $01: ECount := 2;
      $02: ECount := 3;
    else
      ECount := Byte(Value[2]);
      EStart := 2;
    end;
    if ECount > 2 then
      raise Exception.Create('Overflow');
    if ECount = 1 then begin
      ES := ShortInt(Value[EStart + 1]);
      NStart := EStart + 1;
    end else begin
      ES := ShortInt(Value[EStart + 1]) * 256 + Byte(Value[EStart + 2]);
      NStart := EStart + 2;
    end;
    if Length(Value) - NStart > 8 then begin
      Assert(False,'Overflow');
      Exit;
    end;
    if ES + $3FFF < 0 then begin
      Assert(False,'Overflow');
      Exit;
    end;
    E := (ES + $3FFF) + F;
    if E > $FFFF then begin
      Assert(False,'Overflow');
      Exit;
    end;
    TExtRec(X).E := E + S;
    if Byte(Value[NStart + 1]) and $80 = $80 then begin
      for I := NStart + 1 to Length(Value) - 1 do begin
        if 7-I+NStart < 0 then Break;
        TExtRec(X).A[7-I+NStart] := Byte(Value[I + 1]);
      end;
      TExtRec(X).A[7] := Byte(Value[NStart + 1]) and $7E;
      TExtRec(X).F := (TExtRec(X).F shl 1) or $01;
    end else begin
      for I := NStart to Length(Value) - 1 do
        TExtRec(X).A[7-I+NStart] := Byte(Value[I + 1]);
      TExtRec(X).F := TExtRec(X).F shl 1;
    end;
  end;
  Result := X;
end;

function InterpretRealtoBER(Value: Extended): string;
var
  I: Integer;
  S: byte;
  E: SmallInt;
  F: record
     case Byte of
       0: (F: Int64);
       1: (A: array [0..7] of Byte);
     end;
begin
  Assert((Value >= 0) xor (TExtRec(Value).E and $8000 > 0));
  SetLength(Result,11);
  if (Value < 0) then
    S := $40
  else
    S := $00;
  Result[1] := Char($80 + S + $01);

  E := (TExtRec(Value).E and $7FFF) - $3FFF;
  Result[2] := Char((E shr 8) and $FF);
  Result[3] := Char(E and $FF);

  F.F := TExtRec(Value).F shr 1;
  for I := 0 to 7 do
    Result[I + 4] := Char(F.A[7-I]);
  if TExtRec(Value).A[0] and $01 = $01 then
    Result[4] := Char(F.A[7] or $80);
end;

function Utf8ToUnicode(Str: string): WideString;
var
  I, J: Integer;
  C: Byte;
  WC: Cardinal;
begin
  Result := '';
  if Str = '' then
    Exit;
  SetLength(Result,Length(Str));
  I := 1;
  J := 1;
  while I <= Length(Str) do begin
    WC := Cardinal(Str[I]);
    Inc(I);
    if (WC and $80) <> 0 then begin
      WC := WC and $3F;
      if I > Length(Str) then Exit;
      if (WC and $20) <> 0 then begin
        C := Byte(Str[I]);
        Inc(I);
        if (C and $C0) <> $80 then Exit;
        if I > Length(Str) then Exit;
        WC := (WC shl 6) or (C and $3F);
      end;
      C := Byte(Str[I]);
      Inc(I);
      if (C and $C0) <> $80 then Exit;

      Result[J] := WideChar((WC shl 6) or (C and $3F));
    end else
      Result[J] := WideChar(WC);
    Inc(J);
  end;
  SetLength(Result,J-1);
end;     

function UnicodeToUtf8(Str: WideString): string;
var
  I, J: Integer;
  C: Cardinal;
begin
  Result := '';
  if Str = '' then Exit;
  SetLength(Result,Length(Str)*3);
  I := 1;
  J := 1;
  while I <= Length(Str) do begin
    C := Cardinal(Str[I]);
    Inc(I);
    if C <= $7F then begin
      Result[J] := Char(C);
      Inc(J);
    end else if C > $7FF then begin
      Result[J] := Char($E0 or (C shr 12));
      Result[J+1] := Char($80 or ((C shr 6) and $3F));
      Result[J+2] := Char($80 or (C and $3F));
      Inc(J,3);
    end else begin
      Result[J] := Char($C0 or (C shr 6));
      Result[J+1] := Char($80 or (C and $3F));
      Inc(J,2);
    end;
  end;
  SetLength(Result,J-1);
end;

function BMPToUnicode(Str: PChar; Len: Integer): WideString;
var
  W: Word;
  I: Integer;
begin
  SetLength(Result,Len shr 1);
  for I := 0 to (Len shr 1) - 1 do begin
    W := Byte(Str[I*2]) * 8 + Byte(Str[I*2 + 1]);
    Result[I+1] := WideChar(W);
  end;
end;

function UnicodeToBMP(const Str: WideString): string;
var
  W: Word;
  I: Integer;
begin
  SetLength(Result,Length(Str)*2);
  for I := 1 to Length(Str) do begin
    W := Word(Str[I]);
    Result[I*2-1] := Char(W shr 8);
    Result[I*2] := Char(Byte(W));
  end;
end;

function UTCTimeToSystemTime(Str: PChar; Len: Integer; var Time: TSystemTime): Boolean;
var
  I: Integer;
  T: TSystemTime;
begin
  Result := (Len <= Integer(StrLen(Str))) and (Len >= 13);
  if Result then begin
    Result := ((Str[12] = 'Z') and (Len = 13)) or
              ((Str[12] in ['+','-']) and (Len = 17));
    if Result then begin
      I := StrToIntDef(Copy(Str,1,2),-1);
      if I = -1 then
        Result := False
      else begin
        FillChar(T,SizeOf(T),0);
        if I < 50 then
          T.wYear := I + 2000
        else
          T.wYear := I + 1900;
        I := StrToIntDef(Copy(Str,3,2),-1);
        if (I < 1) or (I > 12) then
          Result := False
        else begin
          T.wMonth := I;
          I := StrToIntDef(Copy(Str,5,2),-1);
          if (I < 1) or (I > 31) then
            Result := False
          else begin
            T.wDay := I;
            I := StrToIntDef(Copy(Str,7,2),-1);
            if (I < 0) or (I > 23) then
              Result := False
            else begin
              T.wHour := I;
              I := StrToIntDef(Copy(Str,9,2),-1);
              if (I < 0) or (I > 59) then
                Result := False
              else begin
                T.wMinute := I;
                I := StrToIntDef(Copy(Str,11,2),-1);
                if (I < 0) or (I > 59) then
                  Result := False
                else begin
                  T.wSecond := I;
                  Time := T;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;
        
{$IFDEF LINUX}
function SystemTimeToDateTime(const T: TSystemTime): TDateTime;
begin
  Result := EncodeDateTime(T.wYear,T.wMonth,T.wDay,T.wHour,T.wMinute,T.wSecond,T.wMilliseconds);
end;
{$ENDIF}

function UTCTimeToDateTime(Str: PChar; Len: Integer; var Time: TDateTime): Boolean;
var
  T: TSystemTime;
begin
  Result := UTCTimeToSystemTime(Str,Len,T);
  if Result then
    Time := SystemTimeToDateTime(T);
end;

function X509GeneralizedTimeToDateTime(Str: PChar; Len: Integer; var Time: TDateTime): Boolean;
var
  I: Integer;
  T: TSystemTime;
begin
  Result := (Len <= Integer(StrLen(Str))) and (Len >= 15);
  if Result then begin
    Result := Str[14] = 'Z';
    if Result then begin
      I := StrToIntDef(Copy(Str,1,4),-1);
      if I = -1 then
        Result := False
      else begin
        FillChar(T,SizeOf(T),0);
        T.wYear := I;
        I := StrToIntDef(Copy(Str,5,2),-1);
        if (I < 1) or (I > 12) then
          Result := False
        else begin
          T.wMonth := I;
          I := StrToIntDef(Copy(Str,7,2),-1);
          if (I < 1) or (I > 31) then
            Result := False
          else begin
            T.wDay := I;
            I := StrToIntDef(Copy(Str,9,2),-1);
            if (I < 0) or (I > 23) then
              Result := False
            else begin
              T.wHour := I;
              I := StrToIntDef(Copy(Str,11,2),-1);
              if (I < 0) or (I > 59) then
                Result := False
              else begin
                T.wMinute := I;
                I := StrToIntDef(Copy(Str,13,2),-1);
                if (I < 0) or (I > 59) then
                  Result := False
                else begin
                  T.wSecond := I;
                  T.wMilliseconds := 0;
                  Time := SystemTimeToDateTime(T);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function GeneralizedTimeToSystemTime(Str: PChar; Len: Integer; var Time: TSystemTime): Boolean;
var
  I: Integer;
  T: TSystemTime;
begin
  Result := (Len <= Integer(StrLen(Str))) and ((Len >= 15) or (Len > 16));
  if Result then begin
    Result := Str[Len-1] = 'Z';
    if Result then begin
      I := StrToIntDef(Copy(Str,1,4),-1);
      if I = -1 then
        Result := False
      else begin
        FillChar(T,SizeOf(T),0);
        T.wYear := I;
        I := StrToIntDef(Copy(Str,5,2),-1);
        if (I < 1) or (I > 12) then
          Result := False
        else begin
          T.wMonth := I;
          I := StrToIntDef(Copy(Str,7,2),-1);
          if (I < 1) or (I > 31) then
            Result := False
          else begin
            T.wDay := I;
            I := StrToIntDef(Copy(Str,9,2),-1);
            if (I < 0) or (I > 23) then
              Result := False
            else begin
              T.wHour := I;
              I := StrToIntDef(Copy(Str,11,2),-1);
              if (I < 0) or (I > 59) then
                Result := False
              else begin
                T.wMinute := I;
                I := StrToIntDef(Copy(Str,13,2),-1);
                if (I < 0) or (I > 59) then
                  Result := False
                else begin
                  T.wSecond := I;
                  if Len = 15 then begin
                    T.wMilliseconds := 0;
                    Time := T;
                  end else if Str[14] = '.' then begin
                    I := StrToIntDef(Copy(Str,16,Len-16),-1);
                    case Len of
                      17: I := I * 100;
                      18: I := I * 10;
                    else
                      while Len > 19 do begin
                        Dec(Len);
                        I := I div 10;
                      end;
                    end;
                    T.wMilliseconds := I;
                    if I < 0 then
                      Result := False
                    else
                      Time := T;
                  end else
                    Result := False;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function GeneralizedTimeToDateTime(Str: PChar; Len: Integer; var Time: TDateTime): Boolean;
var
  T: TSystemTime;
begin
  Result := GeneralizedTimeToSystemTime(Str, Len, T);
  if Result then
    Time := SystemTimeToDateTime(T);
end;

{$IFDEF LINUX}
procedure DateTimeToSystemTime(const DateTime: TDateTime; var T: TSystemTime);
begin
  DecodeDateTime(DateTime,
                 T.wYear,T.wMonth,T.wDay,
                 T.wHour,T.wMinute,T.wSecond,T.wMilliseconds);
end;
{$ENDIF}

procedure DateTimeToASN1(Time: TDateTime; var Struct: TASN1Struct);
var
  T: TSystemTime;
  S: string;
begin
  if ASN1StructAssigned(Struct) then
    DisposeASN1Struct(Struct);
  Struct := TASN1Struct.Create;
  Struct.Cls := V_ASN1_UNIVERSAL;
  Struct.Constructed := False;
  DateTimeToSystemTime(Time,T);
  if (T.wYear < 1950) or (T.wYear >= 2050) then begin
    Struct.Tag := V_ASN1_GENERALIZEDTIME;
    S := DateTimeToX509GeneralizedTime(Time);
  end else begin
    Struct.Tag := V_ASN1_UTCTIME;
    S := DateTimeToUTCTime(Time);
  end;
  Struct.SetContent(S[1],Length(S));
end;

procedure WordToBuf(Value: Word; Buf: PChar; Len: Integer);
var
  I: Integer;
begin
  for I := Len-1 downto 0 do begin
    Buf[I] := Char(Ord('0') + (Value mod 10));
    Value := Value div 10;
  end;
end;

function SystemTimeToUTCTime(Time: TSystemTime): string;
begin
  SetLength(Result,13);
  WordToBuf(Time.wYear,@Result[1],2);
  WordToBuf(Time.wMonth,@Result[3],2);
  WordToBuf(Time.wDay,@Result[5],2);
  WordToBuf(Time.wHour,@Result[7],2);
  WordToBuf(Time.wMinute,@Result[9],2);
  WordToBuf(Time.wSecond,@Result[11],2);
  Result[13] := 'Z';
end;

function DateTimeToUTCTime(Time: TDateTime): string;
var
  T: TSystemTime;
begin
  DateTimeToSystemTime(Time,T);
  Result := SystemTimeToUTCTime(T);
end;

function SystemTimeToX509GeneralizedTime(Time: TSystemTime): string;
begin
  SetLength(Result,15);
  WordToBuf(Time.wYear,@Result[1],4);
  WordToBuf(Time.wMonth,@Result[5],2);
  WordToBuf(Time.wDay,@Result[7],2);
  WordToBuf(Time.wHour,@Result[9],2);
  WordToBuf(Time.wMinute,@Result[11],2);
  WordToBuf(Time.wSecond,@Result[13],2);
  Result[15] := 'Z';
end;

function DateTimeToX509GeneralizedTime(Time: TDateTime): string;
var
  T: TSystemTime;
begin
  DateTimeToSystemTime(Time,T);
  Result := SystemTimeToX509GeneralizedTime(T);
end;

function SystemTimeToGeneralizedTime(Time: TSystemTime): string;
var
  I: Integer;
begin
  SetLength(Result,19);
  WordToBuf(Time.wYear,@Result[1],4);
  WordToBuf(Time.wMonth,@Result[5],2);
  WordToBuf(Time.wDay,@Result[7],2);
  WordToBuf(Time.wHour,@Result[9],2);
  WordToBuf(Time.wMinute,@Result[11],2);
  WordToBuf(Time.wSecond,@Result[13],2);
  Result[15] := '.';
  WordToBuf(Time.wMilliseconds,@Result[16],3);
  I := 18;
  while (I > 15) and (Result[I] = '0') do
    Dec(I);
  if I = 15 then
    I := 14;
  Result[I+1] := 'Z';
  SetLength(Result,I+1);
end;

function DateTimeToGeneralizedTime(Time: TDateTime): string;
var
  T: TSystemTime;
begin
  DateTimeToSystemTime(Time,T);
  Result := SystemTimeToGeneralizedTime(T);
end;

{ TASN1Struct }

function TASN1Struct.AddBooleanField(const AVarName: string; AValue,
  APersistent: Boolean): PASN1Struct;
begin
  Result := AddField(AVarName,'BOOLEAN','',APersistent);
  Result.EditContent(AValue);
end;

type
  TTypeState = record
    Tagged: Boolean;
    Implicit: Boolean;
    Cls: Byte;
    Tag: Cardinal;
    ImplicitTypeName: PChar;
    TemplateTypeName: PChar;
    SequenceOf: Boolean;
    SetOf: Boolean;
    Encapsulates: PChar;
    DefaultValue: PChar;
    Optional: Boolean;
    TypeName: PChar;
    LastDelim: PChar;
  end;

procedure ASN1ParseTypeName(const ATypeName: string; var State: TTypeState);
var
  P, Q, E: PChar;
  Len, Code: Integer;

  procedure SkipBlanks(var P: PChar; var Len: Integer);
  begin
    while (Len > 0) and (P[0] in [#0..#32]) do begin
      Dec(Len);
      Inc(P);
    end;
  end;

  function ExtractToken(var P: PChar; var Len: Integer): Boolean;
  begin
    Result := False;
    if (Len < 2) or (P[0] <> '-') or (P[1] <> '-') then
      while (Len > 0) and not (P[0] in [#0..#32,',','}']) do begin
        Result := True;
        Dec(Len);
        Inc(P);
      end;
    if not Result then
      P := nil;
  end;

  procedure Error;
  begin
    raise Exception.Create('ASN1ParseTypeName: Syntax error in token "' + ATypeName + '"');
  end;

begin
  P := PChar(ATypeName);
  Len := Length(ATypeName);

  State.TypeName := P;
  State.LastDelim := P + Len;
  State.Encapsulates := nil;

  SkipBlanks(P,Len);
  if Len > 0 then begin
    // Tagged?
    State.Tagged := P[0] = '[';
    if State.Tagged then begin
      Inc(P);
      Dec(Len);
      Q := P;
      while (Len > 0) and (Q[0] <> ']') do begin
        Dec(Len);
        Inc(Q);
      end;
      if Len = 0 then
        Error;

      State.Cls := V_ASN1_CONTEXT_SPECIFIC;
      if not (P[0] in ['0'..'9']) then begin
        if P[0] in ['a','A'] then begin
          if StrLIComp(P,'APPLICATION ',12) = 0 then
            State.Cls := V_ASN1_APPLICATION
          else
            Error;
          P := P + 12;
        end else if P[0] in ['p','P'] then begin
          if StrLIComp(P,'PRIVATE ',8) = 0 then
            State.Cls := V_ASN1_PRIVATE
          else
            Error;
          P := P + 8;
        end else if P[0] in ['c','C'] then begin
          if StrLIComp(P,'CONTEXT SPECIFIC ',17) = 0 then
            State.Cls := V_ASN1_CONTEXT_SPECIFIC
          else
            Error;
          P := P + 17;
        end else if P[0] in ['u','U'] then begin
          if StrLIComp(P,'UNIVERSAL ',10) = 0 then
            State.Cls := V_ASN1_UNIVERSAL
          else
            Error;
          P := P + 10;
        end
      end;

      Val(Copy(P,1,Q-P),State.Tag,Code);
      if Code <> 0 then
        Error;
      P := Q + 1;
      Dec(Len);
      SkipBlanks(P,Len);
      Q := P;
      if not ExtractToken(Q,Len) then
        Error;
      if P[0] in ['i','I'] then begin
        if (Q-P = 8) and (P[1] in ['m','M']) and
           (StrLIComp(P,'IMPLICIT',8) = 0) then begin
          State.Implicit := True;
          P := Q;
          SkipBlanks(P,Len);
          Q := P;
          if not ExtractToken(Q,Len) then
            Error;
        end else
          State.Implicit := False;
      end else begin
        State.Implicit := False;
        if P[0] in ['e','E'] then begin
          if (Q-P = 8) and (P[1] in ['x','X']) and
             (StrLIComp(P,'EXPLICIT',8) = 0) then begin
            P := Q;
            SkipBlanks(P,Len);
            Q := P;
            if not ExtractToken(Q,Len) then
              Error;
          end;
        end;
      end;
    end else begin
      State.Implicit := False;
      State.Cls := V_ASN1_UNIVERSAL;
      State.Tag := Cardinal(V_ASN1_UNDEF);
      Q := P;
      if not ExtractToken(Q,Len) then
        Error;
    end;
    State.ImplicitTypeName := P;

    // SEQUENCE OF or SET OF ?
    State.TemplateTypeName := nil;
    State.Optional := False;
    State.DefaultValue := nil;
    State.SequenceOf := False;
    State.SetOf := False;
    if P[0] in ['s','S'] then begin
      State.SequenceOf := (Q-P = 8) and (StrLIComp(P,'SEQUENCE',8) = 0);
      if State.SequenceOf then begin
        if not State.Tagged then
          State.Tag := V_ASN1_SEQUENCE;
        State.SetOf := False;
      end else begin
        State.SetOf := (Q-P = 3) and (StrLIComp(P,'SET',3) = 0);
        if State.SetOf and not State.Tagged then
          State.Tag := V_ASN1_SET;
      end;
      if State.SequenceOf or State.SetOf then begin
        P := Q;
        E := P;
        SkipBlanks(P,Len);
        Q := P;
        if ExtractToken(Q,Len) then begin
          // The next token has been extracted and must also be tested for
          // OPTIONAL and DEFAULT:
          if P[0] in ['o','O'] then begin
            if (Q-P = 2) and (P[1] in ['f','F']) then begin
              P := Q;
              SkipBlanks(P,Len);
              Q := P;
              // There MUST be a template type name:
              if not ExtractToken(Q,Len) then
                Error;
              State.TemplateTypeName := P;
            end else if (Q-P  = 8) and (P[1] in ['p','P']) and
                        (StrLIComp(P,'OPTIONAL',Q-P) = 0) then begin
              State.LastDelim := E;
              State.Optional := True;
              P := Q;
              SkipBlanks(P,Len);
              Q := P;
              // There MUST NOT be any token after OPTIONAL:
              if ExtractToken(Q,Len) then
                Error;
            end;
          end else if P[0] in ['d','D'] then begin
            if (Q-P = 7) and (P[1] in ['e','E']) and
               (StrLIComp(P,'DEFAULT',7) = 0) then begin
              State.LastDelim := E;
              P := Q;
              SkipBlanks(P,Len);
              Q := P;
              // There MUST be a default value:
              if not ExtractToken(Q,Len) then
                Error;
              State.DefaultValue := P;
              Q := nil;
            end;
          end else if P[0] in ['e','E'] then begin
            if (Q-P = 12) and (P[1] in ['n','N']) and
               (StrLIComp(P,'ENCAPSULATES',Q-P) = 0) then begin
              P := Q;
              SkipBlanks(P,Len);
              Q := P;
              // There MUST be a encapsulated type:
              if not ExtractToken(Q,Len) then
                Error;
              State.Encapsulates := P;
            end;
          end;
        end;
      end;
    end;
    while Assigned(Q) do begin
      P := Q;
      E := P;
      SkipBlanks(P,Len);
      Q := P;
      if ExtractToken(Q,Len) then
        if P[0] in ['o','O'] then begin
          if (Q-P = 8) and (P[1] in ['p','P']) and
             (StrLIComp(P,'OPTIONAL',8) = 0) then begin
            State.LastDelim := E;
            State.Optional := True;
            P := Q;
            SkipBlanks(P,Len);
            Q := P;
            // There MUST NOT be any token after OPTIONAL:
            if ExtractToken(Q,Len) then
              Error;
            Break;
          end;
        end else if P[0] in ['d','D'] then begin
          if (Q-P = 7) and (P[1] in ['e','E']) and
             (StrLIComp(P,'DEFAULT',7) = 0) then begin
            State.LastDelim := E;
            P := Q;
            SkipBlanks(P,Len);
            Q := P;
            // There MUST be a default value:
            if not ExtractToken(Q,Len) then
              Error;
            State.DefaultValue := P;
            Break;
          end;
        end else if (State.Encapsulates = nil) and
                    (P[0] in ['e','E']) then begin
          if (Q-P = 12) and (P[1] in ['n','N']) and
             (StrLIComp(P,'ENCAPSULATES',12) = 0) then begin
            P := Q;
            SkipBlanks(P,Len);
            Q := P;
            // There MUST be an encapsulated type:
            if not ExtractToken(Q,Len) then
              Error;
            State.Encapsulates := P;
          end;
        end;
    end;
  end;
end;

function TASN1Struct.AddField(const AVarName, ATypeName: string;
  AValue: TASN1Struct): PASN1Struct;
var
  State: TTypeState;
  P: Integer;
  PC: PChar;
  Str: string;
  S: PASN1Struct;
  MS: TMemoryStream;
begin
  if Assigned(AValue) and AValue.Constructed then
    AValue.CalculateLength;

  if ATypeName <> '' then
    ASN1ParseTypeName(ATypeName,State)
  else begin
    State.Implicit := False;
    State.Tagged := False;
    State.Cls := V_ASN1_UNIVERSAL;
    State.Tag := Cardinal(V_ASN1_UNDEF);
    State.ImplicitTypeName := nil;
    State.TemplateTypeName := nil;
    State.SequenceOf := False;
    State.SetOf := False;
    State.DefaultValue := nil;
    State.Optional := False;
  end;

  Result := FindField(AVarName);
  if Assigned(Result) then begin
    Result^.Persistent := False;
    Result^.TypeName := ATypeName;
    Result^.Persistent := True;
    if State.Tagged then begin
      Result^.FTag := State.Tag;
      Result^.FCls := State.Cls;
      Result^.FImplicit := State.Implicit;
      if not State.Implicit then begin
        Result := Result^.Items[0];
        Result^.TypeName := State.ImplicitTypeName;
      end else
        Result^.ImplicitTypeName := State.ImplicitTypeName;
    end;
  end else begin
    if AVarName <> '' then begin
      if AVarName[1] = '/' then begin
        Str := AVarName;
        PC := StrRScan(PChar(Str),'/');
        if PC = nil then
          raise Exception.Create('Illegal VarName');
        P := System.Length(Str) - System.Length(PC) + 1;
        S := FindField(Copy(Str,1,P-1));
        if S = nil then
          raise Exception.Create('Path not found');
        Result := S^.AddField(PC + 1,ATypeName,AValue);
        Exit;
      end else if AVarName[1] = '[' then begin
        if State.TemplateTypeName <> nil then begin
          Result := AddField;
          if ATypeName <> '' then
            Result^.TypeName := State.TemplateTypeName;
        end else
          raise Exception.Create('Path not found');
      end;
    end;
    if (State.Tagged and State.Implicit) or not State.Tagged then begin
      if Assigned(AValue) then begin
        AddASN1Field(Self,AValue);
        Result := Items[ItemCount - 1];
        Result^.FPersistent := True;
      end else begin
        AddComposeASN1Field(Self,State.Cls,True,State.Tag);
        Result := Items[ItemCount - 1];
      end;
      Result^.FStrictTagging := FStrictTagging;
      Result^.FVarName := AVarName;
      if State.Implicit then begin
        Result^.FImplicit := State.Implicit;
        Result^.FTypeName := ATypeName;
        Result^.ImplicitTypeName := State.ImplicitTypeName;
      end else begin
        Result^.FImplicit := False;
        Result^.FImplicitTypeName := '';
        Result^.FImplicitTag := 0;
        if ATypeName <> '' then
          Result^.SetTypeName(ATypeName);
      end;
      Result^.FPersistent := True;
      Result^.Optional := State.Optional;
      Result^.Default := HexToOS(State.DefaultValue);
      Result^.HasDefaultValue := Assigned(State.DefaultValue);
    end else if State.Tagged then begin
      AddComposeASN1Field(Self,State.Cls,True,State.Tag);
      Items[ItemCount - 1]^.FPersistent := True;
      Items[ItemCount - 1]^.FTypeName := ATypeName;
      Items[ItemCount - 1]^.VarName := AVarName;
      Items[ItemCount - 1]^.FStrictTagging := FStrictTagging;
      Items[ItemCount - 1]^.Optional := State.Optional;
      Items[ItemCount - 1]^.Default := HexToOS(State.DefaultValue);
      Items[ItemCount - 1]^.HasDefaultValue := Assigned(State.DefaultValue);
      AddComposeASN1Field(Items[ItemCount - 1]^,V_ASN1_UNIVERSAL,True,Cardinal(V_ASN1_UNDEF));
      Result := Items[ItemCount - 1].Items[0];
      Result^.SetTypeName(State.ImplicitTypeName);
      Result^.FStrictTagging := FStrictTagging;
    end else begin
      if State.SequenceOf then
        AddComposeASN1Field(Self,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE)
      else if State.SetOf then
        AddComposeASN1Field(Self,V_ASN1_UNIVERSAL,True,V_ASN1_SET)
      else
        raise Exception.Create('Could not interpret contents: ' + ATypeName);
      Result := Items[ItemCount - 1];
      Result^.FStrictTagging := FStrictTagging;
      Result.Persistent := True;
      Result^.TypeName := State.TemplateTypeName;
      Result^.VarName := AVarName;
      Result^.Optional := State.Optional;
      Result^.Default := HexToOS(State.DefaultValue);
      Result^.HasDefaultValue := Assigned(State.DefaultValue);
    end;
  end;

  if Assigned(State.TemplateTypeName) then begin
    if Assigned(Result.FTemplate) then begin
      Result.FTemplate._Release;
      Result.FTemplate := nil;
    end;
    if StrLIComp(State.TemplateTypeName,'SEQUENCE',8) = 0 then
      NewComposeASN1Struct(Result.FTemplate,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE)
    else if StrLIComp(State.TemplateTypeName,'SET',3) = 0 then
      NewComposeASN1Struct(Result.FTemplate,V_ASN1_UNIVERSAL,True,V_ASN1_SET)
    else begin
      NewComposeASN1Struct(Result.FTemplate,V_ASN1_UNIVERSAL,True,Cardinal(V_ASN1_UNDEF));
      Result.FTemplate.TypeName := State.TemplateTypeName;
    end;
    Result.FTemplate._AddRef;
    Result.Persistent := True;
    Result := @Result.FTemplate;
    Result^.FStrictTagging := FStrictTagging;
    Result.Persistent := True;
  end else if Assigned(Result.FTemplate) then begin
    Result.FTemplate._Release;
    Result.FTemplate := nil;
  end;

  if Assigned(AValue) then begin
    Result^.Assign(AValue);
    Result^.FVarName := AVarName;
  end else if Result^.HasDefaultValue then begin
    MS := TMemoryStream.Create;
    try
      MS.Write(Result^.Default[1],System.Length(Result^.Default));
      MS.Position := 0;
      Result^.Persistent := False;
      ASN1FromStream(MS,Result^);
      Result^.Persistent := True;
      Result^.TypeName := ATypeName;
    finally
      MS.Free;
    end;
  end else if Result^.Constructed and
              ((TypeName = '') or (TypeName[1] <> '[')) then
    Result.AllocContents(0);
  if Constructed then
    CalculateLength;
end;

function TASN1Struct.AddField(const AVarName, ATypeName: string;
  const AValue: WideString; APersistent: Boolean): PASN1Struct;
var
  Str: string;
  PC: PChar;
  P: Integer;
  SS: TStringStream;
  S: PASN1Struct;
begin
  Result := FindField(AVarName);
  if Assigned(Result) then begin
    Result^.TypeName := ATypeName;
    Result^.EditContent(AValue);
    Result^.Persistent := APersistent;
    Exit;
  end;

  if AVarName <> '' then begin
    if AVarName[1] = '/' then begin
      Str := AVarName;
      PC := StrRScan(PChar(Str),'/');
      if PC = nil then
        raise Exception.Create('Illegal VarName');
      P := System.Length(Str) - System.Length(PC) + 1;
      S := FindField(Copy(Str,1,P-1));
      if S = nil then
        raise Exception.Create('Path not found');
      Result := S^.AddField(PC + 1,ATypeName,AValue,APersistent);
      Exit;
    end;
  end;
  if ((Pos(' implicit ',LowerCase(ATypeName)) > 0) or
      (ATypeName = '') or (Trim(ATypeName)[1] <> '[')) and
     (Pos(' explicit ',LowerCase(ATypeName)) = 0) and
     (Pos(' of ',LowerCase(ATypeName)) = 0) then begin
    AddComposeASN1Field(Self,V_ASN1_UNIVERSAL,False,Cardinal(V_ASN1_UNDEF));
    Result := Items[ItemCount - 1];
    Result^.FStrictTagging := FStrictTagging;
    Result^.FPersistent := True;
    Result^.FVarName := AVarName;
    Result^.SetTypeName(ATypeName);
    if System.Length(AValue) > 0 then begin
      if Result.Constructed then begin
        Str := HexToOS(AValue);
        SS := TStringStream.Create(Str);
        try
          ASN1FromStream(SS,Result^);
          Result^.FVarName := AVarName;
          Result^.SetTypeName(ATypeName);
        finally
          SS.Free;
        end;
      end else if (Result.Cls = V_ASN1_UNIVERSAL) or Result.Implicit then begin
        Result.EditContent(AValue);
        Result.Persistent := APersistent;
      end else
        raise Exception.Create('Could not interpret contents');
    end;
  end else if ((Pos(' implicit ',LowerCase(ATypeName)) = 0) and
               (ATypeName <> '') and (Trim(ATypeName)[1] = '[')) or
              (Pos(' explicit ',LowerCase(ATypeName)) <> 0) then begin
    AddComposeASN1Field(Self,V_ASN1_CONTEXT_SPECIFIC,True,0);
    Items[ItemCount - 1]^.SetTypeName(ATypeName);
    Items[ItemCount - 1]^.VarName := AVarName;
    AddComposeASN1Field(Items[ItemCount - 1]^,V_ASN1_UNIVERSAL,False,Cardinal(V_ASN1_UNDEF));
    Result := Items[ItemCount - 1]^.Items[0];
    Result^.FStrictTagging := FStrictTagging;
    Str := AValue;
    if System.Length(Str) > 0 then
      raise Exception.Create('Could not interpret contents');
  end else
    Result := nil;
  if Assigned(Result) then
    Result^.FPersistent := Result.Constructed or APersistent;
  CalculateLength;
end;

function TASN1Struct.AddField: PASN1Struct;
var
  SaveReadOnly: Boolean;
begin
  if Assigned(FTemplate) then begin
    SaveReadOnly := ReadOnly;
    FTemplate.CalculateLength;
    if ItemCount <= 0 then
      AllocContents(1)
    else
      AllocContents(ItemCount + 1);
    Result := Items[ItemCount - 1];
    Result.Assign(Template);
    Result.FReadOnly := SaveReadOnly;
    Result.SetForgetOptionalDefault(FForgetOptionalDefault);
  end else begin
    AddComposeASN1Field(Self,V_ASN1_UNIVERSAL,True,Cardinal(V_ASN1_UNDEF));
    Result := Items[ItemCount - 1];
    Result^.Persistent := True;
  end;
  Result^.FStrictTagging := FStrictTagging;
end;  

function TASN1Struct.AddField(ChoiceIndex: Integer): PASN1Struct;
begin
  Result := AddField;
  Result^.SelectChoice(ChoiceIndex);
end;

function TASN1Struct.AddGeneralizedTimeField(const AVarName: string;
  AValue: TDateTime; APersistent: Boolean): PASN1Struct;
begin
  Result := AddField(AVarName,'GeneralizedTime','',APersistent);
  Result.EditContent(AValue);
end;

function TASN1Struct.AddIntegerField(const AVarName: string; AValue: Int64;
  StoreAsUnsigned, APersistent: Boolean): PASN1Struct;
begin
  Result := AddField(AVarName,'INTEGER','',APersistent);
  Result.EditContent(AValue,StoreAsUnsigned);
end;

function TASN1Struct.AddRealField(const AVarName: string;
  AValue: Extended; APersistent: Boolean): PASN1Struct;
begin
  Result := AddField(AVarName,'REAL','',APersistent);
  Result.EditContent(AValue);
end;

function TASN1Struct.AddUTCTimeField(const AVarName: string;
  AValue: TDateTime; APersistent: Boolean): PASN1Struct;
begin
  Result := AddField(AVarName,'UTCTime','',APersistent);
  Result.EditContent(AValue);
end;

procedure TASN1Struct.AllocContents(ItemCount: Cardinal);
var
  OldItemCount, Idx: Integer;
  Cnts: PASN1Structs;
begin
  Assert(Integer(ItemCount) >= 0);
  Assert(Constructed);
//  Assert(not ReadOnly);
  OldItemCount := GetItemCount;
  if OldItemCount < 0 then OldItemCount := 0;
  if (Cardinal(FCapacity) < ItemCount) or (Cnt = nil) then
    SetCapacity(ItemCount + 16);
  Cnts := Contents;
  if (ItemCount > 0) and (Integer(ItemCount) > OldItemCount) then begin
    for Idx := OldItemCount to ItemCount - 1 do begin
      Cnts^.Items[Idx] := TASN1Struct.Create;           
      Assert(Cnts^.Items[Idx].FRefCount = 0,'Internal error - wrong ref count');
      Assert(Cnts^.Items[Idx] <> Self,'Internal error');
      Cnts^.Items[Idx]._AddRef;
      Cnts^.Items[Idx].FNoChange := FNoChange;
      Cnts^.Items[Idx].FStrictTagging := FStrictTagging;
      Cnts^.Items[Idx].FOwner := Self;
      Cnts^.Items[Idx].FAllowResume := AllowResume;
      Cnts^.Items[Idx].FReadOnly := ReadOnly;
    end;
    Change(ctNewRec);
  end else begin
    for Idx := ItemCount to OldItemCount - 1 do begin
      try
        Assert(Assigned(Cnts^.Items[Idx]),'Item not assigned');
        Cnts^.Items[Idx].FOwner := nil;
        Cnts^.Items[Idx]._Release;
        Cnts^.Items[Idx] := nil;
      except
        on E: Exception do
          raise Exception.Create(Format('Failure releasing item %d (%d - %d):'#13#10'%s',
                                        [Idx,ItemCount,OldItemCount,E.Message]));
      end;
    end;
    Change(ctDelRec);
  end;
  Cnts^.ItemCount := ItemCount;
end;

procedure TASN1Struct.Assign(Source: TASN1Struct);
var
  Idx: Integer;
  RS: TReadStream;
  IC: Integer;
begin
  BeginUpdate;
  try
    LockInstanceList;
    try
      PreAllocInstances(TotalCount - Source.TotalCount);
      if (Assigned(Source.FReadOnlyCnt) or Assigned(Source.FReadOnlyCntSlave)) and
         ReadOnly and Source.ReadOnly and not Source.FModifiedChild then begin
        CopyTypeInfo(Source);
        if Assigned(Source.FReadOnlyCnt) then
          RS := TReadStream.Create(Source.FReadOnlyCnt^,Source.FReadOnlyCntLen)
        else
          RS := TReadStream.Create(Source.FReadOnlyCntSlave^,Source.FReadOnlyCntLenSlave);
        try
          ASN1FromStream(RS,Self);
        finally
          RS.Free;
        end;
      end else begin
        ReadOnly := False;
        IC := InstanceCount;
        try
          if Source = nil then
            raise Exception.Create('Source is nil');
          CopyTypeInfo(Source,True);
        except
          on E: Exception do
            raise Exception.Create(Format('CopyTypeInfo failure %s.%s to %s.%s:'#13#10'%s',
                                          [Source.FModuleName,Source.FTypeName,FModuleName,FTypeName,E.Message]));
        end;
        if Constructed then begin
          FLength := Source.FLength;
          if Source.ItemCount > 0 then
            AllocContents(Source.ItemCount)
          else
            AllocContents(0);
          for Idx := 0 to ItemCount - 1 do
            try
              Items[Idx]^.Assign(Source.Items[Idx]^)
            except
              on E: Exception do
                raise Exception.Create(Format('Assign item %d failure %s.%s to %s.%s:'#13#10'%s',
                                              [Idx,Source.FModuleName,Source.FTypeName,
                                               FModuleName,FTypeName,E.Message]));
            end;
        end else begin
          SetContent(Source.Cnt^,Source.Length);
        end;
        FEstTotalCount := FEstTotalCount + InstanceCount - IC;
      end;
    finally
      UnlockInstanceList;
    end;
  finally
    EndUpdate;
  end;
end;

function TASN1Struct.CalculateLength: Integer;
var
  MS, MSD: TSecureMemoryStream;
  Idx: Integer;
  SaveAllowResume: Boolean;
  Dummy: Boolean;
begin
  if not Constructed then
    Result := FLength
  else if (Assigned(FReadOnlyCnt) or Assigned(FReadOnlyCntSlave)) and
          ReadOnly and not FModifiedChild then begin
    Result := FReadOnlyCntLenSlave - CalcASN1IdenAndLen(Self);
    Assert(FLength = Result);
  end else begin

    SaveAllowResume := AllowResume;
    AllowResume := False;

    if Cnt = nil then
      AllocContents(0);
    Result := 0;
    MS := TSecureMemoryStream.Create;
    try
      for Idx := 0 to ItemCount - 1 do begin
        if Contents^.Items[Idx].CalculateLength < 0 then
          Result := -1;
        if Result < 0 then Continue;
        if Items[Idx]^.HasDefaultValue then begin
          MSD := TSecureMemoryStream.Create;
          try
            Items[Idx]^.FHasDefaultValue := False;
            ASN1ToStream(Items[Idx]^,MSD);
            Items[Idx]^.FHasDefaultValue := True;
            if (MSD.Size <> System.Length(Items[Idx].Default)) or
               not CompareMem(MSD.Memory,Pointer(Items[Idx].Default),MSD.Size) then
              Result := Result + MSD.Size;
          finally
            MSD.Free;
          end;
        end else if not (Items[Idx].IsEmpty and Items[Idx].Optional) then
          Result := Result + ASN1IdenAndLenToStream(Contents^.Items[Idx],MS,Dummy) +
                             Contents^.Items[Idx].FLength;
      end;
    finally
      MS.Free;
    end;
    FLength := Result;
    AllowResume := SaveAllowResume;
  end;
end;

procedure TASN1Struct.Clear;
var
  P: Pointer;
  C, Id: Cardinal;
  SaveReadOnly: Boolean;
begin
  if (ChoiceCount > 0) and Optional then begin
    FChoiceIndex := -1;
    FChoiceVarName := '';
    FChoiceTypeName := '';
    FEncapsulated.Free;
    FEncapsulated := nil;
    if Assigned(FTemplate) then
      FTemplate._Release;
    FTemplate := nil;
    DisposeContent;
    FTag := Cardinal(-1);
    FCls := V_ASN1_UNIVERSAL;
  end else if Constructed and Assigned(Cnt) then begin
    if Assigned(FTemplate) then begin
      SaveReadOnly := FReadOnly;
      FReadOnly := False;
      try
        AllocContents(0);
      finally
        FReadOnly := SaveReadOnly;
      end;
    end;
    C := Contents^.ItemCount;
    if C > 0 then
      for Id := 0 to C - 1 do
        Contents^.Items[Id].Clear;
  end else if ReadOnly and not Constructed then begin
    FLength := 0;
    Cnt := nil;
    FReadOnlyCntSlave := nil;
    FReadOnlyCntLenSlave := 0;
    ReallocMem(FReadOnlyCnt,0);
  end else if not Constructed then begin
    FillChar(Cnt^,Length,0);
    FLength := 0;
    P := Cnt;
    Cnt := nil;
    FreeMem(P);
  end;
end;

function TASN1Struct.Content: PChar;
begin
  Assert(not Constructed);
  Result := Cnt;
end;

procedure TASN1Struct.ContentAsASN1Struct(var Struct: TASN1Struct);
var
  MS: TSecureMemoryStream;
begin
  if Assigned(Struct) and
     (Struct.FReadOnlyCntLenSlave = Length) and
     CompareMem(Struct.FReadOnlyCntSlave,Cnt,Length) then
    Exit
  else if Assigned(Struct) and (Struct.FOwner = nil) and
     (Struct <> FEncapsulated) and not Struct.Persistent then begin
    if Struct.FRefCount <= 0 then
      Struct.Free
    else
      Struct._Release;
    Struct := nil;
  end else if Assigned(Struct) then begin
    Struct.Clear;
    Struct.ReadOnly := True;
  end;
  MS := TSecureMemoryStream.Create;
  try
    if Constructed then
      SaveToStream(MS,fmtDER)
    else if ActualTag = V_ASN1_BIT_STRING then begin
      if Length > 0 then
        MS.Write(Content[1],Length - 1);
    end else
      MS.Write(Cnt^,Length);
    if Assigned(FEncapsulated) and (Struct <> FEncapsulated) then begin
      if Struct = nil then
        Struct := TASN1Struct.Create;
      Struct.CopyTypeInfo(FEncapsulated);
    end;
    MS.Position := 0;
    if MS.Size > 0 then
      ASN1FromStream(MS,Struct);
  finally
    MS.Free;
  end;
end;

function TASN1Struct.ContentAsBoolean: Boolean;
begin
  if ((Tag = V_ASN1_BOOLEAN) or
      (Cls <> V_ASN1_UNIVERSAL)) and
     (Length = 1) then
    Result := Content[0] = #$FF
  else
    raise Exception.Create('Could not interpret contents');
end;

function TASN1Struct.ContentAsCardinal: Cardinal;
var
  Str: string;
  X: LongWord;
begin
  if ((Tag in [V_ASN1_INTEGER,V_ASN1_ENUMERATED]) or
      (Cls <> V_ASN1_UNIVERSAL)) and
     (Length > 0) and 
     (Length <= SizeOf(Cardinal)) and
     not Constructed then begin
    if Byte(Content[0]) and $80 = 0 then
      Str := StringOfChar(#0,SizeOf(Cardinal))
    else
      raise Exception.Create('Could not interpret contents');
    Move(Content[0],Str[1 + SizeOf(Cardinal) - Length],Length);
    Move(Str[1],X,SizeOf(LongWord));
    Result := ByteSwap(X);
  end else
    raise Exception.Create('Could not interpret contents');
end;

function TASN1Struct.ContentAsDateTime: TDateTime;
begin
  if ((Tag = V_ASN1_UTCTIME) or
      (Cls <> V_ASN1_UNIVERSAL)) and
     not Constructed then begin
    if not UTCTimeToDateTime(Content,Length,Result) then
      if not GeneralizedTimeToDateTime(Content,Length,Result) then
        raise Exception.Create('Could not interpret contents');
  end else if ((Tag = V_ASN1_GENERALIZEDTIME) or
               (Cls <> V_ASN1_UNIVERSAL)) and
              not Constructed then begin
    if not GeneralizedTimeToDateTime(Content,Length,Result) then
      raise Exception.Create('Could not interpret contents');
  end else
    raise Exception.Create('Could not interpret contents');
end;

function TASN1Struct.ContentAsInt64: Int64;
var
  Str: string;
  X: Int64;
begin
  if ((Tag in [V_ASN1_INTEGER,V_ASN1_ENUMERATED]) or
      (Cls <> V_ASN1_UNIVERSAL)) and
     (Length > 0) and
     (Length <= SizeOf(Int64)) and
     not Constructed then begin
    if Byte(Content[0]) and $80 = 0 then
      Str := StringOfChar(#0,SizeOf(Int64))
    else
      Str := StringOfChar(#$FF,SizeOf(Int64));
    Move(Content[0],Str[1 + SizeOf(Int64) - Length],Length);
    Move(Str[1],X,SizeOf(Int64));
    Result := ByteSwap(X);
  end else
    raise Exception.Create('Could not interpret contents');
end;

function TASN1Struct.ContentAsInteger: Integer;
var
  Str: string;
  X: LongWord;
begin
  if ((Tag in [V_ASN1_INTEGER,V_ASN1_ENUMERATED]) or
      (Cls <> V_ASN1_UNIVERSAL)) and
     (Length > 0) and
     (Length <= SizeOf(Integer)) and
     not Constructed then begin
    if Byte(Content[0]) and $80 = 0 then
      Str := StringOfChar(#0,SizeOf(Integer))
    else
      Str := StringOfChar(#$FF,SizeOf(Integer));
    Move(Content[0],Str[1 + SizeOf(Integer) - Length],Length);
    Move(Str[1],X,SizeOf(LongWord));
    Result := Integer(ByteSwap(X));
  end else
    raise Exception.Create('Could not interpret contents');
end;

procedure TASN1Struct.ContentAsMPInt(var X: PMPInteger);
var
  vTag: Cardinal;
  U: Byte;
begin
  Assert(not Constructed);
  vTag := ActualTag;
  if vTag = V_ASN1_BIT_STRING then begin
    if Length > 1 then begin
      Base256ToMPInt(X,Content[1],Length-1,0);
      U := Byte(Content[0]);
      if U > 0 then
        MPShr(X,U);
    end else begin
      MPDealloc(X);
      X := nil;
    end;
  end else if Length = 0 then begin
    MPDealloc(X);
    X := nil;
  end else
    Base256ToMPInt(X,Cnt^,Length,0);
end;

procedure TASN1Struct.ContentAsOctetString(var OS: ISecretKey);
var
  MS: TSecureMemoryStream;
begin
  if OS = nil then
    OS := TSecretKey.Create('');
  if Constructed then begin
    MS := TSecureMemoryStream.Create;
    try
      ASN1ToStream(Self,MS);
      OS.SetKey(MS.Memory,MS.Size,0);
    finally
      MS.Free;
    end;
  end else if ActualTag = V_ASN1_BIT_STRING then begin
    if Assigned(Cnt) and (Length > 1) then
      OS.SetKey(@Content[1],Length-1,0)
    else
      OS.SetKey(nil,0,0);
  end else
    OS.SetKey(Cnt,Length,0);
end;

function TASN1Struct.ContentAsOctetString: string;
var
  SS: TStringStream;
begin
  if Constructed then begin
    SS := TStringStream.Create('');
    try
      ASN1ToStream(Self,SS);
      Result := SS.DataString;
    finally
      SS.Free;
    end;
  end else if ActualTag = V_ASN1_BIT_STRING then begin
    if Assigned(Cnt) and (Length > 1) then begin
      System.SetLength(Result,Length-1);
      Move(Content[1],Pointer(Result)^,Length-1);
    end else
      Result := '';
  end else begin
    System.SetLength(Result,Length);
    Move(Cnt^,Pointer(Result)^,Length);
  end;
end;

function TASN1Struct.ContentAsOID: string;
begin
  if Constructed and (ItemCount = 1) then
    Result := Items[0].ContentAsOID
  else begin
    Assert(not Constructed);
    if Length = 0 then
      Result := ''
    else
      Result := InterpretBERtoOID(Content,Length);
  end;
end;

function TASN1Struct.ContentAsReal: Extended;
var
  S: string;
begin
  Assert(not Constructed);
  Assert(Length > 0);
  System.SetLength(S,Length);
  Move(Cnt^,S[1],Length);
  Result := InterpretBERToReal(S);
end;

function TASN1Struct.ContentAsString: WideString;
var
  S: string;
begin
  if Constructed and (ItemCount = 1) then
    // IMPLICIT /EXPLICIT Confusion
    Result := Items[0]^.ContentAsString
  else begin
    Assert(not Constructed);
    if ActualTag = V_ASN1_BMPSTRING then
      Result := BMPToUnicode(Content,Length)
    else if ActualTag = V_ASN1_PRINTABLESTRING then begin
      System.SetLength(S,Length);
      Move(Cnt^,Pointer(S)^,Length);
      Result := TrimPrintable(S)
    end else if ActualTag = V_ASN1_UTF8STRING then begin
      System.SetLength(S,Length);
      Move(Cnt^,Pointer(S)^,Length);
      Result := Utf8ToUnicode(S)
    end else begin
      System.SetLength(S,Length);
      Move(Cnt^,Pointer(S)^,Length);
      Result := S;
    end;
  end;
end;

function TASN1Struct.ContentAsUInt64: Int64;
var
  Str: string;
  X: Int64;
begin
  if ((Tag = V_ASN1_INTEGER) or
      (Cls <> V_ASN1_UNIVERSAL)) and
     (Length > 0) and
     (Length <= SizeOf(Int64)) and
     not Constructed then begin
    Str := StringOfChar(#0,SizeOf(Int64));
    Move(Content[0],Str[1 + SizeOf(Int64) - Length],Length);
    Move(Str[1],X,SizeOf(Int64));
    Result := ByteSwap(X);
  end else
    raise Exception.Create('Could not interpret contents');
end;

procedure TASN1Struct.ContentAsUMPInt(var X: PMPInteger);
var
  vTag: Cardinal;
  U: Byte;
begin
  Assert(not Constructed);
  if Implicit then
    vTag := ImplicitTag
  else
    vTag := Tag;
  if vTag = V_ASN1_BIT_STRING then begin
    Base256ToUMPInt(X,Content[1],Length-1,0);
    U := Byte(Content[0]);
    if U > 0 then
      MPShr(X,U);
  end else
    Base256ToUMPInt(X,Cnt^,Length,0);
end;

function TASN1Struct.Contents: PASN1Structs;
begin
  Assert(Constructed);
  Result := Cnt;
end;

procedure TASN1Struct.CopyTypeInfo(const Source: TASN1Struct; Flat: Boolean);
var
  ID, OldCount: Integer;
  SS: TStringStream;
  SaveReadOnly, IsChoice: Boolean;
  F: PASN1Struct;
begin
  Assert(Assigned(Source),'CopyTypeInfo: Source is not assigned');
  if Source = Self then Exit;
  BeginUpdate;
  try                                                 
    try
      if FConstructed xor Source.FConstructed then begin
        DisposeContent;
        FConstructed := Source.FConstructed;
      end;
    except
      on E: Exception do
        raise Exception.Create('CopyTypeInfo: Failure disposing content:'#13#10 + E.Message);
    end;
    try
      if FTemplate <> Source.FTemplate then begin
        if Assigned(FTemplate) then
          FTemplate._Release;
        if Assigned(Source.FTemplate) and
           (Source.FTemplate.ChoiceCount > 0) then begin
          FTemplate := TASN1Struct.Create;
          FTemplate.CopyTypeInfo(Source.FTemplate);
        end else
          FTemplate := Source.FTemplate;
        if Assigned(FTemplate) and not IsParent(FTemplate) then
          FTemplate._AddRef;
      end;
    except
      on E: Exception do
        raise Exception.Create('CopyTypeInfo: Failure copying template:'#13#10 + E.Message);
    end;

    try
      if Assigned(FEncapsulated) then FEncapsulated._Release;
      if Assigned(Source.FEncapsulated) then begin
        FEncapsulated := TASN1Struct.Create;
        FEncapsulated._AddRef;
        FEncapsulated.CopyTypeInfo(Source.FEncapsulated);
      end else
        FEncapsulated := nil;
    except
      on E: Exception do
        raise Exception.Create('CopyTypeInfo: Failure copying encapsulated:'#13#10 + E.Message);
    end;

    try
      FIdentifies.Free;
      if Assigned(Source.FIdentifies) then begin
        FIdentifies := TStringList.Create;
        FIdentifies.Assign(Source.FIdentifies);
      end else
        FIdentifies := nil;
    except
      on E:Exception do
        raise Exception.Create('Failure copying FIdentifies:'#13#10 + E.Message);
    end;

    IsChoice := IsChoiceTemplate(Source);
    if not IsChoice then begin

      try
        if Constructed and Source.Constructed and
           ((FTypeName <> Source.FTypeName) or
            (FModuleName <> Source.FModuleName)) then
          AllocContents(0);
      except
        on E:Exception do
          raise Exception.Create(Format('Failure calling AllocContents(0), Old typename %s.%s:'#13#10'%s',
                                        [FModuleName,FTypeName,E.Message]));
      end;

      FChoiceIndex := Source.FChoiceIndex;

      try
        for Id := 0 to ChoiceCount - 1 do
          FChoices^.Items[Id]._Release;
        if (FChoices = nil) or (ChoiceCount <> Source.ChoiceCount) then
          ReallocMem(FChoices,SizeOf(Integer) + SizeOf(TASN1Struct)*Source.ChoiceCount);
        for Id := 0 to Source.ChoiceCount - 1 do begin
          F := Source.Choices[Id];
          if F^.ChoiceCount > 0 then begin
            FChoices^.Items[Id] := TASN1Struct.Create;
            FChoices^.Items[Id].CopyTypeInfo(F^);
          end else
            FChoices^.Items[Id] := F^;
          FChoices^.Items[Id]._AddRef;
        end;
        FChoices^.ItemCount := Source.ChoiceCount;
      except
        on E:Exception do
          raise Exception.Create('Failure copying choices:'#13#10 + E.Message);
      end;

      try
        FChoiceTypeName := Source.ChoiceTypeName;
        FChoiceVarName := Source.ChoiceVarName;
        FBERIdentifiedBy := Source.FBERIdentifiedBy;
        FTypeIdentified := Source.TypeIdentified;
      except
        on E:Exception do
          raise Exception.Create('Failure copying choice related fields:'#13#10 + E.Message);
      end;
      try
        if Source.FIDField <> '' then begin
          if FOwner = nil then
            FIDField := Source.IDField
          else
            IDField := Source.IDField
        end else
          FIDField := '';
      except
        on E:Exception do
          raise Exception.Create('Failure setting IDField:'#13#10 + E.Message);
      end;
      if (FModuleName = '') or (FModuleName = Source.FModuleName) then
        FStrictTagging := Source.FStrictTagging;
      FModuleName := Source.FModuleName;
      FOptional := Source.FOptional;
      Default := Source.Default;
      FHasDefaultValue := Source.FHasDefaultValue;
      if FHasDefaultValue then begin
        SS := TStringStream.Create(Default);
        try
          ResetStreaming;
          try
            ASN1FromStream(SS,Self);
          except
            on E:Exception do
              raise Exception.Create('Failure streaming default value:'#13#10 + E.Message);
          end;
          ResetStreaming;
        finally
          SS.Free;
        end;
      end;
    end else if Source.Implicit or not Implicit then begin
      FImplicit := Source.FImplicit;
      FTag := Source.FTag;
      FCls := Source.FCls;
    end;

    try
      FVarName := Source.FVarName;
      FAliasTypeName := Source.FAliasTypeName;
      if not IsChoice then begin
        if Source.FTypeName <> '' then
          FTypeName := Source.FTypeName;
        FCls := Source.FCls;
        FTag := Source.FTag;
        FImplicit := Source.FImplicit;
        FImplicitTag := Source.FImplicitTag;
        FImplicitTypeName := Source.FImplicitTypeName;
      end else if Implicit then begin
        if Source.Implicit then begin
          FImplicitTag := Source.FImplicitTag;
          if Source.FTypeName <> '' then
            FImplicitTypeName := Source.FImplicitTypeName;
        end else begin
          FImplicitTag := Source.FTag;
          if Source.FTypeName <> '' then
            FImplicitTypeName := Source.FTypeName;
        end;
      end else begin
        FCls := Source.FCls;
        FTag := Source.FTag;
        if Source.FTypeName <> '' then
          FTypeName := Source.FTypeName;
      end;
      FPersistent := True;
    except
      raise Exception.Create('Failure copying fields');
    end;
    if Constructed and (Source.ChoiceCount = 0) and not Flat then begin
      SaveReadOnly := FReadOnly;
      FReadOnly := False;
      try
        if Assigned(FTemplate) or (Source.ItemCount <= 0) then
          AllocContents(0)
        else begin
          OldCount := ItemCount;
          try
            AllocContents(Source.ItemCount);
          except
            raise Exception.Create(Format('Could not allocate %d items',[Source.ItemCount]));
          end;
          try
            for Id := 0 to Source.ItemCount - 1 do begin
              F := Items[Id];
              if Id >= OldCount then F^.FReadOnly := SaveReadOnly;
              F^.CopyTypeInfo(Source.Items[Id]^);
            end;
          except
            on E: Exception do
              raise Exception.Create('Failure copying item type info:'#13#10 + E.Message);
          end;
        end;
      finally
        FReadOnly := SaveReadOnly;
      end;
    end;
    try
      Change(ctDecl);
    except
      raise Exception.Create('Failure calling Change(ctDecl)');
    end;
  finally
    EndUpdate;
  end;
end;              

procedure TASN1Struct.CreateEncapsulated;
begin
  if Assigned(FEncapsulated) then Exit;
  FEncapsulated := TASN1Struct.Create;
  FEncapsulated.FStrictTagging := FStrictTagging;
  FEncapsulated.FForgetOptionalDefault := FForgetOptionalDefault;
  FEncapsulated._AddRef;
  Change(ctDecl);
end;

procedure TASN1Struct.CreateOFTemplate;
begin
  if Assigned(FTemplate) then Exit;
  FTemplate := TASN1Struct.Create;
  FTemplate.FStrictTagging := FStrictTagging;
  FTemplate.FForgetOptionalDefault := FForgetOptionalDefault;
  FTemplate._AddRef;
  Change(ctDecl);
end;

destructor TASN1Struct.Destroy;
begin
  Dispose(nil);
  inherited;
end;

function TASN1Struct.DisplayContent: WideString;
var
  S: string;
  D: TDateTime;
  vTag: Cardinal;
begin
  if Constructed and (FTypeName <> '') and (FTypeName[1] = '[') and not Implicit then begin
    Result := Items[0].DisplayContent;
    Exit;
  end;
  Assert(not Constructed);
  System.SetLength(S,Length);
  Move(Content^,S[1],Length);
  if System.Length(S) > 0 then begin
    if (Cls = V_ASN1_UNIVERSAL) or Implicit then begin
      if Implicit then
        vTag := ImplicitTag
      else
        vTag := Tag;
      if vTag in [V_ASN1_INTEGER,V_ASN1_OCTET_STRING,V_ASN1_ENUMERATED] then
        Result := OSToHex(S)
      else if vTag = V_ASN1_BIT_STRING then
        Result := Format('(%d unused bits) ',[Byte(S[1])]) + OSToHex(Copy(S,2,MaxInt))
      else if vTag = V_ASN1_BOOLEAN then begin
        if S = #$FF then
          Result := 'True'
        else if S = #0 then
          Result := 'False'
        else
          raise Exception.Create('Could not interpret contents');
      end else if vTag = V_ASN1_BMPSTRING then
        Result := BMPtoUnicode(PChar(S),Length)
      else if vTag = V_ASN1_UTF8STRING then
        Result := UTF8toUnicode(S)
      else if vTag in [V_ASN1_PRINTABLESTRING,
                       V_ASN1_VISIBLESTRING,
                       V_ASN1_NUMERICSTRING,
                       V_ASN1_IA5STRING,
                       V_ASN1_TELETEXSTRING,
                       V_ASN1_VIDEOTEXSTRING,
                       V_ASN1_GENERALSTRING,
                       V_ASN1_UNIVERSALSTRING] then
        Result := S
      else if vTag = V_ASN1_OBJECT then
        Result := GetObjectName(InterpretBERtoOID(S))
      else if vTag = V_ASN1_UTCTIME then begin
        if UTCTimeToDateTime(Pchar(S),Length,D) then
          Result := FormatDateTime('yyyy-mm-dd hh:mm:ss',D)
      end else if vTag = V_ASN1_GENERALIZEDTIME then begin
        if GeneralizedTimeToDateTime(Pchar(S),Length,D) then
          Result := FormatDateTime('yyyy-mm-dd hh:mm:ss:zzz',D)
      end else if vTag = V_ASN1_REAL then
        Result := FloatToStr(InterpretBERToReal(S))
      else
        Result := OSToHex(ContentAsOctetString);
    end else
      Result := OSToHex(ContentAsOctetString);
  end;
end;

procedure TASN1Struct.DisposeContent;
var
  C, Id: Integer;
begin
  if Constructed and Assigned(Cnt) then begin
    C := Contents^.ItemCount;
    for Id := C - 1 downto 0 do begin
      if Id < Contents^.ItemCount then
        ReleaseASN1Struct(Contents^.Items[Id],Self,Garbage);
    end;
    Contents^.ItemCount := 0;
    FLength := 0;
  end else if (Assigned(Cnt) or (FLength <> 0)) and not Constructed then begin
    if FReadOnly then begin
      Cnt := nil;
      FLength := 0;
    end else
      SetLength(0);
  end;
//  if not FDestroying then
  Change(ctCnt);
  if Constructed then
    if Assigned(Cnt) then
      ReallocMem(Cnt,0);
  if Assigned(FReadOnlyCnt) then begin
    ProtectClear(FReadOnlyCnt^,FReadOnlyCntLen);
    ReallocMem(FReadOnlyCnt,0);
  end;                                          
  FReadOnlyCntLen := 0;
  FCapacity := 0;
end;

procedure TASN1Struct.EditContent(const AValue: WideString);
var
  Str: string;
  SS: TStringStream;
  vTag: Cardinal;
  Id: Integer;
  F: PASN1Struct;
begin
    if System.Length(AValue) > 0 then begin
      if Constructed and (FTypeName <> '') and (FTypeName[1] = '[') and not Implicit then
        Items[0].EditContent(AValue)
      else if Constructed then begin
        Str := HexToOS(AValue);
        SS := TStringStream.Create(Str);
        try
          ASN1FromStream(SS,Self);
        finally
          SS.Free;
        end;
      end else if (Cls = V_ASN1_UNIVERSAL) or Implicit then begin
        if Implicit then
          vTag := ImplicitTag
        else
          vTag := Tag;
        if vTag in [V_ASN1_INTEGER,V_ASN1_OCTET_STRING] then
          Str := HexToOS(AValue)
        else if vTag = V_ASN1_BIT_STRING then
          Str := #0 + HexToOS(AValue)
        else if vTag = V_ASN1_BOOLEAN then begin
          if SameText(AValue,'true') then
            Str := #$FF
          else if SameText(AValue,'false') then
            Str := #0
          else
            raise Exception.Create('Could not interpret contents');
        end else if vTag = V_ASN1_BMPSTRING then
          Str := UnicodeToBMP(AValue)
        else if vTag = V_ASN1_UTF8STRING then
          Str := UnicodeToUTF8(AValue)
        else if vTag in [V_ASN1_PRINTABLESTRING,
                         V_ASN1_VISIBLESTRING,
                         V_ASN1_NUMERICSTRING,
                         V_ASN1_IA5STRING,
                         V_ASN1_TELETEXSTRING,
                         V_ASN1_VIDEOTEXSTRING,
                         V_ASN1_GENERALSTRING,
                         V_ASN1_UNIVERSALSTRING] then
          Str := AValue
        else if vTag = V_ASN1_OBJECT then begin
          Str := GetRegistredOID(AValue);
          if Str = '' then
            Str := AValue;
          Str := InterpretOIDToBER(Str);
          if Assigned(FIdentifies) and (FIdentifies.Count > 0) then begin
            SetContent(Str[1],System.Length(Str));
            for Id := 0 to FIdentifies.Count - 1 do begin
              F := Owner.FindField(FIdentifies[Id]);
              if Assigned(F) then
                F^.TypeIdentify;
            end;
          end;
        end else if vTag = V_ASN1_UTCTIME then
          Str := DateTimeToUTCTime(StrToDateTime(AValue))
        else if vTag = V_ASN1_GENERALIZEDTIME then
          Str := DateTimeToGeneralizedTime(StrToDateTime(AValue))
        else if vTag = V_ASN1_REAL then
          Str := InterpretRealToBER(StrToFloat(AValue))
        else
          raise Exception.Create('Could not interpret contents');
        SetContent(Str[1],System.Length(Str));
      end else
        raise Exception.Create('Could not interpret contents');
    end;
end;

procedure TASN1Struct.EditContent(const AValue: Cardinal);    
var
  X: Int64;
begin
  if ((Tag in [V_ASN1_INTEGER,V_ASN1_ENUMERATED]) or
      (Implicit and (ImplicitTag in [V_ASN1_INTEGER,V_ASN1_ENUMERATED]))) and
     not Constructed then begin
    X := AValue;
    EditContent(X,False);
  end else
    raise Exception.Create('Could not interpret contents');
end;

procedure TASN1Struct.EditContent(const AValue: Int64;
  StoreAsUnsigned: Boolean);        
var
  X: Int64;
  Str: string;
begin
  if ((Tag in [V_ASN1_INTEGER,V_ASN1_ENUMERATED]) or
      (Implicit and (ImplicitTag in [V_ASN1_INTEGER,V_ASN1_ENUMERATED]))) and
     not Constructed then begin
    X := AValue;
    X := ByteSwap(X);
    System.SetLength(Str,SizeOf(X));
    Move(X,Str[1],SizeOf(X));
    if StoreAsUnsigned then
      while (Str <> #0) and (Str[1] = #0) do
        Delete(Str,1,1)
    else
      while ((Str <> #0) and (Str[1] = #0) and (Byte(Str[2]) and $80 = 0)) or
            (((Str <> #$FF) and (Str[1] = #$FF) and (Byte(Str[2]) and $80 = $80))) do
        Delete(Str,1,1);
    SetContent(Str[1],System.Length(Str));
  end else
    raise Exception.Create('Could not interpret contents');
end;

procedure TASN1Struct.EditContent(const AValue: Boolean);
var
  B: Byte;
begin
  if ((Tag = V_ASN1_BOOLEAN) or
      (Implicit and (ImplicitTag = V_ASN1_BOOLEAN))) and
     not Constructed then begin
    if AValue then
      B := $FF
    else
      B := 0;
    SetContent(B,1);
  end else
    raise Exception.Create('Could not interpret contents');
end;

procedure TASN1Struct.EditContent(const AValue: Integer); 
var
  X: Int64;
begin
  if ((Tag in [V_ASN1_INTEGER,V_ASN1_ENUMERATED]) or
      (Implicit and (ImplicitTag in [V_ASN1_INTEGER,V_ASN1_ENUMERATED]))) and
     not Constructed then begin
    X := AValue;
    EditContent(X,False);
  end else
    raise Exception.Create('Could not interpret contents');
end;

procedure TASN1Struct.EditContent(const AValue: TDateTime);
var
  Str: string;
begin
  if (ActualTag = V_ASN1_UTCTIME) and not Constructed then begin
    Str := DateTimeToUTCTime(AValue);
    SetContent(Str[1],System.Length(Str));
  end else if (ActualTag = V_ASN1_GENERALIZEDTIME) and not Constructed then begin
    Str := DateTimeToGeneralizedTime(AValue);
    SetContent(Str[1],System.Length(Str));
  end else if (ActualTag = V_ASN1_REAL) and not Constructed then begin
    Str := InterpretRealToBER(AValue);
    SetContent(Str[1],System.Length(Str));
  end else
    raise Exception.Create(Format('Could not interpret contents: %d is not a Time type tag.',[Tag]));
end;

procedure TASN1Struct.DoAfterLoad;
begin
  if Assigned(FAfterLoad) then
    FAfterLoad(Self,Field)
  {$IFNDEF THIN_TASN1STRUCT}
  else if Assigned(FOwner) then
    FOwner.DoAfterLoad(Field);   
  {$ENDIF}
end;

procedure TASN1Struct.DoAfterSave;
begin
  if Assigned(FAfterSave) then
    FAfterSave(Self,Field)  
  {$IFNDEF THIN_TASN1STRUCT}
  else if Assigned(FOwner) then
    FOwner.DoAfterSave(Field);
  {$ENDIF}
end;

procedure TASN1Struct.DoBeforeLoad;
begin
  if Assigned(FBeforeLoad) then
    FBeforeLoad(Self,Field)  
  {$IFNDEF THIN_TASN1STRUCT}
  else if Assigned(FOwner) then
    FOwner.DoBeforeLoad(Field);
  {$ENDIF}
end;

procedure TASN1Struct.DoBeforeSave;
begin
  if Assigned(FBeforeSave) then
    FBeforeSave(Self,Field)        
  {$IFNDEF THIN_TASN1STRUCT}
  else if Assigned(FOwner) then
    FOwner.DoBeforeSave(Field);
  {$ENDIF}
end;

function TASN1Struct.DoCaptureLoad(Field: TASN1Struct;
                                   Stream: TStream;
                                   var Done: Boolean): Integer;
begin
  if Assigned(FOnCaptureLoad) then begin
    Result := 0;
    FOnCaptureLoad(Self,Field,Stream,Field.Length,Result,Done)
  end 
  {$IFNDEF THIN_TASN1STRUCT}else if Assigned(FOwner) then
    Result := FOwner.DoCaptureLoad(Field,Stream,Done);
  {$ENDIF}
end;

function TASN1Struct.DoCaptureSave(Field: TASN1Struct;
                                   Stream: TStream;
                                   var Done: Boolean): Integer;
begin
  if Assigned(FOnCaptureSave) then begin
    Result := 0;
    FOnCaptureSave(Self,Field,Stream,Field.Length,Result,Done)
  end 
  {$IFNDEF THIN_TASN1STRUCT}else if Assigned(FOwner) then
    Result := FOwner.DoCaptureSave(Field,Stream,Done);
  {$ENDIF}
end;

procedure TASN1Struct.EditContent(const AValue: Extended);
var
  Str: string;
begin
  if (ActualTag = V_ASN1_UTCTIME) and not Constructed then begin
    Str := DateTimeToUTCTime(AValue);
    SetContent(Str[1],System.Length(Str));
  end else if (ActualTag = V_ASN1_GENERALIZEDTIME) and not Constructed then begin
    Str := DateTimeToGeneralizedTime(AValue);
    SetContent(Str[1],System.Length(Str));
  end else if (ActualTag = V_ASN1_REAL) and not Constructed then begin
    Str := InterpretRealToBER(AValue);
    SetContent(Str[1],System.Length(Str));
  end else
    raise Exception.Create('Could not interpret contents');
end;

procedure TASN1Struct.EditContent(const AValue: Int64);
begin
  EditContent(AValue,False);
end;

function TASN1Struct.FindField(AVarName: string): PASN1Struct;
var
  IdxStr: string;
  Idx, P, Code: Integer;
begin
   Result := nil;
   if (AVarName = '') or (Cnt = nil) or not Constructed then
     Result := nil
   else begin
     if AVarName[1] = '/' then begin
       if Pos('//',AVarName) = 1 then begin
         Delete(AVarName,1,1);
         Result := Items[0];
         if AVarName = '' then Exit;
       end else if (Pos('/[',AVarName) = 1) and
               (Pos(']',AVarName) > 3) then begin
         P := Pos(']',AVarName);
         IdxStr := Copy(AVarName,3,P-3);
         Delete(AVarName,1,P);
         Val(IdxStr,Idx,Code);
         if Code <> 0 then begin
           if Assigned(FTemplate) and (IdxStr[1] in ['c','C']) then begin
             Result := CurrentRecord;
             if Result = nil then Exit;
           end else begin
             Result := nil;
             Exit;
           end;
         end else
           Result := Items[Idx];
         if AVarName = '' then Exit;
       end else begin
         Idx := 0;
         while Idx < ItemCount do begin
           Result := Items[Idx];
           if (Result^.VarName = '') and
              (AVarName = '/') then
             Exit
           else if (Pos(Result^.VarName,AVarName) = 2) and
                   (PChar(AVarName)[1 + System.Length(Result^.VarName)] in [#0,'/']) then begin
             Delete(AVarName,1,1);
             P := Pos('/',AVarName);
             if P > 0 then begin
               Delete(AVarName,1,P-1);
               Break;
             end else
               Exit;
           end else
             Result := nil;
           Inc(Idx);
         end;
       end;
       if Assigned(Result) then
         Result := Result^.FindField(AVarName);
     end else begin
       for Idx := 0 to ItemCount - 1 do begin
         Result := Items[Idx];
         if SameText(Result^.VarName,AVarName) then Exit;
       end;
       Result := nil;
     end;
   end;
end;

function TASN1Struct.GetItemCount: Integer;
begin
  if (Cnt = nil) or not Constructed then
    Result := -1
  else
    Result := PASN1Structs(Cnt)^.ItemCount;
end;

function TASN1Struct.GetItems(index: Integer): PASN1Struct;
begin
  Assert((index >= 0) and (index < ItemCount),
         Format('Index %d out of range (%d)'#13#10'TypeName: %s',
                [index,ItemCount,TypeName]));
  Result := @PASN1Structs(Cnt)^.Items[index];
end;

function TASN1Struct.GetTemplate: TASN1Struct;
begin
  Result := FTemplate;
end;

function TASN1Struct.LoadFromStream(AStream: TStream;
  Format: Byte): Integer;
var
  SL,DT,DI: TStringList;
  I, P: Integer;
  RegTmpl: TASN1Struct;

  procedure RegisterOIDs;
  var
    I, J, K: Integer;
    Decl: string;
    N,O: string;
  begin
    for I := 0 to SL.Count - 1 do begin
      Decl := SL[I];
      if Decl = 'END' then Break;
      J := Pos(' ::= ',Decl);
      if J > 0 then begin
        K := Pos(' OBJECT ',Decl);
        if K > 0 then begin
          N := '';
          O := '';
          RegisterOID(O,N,Decl);
        end;
      end;
    end;
  end;

begin
  BeginUpdate;
  try
    LockInstanceList;
    try
      Result := -1;
      if Format and 2 > 0 then begin
        DisposeContent;
        ChoiceCount := 0;
        if Assigned(FTemplate) then begin
          FTemplate._Release;
          FTemplate := nil;
        end;
        Result := AStream.Size;
        SL := TStringList.Create;
        try
          SL.LoadFromStream(AStream);
          RegisterOIDs;
          FTypeName := '';
          DI := TStringList.Create;
          try
            TStringList(DI).Sorted := True;
            TStringList(DI).Duplicates := dupIgnore;
            for I := 0 to SL.Count - 1 do begin
              P := Pos('::=',SL[I]);
              if P > 0 then
                DI.AddObject(Trim(Copy(SL[I],1,P-1)),TObject(I));
            end;
            DT := TStringList.Create;
            try
              ExplicitImport(SL,DT);
              RegTmpl := TASN1Struct.Create;
              RegTmpl._AddRef;
              {$IFDEF NEVER}
              RegTmpl.FTypeName := '';
              RegTmpl.NoChangeEvent := True;
              RegTmpl.LoadFromStrings(SL,DT,DI);
              RegTmpl.FModuleName := ExtractModuleName(SL);
              CopyTypeInfo(RegTmpl);
              {$ELSE}
              LoadFromStrings(SL,DT,DI);
              FModuleName := ExtractModuleName(SL);
              RegTmpl.CopyTypeInfo(Self);
              {$ENDIF}
              if Implicit then
                DT.InsertObject(0,ImplicitTypeName,RegTmpl)
              else
                DT.InsertObject(0,TypeName,RegTmpl);
              RegisterModule(FModuleName,DT);
              RegTmpl._Release;
            finally
              DT.Free;
            end;
          finally
            DI.Free;
          end;
        finally
          SL.Free;
        end;
      end else if Format and 1 > 0 then begin
        if AllowResume then begin
          if FResumeStream = nil then begin
            ResetStreaming;
            FResumeStream := TReadResumeStream.Create;
          end;
          FResumeStream.DataStream := AStream;
          FResumeStream.Position := 0;
          Result := ASN1FromStream(FResumeStream,Self);
          if DoneLoading then begin
            FResumeStream.Free;
            FResumeStream := nil;
            ResetStreaming;
            DoneLoading := True;
          end;
        end else
          Result := ASN1FromStream(AStream,Self);
      end;
    finally
      UnlockInstanceList;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TASN1Struct.LoadFromStrings(AStrings,
                                      DeclaredTypes,
                                      DeclIndex: TStrings);
var
  S, OID, DeclTN: string;
  F: PASN1Struct;
  T: TASN1Struct;
  P, P0, Idx, Id, ParCount, L: Integer;
  SS: TStringStream;
  TopItem, Choice, TypeID, Impl: Boolean;
  vTypeName, vAlias: string;
  vCls: Byte;
  vTag: Cardinal;
  Str: PChar;
  Delim: Char;
  IC: Integer;

  procedure LoadField(F: PASN1Struct);
  begin
    if F^.Constructed or F^.TypeIdentified then
      F^.LoadFromStrings(AStrings,DeclaredTypes,DeclIndex);
  end;

begin
  IC := InstanceCount;

  DoLoadStructure(Self);

  TopItem := FTypeName = '';
  if TopItem then begin
    if AStrings.Count < 4 then
      raise Exception.Create('Illegal format');
    Idx := AStrings.IndexOf('BEGIN');
    if Idx < 1 then
      raise Exception.Create('Illegal format');
  end else begin
    if Assigned(DeclaredTypes) then begin
      if FImplicitTypeName <> '' then
        Id := DeclaredTypes.IndexOf(FImplicitTypeName)
      else
        Id := DeclaredTypes.IndexOf(FTypeName);
      if Id >= 0 then begin
        T := TASN1Struct(DeclaredTypes.Objects[Id]);
        CopyTypeInfoAsField(T);

        Exit;
      end else
        DeclaredTypes.AddObject(FTypeName,Self);
    end;

    Idx := 0;
  end;
  P := 0;
  if Assigned(DeclIndex) and not TopItem then begin
    if Implicit then
      Idx := DeclIndex.IndexOf(FImplicitTypeName)
    else
      Idx := DeclIndex.IndexOf(FTypeName);
    if Idx >= 0 then begin
      Idx := Integer(DeclIndex.Objects[Idx]) + 1;
      S := AStrings[Idx-1];
      P := Pos(' ::=',S);
    end;
  end else
    while (Idx < AStrings.Count) and (P = 0) do begin
      S := AStrings[Idx];
      P := Pos(' ::= ',S);
      if Implicit then begin
        if (P > 0) and (FImplicitTypeName <> '') then
          if Pos(FImplicitTypeName + ' ::=',S) <> 1 then
            P := 0
      end else
        if (P > 0) and (FTypeName <> '') then
          if Pos(FTypeName + ' ::=',S) <> 1 then
            P := 0;
      Inc(Idx);
    end;
  Choice := False;
  TypeID := False;
  if P > 0 then begin
    if Implicit and (FImplicitTypeName = '') then
      ImplicitTypeName := Trim(Copy(S,1,P))
    else if (FTypeName = '') and not Implicit then
      TypeName := Trim(Copy(S,1,P));
    Delete(S,1,P+4);
    if Tag = Cardinal(V_ASN1_UNDEF) then
      Constructed := True;
    Persistent := True;
    if Constructed then begin
      if Implicit then begin
        if StrLIComp('sequence',PChar(S),8) = 0 then
          ImplicitTag := V_ASN1_SEQUENCE
        else if StrLIComp('set',PChar(S),3) = 0 then
          ImplicitTag := V_ASN1_SET
        else if StrLIComp('choice',PChar(S),6) = 0 then
          Choice := True
        else if StrLIComp('type-identifier',PChar(S),15) = 0 then
          TypeID := True
        else begin
          ImplicitTypeName := S;
          if Pos(' encapsulates ',LowerCase(S)) = 0 then
            Exit;
        end;
      end else
        if S[1] in ['s','S'] then begin
          if StrLIComp('sequence',PChar(S),8) = 0 then
            Tag := V_ASN1_SEQUENCE
          else if StrLIComp('set',PChar(S),3) = 0 then
            Tag := V_ASN1_SET
        end else if (S[1] in ['c','C']) and
                    (StrLIComp('choice',PChar(S),6) = 0) then
          Choice := True
        else if (S[1] in ['t','T']) and
                (StrLIComp('type-identifier',PChar(S),15) = 0) then
          TypeID := True
        else begin
          DeclTN := TypeName;
          TypeName := S;
          if Tag = Cardinal(V_ASN1_UNDEF) then begin
            F := @Self;
            FConstructed := True;
            vAlias := S;
            LoadField(F);
            Persistent := True;
            FAliasTypeName := vAlias;
            FTypeName := DeclTN;
            Exit;
          end else begin
            Persistent := True;
            TypeName := DeclTN;
            if Pos(' encapsulates ',LowerCase(S)) = 0 then
              Exit;
          end;
        end;
    end;
    P := Pos('{',S);
    if P > 0 then begin
      ParCount := 1;
      Inc(P);
      while P <= System.Length(S) do begin
        if S[P] = '{' then
          Inc(ParCount)
        else if S[P] = '}' then
          Dec(ParCount);
        Inc(P);
      end;
    end else
      ParCount := 0;
    if ParCount = 0 then begin
      P := Pos(' of ',LowerCase(S));
      if P > 0 then begin
        CreateOFTemplate;
        FTemplate.TypeName := Copy(S,P + 4,MaxInt);
        if FTemplate.Constructed then
          FTemplate.LoadFromStrings(AStrings,DeclaredTypes,DeclIndex);
        Exit;
      end;
      P := Pos(' encapsulates ',LowerCase(S));
      if P > 0 then begin
        CreateEncapsulated;
        FEncapsulated.TypeName := Copy(S,P + 14,MaxInt);
        if FEncapsulated.Constructed then
          FEncapsulated.LoadFromStrings(AStrings,DeclaredTypes,DeclIndex);
        Exit;
      end;
    end;
  end else begin
    ImplicitImport(FTypeName,Self);
    Exit;
  end;
  if Constructed or Choice or TypeID then begin
    if Constructed and not (Choice or TypeID) then begin
      AllocContents(0);
      SetCapacity(AStrings.Count - 1);
    end;

      P := Pos(' of ',LowerCase(FTypeName));
      Id := 0;
      FTypeIdentified := TypeID;
      if TypeID then begin
        ChoiceCount := 32;
        while (Idx < AStrings.Count) and
              (AStrings[Idx] <> 'END') and
              (ParCount > 0) do begin
          S := AStrings[Idx] + #0;
          Str := PChar(S);
          while Str^ in [#9,' '] do
            Inc(Str);
          if Str^ <> '{' then
            raise Exception.Create('Illegal format: Expected symbol "{" not found in the string '#13#10 + S);
          Inc(Str);
          while Str^ in [#9,' '] do
            Inc(Str);
          P := 0;
          repeat
            while Str[P] in [#9,' '] do
              Inc(P);
            while Str[P] in ['a'..'z','A'..'Z','0'..'9','_','-'] do
              Inc(P);
          until (Str[P] in [#0,'}']) or
                (StrLIComp(Str + P + 1,'IDENTIFIED BY ',14) = 0);
          Str[P] := #0;
          vTypeName := Str;
          UniqueString(vTypeName);
          S := Str + P + 1;
          if Id >= ChoiceCount then begin
            F := AddChoice(IntToStr(Id),vTypeName);
            F^.Persistent := False;
          end else begin
            F := Choices[Id];
            F^.VarName := IntToStr(Id);
            F^.TypeName := vTypeName;
          end;
          P0 := Pos('}',S);
          if P0 < 0 then
            P0 := System.Length(S);
          P := Pos('IDENTIFIED BY ',UpperCase(S)) + 14;
          if P = 14 then
            raise Exception.Create('Syntax error: Expected token "IDENTIFIED BY" not found');
          OID := GetRegistredOID(Trim(Copy(S,P,P0-P)));
          if OID = '' then
            OID := Trim(Copy(S,P,P0-P));
          F^.IdentifiedBy := OID;
          F^.Persistent := True;
          if F^.Constructed then
            LoadField(F);
          S := AStrings[Idx];
          P := Pos('{',S);
          P0 := Pos('}',S);
          if (P <= 0) or ((P0 > 0) and ((P0 < P) or (P <= 0))) then
            P := P0;
          if P > 0 then begin
            while P <= System.Length(S) do begin
              if S[P] = '{' then
                Inc(ParCount)
              else if S[P] = '}' then
                Dec(ParCount);
              Inc(P);
            end;
          end;
          Inc(Idx);
          Inc(Id);
        end;
      end else if Choice then begin
        ChoiceCount := 32;
        while (Idx < AStrings.Count) and
              (AStrings[Idx] <> 'END') and
              (ParCount > 0) do begin
          S := AStrings[Idx];
          P := Pos(#9,S);
          if P = 0 then
            P := Pos(' ',S);
          if P = 0 then
            raise Exception.Create('Illegal format: Expected blank space not found');
          if Id >= ChoiceCount then
            F := AddChoice(Trim(Copy(S,1,P-1)),Copy(S,P+1,System.Length(S)-P-1))
          else begin
            F := Choices[Id];
            F^.FVarName := Trim(Copy(S,1,P-1));
            F^.TypeName := Copy(S,P+1,System.Length(S)-P-1);
            F^.Persistent := True;
          end;
          if F^.Constructed then begin
            if (F^.FTypeName <> '') and
               (F^.FTypeName[1] = '[') and
               not F^.Implicit then
              LoadField(F^.Items[0])
            else
              LoadField(F);
          end;
          if F^.Implicit and not FStrictTagging then begin
            Inc(Id);
            if Id >= ChoiceCount then
              F := AddChoice(F^.FVarName,StringReplace(F^.TypeName,' IMPLICIT','',[rfIgnoreCase]))
            else begin
              Choices[Id].FVarName := F^.FVarName;
              Choices[Id].TypeName := StringReplace(F^.TypeName,' IMPLICIT','',[rfIgnoreCase]);
              Choices[Id].Persistent := True;
              F := Choices[Id];
            end;
            LoadField(F^.Items[0]);
          end;
          P := Pos('{',S);
          P0 := Pos('}',S);
          if (P <= 0) or ((P0 > 0) and ((P0 < P) or (P <= 0))) then
            P := P0;
          if P > 0 then begin
            while P <= System.Length(S) do begin
              if S[P] = '{' then
                Inc(ParCount)
              else if S[P] = '}' then
                Dec(ParCount);
              Inc(P);
            end;
          end;
          Inc(Idx);
          Inc(Id);
        end;
      end else if P > 0 then begin
        S := Copy(FTypeName,P+4,MaxInt);
        if FTemplate = nil then begin
          NewComposeASN1Struct(FTemplate,V_ASN1_UNIVERSAL,True,Cardinal(V_ASN1_UNDEF));
          FTemplate._AddRef;
        end;
        FTemplate.TypeName := S;
        LoadField(@FTemplate);
      end else
        while (Idx < AStrings.Count) and
              (AStrings[Idx] <> 'END') and
              (ParCount > 0) do begin
          S := Trim(AStrings[Idx]);
          P := Pos(#9,S);
          if P = 0 then
            P := Pos(' ',S);
          if P = 0 then
            raise Exception.Create('Illegal format: Expected blank space not found in "' + S + '".');
          Str := @S[P + 1];
          P0 := 0;
          Delim := #0;
          for L := P to System.Length(S) do begin
            Delim := Str[0];
            case Delim of
              '.':
                if StrLIComp(Str,'.&type(&',8) = 0 then begin
                  P0 := L;
                  Break
                end else
                  raise Exception.Create('Illegal format: Illegal use of symbol "." in "' + S + '".');
              ',','}':
                begin
                  P0 := L;
                  Break;
                end;
            end;
            Inc(Str);
          end;
          if P0 = 0 then
            raise Exception.Create('Illegal format: Expected line termination not found.');
          S[P] := #0;
          S[P0 + 1] := #0;
          F := AddField(PChar(S),PChar(@S[P+1]),nil);
          if Delim = '.' then begin
            P0 := P0 + 8;
            if S[P0] <> '&' then
              raise Exception.Create('Illegal format: Expected symbol "&" not found');
            P := Pos(')',S) - 1;
            if P < 0 then
              raise Exception.Create('Illegal format: Expected symbol ")" not found');
            if F^.Owner = nil then
              F^.FIDField := Copy(S,P0+1,P-P0)
            else
              F^.IDField := Copy(S,P0+1,P-P0);
            F^.FTypeIdentified := True;
            if F^.FVarName = '' then begin
              if F^.Owner = nil then
                Items[ItemCount-1]^.Optional := Pos('optional',LowerCase(S)) > P0
              else
                F^.Owner.FOptional := Pos('optional',LowerCase(S)) > P0;
            end else
              F^.FOptional := Pos('optional',LowerCase(S)) > P0;
            Str := @S[P+2];
          end else begin
            Str := @S[P0 + 1];
            Str[0] := Delim;
          end;
          if Items[ItemCount - 1]^.Constructed then begin
            Impl := F.Implicit;
            vTag := F.Tag;
            vCls := F.Cls;
            vTypeName := F.TypeName;
            LoadField(F);
            if Impl then begin
              F.FTypeName := vTypeName;
              F.FImplicit := Impl;
              F.FTag := vTag;
              F.FCls := vCls;
            end;
          end;
          P := StrPos(Str,'{') - Str;
          P0 := StrPos(Str,'}') - Str;
          if (P < 0) or ((P0 >= 0) and (P0 < P)) then
            P := P0;
          if P >= 0 then begin
            L := System.Length(Str);
            while P < L do begin
              if Str[P] = '{' then
                Inc(ParCount)
              else if Str[P] = '}' then
                Dec(ParCount);
              Inc(P);
            end;
          end;
          Inc(Idx);
        end;
      SS := TStringStream.Create('');
      try
        if Choice or TypeID then ChoiceCount := Id;
        if HasDefaultValue then begin
          SS.Size := 0;
          SS.WriteString(Default);
          SS.Position := 0;
          ASN1FromStream(SS,Self);
        end;
        SS.Size := 0;
        Idx := AStrings.IndexOf('-----BEGIN ENCODED CONTENT-----') + 2;
        if (Idx > 1) and TopItem then begin
          S := '';
          while (Idx < AStrings.Count) and
                (AStrings[Idx] <> '-----END ENCODED CONTENT-----') do begin
            S := S + AStrings[Idx];
            Inc(Idx);
          end;
          SS.WriteString(MIME64ToStr(S));
          SS.Position := 0;
          ASN1FromStream(SS,Self);
        end;
      finally
        SS.Free;
      end;

  end else if Assigned(DeclaredTypes) then begin
    Idx := DeclaredTypes.IndexOf(FTypeName);
    if Idx >= 0 then
      DeclaredTypes.Delete(Idx);
  end;

  FEstTotalCount := FEstTotalCount + InstanceCount - IC;
end;

function TASN1Struct.SaveToStream(AStream: TStream; Format: Byte;
  DeclaredTypes: TStrings): Integer;
var
  S, DeclTN, DeclImplTN: string;
  DeclImpl: Boolean;
  SL, DTs: TStringList;
  P, Idx, J: Integer;
  L: LongWord;
  SS: TStringStream;
  Idf: PASN1Struct;
  Buf: PChar;

  procedure DeclareTypeName(Struct: TASN1Struct);
  begin
    if Struct.Implicit then
      DeclaredTypes.AddObject(Struct.FImplicitTypeName,Struct)
    else
      DeclaredTypes.AddObject(Struct.FTypeName,Struct);
  end;

  function TypeNameNotDeclared(Struct: TASN1Struct): Boolean;
  begin
    if Struct.Implicit then
      Result := DeclaredTypes.IndexOf(Struct.FImplicitTypeName) < 0
    else
      Result := DeclaredTypes.IndexOf(Struct.FTypeName) < 0
  end;

  function CheckModule(Struct: TASN1Struct): Boolean;
  begin
    Result := (Struct.ModuleName = '') or
              (Struct.ModuleName = ModuleName);
    if not Result then
      DeclareTypeName(Struct)
    else if Struct.FModuleName = '' then
      Struct.FModuleName := ModuleName;
  end;

  procedure WriteImports(Strings: TStrings);
  var
    Imports, IL: TStringList;
    I, J: Integer;
    F: TASN1Struct;
    S: string;
  begin
    Imports := TStringList.Create;
    try
      for I := 0 to DTs.Count - 1 do begin
        F := TASN1Struct(DTs.Objects[I]);
        if F = nil then Continue;
        if (F.ModuleName <> '') and
           (F.ModuleName <> ModuleName) then begin
          J := Imports.IndexOf(F.ModuleName);
          if J < 0 then
            J := Imports.AddObject(F.ModuleName,TStringList.Create);
          IL := TStringList(Imports.Objects[J]);
          if F.AliasTypeName <> '' then begin
            J := IL.IndexOf(F.AliasTypeName);
            if J < 0 then
              IL.Add(F.AliasTypeName);
          end else if F.Implicit then begin
            J := IL.IndexOf(F.ImplicitTypeName);
            if J < 0 then
              IL.Add(F.ImplicitTypeName);
          end else begin
            J := IL.IndexOf(F.TypeName);
            if J < 0 then
              IL.Add(F.TypeName);
          end;
        end;
      end;
      if Imports.Count > 0 then begin
        Strings.Insert(2,'');
        for I := 0 to Imports.Count - 1 do begin
          IL := TStringList(Imports.Objects[I]);
          if IL.Count = 0 then Continue; // <-- Shouldn't happen
          S := 'IMPORTS';
          for J := 0 to IL.Count - 1 do
            S := S + ' ' + IL[J] + ',';
          S[System.Length(S)] := ' ';
          S := S + 'FROM ' + Imports[I] + ';';
          Strings.Insert(3,S);
          IL.Free;
        end;
      end;
    finally
      Imports.Free;
    end;
  end;

begin
  Result := 0;
  if Format and 2 > 0 then begin
    if True then begin
      if DeclaredTypes = nil then begin
        DTs := TStringList.Create;
        DeclaredTypes := DTs;
      end else
        DTs := nil;
      SL := TStringList.Create;
      try
        SL.BeginUpdate;
        DeclareTypeName(Self);
        if Assigned(DTs) then begin
          if FModuleName <> '' then
            SL.Add(FModuleName + ' DEFINITIONS ::=')
          else
            SL.Add('ASN.1 Module');
          SL.Add('BEGIN');
          SL.Add('');
        end;
        if FAliasTypeName <> '' then begin
          if Implicit then begin
            if FAliasTypeName <> ImplicitTypeName then begin
              S := SysUtils.Format('%s ::= %s',[FImplicitTypeName,
                                                FAliasTypeName]);
              SL.Add(S);
              SL.Add('');
              SS := TStringStream.Create(SL.Text);
              try
                Result := AStream.CopyFrom(SS,0);
              finally
                SS.Free;
              end;
            end;
          end else if Tag <> $FFFFFFFF then begin
            if FAliasTypeName <> TypeName then begin
              S := SysUtils.Format('%s ::= %s',[FTypeName,
                                                FAliasTypeName]);
              SL.Add(S);
              SL.Add('');
              SS := TStringStream.Create(SL.Text);
              try
                Result := AStream.CopyFrom(SS,0);
              finally
                SS.Free;
              end;
            end;
          end;
          if DeclaredTypes.IndexOf(FAliasTypeName) < 0 then begin
            DeclTN := FTypeName;
            DeclImplTN := FImplicitTypeName;
            DeclImpl := FImplicit;
            FImplicit := False;
            FTypeName := FAliasTypeName;
            FAliasTypeName := '';
            try
              SaveToStream(AStream,2,DeclaredTypes);
            finally
              FAliasTypeName := FTypeName;
              FTypeName := DeclTN;
              FImplicit := DeclImpl;
              FImplicitTypeName := DeclImplTN;
            end;
          end;
        end else if (ChoiceCount = 0) and (FEncapsulated = nil) and not Constructed then begin
          if Implicit then begin
            if UniversalTypes[ImplicitTag] <> ImplicitTypeName then begin
              S := SysUtils.Format('%s ::= %s',[FImplicitTypeName,
                                                UniversalTypes[ImplicitTag]]);
              SL.Add(S);
              SL.Add('');
              SS := TStringStream.Create(SL.Text);
              try
                Result := AStream.CopyFrom(SS,0);
              finally
                SS.Free;
              end;
            end;
          end else if Tag <> $FFFFFFFF then begin
            if UniversalTypes[Tag] <> TypeName then begin
              S := SysUtils.Format('%s ::= %s',[FTypeName,
                                                UniversalTypes[Tag]]);
              SL.Add(S);
              SL.Add('');
              SS := TStringStream.Create(SL.Text);
              try
                Result := AStream.CopyFrom(SS,0);
              finally
                SS.Free;
              end;
            end;
          end;
        end else if FTypeIdentified then begin
          S := SysUtils.Format('%s ::= TYPE-IDENTIFIER {',[FTypeName]);
          Idf := FindIDField;
          if (Idf = nil) or (Idf^.Tag = V_ASN1_OBJECT) then
            for Idx := 0 to ChoiceCount - 1 do begin
              SL.Add(S);
              S := SysUtils.Format('     {%s'#9'IDENTIFIED BY %s}|',
                                   [Choices[Idx]^.FTypeName,
                                    GetObjectName(Choices[Idx]^.IdentifiedBy)]);
            end
          else
            for Idx := 0 to ChoiceCount - 1 do begin
              SL.Add(S);
              S := Choices[Idx]^.FBERIdentifiedBy;
              L := 0;
              for J := 1 to System.Length(S) do
                L := L shl 8 + Byte(S[J]);
              S := SysUtils.Format('     {%s'#9'IDENTIFIED BY %s}|',
                                   [Choices[Idx]^.FTypeName,
                                    IntToStr(L)]);
            end;
          S[System.Length(S)] := '}';
          SL.Add(S);
          SL.Add('');
          SS := TStringStream.Create('');
          try
            for Idx := 0 to ChoiceCount - 1 do begin
              if (((DeclaredTypes.IndexOf(Choices[Idx]^.TypeName) < 0) and
                   not Choices[Idx]^.Implicit) or
                  ((DeclaredTypes.IndexOf(Choices[Idx]^.ImplicitTypeName) < 0) and
                   Choices[Idx]^.Implicit)) and
                 CheckModule(Choices[Idx]^) then
                Choices[Idx]^.SaveToStream(SS,2,DeclaredTypes);
              S := GetOIDDeclaration(Choices[Idx]^.IdentifiedBy);
              if (S <> '') and
                 (DeclaredTypes.IndexOf(Choices[Idx]^.IdentifiedBy) < 0) then begin
                SS.WriteString(S);
                SS.WriteString(#13#10#13#10);
                DeclaredTypes.Add(Choices[Idx]^.IdentifiedBy);
              end;
            end;
            if SS.DataString <> '' then
              SL.Add(SS.DataString);
            SL.Text := SL.Text;
            while (SL.Count > 1) and
                  (SL[SL.Count - 1] = '') and (SL[SL.Count - 2] = '') do
              SL.Delete(SL.Count - 1);
            if Assigned(DTs) then begin
              WriteImports(SL);
              SL.Add('END'#13#10);
            end;
            SS.Size := 0;
            SL.SaveToStream(SS);
            Result := AStream.CopyFrom(SS,0);
          finally
            SS.Free;
          end;
        end else if ChoiceCount > 0 then begin
          if Implicit and ((FTypeName = '') or (FTypeName[1] = '[')) then
            S := SysUtils.Format('%s ::= CHOICE {',[FImplicitTypeName])
          else
            S := SysUtils.Format('%s ::= CHOICE {',[FTypeName]);
          for Idx := 0 to ChoiceCount - 1 do begin
            SL.Add(S);
            if (Idx > 0) and
               (Choices[Idx]^.FVarName = Choices[Idx-1]^.FVarName) then
              Continue;
            S := SysUtils.Format('     %s'#9'%s,',
                                 [Choices[Idx]^.FVarName,
                                  Choices[Idx]^.FTypeName]);
          end;
          S[System.Length(S)] := '}';
          SL.Add(S);
          SL.Add('');
          SS := TStringStream.Create('');
          try
            for Idx := 0 to ChoiceCount - 1 do
              if (((DeclaredTypes.IndexOf(Choices[Idx]^.TypeName) < 0) and
                   not Choices[Idx]^.Implicit) or
                  ((DeclaredTypes.IndexOf(Choices[Idx]^.ImplicitTypeName) < 0) and
                   Choices[Idx]^.Implicit)) and
                 CheckModule(Choices[Idx]^) then
                Choices[Idx]^.SaveToStream(SS,2,DeclaredTypes);
            if SS.DataString <> '' then
              SL.Add(SS.DataString);
            SL.Text := SL.Text;
            while (SL.Count > 1) and
                  (SL[SL.Count - 1] = '') and (SL[SL.Count - 2] = '') do
              SL.Delete(SL.Count - 1);
            if Assigned(DTs) then begin
              WriteImports(SL);
              SL.Add('END'#13#10);
            end;
            SS.Size := 0;
            SL.SaveToStream(SS);
            Result := AStream.CopyFrom(SS,0);
          finally
            SS.Free;
          end;
        end else if Assigned(FTemplate) then begin
          if Pos(' of ',LowerCase(FTypeName)) = 0 then begin
            if FImplicit then
              S := SysUtils.Format('%s ::= %s OF %s',[FImplicitTypeName,
                                                      UniversalTypes[ImplicitTag],
                                                      FTemplate.TypeName])
            else
              S := SysUtils.Format('%s ::= %s OF %s',[FTypeName,
                                                      UniversalTypes[Tag],
                                                      FTemplate.TypeName]);
            SL.Add(S);
            SL.Add('');
          end;
          if (((DeclaredTypes.IndexOf(FTemplate.TypeName) < 0) and
               not FTemplate.Implicit) or
              ((DeclaredTypes.IndexOf(FTemplate.ImplicitTypeName) < 0) and
               FTemplate.Implicit)) and
             CheckModule(FTemplate) then begin
            SS := TStringStream.Create('');
            try
              FTemplate.SaveToStream(SS,2,DeclaredTypes);
              SL.Add(SS.DataString);
              while (SL.Count > 1) and
                    (SL[SL.Count - 1] = '') and (SL[SL.Count - 2] = '') do
                SL.Delete(SL.Count - 1);
              if Assigned(DTs) then begin
                WriteImports(SL);
                SL.Add('END'#13#10);
              end;
              SS.Size := 0;
              SL.SaveToStream(SS);
              Result := AStream.CopyFrom(SS,0);
            finally
              SS.Free;
            end;
          end else begin
            SS := TStringStream.Create('');
            try
              while (SL.Count > 1) and
                    (SL[SL.Count - 1] = '') and (SL[SL.Count - 2] = '') do
                SL.Delete(SL.Count - 1);
              if Assigned(DTs) then begin
                WriteImports(SL);
                SL.Add('END'#13#10);
              end;
              SL.SaveToStream(SS);
              Result := AStream.CopyFrom(SS,0);
            finally
              SS.Free;
            end;
          end;
        end else if Assigned(FEncapsulated) then begin
          if Pos(' encapsulates ',LowerCase(FTypeName)) = 0 then begin
            if FImplicit then
              S := SysUtils.Format('%s ::= %s ENCAPSULATES %s',
                                   [FImplicitTypeName,
                                    UniversalTypes[ImplicitTag],
                                    FEncapsulated.TypeName])
            else
              S := SysUtils.Format('%s ::= %s ENCAPSULATES %s',
                                   [FTypeName,
                                    UniversalTypes[Tag],
                                    FEncapsulated.TypeName]);
            SL.Add(S);
            SL.Add('');
          end;
          SS := TStringStream.Create('');
          try
            if (((DeclaredTypes.IndexOf(FEncapsulated.TypeName) < 0) and
                 not FEncapsulated.Implicit) or
                ((DeclaredTypes.IndexOf(FEncapsulated.ImplicitTypeName) < 0) and
                 FEncapsulated.Implicit)) and
               CheckModule(FEncapsulated) then begin
              FEncapsulated.SaveToStream(SS,2,DeclaredTypes);
              SL.Add(SS.DataString);

              while (SL.Count > 1) and
                    (SL[SL.Count - 1] = '') and (SL[SL.Count - 2] = '') do
                SL.Delete(SL.Count - 1);
            end;
            if Assigned(DTs) then begin
              WriteImports(SL);
              SL.Add('END'#13#10);
            end;
            SS.Size := 0;
            SL.SaveToStream(SS);
            Result := AStream.CopyFrom(SS,0);
            DeclaredTypes.Add(FEncapsulated.FTypeName);
          finally
            SS.Free;
          end;
        end else begin
          if ((Cls = V_ASN1_UNIVERSAL) and (Tag in [1..30])) or
             Implicit then begin
            if Implicit then
              S := SysUtils.Format('%s ::= %s {',[FImplicitTypeName,
                                                  UniversalTypes[ImplicitTag]])
            else
              S := SysUtils.Format('%s ::= %s {',[FTypeName,
                                                  UniversalTypes[Tag]]);
            for Idx := 0 to ItemCount - 1 do begin
              SL.Add(S);
              if (Items[Idx]^.FCls <> V_ASN1_UNIVERSAL) and
                 not Items[Idx].Implicit and
                 Items[Idx]^.Items[0]^.TypeIdentified then
                S := SysUtils.Format('     %s'#9'%s.&Type(&%s)',
                                     [Items[Idx]^.FVarName,
                                      Items[Idx]^.FTypeName,
                                      Items[Idx]^.Items[0]^.IDField])
              else if Items[Idx]^.TypeIdentified then
                S := SysUtils.Format('     %s'#9'%s.&Type(&%s)',
                                     [Items[Idx]^.FVarName,
                                      Items[Idx]^.FTypeName,
                                      Items[Idx]^.IDField])
              else
                S := SysUtils.Format('     %s'#9'%s',
                                     [Items[Idx]^.FVarName,
                                      Items[Idx]^.FTypeName]);
              if Items[Idx]^.Optional then
                S := SysUtils.Format('%s OPTIONAL,',[S])
              else if Items[Idx]^.HasDefaultValue then
                S := SysUtils.Format('%s DEFAULT %s,',
                                     [S,OsToHex(Items[Idx]^.Default,True)])
              else
                S := SysUtils.Format('%s,',[S]);
            end;
            if S[System.Length(S)] = '{' then
              Delete(S,System.Length(S),1)
            else
              S[System.Length(S)] := '}';
            SL.Add(S);
            SL.Add('');
          end;
          SS := TStringStream.Create('');
          try
            for Idx := 0 to ItemCount - 1 do begin
              if (Items[Idx].TypeName = 'OBJECT') and
                 (Items[Idx].ContentAsOID <> '') and
                 Assigned(Items[Idx].FIdentifies) and
                 (DeclaredTypes.IndexOf(Items[Idx].ContentAsOID) < 0) then begin
                S := GetOIDDeclaration(Items[Idx].ContentAsOID);
                if S <> '' then begin
                  SS.WriteString(S);
                  SS.WriteString(#13#10#13#10);
                  DeclaredTypes.Add(Items[Idx].ContentAsOID);
                end;
              end;
              if (((DeclaredTypes.IndexOf(Items[Idx]^.TypeName) < 0) and not Items[Idx]^.Implicit) or
                  ((DeclaredTypes.IndexOf(Items[Idx]^.ImplicitTypeName) < 0) and Items[Idx]^.Implicit) or
                  (Items[Idx]^.ChoiceCount > 0)) and
                 CheckModule(Items[Idx]^) then
                Items[Idx]^.SaveToStream(SS,2,DeclaredTypes);
            end;
            if SS.DataString <> '' then
              SL.Add(SS.DataString);
            SL.Text := SL.Text;
            while (SL.Count > 1) and
                  (SL[SL.Count - 1] = '') and (SL[SL.Count - 2] = '') do
              SL.Delete(SL.Count - 1);
            if Assigned(DTs) then begin
              WriteImports(SL);
              SL.Add('END'#13#10);
            end;
            SS.Size := 0;
            SL.SaveToStream(SS);
            Result := AStream.CopyFrom(SS,0);
          finally
            SS.Free;
          end;
        end;
        if Assigned(DTs) then
          RegisterModule(FModuleName,DTs);
      finally
        SL.EndUpdate;
        SL.Free;
        DTs.Free;
      end;
    end;
    if Format and 1 > 0 then begin
      if Length >= 0 then
        CalculateLength;
      SS := TStringStream.Create('');
      try
        AllowResume := False;
        ASN1ToStream(Self,SS);
        S := StrToMIME64(SS.DataString);
      finally
        SS.Free;
      end;
      P := 77;
      while P < System.Length(S) do begin
        Insert(#13#10,S,P);
        Inc(P,78);
      end;
      S := SysUtils.Format('-----BEGIN ENCODED CONTENT-----'#13#10 +
                           'Content-Encoding: Base64'#13#10 +
                           '%s'#13#10 +
                           '-----END ENCODED CONTENT-----'#13#10,
                           [S]);
      AStream.Write(S[1],System.Length(S))
    end;
  end else if Format and 1 > 0 then begin
    if ReadOnly and Assigned(FReadOnlyCntSlave) and (not FModifiedChild) and
       (Length + CalcASN1IdenAndLen(Self) = FReadOnlyCntLenSlave) then
      AStream.Write(FReadOnlyCntSlave^,FReadOnlyCntLenSlave)
    else begin
      if Length >= 0 then
        CalculateLength;
      if ReadOnly then begin
        FReadOnlyCntLen := Length + CalcASN1IdenAndLen(Self);
        ReallocMem(FReadOnlyCnt,FReadOnlyCntLen);
        Buf := FReadOnlyCnt;
        P := FReadOnlyCntLen;
        Result := ASN1ToBuf(Self,Buf,P);
        Assert(Result = FReadOnlyCntLen);
        FReadOnlyCntSlave := FReadOnlyCnt;
        FReadOnlyCntLenSlave := FReadOnlyCntLen;
        AStream.Write(FReadOnlyCnt^,FReadOnlyCntLen);
      end else
        Result := ASN1ToStream(Self,AStream);
    end;
  end;
end;

procedure TASN1Struct.SetAfterLoad(const Value: TASN1FieldEvent);
begin
  FAfterLoad := Value;
end;

procedure TASN1Struct.SetAfterSave(const Value: TASN1FieldEvent);
begin
  FAfterSave := Value;
end;

procedure TASN1Struct.SetAsDefault;
var
  SS: TStringStream;
begin
  SS := TStringStream.Create('');
  try
    if Constructed then
      CalculateLength;
    ASN1ToStream(Self,SS);
    Default := SS.DataString;
    FHasDefaultValue := Default <> '';
  finally
    SS.Free;
  end;
  Change(ctDecl);
end;

procedure TASN1Struct.SetBeforeLoad(const Value: TASN1FieldEvent);
begin
  FBeforeLoad := Value;
end;

procedure TASN1Struct.SetBeforeSave(const Value: TASN1FieldEvent);
begin
  FBeforeSave := Value;
end;

procedure TASN1Struct.SetCapacity(NewCapacity: Integer);
var
  Idx: Integer;
begin
  Assert(Integer(NewCapacity) >= 0);
  Assert(Constructed);
  if Assigned(Cnt) and (Integer(NewCapacity) < Contents^.ItemCount) then begin
    for Idx := NewCapacity to Contents^.ItemCount - 1 do
      ReleaseASN1Struct(Contents^.Items[Idx],Self);
    Contents^.ItemCount := NewCapacity;
  end;
  if Cnt = nil then begin
    GetMem(Cnt,SizeOf(Integer) + SizeOf(TASN1Struct) * NewCapacity);
    FillChar(Contents^,SizeOf(Integer) + SizeOf(TASN1Struct) * NewCapacity,0);
  end else
    ReallocMem(Cnt,SizeOf(Integer) + SizeOf(TASN1Struct) * NewCapacity);
  if (NewCapacity > 0) and (Integer(NewCapacity) > Contents^.ItemCount) then begin
    FillChar(Contents^.Items[Contents^.ItemCount],
             SizeOf(TASN1Struct)*(Integer(NewCapacity) - Contents^.ItemCount),0);
  end;
  FCapacity := NewCapacity;
end;

procedure TASN1Struct.SetCaptureContent(const Value: Boolean);
var
  P: Pointer;
begin
  Assert(AllowResume and not Constructed);
  FCaptureContent := Value;
  if Value and Assigned(Cnt) then begin
    P := Cnt;
    Cnt := nil;
    FreeMem(P);
  end;
end;

procedure TASN1Struct.SetCls(const Value: Byte);
begin
  FCls := Value;
  if FPersistent then Exit;
  if Value = V_ASN1_UNIVERSAL then
    SetTag(FTag)
  else begin
    case Value of
      V_ASN1_CONTEXT_SPECIFIC: FTypeName := Format('[%d]',[FTag]);
      V_ASN1_APPLICATION:      FTypeName := Format('[APPLICATION %d]',[FTag]);
      V_ASN1_PRIVATE:          FTypeName := Format('[PRIVATE %d]',[FTag]);
    end;
    Change(ctDecl);
  end;
end;

procedure TASN1Struct.SetConstructed(const Value: Boolean);
begin
  if FConstructed xor Value then
    DisposeContent;
  FConstructed := Value;
  FCaptureContent := FCaptureContent and not Value;
end;

procedure TASN1Struct.SetContent(const Buf; Count: Integer);
begin
  Assert(not Constructed);
  Assert(not ReadOnly);
  if Assigned(Cnt) then begin
    if Length > Count then
      FillChar(Content[Count],Length-Count,0);
    if (Count > Length) or (Count = 0) then
      ReallocMem(Cnt,Count);
  end else if Count > 0 then
    GetMem(Cnt,Count);
  if @Buf <> nil then
    Move(Buf,Cnt^,Count);
  FLength := Count;
  Change(ctCnt);
end;

procedure TASN1Struct.SetItems(index: Integer; const Value: PASN1Struct);
begin
  Assert((index >= 0) and (index < ItemCount));
  if LongInt(Value) <> LongInt(Items[index]) then
    CopyASN1Struct(Contents^.Items[index],Value^);
end;

procedure TASN1Struct.SetLength(const Value: Integer);
begin
  Assert(((Value >= 0) or CaptureContent) and not Constructed,
         Format('Field: %s. Cannot set length %d ',[GetNamePath,Value]));
  if Value >= 0 then begin
    SetContent(Pointer(nil)^,Value);
    FillChar(Cnt^,Value,0);
  end else begin
    SetContent(Pointer(nil)^,0);
    FLength := -1;
  end;
end;

procedure TASN1Struct.SetOnCaptureLoad(const Value: TASN1CaptureEvent);
begin
  FOnCaptureLoad := Value;
end;

procedure TASN1Struct.SetOnCaptureSave(const Value: TASN1CaptureEvent);
begin
  FOnCaptureSave := Value;
end;

procedure TASN1Struct.SetTag(const Value: Cardinal);
begin
  FTag := Value;
  if FPersistent then Exit;
  if Cls = V_ASN1_UNIVERSAL then begin
    if Value < 31 then
      FTypeName := UniversalTypes[Value]
    else
      FTypeName := Format('[UNIVERSAL %d]',[Value]);
    Change(ctDecl);
  end else
    SetCls(Cls);
end;

function UniversalTag(P: PChar; Len: Integer): Byte;
var
  C: Char;
begin
  Result := 31;
  if Len >= 3 then begin
    C := P[0];
    if ((C >= 'B') and (C <= 'O')) or ((C >= 'b') and (C <= 'o')) then case C of
      'b','B':
        begin
          if P[1] in ['o','O'] then begin
            if StrLIComp(P,'BOOLEAN',7) = 0 then
              Result := V_ASN1_BOOLEAN;
          end else if P[1] in ['i','I'] then begin
            if StrLIComp(P,'BIT STRING',10) = 0 then
              Result := V_ASN1_BIT_STRING;
          end else if P[1] in ['m','M'] then begin
            if StrLIComp(P,'BMPString',9) = 0 then
              Result := V_ASN1_BMPSTRING;
          end;
        end;
      'e','E':
        begin
          if P[1] in ['o','O'] then begin
            if StrLIComp(P,'EOC',3) = 0 then
              Result := V_ASN1_EOC;
          end else if P[1] in ['x','X'] then begin
            if StrLIComp(P,'EXTERNAL',8) = 0 then
              Result := V_ASN1_EXTERNAL;
          end else if P[1] in ['n','N'] then begin
            if StrLIComp(P,'ENUMERATED',10) = 0 then
              Result := V_ASN1_ENUMERATED;
          end;
        end;
      'g','G':
        begin
          if P[1] in ['r','R'] then begin
            if StrLIComp(P,'GraphicString',13) = 0 then
              Result := V_ASN1_GRAPHICSTRING;
          end else if P[1] in ['e','E'] then begin
            if StrLIComp(P,'GeneralizedTime',15) = 0 then
              Result := V_ASN1_GENERALIZEDTIME
            else if StrLIComp(P,'GeneralString',13) = 0 then
              Result := V_ASN1_GENERALSTRING;
          end;
        end;
      'i','I':
        begin
          if P[1] in ['n','N'] then begin
            if StrLIComp(P,'INTEGER',7) = 0 then
              Result := V_ASN1_INTEGER;
          end else if P[1] in ['a','A'] then begin
            if StrLIComp(P,'IA5String',8) = 0 then
              Result := V_ASN1_IA5STRING;
          end;
        end;
      'n','N':
        begin
          if P[2] in ['l','L'] then begin
            if StrLIComp(P,'NULL',4) = 0 then
              Result := V_ASN1_NULL;
          end else if P[2] in ['m','M'] then begin
            if StrLIComp(P,'NumericString',13) = 0 then
              Result := V_ASN1_NUMERICSTRING;
          end;
        end;
      'o','O':
        begin
          if P[1] in ['c','C'] then begin
            if StrLIComp(P,'OCTET STRING',12) = 0 then
              Result := V_ASN1_OCTET_STRING;
          end else if P[1] in ['b','B'] then begin
            if StrLIComp(P,'OBJECT-DESCRIPTOR',17) = 0 then
              Result := V_ASN1_OBJECT_DESCRIPTOR
            else if StrLIComp(P,'OBJECT',6) = 0 then
              Result := V_ASN1_OBJECT;
          end;
        end;
    end else
    if ((C >= 'P') and (C <= 'V')) or ((C >= 'p') and (C <= 'v')) then case C of
      'p','P':
        begin
          if P[1] in ['r','r'] then
            if StrLIComp(P,'PrintableString',15) = 0 then
              Result := V_ASN1_PRINTABLESTRING;
        end;
      'r','R':
        begin
          if P[1] in ['e','E'] then
            if StrLIComp(P,'REAL',4) = 0 then
              Result := V_ASN1_REAL;
        end;
      's','S':
        begin
          if P[2] in ['q','Q'] then begin
            if StrLIComp(P,'SEQUENCE',8) = 0 then
              Result := V_ASN1_SEQUENCE;
          end else if P[2] in ['t','T'] then begin
            if StrLIComp(P,'SET',3) = 0 then
              Result := V_ASN1_SET;
          end;
        end;
      't','T':
        begin
          if P[1] in ['e','E'] then begin
            if StrLIComp(P,'TeletexString',13) = 0 then
              Result := V_ASN1_TELETEXSTRING;
          end else if P[1] = '6' then begin
            if StrLIComp(P,'T61STRING',9) = 0 then
              Result := V_ASN1_T61STRING;
          end;
        end;
      'u','U':
        begin
          if P[2] in ['f','F'] then begin
            if StrLIComp(P,'UTF8STRING',10) = 0 then
              Result := V_ASN1_UTF8STRING;
          end else if P[2] in ['c','C'] then begin
            if StrLIComp(P,'UTCTime',7) = 0 then
              Result := V_ASN1_UTCTIME;
          end else if P[2] in ['i','I'] then begin
            if StrLIComp(P,'UniversalString',15) = 0 then
              Result := V_ASN1_UNIVERSALSTRING;
          end;
        end;
      'v','V':
        begin
          if P[2] in ['d','D'] then begin
            if StrLIComp(P,'VideotexString',14) = 0 then
              Result := V_ASN1_VIDEOTEXSTRING;
          end else if P[2] in ['s','S'] then begin
            if StrLIComp(P,'VisibleString',13) = 0 then
              Result := V_ASN1_VISIBLESTRING;
          end;
        end;
    end;
  end;
end;

procedure TASN1Struct.SetTypeName(const Value: string);
var
  Str: string;
  Id: Integer;
  State: TTypeState;
begin
  BeginUpdate;
  try
    Str := Trim(Value);
    if Str = '' then begin
      Persistent := False;
      SetTag(Tag);
      Persistent := Constructed;
    end else begin
      Str := Str + #0;
      ASN1ParseTypeName(Str,State);
      if State.Tagged then begin
        FTag := State.Tag;
        FCls := State.Cls;
        if State.Implicit then begin
          FImplicit := True;
          SetImplicitTypeName(State.ImplicitTypeName);
        end else begin
          FImplicit := False;
          if not Constructed or (ItemCount <> 1) then begin
            DisposeContent;
            Constructed := True;
            AddField('',State.ImplicitTypeName,nil);
          end else
            Items[0].SetTypeName(State.ImplicitTypeName);
        end;
      end else begin
        Id := UniversalTag(State.TypeName,State.LastDelim - State.TypeName);
        if Id = 31 then begin
          if not Persistent then begin
            DisposeContent;
            FTag := Cardinal(V_ASN1_UNDEF);
            FCls := V_ASN1_UNIVERSAL;
            Constructed := True;
          end;
        end else if FCls = V_ASN1_UNIVERSAL then begin
          FTag := Id;
          if (Id in [16,17]) xor Constructed then
            DisposeContent;
          Constructed := Id in [16,17];
          FCls := V_ASN1_UNIVERSAL;
        end;

        if State.Encapsulates <> nil then begin
          if Assigned(FEncapsulated) then
            FEncapsulated._Release;
          if StrLIComp(State.Encapsulates,'SEQUENCE',8) = 0 then
            NewComposeASN1Struct(FEncapsulated,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE)
          else if StrLIComp(State.Encapsulates,'SET',8) = 0 then
            NewComposeASN1Struct(FEncapsulated,V_ASN1_UNIVERSAL,True,V_ASN1_SET)
          else begin
            NewComposeASN1Struct(FEncapsulated,V_ASN1_UNIVERSAL,True,Cardinal(V_ASN1_UNDEF));
            FEncapsulated.TypeName := State.Encapsulates;
          end;
          FEncapsulated._AddRef;
          Persistent := Constructed or Persistent;
          FEncapsulated.Persistent := True;
        end else if Assigned(FEncapsulated) and not Persistent then begin
          FEncapsulated._Release;
          FEncapsulated := nil;
        end;

        if Assigned(State.TemplateTypeName) then begin
          if Assigned(FTemplate) then
            FTemplate._Release;
          if StrLIComp(State.TemplateTypeName,'SEQUENCE',8) = 0 then
            NewComposeASN1Struct(FTemplate,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE)
          else if StrLIComp(State.TemplateTypeName,'SET',3) = 0 then
            NewComposeASN1Struct(FTemplate,V_ASN1_UNIVERSAL,True,V_ASN1_SET)
          else begin
            NewComposeASN1Struct(FTemplate,V_ASN1_UNIVERSAL,True,Cardinal(V_ASN1_UNDEF));
            FTemplate.TypeName := State.TemplateTypeName;
          end;
          FTemplate._AddRef;
          Persistent := Constructed or Persistent;
          FTemplate.Persistent := True;
        end else if Assigned(FTemplate) and not Persistent then begin
          FTemplate._Release;
          FTemplate := nil;
        end;
      end;

      if State.Optional then
        FOptional := True
      else if Assigned(State.DefaultValue) then begin
        Default := HexToOS(State.DefaultValue);
        HasDefaultValue := True;
      end;

      State.LastDelim[0] := #0;
      FTypeName := State.TypeName;
    end;
    Change(ctDecl);
  finally
    EndUpdate;
  end;
end;

procedure TASN1Struct.SetTypeNameNoCheck(const ATypeName: string);
begin
  FTypeName := ATypeName;
end;

function TASN1Struct._AddRef: Integer;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TASN1Struct._Release(Garbage: THashList): Integer;
begin
  Dec(FRefCount);
  Result := FRefCount;
  if FRefCount = 0 then begin
    if Assigned(Garbage) then Dispose(Garbage)
    else Free;
  end;
end;

procedure TASN1Struct.SetDoneLoading(const Value: Boolean);
begin
  FDoneLoading := Value and FAllowResume;
end;

procedure TASN1Struct.SetDoneSaving(const Value: Boolean);
begin
  FDoneSaving := Value and FAllowResume;
end;

procedure TASN1Struct.SetOnLoad(const Value: TASN1FieldEvent);
begin
  FOnLoad := Value;
end;

procedure TASN1Struct.SetOnSave(const Value: TASN1FieldEvent);
begin
  FOnSave := Value;
end;

procedure TASN1Struct.DoLoad(Field: TASN1Struct);
begin
  if Assigned(FOnLoad) then
    FOnLoad(Self,Field)     
  {$IFNDEF THIN_TASN1STRUCT}
  else if Assigned(FOwner) then
    FOwner.DoLoad(Field);   
  {$ENDIF}
end;

procedure TASN1Struct.DoSave(Field: TASN1Struct);
begin
  if Assigned(FOnSave) then
    FOnSave(Self,Field)
  {$IFNDEF THIN_TASN1STRUCT}
  else if Assigned(FOwner) then
    FOwner.DoSave(Field);
  {$ENDIF}
end;

procedure TASN1Struct.SetAllowResume(const Value: Boolean);
var
  I: Integer;
begin
  FAllowResume := Value;
  if Constructed then
    for I := 0 to ItemCount - 1 do
      Items[I].AllowResume := Value;
end;

function TASN1Struct.UnambiguousField(ItemIndex: Integer): Boolean;  
var
  I: Integer;
begin
  if FUFTested = FUnambiguousField then begin
    Result := True;
    if ItemCount > ItemIndex then
      for I := ItemIndex downto FUFTested + 1 do begin
        Result := Result and
                  not Items[I].Optional and
                  not Items[I].HasDefaultValue;
        if not Result then Break;
      end;
    if Result then FUnambiguousField := ItemIndex;
    FUFTested := ItemIndex;
  end else
    Result := ItemIndex <= FUnambiguousField;
end;

procedure TASN1Struct.ResetStreaming;
var
  I: Integer;
begin
  FDoneLoading := False;
  FDoneSaving := False;
  FContentCountLoad := 0;
  FContentCountSave := 0;
  FIdenAndLenCountLoad := 0;
  FIdenAndLenCountSave := 0;
  if Constructed then
    for I := 0 to ItemCount - 1 do
      Items[I]^.ResetStreaming;
end;

procedure TASN1Struct.SetImplicitTag(const Value: Cardinal);
begin
  Assert(Implicit);
  if Value = Cardinal(V_ASN1_UNDEF) then begin
    FImplicitTag := V_ASN1_SEQUENCE;
    if FPersistent then Exit;
    FImplicitTypeName := UniversalTypes[V_ASN1_SEQUENCE];
  end else begin
    Assert(Value < 31);
    FImplicitTag := Value;
    if FPersistent then Exit;
    FImplicitTypeName := UniversalTypes[Value];
  end;
  Change(ctDecl);
end;

procedure TASN1Struct.SetImplicitTypeName(const Value: string); 
var
  Str: string;
  Id: Integer;
  State: TTypeState;
begin
  Assert(Implicit);
  BeginUpdate;
  try
    Str := Trim(Value);
    if Str = '' then begin
      Persistent := False;
      SetImplicitTag(ImplicitTag);
      Persistent := True;
    end else begin
      if Str[1] = '[' then
        raise Exception.Create('Illegal type name')
      else begin
        Str := Str + #0;
        ASN1ParseTypeName(Str,State);
        {
        Id := 0;
        while (Id < 31) and
              (StrLIComp(PChar(UniversalTypes[Id]),
                         PChar(Str),
                         System.Length(UniversalTypes[Id])) <> 0)  do
          Inc(Id);
        }
        if State.SequenceOf or State.SetOf then
          Id := State.Tag
        else
          Id := UniversalTag(State.TypeName,State.LastDelim - State.TypeName);
        if Id = 31 then begin
          if not Persistent then begin
            DisposeContent;
            FImplicitTag := Cardinal(V_ASN1_UNDEF);
            Constructed := True;
          end;
        end else begin
          FImplicitTag := Id;
          if (Id in [16,17]) xor Constructed then
            DisposeContent;
          Constructed := Id in [16,17];
        end;
      end;

      if State.Encapsulates <> nil then begin
        if Assigned(FEncapsulated) then
          FEncapsulated._Release;
        if StrLIComp(State.Encapsulates,'SEQUENCE',8) = 0 then
          NewComposeASN1Struct(FEncapsulated,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE)
        else if StrLIComp(State.Encapsulates,'SET',3) = 0 then
          NewComposeASN1Struct(FEncapsulated,V_ASN1_UNIVERSAL,True,V_ASN1_SET)
        else begin
          NewComposeASN1Struct(FEncapsulated,V_ASN1_UNIVERSAL,True,Cardinal(V_ASN1_UNDEF));
          FEncapsulated.TypeName := State.Encapsulates;
        end;
        FEncapsulated._AddRef;
        Persistent := Constructed or Persistent;
        FEncapsulated.Persistent := True;
      end else if Assigned(FEncapsulated) and not Persistent then begin
        FEncapsulated._Release;
        FEncapsulated := nil;
      end;

      if Assigned(State.TemplateTypeName) then begin
        if Assigned(FTemplate) then
          FTemplate._Release;
        if StrLIComp(State.TemplateTypeName,'SEQUENCE',8) = 0 then
          NewComposeASN1Struct(FTemplate,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE)
        else if StrLIComp(State.TemplateTypeName,'SET',3) = 0  then
          NewComposeASN1Struct(FTemplate,V_ASN1_UNIVERSAL,True,V_ASN1_SET)
        else begin
          NewComposeASN1Struct(FTemplate,V_ASN1_UNIVERSAL,True,Cardinal(V_ASN1_UNDEF));
          FTemplate.TypeName := State.TemplateTypeName;
        end;
        FTemplate._AddRef;
        Persistent := True;
        FTemplate.Persistent := True;
      end else if Assigned(FTemplate) then begin
        FTemplate._Release;
        FTemplate := nil;
      end;

      if State.Optional then
        FOptional := True
      else if Assigned(State.DefaultValue) then begin
        Default := HexToOS(State.DefaultValue);
        HasDefaultValue := True;
      end;

      State.LastDelim[0] := #0;
      FImplicitTypeName := State.TypeName;
    end;
    Change(ctDecl);
  finally
    EndUpdate;
  end;
end;

function TASN1Struct.GetChoiceCount: Integer;
begin
  if FChoices = nil then
    Result := 0
  else
    Result := FChoices^.ItemCount;
end;

function TASN1Struct.GetChoices(index: Integer): PASN1Struct;
begin
  Assert((index >= 0) and (index < ChoiceCount),
         Format('Choice Index %d out of range (%d)'#13#10 +
                'TypeName: %s'#13#10 +
                'VarName: %s',
                [index,ChoiceCount,TypeName,GetNamePath]));
  Result := @FChoices^.Items[index];
end;

procedure TASN1Struct.SetChoices(index: Integer; const Value: PASN1Struct);
begin
  Assert((index >= 0) and (index < ChoiceCount));
  if LongInt(Value^) <> LongInt(Choices[index]^) then begin
    FChoices^.Items[index]._Release;
    FChoices^.Items[index] := Value^;
    FChoices^.Items[index]._AddRef;
  end;
end;

procedure TASN1Struct.SetChoiceCountEx(const Value: Integer; Garbage: THashList);
var
  Idx, OldCount: Integer;
begin
  Assert(Integer(Value) >= 0);
  OldCount := ChoiceCount;
  if Value < OldCount then
    for Idx := Value to OldCount - 1 do
      ReleaseASN1Struct(FChoices^.Items[Idx],Self,Garbage);
  if (FChoices = nil) or (OldCount <> Value) then
    ReallocMem(FChoices,SizeOf(Integer) + SizeOf(TASN1Struct)*Value);
  if Value > OldCount then
    for Idx := OldCount to Value - 1 do begin
      FChoices^.Items[Idx] := TASN1Struct.Create;
      FChoices^.Items[Idx]._AddRef;
      FChoices^.Items[Idx].FNoChange := FNoChange;
      FChoices^.Items[Idx].FStrictTagging := FStrictTagging;
    end;
  FChoices^.ItemCount := Value;
end;

procedure TASN1Struct.SetChoiceCount(const Value: Integer);
begin
  SetChoiceCountEx(Value,nil);
end;

function TASN1Struct.IsChoiceTemplate(Struct: TASN1Struct): Boolean;
var
  Idx: Integer;
  C: PASN1Struct;
begin
  Result := False;
  for Idx := 0 to ChoiceCount - 1 do begin
    C := Choices[Idx];
    Result := (Struct = C^) or
              ((C^.ChoiceCount > 0) and
               C^.IsChoiceTemplate(Struct));
    if Result then Exit;
  end;
end;

procedure TASN1Struct.SelectChoice(ChoiceIndex: Integer);
var
  T: TASN1Struct;
begin
  if ChoiceIndex < 0 then begin
    if Assigned(FTemplate) and
       FTemplate.TypeIdentified then begin
      if FTemplate.FRefCount > 1 then begin
        T := TASN1Struct.Create;
        T._AddRef;
        T.CopyTypeInfo(FTemplate);
        FTemplate._Release;
        FTemplate := T;
      end;
      FTemplate.SelectChoice(FTemplate.Choices[ChoiceIndex]);
      FTemplate.FChoiceIndex := ChoiceIndex;
    end else
      SelectChoice(nil)
  end else begin
    SelectChoice(Choices[ChoiceIndex]);
    FChoiceIndex := ChoiceIndex;
  end;
end;

procedure TASN1Struct.SelectChoice(ChoiceTempl: PASN1Struct);
var
  vVarName, vTypeName: string;
begin
  if (ChoiceCount <= 0) and Assigned(FTemplate) and FTemplate.TypeIdentified then
    FTemplate.SelectChoice(ChoiceTempl)
  else if ChoiceTempl = nil then begin
    FChoiceVarName := '';
    FChoiceTypeName := '';
    SetTypeName('ANY');
    FEncapsulated.Free;
    FEncapsulated := nil;
    FPersistent := False;
  end else begin
    vVarName := VarName;
    vTypeName := TypeName;
    CopyTypeInfo(ChoiceTempl^);
    SetForgetOptionalDefault(FForgetOptionalDefault);
    if ChoiceTempl^.FBERIdentifiedBy <> '' then
      FBerIdentifiedBy := ChoiceTempl^.FBERIdentifiedBy;
    FChoiceVarName := FVarName;
    FChoiceTypeName := FTypeName;
    FVarName := vVarName;
    FTypeName := vTypeName;
  end;
end;

procedure TASN1Struct.SetImplicit(const Value: Boolean);
begin
  Assert((ChoiceCount = 0) or TypeIdentified or not Value);
  FImplicit := Value;
end;

function TASN1Struct.AddChoice(const AVarName,
  ATypeName: string): PASN1Struct;
begin
  ChoiceCount := ChoiceCount + 1;
  Result := Choices[ChoiceCount - 1];
  Result^.FVarName := AVarName;
  Result^.TypeName := ATypeName;
  Result^.FStrictTagging := FStrictTagging;
  Change(ctDecl);
end;

procedure TASN1Struct.SetOnLoadStructure(const Value: TASN1FieldEvent);
begin
  FOnLoadStructure := Value;
end;

procedure TASN1Struct.DoLoadStructure(Field: TASN1Struct);
begin
  if Assigned(FOnLoadStructure) then
    FOnLoadStructure(Self,Field)
  {$IFNDEF THIN_TASN1STRUCT}
  else if Assigned(FOwner) then
    FOwner.DoLoadStructure(Field);
  {$ENDIF}
end;

function TASN1Struct.GetIdenAndLen: string;
begin
  System.SetLength(Result,FIdenAndLenCountSave);
  Move(FIdenAndLen,Pointer(Result)^,System.Length(Result));
end;

procedure TASN1Struct.SetNoSave(const Value: Boolean);
begin
  FNoSave := Value;
end;

procedure TASN1Struct.SetIdenAndLen(const Value: string);
begin
  FIdenAndLenCountSave := System.Length(Value);
  Move(Pointer(Value)^,FIdenAndLen,System.Length(Value));
end;

function TASN1Struct.IsEmpty: Boolean;
var
  I: Integer;
  MSD: TSecureMemoryStream;
begin
  Result := (Cnt = nil) or ((Length = 0) and not Constructed);
  if FHasDefaultValue and not Result then begin
    MSD := TSecureMemoryStream.Create;
    try
      FHasDefaultValue := False;
      try
        ASN1ToStream(Self,MSD);
      finally
        FHasDefaultValue := True;
      end;
      if not ((System.Length(Default) = MSD.Size) and
              CompareMem(MSD.Memory,Pointer(Default),System.Length(Default))) then
        Result := False;
    finally
      MSD.Free;
    end;
  end else if Constructed and not Result then begin
    Result := True;
    for I := 0 to ItemCount - 1 do
      Result := Result and Items[I]^.IsEmpty;
  end;
end;

function TASN1Struct.EditField(AVarName: string;
  const AValue: Integer): Boolean;
var
  F: PASN1Struct;
begin
  F := FindField(AVarName);
  Result := Assigned(F);
  if Assigned(F) then
    F.EditContent(AValue);
end;

function TASN1Struct.EditField(AVarName: string;
  const AValue: Cardinal): Boolean;
var
  F: PASN1Struct;
begin
  F := FindField(AVarName);
  Result := Assigned(F);
  if Assigned(F) then
    F.EditContent(AValue);
end;

function TASN1Struct.EditField(AVarName: string;
  const AValue: WideString): Boolean;
var
  F: PASN1Struct;
begin
  F := FindField(AVarName);
  Result := Assigned(F);
  if Assigned(F) then
    F.EditContent(AValue);
end;

function TASN1Struct.EditField(AVarName: string;
  const AValue: Boolean): Boolean;
var
  F: PASN1Struct;
begin
  F := FindField(AVarName);
  Result := Assigned(F);
  if Assigned(F) then
    F.EditContent(AValue);
end;

function TASN1Struct.EditField(AVarName: string;
  const AValue: TDateTime): Boolean;
var
  F: PASN1Struct;
begin
  F := FindField(AVarName);
  Result := Assigned(F);
  if Assigned(F) then
    F.EditContent(AValue);
end;

function TASN1Struct.EditField(AVarName: string;
  const AValue: Extended): Boolean;
var
  F: PASN1Struct;
begin
  F := FindField(AVarName);
  Result := Assigned(F);
  if Assigned(F) then
    F.EditContent(AValue);
end;

function TASN1Struct.EditField(AVarName: string;
  const AValue: Int64): Boolean;
var
  F: PASN1Struct;
begin
  F := FindField(AVarName);
  Result := Assigned(F);
  if Assigned(F) then
    F.EditContent(AValue);
end;

function TASN1Struct.EditField(AVarName: string; const AValue: Int64;
  StoreAsUnsigned: Boolean): Boolean;
var
  F: PASN1Struct;
begin
  F := FindField(AVarName);
  Result := Assigned(F);
  if Assigned(F) then
    F.EditContent(AValue,StoreAsUnsigned);
end;

procedure TASN1Struct.SortSET;
var
  I: Integer;
  SS: TStringStream;
  SL: TSecureStrings;
  Dummy, SaveAllowResume: Boolean;
  S: string;
begin
  if Constructed then begin
    for I := 0 to ItemCount - 1 do
      Items[I]^.SortSET;
    if (ActualTag = V_ASN1_SET) and
       Assigned(FTemplate) and
       (ItemCount > 1) then begin
      ResetStreaming;
      CalculateLength;
      SL := TSecureStrings.Create;
      try
        SL.Duplicates := dupIgnore;
        SL.Sorted := True;
        SS := TStringStream.Create('');
        try
          for I := 0 to ItemCount - 1 do begin
            ASN1ToStream(Items[I]^,SS);
            S := SS.DataString;
            UniqueString(S);
            SL.Add(S);
            SS.Size := 0;
          end;
          SaveAllowResume := AllowResume;
          try
            AllowResume := True;
            ASN1IdenAndLenToStream(Self,SS,Dummy);
            FIdenAndLenCountLoad := FIdenAndLenCountSave;
          finally
            AllowResume := SaveAllowResume;
          end;
          for I := 0 to SL.Count - 1 do
            SS.WriteString(SL[I]);
          SS.Position := 0;
          ASN1FromStream(SS,Self);
        finally
          SS.Free;
        end;
      finally
        SL.Free;
      end;
    end;
  end;
end;

function TASN1Struct.GetNamePath(BrowsePath: Boolean): string;
begin                
  if Assigned(Owner) then begin
    Result := Owner.GetNamePath(BrowsePath);
    if Result <> '' then begin
      if Result[1] <> '/' then
        Result := '/' + Result;
      Result := Result + '/';
    end;
    if Assigned(Owner.Template) then begin
      if BrowsePath then
        Result := Format('%s[c]',[Result])
      else
        Result := Format('%s[%d]',[Result,Owner.IndexOf(Self)])
    end else
      Result := Result + VarName;
  end else
    Result := '';
end;

function TASN1Struct.IndexOf(AItem: TASN1Struct): Integer;
begin
  Result := 0;
  while Result < ItemCount do begin
    if Items[Result]^ = AItem then Exit;
    Inc(Result);
  end;
  Result := -1;
end;

procedure TASN1Struct.SetIDField(const Value: string);
var
  Idf: PASN1Struct;
begin
  Assert(Assigned(FOwner));
  if (Owner.Cls <> V_ASN1_UNIVERSAL) and not Owner.Implicit then begin
    Idf := Owner.Owner.FindField(Value);
    Assert(Assigned(Idf));
    if Idf^.FIdentifies = nil then
      Idf^.FIdentifies := TStringList.Create;
    Idf^.FIdentifies.Add('/' + Owner.VarName + '/');
  end else begin
    Idf := Owner.FindField(Value);
    if (Idf = nil) and Assigned(Owner.FTemplate) and Assigned(Owner.Owner) then
      Idf := Owner.Owner.FindField(Value);
    Assert(Assigned(Idf));
    if Idf^.FIdentifies = nil then
      Idf^.FIdentifies := TStringList.Create;
    Idf^.FIdentifies.Add(VarName);
  end;
  FIDField := Value;
  Change(ctDecl);
end;

procedure TASN1Struct.TypeIdentify;
var
  I: Integer;
  T: PASN1Struct;
begin
  TypeIdentify(T,I);
end;

procedure TASN1Struct.TypeIdentify(var Template: PASN1Struct);
var
  I: Integer;
begin
  TypeIdentify(Template,I);
end;

procedure TASN1Struct.TypeIdentify(var Template: PASN1Struct;
  var Index: Integer; Gently: Boolean);
var
  I: Integer;
  Idf: PASN1Struct;
  B: string;
  T: TASN1Struct;
begin
  if (FIDField = '') and
     Assigned(FTemplate) and
     FTemplate.TypeIdentified then begin
    Idf := FindIDField;
    Assert(Assigned(Idf));
    for I := 0 to FTemplate.ChoiceCount - 1 do begin
      B := FTemplate.Choices[I]^.FBERIdentifiedBy;
      if (System.Length(B) = Idf^.Length) and
         CompareMem(Pointer(B),Idf^.Content,System.Length(B)) then begin
        Index := I;
        FTemplate.FChoiceIndex := I;
        Template := FTemplate.Choices[I];
        if FTemplate.FChoiceTypeName = Template^.FTypeName then
          Exit;
        if Template.FRefCount > 1 then begin
          T := TASN1Struct.Create;
          T._AddRef;
          T.CopyTypeInfo(Template^);
          Template^._Release;
          Template^ := T;
        end;
        FTemplate.SelectChoice(Template);
        Exit;
      end;
    end;
  end else begin
    Assert(TypeIdentified,FTypeName + ' is not type identified');
    Idf := FindIDField;
    Assert(Assigned(Idf));
    for I := 0 to ChoiceCount - 1 do begin
      B := Choices[I]^.FBERIdentifiedBy;
      if (System.Length(B) = Idf^.Length) and
         CompareMem(Pointer(B),Idf^.Content,System.Length(B)) then begin
        Index := I;
        FChoiceIndex := I;
        Template := Choices[I];
        if FChoiceTypeName = Template^.FTypeName then
          Exit;
        if (Template.ChoiceCount > 0) and
           (Template.FRefCount > 1) then begin
          T := TASN1Struct.Create;
          T._AddRef;
          T.CopyTypeInfo(Template^);
          Template^._Release;
          Template^ := T;
        end;
        SelectChoice(Template);
        Exit;
      end;
    end;
  end;
  FChoiceVarName := '';
  FBERIdentifiedBy := '';
  FEncapsulated.Free;
  FEncapsulated := nil;
  Index := -1;
  Template := nil;
  if not Gently then begin
    DisposeContent;
    if Implicit then
      FImplicitTag := Cardinal(V_ASN1_UNDEF)
    else begin
      FTag := Cardinal(V_ASN1_UNDEF);
      FCls := V_ASN1_UNIVERSAL;
    end;
    FConstructed := True;
  end;
end;

{ TResumeStream }

constructor TReadResumeStream.Create;
begin
  FBufferStream := TSecureMemoryStream.Create;
end;

destructor TReadResumeStream.Destroy;
begin
  inherited;
  FBufferStream.Free;
end;

function TReadResumeStream.Read(var Buffer; Count: Integer): Longint;
var
  P: Pointer;
  DCount: Integer;
begin
  Result := FBufferStream.Read(Buffer,Count);
  if Result < Count then begin
    P := Pointer(LongInt(@Buffer) + Result);
    DCount := FDataStream.Read(P^,Count - Result);
    FBufferStream.Write(P^,DCount);
    Result := Result + DCount;
  end;
end;

function TReadResumeStream.Seek(Offset: Integer; Origin: Word): Longint;
var
  OldPos: Integer;
begin
  if Origin = soFromBeginning then begin
    if Offset > FBufferStream.Size then begin
      FBufferStream.Seek(0,soFromEnd);
      FBufferStream.CopyFrom(FDataStream,Offset - FBufferStream.Size);
      Result := FBufferStream.Size;
    end else begin
      FBufferStream.Position := Offset;
      Result := Offset;
    end;
  end else if Origin = soFromCurrent then begin
    OldPos := FBufferStream.Position;
    if Offset + OldPos > FBufferStream.Size then begin
      FBufferStream.Seek(0,soFromEnd);
      FBufferStream.CopyFrom(FDataStream,Offset + OldPos - FBufferStream.Size);
      Result := FBufferStream.Size;
    end else
      Result := FBufferStream.Seek(Offset,Origin);
  end else
    // Not supported
    Result := 0;
end;

procedure TReadResumeStream.SetDataStream(const Value: TStream);
begin
  FDataStream := Value;
end;

function TReadResumeStream.Write(const Buffer; Count: Integer): Longint;
begin
  // Not supported
  Result := 0;
end;

procedure TASN1Struct.DeleteItem(Index: Integer; Garbage: THashList);
var
  Item: TASN1Struct;
begin
  Assert((index >= 0) and (index < ItemCount));
  BeginUpdate;
  try
    Change(ctDelRec);
    Item := Contents^.Items[index];
    ReleaseASN1Struct(Item,Self,Garbage);
    if index < ItemCount - 1 then
      Move(Contents^.Items[index + 1],Contents^.Items[index],(ItemCount - index - 1)*4);
    Contents^.Items[ItemCount - 1] := nil;
    Contents^.ItemCount := ItemCount - 1;
  finally
    EndUpdate;
  end;
end;

procedure TASN1Struct.EditContent(const AValue: PMPInteger;
  StoreAsUnsigned: Boolean);
var
  Str: string;
begin
  Assert(Assigned(AValue));
  if not Constructed then begin
    if StoreAsUnsigned then begin
      System.SetLength(Str,(MPMSB(AValue) + 7) shr 3);
      UMPIntToBase256(AValue,Pointer(Str)^,System.Length(Str));
    end else
      Str := MPIntToBase256(AValue);
    if ((Cls = V_ASN1_UNIVERSAL) and
        (Tag = V_ASN1_BIT_STRING)) or
       (Implicit and (ImplicitTag = V_ASN1_BIT_STRING)) then
      Str := #0 + Str;
    SetContent(Str[1],System.Length(Str));
  end else
    raise Exception.Create('Could not interpret contents');
end;

function TASN1Struct.EditField(AVarName: string; const AValue: PMPInteger;
  StoreAsUnsigned: Boolean): Boolean;
var
  F: PASN1Struct;
begin
  F := FindField(AVarName);
  Result := Assigned(F);
  if Assigned(F) then
    F.EditContent(AValue,StoreAsUnsigned);
end;

function TASN1Struct.ContentAsECPoint(var P: TECPoint; const C: TECurve): Boolean;
var
  L, Len: Integer;
  Ct: PChar;
begin
  Assert(not Constructed);
  L := (MPMSB(C.Q) + 7) shr 3;
  if ActualTag = V_ASN1_BIT_STRING then begin
    Result := Length > 1;
    Len := Length - 1;
    Ct := Content + 1;
  end else begin
    Result := Length > 0;
    Len := Length;
    Ct := Content;
  end;
  if Result then begin
    case Ct[0] of
      #$00:
        begin
          ECInf(P,C);
          Result := True;
        end;
      #$02:
        begin
          Result := Len >= (L + 1);
          if Result then begin
            Base256ToUMPInt(P.X,Ct[1],L,L*8);
            Result := ECDecompress(False,P,C);
          end;
        end;
      #$03:
        begin
          Result := Len >= (L + 1);
          if Result then begin
            Base256ToUMPInt(P.X,Ct[1],L,L*8);
            Result := ECDecompress(True,P,C);
          end;
        end;
      #$04:
        begin
          Result := Len >= (2*L + 1);
          if Result then begin
            Base256ToUMPInt(P.X,Ct[1],L,L*8);
            Base256ToUMPInt(P.Y,Ct[1+L],L,L*8);
            Result := ECOnCurve(P,C);
          end;
        end;
      #$06:
        begin
          Result := Len >= (2*L + 1);
          if Result then begin
            Base256ToUMPInt(P.X,Ct[1],L,L*8);
            Base256ToUMPInt(P.Y,Ct[1+L],L,L*8);
            Result := not ECCompressedBit(P);
            Result := Result and ECOnCurve(P,C);
          end;
        end;
      #$07:
        begin
          Result := Len >= (2*L + 1);
          if Result then begin
            Base256ToUMPInt(P.X,Ct[1],L,L*8);
            Base256ToUMPInt(P.Y,Ct[1+L],L,L*8);
            Result := ECCompressedBit(P);
            Result := Result and ECOnCurve(P,C);
          end;
        end;
    end;
  end;
end;

procedure TASN1Struct.EditContent(const AValue: TECPoint;
  const C: TECurve; Format: TECP2OSFormat);
var
  L, Len: Integer;
  P, Ct: PChar;
begin
  if (ActualTag in [V_ASN1_OCTET_STRING,V_ASN1_BIT_STRING]) and
     not Constructed then begin
    P := nil;
    Len := 0;
    if ECIsInf(AValue,C) then begin
      P := StrAlloc(1);
      Len := 1;
      P[0] := #$00;
    end else case Format of
      cCompressed:
        begin
          L := (MPMSB(C.Q) + 7) shr 3;
          Len := 1 + L;
          P := StrAlloc(Len);
          if ECCompressedBit(AValue) then
            P[0] := #$03
          else
            P[0] := #$02;
          UMPIntToBase256(AValue.X,P[1],L);
        end;
      cUncompressed:
        begin
          L := (MPMSB(C.Q) + 7) shr 3;
          Len := 1 + 2*L;
          P := StrAlloc(Len);
          P[0] := #$04;
          UMPIntToBase256(AValue.X,P[1],L);
          UMPIntToBase256(AValue.Y,P[1+L],L);
        end;
      cHybrid:
        begin
          L := (MPMSB(C.Q) + 7) shr 3;
          Len := 1 + 2*L;
          P := StrAlloc(Len);
          if ECCompressedBit(AValue) then
            P[0] := #$07
          else
            P[0] := #$06;
          UMPIntToBase256(AValue.X,P[1],L);
          UMPIntToBase256(AValue.Y,P[1+L],L);
        end;
    end;
    if ActualTag = V_ASN1_OCTET_STRING then
      SetContent(P^,Len)
    else begin
      SetLength(Len + 1);
      Ct := Cnt;
      Ct[0] := #0;
      Move(P^,(Ct + 1)^,Len);
    end;
    ProtectClear(P^,Len);
    StrDispose(P);
  end else
    raise Exception.Create('Could not interpret contents');
end;

function TASN1Struct.EditField(AVarName: string; const AValue: TECPoint;
  const C: TECurve; Format: TECP2OSFormat): Boolean;
var
  F: PASN1Struct;
begin
  F := FindField(AVarName);
  Result := Assigned(F);
  if Assigned(F) then
    F.EditContent(AValue,C,Format);
end;

function TASN1Struct.AddIntegerField(const AVarName: string;
  AValue: PMPInteger; StoreAsUnsigned, APersistent: Boolean): PASN1Struct;
begin
  Result := AddField(AVarName,'INTEGER','',APersistent);
  Result.EditContent(AValue,StoreAsUnsigned);
end;

function TASN1Struct.TrySelectChoice(ACls: Byte; AConstructed: Boolean;
  ATag: Cardinal): Boolean;
var
  Id: Integer;
  P: PASN1Struct;
  C: TASN1Struct;
begin
  Result := (Cls = ACls) and (Tag = ATag) and (Constructed = AConstructed);
  if not Result then begin
    for Id := 0 to ChoiceCount - 1 do begin
      P := Choices[Id];
      if (P^.ChoiceCount > 0) and (P^.FRefCount > 1) then begin
        Result := (P^.Cls = ACls) and
                  (P^.Tag = ATag) and
                  (P^.Constructed = AConstructed);
        if Result then begin
          C := TASN1Struct.Create;
          C._AddRef;
          C.CopyTypeInfo(P^);
          P^._Release;
          P^ := C;
        end;
      end else
        Result := P^.TrySelectChoice(ACls,AConstructed,ATag);
      if Result then begin
        SelectChoice(Id);
        Exit;
      end;
    end;
    Result := (TypeIdentified or (ChoiceCount = 0)) and
              (Cls = V_ASN1_UNIVERSAL) and (Tag = Cardinal(V_ASN1_UNDEF));
    if Result then begin
      FTag := ATag;
      FCls := ACls;
      FConstructed := AConstructed;
    end;
  end;
end;

procedure TASN1Struct.DeleteChoice(Index: Integer);
begin
  Assert((index >= 0) and (index < ChoiceCount));
  FChoices^.Items[index]._Release;
  if index < ChoiceCount - 1 then
    Move(FChoices^.Items[index + 1],FChoices^.Items[index],(ChoiceCount - index - 1)*4);
  FChoices^.Items[ChoiceCount - 1] := nil;
  FChoices^.ItemCount := ChoiceCount - 1;
  Change(ctDecl);
end;

function TASN1Struct.BeginUpdate: Integer;
begin
  Inc(FUpdateCount);
  Result := FUpdateCount;
end;

function TASN1Struct.EndUpdate: Integer;
begin
  Dec(FUpdateCount);
  Result := FUpdateCount;
  if Result = 0 then
    DoChange;
end;

procedure TASN1Struct.Change(ChangeType: TChangeType);
begin
  if FNoChange then Exit;
  if (ChangeType in [ctDelRec,ctNewRec]) and (FTemplate = nil) then
    ChangeType := ctDecl
  else if (ChangeType = ctCnt) and Constructed and (FTemplate = nil) then
    ChangeType := ctDecl;
  Include(FChanges,ChangeType);
  if FUpdateCount = 0 then
    DoChange;
end;

procedure TASN1Struct.DoChange;
begin
  if FChanges <> [] then begin
    ItemChanged(Self,FChanges);
    FChanges := [];
  end;
end;

procedure TASN1Struct.ItemChanged(Field: TASN1Struct;
  ChangeTypes: TChangeTypes);
begin
  if Assigned(FOnChange) then
    FOnChange(Self,Field,ChangeTypes)    
  {$IFNDEF THIN_TASN1STRUCT}
  else if Assigned(FOwner) then
    FOwner.ItemChanged(Field,ChangeTypes);
  {$ENDIF}
end;

procedure TASN1Struct.SetOnChange(const Value: TASN1ChangeEvent);
begin
  FOnChange := Value;
end;

function TASN1Struct.IsParent(const Struct: TASN1Struct): Boolean;
begin
  Result := Assigned(Owner);
  if Result then
    Result := (Owner = Struct) or Owner.IsParent(Struct);
end;

function TASN1Struct.IsSubItem(const Struct: TASN1Struct): Boolean;
begin
  Result := Struct.IsParent(Self);
end;

procedure TASN1Struct.BrowseFirst;
begin
  InternalBrowse(0);
end;

procedure TASN1Struct.BrowseLast;
begin
  InternalBrowse(ItemCount);
end;

procedure TASN1Struct.BrowseNext;
begin
  InternalBrowse(FCurrentRecNo + 1);
end;

procedure TASN1Struct.BrowsePrior;
begin
  InternalBrowse(FCurrentRecNo - 1);
end;

procedure TASN1Struct.InternalBrowse(ARecNo: Integer; Silent: Boolean);
begin
  Assert(Constructed and Assigned(FTemplate));
  PostItem;
  if ARecNo > ItemCount then
    ARecNo := ItemCount
  else if ARecNo < 0 then
    ARecNo := 0;
  FCurrentRecNo := ARecNo;
  ParentBrowseHereSilently;
  if Silent then Exit;
  if FCurrentRecNo < ItemCount then
    Items[FCurrentRecNo]^.Change(ctBrowse)
  else
    Change(ctBrowse);
end;

function TASN1Struct.GetCurrentRecord: PASN1Struct;
begin
  if Assigned(FCurrentRecord) then
    Result := @FCurrentRecord
  else if FCurrentRecNo >= ItemCount then
    Result := nil
  else
    Result := Items[FCurrentRecNo];
end;

procedure TASN1Struct.DeleteItem;
begin
  Assert(Constructed and Assigned(FTemplate));
  ParentBrowseHere;
  if FInserting then
    CancelItem
  else if FCurrentRecNo < ItemCount then
    DeleteItem(FCurrentRecNo);
end;

procedure TASN1Struct.EditItem;
var
  F: TASN1Struct;
begin
  if Constructed and Assigned(FTemplate) then begin
    if GetCurrentRecord = nil then
      InsertItem
    else if FCurrentRecord = nil then begin
      F := TASN1Struct.Create;
      F.Assign(GetCurrentRecord^);
      F._AddRef;
      FCurrentRecord := F;
      FEditing := True;
      ParentBrowseHere;
    end;
  end else begin
    F := FindBrowseableParent;
    Assert(Assigned(F));
    F.EditItem;
  end;
end;

procedure TASN1Struct.InsertItem;
begin
  Assert(Constructed and Assigned(FTemplate));
  if FCurrentRecord = nil then begin
    FCurrentRecord := TASN1Struct.Create;
    FCurrentRecord.CopyTypeInfo(FTemplate);
    FCurrentRecord._AddRef;
    FInserting := True;
    ParentBrowseHere;
  end;
end;

procedure TASN1Struct.PostItem;
var
  OldItemCount, I: Integer;
  F: TASN1Struct;
begin
  if Assigned(FCurrentRecord) then begin
    BeginUpdate;
    try
      if FInserting then begin
        OldItemCount := ItemCount;
        if OldItemCount < 0 then
          OldItemCount := 0;
        if OldItemCount = FCapacity then
          SetCapacity(OldItemCount + 4);
        for I := OldItemCount-1 downto FCurrentRecNo do
          Contents^.Items[I+1] := Contents^.Items[I];
        FCurrentRecord.FOwner := Self;
        Contents^.Items[FCurrentRecNo] := FCurrentRecord;
        Contents^.ItemCount := OldItemCount + 1;
        Change(ctNewRec);
      end else if FEditing then begin
        Items[FCurrentRecNo]^.Assign(FCurrentRecord);
        FCurrentRecord._Release;
        Change(ctCnt);
      end else
        FCurrentRecord._Release;
      FCurrentRecord := nil;
      FInserting := False;
      FEditing := False;
    finally
      EndUpdate;
    end;
  end else begin
    F := FindBrowseableParent;
    if Assigned(F) then
      F.PostItem;
  end;
end;

procedure TASN1Struct.CancelItem;
begin
  if Assigned(FCurrentRecord) then
    ReleaseASN1Struct(FCurrentRecord,Self);
  FEditing := False;
  FInserting := False;
end;

function TASN1Struct.GetCurrentRecNo: Integer;
begin
  Result := ItemCount;
  if Result > FCurrentRecNo then
    Result := FCurrentRecNo
  else
    FCurrentRecNo := Result;
end;

function TASN1Struct.ListBrowseableFields(List: TStrings;
  BrowsePath, FullPath, Recurse: Boolean): Integer;
var
  I: Integer;
  F: PASN1Struct;
begin
  Result := ItemCount;
  for I := 0 to Result - 1 do begin
    F := Items[I];
    if Assigned(F^.FTemplate) then begin
      if BrowsePath then
        List.AddObject(F^.GetNamePath(True),F^)
      else if FullPath then
        List.AddObject(F^.GetNamePath,F^)
      else
        List.AddObject(F^.VarName,F^);
    end;
    if Recurse and F^.Constructed then
      Result := Result + F^.ListBrowseableFields(List,BrowsePath,FullPath,True);
  end;

end;

function TASN1Struct.ListFields(List: TStrings; BrowsePath, FullPath,
  Recurse: Boolean): Integer;
var
  I: Integer;
  SL: TStringList;
  Pfx: string;
begin
  Result := 1;
  if (FOwner = nil) and (FVarName = '') then
    List.AddObject('(root)',Self)
  else if BrowsePath then
    List.AddObject(GetNamePath(True),Self)
  else if FullPath then
    List.AddObject(GetNamePath,Self)
  else
    List.AddObject(VarName,Self);
  if Constructed and Recurse then begin
    if BrowsePath and Assigned(FTemplate) then begin
      SL := TStringList.Create;
      try
        FTemplate.FVarName := '';
        FTemplate.ListFields(SL,True,FullPath,True);
        Pfx := GetNamePath(True);
        if Pfx = '' then
          Pfx := '/[c]/'
        else if Pfx[1] = '/' then
          Pfx := Pfx + '/[c]/'
        else
          Pfx := '/' + Pfx + '/[c]/';
        SL[0] := Pfx;
        for I := 1 to SL.Count - 1 do
          SL[I] := Pfx + SL[I];
        List.AddStrings(SL);
      finally
        SL.Free;
      end;
    end else
      for I := 0 to ItemCount - 1 do
        Result := Result + Items[I].ListFields(List,BrowsePath,FullPath,True);
  end;
end;

function TASN1Struct.ListObjectFields(List: TStrings; MaxIndex: Integer;
  BrowsePath, FullPath: Boolean): Integer;
var
  I: Integer;
  F: PASN1Struct;
  vTag: Cardinal;
begin
  Result := 0;
  if MaxIndex >= ItemCount then
    MaxIndex := ItemCount - 1;
  for I := 0 to MaxIndex do begin
    F := Items[I];
    if F^.Implicit then
      vTag := F^.ImplicitTag
    else
      vTag := F^.Tag;
    if vTag = V_ASN1_OBJECT then begin
      Inc(Result);
      if BrowsePath then
        List.AddObject(F^.GetNamePath(True),F^)
      else if FullPath then
        List.AddObject(F^.GetNamePath,F^)
      else
        List.AddObject(F^.VarName,F^);
    end;
  end;
end;

function TASN1Struct.ListPrimitiveFields(List: TStrings; BrowsePath, FullPath,
  Recurse: Boolean): Integer;
var
  I: Integer;
  F: PASN1Struct;
begin
  Result := ItemCount;
  for I := 0 to Result - 1 do begin
    F := Items[I];
    if not F^.Constructed then begin
      if BrowsePath then
        List.AddObject(F^.GetNamePath(True),F^)
      else if FullPath then
        List.AddObject(F^.GetNamePath,F^)
      else
        List.AddObject(F^.VarName,F^);
    end;
    if Recurse and F^.Constructed then
      Result := Result + F^.ListPrimitiveFields(List,BrowsePath,FullPath,True);
  end;
end;

function TASN1Struct.ListTypedFields(List: TStrings; TypeName: string;
  BrowsePath, FullPath, Recurse: Boolean): Integer;
var
  I: Integer;
  F: PASN1Struct;
begin
  Result := ItemCount;
  for I := 0 to Result - 1 do begin
    F := Items[I];
    if SameText(F^.TypeName,TypeName) or
       SameText(F^.ImplicitTypeName,TypeName) then begin
      if BrowsePath then
        List.AddObject(F^.GetNamePath(True),F^)
      else if FullPath then
        List.AddObject(F^.GetNamePath,F^)
      else
        List.AddObject(F^.VarName,F^);
    end;
    if Recurse and F^.Constructed then
      Result := Result + F^.ListTypedFields(List,TypeName,BrowsePath,FullPath,True);
  end;
end;

function TASN1Struct.ListTypedFields(List: TStrings;
  UniversalTag: Cardinal; BrowsePath, FullPath, Recurse: Boolean): Integer;
var
  I: Integer;
  F: PASN1Struct;
  vTag: Cardinal;
begin
  Result := ItemCount;
  for I := 0 to Result - 1 do begin
    F := Items[I];
    if F^.Implicit then
      vTag := F^.ImplicitTag
    else
      vTag := F^.Tag;
    if vTag = UniversalTag then begin
      if BrowsePath then
        List.AddObject(F^.GetNamePath(True),F^)
      else if FullPath then
        List.AddObject(F^.GetNamePath,F^)
      else
        List.AddObject(F^.VarName,F^);
    end;
    if Recurse and F^.Constructed then
      Result := Result + F^.ListTypedFields(List,UniversalTag,BrowsePath,FullPath,True);
  end;
end;

function TASN1Struct.ListConstructedFields(List: TStrings; BrowsePath, FullPath,
  Recurse: Boolean): Integer;
var
  I: Integer;
  F: PASN1Struct;
begin
  Result := ItemCount;
  for I := 0 to Result - 1 do begin
    F := Items[I];
    if F^.Constructed then begin
      if BrowsePath then
        List.AddObject(F^.GetNamePath(True),F^)
      else if FullPath then
        List.AddObject(F^.GetNamePath,F^)
      else
        List.AddObject(F^.VarName,F^);
    end;
    if Recurse and F^.Constructed then
      Result := Result + F^.ListConstructedFields(List,BrowsePath,FullPath,True);
  end;
end;

function TASN1Struct.FindBrowseableParent: TASN1Struct;
begin
  Result := Owner;
  if Result = nil then Exit;
  if not Assigned(Result.FTemplate) then
    Result := Result.FindBrowseableParent;
end;

procedure TASN1Struct.ParentBrowseHere;
begin
  if Owner = nil then
    Exit;
  if Assigned(Owner.FTemplate) then
    Owner.InternalBrowse(Owner.IndexOf(Self))
  else
    Owner.ParentBrowseHere;
end;

function TASN1Struct.GetIdentifiedBy: string;
begin
  Result := InterpretBERtoOID(FBERIdentifiedBy);
end;

procedure TASN1Struct.SetIdentifiedBy(const Value: string);
begin
  FBERIdentifiedBy := InterpretOIDtoBER(Value);
end;

constructor TASN1Struct.Create;
begin
  if CollectingGarbage then Abort;
end;

procedure TASN1Struct.SetModuleName(const Value: string);
begin
  FModuleName := Value;
end;

procedure TASN1Struct.InsertItemEx(Item: TASN1Struct; Index: Integer);
var
  OldItemCount, I: Integer;
  F: TASN1Struct;
begin
  if IsParent(Item) or (Item = Self) then
    raise Exception.Create('TASN1Struct.InsertItemEx: Circular reference');
  OldItemCount := ItemCount;
  if OldItemCount < 0 then
    OldItemCount := 0;
  AllocContents(OldItemCount + 1);
  if Index > OldItemCount then
    Index := OldItemCount;
  if Assigned(Item.Owner) and (Item.Owner <> Self) then begin
    F := Contents^.Items[OldItemCount];
    for I := OldItemCount-1 downto Index do
      Contents^.Items[I+1] := Contents^.Items[I];
    Contents^.Items[Index] := F;
    F.Assign(Item);
    Item.FOwner := Self;
  end else begin
    Contents^.Items[OldItemCount]._Release;
    for I := OldItemCount-1 downto Index do
      Contents^.Items[I+1] := Contents^.Items[I];
    Contents^.Items[Index] := Item;
    Item._AddRef;
    Item.FOwner := Self;
  end;
end;

procedure TASN1Struct.RemoveItem(Item: TASN1Struct; Garbage: THashList);
var
  I: Integer;
begin
  I := IndexOf(Item);
  if I >= 0 then
    DeleteItem(I,Garbage);
end;

function TASN1Struct.ContentAsVariant: Variant;
var
  S: string;
  D: TDateTime;
  vTag: Cardinal;
begin
  Assert(not Constructed);
  System.SetLength(S,Length);
  Move(Content^,S[1],Length);
  if System.Length(S) > 0 then begin
    if (Cls = V_ASN1_UNIVERSAL) or Implicit then begin
      if Implicit then
        vTag := ImplicitTag
      else
        vTag := Tag;
      {$IFDEF D6UP}
      if (vTag = V_ASN1_INTEGER) and (Length <= 8) then
        Result := ContentAsInt64
      {$ELSE}
      if (vTag = V_ASN1_INTEGER) and (Length <= 4) then
        Result := ContentAsInteger
      {$ENDIF}
      else if vTag in [V_ASN1_INTEGER,V_ASN1_OCTET_STRING,V_ASN1_ENUMERATED] then
        Result := OSToHex(S)
      else if vTag = V_ASN1_BIT_STRING then
        Result := Format('(%d unused bits) ',[Byte(S[1])]) + OSToHex(Copy(S,2,MaxInt))
      else if vTag = V_ASN1_BOOLEAN then begin
        if S = #$FF then
          Result := True
        else if S = #0 then
          Result := False
        else
          Result := NULL
      end else if vTag = V_ASN1_BMPSTRING then
        Result := BMPtoUnicode(PChar(S),Length)
      else if vTag = V_ASN1_UTF8STRING then
        Result := UTF8toUnicode(S)
      else if vTag in [V_ASN1_PRINTABLESTRING,
                       V_ASN1_VISIBLESTRING,
                       V_ASN1_NUMERICSTRING,
                       V_ASN1_IA5STRING,
                       V_ASN1_TELETEXSTRING,
                       V_ASN1_VIDEOTEXSTRING,
                       V_ASN1_GENERALSTRING,
                       V_ASN1_UNIVERSALSTRING] then
        Result := S
      else if vTag = V_ASN1_OBJECT then
        Result := GetObjectName(InterpretBERtoOID(S))
      else if vTag = V_ASN1_UTCTIME then begin
        if UTCTimeToDateTime(Pchar(S),Length,D) then
          Result := D
        else
          Result := NULL;
      end else if vTag = V_ASN1_GENERALIZEDTIME then begin
        if GeneralizedTimeToDateTime(Pchar(S),Length,D) then
          Result := D
        else
          Result := NULL;
      end else if vTag = V_ASN1_REAL then
        Result := InterpretBERToReal(S)
      else
        Result := NULL;
    end else
      Result := NULL;
  end else
    Result := NULL;
end;

procedure TASN1Struct.RemoveItemRecurse(Item: TASN1Struct);
var
  I: Integer;
begin
  if Item = Self then Exit;
  if Item = FTemplate then begin
    FTemplate._Release;
    FTemplate := nil;
  end;
  RemoveItem(Item);
  for I := 0 to ItemCount - 1 do
    if Items[I]^.Constructed then
      Items[I]^.RemoveItemRecurse(Item);
end;

procedure TASN1Struct.ParentBrowseHereSilently;
begin
  if Owner = nil then
    Exit;
  if Assigned(Owner.FTemplate) then
    Owner.InternalBrowse(Owner.IndexOf(Self),True)
  else
    Owner.ParentBrowseHereSilently;
end;

procedure TASN1Struct.ImposeTypeInfo(const Source: TASN1Struct;
  Flat: Boolean);
var
  ID: Integer;
  SaveReadOnly: Boolean;
begin
  if Source = Self then Exit;
  if Source.FTag = Cardinal(V_ASN1_UNDEF) then begin
//    Assert(False,Format('The type "%s" is not defined',[Source.FTypeName]));
    Exit;
  end;
  BeginUpdate;
  try
    FCls := Source.FCls;
    FTag := Source.FTag;
    if FConstructed xor Source.FConstructed then begin
      DisposeContent;
      FConstructed := Source.FConstructed;
    end;
    if FTemplate <> Source.FTemplate then begin
      if Assigned(FTemplate) then
        FTemplate._Release;
      FTemplate := Source.FTemplate;
      if Assigned(FTemplate) and not IsParent(FTemplate) then
        FTemplate._AddRef;
    end;
    if not IsChoiceTemplate(Source) then begin
      ChoiceCount := Source.ChoiceCount;
      for ID := 0 to ChoiceCount - 1 do
        Choices[ID] := Source.Choices[ID];
      FChoiceTypeName := Source.ChoiceTypeName;
      FChoiceVarName := Source.ChoiceVarName;
      FBERIdentifiedBy := Source.FBERIdentifiedBy;
      if Source.FIDField <> '' then
        IDField := Source.IDField
      else
        FIDField := '';
      FIdentifies := Source.FIdentifies;
      FTypeIdentified := Source.TypeIdentified;
      FModuleName := Source.FModuleName;
    end;
    Default := Source.Default;
    FHasDefaultValue := Source.FHasDefaultValue;
    FVarName := Source.FVarName;
    if Source.FTypeName <> '' then
      FTypeName := Source.FTypeName;
    FImplicit := Source.FImplicit;
    FImplicitTag := Source.FImplicitTag;
    FImplicitTypeName := Source.FImplicitTypeName;
    FOptional := Source.FOptional;
    FPersistent := True;
    if Constructed and not Flat then begin
      SaveReadOnly := FReadOnly;
      FReadOnly := False;
      try
        if Assigned(FTemplate) then begin
          for Id := 0 to ItemCount - 1 do
            Items[Id].ImposeTypeInfo(FTemplate);
        end else if (Source.ItemCount <= 0) then
          AllocContents(0)
        else begin
          AllocContents(Source.ItemCount);
          for Id := 0 to Source.ItemCount - 1 do
            Items[Id].ImposeTypeInfo(Source.Items[Id]^);
        end;
      finally
        FReadOnly := SaveReadOnly;
      end;
    end;
    Change(ctDecl);
  finally
    EndUpdate;
  end;
end;

procedure TASN1Struct.SetNoChange(const Value: Boolean);
var
  Idx: Integer;
begin
  if FNoChange = Value then Exit;
  FNoChange := Value;
  {$IFNDEF THIN_TASN1STRUCT}
  if Constructed then begin
    for Idx := 0 to ItemCount - 1 do
      Contents^.Items[Idx].SetNoChange(Value);
    if Assigned(FTemplate) then
      FTemplate.SetNoChange(Value);
  end;
  for Idx := 0 to ChoiceCount - 1 do
    Choices[Idx]^.SetNoChange(Value);
  {$ENDIF}
end;

procedure TASN1Struct.SetReadOnly(const Value: Boolean);
var
  Idx, OldLength: Integer;
  OldCnt: Pointer;
begin
  if FReadOnly xor Value then begin
    FReadOnly := Value;
    if (not Value) and Assigned(Owner) then
      Owner.DoModifiedChild;
    if Assigned(FEncapsulated) then
      FEncapsulated.ReadOnly := Value;
    if Constructed then begin
      for Idx := 0 to ItemCount - 1 do
        Contents^.Items[Idx].SetReadOnly(Value);
      if not Value then begin
        ReallocMem(FReadOnlyCnt,0);
        FReadOnlyCntSlave := nil;
        FReadOnlyCntLenSlave := 0;
      end;
    end else begin
      if not (Value or CaptureContent) then begin
        OldCnt := Cnt;
        OldLength := FLength;
        Cnt := nil;
        FLength := 0;
        SetContent(OldCnt^,OldLength);
      end else if Value then
        SetToDefault;
    end;
  end;
end;

function TASN1Struct.FindIDField: PASN1Struct;
begin
  if Assigned(FIDFieldStruct) then
    Result := @FIDFieldStruct
  else if (FIDField = '') and
          Assigned(FTemplate) and
          FTemplate.TypeIdentified then begin
    if (Owner.Cls <> V_ASN1_UNIVERSAL) and not Owner.Implicit then
      Result := Owner.Owner.FindField(FTemplate.FIDField)
    else
      Result := Owner.FindField(FTemplate.FIDField);
    if Assigned(Result) then
      FTemplate.FIDFieldStruct := Result^
    else
      FTemplate.FIDFieldStruct := nil;
  end else begin
    Assert(TypeIdentified);
    if Owner = nil then
      Result := nil
    else if (Owner.Cls <> V_ASN1_UNIVERSAL) and not Owner.Implicit then
      Result := Owner.Owner.FindField(FIDField)
    else begin
      Result := Owner.FindField(FIDField);
      if (Result = nil) and
         Assigned(Owner.FOwner) and Assigned(Owner.FTemplate) then
        Result := Owner.Owner.FindField(FIDField);
    end;
  end;
  if Assigned(Result) then begin
    FIDFieldStruct := Result^;
    if Assigned(FTemplate) and FTemplate.FTypeIdentified then
      FTemplate.FIDFieldStruct := Result^;
  end else
    FIDFieldStruct := nil;
end;

function TASN1Struct.ContentAsSystemTime(var T: TSystemTime): Boolean;
begin
  if ((Tag = V_ASN1_UTCTIME) or
      (Cls <> V_ASN1_UNIVERSAL)) and
     not Constructed then begin
    Result := UTCTimeToSystemTime(Content,Length,T);
  end else if ((Tag = V_ASN1_GENERALIZEDTIME) or
               (Cls <> V_ASN1_UNIVERSAL)) and
              not Constructed then begin
    Result := GeneralizedTimeToSystemTime(Content,Length,T);
  end else
    Result := False;
end;

procedure TASN1Struct.EditContent(const AValue: TSystemTime);
var
  Str: string;
begin
  if ((Tag = V_ASN1_UTCTIME) or
      (Implicit and (ImplicitTag = V_ASN1_UTCTIME))) and
     not Constructed then begin
    Str := SystemTimeToUTCTime(AValue);
    SetContent(Str[1],System.Length(Str));
  end else if ((Tag = V_ASN1_GENERALIZEDTIME) or
               (Implicit and (ImplicitTag = V_ASN1_GENERALIZEDTIME))) and
              not Constructed then begin
    Str := SystemTimeToGeneralizedTime(AValue);
    SetContent(Str[1],System.Length(Str));
  end else
    raise Exception.Create('Could not interpret contents');
end;

function TASN1Struct.GetActualTag: Cardinal;
begin
  if Implicit then
    Result := FImplicitTag
  else
    Result := FTag;
end;

function TASN1Struct.GetFields(const FieldName: string): PASN1Struct;
begin
  Result := FindField(FieldName);
  if Result = nil then
    raise Exception.Create(Format('Field %s not found',[FieldName]));
end;

procedure TASN1Struct.SetFields(const FieldName: string;
  const Value: PASN1Struct);
var
  F: PASN1Struct;
begin
  if Value = nil then
    raise Exception.Create('Cannot assign a nil pointer to a TASN1Struct');
  F := FindField(FieldName);
  if F = nil then
    raise Exception.Create(Format('Field %s not found',[FieldName]));
  if F <> Value then
    F^.Assign(Value^)
end;

procedure TASN1Struct.SetToDefault;
var
  SS: TStringStream;
  P: Boolean;
begin
  if HasDefaultValue then begin
    P := Persistent;
    try
      Persistent := True;
      SS := TStringStream.Create(Default);
      try
        ASN1FromStream(SS,Self)
      finally
        SS.Free;
      end;
    finally
      Persistent := P;
    end;
  end else
    DisposeContent;
end;

function TASN1Struct.GetEncapsulated: TASN1Struct;
begin
  Result := FEncapsulated;
end;

procedure TASN1Struct.EditContent(const AValue: TASN1Struct);
var
  MS: TSecureMemoryStream;
  P: PChar;
begin
  if Constructed then
    Assign(AValue)
  else if AValue = nil then
    SetLength(0)
  else begin
    MS := TSecureMemoryStream.Create;
    try
      AValue.CalculateLength;
      AValue.SaveToStream(MS,fmtDER);
      if Assigned(FEncapsulated) and (AValue <> FEncapsulated) then begin
        MS.Position := 0;
        FEncapsulated.LoadFromStream(MS,fmtDER);
      end;
      if ActualTag = V_ASN1_BIT_STRING then begin
        P := StrAlloc(MS.Size + 1);
        try
          P[0] := #0;
          MS.Position := 0;
          MS.Read(P[1],MS.Size);
          SetContent(P[0],MS.Size + 1);
        finally
          ProtectClear(P[0],MS.Size + 1);
          StrDispose(P);
        end;
      end else
        SetContent(MS.Memory^,MS.Size);
    finally
      MS.Free;
    end;
  end;
end;

procedure TASN1Struct.EditContent(const AValue: TASN1Struct; PadAlign: Byte);
var
  MS: TSecureMemoryStream;
  P: PChar;
  Len: Integer;
  Pad: Byte;
begin
  if Constructed then
    Assign(AValue)
  else if AValue = nil then
    SetLength(0)
  else begin
    MS := TSecureMemoryStream.Create;
    try
      AValue.CalculateLength;
      AValue.SaveToStream(MS,fmtDER);
      if Assigned(FEncapsulated) and (AValue <> FEncapsulated) then begin
        MS.Position := 0;
        FEncapsulated.LoadFromStream(MS,fmtDER);
      end;
      if ActualTag = V_ASN1_BIT_STRING then begin
        Len := MS.Size + 1;
        Pad := PadAlign - (Len mod PadAlign);
        Len := Len + Pad;
        P := StrAlloc(Len);
        try
          FillChar(P[MS.Size + 1],Pad,Pad);
          P[0] := #0;
          MS.Position := 0;
          MS.Read(P[1],MS.Size);
          SetContent(P[0],Len);
        finally
          ProtectClear(P[0],Len);
          StrDispose(P);
        end;
      end else begin
        Len := MS.Size;
        Pad := PadAlign - (Len mod PadAlign);
        Len := Len + Pad;
        while MS.Size < Len do
          MS.Write(Pad,1);
        SetContent(MS.Memory^,Len);
      end;
    finally
      MS.Free;
    end;
  end;
end;

function TASN1Struct.EditField(AVarName: string;
  const AValue: TASN1Struct): Boolean;
var
  F: PASN1Struct;
begin
  F := FindField(AVarName);
  Result := Assigned(F);
  if Assigned(F) then
    F.EditContent(AValue);
end;

function TASN1Struct.GetTypeIdentified: Boolean;
begin
  Result := FTypeIdentified;
  if Assigned(FTemplate) and not Result then
    Result := FTemplate.TypeIdentified;
end;

function TASN1Struct.GetIDField: string;
begin
  if not FTypeIdentified and Assigned(FTemplate) then
    Result := FTemplate.FIDField
  else
    Result := FIDField;
end;

procedure TASN1Struct.FreeInstance;
begin
  CleanUpInstance;
  DeallocInstance(Self);
end;

class function TASN1Struct.NewInstance: TObject;
begin
  Result := AllocInstance;
end;

procedure TASN1Struct.CopyTypeInfoAsField(Struct: TASN1Struct);
var
  vCls: Byte;
  vTag, vImplicitTag: Cardinal;
  vImplicit: Boolean;
  vVarName, vIdentifiedBy, vIDField, vTypeName, vImplicitTypeName: string;
  vOptional: Boolean;
  vDefault: string;
begin
  vVarName := VarName;
  vOptional := Optional;
  vDefault := Default;
  vImplicit := Implicit;
  if vImplicit then begin
    vCls := Cls;
    vTag := Tag;
    vTypeName := TypeName;
    if Struct.Implicit then begin
      vImplicitTag := Struct.ImplicitTag;
      vImplicitTypeName := Struct.ImplicitTypeName;
    end else begin
      vImplicitTag := Struct.Tag;
      vImplicitTypeName := Struct.TypeName;
    end;
  end else begin
    vCls := V_ASN1_UNIVERSAL;
    vTag := Struct.ActualTag;
    if Struct.Implicit then
      vTypeName := Struct.ImplicitTypeName
    else
      vTypeName := Struct.TypeName;
    vImplicitTag := 0;
    vImplicitTypeName := '';
  end;
  vIdentifiedBy := FBERIdentifiedBy;
  vIDField := FIDField;

  CopyTypeInfo(Struct);

  FVarName := vVarName;
  FOptional := vOptional;
  FDefault := vDefault;
  FImplicit := vImplicit;
  FCls := vCls;
  FTag := vTag;
  FTypeName := vTypeName;
  FImplicitTag := vImplicitTag;
  FImplicitTypeName := vImplicitTypeName;
  FIDField := vIDField;
  FBERIdentifiedBy := vIdentifiedBy;
end;

procedure TASN1Struct.Dispose(Garbage: THashList);
var
  HandleGarbage: Boolean;
begin
  if FDestroying then Exit;
  FDestroying := True;
  HandleGarbage := Garbage = nil;
  if HandleGarbage then begin
    if not GarbageFreeNotification(Self) then
      Exit;
    Garbage := THashList.Create
  end else
    Garbage.Add(Self);
  try
    CancelItem;
    if Assigned(FTemplate) then
      ReleaseASN1Struct(FTemplate,Self,Garbage);
    if Assigned(FEncapsulated) then
      ReleaseASN1Struct(FEncapsulated,Self,Garbage);
    DisposeContent(Garbage);
    if Assigned(FChoices) then
      SetChoiceCountEx(0,Garbage);
    ReallocMem(FChoices,0);
    FResumeStream.Free;
    FResumeStream := nil;
    FIdentifies.Free;
    FIdentifies := nil;
    Assert(FReadOnlyCnt = nil);
    if HandleGarbage then
      DeallocInstances(Garbage);
  finally
    if HandleGarbage then
      Garbage.Free;
  end;
end;

procedure TASN1Struct.PropagateChoices;
var
  I: Integer;
begin
  if ChoiceCount > 0 then begin
    if Template = nil then
      Template := Choices[FChoiceIndex]^
    else begin
      Template.FChoiceIndex := FChoiceIndex;
      Template := Choices[FChoiceIndex]^;
    end;
    if Template.ChoiceCount > 0 then begin
      if not Template.TrySelectChoice(Cls,Constructed,Tag) then
        Exit;
      Template := Template.Choices[Template.FChoiceIndex]^
    end;
  end;
  if Constructed then
    for I := 0 to ItemCount - 1 do
      Items[I]^.PropagateChoices(Template.Items[I]^);
end;

function TASN1Struct.TotalCount: Integer;
begin
  Result := FEstTotalCount;
end;

procedure TASN1Struct.DoModifiedChild;
begin
  FModifiedChild := True;
  if Assigned(Owner) then
    Owner.DoModifiedChild;
end;

procedure TASN1Struct.SetForgetOptionalDefault(const Value: Boolean);
var
  I: Integer;
begin
  FForgetOptionalDefault := Value;
  if Constructed then
    for I := 0 to ItemCount - 1 do
      Items[I]^.SetForgetOptionalDefault(Value);
end;

function TASN1Struct.LoadFromFile(AFileName: string;
  Format: Byte): Integer;
var
  FS: TFileStream;
begin
  if not FileExists(AFileName) then
    Result := 0
  else begin
    FS := TFileStream.Create(AFileName,fmOpenRead);
    try
      Result := LoadFromStream(FS,Format);
    finally
      FS.Free;
    end;
  end;
end;

function TASN1Struct.SaveToFile(AFileName: string; Format: Byte;
  DeclaredTypes: TStrings): Integer;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName,fmCreate);
  try
    Result := SaveToStream(FS,Format,DeclaredTypes);
  finally
    FS.Free;
  end;
end;

{ TASNCustomWrapper }

procedure TASNCustomWrapper.AssignStruct(Struct: TASN1Struct);
var
  MS: TSecureMemoryStream;
begin
  MS := TSecureMemoryStream.Create;
  try
    LockInstanceList;
    try
      PreAllocInstances(Struct.FEstTotalCount);
      Struct.SaveToStream(MS,fmtDER);
      MS.Position := 0;
      LoadFromStream(MS);
    finally
      UnlockInstanceList;
    end;
  finally
    MS.Free;
  end;
end;

procedure TASNCustomWrapper.AssignTo(Dest: TPersistent);
var
  MS: TSecureMemoryStream;
begin
  if Dest is TASNCustomWrapper then begin
    MS := TSecureMemoryStream.Create;
    try
      SaveToStream(MS);
      MS.Position := 0;
      TASNCustomWrapper(Dest).LoadFromStream(MS);
    finally
      MS.Free;
    end;
  end else
    inherited;
end;

procedure TASNCustomWrapper.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TASNCustomWrapper.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    TASNCustomWrapper(FItems[I]).Clear
end;

constructor TASNCustomWrapper.Create;
begin
  FItems := TList.Create;
  if Assigned(AData) then begin
    FData := AData;
    if Assigned(FData.FEncapsulated) then try
      FData.FEncapsulated.NoChangeEvent := True;
      FData.ContentAsASN1Struct(FData.FEncapsulated);
    except
      if Assigned(FData.FEncapsulated) then begin   
        ReleaseASN1Struct(FData.FEncapsulated,FData);
      end;
    end;
  end else begin
    FData := TASN1Struct.Create;
    FData.ReadOnly := True;
  end;
  FData.NoChangeEvent := True;
  FData._AddRef;
  FOwner := AOwner;
  FTemplate := ATemplate;
end;

destructor TASNCustomWrapper.Destroy;
begin
  if Assigned(FItems) then DestroyItems;
  FItems.Free;
  if Assigned(FData) then FData._Release;
  inherited;
end;

procedure TASNCustomWrapper.DestroyItems;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    TASNCustomFieldWrapper(FItems[I]).Free;
  FItems.Clear;
end;

procedure TASNCustomWrapper.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount < 0 then FUpdateCount := 0;
  if FUpdateCount = 0 then
    UpdateContents;
end;

procedure TASNCustomWrapper.FieldFactory(AWrapperClass: TASNWrapperClass;
                                         ATemplate: TASN1Struct;
                                         AStruct: TASN1Struct;
                                         var AReference);
var
  F: TASNCustomFieldWrapper;
begin
  if AWrapperClass = nil then Exit;
  F := AWrapperClass.Create(Self,AStruct,ATemplate);
  F.OwnerEncapsulated := FOwnerEncapsulated or Assigned(FData.FEncapsulated);
  FItems.Add(F);
  if @AReference <> nil then
    TASNCustomFieldWrapper(AReference) := F;
end;

function TASNCustomWrapper.GetNamePath: string;
begin
  Result := VarName;
  if FOwner <> nil then
    Result := FOwner.GetNamePath + '.' + Result;
end;

function TASNCustomWrapper.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TASNCustomWrapper.GetVarName: string;
begin
  Result := FData.VarName;
  if (Result = '') and (FData.Owner <> nil) then
    Result := Format('[%d]',[FData.FOwner.IndexOf(FData)]);
end;

function TASNCustomWrapper.GetWrapperOwner: TASNCustomWrapper;
begin
  if GetOwner is TASNCustomWrapper then
    Result := TASNCustomWrapper(GetOwner)
  else
    Result := nil;
end;

procedure TASNCustomWrapper.LoadFromFile(AFileName: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName,fmOpenRead);
  try
    LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TASNCustomWrapper.LoadFromStream(AStream: TStream);
begin
  FData.NoChangeEvent := True;
  FData.ForgetOptionalDefault := True;
  FData.ReadOnly := False;
  if Assigned(FData.FEncapsulated) then
    FData.Encapsulated.LoadFromStream(AStream,fmtDER)
  else
    FData.LoadFromStream(AStream,fmtDER);
  UpdateContents;
end;

function TASNCustomWrapper.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TASNCustomWrapper.SaveToFile(AFileName: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName,fmCreate);
  try
    SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TASNCustomWrapper.SaveToStream(AStream: TStream);
begin
  if Assigned(FData.FEncapsulated) then
    FData.Encapsulated.SaveToStream(AStream,fmtDER)
  else
    FData.SaveToStream(AStream,fmtDER);
end;

procedure TASNCustomWrapper.SetOwnerEncapsulated(const Value: Boolean);
var
  I: Integer;
  CFW: TASNCustomFieldWrapper;
begin
  FOwnerEncapsulated := Value;
  for I := 0 to FItems.Count - 1 do begin      
    CFW := TASNCustomFieldWrapper(FItems[I]);
    if CFW <> nil then
      CFW.OwnerEncapsulated := Value;
  end;
end;

procedure TASNCustomWrapper.Update;
var
  I: Integer;
  CFW: TASNCustomFieldWrapper;
begin
  if UpdateCount > 0 then Exit;
  if Assigned(FData.FEncapsulated) and not FOwnerEncapsulated then
    FData.ContentAsASN1Struct(FData.FEncapsulated);
  if FData.IsEmpty and FData.Optional then
    Clear
  else
    for I := 0 to FItems.Count - 1 do begin
      CFW := TASNCustomFieldWrapper(FItems[I]);
      if Assigned(CFW) then begin
        if Assigned(CFW.FData.Owner) and Assigned(CFW.FData.Owner.Owner) and
           (CFW.FData.Owner.Owner = FData) and
           CFW.FData.Owner.IsEmpty and CFW.FData.Owner.Optional then
          CFW.Clear
        else
          CFW.Update;
      end;
    end;
end;

procedure TASNCustomWrapper.UpdateContents;
begin
  if UpdateCount > 0 then Exit;
  if Assigned(FData.FEncapsulated) then begin
    FData.ReadOnly := False;                 
    FData.Encapsulated.ReadOnly := False;
    FData.Encapsulated.CalculateLength;
    FData.EditContent(FData.Encapsulated);
  end;
  if FOwnerEncapsulated then
    (FOwner as TASNCustomWrapper).UpdateContents
  else
    Update;
end;

function TASNCustomWrapper._AddRef: Integer;
begin
  Result := -1;
end;

function TASNCustomWrapper._Release: Integer;
begin
  Result := -1;
end;

{ TASNCustomFieldWrapper }

function TASNCustomFieldWrapper.DataLength: Integer;
begin
  Result := FData.Length;
end;

class function TASNCustomFieldWrapper.GetClassFactories: IASNClassFactories;
begin
  Result := nil;
end;

class function TASNCustomFieldWrapper.GetClassFactory: IASNClassFactory;
var
  Intf: IASNClassFactories;
begin
  Intf := GetClassFactories;
  if Assigned(Intf) then
    Result := Intf.GetClassFactory(Self)
  else
    Result := nil;
end;

function TASNCustomFieldWrapper.IsTemplateType: Boolean;
begin
  if Assigned(FTemplate) then begin
    if FData.ChoiceCount > 0 then
      Result := FData.ChoiceTypeName = FTemplate.TypeName
    else
      Result := FData.TypeName = FTemplate.TypeName
  end else
    Result := True;
end;

procedure TASNCustomFieldWrapper.Update;
begin
  if IsTemplateType then
    inherited Update;
end;

{ TASNCustomOFFieldWrapper }

procedure TASNCustomOFFieldWrapper.Clear;
begin
  DestroyItems;
  FData.ReadOnly := False;
  if Assigned(FData.FEncapsulated) then begin
    FData.Encapsulated.Clear;
    FData.EditContent(FData.Encapsulated);
  end else
    FData.Clear;
end;

constructor TASNCustomOFFieldWrapper.Create(AOwner: TPersistent;
  AData, ATemplate: TASN1Struct);
begin
  FCreating := True;
  inherited;
  if Assigned(AData) then
    Update;
  FCreating := False;
end;

procedure TASNCustomOFFieldWrapper.Delete(Index: Integer);
begin
  FData.ReadOnly := False;
  if Assigned(FData.FEncapsulated) then begin
    FData.FEncapsulated.DeleteItem(Index);
    FData.EditContent(FData.Encapsulated);
  end else
    FData.DeleteItem(Index);
  TASNCustomFieldWrapper(FItems[Index]).Free;
  FItems.Delete(Index);
end;

procedure TASNCustomOFFieldWrapper.Exchange(Index1, Index2: Integer);
var
  F: TASN1Struct;
begin
  if Index1 <> Index2 then begin
    FData.ReadOnly := False;
    FItems.Exchange(Index1,Index2);
    if Assigned(FData.FEncapsulated) then begin
      F := FData.Encapsulated.Contents^.Items[Index1];
      FData.Encapsulated.Contents^.Items[Index1] := FData.Encapsulated.Contents^.Items[Index2];
      FData.Encapsulated.Contents^.Items[Index2] := F;
      FData.EditContent(FData.Encapsulated);
    end else begin
      F := FData.Contents^.Items[Index1];
      FData.Contents^.Items[Index1] := FData.Contents^.Items[Index2];
      FData.Contents^.Items[Index2] := F;
    end;
  end;
end;

function TASNCustomOFFieldWrapper.IndexOf(
  Item: TASNCustomWrapper): Integer;
begin
  Result := FItems.IndexOf(Item);
end;

function TASNCustomOFFieldWrapper.InternalAdd: TASNCustomFieldWrapper;
var
  P: PASN1Struct;
begin
  FData.ReadOnly := False;
  if Assigned(FData.FEncapsulated) then begin
    FData.Encapsulated.ReadOnly := False;
    P := FData.Encapsulated.AddField;
    try
      FieldFactory(FFieldType,FData.Encapsulated.Template,P^,Result);
      FData.EditContent(FData.Encapsulated);
    except
      FData.Encapsulated.RemoveItem(P^);
      raise;
    end;
  end else begin
    P := FData.AddField;
    try
      if P^.TypeIdentified then begin
        P^.TypeIdentify;
        FData.Template.FIDFieldStruct := P^.FIDFieldStruct;
      end;
      FieldFactory(FFieldType,FData.Template,P^,Result);
    except
      FData.RemoveItem(P^);
      raise;
    end;
  end;
//  if FOwnerEncapsulated then
//    (FOwner as TASNCustomWrapper).UpdateContents;
end;

procedure TASNCustomOFFieldWrapper.LoadFromStream(AStream: TStream);
begin
  DestroyItems;
  inherited;
end;

procedure TASNCustomOFFieldWrapper.Move(CurIndex, NewIndex: Integer);
var
  F: TASN1Struct;
begin
  if CurIndex <> NewIndex then begin
    FItems.Move(CurIndex,NewIndex);
    FData.ReadOnly := False;
    F := FData.Items[CurIndex]^;
    if Assigned(FData.FEncapsulated) then begin
      if CurIndex > NewIndex then
        System.Move(FData.FEncapsulated.Contents^.Items[NewIndex],
                    FData.FEncapsulated.Contents^.Items[NewIndex + 1],
                    (CurIndex - NewIndex)*SizeOf(TASN1Struct))
      else
        System.Move(FData.FEncapsulated.Contents^.Items[CurIndex + 1],
                    FData.FEncapsulated.Contents^.Items[CurIndex],
                    (NewIndex - CurIndex)*SizeOf(TASN1Struct));
      FData.FEncapsulated.Contents^.Items[NewIndex] := F;
      FData.EditContent(FData.Encapsulated);
    end else begin
      if CurIndex > NewIndex then
        System.Move(FData.Contents^.Items[NewIndex],
                    FData.Contents^.Items[NewIndex + 1],
                    (CurIndex - NewIndex)*SizeOf(TASN1Struct))
      else
        System.Move(FData.Contents^.Items[CurIndex + 1],
                    FData.Contents^.Items[CurIndex],
                    (NewIndex - CurIndex)*SizeOf(TASN1Struct));
      FData.Contents^.Items[NewIndex] := F;
    end;
  end;
end;

procedure TASNCustomOFFieldWrapper.Update;
var
  I: Integer;
begin
  DestroyItems;
  if Assigned(FData.FEncapsulated) then begin
    if not FCreating then
      FData.ContentAsASN1Struct(FData.FEncapsulated);
    for I := 0 to FData.Encapsulated.ItemCount - 1 do
      FieldFactory(FFieldType,
                   FData.Encapsulated.Template,
                   FData.Encapsulated.Items[I]^,
                   nil^);
  end else
    for I := 0 to FData.ItemCount - 1 do begin
      FieldFactory(FFieldType,FData.Template,FData.Items[I]^,nil^);
      Items[I].Update;
    end;
end;

{ TOctetString }

function TOctetString.GetAsHexString: string;
begin
  Result := OSToHex(AsString,True);
end;

function TOctetString.GetAsString: string;
begin
  SetLength(Result,Length);
  GetBinary(Pointer(Result)^,Length);
  if FData.ActualTag = V_ASN1_BIT_STRING then
    Delete(Result,1,1);
end;

procedure TOctetString.GetAsStruct(var Struct: TASN1Struct);
begin
  FData.ContentAsASN1Struct(Struct);
end;

function TOctetString.GetBinary(var Buf; Count: Integer): Integer;
var
  F: TASN1Struct;
  G: PASN1Struct;
  B: PChar;
  I, Res, Unused, PrevUnused: Integer;
  X: PMPInteger;
  PrevTrail: Byte;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  if F.Constructed then begin
    if FData.ActualTag = V_ASN1_BIT_STRING then begin
      B := @Buf;
      B := B + 1;
      Count := Count - 1;
      I := 0;
      Result := 0;
      PrevUnused := 0;
      while (Count > 0) and (I < F.ItemCount) do begin
        G := F.Items[I];
        if G.ActualTag = V_ASN1_BIT_STRING then begin
          Res := G.Length - 1;
          if Res > 0 then begin
            Unused := Byte(G.Content[0]);
            if PrevUnused = 0 then begin
              if Count < Res then
                Res := Count;
              Move(G.Content[1],B^,Res);
            end else begin
              X := nil;
              try
                G.ContentAsUMPInt(X);
                if PrevUnused + Unused <= 8 then
                  Res := Res - 1;
                if Count < Res then
                  Res := Count;
                if Unused > PrevUnused then
                  MPShl(X,Unused - PrevUnused)
                else
                  MPShl(X,8 - PrevUnused + Unused);
                B := B - 1;
                PrevTrail := Byte(B[0]);
                UMPIntToBase256(X,B[0],Res + 1);
                B[0] := Char(Byte(B[0]) or PrevTrail);
              finally
                MPDealloc(X);
              end;
            end;
            PrevUnused := (Unused + PrevUnused) and $7;
            Inc(B,Res);
            Inc(Result,Res);
            Dec(Count,Res);
          end;
        end;
        Inc(I);
      end;
      if (Count = 0) and (Result > 0) then begin
        B := @Buf;
        B[0] := Char(PrevUnused);
      end;
    end else begin
      B := @Buf;
      I := 0;
      Result := 0;
      while (Count > 0) and (I < F.ItemCount) do begin
        G := F.Items[I];
        if G.ActualTag <> V_ASN1_EOC then begin
          Res := G.Length;
          if Count >= Res then
            Move(G.Content^,B^,Res)
          else
            Move(G.Content^,B^,Count);
          Inc(B,Res);
          Inc(Result,Res);
          Dec(Count,Res);
        end;
        Inc(I);
      end;
    end;
  end else begin
    Result := F.Length;
    if Count >= Result then
      Move(F.Content^,Buf,Result)
    else if Count > 0 then
      Move(F.Content^,Buf,Count)
  end;
end;

function TOctetString.GetLength: Integer;
var
  F: TASN1Struct;
  I: Integer;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  if F.Constructed then begin
    Result := 0;
    for I := 0 to F.ItemCount - 1 do
      Result := Result + F.Items[I].Length;
  end else
    Result := F.Length;
end;

procedure TOctetString.SetAsHexString(const Value: string);
begin
  FData.ReadOnly := False;
  FData.Constructed := False;
  if Assigned(FData.FEncapsulated) then
    FData.FEncapsulated.EditContent(Value)
  else
    FData.EditContent(Value);
  UpdateContents;
end;

procedure TOctetString.SetAsString(const Value: string);
var
  F: TASN1Struct;
  vTag: Cardinal;
  B: string;
begin
  FData.ReadOnly := False;
  FData.Constructed := False;
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  vTag := F.ActualTag;
  if vTag = V_ASN1_BIT_STRING then begin
    SetLength(B,System.Length(Value) + 1);
    try
      Move(Pointer(Value)^,B[2],System.Length(Value));
      B[1] := #0;
      F.SetContent(Pointer(B)^,System.Length(B));
    finally
      ProtectClear(Pointer(B)^,System.Length(B));
    end;
  end else
    F.SetContent(Pointer(Value)^,System.Length(Value));
  UpdateContents;
end;

procedure TOctetString.SetAsStruct(Struct: TASN1Struct);
var
  MS: TSecureMemoryStream;
begin
  FData.ReadOnly := False;
  MS := TSecureMemoryStream.Create;
  try
    MS.Capacity := Struct.CalculateLength + 8;
    Struct.SaveToStream(MS,fmtDER);
    FData.SetContent(MS.Memory^,MS.Size);
  finally
    MS.Free;
  end;
  UpdateContents;
end;

procedure TOctetString.SetBinary(const Buf; Count: Integer);
begin
  FData.ReadOnly := False;
  FData.Constructed := False;
  if Assigned(FData.FEncapsulated) then
    FData.FEncapsulated.SetContent(Buf,Count)
  else
    FData.SetContent(Buf,Count);
  UpdateContents;
end;

{ TBitString }

function TBitString.GetBitCount: Integer;
begin
  if Length <= 0 then
    Result := 0
  else if Assigned(FData.FEncapsulated) then
    Result := (FData.FEncapsulated.Length - 1)*8 -
               Byte(FData.FEncapsulated.Content[0])
  else
    Result := (Length - 1)*8 - Byte(FData.Content[0]);
end;

type
  TSet8 = set of 0..7;

function TBitString.GetBits(Index: Integer): Boolean;
var
  F: TASN1Struct;
  BI: Integer;
begin
  if FData.Constructed then
    F := FData.Items[0]^
  else
    F := FData;
  if Assigned(F.FEncapsulated) then
    F := F.FEncapsulated
  else
    F := FData;
  if Index >= 0 then begin
    if Index < BitCount then begin
      BI := Index shr 3;
      Index := 7 - (Index and 7);
      Result := Index in TSet8(F.Content[BI + 1]);
    end else
      Result := False;
  end else
    raise Exception.Create('Index out of range');
end;

procedure TBitString.SetBitCount(const Value: Integer);
const
  UnusedBitsMask: array [0..7] of Byte = ($FF,$FE,$FC,$F8,$F0,$E0,$C0,$80);
var
  F: TASN1Struct;
  Cnt: Pointer;
  BI: Integer;
  UB: Byte;
begin
  BeginUpdate;
  try
    if Assigned(FData.FEncapsulated) then
      F := FData.FEncapsulated
    else
      F := FData;
    F.ReadOnly := False;
    BI := (Value + 7) shr 3;
    UB := (8 - Value) and 7;
    if (F.Length > 1) and (BI + 1 <> F.Length) then begin
      GetMem(Cnt,BI + 1);
      try
        FillChar(Cnt^,BI + 1,0);
        if F.Length < BI + 1 then
          Move(F.Content^,Cnt^,F.Length)
        else begin
          Move(F.Content^,Cnt^,BI + 1);
          PChar(Cnt)[BI] := Char(Byte(PChar(Cnt)[BI]) and UnusedBitsMask[UB]);
        end;
        F.SetContent(Cnt^,BI + 1);
        FillChar(Cnt^,BI + 1,0);
      finally
        FreeMem(Cnt);
      end;
    end else if BI + 1 <> F.Length then
      F.Length := BI + 1;
    F.Content[0] := Char(UB);
    if Assigned(FData.FEncapsulated) then
      FData.EditContent(FData.FEncapsulated);
  finally
    EndUpdate;
  end;
end;

procedure TBitString.SetBits(Index: Integer; const Value: Boolean);
var
  F: TASN1Struct;
  BI: Integer;
  C: TSet8;
begin
  BeginUpdate;
  try
    if Assigned(FData.FEncapsulated) then
      F := FData.FEncapsulated
    else
      F := FData;
    FData.ReadOnly := False;
    if Index >= 0 then begin
      if Index >= BitCount then begin
        BitCount := Index + 1;
      end;
      BI := Index shr 3;
      Index := 7 - (Index and 7);
      C := TSet8(F.Content[BI + 1]);
      if Value then
        Include(C,Index)
      else
        Exclude(C,Index);
      F.Content[BI + 1] := Char(C);
    end else
      raise Exception.Create('Index out of range');
  finally
    EndUpdate;
  end;
end;

{ TStringWrapper }

function TStringWrapper.GetString: string;
var
  F: TASN1Struct;
  vTag: Cardinal;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  if F.Constructed then
    F := F.Items[0]^;
  vTag := F.ActualTag;
  if vTag = V_ASN1_OBJECT then
    Result := F.ContentAsOID
  else
    Result := F.ContentAsString;
end;

procedure TStringWrapper.SetString(const Value: string);
var
  F: TASN1Struct;
begin            
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  FData.ReadOnly := False;
  if Value = '' then
    F.Length := 0
  else
    F.EditContent(Value);
  UpdateContents;
end;

{ TWideStringWrapper }

function TWideStringWrapper.GetWideString: WideString;
var
  F: TASN1Struct;
begin          
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  Result := F.ContentAsString;
end;

procedure TWideStringWrapper.SetWideString(const Value: WideString);
var
  F: TASN1Struct;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  FData.ReadOnly := False;
  if Value = '' then
    F.Length := 0
  else
    F.EditContent(Value);
  UpdateContents;
end;

{ TBooleanWrapper }

function TBooleanWrapper.GetBoolean: Boolean;
var
  F: TASN1Struct;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  if F.Length > 0 then
    Result := F.ContentAsBoolean
  else
    Result := False;
end;

procedure TBooleanWrapper.SetBoolean(const Value: Boolean);
var
  F: TASN1Struct;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  FData.ReadOnly := False;
  F.EditContent(Value);
  UpdateContents;
end;

{ TIntegerWrapper }

function TIntegerWrapper.GetAsCardinal: Cardinal;
begin
  Result := FData.ContentAsCardinal;
end;

function TIntegerWrapper.GetAsInt64: Integer;
begin
  if Assigned(FData.FEncapsulated) then
    Result := FData.FEncapsulated.ContentAsInt64
  else
    Result := FData.ContentAsInt64;
end;

function TIntegerWrapper.GetAsInteger: Integer;
begin                            
  if Assigned(FData.FEncapsulated) then
    Result := FData.FEncapsulated.ContentAsInteger
  else
    Result := FData.ContentAsInteger;
end;

function TIntegerWrapper.GetIntegerString: string;
var
  P: PMPInteger;
begin
  P := nil;
  try
    GetMPInt(P);
    if P = nil then
      Result := ''
    else
      Result := MPIntToBase10(P);
  finally
    MPDealloc(P);
  end;
end;

procedure TIntegerWrapper.GetMPInt(var Value: PMPInteger);
begin
  if Assigned(FData.FEncapsulated) then
    FData.FEncapsulated.ContentAsMPInt(Value)
  else
    FData.ContentAsMPInt(Value);
end;

procedure TIntegerWrapper.SetAsCardinal(const Value: Cardinal);
begin
  FData.ReadOnly := False;
  if Assigned(FData.FEncapsulated) then
    FData.FEncapsulated.EditContent(Value)
  else
    FData.EditContent(Value);
  UpdateContents;
end;

procedure TIntegerWrapper.SetAsInt64(const Value: Integer);
begin
  FData.ReadOnly := False;
  if Assigned(FData.FEncapsulated) then
    FData.FEncapsulated.EditContent(Value)
  else
    FData.EditContent(Value);
  UpdateContents;
end;

procedure TIntegerWrapper.SetAsInteger(const Value: Integer);
begin
  FData.ReadOnly := False;
  if Assigned(FData.FEncapsulated) then
    FData.FEncapsulated.EditContent(Value)
  else
    FData.EditContent(Value);
  Update;
end;

procedure TIntegerWrapper.SetIntegerString(const Value: string);
var
  P: PMPInteger;
begin
  if Value = '' then
    FData.SetLength(0)
  else begin
    P := nil;
    try
      Base10ToMPInt(P,Value);
      SetMPInt(P);
    finally
      MPDealloc(P);
    end;
  end;
  UpdateContents;
end;

procedure TIntegerWrapper.SetMPInt(Value: PMPInteger);
begin
  FData.ReadOnly := False;
  if Assigned(FData.FEncapsulated) then
    FData.FEncapsulated.EditContent(Value,False)
  else
    FData.EditContent(Value,False);
  UpdateContents;
end;

{ TRealWrapper }

function TRealWrapper.GetExtended: Extended;
begin
  if Assigned(FData.FEncapsulated) then
    Result := FData.FEncapsulated.ContentAsReal
  else
    Result := FData.ContentAsReal;
end;

procedure TRealWrapper.SetExtended(const Value: Extended);
begin
  FData.ReadOnly := False;
  if Assigned(FData.FEncapsulated) then
    FData.FEncapsulated.EditContent(Value)
  else
    FData.EditContent(Value);
  UpdateContents;
end;

{ TUTCTime }

function TUTCTime.GetAsDateTime: TDateTime;
var
  F: TASN1Struct;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  if F.Length > 0 then
    Result := F.ContentAsDateTime
  else
    Result := 0;
end;

function TUTCTime.GetDay: Byte;
var
  T: TSystemTime;
  F: TASN1Struct;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  F.ContentAsSystemTime(T);
  Result := T.wDay;
end;

function TUTCTime.GetHour: Byte;
var
  T: TSystemTime;
  F: TASN1Struct;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  F.ContentAsSystemTime(T);
  Result := T.wHour;
end;

function TUTCTime.GetMinutes: Byte;
var
  T: TSystemTime;
  F: TASN1Struct;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  F.ContentAsSystemTime(T);
  Result := T.wMinute;
end;

function TUTCTime.GetMonth: Byte;
var
  T: TSystemTime;
  F: TASN1Struct;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  F.ContentAsSystemTime(T);
  Result := T.wMonth;
end;

function TUTCTime.GetSeconds: Byte;
var
  T: TSystemTime;
  F: TASN1Struct;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  F.ContentAsSystemTime(T);
  Result := T.wSecond;
end;

function TUTCTime.GetTimeString: TimeString;
begin
  if Assigned(FData.FEncapsulated) then
    Result := FData.FEncapsulated.ContentAsString
  else
    Result := FData.ContentAsString;
end;

function TUTCTime.GetYear: Word;
var
  T: TSystemTime;
  F: TASN1Struct;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  F.ContentAsSystemTime(T);
  Result := T.wYear;
end;

procedure TUTCTime.SetAsDateTime(const Value: TDateTime);
begin
  FData.ReadOnly := False;
  if Assigned(FData.FEncapsulated) then
    FData.FEncapsulated.EditContent(Value)
  else
    FData.EditContent(Value);
  UpdateContents;
end;

procedure TUTCTime.SetDay(const Value: Byte);
var
  T: TSystemTime;
  F: TASN1Struct;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  FData.ReadOnly := False;
  F.ContentAsSystemTime(T);
  T.wDay := Value;
  F.EditContent(T);
  UpdateContents;
end;

procedure TUTCTime.SetHour(const Value: Byte);
var
  T: TSystemTime;
  F: TASN1Struct;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  FData.ReadOnly := False;
  F.ContentAsSystemTime(T);
  T.wHour := Value;
  F.EditContent(T);
  UpdateContents;
end;

procedure TUTCTime.SetMinutes(const Value: Byte);
var
  T: TSystemTime;
  F: TASN1Struct;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  FData.ReadOnly := False;
  F.ContentAsSystemTime(T);
  T.wMinute := Value;
  F.EditContent(T);
  UpdateContents;
end;

procedure TUTCTime.SetMonth(const Value: Byte);
var
  T: TSystemTime;
  F: TASN1Struct;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  FData.ReadOnly := False;
  F.ContentAsSystemTime(T);
  T.wMonth := Value;
  F.EditContent(T);
  Update;
end;

procedure TUTCTime.SetSeconds(const Value: Byte);
var
  T: TSystemTime;
  F: TASN1Struct;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  FData.ReadOnly := False;
  F.ContentAsSystemTime(T);
  T.wMonth := Value;
  F.EditContent(T);
  UpdateContents;
end;

procedure TUTCTime.SetTimeString(const Value: TimeString);
var
  S: TSystemTime;
  F: TASN1Struct;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  FData.ReadOnly := False;
  if Value = '' then
    F.Length := 0
  else if UTCTimeToSystemTime(PChar(Value),Length(Value),S) then
    F.EditContent(S);
  UpdateContents;
end;

procedure TUTCTime.SetYear(const Value: Word);
var
  T: TSystemTime;
  F: TASN1Struct;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  FData.ReadOnly := False;
  F.ContentAsSystemTime(T);
  T.wYear := Value;
  F.EditContent(T);
  UpdateContents;
end;

{ TGeneralizedTime }

function TGeneralizedTime.GetMilliseconds: Word;
var
  T: TSystemTime;
begin
  if Assigned(FData.FEncapsulated) then
    FData.Encapsulated.ContentAsSystemTime(T)
  else
    FData.ContentAsSystemTime(T);
  Result := T.wMilliseconds;
end;

procedure TGeneralizedTime.SetGeneralizedTime(const Value: TimeString);
var
  S: TSystemTime;
  F: TASN1Struct;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  FData.ReadOnly := False;
  if Value = '' then
    F.Length := 0
  else if GeneralizedTimeToSystemTime(PChar(Value),Length(Value),S) then
    F.EditContent(S);
  UpdateContents;
end;

procedure TGeneralizedTime.SetMilliseconds(const Value: Word);
var
  T: TSystemTime;
  F: TASN1Struct;
begin
  if Assigned(FData.FEncapsulated) then
    F := FData.FEncapsulated
  else
    F := FData;
  FData.ReadOnly := False;
  F.ContentAsSystemTime(T);
  T.wMilliseconds := Value;
  F.EditContent(T);
  UpdateContents;
end;

{ TASNConstructedWrapper }

constructor TASNConstructedWrapper.Create(AOwner: TPersistent; AData,
  ATemplate: TASN1Struct);
var
  Intf: IASNClassFactory;
  DataTmpl: TASN1Struct;
begin
  inherited Create(AOwner,AData,ATemplate);
  if FTemplate = nil then
    FTemplate := FData;
  if FData.TypeName = '' then begin
    Intf := GetClassFactory;
    if Assigned(Intf) then begin
      DataTmpl := Intf.GetDataTemplate;
      if Assigned(DataTmpl) then
        FData.Assign(DataTmpl);
    end;
  end;
end;

function TASNConstructedWrapper.GetCount: Integer;
begin
  if Assigned(FData.FEncapsulated) then
    Result := FData.FEncapsulated.ItemCount
  else
    Result := FData.ItemCount;
end;

class function TASNConstructedWrapper.GetItemClass(
  Index: Integer): TASNWrapperClass;
var
  Intf: IASNClassFactory;
begin
  Intf := GetClassFactory;
  if Assigned(Intf) then
    Result := Intf.GetItemClass(Index)
  else
    Result := nil;
end;

class function TASNConstructedWrapper.GetItemClassCount: Integer;
var
  Intf: IASNClassFactory;
begin
  Intf := GetClassFactory;
  if Assigned(Intf) then
    Result := Intf.GetItemCount
  else
    Result := -1;
end;

function TASNConstructedWrapper.GetItems(
  Index: Integer): TASNCustomFieldWrapper;
begin
  Result := InternalGetItems(Index);
end;

function TASNConstructedWrapper.InternalGetItems(
  Index: Integer): TASNCustomFieldWrapper;
begin
  Result := FItems[Index];
end;

function TASNConstructedWrapper.ItemByName(
  const VarName: string): TASNCustomFieldWrapper;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do begin
    Result := Items[I];
    if Result.FData.VarName = VarName then
      Exit;
  end;
  Result := nil;
end;

{ TASNChoiceWrapper }

constructor TASNChoiceWrapper.Create(AOwner: TPersistent; AData,
  ATemplate: TASN1Struct);
begin
  inherited;
  if GetClassFactory = nil then
    // Maintained for backwards compatibility
    FChoiceList := TList.Create;
end;

destructor TASNChoiceWrapper.Destroy;
begin
  FSelected.Free;
  FChoiceList.Free;
  inherited;
end;

function TASNChoiceWrapper.GetChoiceClass(Index: Integer): TASNWrapperClass;
var
  Intf: IASNClassFactory;
begin
  Intf := GetClassFactory;
  if Assigned(Intf) then
    Result := Intf.GetChoiceClass(Index)
  else
    Result := FChoiceList[Index];
end;

function TASNChoiceWrapper.GetChoiceClassCount: Integer;
var
  Intf: IASNClassFactory;
begin
  Intf := GetClassFactory;
  if Assigned(Intf) then
    Result := Intf.GetChoiceCount
  else
    Result := FChoiceList.Count;
end;

function TASNChoiceWrapper.GetGeneric: TOctetString;
begin
  Result := (FSelected as TOctetString);
end;

function TASNChoiceWrapper.GetIndexOfChoice(AClass: TClass): Integer;
var
  Intf: IASNClassFactory;
begin
  if AClass.InheritsFrom(TASNCustomFieldWrapper) then begin
    Intf := GetClassFactory;
    if Assigned(Intf) then
      Result := Intf.GetIndexOfChoice(TASNWrapperClass(AClass))
    else
      Result := FChoiceList.IndexOf(AClass);
  end else
    Result := -1;
end;

function TASNChoiceWrapper.GetSelected: TASNCustomFieldWrapper;
var
  I: Integer;
begin
  Result := FSelected;
  if FSelected = nil then begin
    for I := 0 to FItems.Count - 1 do begin
      Result := FItems.Items[I];
      if Result.IsTemplateType then
        Exit;
    end;
    Result := nil;
  end;
end;

function TASNChoiceWrapper.GetValue: WideString;
begin
  if Assigned(FSelected) then begin
    if FSelected is TASNChoiceWrapper then
      Result := TASNChoiceWrapper(FSelected).GetValue
    else if FSelected is TWideStringWrapper then
      Result := TWideStringWrapper(FSelected).Value
    else if FSelected is TStringWrapper then
      Result := TStringWrapper(FSelected).Value
    else if FSelected is TIntegerWrapper then
      Result := TIntegerWrapper(FSelected).Value
    else if FSelected is TOctetString then
      Result := TOctetString(FSelected).Value
    else if FSelected is TUTCTime then
      Result := TUTCTime(FSelected).GetTimeString
    else
      Result := FSelected.FData.DisplayContent
  end else
    Result := OSToHex(FData.ContentAsOctetString);
end;

function TASNChoiceWrapper.InternalGetChoice: Integer;
var
  Id: Integer;
begin
  Result := 0;
  if FData.ChoiceVarName = '' then
    Exit;
  if FTemplate.TypeIdentified then begin
    while Result < FTemplate.ChoiceCount do begin
      Inc(Result);
      if FTemplate.Choices[Ord(Result)-1]^.BERIdentifiedBy = FData.BERIdentifiedBy then
        Exit;
    end;
  end else begin
    Id := 0;
    while Id < FTemplate.ChoiceCount do begin
      if (Id > 0) and
         (FTemplate.Choices[Id]^.VarName =
            FTemplate.Choices[Id-1]^.VarName) then
        Inc(Id);
      Inc(Result);
      if FTemplate.Choices[Id]^.VarName = FData.ChoiceVarName then
        Exit;
      Inc(Id);
    end;
  end;
  Result := 0;
end;

function TASNChoiceWrapper.InternalIsChoice(Value: Integer): Boolean;
var
  Id: Integer;
begin
  if FSelected = nil then
    Result := False
  else if FTemplate.TypeIdentified then
    Result := FTemplate.Choices[Value-1]^.FBERIdentifiedBy = FData.FBERIdentifiedBy
  else begin
    Id := 1;
    while Id < Value do begin
      if FTemplate.Choices[Id]^.FVarName = FTemplate.Choices[Id-1]^.FVarName then
        Inc(Value);
      Inc(Id);
    end;
    Result := FTemplate.Choices[Value-1]^.VarName = FData.ChoiceVarName;
  end;
end;

procedure TASNChoiceWrapper.InternalSelectChoice(Value: Integer);
var
  P, C: PASN1Struct;
  OK: Boolean;
  AValue, Id: Integer;
  SelectedClass: TASNWrapperClass;
begin
  if Value < 0 then begin
    FData.Clear;
    FSelected.Free;
    FSelected := nil;
    Exit;
  end;
  AValue := Value;
  if (FData.ChoiceCount > 0) and (FTemplate <> nil) then begin
    FData.ReadOnly := False;
    if FTemplate.TypeIdentified then begin
      P := FTemplate.FindIDField;
      if Assigned(P) then begin
        C := FTemplate.Choices[Value];
        if Assigned(C) then begin
          OK := True;
          if (P^.Length <> Length(C^.FBERIdentifiedBy)) or
             not CompareMem(P^.Cnt,Pointer(C^.FBERIdentifiedBy),Length(C^.FBERIdentifiedBy)) then begin
            OK := False;
            P^.SetContent(Pointer(C^.FBERIdentifiedBy)^,Length(C^.FBERIdentifiedBy));
          end;
          if (P^.Length <> Length(FData.FBERIdentifiedBy)) or
             not CompareMem(P^.Cnt,Pointer(FData.FBERIdentifiedBy),Length(FData.FBERIdentifiedBy)) then begin
            OK := False;
            FData.SelectChoice(C);
          end;
          if OK and Assigned(FSelected) then
            Exit;
        end;
      end;
    end else begin
      Id := 1;
      while Id < Value do begin
        if FTemplate.Choices[Id]^.FVarName = FTemplate.Choices[Id-1]^.FVarName then
          Inc(Value);
        Inc(Id);
      end;
      if Id = Value then       
        if FTemplate.Choices[Id]^.FVarName = FTemplate.Choices[Id-1]^.FVarName then
          Inc(Value);
      if FData.ChoiceVarName <> FTemplate.Choices[Value]^.VarName then
        FData.SelectChoice(FTemplate.Choices[Value])
      else
        Exit;
    end;
  end;
  if Assigned(FChoiceList) then begin
    // Maintained for backwards compatibility:
    if FChoiceList.Count > 0 then
      SelectedClass := FChoiceList[AValue]
    else
      SelectedClass := nil;
  end else begin
    // New style:
    SelectedClass := Self.GetChoiceClass(AValue);
  end;
  if Assigned(SelectedClass) then begin
    FSelected.Free;
    FSelected := nil;
    if FTemplate.Choices[Value]^.Constructed and
       (FTemplate.Choices[Value]^.ItemCount = 1) and
       (FTemplate.Choices[Value]^.Cls <> V_ASN1_UNIVERSAL) and
       not FTemplate.Choices[Value]^.Implicit then
      FieldFactory(SelectedClass,FTemplate.Choices[Value]^.Items[0]^,FData.Items[0]^,FSelected)
    else
      FieldFactory(SelectedClass,FTemplate.Choices[Value]^,FData,FSelected);
    if Assigned(FSelected) then
      FItems.Delete(FItems.Count - 1);
  end;
end;

procedure TASNChoiceWrapper.SelectGeneric;
begin
  if FSelected is TOctetString then Exit;
  if Assigned(FSelected) then begin
    if FData.ChoiceCount = 0 then Exit;
    FData.DisposeContent;
    FSelected.Free;
    FSelected := nil;
  end;
  FData.Tag := V_ASN1_OCTET_STRING;
  FData.Cls := V_ASN1_UNIVERSAL;
  FData.Constructed := False;
  FData.FChoiceVarName := '';
  FData.FBERIdentifiedBy := '';
  FieldFactory(TOctetString,nil,FData,FSelected);
  if Assigned(FSelected) then
    FItems.Delete(FItems.Count - 1);
end;

procedure TASNChoiceWrapper.SetGeneric(const Value: TOctetString);
begin
  SelectGeneric;
  FSelected.FData.Assign(Value.FData);
end;

procedure TASNChoiceWrapper.SetSelected(
  const Value: TASNCustomFieldWrapper);
var
  F: TASNCustomFieldWrapper;
begin
  FData.ReadOnly := False;
  if Value.GetOwner = Self then begin
    if Value <> FSelected then
      InternalSelectChoice(FItems.IndexOf(Value));
  end else begin
    if Assigned(FChoiceList) then begin
      // Maintained for backwards compatibility:
      if FChoiceList.Count > 0 then
        InternalSelectChoice(FChoiceList.IndexOf(Value.ClassType));
    end else begin
      // New style:
      InternalSelectChoice(GetIndexOfChoice(Value.ClassType));
    end;
    F := GetSelected;
    if Assigned(FSelected) then
      F.Assign(Value);
  end;
end;

procedure TASNChoiceWrapper.SetValue(const Value: WideString);
var
  SS: TStringStream;
begin
  if Assigned(FSelected) then begin
    if FSelected is TASNChoiceWrapper then
      TASNChoiceWrapper(FSelected).SetValue(Value)
    else if FSelected is TWideStringWrapper then
      TWideStringWrapper(FSelected).Value := Value
    else if FSelected is TStringWrapper then
      TStringWrapper(FSelected).Value := Value
    else if FSelected is TIntegerWrapper then
      TIntegerWrapper(FSelected).Value := Value
    else if FSelected is TOctetString then
      TOctetString(FSelected).Value := Value
    else if FSelected is TGeneralizedTime then
      TGeneralizedTime(FSelected).SetGeneralizedTime(Value)
    else if FSelected is TUTCTime then
      TUTCTime(FSelected).SetTimeString(Value)
    else
      FSelected.FData.EditContent(Value);
  end else begin
    SS := TStringStream.Create(Value);
    try
      FData.LoadFromStream(SS,fmtDER);
    finally
      SS.Free;
    end;
  end;
end;

procedure TASNChoiceWrapper.Update;
var
  Value, Id: Integer;
  P: PASN1Struct;
  SelectedClass: TASNWrapperClass;
begin
  inherited;
  if GetChoiceClassCount > 0 then begin
    P := nil;
    if FTemplate.TypeIdentified then
      FTemplate.TypeIdentify(P,Value,True)
    else begin
      Value := FTemplate.FChoiceIndex;
      if Value >= 0 then
        P := FTemplate.Choices[Value]
      else
        P := nil;
      if (P = nil) or
         not ((P^.VarName = FTemplate.FChoiceVarName) or
              (not FData.TypeIdentified and
               (P^.VarName = FData.FChoiceVarName))) then begin
        P := nil;
        Value := 0;
        while Value < FTemplate.ChoiceCount do begin
          P := FTemplate.Choices[Value];
          if (FData.Tag = P^.Tag) and
             (FData.Cls = P^.Cls) and 
             not (FData.Constructed xor P^.Constructed) then
            Break;
          P := nil;
          Inc(Value);
        end;
      end;
      if (Value < 0) or (Value >= FTemplate.ChoiceCount) then
        Value := -1
      else begin
        Id := Value;
        while Id > 0 do begin
          if FTemplate.Choices[Id]^.FVarName = FTemplate.Choices[Id-1]^.FVarName then
            Dec(Value);
          Dec(Id);
        end;
      end;
    end;
    if (Value < 0) or (Value > GetChoiceClassCount) or (P = nil) then begin
      FSelected.Free;
      FSelected := nil;
      if FTemplate.TypeIdentified or FData.Optional then begin
        if FData.IsEmpty then
          SelectGeneric
      end else if Assigned(P) then
        raise Exception.Create(Format('Wrapper not registred for type %s (%s)',
                                      [FData.TypeName,P^.TypeName]))
      else
        raise Exception.Create(Format('Choice not registred for type %s:'#13#10 +
                                      'Tag: %d'#13#10'Cls: %d',
                                      [FData.TypeName,FData.Tag,FData.Cls]));
    end else begin
      SelectedClass := GetChoiceClass(Value);
      if (FSelected = nil) or
         (SelectedClass = nil) or
         not FSelected.InheritsFrom(SelectedClass) then begin
        FSelected.Free;
        FSelected := nil;
        if SelectedClass <> nil then begin
          if P^.Constructed and
             (P^.ItemCount = 1) then
            FieldFactory(SelectedClass,P^.Items[0]^,FData.Items[0]^,FSelected)
          else
            FieldFactory(SelectedClass,P^,FData,FSelected);
          FItems.Delete(FItems.Count - 1);
        end;
      end;
    end;
    if Assigned(FSelected) then
      FSelected.Update;
  end;
end;

procedure InitASN;
begin
  GarbageList := TThreadHashList.Create;
  RTLock := TCriticalSection.Create;
  InstancePool := TList.Create;
  MemoryBlocks := TList.Create;
end;

{ TASNClassFactory }

destructor TASNClassFactory.Destroy;
begin
  if Assigned(FDataTemplate) then
    FDataTemplate._Release;
  FChoiceClasses.Free;
  FItemClasses.Free;
  inherited;
end;

function TASNClassFactory.GetChoiceClass(Index: Integer): TASNWrapperClass;
begin
  if Assigned(FChoiceClasses) then
    Result := FChoiceClasses[Index]
  else
    Result := nil;
end;

function TASNClassFactory.GetChoiceCount: Integer;
begin
  if Assigned(FChoiceClasses) then
    Result := FChoiceClasses.Count
  else
    Result := 0;
end;

function TASNClassFactory.GetDataTemplate: TASN1Struct;
begin
  Result := FDataTemplate;
end;

function TASNClassFactory.GetIndexOfChoice(
  AClass: TASNWrapperClass): Integer;
begin
  if Assigned(FChoiceClasses) then
    Result := FChoiceClasses.IndexOf(AClass)
  else
    Result := -1;
end;

function TASNClassFactory.GetItemClass(Index: Integer): TASNWrapperClass;
begin
  if Assigned(FItemClasses) then
    Result := FItemClasses[Index]
  else
    Result := nil;
end;

function TASNClassFactory.GetItemCount: Integer;
begin
  if Assigned(FItemClasses) then
    Result := FItemClasses.Count
  else
    Result := 0;
end;

function TASNClassFactory.GetTargetClass: TASNWrapperClass;
begin
  Result := FTargetClass;
end;

{ TASNClassFactories }

procedure TASNClassFactories.Clear;
var
  I: Integer;
begin
  for I := 0 to FFactories.Count - 1 do
    IUnknown(FFactories.List[I]) := nil;
  FFactories.Clear;
end;

constructor TASNClassFactories.Create;
begin
  FLock := TCriticalSection.Create;
  FClasses := THashList.Create;
  FFactories := TList.Create;
end;

destructor TASNClassFactories.Destroy;
begin
  Clear;
  FLock.Free;
  FClasses.Free;
  FFactories.Free;
  inherited;
end;

function TASNClassFactories.GetClassFactory(
  AClass: TASNWrapperClass): IASNClassFactory;
var
  Idx: Integer;
begin
  Idx := FClasses.IndexOf(AClass);
  if Idx < 0 then
    Result := nil
  else
    Result := IASNClassFactory(FFactories[Idx]);
end;

procedure TASNClassFactories.Lock;
begin
  FLock.Acquire;
end;

procedure TASNClassFactories.RegisterChoiceClass(
  AChoiceClass: TASNWrapperClass; ADataTemplate: TASN1Struct;
  AChoices: array of TASNWrapperClass);
var
  Idx: Integer;
  Intf: IASNClassFactory;
begin
  Intf := TASNChoiceClassFactory.Create(AChoiceClass,ADataTemplate,AChoices);
  Idx := FClasses.IndexOf(AChoiceClass);
  if Idx < 0 then begin
    Idx := FClasses.Add(AChoiceClass);
    FFactories.Insert(Idx,nil);
  end;
  IASNClassFactory(FFactories.List[Idx]) := Intf;
end;

procedure TASNClassFactories.RegisterConstructedClass(
  AOwnerClass: TASNWrapperClass; ADataTemplate: TASN1Struct;
  AItemClasses: array of TASNWrapperClass);
var
  Idx: Integer;
  Intf: IASNClassFactory;
begin
  Intf := TASNConstructedClassFactory.Create(AOwnerClass,ADataTemplate,AItemClasses);
  Idx := FClasses.IndexOf(AOwnerClass);
  if Idx < 0 then begin
    Idx := FClasses.Add(AOwnerClass);
    FFactories.Insert(Idx,nil);
  end;
  IASNClassFactory(FFactories.List[Idx]) := Intf;
end;

procedure TASNClassFactories.Unlock;
begin
  FLock.Release;
end;

{ TASNChoiceClassFactory }

constructor TASNChoiceClassFactory.Create(AChoiceClass: TASNWrapperClass;
  ADataTemplate: TASN1Struct; AChoices: array of TASNWrapperClass);
var
  I: Integer;
begin
  FTargetClass := AChoiceClass;
  FDataTemplate := ADataTemplate;
  if Assigned(FDataTemplate) then
    FDataTemplate._AddRef;
  FChoiceClasses := TList.Create;
  FChoiceClasses.Capacity := Length(AChoices);
  for I := Low(AChoices) to High(AChoices) do
    FChoiceClasses.Add(AChoices[I]);
end;

{ TASNConstructedClassFactory }

constructor TASNConstructedClassFactory.Create(
  AOwnerClass: TASNWrapperClass; ADataTemplate: TASN1Struct;
  AItemClasses: array of TASNWrapperClass);
var
  I: Integer;
begin
  FTargetClass := AOwnerClass;
  FDataTemplate := ADataTemplate;
  if Assigned(FDataTemplate) then
    FDataTemplate._AddRef;
  FItemClasses := TList.Create;
  FItemClasses.Capacity := Length(AItemClasses);
  for I := Low(AItemClasses) to High(AItemClasses) do
    FItemClasses.Add(AItemClasses[I]);
end;

{$IFDEF INITIALIZATION}
initialization
{$ENDIF}
{$IFDEF INIT_SECTIONS}
  InitASN;
{$ENDIF}
{$IFDEF FINI_SECTIONS}
finalization
  CollectGarbage;
{$ENDIF}
end.
