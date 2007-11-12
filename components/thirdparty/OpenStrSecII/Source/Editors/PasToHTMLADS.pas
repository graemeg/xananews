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
}
unit PasToHTMLADS;

interface

uses
  SysUtils, Classes, Asn1;

const
  crlf = #13#10;
  ASNModule =
    'ASN.1 Module' + crlf +
    'BEGIN' + crlf + crlf +
    'ASNClientData1Records ::= SEQUENCE OF ASNClientData1Record' + crlf + crlf +
    'ASNClientData1Record ::= SEQUENCE {' + crlf +
    '     Name'#9'UTF8STRING,' + crlf +
    '     FileName'#9'UTF8STRING,' + crlf +
    '     Kind'#9'INTEGER,' + crlf +
    '     InheritsFrom'#9'UTF8STRING,' + crlf +
    '     UnitName'#9'UTF8STRING,' + crlf +
    '     ClassName'#9'UTF8STRING,' + crlf +
    '     Declaration'#9'UTF8STRING,' + crlf +
    '     Description'#9'UTF8STRING,' + crlf +
    '     Beta'#9'[6] IMPLICIT BOOLEAN DEFAULT 860100,' + crlf +
    '     Deprecated'#9'[7] IMPLICIT BOOLEAN DEFAULT 870100,' + crlf +
    '     Hidden'#9'[8] IMPLICIT BOOLEAN DEFAULT 880100,' + crlf +
    '     Classes'#9'[0] IMPLICIT ClassesRecords OPTIONAL,' + crlf +
    '     Routines'#9'[3] IMPLICIT RoutinesRecords OPTIONAL,' + crlf +
    '     IntfSect'#9'[9] IMPLICIT FieldsRecord OPTIONAL}' + crlf + crlf +
    'ClassesRecords ::= SEQUENCE OF ClassesRecord' + crlf + crlf +
    'ClassesRecord ::= SEQUENCE {' + crlf +
    '     Name'#9'UTF8STRING,' + crlf +
    '     FileName'#9'UTF8STRING,' + crlf +
    '     Kind'#9'INTEGER,' + crlf +
    '     InheritsFrom'#9'UTF8STRING,' + crlf +
    '     UnitName'#9'UTF8STRING,' + crlf +
    '     ClassName'#9'UTF8STRING,' + crlf +
    '     Declaration'#9'UTF8STRING,' + crlf +
    '     Description'#9'UTF8STRING,' + crlf +
    '     Beta'#9'[6] IMPLICIT BOOLEAN DEFAULT 860100,' + crlf +
    '     Deprecated'#9'[7] IMPLICIT BOOLEAN DEFAULT 870100,' + crlf +
    '     Hidden'#9'[8] IMPLICIT BOOLEAN DEFAULT 880100,' + crlf +
    '     Methods'#9'[1] IMPLICIT MethodsRecords OPTIONAL,' + crlf +
    '     PublicProperties'#9'[2] IMPLICIT PublicPropertiesRecords OPTIONAL,' + crlf +
    '     PublishedProperties'#9'[4] IMPLICIT PublishedPropertiesRecords OPTIONAL,' + crlf +
    '     Fields'#9'[5] IMPLICIT FieldsRecords OPTIONAL}' + crlf + crlf +
    'MethodsRecords ::= SEQUENCE OF MethodsRecord' + crlf + crlf +
    'MethodsRecord ::= SEQUENCE {' + crlf +
    '     Name'#9'UTF8STRING,' + crlf +
    '     FileName'#9'UTF8STRING,' + crlf +
    '     Kind'#9'INTEGER,' + crlf +
    '     InheritsFrom'#9'UTF8STRING,' + crlf +
    '     UnitName'#9'UTF8STRING,' + crlf +
    '     ClassName'#9'UTF8STRING,' + crlf +
    '     Declaration'#9'UTF8STRING,' + crlf +
    '     Description'#9'UTF8STRING,' + crlf +
    '     Beta'#9'[6] IMPLICIT BOOLEAN DEFAULT 860100,' + crlf +
    '     Deprecated'#9'[7] IMPLICIT BOOLEAN DEFAULT 870100,' + crlf +
    '     Hidden'#9'[8] IMPLICIT BOOLEAN DEFAULT 880100}' + crlf + crlf + crlf +
    'PublicPropertiesRecords ::= SEQUENCE OF PublicPropertiesRecord' + crlf + crlf +
    'PublicPropertiesRecord ::= SEQUENCE {' + crlf +
    '     Name'#9'UTF8STRING,' + crlf +
    '     FileName'#9'UTF8STRING,' + crlf +
    '     Kind'#9'INTEGER,' + crlf +
    '     InheritsFrom'#9'UTF8STRING,' + crlf +
    '     UnitName'#9'UTF8STRING,' + crlf +
    '     ClassName'#9'UTF8STRING,' + crlf +
    '     Declaration'#9'UTF8STRING,' + crlf +
    '     Description'#9'UTF8STRING,' + crlf +
    '     Beta'#9'[6] IMPLICIT BOOLEAN DEFAULT 860100,' + crlf +
    '     Deprecated'#9'[7] IMPLICIT BOOLEAN DEFAULT 870100,' + crlf +
    '     Hidden'#9'[8] IMPLICIT BOOLEAN DEFAULT 880100}' + crlf + crlf + crlf +
    'PublishedPropertiesRecords ::= SEQUENCE OF PublishedPropertiesRecord' + crlf + crlf +
    'PublishedPropertiesRecord ::= SEQUENCE {' + crlf +
    '     Name'#9'UTF8STRING,' + crlf +
    '     FileName'#9'UTF8STRING,' + crlf +
    '     Kind'#9'INTEGER,' + crlf +
    '     InheritsFrom'#9'UTF8STRING,' + crlf +
    '     UnitName'#9'UTF8STRING,' + crlf +
    '     ClassName'#9'UTF8STRING,' + crlf +
    '     Declaration'#9'UTF8STRING,' + crlf +
    '     Description'#9'UTF8STRING,' + crlf +
    '     Beta'#9'[6] IMPLICIT BOOLEAN DEFAULT 860100,' + crlf +
    '     Deprecated'#9'[7] IMPLICIT BOOLEAN DEFAULT 870100,' + crlf +
    '     Hidden'#9'[8] IMPLICIT BOOLEAN DEFAULT 880100}' + crlf + crlf + crlf +
    'FieldsRecords ::= SEQUENCE OF FieldsRecord' + crlf + crlf +
    'FieldsRecord ::= SEQUENCE {' + crlf +
    '     Name'#9'UTF8STRING,' + crlf +
    '     FileName'#9'UTF8STRING,' + crlf +
    '     Kind'#9'INTEGER,' + crlf +
    '     InheritsFrom'#9'UTF8STRING,' + crlf +
    '     UnitName'#9'UTF8STRING,' + crlf +
    '     ClassName'#9'UTF8STRING,' + crlf +
    '     Declaration'#9'UTF8STRING,' + crlf +
    '     Description'#9'UTF8STRING,' + crlf +
    '     Beta'#9'[6] IMPLICIT BOOLEAN DEFAULT 860100,' + crlf +
    '     Deprecated'#9'[7] IMPLICIT BOOLEAN DEFAULT 870100,' + crlf +
    '     Hidden'#9'[8] IMPLICIT BOOLEAN DEFAULT 880100}' + crlf + crlf + crlf +
    'RoutinesRecords ::= SEQUENCE OF RoutinesRecord' + crlf + crlf +
    'RoutinesRecord ::= SEQUENCE {' + crlf +
    '     Name'#9'UTF8STRING,' + crlf +
    '     FileName'#9'UTF8STRING,' + crlf +
    '     Kind'#9'INTEGER,' + crlf +
    '     InheritsFrom'#9'UTF8STRING,' + crlf +
    '     UnitName'#9'UTF8STRING,' + crlf +
    '     ClassName'#9'UTF8STRING,' + crlf +
    '     Declaration'#9'UTF8STRING,' + crlf +
    '     Description'#9'UTF8STRING,' + crlf +
    '     Beta'#9'[6] IMPLICIT BOOLEAN DEFAULT 860100,' + crlf +
    '     Deprecated'#9'[7] IMPLICIT BOOLEAN DEFAULT 870100,' + crlf +
    '     Hidden'#9'[8] IMPLICIT BOOLEAN DEFAULT 880100}' + crlf + crlf + crlf +
    'END' + crlf + crlf;

type
  TAsnclientData1Records = class;

{$WARNINGS OFF}
  TMember = class(TASNConstructedWrapper)
  private
    function GetBeta: Boolean;
    function GetDeprecated: Boolean;
    procedure SetBeta(const Value: Boolean);
    procedure SetDeprecated(const Value: Boolean);
    function GetHidden: Boolean;
    procedure SetHidden(const Value: Boolean);
  protected
    function GetName: WideString; virtual; abstract;
    procedure SetName(const Value: WideString); virtual; abstract;
    function GetFileName: WideString;
    procedure SetFileName(const Value: WideString); 
    function GetKind: TIntegerWrapper; virtual; abstract;
    procedure SetKind(const Value: TIntegerWrapper); virtual; abstract;
    function GetInheritsFrom: WideString; virtual; abstract;
    procedure SetInheritsFrom(const Value: WideString); virtual; abstract;
    function GetUnitName: WideString; virtual; abstract;
    procedure SetUnitName(const Value: WideString); virtual; abstract;
    function GetClassName: WideString; virtual; abstract;
    procedure SetClassName(const Value: WideString); virtual; abstract;
    function GetDeclaration: WideString; virtual; abstract;
    procedure SetDeclaration(const Value: WideString); virtual; abstract;
    function GetDescription: WideString; virtual; abstract;
    procedure SetDescription(const Value: WideString); virtual; abstract;
    function GetHTMLPath: string;
    function GetTopOwner: TAsnclientData1Records;
    procedure InternalRecordToStrings(List: TStrings); virtual; abstract;
  public
    function LocateAny(const FileName: string): TMember; dynamic;
    function LocateAnyByName(const Name: string): TMember; dynamic;
    procedure ParseFileForDescription;
    procedure ParseFiles; dynamic;
    procedure RecordToFile(const FileName: string);
    function RecordToString: string;
    procedure RecordsToFiles; dynamic;
    function DescriptionEmpty: Boolean;
    function SameName(const Name: string): Boolean; dynamic;
  published
    property Name: WideString read GetName write SetName;
    property FileName: WideString read GetFileName write SetFileName;
    property Kind: TIntegerWrapper read GetKind write SetKind;
    property InheritsFrom: WideString read GetInheritsFrom write SetInheritsFrom;
    property UnitName: WideString read GetUnitName write SetUnitName;
    property ClassName: WideString read GetClassName write SetClassName;
    property Declaration: WideString read GetDeclaration write SetDeclaration;
    property Description: WideString read GetDescription write SetDescription;
    property IsBeta: Boolean read GetBeta write SetBeta;
    property IsDeprecated: Boolean read GetDeprecated write SetDeprecated;
    property IsHidden: Boolean read GetHidden write SetHidden;
  end;
{$WARNINGS ON}

  TMembers = class(TASNCustomOFFieldWrapper)
  protected
    procedure AddNewSubItems(Dst, Src: TMember); virtual;
  public
    procedure AddNew(Src: TMembers);
    procedure RecordsToFiles;
    function Locate(const FileName: string): TMember;
    function LocateAny(const FileName: string): TMember;
    function LocateAnyByName(const Name: string): TMember;
    function LocateByName(const Name: string): TMember;
  end;

  TMethodsRecord = class(TMember)
  protected
    function GetName: WideString; override;
    procedure SetName(const Value: WideString); override;
    function GetKind: TIntegerWrapper; override;
    procedure SetKind(const Value: TIntegerWrapper); override;
    function GetInheritsFrom: WideString; override;
    procedure SetInheritsFrom(const Value: WideString); override;
    function GetUnitName: WideString; override;
    procedure SetUnitName(const Value: WideString); override;
    function GetClassName: WideString; override;
    procedure SetClassName(const Value: WideString); override;
    function GetDeclaration: WideString; override;
    procedure SetDeclaration(const Value: WideString); override;
    function GetDescription: WideString;  override;
    procedure SetDescription(const Value: WideString); override;
    procedure InternalRecordToStrings(List: TStrings); override;
  public
    constructor Create(AOwner: TPersistent; AData, ATemplate: TASN1Struct); override;
    destructor Destroy; override;
    function SameName(const Name: string): Boolean; override;
  end;

  TMethodsRecords = class(TMembers)
  private
    function GetItems(Index: Integer): TMethodsRecord;
  public
    constructor Create(AOwner: TPersistent; AData, ATemplate: TASN1Struct); override;
    destructor Destroy; override;
    function Add: TMethodsRecord;
    property Items[Index: Integer]: TMethodsRecord read GetItems; default;
  end;

  TPublicPropertiesRecord = class(TMember)
  protected
    function GetName: WideString; override;
    procedure SetName(const Value: WideString); override;
    function GetKind: TIntegerWrapper; override;
    procedure SetKind(const Value: TIntegerWrapper); override;
    function GetInheritsFrom: WideString; override;
    procedure SetInheritsFrom(const Value: WideString); override;
    function GetUnitName: WideString; override;
    procedure SetUnitName(const Value: WideString); override;
    function GetClassName: WideString; override;
    procedure SetClassName(const Value: WideString); override;
    function GetDeclaration: WideString; override;
    procedure SetDeclaration(const Value: WideString); override;
    function GetDescription: WideString; override;
    procedure SetDescription(const Value: WideString); override;
    procedure InternalRecordToStrings(List: TStrings); override;
  public
    constructor Create(AOwner: TPersistent; AData, ATemplate: TASN1Struct); override;
    destructor Destroy; override;
    function SameName(const Name: string): Boolean; override;
  end;

  TPublicPropertiesRecords = class(TMembers)
  private
    function GetItems(Index: Integer): TPublicPropertiesRecord;
  public
    constructor Create(AOwner: TPersistent; AData, ATemplate: TASN1Struct); override;
    destructor Destroy; override;
    function Add: TPublicPropertiesRecord;
    property Items[Index: Integer]: TPublicPropertiesRecord read GetItems; default;
  end;

  TPublishedPropertiesRecord = class(TMember)
  protected
    function GetName: WideString; override;
    procedure SetName(const Value: WideString); override;
    function GetKind: TIntegerWrapper; override;
    procedure SetKind(const Value: TIntegerWrapper); override;
    function GetInheritsFrom: WideString; override;
    procedure SetInheritsFrom(const Value: WideString); override;
    function GetUnitName: WideString; override;
    procedure SetUnitName(const Value: WideString); override;
    function GetClassName: WideString; override;
    procedure SetClassName(const Value: WideString); override;
    function GetDeclaration: WideString; override;
    procedure SetDeclaration(const Value: WideString); override;
    function GetDescription: WideString; override;
    procedure SetDescription(const Value: WideString); override;
    procedure InternalRecordToStrings(List: TStrings); override;
  public
    constructor Create(AOwner: TPersistent; AData, ATemplate: TASN1Struct); override;
    destructor Destroy; override;                 
    function SameName(const Name: string): Boolean; override;
  end;

  TPublishedPropertiesRecords = class(TMembers)
  private
    function GetItems(Index: Integer): TPublishedPropertiesRecord;
  public
    constructor Create(AOwner: TPersistent; AData, ATemplate: TASN1Struct); override;
    destructor Destroy; override;
    function Add: TPublishedPropertiesRecord;
    property Items[Index: Integer]: TPublishedPropertiesRecord read GetItems; default;
  end;

  TFieldsRecord = class(TMember)
  protected
    function GetName: WideString; override;
    procedure SetName(const Value: WideString); override;
    function GetKind: TIntegerWrapper; override;
    procedure SetKind(const Value: TIntegerWrapper); override;
    function GetInheritsFrom: WideString; override;
    procedure SetInheritsFrom(const Value: WideString); override;
    function GetUnitName: WideString; override;
    procedure SetUnitName(const Value: WideString); override;
    function GetClassName: WideString; override;
    procedure SetClassName(const Value: WideString); override;
    function GetDeclaration: WideString; override;
    procedure SetDeclaration(const Value: WideString); override;
    function GetDescription: WideString; override;
    procedure SetDescription(const Value: WideString); override;   
    procedure InternalRecordToStrings(List: TStrings); override;
  public
    constructor Create(AOwner: TPersistent; AData, ATemplate: TASN1Struct); override;
    destructor Destroy; override;
  end;

  TFieldsRecords = class(TMembers)
  private
    function GetItems(Index: Integer): TFieldsRecord;
  public
    constructor Create(AOwner: TPersistent; AData, ATemplate: TASN1Struct); override;
    destructor Destroy; override;
    function Add: TFieldsRecord;
    property Items[Index: Integer]: TFieldsRecord read GetItems; default;
  end;

  TClassesRecord = class(TMember)
  protected
    function GetName: WideString; override;
    procedure SetName(const Value: WideString); override;
    function GetKind: TIntegerWrapper; override;
    procedure SetKind(const Value: TIntegerWrapper); override;
    function GetInheritsFrom: WideString; override;
    procedure SetInheritsFrom(const Value: WideString); override;
    function GetUnitName: WideString; override;
    procedure SetUnitName(const Value: WideString); override;
    function GetClassName: WideString; override;
    procedure SetClassName(const Value: WideString); override;
    function GetDeclaration: WideString; override;
    procedure SetDeclaration(const Value: WideString); override;
    function GetDescription: WideString; override;
    procedure SetDescription(const Value: WideString); override;
    function GetMethods: TMethodsRecords;
    procedure SetMethods(const Value: TMethodsRecords);
    function GetPublicProperties: TPublicPropertiesRecords;
    procedure SetPublicProperties(const Value: TPublicPropertiesRecords);
    function GetPublishedProperties: TPublishedPropertiesRecords;
    procedure SetPublishedProperties(const Value: TPublishedPropertiesRecords);
    function GetFields: TFieldsRecords;
    procedure SetFields(const Value: TFieldsRecords);    
    procedure InternalRecordToStrings(List: TStrings); override;
  public
    constructor Create(AOwner: TPersistent; AData, ATemplate: TASN1Struct); override;
    destructor Destroy; override;
    function AddMember(Kind: Integer): TMember;
    function FindMethod(const MethodName: string): TMethodsRecord;
    function LocateAny(const FileName: string): TMember; override;
    function LocateAnyByName(const Name: string): TMember; override;
    procedure ParseFiles; override;
    procedure RecordsToFiles; override;
  published
    property Methods: TMethodsRecords read GetMethods write SetMethods;
    property PublicProperties: TPublicPropertiesRecords read GetPublicProperties write SetPublicProperties;
    property PublishedProperties: TPublishedPropertiesRecords read GetPublishedProperties write SetPublishedProperties;
    property Fields: TFieldsRecords read GetFields write SetFields;
  end;

  TClassesRecords = class(TMembers)
  private
    function GetItems(Index: Integer): TClassesRecord;
  protected
    procedure AddNewSubItems(Dst, Src: TMember); override;
  public
    constructor Create(AOwner: TPersistent; AData, ATemplate: TASN1Struct); override;
    destructor Destroy; override;
    function Add: TClassesRecord;
    property Items[Index: Integer]: TClassesRecord read GetItems; default;
  end;

  TRoutinesRecord = class(TMember)
  protected
    function GetName: WideString; override;
    procedure SetName(const Value: WideString); override;
    function GetKind: TIntegerWrapper; override;
    procedure SetKind(const Value: TIntegerWrapper); override;
    function GetInheritsFrom: WideString; override;
    procedure SetInheritsFrom(const Value: WideString); override;
    function GetUnitName: WideString; override;
    procedure SetUnitName(const Value: WideString); override;
    function GetClassName: WideString; override;
    procedure SetClassName(const Value: WideString); override;
    function GetDeclaration: WideString; override;
    procedure SetDeclaration(const Value: WideString); override;
    function GetDescription: WideString; override;
    procedure SetDescription(const Value: WideString); override;    
    procedure InternalRecordToStrings(List: TStrings); override;
  public
    constructor Create(AOwner: TPersistent; AData, ATemplate: TASN1Struct); override;
    destructor Destroy; override;
  end;

  TRoutinesRecords = class(TMembers)
  private
    function GetItems(Index: Integer): TRoutinesRecord;
  public
    constructor Create(AOwner: TPersistent; AData, ATemplate: TASN1Struct); override;
    destructor Destroy; override;
    function Add: TRoutinesRecord;
    property Items[Index: Integer]: TRoutinesRecord read GetItems; default;
  end;

  TAsnclientData1Record = class(TMember)
  private
    function GetIntfSect: TFieldsRecord;
    procedure SetIntfSect(const Value: TFieldsRecord);
  protected
    function GetName: WideString; override;
    procedure SetName(const Value: WideString); override;
    function GetKind: TIntegerWrapper; override;
    procedure SetKind(const Value: TIntegerWrapper); override;
    function GetInheritsFrom: WideString; override;
    procedure SetInheritsFrom(const Value: WideString); override;
    function GetUnitName: WideString; override;
    procedure SetUnitName(const Value: WideString); override;
    function GetClassName: WideString; override;
    procedure SetClassName(const Value: WideString); override;
    function GetDeclaration: WideString; override;
    procedure SetDeclaration(const Value: WideString); override;
    function GetDescription: WideString; override;
    procedure SetDescription(const Value: WideString); override;
    function GetClasses: TClassesRecords;
    procedure SetClasses(const Value: TClassesRecords);
    function GetRoutines: TRoutinesRecords;
    procedure SetRoutines(const Value: TRoutinesRecords); 
    procedure InternalRecordToStrings(List: TStrings); override;
  public
    constructor Create(AOwner: TPersistent; AData, ATemplate: TASN1Struct); override;
    destructor Destroy; override;
    function LocateAny(const FileName: string): TMember; override;
    function LocateAnyByName(const Name: string): TMember; override;
    procedure ParseFiles; override;
    procedure RecordsToFiles; override;
  published
    property Classes: TClassesRecords read GetClasses write SetClasses;
    property Routines: TRoutinesRecords read GetRoutines write SetRoutines;
    property IntfSect: TFieldsRecord read GetIntfSect write SetIntfSect;
  end;

  TAsnclientData1Records = class(TMembers)
  private
    FHTMLPath: string;
    function GetItems(Index: Integer): TAsnclientData1Record;
    function GetHTMLPath: string;
    procedure SetHTMLPath(const Value: string);
  protected         
    procedure AddNewSubItems(Dst, Src: TMember); override;
  public
    constructor Create(AOwner: TPersistent; AData, ATemplate: TASN1Struct); override;
    destructor Destroy; override;
    function Add: TAsnclientData1Record;
    function FindClass(const UnitName, ClassName: string): TClassesRecord;
    function FindUnit(const UnitName: string): TAsnclientData1Record;
    procedure LoadFromStream(AStream: TStream); override;
    procedure ParseFiles;
    property Items[Index: Integer]: TAsnclientData1Record read GetItems; default;
    property HTMLPath: string read GetHTMLPath write SetHTMLPath;
  end;

implementation

uses
  Registry, FastHelpHTML;

var
  GlobalObject: TASN1Struct;

procedure InitGlobalObject;
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(ASNModule);
  try
    GlobalObject := TASN1Struct.Create;
    GlobalObject._AddRef;
    GlobalObject.LoadFromStream(SS,fmtASN1);
  finally
    SS.Free;
  end;
end;

{ TMethodsRecord }

constructor TMethodsRecord.Create;
begin
  inherited Create(AOwner,AData,ATemplate);
  FieldFactory(TWideStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[1]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[2]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[3]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[4]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[5]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[6]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[7]^,nil^);
  FieldFactory(TBooleanWrapper,nil,FData.Items[8]^,nil^);
  FieldFactory(TBooleanWrapper,nil,FData.Items[9]^,nil^);  
  FieldFactory(TBooleanWrapper,nil,FData.Items[10]^,nil^);
end;

destructor TMethodsRecord.Destroy;
begin
  inherited Destroy;
end;

function TMethodsRecord.GetName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[0]).Value);
end;

procedure TMethodsRecord.SetName(const Value: WideString);
begin
  TWideStringWrapper(FItems[0]).Value := Value;
end;

function TMethodsRecord.GetKind: TIntegerWrapper;
begin
  Result := FItems[2];
end;

procedure TMethodsRecord.SetKind(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[2]).Assign(Value);
end;

function TMethodsRecord.GetInheritsFrom: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[3]).Value);
end;

procedure TMethodsRecord.SetInheritsFrom(const Value: WideString);
begin
  TWideStringWrapper(FItems[3]).Value := Value;
end;

function TMethodsRecord.GetUnitName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[4]).Value);
end;

procedure TMethodsRecord.SetUnitName(const Value: WideString);
begin
  TWideStringWrapper(FItems[4]).Value := Value;
end;

function TMethodsRecord.GetClassName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[5]).Value);
end;

procedure TMethodsRecord.SetClassName(const Value: WideString);
begin
  TWideStringWrapper(FItems[5]).Value := Value;
end;

function TMethodsRecord.GetDeclaration: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[6]).Value);
end;

procedure TMethodsRecord.SetDeclaration(const Value: WideString);
begin
  TWideStringWrapper(FItems[6]).Value := Value;
end;

function TMethodsRecord.GetDescription: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[7]).Value);
end;

procedure TMethodsRecord.SetDescription(const Value: WideString);
begin
  TWideStringWrapper(FItems[7]).Value := Value;
end;

procedure TMethodsRecord.InternalRecordToStrings(List: TStrings);
var
  ClassItem: TClassesRecord;
  Method: TMethodsRecord;
  P: Integer;
  MethodName: string;
  SL: TStringList;
begin
  List.Add(Format('<h3>%s</h3>',[Name]));
  List.Add(Format('<p><b>Declaration:</b><br>'#13#10'<font face="Courier New">%s</font></p>',
                  [Declaration]));
  SL := TStringList.Create;
  try
    ClassItem := TClassesRecord(Owner.Owner);
    while ClassItem.InheritsFrom <> '' do begin
      ClassItem := GetTopOwner.FindClass('',ClassItem.InheritsFrom);
      if ClassItem = nil then Break;
      MethodName := Name;
      P := Pos('.',MethodName);
      Delete(MethodName,1,P);
      Method := ClassItem.FindMethod(MethodName);
      if Assigned(Method) then
        SL.Add(Format('<br>'#13#10'<a href=%s>%s.%s</a>',
                      [Method.FileName,Method.ClassName,MethodName]));
    end;
    if SL.Count > 0 then
      List.Add(Format('<p><b>Inherits from:</b>%s</p>',
                      [SL.Text]));
  finally
    SL.Free;
  end;
  List.Add(Format('<p><b>Description:</b><br>'#13#10'%s</p>',
                  [Description]));
end;

function TMethodsRecord.SameName(const Name: string): Boolean;
var
  P: Integer;
begin
  Result := inherited SameName(Name);
  if not Result then begin
    P := Pos('.',Self.Name);
    if P > 0 then
      Result := AnsiCompareText(Name,Copy(Self.Name,P+1,MaxInt)) = 0;
  end;
end;

{ TMethodsRecords }

constructor TMethodsRecords.Create;
begin
  FFieldType := TMethodsRecord;
  inherited Create(AOwner,AData,ATemplate);
end;

destructor TMethodsRecords.Destroy;
begin
  inherited Destroy;
end;

function TMethodsRecords.Add: TMethodsRecord;
begin
  Result := TMethodsRecord(InternalAdd);
end;

function TMethodsRecords.GetItems(Index: Integer): TMethodsRecord;
begin
  Result := TMethodsRecord(InternalGetItems(Index));
end;

{ TPublicPropertiesRecord }

constructor TPublicPropertiesRecord.Create;
begin
  inherited Create(AOwner,AData,ATemplate);
  FieldFactory(TWideStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[1]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[2]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[3]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[4]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[5]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[6]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[7]^,nil^);   
  FieldFactory(TBooleanWrapper,nil,FData.Items[8]^,nil^);
  FieldFactory(TBooleanWrapper,nil,FData.Items[9]^,nil^); 
  FieldFactory(TBooleanWrapper,nil,FData.Items[10]^,nil^);
end;

destructor TPublicPropertiesRecord.Destroy;
begin
  inherited Destroy;
end;

function TPublicPropertiesRecord.GetName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[0]).Value);
end;

procedure TPublicPropertiesRecord.SetName(const Value: WideString);
begin
  TWideStringWrapper(FItems[0]).Value := Value;
end;

function TPublicPropertiesRecord.GetKind: TIntegerWrapper;
begin
  Result := FItems[2];
end;

procedure TPublicPropertiesRecord.SetKind(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[2]).Assign(Value);
end;

function TPublicPropertiesRecord.GetInheritsFrom: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[3]).Value);
end;

procedure TPublicPropertiesRecord.SetInheritsFrom(const Value: WideString);
begin
  TWideStringWrapper(FItems[3]).Value := Value;
end;

function TPublicPropertiesRecord.GetUnitName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[4]).Value);
end;

procedure TPublicPropertiesRecord.SetUnitName(const Value: WideString);
begin
  TWideStringWrapper(FItems[4]).Value := Value;
end;

function TPublicPropertiesRecord.GetClassName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[5]).Value);
end;

procedure TPublicPropertiesRecord.SetClassName(const Value: WideString);
begin
  TWideStringWrapper(FItems[5]).Value := Value;
end;

function TPublicPropertiesRecord.GetDeclaration: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[6]).Value);
end;

procedure TPublicPropertiesRecord.SetDeclaration(const Value: WideString);
begin
  TWideStringWrapper(FItems[6]).Value := Value;
end;

function TPublicPropertiesRecord.GetDescription: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[7]).Value);
end;

procedure TPublicPropertiesRecord.SetDescription(const Value: WideString);
begin
  TWideStringWrapper(FItems[7]).Value := Value;
end;

procedure TPublicPropertiesRecord.InternalRecordToStrings(List: TStrings);
begin
  List.Add(Format('<h3>%s</h3>',[Name]));
  List.Add(Format('<p><b>Declaration:</b><br>'#13#10'<font face="Courier New">%s</font></p>',
                  [Declaration]));
  List.Add(Format('<p><b>Description:</b><br>'#13#10'%s</p>',
                  [Description]));
end;

function TPublicPropertiesRecord.SameName(const Name: string): Boolean;
var
  P: Integer;
begin
  Result := inherited SameName(Name);
  if not Result then begin
    P := Pos('.',Self.Name);
    if P > 0 then
      Result := AnsiCompareText(Name,Copy(Self.Name,P+1,MaxInt)) = 0;
  end;
end;

{ TPublicPropertiesRecords }

constructor TPublicPropertiesRecords.Create;
begin
  FFieldType := TPublicPropertiesRecord;
  inherited Create(AOwner,AData,ATemplate);
end;

destructor TPublicPropertiesRecords.Destroy;
begin
  inherited Destroy;
end;

function TPublicPropertiesRecords.Add: TPublicPropertiesRecord;
begin
  Result := TPublicPropertiesRecord(InternalAdd);
end;

function TPublicPropertiesRecords.GetItems(Index: Integer): TPublicPropertiesRecord;
begin
  Result := TPublicPropertiesRecord(InternalGetItems(Index));
end;

{ TPublishedPropertiesRecord }

constructor TPublishedPropertiesRecord.Create;
begin
  inherited Create(AOwner,AData,ATemplate);
  FieldFactory(TWideStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[1]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[2]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[3]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[4]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[5]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[6]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[7]^,nil^);   
  FieldFactory(TBooleanWrapper,nil,FData.Items[8]^,nil^);
  FieldFactory(TBooleanWrapper,nil,FData.Items[9]^,nil^);        
  FieldFactory(TBooleanWrapper,nil,FData.Items[10]^,nil^);
end;

destructor TPublishedPropertiesRecord.Destroy;
begin
  inherited Destroy;
end;

function TPublishedPropertiesRecord.GetName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[0]).Value);
end;

procedure TPublishedPropertiesRecord.SetName(const Value: WideString);
begin
  TWideStringWrapper(FItems[0]).Value := Value;
end;

function TPublishedPropertiesRecord.GetKind: TIntegerWrapper;
begin
  Result := FItems[2];
end;

procedure TPublishedPropertiesRecord.SetKind(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[2]).Assign(Value);
end;

function TPublishedPropertiesRecord.GetInheritsFrom: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[3]).Value);
end;

procedure TPublishedPropertiesRecord.SetInheritsFrom(const Value: WideString);
begin
  TWideStringWrapper(FItems[3]).Value := Value;
end;

function TPublishedPropertiesRecord.GetUnitName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[4]).Value);
end;

procedure TPublishedPropertiesRecord.SetUnitName(const Value: WideString);
begin
  TWideStringWrapper(FItems[4]).Value := Value;
end;

function TPublishedPropertiesRecord.GetClassName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[5]).Value);
end;

procedure TPublishedPropertiesRecord.SetClassName(const Value: WideString);
begin
  TWideStringWrapper(FItems[5]).Value := Value;
end;

function TPublishedPropertiesRecord.GetDeclaration: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[6]).Value);
end;

procedure TPublishedPropertiesRecord.SetDeclaration(const Value: WideString);
begin
  TWideStringWrapper(FItems[6]).Value := Value;
end;

function TPublishedPropertiesRecord.GetDescription: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[7]).Value);
end;

procedure TPublishedPropertiesRecord.SetDescription(const Value: WideString);
begin
  TWideStringWrapper(FItems[7]).Value := Value;
end;  

procedure TPublishedPropertiesRecord.InternalRecordToStrings(List: TStrings);
begin
  List.Add(Format('<h3>%s</h3>',[Name]));
  List.Add(Format('<p><b>Declaration:</b><br>'#13#10'<font face="Courier New">%s</font></p>',
                  [Declaration]));
  List.Add(Format('<p><b>Description:</b><br>'#13#10'%s</p>',
                  [Description]));
end;

function TPublishedPropertiesRecord.SameName(const Name: string): Boolean;
var
  P: Integer;
begin
  Result := inherited SameName(Name);
  if not Result then begin
    P := Pos('.',Self.Name);
    if P > 0 then
      Result := AnsiCompareText(Name,Copy(Self.Name,P+1,MaxInt)) = 0;
  end;
end;

{ TPublishedPropertiesRecords }

constructor TPublishedPropertiesRecords.Create;
begin
  FFieldType := TPublishedPropertiesRecord;
  inherited Create(AOwner,AData,ATemplate);
end;

destructor TPublishedPropertiesRecords.Destroy;
begin
  inherited Destroy;
end;

function TPublishedPropertiesRecords.Add: TPublishedPropertiesRecord;
begin
  Result := TPublishedPropertiesRecord(InternalAdd);
end;

function TPublishedPropertiesRecords.GetItems(Index: Integer): TPublishedPropertiesRecord;
begin
  Result := TPublishedPropertiesRecord(InternalGetItems(Index));
end;

{ TFieldsRecord }

constructor TFieldsRecord.Create;
begin
  inherited Create(AOwner,AData,ATemplate);
  FieldFactory(TWideStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[1]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[2]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[3]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[4]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[5]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[6]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[7]^,nil^);    
  FieldFactory(TBooleanWrapper,nil,FData.Items[8]^,nil^);
  FieldFactory(TBooleanWrapper,nil,FData.Items[9]^,nil^);
  FieldFactory(TBooleanWrapper,nil,FData.Items[10]^,nil^);
end;

destructor TFieldsRecord.Destroy;
begin
  inherited Destroy;
end;

function TFieldsRecord.GetName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[0]).Value);
end;

procedure TFieldsRecord.SetName(const Value: WideString);
begin
  TWideStringWrapper(FItems[0]).Value := Value;
end;

function TFieldsRecord.GetKind: TIntegerWrapper;
begin
  Result := FItems[2];
end;

procedure TFieldsRecord.SetKind(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[2]).Assign(Value);
end;

function TFieldsRecord.GetInheritsFrom: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[3]).Value);
end;

procedure TFieldsRecord.SetInheritsFrom(const Value: WideString);
begin
  TWideStringWrapper(FItems[3]).Value := Value;
end;

function TFieldsRecord.GetUnitName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[4]).Value);
end;

procedure TFieldsRecord.SetUnitName(const Value: WideString);
begin
  TWideStringWrapper(FItems[4]).Value := Value;
end;

function TFieldsRecord.GetClassName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[5]).Value);
end;

procedure TFieldsRecord.SetClassName(const Value: WideString);
begin
  TWideStringWrapper(FItems[5]).Value := Value;
end;

function TFieldsRecord.GetDeclaration: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[6]).Value);
end;

procedure TFieldsRecord.SetDeclaration(const Value: WideString);
begin
  TWideStringWrapper(FItems[6]).Value := Value;
end;

function TFieldsRecord.GetDescription: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[7]).Value);
end;

procedure TFieldsRecord.SetDescription(const Value: WideString);
begin
  TWideStringWrapper(FItems[7]).Value := Value;
end;

procedure TFieldsRecord.InternalRecordToStrings(List: TStrings);
begin
  List.Add(Format('<h3>%s</h3>',[Name]));
  List.Add(Format('<p><b>Declaration:</b><br>'#13#10'<font face="Courier New">%s</font></p>',
                  [Declaration]));
  if Description <> '' then
    List.Add(Format('<p><b>Description:</b><br>'#13#10'%s</p>',
                    [Description]));
end;

{ TFieldsRecords }

constructor TFieldsRecords.Create;
begin
  FFieldType := TFieldsRecord;
  inherited Create(AOwner,AData,ATemplate);
end;

destructor TFieldsRecords.Destroy;
begin
  inherited Destroy;
end;

function TFieldsRecords.Add: TFieldsRecord;
begin
  Result := TFieldsRecord(InternalAdd);
end;

function TFieldsRecords.GetItems(Index: Integer): TFieldsRecord;
begin
  Result := TFieldsRecord(InternalGetItems(Index));
end;

{ TClassesRecord }

constructor TClassesRecord.Create;
begin
  inherited Create(AOwner,AData,ATemplate);
  FieldFactory(TWideStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[1]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[2]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[3]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[4]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[5]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[6]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[7]^,nil^); 
  FieldFactory(TBooleanWrapper,nil,FData.Items[8]^,nil^);
  FieldFactory(TBooleanWrapper,nil,FData.Items[9]^,nil^);         
  FieldFactory(TBooleanWrapper,nil,FData.Items[10]^,nil^);
  FieldFactory(TMethodsRecords,nil,FData.Items[11]^,nil^);
  FieldFactory(TPublicPropertiesRecords,nil,FData.Items[12]^,nil^);
  FieldFactory(TPublishedPropertiesRecords,nil,FData.Items[13]^,nil^);
  FieldFactory(TFieldsRecords,nil,FData.Items[14]^,nil^);
end;

destructor TClassesRecord.Destroy;
begin
  inherited Destroy;
end;

function TClassesRecord.GetName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[0]).Value);
end;

procedure TClassesRecord.SetName(const Value: WideString);
begin
  TWideStringWrapper(FItems[0]).Value := Value;
end;

function TClassesRecord.GetKind: TIntegerWrapper;
begin
  Result := FItems[2];
end;

procedure TClassesRecord.SetKind(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[2]).Assign(Value);
end;

function TClassesRecord.GetInheritsFrom: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[3]).Value);
end;

procedure TClassesRecord.SetInheritsFrom(const Value: WideString);
begin
  TWideStringWrapper(FItems[3]).Value := Value;
end;

function TClassesRecord.GetUnitName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[4]).Value);
end;

procedure TClassesRecord.SetUnitName(const Value: WideString);
begin
  TWideStringWrapper(FItems[4]).Value := Value;
end;

function TClassesRecord.GetClassName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[5]).Value);
end;

procedure TClassesRecord.SetClassName(const Value: WideString);
begin
  TWideStringWrapper(FItems[5]).Value := Value;
end;

function TClassesRecord.GetDeclaration: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[6]).Value);
end;

procedure TClassesRecord.SetDeclaration(const Value: WideString);
begin
  TWideStringWrapper(FItems[6]).Value := Value;
end;

function TClassesRecord.GetDescription: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[7]).Value);
end;

procedure TClassesRecord.SetDescription(const Value: WideString);
begin
  TWideStringWrapper(FItems[7]).Value := Value;
end;

function TClassesRecord.GetMethods: TMethodsRecords;
begin
  Result := FItems[11];
end;

procedure TClassesRecord.SetMethods(const Value: TMethodsRecords);
begin
  TMethodsRecords(FItems[11]).Assign(Value);
end;

function TClassesRecord.GetPublicProperties: TPublicPropertiesRecords;
begin
  Result := FItems[12];
end;

procedure TClassesRecord.SetPublicProperties(const Value: TPublicPropertiesRecords);
begin
  TPublicPropertiesRecords(FItems[12]).Assign(Value);
end;

function TClassesRecord.GetPublishedProperties: TPublishedPropertiesRecords;
begin
  Result := FItems[13];
end;

procedure TClassesRecord.SetPublishedProperties(const Value: TPublishedPropertiesRecords);
begin
  TPublishedPropertiesRecords(FItems[13]).Assign(Value);
end;

function TClassesRecord.GetFields: TFieldsRecords;
begin
  Result := FItems[14];
end;

procedure TClassesRecord.SetFields(const Value: TFieldsRecords);
begin
  TFieldsRecords(FItems[14]).Assign(Value);
end;

function TClassesRecord.AddMember(Kind: Integer): TMember;
begin
  case Kind of
    3: Result := Methods.Add;
    4: Result := PublicProperties.Add;
    5: Result := PublishedProperties.Add;
    6: Result := Fields.Add;
  else
    Result := nil;
  end;
end;

procedure TClassesRecord.ParseFiles;
var
  I: Integer;
begin
  inherited;
  for I := 0 to Fields.Count - 1 do
    Fields[I].ParseFiles;
  if Kind.AsInteger = 1 then begin
    for I := 0 to Methods.Count - 1 do
      Methods[I].ParseFiles;
    for I := 0 to PublicProperties.Count - 1 do
      PublicProperties[I].ParseFiles;
    for I := 0 to PublishedProperties.Count - 1 do
      PublishedProperties[I].ParseFiles;
  end;
end;

function TClassesRecord.FindMethod(
  const MethodName: string): TMethodsRecord;
var
  I: Integer;
  MN: string;
begin
  MN := Name + '.' + MethodName;
  for I := 0 to Methods.Count - 1 do begin
    Result := Methods[I];
    if AnsiCompareStr(Result.Name,MN) = 0 then
      Exit;
  end;
  Result := nil;
end;

procedure TClassesRecord.InternalRecordToStrings(List: TStrings);
var
  Cls, Flds, PublM, PublP, Publ: TStringList;
  ClassItem: TClassesRecord;
  Member: TMember;
  S: string;
  I, J, P: Integer;
begin
  if Kind.AsInteger = 1 then begin
    List.Add(Format('<h3>Class %s</h3>',[Name]));
    List.Add(Format('<p><b>Declaration:</b><br>'#13#10'<font face="Courier New">%s</font></p>',
                    [Declaration]));
    List.Add(Format('<p><b>Description:</b><br>'#13#10'%s</p>',
                    [Description]));
    Cls := TStringList.Create;
    Flds := TStringList.Create;
    PublP := TStringList.Create;
    PublM := TStringList.Create;
    Publ := TStringList.Create;
    try
      ClassItem := Self;
      repeat
        Cls.AddObject(ClassItem.Name,ClassItem);
        if ClassItem.InheritsFrom <> '' then
          ClassItem := GetTopOwner.FindClass('',ClassItem.InheritsFrom)
        else
          ClassItem := nil;
      until ClassItem = nil;
      for I := 0 to Cls.Count - 1 do begin
        ClassItem := TClassesRecord(Cls.Objects[I]);
        for J := 0 to ClassItem.Methods.Count - 1 do begin
          Member := ClassItem.Methods[J];
          if Member.IsHidden then Continue;
          P := Pos('.',Member.Name);
          S := Copy(Member.Name,P+1,MaxInt);
          S := S + StringOfChar(' ',64 - Length(S));
          S := Format('%s%4x <a href="%s">%s</a><br>',
                      [S,I,Member.FileName,StringReplace(Member.Name,' ','_',[rfReplaceAll])]);
          PublM.Add(S);
        end;
        for J := 0 to ClassItem.PublicProperties.Count - 1 do begin
          Member := ClassItem.PublicProperties[J];
          if Member.IsHidden then Continue;
          P := Pos('.',Member.Name);
          S := Copy(Member.Name,P+1,MaxInt);
          S := S + StringOfChar(' ',64 - Length(S));
          S := Format('%s%4x <a href="%s">%s</a><br>',
                      [S,I,Member.FileName,StringReplace(Member.Name,' ','_',[rfReplaceAll])]);
          PublP.Add(S);
        end;
        for J := 0 to ClassItem.PublishedProperties.Count - 1 do begin
          Member := ClassItem.PublishedProperties[J];
          if Member.IsHidden then Continue;
          P := Pos('.',Member.Name);
          S := Copy(Member.Name,P+1,MaxInt);
          S := S + StringOfChar(' ',64 - Length(S));
          S := Format('%s%4x <a href="%s">%s</a><br>',
                      [S,I,Member.FileName,StringReplace(Member.Name,' ','_',[rfReplaceAll])]);
          Publ.Add(S);
        end;
        for J := 0 to ClassItem.Fields.Count - 1 do begin
          Member := ClassItem.Fields[J];
          if Member.IsHidden then Continue;
          P := Pos('.',Member.Name);
          S := Copy(Member.Name,P+1,MaxInt);
          S := S + StringOfChar(' ',64 - Length(S));
          S := Format('%s%4x <a href="%s">%s</a><br>',
                      [S,I,Member.FileName,StringReplace(Member.Name,' ','_',[rfReplaceAll])]);
          Flds.Add(S);
        end;
      end;
      if PublM.Count > 0 then begin
        PublM.Sort;
        List.Add('<h4>Public methods</h4>');
        S := '';
        for I := 0 to PublM.Count - 1 do begin
          if CompareStr(Copy(PublM[I],1,64),S) = 0 then
            Continue;
          S := Copy(PublM[I],1,64);
          List.Add(Copy(PublM[I],70,MaxInt));
        end;
      end;
      if PublP.Count > 0 then begin
        PublP.Sort;
        List.Add('<h4>Public properties</h4>');
        S := '';
        for I := 0 to PublP.Count - 1 do begin
          if CompareStr(Copy(PublP[I],1,64),S) = 0 then
            Continue;
          S := Copy(PublP[I],1,64);
          List.Add(Copy(PublP[I],70,MaxInt));
        end;
      end;
      if Publ.Count > 0 then begin
        Publ.Sort;
        List.Add('<h4>Published properties</h4>');
        S := '';
        for I := 0 to Publ.Count - 1 do begin
          if CompareStr(Copy(Publ[I],1,64),S) = 0 then
            Continue;
          S := Copy(Publ[I],1,64);
          List.Add(Copy(Publ[I],70,MaxInt));
        end;
      end;
      if Flds.Count > 0 then begin
        Flds.Sort;
        List.Add('<h4>Fields</h4>');
        S := '';
        for I := 0 to Flds.Count - 1 do begin
          if CompareStr(Copy(Flds[I],1,64),S) = 0 then
            Continue;
          S := Copy(Flds[I],1,64);
          List.Add(Copy(Flds[I],70,MaxInt));
        end;
      end;
    finally
      Flds.Free;
      PublP.Free;
      PublM.Free;
      Publ.Free;
      Cls.Free;
    end;
  end else begin
    List.Add(Format('<h3>%s</h3>',[Name]));
    List.Add(Format('<p>%s</p>',[Description]));
    if Methods.Count > 0 then begin
      List.Add('<h4>Topics:</h4>');
      for I := 0 to Methods.Count - 1 do
        List.Add(Format('<a href="%s">%s</a><br>',
                        [Methods[I].FileName,StringReplace(Methods[I].Name,' ','_',[rfReplaceAll])]));
    end;
    if Fields.Count > 0 then begin
      List.Add('<h4>See also:</h4>');
      for I := 0 to Fields.Count - 1 do
        List.Add(Format('<a href="%s">%s</a><br>',
                        [Fields[I].Description,StringReplace(Fields[I].Name,' ','_',[rfReplaceAll])]));
    end;
  end;
end;

procedure TClassesRecord.RecordsToFiles;
begin
  inherited;
  Methods.RecordsToFiles;
  if Kind.AsInteger = 1 then begin
    PublicProperties.RecordsToFiles;
    PublishedProperties.RecordsToFiles;
    Fields.RecordsToFiles;
  end;
end;

function TClassesRecord.LocateAny(const FileName: string): TMember;
begin
  if Pos(ExtractFilePath(FileName),FileName) <> 1 then
    Result := nil
  else begin
    Result := Methods.Locate(FileName);
    if Result = nil then begin
      Result := PublicProperties.Locate(FileName);
      if Result = nil then begin
        Result := PublishedProperties.Locate(FileName);
        if Result = nil then
          Result := Fields.Locate(FileName);
      end;
    end;
  end;
end;

function TClassesRecord.LocateAnyByName(const Name: string): TMember;
begin
  Result := Methods.LocateByName(Name);
  if Result = nil then begin
    Result := PublicProperties.LocateByName(Name);
    if Result = nil then begin
      Result := PublishedProperties.LocateByName(Name);
      if Result = nil then
        Result := Fields.LocateByName(Name);
    end;
  end;
end;

{ TClassesRecords }

constructor TClassesRecords.Create;
begin
  FFieldType := TClassesRecord;
  inherited Create(AOwner,AData,ATemplate);
end;

destructor TClassesRecords.Destroy;
begin
  inherited Destroy;
end;

function TClassesRecords.Add: TClassesRecord;
begin
  Result := TClassesRecord(InternalAdd);
end;

function TClassesRecords.GetItems(Index: Integer): TClassesRecord;
begin
  Result := TClassesRecord(InternalGetItems(Index));
end;

procedure TClassesRecords.AddNewSubItems(Dst, Src: TMember);
var
  D, S: TClassesRecord;
begin
  D := TClassesRecord(Dst);
  S := TClassesRecord(Src);
  D.Fields.AddNew(S.Fields);
  D.Methods.AddNew(S.Methods);
  D.PublicProperties.AddNew(S.PublicProperties);
  D.PublishedProperties.AddNew(S.PublishedProperties);
end;

{ TRoutinesRecord }

constructor TRoutinesRecord.Create;
begin
  inherited Create(AOwner,AData,ATemplate);
  FieldFactory(TWideStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[1]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[2]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[3]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[4]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[5]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[6]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[7]^,nil^); 
  FieldFactory(TBooleanWrapper,nil,FData.Items[8]^,nil^);
  FieldFactory(TBooleanWrapper,nil,FData.Items[9]^,nil^);     
  FieldFactory(TBooleanWrapper,nil,FData.Items[10]^,nil^);
end;

destructor TRoutinesRecord.Destroy;
begin
  inherited Destroy;
end;

function TRoutinesRecord.GetName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[0]).Value);
end;

procedure TRoutinesRecord.SetName(const Value: WideString);
begin
  TWideStringWrapper(FItems[0]).Value := Value;
end;

function TRoutinesRecord.GetKind: TIntegerWrapper;
begin
  Result := FItems[2];
end;

procedure TRoutinesRecord.SetKind(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[2]).Assign(Value);
end;

function TRoutinesRecord.GetInheritsFrom: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[3]).Value);
end;

procedure TRoutinesRecord.SetInheritsFrom(const Value: WideString);
begin
  TWideStringWrapper(FItems[3]).Value := Value;
end;

function TRoutinesRecord.GetUnitName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[4]).Value);
end;

procedure TRoutinesRecord.SetUnitName(const Value: WideString);
begin
  TWideStringWrapper(FItems[4]).Value := Value;
end;

function TRoutinesRecord.GetClassName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[5]).Value);
end;

procedure TRoutinesRecord.SetClassName(const Value: WideString);
begin
  TWideStringWrapper(FItems[5]).Value := Value;
end;

function TRoutinesRecord.GetDeclaration: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[6]).Value);
end;

procedure TRoutinesRecord.SetDeclaration(const Value: WideString);
begin
  TWideStringWrapper(FItems[6]).Value := Value;
end;

function TRoutinesRecord.GetDescription: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[7]).Value);
end;

procedure TRoutinesRecord.SetDescription(const Value: WideString);
begin
  TWideStringWrapper(FItems[7]).Value := Value;
end;

procedure TRoutinesRecord.InternalRecordToStrings(List: TStrings);
begin
  List.Add(Format('<h3>%s</h3>',[Name]));
  List.Add(Format('<p><b>Declaration:</b><br>'#13#10'<font face="Courier New">%s</font></p>',
                  [Declaration]));
  List.Add(Format('<p><b>Description:</b><br>'#13#10'%s</p>',
                  [Description]));
end;

{ TRoutinesRecords }

constructor TRoutinesRecords.Create;
begin
  FFieldType := TRoutinesRecord;
  inherited Create(AOwner,AData,ATemplate);
end;

destructor TRoutinesRecords.Destroy;
begin
  inherited Destroy;
end;

function TRoutinesRecords.Add: TRoutinesRecord;
begin
  Result := TRoutinesRecord(InternalAdd);
end;

function TRoutinesRecords.GetItems(Index: Integer): TRoutinesRecord;
begin
  Result := TRoutinesRecord(InternalGetItems(Index));
end;

{ TAsnclientData1Record }

constructor TAsnclientData1Record.Create;
begin
  inherited Create(AOwner,AData,ATemplate);
  FieldFactory(TWideStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[1]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[2]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[3]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[4]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[5]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[6]^,nil^);
  FieldFactory(TWideStringWrapper,nil,FData.Items[7]^,nil^);  
  FieldFactory(TBooleanWrapper,nil,FData.Items[8]^,nil^);
  FieldFactory(TBooleanWrapper,nil,FData.Items[9]^,nil^);            
  FieldFactory(TBooleanWrapper,nil,FData.Items[10]^,nil^);
  FieldFactory(TClassesRecords,nil,FData.Items[11]^,nil^);
  FieldFactory(TRoutinesRecords,nil,FData.Items[12]^,nil^);
  FieldFactory(TFieldsRecord,nil,FData.Items[13]^,nil^);
  IntfSect.Name := 'Interface section';
end;

destructor TAsnclientData1Record.Destroy;
begin
  inherited Destroy;
end;

function TAsnclientData1Record.GetName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[0]).Value);
end;

procedure TAsnclientData1Record.SetName(const Value: WideString);
begin
  TWideStringWrapper(FItems[0]).Value := Value;
end;

function TAsnclientData1Record.GetKind: TIntegerWrapper;
begin
  Result := FItems[2];
end;

procedure TAsnclientData1Record.SetKind(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[2]).Assign(Value);
end;

function TAsnclientData1Record.GetInheritsFrom: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[3]).Value);
end;

procedure TAsnclientData1Record.SetInheritsFrom(const Value: WideString);
begin
  TWideStringWrapper(FItems[3]).Value := Value;
end;

function TAsnclientData1Record.GetUnitName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[4]).Value);
end;

procedure TAsnclientData1Record.SetUnitName(const Value: WideString);
begin
  TWideStringWrapper(FItems[4]).Value := Value;
end;

function TAsnclientData1Record.GetClassName: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[5]).Value);
end;

procedure TAsnclientData1Record.SetClassName(const Value: WideString);
begin
  TWideStringWrapper(FItems[5]).Value := Value;
end;

function TAsnclientData1Record.GetDeclaration: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[6]).Value);
end;

procedure TAsnclientData1Record.SetDeclaration(const Value: WideString);
begin
  TWideStringWrapper(FItems[6]).Value := Value;
end;

function TAsnclientData1Record.GetDescription: WideString;
begin
  Result := WideString(TWideStringWrapper(FItems[7]).Value);
end;

procedure TAsnclientData1Record.SetDescription(const Value: WideString);
begin
  TWideStringWrapper(FItems[7]).Value := Value;
end;

function TAsnclientData1Record.GetClasses: TClassesRecords;
begin
  Result := FItems[11];
end;

procedure TAsnclientData1Record.SetClasses(const Value: TClassesRecords);
begin
  TClassesRecords(FItems[11]).Assign(Value);
end;

function TAsnclientData1Record.GetRoutines: TRoutinesRecords;
begin
  Result := FItems[12];
end;

procedure TAsnclientData1Record.SetRoutines(const Value: TRoutinesRecords);
begin
  TRoutinesRecords(FItems[12]).Assign(Value);
end;

procedure TAsnclientData1Record.ParseFiles;
var
  I: Integer;
begin
  inherited;
  for I := 0 to Classes.Count - 1 do
    Classes[I].ParseFiles;
  if Kind.AsInteger = 0 then
    for I := 0 to Routines.Count - 1 do
      Routines[I].ParseFiles;
end;

procedure TAsnclientData1Record.RecordsToFiles;
begin
  inherited;
  Classes.RecordsToFiles;
  if Kind.AsInteger = 0 then
    Routines.RecordsToFiles;
end;

procedure TAsnclientData1Record.InternalRecordToStrings(List: TStrings);
var
  I: Integer;
begin
  if Kind.AsInteger = 0 then begin
    List.Add(Format('<h3>Unit %s</h3>',[Name]));
    List.Add(Format('<p><b>Description:</b><br>'#13#10'%s</p>',
                    [Description]));
    if Classes.Count > 0 then begin
      List.Add('<h4>Classes</h4>');
      for I := 0 to Classes.Count - 1 do
        if not Classes[I].GetHidden then
          List.Add(Format('<a href="%s">%s</a><br>',
                          [Classes[I].FileName,StringReplace(Classes[I].Name,' ','_',[rfReplaceAll])]));
    end;
    if Routines.Count > 0 then begin
      List.Add('<h4>Routines</h4>');
      for I := 0 to Routines.Count - 1 do
        if not Routines[I].GetHidden then
          List.Add(Format('<a href="%s">%s</a><br>',
                          [Routines[I].FileName,StringReplace(Routines[I].Name,' ','_',[rfReplaceAll])]));
    end;
  end else begin
    List.Add(Format('<h3>%s</h3>',[Name]));
    List.Add(Format('<p>%s</p>',[Description]));
    if Classes.Count > 0 then begin
      List.Add('<h4>Topics:</h4>');
      for I := 0 to Classes.Count - 1 do
        List.Add(Format('<a href="%s">%s</a><br>',
                        [Classes[I].FileName,StringReplace(Classes[I].Name,' ','_',[rfReplaceAll])]));
    end;
    if Routines.Count > 0 then begin
      List.Add('<h4>See also:</h4>');
      for I := 0 to Routines.Count - 1 do
        List.Add(Format('<a href="%s">%s</a><br>',
                        [Routines[I].Description,StringReplace(Routines[I].Name,' ','_',[rfReplaceAll])]));
    end;
  end;
end;

function TAsnclientData1Record.LocateAny(const FileName: string): TMember;
var
  FP: string;
begin
  FP := ChangeFileExt(Self.FileName,'');
  if Pos(FP,FileName) <> 1 then
    Result := nil 
  else if IntfSect.FileName = FileName then
    Result := IntfSect
  else begin
    Result := Classes.LocateAny(FileName);
    if Result = nil then
      Result := Routines.Locate(FileName);
  end;
end;

function TAsnclientData1Record.LocateAnyByName(
  const Name: string): TMember;
begin
  Result := Classes.LocateAnyByName(Name);
  if Result = nil then
    Result := Routines.LocateByName(Name);
end;

function TAsnclientData1Record.GetIntfSect: TFieldsRecord;
begin
  Result := FItems[13];
end;

procedure TAsnclientData1Record.SetIntfSect(const Value: TFieldsRecord);
begin
  TFieldsRecord(FItems[13]).Assign(Value);
end;

{ TAsnclientData1Records }

constructor TAsnclientData1Records.Create;
begin
  FFieldType := TAsnclientData1Record;
  inherited Create(AOwner,AData,ATemplate);
  FData.Assign(GlobalObject);
end;

destructor TAsnclientData1Records.Destroy;
begin
  inherited Destroy;
end;

function TAsnclientData1Records.Add: TAsnclientData1Record;
begin
  Result := TAsnclientData1Record(InternalAdd);
end;

function TAsnclientData1Records.GetItems(Index: Integer): TAsnclientData1Record;
begin
  Result := TAsnclientData1Record(InternalGetItems(Index));
end;

function TAsnclientData1Records.FindUnit(
  const UnitName: string): TAsnclientData1Record;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do begin
    Result := Items[I];
    if CompareStr(Result.Name,UnitName) = 0 then
      Exit;
  end;
  Result := nil;
end;

function TAsnclientData1Records.FindClass(const UnitName,
  ClassName: string): TClassesRecord;
var
  CN: string;
  UnitItem: TAsnclientData1Record;
  P, I: Integer;
begin
  P := Pos(',',ClassName);
  if P > 0 then
    CN := Copy(ClassName,1,P-1)
  else
    CN := ClassName;
  if UnitName = '' then begin
    for I := 0 to Count - 1  do begin
      if Items[I].Name <> '' then begin
        Result := FindClass(Items[I].Name,CN);
        if Assigned(Result) then
          Exit;
      end;
    end;
  end else begin
    UnitItem := FindUnit(UnitName);
    if Assigned(UnitItem) then begin
      for I := 0 to UnitItem.Classes.ItemCount - 1 do begin
        Result := UnitItem.Classes[I];
        if CompareStr(Result.Name,CN) = 0 then
          Exit;
      end;
    end;
  end;
  Result := nil;
end;

procedure TAsnclientData1Records.ParseFiles;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].ParseFiles;
end;

procedure TAsnclientData1Records.AddNewSubItems(Dst, Src: TMember);
var
  D, S: TASNClientData1Record;
begin
  D := TASNClientData1Record(Dst);
  S := TASNClientData1Record(Src);
  D.Classes.AddNew(S.Classes);
  D.Routines.AddNew(S.Routines);
  D.IntfSect.Declaration := S.IntfSect.Declaration;
end;

function TAsnclientData1Records.GetHTMLPath: string;
var
  Reg: TRegistry;
begin
  if FHTMLPath = '' then begin
    Reg := TRegistry.Create;
    try
      if Reg.OpenKeyReadOnly('Software\StreamSec\StrSecII') then
        if Reg.ValueExists('HTMLPath') then
          FHTMLPath := Reg.ReadString('HTMLPath');
    finally
      Reg.Free;
    end;
  end;
  Result := FHTMLPath;
end;

procedure TAsnclientData1Records.SetHTMLPath(const Value: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    if Reg.OpenKey('Software\StreamSec\StrSecII',False) then
      Reg.WriteString('HTMLPath',Value);
  finally
    Reg.Free;
  end;
  FHTMLPath := Value;
end;

procedure TAsnclientData1Records.LoadFromStream(AStream: TStream);
var
  Cls: TClassesRecord;
  Mbr: TMember;
  I, J, K: Integer;

  procedure FixMbr(Mbr: TMember);
  begin
    if Mbr.IsDeprecated and Mbr.IsHidden and Mbr.IsBeta then begin
      Mbr.IsDeprecated := False;
      Mbr.IsHidden := False;
      Mbr.IsBeta := False;
    end;
  end;

begin
  inherited;
  for I := 0 to Count - 1 do begin
    for J := 0 to Items[I].Classes.Count - 1 do begin
      Cls := Items[I].Classes[J];
      FixMbr(Cls);
      for K := 0 to Cls.Methods.Count - 1 do begin
        Mbr := Cls.Methods[K];
        FixMbr(Mbr);
      end;
      for K := 0 to Cls.PublicProperties.Count - 1 do begin
        Mbr := Cls.PublicProperties[K];
        FixMbr(Mbr);
      end;
      for K := 0 to Cls.PublishedProperties.Count - 1 do begin
        Mbr := Cls.PublishedProperties[K];
        FixMbr(Mbr);
      end;
      for K := 0 to Cls.Fields.Count - 1 do begin
        Mbr := Cls.Fields[K];
        FixMbr(Mbr);
      end;
    end;
    for J := 0 to Items[I].Routines.Count - 1 do
      FixMbr(Items[I].Routines[J]);
  end;
end;

{ TMember }

type THack = class(TASNCustomFieldWrapper);

function TMember.DescriptionEmpty: Boolean;
var
  S: string;
begin
  Result := THack(FItems[7]).DataLength < 50;
  if Result then begin
    S := Description;
    Result := StrippedLength(PChar(S),Length(S)) = 0;
  end;
end;

function TMember.GetBeta: Boolean;
begin
  Result := TBooleanWrapper(FItems[8]).Value; 
  if not Result then
    if Assigned(Owner) then begin
      if Owner is TMember then
        Result := TMember(Owner).GetBeta
      else if Assigned(Owner.Owner) then
        Result := TMember(Owner.Owner).GetBeta;
    end;
end;

function TMember.GetDeprecated: Boolean;
begin
  Result := TBooleanWrapper(FItems[9]).Value;
  if not Result then
    if Assigned(Owner) then begin
      if Owner is TMember then
        Result := TMember(Owner).GetDeprecated
      else if Assigned(Owner.Owner) then
        Result := TMember(Owner.Owner).GetDeprecated;
    end;
end;

function TMember.GetFileName: WideString;
begin
  Result := TWideStringWrapper(FItems[1]).Value;
  if ExtractFileDrive(Result) = '' then
    {$IFDEF MSWINDOWS}
    Result := GetHTMLPath + '\' + Result;
    {$ENDIF}
    {$IFDEF LINUX}                       
    Result := GetHTMLPath + '/' + StringReplace(Result,'\','/',[rfReplaceAll]);
    {$ENDIF}
end;

function TMember.GetHidden: Boolean;
begin
  Result := TBooleanWrapper(FItems[10]).Value;
  if not Result then
    if Assigned(Owner) then begin
      if Owner is TMember then
        Result := TMember(Owner).GetHidden
      else if Assigned(Owner.Owner) then
        Result := TMember(Owner.Owner).GetHidden;
    end;
end;

function TMember.GetHTMLPath: string;
begin
  Result := GetTopOwner.HTMLPath;
end;

function TMember.GetTopOwner: TAsnclientData1Records;
begin
  if Owner is TAsnclientData1Records then
    Result := TAsnclientData1Records(Owner)
  else if Owner is TMember then
    Result := TMember(Owner).GetTopOwner
  else
    Result := TMember(Owner.Owner).GetTopOwner;
end;

function TMember.LocateAny(const FileName: string): TMember;
begin
  Result := nil;
end;

function TMember.LocateAnyByName(const Name: string): TMember;
begin
  Result := nil;
end;

procedure TMember.ParseFileForDescription;
var
  FN, S: string;
  SL: TStringList;
  J, K, L: Integer;
begin
    FN := FileName;
    if FileExists(FN) then begin
      SL := TStringList.Create;
      try
        SL.LoadFromFile(FN);
        K := SL.Count;
        S := '';
        while (K > 0) and (S = '') do begin
          Dec(K);
          S := Trim(SL[K]);
          if S = '' then
            SL.Delete(K);
        end;
        J := SL.IndexOf('<p><b>Declaration:</b><br>');
        if J < 0 then begin
          J := 0;
          while J < SL.Count do begin
            if Pos('<p><b>Declaration:',SL[J]) = 1 then begin
              SL.Insert(J+1,Copy(SL[J],Length('<p><b>Declaration: '),MaxInt));
              Break;
            end;
            Inc(J);
          end;
          if J = SL.Count then
            J := -1;
        end;
        K := SL.IndexOf('<p><b>Description:</b><br>');
        if K < 0 then
          K := SL.IndexOf('<p><b>Description:<br>');
        if K < 0 then
          K := SL.IndexOf('<p><b>Description:</b>');
        if ((J >= 0) and (K >= 0) and (J < K)) or
           ((J < 0) and (K >= 0)) then begin
          S := '';
          while J < K - 1 do begin
            Inc(J);
            L := Pos('</p>',SL[J]);
            if SL[J] = '' then
              Break
            else if L = 0 then
              S := S + SL[J] + #13#10
            else begin
              S := S + Copy(SL[J],1,L-1) + #13#10;
              Break;
            end;
          end;
          if S <> '' then
            Delete(S,Length(S)-1,2);
//          Declaration := S;
          S := '';
          while K < SL.Count - 1 do begin
            Inc(K);
            L := Pos('</body>',LowerCase(SL[K]));
            if (L = 0) and (Kind.AsInteger <= 1) then
              L := Pos('<h4>',SL[K]);
            if L = 0 then
              S := S + SL[K] + #13#10
            else begin
              S := S + Copy(SL[K],1,L-1) + #13#10;
              Break;
            end;
          end;
          K := Length(S)+1;
          J := K-1;
          while J > 0 do begin
            if S[J] in [#0..#32] then
              K := J
            else if S[J] = '>' then begin
              if LowerCase(Copy(S,J-3,4)) = '</p>' then begin
                J := J-3;
                K := J;
              end else
                Break;
            end else
              Break;
            Dec(J);
          end;
          if K = 1 then
            S := ''
          else if (S <> '') and (K <= Length(S)) then
            Delete(S,K,Length(S)-K+1);
          Description := S;
        end else begin
          if J > 0 then begin
            S := '';
            while J < SL.Count - 1 do begin
              Inc(J);
              K := Pos('</p>',SL[J]);
              if SL[J] = '' then
                Break
              else if K = 0 then
                S := S + SL[J] + #13#10
              else begin
                S := S + Copy(SL[J],1,K-1) + #13#10;
                Break;
              end;
            end;
            if S <> '' then
              Delete(S,Length(S)-1,2);
          end else
            S := '';
          // Description := S;
        end;
      finally
        SL.Free;
      end;
    end;
end;

procedure TMember.ParseFiles;
begin
  ParseFileForDescription;
end;

procedure TMember.RecordsToFiles;
begin
  RecordToFile(FileName);
end;

procedure TMember.RecordToFile(const FileName: string);
var
  SL: TStringList;
begin
  if IsHidden then Exit;
  SL := TStringList.Create;
  try
    SL.Add('<html>');
    SL.Add('<head>');
    SL.Add(Format('<title>%s</title>',[Name]));
    SL.Add('</head>');
    SL.Add('<body>');
    if IsBeta or IsDeprecated then begin
      SL.Add('<p><b>Status:</b><br>');
      if IsDeprecated then
        SL.Add('Deprecated</p>')
      else
        SL.Add('Beta</p>');
    end;
    InternalRecordToStrings(SL);
    SL.Add('</body>');
    SL.Add('</html>');
    {$IFDEF LINUX}
    ForceDirectories(ExtractFilePath(FileName));
    {$ENDIF}
    SL.SaveToFile(FileName);
  finally
    SL.Free;
  end;
end;

function TMember.RecordToString: string;
var
  SL: TStringList;
begin
  if IsHidden then Exit;
  SL := TStringList.Create;
  try
    SL.Add('<html>');
    SL.Add('<head>');
    SL.Add(Format('<title>%s</title>',[Name]));
    SL.Add('</head>');
    SL.Add('<body>');
    if IsBeta or IsDeprecated then begin
      SL.Add('<p><b>Status:</b><br>');
      if IsDeprecated then
        SL.Add('Deprecated</p>')
      else
        SL.Add('Beta</p>');
    end;
    InternalRecordToStrings(SL);
    SL.Add('</body>');
    SL.Add('</html>');
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TMember.SameName(const Name: string): Boolean;
begin
  Result := AnsiCompareText(Name,Self.Name) = 0;
end;

procedure TMember.SetBeta(const Value: Boolean);
begin
  TBooleanWrapper(FItems[8]).Value := Value;
end;

procedure TMember.SetDeprecated(const Value: Boolean);
begin
  TBooleanWrapper(FItems[9]).Value := Value;
end;

procedure TMember.SetFileName(const Value: WideString);
var
  V, H: string;
begin
  V := Value;
  H := GetHTMLPath;
  if AnsiCompareStr(H,Copy(V,1,Length(H))) = 0 then
    Delete(V,1,Length(H)+1);
  TWideStringWrapper(FItems[1]).Value := V;
end;

procedure TMember.SetHidden(const Value: Boolean);
begin
  TBooleanWrapper(FItems[10]).Value := Value;
end;

{ TMembers }

procedure TMembers.AddNew(Src: TMembers);
var
  I, J: Integer;
  SrcM, M: TMember;
begin
  J := 0;
  if Src.Owner = nil then
    Assert(Src.Count = Src.ItemCount,'Not updated.')
  else
    Assert(Src.Count = Src.ItemCount,TMember(Src.Owner).Name + '.' + Src.VarName + ' is not updated.');
  for I := 0 to ItemCount - 1 do
    if TMember(Items[I]).IsHidden or
       (Src.Locate(TMember(Items[I]).FileName) = nil) then try
      TMember(Items[I]).IsHidden := True;
      TMember(Items[I]).Declaration := '';
      TMember(Items[I]).Description := '';
      if FFieldType = TClassesRecord then begin
        TClassesRecord(Items[I]).Methods.Clear;
        TClassesRecord(Items[I]).PublicProperties.Clear;
        TClassesRecord(Items[I]).PublishedProperties.Clear;
        TClassesRecord(Items[I]).Fields.Clear;
      end else if FFieldType = TAsnclientData1Record then begin
        TAsnclientData1Record(Items[I]).Classes.Clear;
        TAsnclientData1Record(Items[I]).Routines.Clear;
        TAsnclientData1Record(Items[I]).IntfSect.Declaration := '';
        TAsnclientData1Record(Items[I]).IntfSect.Description := '';
      end;
      TMember(Items[I]).Update;
    except
    end;
  for I := 0 to Src.Count - 1 do begin
    SrcM := TMember(Src.Items[I]);
    M := Locate(SrcM.FileName);
    if Assigned(M) and M.IsHidden then begin
    end else if Assigned(M) then begin
      J := IndexOf(M);
      M.Name := SrcM.Name;
      M.FileName := SrcM.FileName; // Fix.
      M.Declaration := SrcM.Declaration;
      M.Description := SrcM.Description;
      AddNewSubItems(M,SrcM);
    end else begin
      M := TMember(InternalAdd);
      M.Assign(SrcM);
      if J < Count-1 then
        Move(Count-1,J);
    end;
    Inc(J);
  end;
end;

procedure TMembers.AddNewSubItems(Dst, Src: TMember);
begin

end;

function TMembers.Locate(const FileName: string): TMember;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do begin
    Result := TMember(Items[I]);
    if AnsiCompareText(FileName,Result.FileName) = 0 then
      Exit;
  end;
  Result := nil;
end;

function TMembers.LocateAny(const FileName: string): TMember;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do begin
    Result := TMember(Items[I]);
    if AnsiCompareText(FileName,Result.FileName) = 0 then
      Exit;
    Result := Result.LocateAny(FileName);
    if Assigned(Result) then
      Break;
  end;
end;

function TMembers.LocateAnyByName(const Name: string): TMember;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do begin
    Result := TMember(Items[I]);
    if Result.SameName(Name) then
      Exit;
    Result := Result.LocateAnyByName(Name);
    if Assigned(Result) then
      Break;
  end;
end;

function TMembers.LocateByName(const Name: string): TMember;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do begin
    Result := TMember(Items[I]);
    if Result.SameName(Name) then
      Exit;
  end;
  Result := nil;
end;

procedure TMembers.RecordsToFiles;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TMember(Items[I]).RecordsToFiles;
end;

initialization
  InitGlobalObject;
finalization
  GlobalObject._Release;
end.