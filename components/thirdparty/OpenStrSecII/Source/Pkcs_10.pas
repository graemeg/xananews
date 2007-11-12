{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     PKCS_10 Unit                                      }
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
{                                                       }
{     "RSA Security Inc.                                }
{      Public-Key Cryptography Standards (PKCS)"        }
{                                                       }
{*******************************************************}
{$I ver.inc}
unit Pkcs_10;

interface

uses
  Classes, SecUtils, Pkix, Asn1, Pkix_Cert, MpPK, MpDL, MpEC, MpIF;

const   
  pkcs_9_at_challengePassword = pkcs_9 + '.7';
  pkcs_9_at_extensionRequest  = pkcs_9 + '.14';

  crlf = #13#10;
  ASNModule = 
    'PKCS-10 {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-10(10) modules(1) pkcs-10(1) } DEFINITIONS ::=' + crlf + 
    'BEGIN' + crlf + crlf + 
    'IMPORTS RDNSequence, SubjectPublicKeyInfo, Extensions, AlgorithmIdentifier FROM PKIX-Cert;' + crlf + crlf +
    'CertificationRequest ::= SEQUENCE {' + crlf + 
    '     certificationRequestInfo'#9'CertificationRequestInfo,' + crlf +
    '     signatureAlgorithm'#9'AlgorithmIdentifier,' + crlf +
    '     signature'#9'BIT STRING}' + crlf + crlf +
    'CertificationRequestInfo ::= SEQUENCE {' + crlf +
    '     version'#9'INTEGER,' + crlf +
    '     subject'#9'RDNSequence,' + crlf +
    '     subjectPKInfo'#9'SubjectPublicKeyInfo,' + crlf +
    '     attributes'#9'[0] IMPLICIT CRIAttributes}' + crlf + crlf +
    'CRIAttributes ::= SET OF CRIAttribute' + crlf + crlf +
    'CRIAttribute ::= SEQUENCE {' + crlf +
    '     typeID'#9'OBJECT,' + crlf +
    '     value'#9'SET OF CRIAttributeType.&Type(&typeID)}' + crlf + crlf +
    'CRIAttributeType ::= TYPE-IDENTIFIER {' + crlf +
    '     {DirectoryString'#9'IDENTIFIED BY pkcs-9-at-challengePassword}|' + crlf +
    '     {Extensions'#9'IDENTIFIED BY pkcs-9-at-extensionRequest}|' + crlf +
    '     {UTF8STRING'#9'IDENTIFIED BY id-regCtrl-regToken}}' + crlf + crlf +
    'DirectoryString ::= CHOICE {' + crlf +
    '     teletexString'#9'TeletexString,' + crlf +
    '     printableString'#9'PrintableString,' + crlf +
    '     universalString'#9'UniversalString,' + crlf +
    '     utf8String'#9'UTF8STRING,' + crlf +
    '     bmpString'#9'BMPString}' + crlf + crlf +
    'pkcs-9-at-challengePassword OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-9(9) 7 }' + crlf + crlf +
    'pkcs-9-at-extensionRequest OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-9(9) 14 }' + crlf + crlf +
    'id-regCtrl-regToken OBJECT IDENTIFIER ::= { iso(1) identified-organization(3) dod(6) internet(1) security(5) mechanisms(5) pkix(7) pkip(5) regCtrl(1) 1 }' + crlf + crlf +
    'END' + crlf + crlf;

type
{ Declaration tree: 
    TCertificationRequest
     TCertificationRequestInfo
      TCriattributes
       TCriattribute
        TSetOfCriattributeType
         TCriattributeType}

  TCriattributeTypeEnum = (
    cteUndefined, ctePkcs9AtChallengePassword, ctePkcs9AtExtensionRequest,
    cteRegCtrlRegToken);

  TCriattributeType = class(TASNChoiceWrapper)
  private
    function GetAsPkcs_9_At_ChallengePassword: TDirectoryString;
    procedure SetAsPkcs_9_At_ChallengePassword(const Value: TDirectoryString);
    function GetAsPkcs_9_At_ExtensionRequest: TExtensions;
    procedure SetAsPkcs_9_At_ExtensionRequest(const Value: TExtensions);
    function GetChoice: TCriattributeTypeEnum;
    procedure SetChoice(const Value: TCriattributeTypeEnum);
    function GetAsRegCtrl_RegToken: WideString;
    procedure SetAsRegCtrl_RegToken(const Value: WideString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsPkcs_9_At_ChallengePassword: TDirectoryString read GetAsPkcs_9_At_ChallengePassword write SetAsPkcs_9_At_ChallengePassword;
    property AsPkcs_9_At_ExtensionRequest: TExtensions read GetAsPkcs_9_At_ExtensionRequest write SetAsPkcs_9_At_ExtensionRequest;
    property AsRegCtrl_RegToken: WideString read GetAsRegCtrl_RegToken write SetAsRegCtrl_RegToken;
  published
    property Choice: TCriattributeTypeEnum read GetChoice write SetChoice;
  end;

{ Declaration tree:
    TCertificationRequest
     TCertificationRequestInfo
      TCriattributes
       TCriattribute
        TSetOfCriattributeType}

  TSetOfCriattributeType = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TCriattributeType;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TCriattributeType;
    property Items[Index: Integer]: TCriattributeType read GetItems; default;
  end;

{ Declaration tree: 
    TCertificationRequest
     TCertificationRequestInfo
      TCriattributes
       TCriattribute}

  TCriattribute = class(TASNConstructedWrapper)
  private
    function GetTypeID: ObjectIdentifier;
    procedure SetTypeID(const Value: ObjectIdentifier);
    function GetValue: TSetOfCriattributeType;
    procedure SetValue(const Value: TSetOfCriattributeType);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TCriattributeTypeEnum): Boolean;
  published
    property TypeID: ObjectIdentifier read GetTypeID write SetTypeID;
    property Value: TSetOfCriattributeType read GetValue write SetValue;
  end;

{ Declaration tree: 
    TCertificationRequest
     TCertificationRequestInfo
      TCriattributes}

  TCriattributes = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TCriattribute;
    function GetExtensions: TExtensions;
    function GetChallengePassword: WideString;
    procedure SetChallengePassword(const Value: WideString);
    function GetRegToken: WideString;
    procedure SetRegToken(const Value: WideString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TCriattribute;
    property Items[Index: Integer]: TCriattribute read GetItems; default;
  published
    property ChallengePassword: WideString read GetChallengePassword write SetChallengePassword;
    property Extensions: TExtensions read GetExtensions;
    property RegToken: WideString read GetRegToken write SetRegToken;
  end;

{ Declaration tree: 
    TCertificationRequest
     TCertificationRequestInfo}

  TCertificationRequestInfo = class(TASNConstructedWrapper)
  private
    function GetVersion: TIntegerWrapper;
    procedure SetVersion(const Value: TIntegerWrapper);
    function GetSubject: TRdnsequence;
    procedure SetSubject(const Value: TRdnsequence);
    function GetSubjectPKInfo: TSubjectPublicKeyInfo;
    procedure SetSubjectPKInfo(const Value: TSubjectPublicKeyInfo);
    function GetAttributes: TCriattributes;
    procedure SetAttributes(const Value: TCriattributes);
    function GetExtensions: TExtensions;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Version: TIntegerWrapper read GetVersion write SetVersion;
    property Subject: TRdnsequence read GetSubject write SetSubject;
    property SubjectPKInfo: TSubjectPublicKeyInfo read GetSubjectPKInfo write SetSubjectPKInfo;
    property Attributes: TCriattributes read GetAttributes write SetAttributes;
    property Extensions: TExtensions read GetExtensions;
  end;

{ Declaration tree:
    TCertificationRequest}

  TCertificationRequest = class(TSigned,
                                IPublicKeyInfo,
                                IReadOnlyPublicKeyInfo,
                                IWriteOnlyPublicKeyInfo,
                                IPKPublicKeyInfo,
                                IIFPublicKeyInfo,
                                IDLPublicKeyInfo,
                                IECPublicKeyInfo)
  private
    function GetCertificationRequestInfo: TCertificationRequestInfo;
    procedure SetCertificationRequestInfo(const Value: TCertificationRequestInfo);
    function GetSubject: TRdnsequence;
    procedure SetSubject(const Value: TRdnsequence);
    function GetSubjectPKInfo: TSubjectPublicKeyInfo;
    procedure SetSubjectPKInfo(const Value: TSubjectPublicKeyInfo);
  protected                    
    function AlgorithmIdentifier: string;
    function BeforeSign(var CACert: TCertificate;
                        SignAlg: ObjectIdentifier;
                        out Params: TASNCustomWrapper): Boolean; override;
    function CheckSignAlgCoherence: Boolean; override;
    function GetAlgorithm: TAlgorithmIdentifier;
    function InternalCheckSignature(CACert: IReadOnlyPublicKeyInfo): Boolean; override;
    function IsAllegedIssuer(CACert: TCertificate): Boolean; override;
  published
    property CertificationRequestInfo: TCertificationRequestInfo read GetCertificationRequestInfo write SetCertificationRequestInfo;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function ExtractSubjectRSAPublicKey(var RSAKey: TIFPublicKey): Boolean;
    function ExtractSubjectDSAPublicKey(var DSAKey: TDLPublicKey): Boolean;
    function ExtractSubjectDHPublicKey(var DHKey: TDLPublicKey): Boolean;
    function ExtractSubjectNRPublicKey(var NRKey: TDLPublicKey): Boolean;
    function ExtractSubjectECPublicKey(var ECKey: TECPublicKey): Boolean;
    function ExtractSubjectRSAES_OAEPPublicKey(var RSAKey: TIFPublicKey;
                                               var HA: THashAlgorithm;
                                               var MGFHA: THashAlgorithm;
                                               var P: string): Boolean;
    procedure ImposeSubjectRSAPublicKey(const RSAKey: TIFPublicKey);
    procedure ImposeSubjectDSAPublicKey(const DSAKey: TDLPublicKey);
    procedure ImposeSubjectDHPublicKey(const DHKey: TDLPublicKey);
    procedure ImposeSubjectNRPublicKey(const NRKey: TDLPublicKey);
    procedure ImposeSubjectECPublicKey(var ECKey: TECPublicKey);
    procedure ImposeSubjectRSAES_OAEPPublicKey(const RSAKey: TIFPublicKey;
                                               HA: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF};
                                               MGFHA: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF};
                                               P: OctetString = '');
    function PublicKeyIdentifier: string;
  published
    property Subject: TRdnsequence read GetSubject write SetSubject;
    property SubjectPKInfo: TSubjectPublicKeyInfo read GetSubjectPKInfo write SetSubjectPKInfo implements IReadOnlyPublicKeyInfo;
  end;

{$IFNDEF INIT_SECTIONS}
procedure Initialize;
{$ENDIF}
{$IFNDEF FINI_SECTIONS}
procedure Finalize;
{$ENDIF}

implementation

uses
  SysUtils, ReadStrm;

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

procedure Initialize;
begin
  if GlobalObject = nil then begin
    InitGlobalObject;
  end;
end;

procedure Finalize;
var
  F: TASN1Struct;
begin
  if Assigned(GlobalObject) then begin
    F := GlobalObject;
    GlobalObject := nil;
    F._Release;
  end;
end;

{ TCriattributeType }

constructor TCriattributeType.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'PKCS_10 not initialized: Call StrSecInit.InitPKCS_10 to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/certificationRequestInfo/attributes')^.Template.FindField('value')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TDirectoryString);
  FChoiceList.Add(TExtensions);
  FChoiceList.Add(TWideStringWrapper);
  if FData.ChoiceVarName <> '' then
    Update;
end;

destructor TCriattributeType.Destroy;
begin
  inherited Destroy;
end;

function TCriattributeType.GetAsPkcs_9_At_ChallengePassword: TDirectoryString;
begin
  Result := (FSelected as TDirectoryString);
end;

procedure TCriattributeType.SetAsPkcs_9_At_ChallengePassword(const Value: TDirectoryString);
begin
  InternalSelectChoice(0);
  (FSelected as TDirectoryString).Assign(Value);
end;

function TCriattributeType.GetAsPkcs_9_At_ExtensionRequest: TExtensions;
begin
  Result := (FSelected as TExtensions);
end;

procedure TCriattributeType.SetAsPkcs_9_At_ExtensionRequest(const Value: TExtensions);
begin
  InternalSelectChoice(1);
  (FSelected as TExtensions).Assign(Value);
end;

function TCriattributeType.GetChoice: TCriattributeTypeEnum;
begin
  Result := TCriattributeTypeEnum(InternalGetChoice);
end;

procedure TCriattributeType.SetChoice(const Value: TCriattributeTypeEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

function TCriattributeType.GetAsRegCtrl_RegToken: WideString;
begin
  Result := (FSelected as TWideStringWrapper).Value;
end;

procedure TCriattributeType.SetAsRegCtrl_RegToken(const Value: WideString);
begin
  InternalSelectChoice(2);
  (FSelected as TWideStringWrapper).Value := Value;
end;

{ TSetOfCriattributeType }

constructor TSetOfCriattributeType.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKCS_10 not initialized: Call StrSecInit.InitPKCS_10 to correct.');
  FFieldType := TCriattributeType;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/certificationRequestInfo/attributes')^.Template;
    F := F.FindField('value')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TSetOfCriattributeType.Destroy;
begin
  inherited Destroy;
end;

function TSetOfCriattributeType.Add: TCriattributeType;
begin
  Result := TCriattributeType(InternalAdd);
end;

function TSetOfCriattributeType.GetItems(Index: Integer): TCriattributeType;
begin
  Result := TCriattributeType(InternalGetItems(Index));
end;

{ TCriattribute }

constructor TCriattribute.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKCS_10 not initialized: Call StrSecInit.InitPKCS_10 to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/certificationRequestInfo/attributes')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TSetOfCriattributeType,nil,FData.Items[1]^,nil^);
end;

destructor TCriattribute.Destroy;
begin
  inherited Destroy;
end;

function TCriattribute.GetTypeID: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TCriattribute.SetTypeID(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TCriattribute.GetValue: TSetOfCriattributeType;
begin
  Result := FItems[1];
end;

procedure TCriattribute.SetValue(const Value: TSetOfCriattributeType);
begin
  TSetOfCriattributeType(FItems[1]).Assign(Value);
end;

function TCriattribute.IsType(Id: TCriattributeTypeEnum): Boolean;
begin
  if TASNCustomOFFieldWrapper(FItems[1]).ItemCount > 0 then
    Result := TCriattributeType(FItems[1]).Choice = Id
  else
    Result := False;
end;

{ TCriattributes }

constructor TCriattributes.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKCS_10 not initialized: Call StrSecInit.InitPKCS_10 to correct.');
  FFieldType := TCriattribute;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/certificationRequestInfo/attributes')^;
    FData.CopyTypeInfo(F);
  end;
  // There will always be an ExtensionRequest
  with Add do begin
    TypeId := pkcs_9_at_extensionRequest;
    Value.Add.Choice := ctePkcs9AtExtensionRequest;
  end;
end;

destructor TCriattributes.Destroy;
begin
  inherited Destroy;
end;

function TCriattributes.Add: TCriattribute;
begin
  Result := TCriattribute(InternalAdd);
end;

function TCriattributes.GetItems(Index: Integer): TCriattribute;
begin
  Result := TCriattribute(InternalGetItems(Index));
end;

function TCriattributes.GetExtensions: TExtensions;
var
  I, J: Integer;
begin
  for I := 0 to Count - 1 do
    for J := 0 to Items[I].Value.ItemCount - 1 do
      if Items[I].Value.Items[J].Choice = ctePkcs9AtExtensionRequest then begin
        Result := Items[I].Value.Items[J].AsPkcs_9_At_ExtensionRequest;
        Exit;
      end;
  Result := nil;
end;

function TCriattributes.GetChallengePassword: WideString;
var
  I, J: Integer;
begin
  for I := 0 to Count - 1 do
    for J := 0 to Items[I].Value.ItemCount - 1 do
      if Items[I].Value.Items[J].Choice = ctePkcs9AtChallengePassword then begin
        Result := Items[I].Value.Items[J].AsPkcs_9_At_ChallengePassword.Value;
        Exit;
      end;
  Result := '';
end;

procedure TCriattributes.SetChallengePassword(const Value: WideString);
var
  I, J: Integer;
  Attr: TCriAttributeType;
begin
  for I := 0 to Count - 1 do
    for J := 0 to Items[I].Value.ItemCount - 1 do
      if Items[I].Value.Items[J].Choice = ctePkcs9AtChallengePassword then begin
        Items[I].Value.Items[J].AsPkcs_9_At_ChallengePassword.AsUtf8String := Value;
        Exit;
      end;
  Attr := Add.Value.Add;
  Attr.Choice := ctePkcs9AtChallengePassword;
  Attr.AsPkcs_9_At_ChallengePassword.AsUtf8String := Value;
end;

function TCriattributes.GetRegToken: WideString;
var
  I, J: Integer;
begin
  for I := 0 to Count - 1 do
    for J := 0 to Items[I].Value.ItemCount - 1 do
      if Items[I].Value.Items[J].Choice = cteRegCtrlRegToken then begin
        Result := Items[I].Value.Items[J].AsRegCtrl_RegToken;
        Exit;
      end;
  Result := '';
end;

procedure TCriattributes.SetRegToken(const Value: WideString);
var
  I, J: Integer;          
  Attr: TCriAttributeType;
begin
  for I := 0 to Count - 1 do
    for J := 0 to Items[I].Value.ItemCount - 1 do
      if Items[I].Value.Items[J].Choice = cteRegCtrlRegToken then begin
        Items[I].Value.Items[J].AsRegCtrl_RegToken := Value;
        Exit;
      end;
  Attr := Add.Value.Add;
  Attr.Choice := cteRegCtrlRegToken;
  Attr.AsRegCtrl_RegToken := Value;
end;

{ TCertificationRequestInfo }

constructor TCertificationRequestInfo.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'PKCS_10 not initialized: Call StrSecInit.InitPKCS_10 to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('certificationRequestInfo')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TRdnsequence,nil,FData.Items[1]^,nil^);
  FieldFactory(TSubjectPublicKeyInfo,nil,FData.Items[2]^,nil^);
  FieldFactory(TCriattributes,nil,FData.Items[3]^,nil^);
end;

destructor TCertificationRequestInfo.Destroy;
begin
  inherited Destroy;
end;

function TCertificationRequestInfo.GetVersion: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TCertificationRequestInfo.SetVersion(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TCertificationRequestInfo.GetSubject: TRdnsequence;
begin
  Result := FItems[1];
end;

procedure TCertificationRequestInfo.SetSubject(const Value: TRdnsequence);
begin
  TRdnsequence(FItems[1]).Assign(Value);
end;

function TCertificationRequestInfo.GetSubjectPKInfo: TSubjectPublicKeyInfo;
begin
  Result := FItems[2];
end;

procedure TCertificationRequestInfo.SetSubjectPKInfo(const Value: TSubjectPublicKeyInfo);
begin
  TSubjectPublicKeyInfo(FItems[2]).Assign(Value);
end;

function TCertificationRequestInfo.GetAttributes: TCriattributes;
begin
  Result := FItems[3];
end;

procedure TCertificationRequestInfo.SetAttributes(const Value: TCriattributes);
begin
  TCriattributes(FItems[3]).Assign(Value);
end;

function TCertificationRequestInfo.GetExtensions: TExtensions;
begin
  Result := Attributes.Extensions
end;

procedure TCertificationRequestInfo.AssignTo(Dest: TPersistent);
var
  Tbs: TTbsCertificate;
  Ext: TExtension;
begin
  if Dest is TTbsCertificate then begin
    Tbs := TTbsCertificate(Dest);
    Tbs.Version.AsInteger := 2; // Version 3
    Tbs.Subject.Choice := neRdnSequence;
    Tbs.Subject.AsRdnSequence.Assign(Subject);
    Tbs.SubjectPublicKeyInfo.Assign(SubjectPKInfo);
    Tbs.Extensions.Assign(Attributes.Extensions);
    Ext := Tbs.Extensions.AddUniqueItem(eveIdCeSubjectKeyIdentifier);
    Ext.ExtnValue.AsCe_SubjectKeyIdentifier.AsString :=
      SubjectPKInfo.PublicKeyIdentifier;
    Tbs.Extensions.AddUniqueItem(eveIdCeBasicConstraints); // Defaults to CA = False
  end else
    inherited;
end;

{ TCertificationRequest }

function TCertificationRequest.AlgorithmIdentifier: string;
begin
  Result := GetAlgorithm.Algorithm;
end;

function TCertificationRequest.BeforeSign(var CACert: TCertificate;
                                          SignAlg: ObjectIdentifier;
                                          out Params: TASNCustomWrapper): Boolean;
begin
  Result := True;

  Params := CertificationRequestInfo.SubjectPKInfo.Algorithm.Parameters.Selected;
end;

function TCertificationRequest.CheckSignAlgCoherence: Boolean;
begin
  Result := True;
end;

constructor TCertificationRequest.Create;
begin                      
  Assert(Assigned(GlobalObject),'PKCS_10 not initialized: Call StrSecInit.InitPKCS_10 to correct.');
  inherited;
  FData.Assign(GlobalObject);
  FieldFactory(TCertificationRequestInfo,nil,FData.Items[0]^,nil^);
  FieldFactory(TAlgorithmIdentifier,nil,FData.Items[1]^,nil^);
  FieldFactory(TBitString,nil,FData.Items[2]^,nil^);
end;

destructor TCertificationRequest.Destroy;
begin
  inherited Destroy;
end;

function TCertificationRequest.ExtractSubjectDHPublicKey(
  var DHKey: TDLPublicKey): Boolean;
begin
  Result := False;
end;

function TCertificationRequest.ExtractSubjectDSAPublicKey(
  var DSAKey: TDLPublicKey): Boolean;
begin
  Result := CertificationRequestInfo.SubjectPKInfo.ExtractSubjectDSAPublicKey(DSAKey);
end;

function TCertificationRequest.ExtractSubjectECPublicKey(
  var ECKey: TECPublicKey): Boolean;
begin
  Result := CertificationRequestInfo.SubjectPKInfo.ExtractSubjectECPublicKey(ECKey);
end;

function TCertificationRequest.ExtractSubjectNRPublicKey(
  var NRKey: TDLPublicKey): Boolean;
begin
  Result := CertificationRequestInfo.SubjectPKInfo.ExtractSubjectNRPublicKey(NRKey);
end;

function TCertificationRequest.ExtractSubjectRSAES_OAEPPublicKey(
  var RSAKey: TIFPublicKey; var HA, MGFHA: THashAlgorithm;
  var P: string): Boolean;
begin
  Result := CertificationRequestInfo.SubjectPKInfo.ExtractSubjectRSAES_OAEPPublicKey(RSAKey,HA,MGFHA,P);
end;

function TCertificationRequest.ExtractSubjectRSAPublicKey(
  var RSAKey: TIFPublicKey): Boolean;
begin
  Result := CertificationRequestInfo.SubjectPKInfo.ExtractSubjectRSAPublicKey(RSAKey);
end;

function TCertificationRequest.GetAlgorithm: TAlgorithmIdentifier;
begin
  Result := CertificationRequestInfo.SubjectPKInfo.Algorithm;
end;

function TCertificationRequest.GetCertificationRequestInfo: TCertificationRequestInfo;
begin
  Result := FItems[0];
end;

function TCertificationRequest.GetSubject: TRdnsequence;
begin
  Result := CertificationRequestInfo.Subject;
end;

function TCertificationRequest.GetSubjectPKInfo: TSubjectPublicKeyInfo;
begin
  Result := CertificationRequestInfo.SubjectPKInfo;
end;

procedure TCertificationRequest.ImposeSubjectDHPublicKey(
  const DHKey: TDLPublicKey);
begin
  raise Exception.Create('DH keys cannot be imposed onto PKCS_10.TCertificateRequest instances');
end;

procedure TCertificationRequest.ImposeSubjectDSAPublicKey(
  const DSAKey: TDLPublicKey);
var
  Ext: TExtension;
begin
  CertificationRequestInfo.SubjectPKInfo.ImposeSubjectDSAPublicKey(DSAKey);
  Ext := CertificationRequestInfo.Extensions.AddUniqueItem(eveIdCeSubjectKeyIdentifier);
  Ext.ExtnValue.AsCe_SubjectKeyIdentifier.AsString :=
    CertificationRequestInfo.SubjectPKInfo.PublicKeyIdentifier;
end;

procedure TCertificationRequest.ImposeSubjectECPublicKey(
  var ECKey: TECPublicKey);
var
  Ext: TExtension;
begin
  CertificationRequestInfo.SubjectPKInfo.ImposeSubjectECPublicKey(ECKey);
  Ext := CertificationRequestInfo.Extensions.AddUniqueItem(eveIdCeSubjectKeyIdentifier);
  Ext.ExtnValue.AsCe_SubjectKeyIdentifier.AsString :=
    CertificationRequestInfo.SubjectPKInfo.PublicKeyIdentifier;
end;

procedure TCertificationRequest.ImposeSubjectNRPublicKey(
  const NRKey: TDLPublicKey);
var
  Ext: TExtension;
begin
  CertificationRequestInfo.SubjectPKInfo.ImposeSubjectNRPublicKey(NRKey);
  Ext := CertificationRequestInfo.Extensions.AddUniqueItem(eveIdCeSubjectKeyIdentifier);
  Ext.ExtnValue.AsCe_SubjectKeyIdentifier.AsString :=
    CertificationRequestInfo.SubjectPKInfo.PublicKeyIdentifier;
end;

procedure TCertificationRequest.ImposeSubjectRSAES_OAEPPublicKey(
  const RSAKey: TIFPublicKey; HA, MGFHA: THashAlgorithm; P: OctetString);
var
  Ext: TExtension;
begin
  CertificationRequestInfo.SubjectPKInfo.ImposeSubjectRSAES_OAEPPublicKey(RSAKey,HA,MGFHA,P);
  Ext := CertificationRequestInfo.Extensions.AddUniqueItem(eveIdCeSubjectKeyIdentifier);
  Ext.ExtnValue.AsCe_SubjectKeyIdentifier.AsString :=
    CertificationRequestInfo.SubjectPKInfo.PublicKeyIdentifier;
end;

procedure TCertificationRequest.ImposeSubjectRSAPublicKey(
  const RSAKey: TIFPublicKey);
var
  Ext: TExtension;
begin
  CertificationRequestInfo.SubjectPKInfo.ImposeSubjectRSAPublicKey(RSAKey);
  Ext := CertificationRequestInfo.Extensions.AddUniqueItem(eveIdCeSubjectKeyIdentifier);
  Ext.ExtnValue.AsCe_SubjectKeyIdentifier.AsString :=
    CertificationRequestInfo.SubjectPKInfo.PublicKeyIdentifier;
end;

function TCertificationRequest.InternalCheckSignature(
  CACert: IReadOnlyPublicKeyInfo): Boolean;
var
  RSAKey: TIFPublicKey;
  DLKey: TDLPublicKey;
  ECKey: TECPublicKey;
  MS: TSecureMemoryStream;
  HA,MHA: THashAlgorithm;
  EM: TSignEncoding;
begin
//  Result := False;
//  if not CheckSignAlgCoherence then
//    Exit;

  EM := seEMSA3;
{$IFDEF SHA1}
  HA := haSHA1;
  MHA := haSHA1;
{$ENDIF SHA1}
  if (SignatureAlgorithm.Algorithm = id_DSA_with_SHA1) or
     (SignatureAlgorithm.Algorithm = ecdsa_with_SHA1) then
{$IFDEF SHA1}
    Result := True
{$ELSE  SHA1}
    Result := False
{$ENDIF SHA1}
  else
    Result := InterpretRSASignatureAlgorithm(HA,MHA,EM);
  if Result then begin
    FillChar(RSAKey,SizeOf(RSAKey),0);
    FillChar(DLKey,SizeOf(DLKey),0);
    FillChar(ECKey,SizeOf(ECKey),0);
    if ExtractSubjectRSAPublicKey(RSAKey) then begin
      MS := TSecureMemoryStream.Create;
      try
        FData.Items[0]^.SaveToStream(MS,fmtDER);
        MS.Position := 0;
        Result := IFSSASignatureVerification(RSAKey,MS.Memory^,MS.Size,
                                             FData.Items[2]^.Content^,
                                             FData.Items[2]^.Length,
                                             HA,MHA,EM);
      finally
        MS.Free;
      end;
      DisposeIFPublicKey(RSAKey);
    end else if ExtractSubjectDSAPublicKey(DLKey) then begin
      MS := TSecureMemoryStream.Create;
      try
        FData.Items[0]^.SaveToStream(MS,fmtDER);
        MS.Position := 0;
        Result := DLSSASignatureVerification(DLKey,MS.Memory^,MS.Size,
                                             FData.Items[2].Content[1],
                                             FData.Items[2].Length - 1,
                                             True,HA);
      finally
        MS.Free;
      end;
      DisposeDLPublicKey(DLKey);
    end else if ExtractSubjectECPublicKey(ECKey) then begin
      MS := TSecureMemoryStream.Create;
      try
        FData.Items[0]^.SaveToStream(MS,fmtDER);
        MS.Position := 0;
        Result := ECSSASignatureVerification(ECKey,MS.Memory^,MS.Size,
                                             FData.Items[2].Content[1],
                                             FData.Items[2].Length - 1,
                                             True,HA);
      finally
        MS.Free;
      end;
      DisposeECPublicKey(ECKey);
    end else
      Result := False;
  end;
end;

function TCertificationRequest.IsAllegedIssuer(
  CACert: TCertificate): Boolean;
begin
  Result := CACert = nil;
end;

function TCertificationRequest.PublicKeyIdentifier: string;
var
  Ext: TExtension;
begin
  if CertificationRequestInfo.Extensions = nil then
    Result := ''
  else begin
    Ext := CertificationRequestInfo.Extensions.UniqueItem[eveIdCeSubjectKeyIdentifier];
    if Assigned(Ext) then
      Result := Ext.ExtnValue.AsCe_SubjectKeyIdentifier.AsString
    else
      Result := '';
  end;
end;

procedure TCertificationRequest.SetCertificationRequestInfo(const Value: TCertificationRequestInfo);
begin
  TCertificationRequestInfo(FItems[0]).Assign(Value);
end;

procedure TCertificationRequest.SetSubject(const Value: TRdnsequence);
begin
  CertificationRequestInfo.Subject := Value;
end;

procedure TCertificationRequest.SetSubjectPKInfo(
  const Value: TSubjectPublicKeyInfo);
begin
  CertificationRequestInfo.SubjectPKInfo := Value;
end;
              
{$IFDEF INITIALIZATION}
initialization
{$ENDIF}
{$IFDEF INIT_SECTIONS}
  Initialize;
{$ENDIF}
{$IFDEF FINI_SECTIONS}
finalization
  Finalize;
{$ENDIF}
end.
