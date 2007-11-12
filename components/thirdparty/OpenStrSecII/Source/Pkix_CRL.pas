{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     PKIX_CRL Unit                                     }
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
unit Pkix_CRL;

interface

uses
  Classes,
{$IFDEF BCB}
  SecUtils,
{$ENDIF}
  Asn1, Pkix_Cert;

const
  crlf = #13#10;
{$NODEFINE ASNModule}
  ASNModule =
    'PKIX-CRL DEFINITIONS ::=' + crlf + 
    'BEGIN' + crlf + crlf +
    'IMPORTS AlgorithmIdentifier, Name, Time, EncapsulatedGeneralNames, EncapsulatedAuthorityKeyIdentifier, EncapsulatedInteger, GeneralNames, RelativeDistinguishedName FROM PKIX-Cert;' + crlf + crlf +
    'CertificateList ::= SEQUENCE {' + crlf + 
    '     tbsCertList'#9'TBSCertList,' + crlf + 
    '     signatureAlgorithm'#9'AlgorithmIdentifier,' + crlf + 
    '     signature'#9'BIT STRING}' + crlf + crlf + 
    'TBSCertList ::= SEQUENCE {' + crlf + 
    '     version'#9'INTEGER OPTIONAL,' + crlf +
    '     signatureAlgorithm'#9'AlgorithmIdentifier,' + crlf +
    '     issuer'#9'Name,' + crlf + 
    '     thisUpdate'#9'Time,' + crlf + 
    '     nextUpdate'#9'Time OPTIONAL,' + crlf + 
    '     revokedCertificates'#9'SEQUENCE OF RevokedCertificate OPTIONAL,' + crlf + 
    '     crlExtensions'#9'[0] CRLExtensions OPTIONAL}' + crlf + crlf + 
    'RevokedCertificate ::= SEQUENCE {' + crlf + 
    '     userCertificate'#9'CertificateSerialNumber,' + crlf + 
    '     revocationDate'#9'Time,' + crlf + 
    '     crlEntryExtensions'#9'EntryExtensions OPTIONAL}' + crlf + crlf + 
    'CertificateSerialNumber ::= INTEGER' + crlf + crlf + 
    'EntryExtensions ::= SEQUENCE OF EntryExtension' + crlf + crlf + 
    'EntryExtension ::= SEQUENCE {' + crlf + 
    '     extnId'#9'OBJECT,' + crlf + 
    '     critical'#9'BOOLEAN DEFAULT 010100,' + crlf + 
    '     extnValue'#9'EntryExtnValue.&Type(&extnID)}' + crlf + crlf + 
    'EntryExtnValue ::= TYPE-IDENTIFIER {' + crlf + 
    '     {EncapsulatedCRLReason'#9'IDENTIFIED BY id-ce-reasonCode}|' + crlf + 
    '     {EncapsulatedHoldInstruction'#9'IDENTIFIED BY id-ce-instructionCode}|' + crlf + 
    '     {EncapsulatedGeneralizedTime'#9'IDENTIFIED BY id-ce-invalidityDate}|' + crlf + 
    '     {EncapsulatedGeneralNames'#9'IDENTIFIED BY id-ce-certificateIssuer}}' + crlf + crlf + 
    'EncapsulatedCRLReason ::= OCTET STRING ENCAPSULATES CRLReason' + crlf + crlf + 
    'CRLReason ::= ENUMERATED' + crlf + crlf + crlf + 
    'id-ce-reasonCode OBJECT IDENTIFIER ::= {  (2) (5) 29 }' + crlf + crlf + 
    'EncapsulatedHoldInstruction ::= OCTET STRING ENCAPSULATES HoldInstruction' + crlf + crlf + 
    'HoldInstruction ::= OBJECT' + crlf + crlf + crlf + 
    'id-ce-instructionCode OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 23 }' + crlf + crlf + 
    'EncapsulatedGeneralizedTime ::= OCTET STRING ENCAPSULATES GeneralizedTime' + crlf + crlf + 
    'id-ce-invalidityDate OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 24 }' + crlf + crlf + 
    'id-ce-certificateIssuer OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 29 }' + crlf + crlf + crlf + 
    'CRLExtensions ::= SEQUENCE OF CRLExtension' + crlf + crlf + 
    'CRLExtension ::= SEQUENCE {' + crlf + 
    '     extnId'#9'OBJECT,' + crlf + 
    '     critical'#9'BOOLEAN DEFAULT 010100,' + crlf + 
    '     extnValue'#9'CRLExtnValue.&Type(&extnID)}' + crlf + crlf + 
    'CRLExtnValue ::= TYPE-IDENTIFIER {' + crlf + 
    '     {EncapsulatedAuthorityKeyIdentifier'#9'IDENTIFIED BY id-ce-authorityKeyIdentifier}|' + crlf + 
    '     {EncapsulatedGeneralNames'#9'IDENTIFIED BY id-ce-issuerAltName}|' + crlf +
    '     {EncapsulatedOctetInteger'#9'IDENTIFIED BY id-ce-cRLNumber}|' + crlf +
    '     {EncapsulatedOctetInteger'#9'IDENTIFIED BY id-ce-deltaCRLIndicator}|' + crlf +
    '     {EncapsulatedIssuingDistPointSyntax'#9'IDENTIFIED BY id-ce-issuingDistributionPoint}}' + crlf + crlf +
    'id-ce-authorityKeyIdentifier OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 35 }' + crlf + crlf +
    'id-ce-issuerAltName OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 18 }' + crlf + crlf +
    'EncapsulatedOctetInteger ::= OCTET STRING ENCAPSULATES INTEGER' + crlf + crlf +
    'id-ce-cRLNumber OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 20 }' + crlf + crlf +
    'id-ce-deltaCRLIndicator OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 27 }' + crlf + crlf +
    'EncapsulatedIssuingDistPointSyntax ::= OCTET STRING ENCAPSULATES IssuingDistPointSyntax' + crlf + crlf +
    'IssuingDistPointSyntax ::= SEQUENCE {' + crlf +
    '     distributionPoint'#9'[0] DistributionPointName OPTIONAL,' + crlf +
    '     onlyContainsUserCerts'#9'[1] IMPLICIT BOOLEAN DEFAULT 810100,' + crlf +
    '     onlyContainsCACerts'#9'[2] IMPLICIT BOOLEAN DEFAULT 820100,' + crlf +
    '     onlySomeReasons'#9'[3] IMPLICIT BIT STRING OPTIONAL,' + crlf +
    '     indirectCRL'#9'[4] IMPLICIT BOOLEAN DEFAULT 840100}' + crlf + crlf +
    'DistributionPointName ::= CHOICE {' + crlf +
    '     fullName'#9'[0] IMPLICIT GeneralNames,' + crlf +
    '     nameRelativeToCRLIssuer'#9'[1] IMPLICIT RelativeDistinguishedName}' + crlf + crlf + crlf +
    'id-ce-issuingDistributionPoint OBJECT IDENTIFIER ::= {  (2) (5) id-ce-reasonCode(29) 28 }' + crlf + crlf + 
    'END' + crlf + crlf;

type      
{ Declaration tree:
    TCertificateList
     TTbscertList
      TSequenceOfRevokedCertificate
       TRevokedCertificate
        TCertificateSerialNumber}

  TCertificateSerialNumber = TIntegerWrapper;

  TCrlreason = (
        crlrUnspecified,
        crlrKeyCompromise,
        crlrCACompromise,
        crlrAffiliationChanged,
        crlrSuperseded,
        crlrCessationOfOperation,
        crlrCertificateHold,
        crlrReserved7,
        crlrRemoveFromCRL);

{ Declaration tree: 
    TCertificateList
     TTbscertList
      TSequenceOfRevokedCertificate
       TRevokedCertificate
        TEntryExtensions
         TEntryExtension
          TEntryExtnValue}

  TEntryExtnValueEnum = (
    eeveUndefined, eeveIdCeReasonCode, eeveIdCeInstructionCode,
    eeveIdCeInvalidityDate, eeveIdCeCertificateIssuer);

  TEntryExtnValue = class(TASNChoiceWrapper)
  private
    function GetAsCe_ReasonCode: TCrlreason;
    procedure SetAsCe_ReasonCode(const Value: TCrlreason);
    function GetAsCe_InstructionCode: ObjectIdentifier;
    procedure SetAsCe_InstructionCode(const Value: ObjectIdentifier);
    function GetAsCe_InvalidityDate: TGeneralizedTime;
    procedure SetAsCe_InvalidityDate(const Value: TGeneralizedTime);
    function GetAsCe_CertificateIssuer: TGeneralNames;
    procedure SetAsCe_CertificateIssuer(const Value: TGeneralNames);
    function GetChoice: TEntryExtnValueEnum;
    procedure SetChoice(const Value: TEntryExtnValueEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsCe_ReasonCode: TCrlreason read GetAsCe_ReasonCode write SetAsCe_ReasonCode;
    property AsCe_InstructionCode: ObjectIdentifier read GetAsCe_InstructionCode write SetAsCe_InstructionCode;
    property AsCe_InvalidityDate: TGeneralizedTime read GetAsCe_InvalidityDate write SetAsCe_InvalidityDate;
    property AsCe_CertificateIssuer: TGeneralNames read GetAsCe_CertificateIssuer write SetAsCe_CertificateIssuer;
  published
    property Choice: TEntryExtnValueEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertificateList
     TTbscertList
      TSequenceOfRevokedCertificate
       TRevokedCertificate
        TEntryExtensions
         TEntryExtension}

  TEntryExtension = class(TASNConstructedWrapper)
  private
    function GetExtnId: ObjectIdentifier;
    procedure SetExtnId(const Value: ObjectIdentifier);
    function GetCritical: Boolean;
    procedure SetCritical(const Value: Boolean);
    function GetExtnValue: TEntryExtnValue;
    procedure SetExtnValue(const Value: TEntryExtnValue);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TEntryExtnValueEnum): Boolean;
    procedure SetType(Id: TEntryExtnValueEnum);
  published
    property ExtnId: ObjectIdentifier read GetExtnId write SetExtnId;
    property Critical: Boolean read GetCritical write SetCritical;
    property ExtnValue: TEntryExtnValue read GetExtnValue write SetExtnValue;
  end;

{ Declaration tree: 
    TCertificateList
     TTbscertList
      TSequenceOfRevokedCertificate
       TRevokedCertificate
        TEntryExtensions}

  TEntryExtensions = class(TASNCustomOFFieldWrapper)
  private
    function GetUniqueItem(Id: TEntryExtnValueEnum): TEntryExtension;
    function GetItems(Index: Integer): TEntryExtension;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function AddUniqueItem(Id: TEntryExtnValueEnum): TEntryExtension;
    function Add: TEntryExtension;
    property Items[Index: Integer]: TEntryExtension read GetItems;
    property UniqueItem[Id: TEntryExtnValueEnum]: TEntryExtension read GetUniqueItem; default;
  end;

{ Declaration tree: 
    TCertificateList
     TTbscertList
      TSequenceOfRevokedCertificate
       TRevokedCertificate}

  TRevokedCertificate = class(TASNConstructedWrapper)
  private
    function GetUserCertificate: TIntegerWrapper;
    procedure SetUserCertificate(const Value: TIntegerWrapper);
    function GetRevocationDate: TTime;
    procedure SetRevocationDate(const Value: TTime);
    function GetCrlEntryExtensions: TEntryExtensions;
    procedure SetCrlEntryExtensions(const Value: TEntryExtensions);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property UserCertificate: TIntegerWrapper read GetUserCertificate write SetUserCertificate;
    property RevocationDate: TTime read GetRevocationDate write SetRevocationDate;
    property CrlEntryExtensions: TEntryExtensions read GetCrlEntryExtensions write SetCrlEntryExtensions;
  end;

{ Declaration tree: 
    TCertificateList
     TTbscertList
      TSequenceOfRevokedCertificate}

  TSequenceOfRevokedCertificate = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TRevokedCertificate;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TRevokedCertificate;
    property Items[Index: Integer]: TRevokedCertificate read GetItems; default;
  end;

{ Declaration tree: 
    TCertificateList
     TTbscertList
      TCrlextensions
       TCrlextension
        TCrlextnValue
         TIssuingDistPointSyntax
          TDistributionPointName}

  TDistributionPointName = class(TASNChoiceWrapper)
  private
    function GetAsFullName: TGeneralNames;
    procedure SetAsFullName(const Value: TGeneralNames);
    function GetAsNameRelativeToCRLIssuer: TRelativeDistinguishedName;
    procedure SetAsNameRelativeToCRLIssuer(const Value: TRelativeDistinguishedName);
    function GetChoice: TDistributionPointNameEnum;
    procedure SetChoice(const Value: TDistributionPointNameEnum);
    function GetURI: string;
    procedure SetURI(const Value: string);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsFullName: TGeneralNames read GetAsFullName write SetAsFullName;
    property AsNameRelativeToCRLIssuer: TRelativeDistinguishedName read GetAsNameRelativeToCRLIssuer write SetAsNameRelativeToCRLIssuer;
    property URI: string read GetURI write SetURI;
  published
    property Choice: TDistributionPointNameEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertificateList
     TTbscertList
      TCrlextensions
       TCrlextension
        TCrlextnValue
         TIssuingDistPointSyntax}

  TIssuingDistPointSyntax = class(TASNConstructedWrapper)
  private
    function GetDistributionPoint: TDistributionPointName;
    procedure SetDistributionPoint(const Value: TDistributionPointName);
    function GetOnlyContainsUserCerts: Boolean;
    procedure SetOnlyContainsUserCerts(const Value: Boolean);
    function GetOnlyContainsCACerts: Boolean;
    procedure SetOnlyContainsCACerts(const Value: Boolean);
    function GetOnlySomeReasons: TBitString;
    procedure SetOnlySomeReasons(const Value: TBitString);
    function GetIndirectCRL: Boolean;
    procedure SetIndirectCRL(const Value: Boolean);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property DistributionPoint: TDistributionPointName read GetDistributionPoint write SetDistributionPoint;
    property OnlyContainsUserCerts: Boolean read GetOnlyContainsUserCerts write SetOnlyContainsUserCerts;
    property OnlyContainsCACerts: Boolean read GetOnlyContainsCACerts write SetOnlyContainsCACerts;
    property OnlySomeReasons: TBitString read GetOnlySomeReasons write SetOnlySomeReasons;
    property IndirectCRL: Boolean read GetIndirectCRL write SetIndirectCRL;
  end;

{ Declaration tree: 
    TCertificateList
     TTbscertList
      TCrlextensions
       TCrlextension
        TCrlextnValue}

  TCrlextnValueEnum = (
    cveUndefined, cveIdCeAuthorityKeyIdentifier, cveIdCeIssuerAltName,
    cveIdCeCRLNumber, cveIdCeDeltaCRLIndicator, cveIdCeIssuingDistributionPoint);

  TCrlextnValue = class(TASNChoiceWrapper)
  private
    function GetAsCe_AuthorityKeyIdentifier: TAuthorityKeyIdentifier;
    procedure SetAsCe_AuthorityKeyIdentifier(const Value: TAuthorityKeyIdentifier);
    function GetAsCe_IssuerAltName: TGeneralNames;
    procedure SetAsCe_IssuerAltName(const Value: TGeneralNames);
    function GetAsCe_CRLNumber: TIntegerWrapper;
    procedure SetAsCe_CRLNumber(const Value: TIntegerWrapper);
    function GetAsCe_DeltaCRLIndicator: TIntegerWrapper;
    procedure SetAsCe_DeltaCRLIndicator(const Value: TIntegerWrapper);
    function GetAsCe_IssuingDistributionPoint: TIssuingDistPointSyntax;
    procedure SetAsCe_IssuingDistributionPoint(const Value: TIssuingDistPointSyntax);
    function GetChoice: TCrlextnValueEnum;
    procedure SetChoice(const Value: TCrlextnValueEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsCe_AuthorityKeyIdentifier: TAuthorityKeyIdentifier read GetAsCe_AuthorityKeyIdentifier write SetAsCe_AuthorityKeyIdentifier;
    property AsCe_IssuerAltName: TGeneralNames read GetAsCe_IssuerAltName write SetAsCe_IssuerAltName;
    property AsCe_CRLNumber: TIntegerWrapper read GetAsCe_CRLNumber write SetAsCe_CRLNumber;
    property AsCe_DeltaCRLIndicator: TIntegerWrapper read GetAsCe_DeltaCRLIndicator write SetAsCe_DeltaCRLIndicator;
    property AsCe_IssuingDistributionPoint: TIssuingDistPointSyntax read GetAsCe_IssuingDistributionPoint write SetAsCe_IssuingDistributionPoint;
  published
    property Choice: TCrlextnValueEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertificateList
     TTbscertList
      TCrlextensions
       TCrlextension}

  TCrlextension = class(TASNConstructedWrapper)
  private
    function GetExtnId: ObjectIdentifier;
    procedure SetExtnId(const Value: ObjectIdentifier);
    function GetCritical: Boolean;
    procedure SetCritical(const Value: Boolean);
    function GetExtnValue: TCrlextnValue;
    procedure SetExtnValue(const Value: TCrlextnValue);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TCrlextnValueEnum): Boolean;
    procedure SetType(Id: TCrlextnValueEnum);
  published
    property ExtnId: ObjectIdentifier read GetExtnId write SetExtnId;
    property Critical: Boolean read GetCritical write SetCritical;
    property ExtnValue: TCrlextnValue read GetExtnValue write SetExtnValue;
  end;

{ Declaration tree: 
    TCertificateList
     TTbscertList
      TCrlextensions}

  TCrlextensions = class(TASNCustomOFFieldWrapper)
  private
    function GetUniqueItem(Id: TCrlextnValueEnum): TCrlextension;
    function GetItems(Index: Integer): TCrlextension;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function AddUniqueItem(Id: TCrlextnValueEnum): TCrlextension;
    function Add: TCrlextension;
    property Items[Index: Integer]: TCrlextension read GetItems;
    property UniqueItem[Id: TCrlextnValueEnum]: TCrlextension read GetUniqueItem; default;
  end;

{ Declaration tree: 
    TCertificateList
     TTbscertList}

  TTbscertList = class(TASNConstructedWrapper)
  private
    function GetVersion: TIntegerWrapper;
    procedure SetVersion(const Value: TIntegerWrapper);
    function GetSignatureAlgorithm: TAlgorithmIdentifier;
    procedure SetSignatureAlgorithm(const Value: TAlgorithmIdentifier);
    function GetIssuer: TName;
    procedure SetIssuer(const Value: TName);
    function GetThisUpdate: TTime;
    procedure SetThisUpdate(const Value: TTime);
    function GetNextUpdate: TTime;
    procedure SetNextUpdate(const Value: TTime);
    function GetRevokedCertificates: TSequenceOfRevokedCertificate;
    procedure SetRevokedCertificates(const Value: TSequenceOfRevokedCertificate);
    function GetCrlExtensions: TCrlextensions;
    procedure SetCrlExtensions(const Value: TCrlextensions);
    function GetURI: string;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property DistPointURI: string read GetURI;
  published
    property Version: TIntegerWrapper read GetVersion write SetVersion;
    property SignatureAlgorithm: TAlgorithmIdentifier read GetSignatureAlgorithm write SetSignatureAlgorithm;
    property Issuer: TName read GetIssuer write SetIssuer;
    property ThisUpdate: TTime read GetThisUpdate write SetThisUpdate;
    property NextUpdate: TTime read GetNextUpdate write SetNextUpdate;
    property RevokedCertificates: TSequenceOfRevokedCertificate read GetRevokedCertificates write SetRevokedCertificates;
    property CrlExtensions: TCrlextensions read GetCrlExtensions write SetCrlExtensions;
  end;

{ Declaration tree: 
    TCertificateList}

  TCertificateList = class(TSigned,ISigned)
  private
    function GetTbsCertList: TTbscertList;
    procedure SetTbsCertList(const Value: TTbscertList);
  protected            
    function BeforeSign(var CACert: TCertificate;
                        SignAlg: ObjectIdentifier;
                        out Params: TASNCustomWrapper): Boolean; override;
    function CheckSignAlgCoherence: Boolean; override;
    function IsAllegedIssuer(CACert: TCertificate): Boolean; override;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property TbsCertList: TTbscertList read GetTbsCertList write SetTbsCertList;
  end;            

{$IFNDEF INIT_SECTIONS}
procedure Initialize;
{$ENDIF}
{$IFNDEF FINI_SECTIONS}
procedure Finalize;
{$ENDIF}

implementation

var
  GlobalObject: TASN1Struct;

procedure InitGlobalObject;
var
  SS: TStringStream;
begin
  Assert(PKIX_Cert.ASNModule <> '');
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

{ TEntryExtnValue }

constructor TEntryExtnValue.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'PKIX_CRL not initialized: Call StrSecInit.InitPKIX_CRL to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertList/revokedCertificates')^.Template.FindField('crlEntryExtensions')^.Template;
    F := F.FindField('extnValue')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TEnumeratedWrapper);
  FChoiceList.Add(TStringWrapper);
  FChoiceList.Add(TGeneralizedTime);
  FChoiceList.Add(TGeneralNames);
end;

destructor TEntryExtnValue.Destroy;
begin
  inherited Destroy;
end;

function TEntryExtnValue.GetAsCe_ReasonCode: TCrlreason;
begin
  Result := TCrlreason((FSelected as TEnumeratedWrapper).Value);
end;

procedure TEntryExtnValue.SetAsCe_ReasonCode(const Value: TCrlreason);
begin
  InternalSelectChoice(0);
  (FSelected as TEnumeratedWrapper).Value := Ord(Value);
end;

function TEntryExtnValue.GetAsCe_InstructionCode: ObjectIdentifier;
begin
  Result := (FSelected as TStringWrapper).Value;
end;

procedure TEntryExtnValue.SetAsCe_InstructionCode(const Value: ObjectIdentifier);
begin
  InternalSelectChoice(1);
  (FSelected as TStringWrapper).Value := Value;
end;

function TEntryExtnValue.GetAsCe_InvalidityDate: TGeneralizedTime;
begin
  Result := (FSelected as TGeneralizedTime);
end;

procedure TEntryExtnValue.SetAsCe_InvalidityDate(const Value: TGeneralizedTime);
begin
  InternalSelectChoice(2);
  (FSelected as TGeneralizedTime).Assign(Value);
end;

function TEntryExtnValue.GetAsCe_CertificateIssuer: TGeneralNames;
begin
  Result := (FSelected as TGeneralNames);
end;

procedure TEntryExtnValue.SetAsCe_CertificateIssuer(const Value: TGeneralNames);
begin
  InternalSelectChoice(3);
  (FSelected as TGeneralNames).Assign(Value);
end;

function TEntryExtnValue.GetChoice: TEntryExtnValueEnum;
begin
  Result := TEntryExtnValueEnum(InternalGetChoice);
end;

procedure TEntryExtnValue.SetChoice(const Value: TEntryExtnValueEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TEntryExtension }

constructor TEntryExtension.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'PKIX_CRL not initialized: Call StrSecInit.InitPKIX_CRL to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertList/revokedCertificates')^.Template.FindField('crlEntryExtensions')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TBooleanWrapper,nil,FData.Items[1]^,nil^);
  FieldFactory(TEntryExtnValue,nil,FData.Items[2]^,nil^);
end;

destructor TEntryExtension.Destroy;
begin
  inherited Destroy;
end;

function TEntryExtension.GetExtnId: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TEntryExtension.SetExtnId(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TEntryExtension.GetCritical: Boolean;
begin
  Result := Boolean(TBooleanWrapper(FItems[1]).Value);
end;

procedure TEntryExtension.SetCritical(const Value: Boolean);
begin
  TBooleanWrapper(FItems[1]).Value := Value;
end;

function TEntryExtension.GetExtnValue: TEntryExtnValue;
begin
  Result := FItems[2];
end;

procedure TEntryExtension.SetExtnValue(const Value: TEntryExtnValue);
begin
  TEntryExtnValue(FItems[2]).Assign(Value);
end;

function TEntryExtension.IsType(Id: TEntryExtnValueEnum): Boolean;
begin
  Result := TEntryExtnValue(FItems[2]).Choice = Id;
end;

procedure TEntryExtension.SetType(Id: TEntryExtnValueEnum);
begin
  TEntryExtnValue(FItems[2]).Choice := Id;
end;

{ TEntryExtensions }

constructor TEntryExtensions.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'PKIX_CRL not initialized: Call StrSecInit.InitPKIX_CRL to correct.');
  FFieldType := TEntryExtension;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertList/revokedCertificates')^.Template;
    F := F.FindField('crlEntryExtensions')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TEntryExtensions.Destroy;
begin
  inherited Destroy;
end;

function TEntryExtensions.GetUniqueItem(Id: TEntryExtnValueEnum): TEntryExtension;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do begin
    Result := Items[I];
    if Result.IsType(Id) then
      Exit;
  end;
  Result := nil;
end;

function TEntryExtensions.AddUniqueItem(Id: TEntryExtnValueEnum): TEntryExtension;
begin
  Result := GetUniqueItem(Id);
  if Result = nil then begin
    Result := Add;
    Result.SetType(Id);
  end;
end;

function TEntryExtensions.Add: TEntryExtension;
begin
  Result := TEntryExtension(InternalAdd);
end;

function TEntryExtensions.GetItems(Index: Integer): TEntryExtension;
begin
  Result := TEntryExtension(InternalGetItems(Index));
end;

{ TRevokedCertificate }

constructor TRevokedCertificate.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRL not initialized: Call StrSecInit.InitPKIX_CRL to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertList/revokedCertificates')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TTime,nil,FData.Items[1]^,nil^);
  FieldFactory(TEntryExtensions,nil,FData.Items[2]^,nil^);
end;

destructor TRevokedCertificate.Destroy;
begin
  inherited Destroy;
end;

function TRevokedCertificate.GetUserCertificate: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TRevokedCertificate.SetUserCertificate(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TRevokedCertificate.GetRevocationDate: TTime;
begin
  Result := FItems[1];
end;

procedure TRevokedCertificate.SetRevocationDate(const Value: TTime);
begin
  TTime(FItems[1]).Assign(Value);
end;

function TRevokedCertificate.GetCrlEntryExtensions: TEntryExtensions;
begin
  Result := FItems[2];
end;

procedure TRevokedCertificate.SetCrlEntryExtensions(const Value: TEntryExtensions);
begin
  TEntryExtensions(FItems[2]).Assign(Value);
end;

{ TSequenceOfRevokedCertificate }

constructor TSequenceOfRevokedCertificate.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRL not initialized: Call StrSecInit.InitPKIX_CRL to correct.');
  FFieldType := TRevokedCertificate;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertList/revokedCertificates')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TSequenceOfRevokedCertificate.Destroy;
begin
  inherited Destroy;
end;

function TSequenceOfRevokedCertificate.Add: TRevokedCertificate;
begin
  Result := TRevokedCertificate(InternalAdd);
end;

function TSequenceOfRevokedCertificate.GetItems(Index: Integer): TRevokedCertificate;
begin
  Result := TRevokedCertificate(InternalGetItems(Index));
end;

{ TDistributionPointName }

constructor TDistributionPointName.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRL not initialized: Call StrSecInit.InitPKIX_CRL to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertList/crlExtensions/')^.Template.FindField('extnValue')^.Choices[4]^.Encapsulated;
    F := F.FindField('distributionPoint')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TGeneralNames);
  FChoiceList.Add(TRelativeDistinguishedName);
end;

destructor TDistributionPointName.Destroy;
begin
  inherited Destroy;
end;

function TDistributionPointName.GetAsFullName: TGeneralNames;
begin
  Result := (FSelected as TGeneralNames);
end;

procedure TDistributionPointName.SetAsFullName(const Value: TGeneralNames);
begin
  InternalSelectChoice(0);
  (FSelected as TGeneralNames).Assign(Value);
end;

function TDistributionPointName.GetAsNameRelativeToCRLIssuer: TRelativeDistinguishedName;
begin
  Result := (FSelected as TRelativeDistinguishedName);
end;

procedure TDistributionPointName.SetAsNameRelativeToCRLIssuer(const Value: TRelativeDistinguishedName);
begin
  InternalSelectChoice(1);
  (FSelected as TRelativeDistinguishedName).Assign(Value);
end;

function TDistributionPointName.GetChoice: TDistributionPointNameEnum;
begin
  Result := TDistributionPointNameEnum(InternalGetChoice);
end;

procedure TDistributionPointName.SetChoice(const Value: TDistributionPointNameEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

function TDistributionPointName.GetURI: string;
begin
  InternalSelectChoice(0);
  Result := AsFullName.URI;
end;

procedure TDistributionPointName.SetURI(const Value: string);
begin
  InternalSelectChoice(0);
  AsFullName.URI := Value;
end;

{ TIssuingDistPointSyntax }

constructor TIssuingDistPointSyntax.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRL not initialized: Call StrSecInit.InitPKIX_CRL to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertList/crlExtensions/')^.Template.FindField('extnValue')^.Choices[4]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TDistributionPointName,nil,FData.Encapsulated.Items[0]^.Items[0]^,nil^);
  FieldFactory(TBooleanWrapper,nil,FData.Encapsulated.Items[1]^,nil^);
  FieldFactory(TBooleanWrapper,nil,FData.Encapsulated.Items[2]^,nil^);
  FieldFactory(TBitString,nil,FData.Encapsulated.Items[3]^,nil^);
  FieldFactory(TBooleanWrapper,nil,FData.Encapsulated.Items[4]^,nil^);
end;

destructor TIssuingDistPointSyntax.Destroy;
begin
  inherited Destroy;
end;

function TIssuingDistPointSyntax.GetDistributionPoint: TDistributionPointName;
begin
  Result := FItems[0];
end;

procedure TIssuingDistPointSyntax.SetDistributionPoint(const Value: TDistributionPointName);
begin
  TDistributionPointName(FItems[0]).Assign(Value);
end;

function TIssuingDistPointSyntax.GetOnlyContainsUserCerts: Boolean;
begin
  Result := Boolean(TBooleanWrapper(FItems[1]).Value);
end;

procedure TIssuingDistPointSyntax.SetOnlyContainsUserCerts(const Value: Boolean);
begin
  TBooleanWrapper(FItems[1]).Value := Value;
end;

function TIssuingDistPointSyntax.GetOnlyContainsCACerts: Boolean;
begin
  Result := Boolean(TBooleanWrapper(FItems[2]).Value);
end;

procedure TIssuingDistPointSyntax.SetOnlyContainsCACerts(const Value: Boolean);
begin
  TBooleanWrapper(FItems[2]).Value := Value;
end;

function TIssuingDistPointSyntax.GetOnlySomeReasons: TBitString;
begin
  Result := FItems[3];
end;

procedure TIssuingDistPointSyntax.SetOnlySomeReasons(const Value: TBitString);
begin
  TBitString(FItems[3]).Assign(Value);
end;

function TIssuingDistPointSyntax.GetIndirectCRL: Boolean;
begin
  Result := Boolean(TBooleanWrapper(FItems[4]).Value);
end;

procedure TIssuingDistPointSyntax.SetIndirectCRL(const Value: Boolean);
begin
  TBooleanWrapper(FItems[4]).Value := Value;
end;

{ TCrlextnValue }

constructor TCrlextnValue.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRL not initialized: Call StrSecInit.InitPKIX_CRL to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertList/crlExtensions/')^.Template;
    F := F.FindField('extnValue')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TAuthorityKeyIdentifier);
  FChoiceList.Add(TGeneralNames);
  FChoiceList.Add(TIntegerWrapper);
  FChoiceList.Add(TIntegerWrapper);
  FChoiceList.Add(TIssuingDistPointSyntax);
end;

destructor TCrlextnValue.Destroy;
begin
  inherited Destroy;
end;

function TCrlextnValue.GetAsCe_AuthorityKeyIdentifier: TAuthorityKeyIdentifier;
begin
  Result := (FSelected as TAuthorityKeyIdentifier);
end;

procedure TCrlextnValue.SetAsCe_AuthorityKeyIdentifier(const Value: TAuthorityKeyIdentifier);
begin
  InternalSelectChoice(0);
  (FSelected as TAuthorityKeyIdentifier).Assign(Value);
end;

function TCrlextnValue.GetAsCe_IssuerAltName: TGeneralNames;
begin
  Result := (FSelected as TGeneralNames);
end;

procedure TCrlextnValue.SetAsCe_IssuerAltName(const Value: TGeneralNames);
begin
  InternalSelectChoice(1);
  (FSelected as TGeneralNames).Assign(Value);
end;

function TCrlextnValue.GetAsCe_CRLNumber: TIntegerWrapper;
begin
  Result := (FSelected as TIntegerWrapper);
end;

procedure TCrlextnValue.SetAsCe_CRLNumber(const Value: TIntegerWrapper);
begin
  InternalSelectChoice(2);
  (FSelected as TIntegerWrapper).Assign(Value);
end;

function TCrlextnValue.GetAsCe_DeltaCRLIndicator: TIntegerWrapper;
begin
  Result := (FSelected as TIntegerWrapper);
end;

procedure TCrlextnValue.SetAsCe_DeltaCRLIndicator(const Value: TIntegerWrapper);
begin
  InternalSelectChoice(3);
  (FSelected as TIntegerWrapper).Assign(Value);
end;

function TCrlextnValue.GetAsCe_IssuingDistributionPoint: TIssuingDistPointSyntax;
begin
  Result := (FSelected as TIssuingDistPointSyntax);
end;

procedure TCrlextnValue.SetAsCe_IssuingDistributionPoint(const Value: TIssuingDistPointSyntax);
begin
  InternalSelectChoice(4);
  (FSelected as TIssuingDistPointSyntax).Assign(Value);
end;

function TCrlextnValue.GetChoice: TCrlextnValueEnum;
begin
  Result := TCrlextnValueEnum(InternalGetChoice);
end;

procedure TCrlextnValue.SetChoice(const Value: TCrlextnValueEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TCrlextension }

constructor TCrlextension.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRL not initialized: Call StrSecInit.InitPKIX_CRL to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertList/crlExtensions/')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TBooleanWrapper,nil,FData.Items[1]^,nil^);
  FieldFactory(TCrlextnValue,nil,FData.Items[2]^,nil^);
end;

destructor TCrlextension.Destroy;
begin
  inherited Destroy;
end;

function TCrlextension.GetExtnId: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TCrlextension.SetExtnId(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TCrlextension.GetCritical: Boolean;
begin
  Result := Boolean(TBooleanWrapper(FItems[1]).Value);
end;

procedure TCrlextension.SetCritical(const Value: Boolean);
begin
  TBooleanWrapper(FItems[1]).Value := Value;
end;

function TCrlextension.GetExtnValue: TCrlextnValue;
begin
  Result := FItems[2];
end;

procedure TCrlextension.SetExtnValue(const Value: TCrlextnValue);
begin
  TCrlextnValue(FItems[2]).Assign(Value);
end;

function TCrlextension.IsType(Id: TCrlextnValueEnum): Boolean;
begin
  Result := TCrlextnValue(FItems[2]).Choice = Id;
end;

procedure TCrlextension.SetType(Id: TCrlextnValueEnum);
begin
  TCrlextnValue(FItems[2]).Choice := Id;
end;

{ TCrlextensions }

constructor TCrlextensions.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRL not initialized: Call StrSecInit.InitPKIX_CRL to correct.');
  FFieldType := TCrlextension;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('/tbsCertList/crlExtensions/')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TCrlextensions.Destroy;
begin
  inherited Destroy;
end;

function TCrlextensions.GetUniqueItem(Id: TCrlextnValueEnum): TCrlextension;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do begin
    Result := Items[I];
    if Result.IsType(Id) then
      Exit;
  end;
  Result := nil;
end;

function TCrlextensions.AddUniqueItem(Id: TCrlextnValueEnum): TCrlextension;
begin
  Result := GetUniqueItem(Id);
  if Result = nil then begin
    Result := Add;
    Result.SetType(Id);
  end;
end;

function TCrlextensions.Add: TCrlextension;
begin
  Result := TCrlextension(InternalAdd);
end;

function TCrlextensions.GetItems(Index: Integer): TCrlextension;
begin
  Result := TCrlextension(InternalGetItems(Index));
end;

{ TTbscertList }

constructor TTbscertList.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRL not initialized: Call StrSecInit.InitPKIX_CRL to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.FindField('tbsCertList')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TAlgorithmIdentifier,nil,FData.Items[1]^,nil^);
  FieldFactory(TName,nil,FData.Items[2]^,nil^);
  FieldFactory(TTime,nil,FData.Items[3]^,nil^);
  FieldFactory(TTime,nil,FData.Items[4]^,nil^);
  FieldFactory(TSequenceOfRevokedCertificate,nil,FData.Items[5]^,nil^);
  FieldFactory(TCrlextensions,nil,FData.Items[6]^.Items[0]^,nil^);
end;

destructor TTbscertList.Destroy;
begin
  inherited Destroy;
end;

function TTbscertList.GetVersion: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TTbscertList.SetVersion(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TTbscertList.GetSignatureAlgorithm: TAlgorithmIdentifier;
begin
  Result := FItems[1];
end;

procedure TTbscertList.SetSignatureAlgorithm(const Value: TAlgorithmIdentifier);
begin
  TAlgorithmIdentifier(FItems[1]).Assign(Value);
end;

function TTbscertList.GetIssuer: TName;
begin
  Result := FItems[2];
end;

procedure TTbscertList.SetIssuer(const Value: TName);
begin
  TName(FItems[2]).Assign(Value);
end;

function TTbscertList.GetThisUpdate: TTime;
begin
  Result := FItems[3];
end;

procedure TTbscertList.SetThisUpdate(const Value: TTime);
begin
  TTime(FItems[3]).Assign(Value);
end;

function TTbscertList.GetNextUpdate: TTime;
begin
  Result := FItems[4];
end;

procedure TTbscertList.SetNextUpdate(const Value: TTime);
begin
  TTime(FItems[4]).Assign(Value);
end;

function TTbscertList.GetRevokedCertificates: TSequenceOfRevokedCertificate;
begin
  Result := FItems[5];
end;

procedure TTbscertList.SetRevokedCertificates(const Value: TSequenceOfRevokedCertificate);
begin
  TSequenceOfRevokedCertificate(FItems[5]).Assign(Value);
end;

function TTbscertList.GetCrlExtensions: TCrlextensions;
begin
  Result := FItems[6];
end;

procedure TTbscertList.SetCrlExtensions(const Value: TCrlextensions);
begin
  TCrlextensions(FItems[6]).Assign(Value);
end;

function TTbscertList.GetURI: string;
var
  Ext: TCrlExtension;
begin
  Ext := CrlExtensions.UniqueItem[cveIdCeIssuingDistributionPoint];
  if Assigned(Ext) then
    Result := Ext.ExtnValue.AsCe_IssuingDistributionPoint.DistributionPoint.URI
  else
    Result := '';
end;

{ TCertificateList }

constructor TCertificateList.Create;
begin                      
  Assert(Assigned(GlobalObject),'PKIX_CRL not initialized: Call StrSecInit.InitPKIX_CRL to correct.');
  inherited;
  FData.Assign(GlobalObject);
  FieldFactory(TTbscertList,nil,FData.Items[0]^,nil^);
  FieldFactory(TAlgorithmIdentifier,nil,FData.Items[1]^,nil^);
  FieldFactory(TBitString,nil,FData.Items[2]^,nil^);
end;

destructor TCertificateList.Destroy;
begin
  inherited Destroy;
end;

function TCertificateList.GetTbsCertList: TTbscertList;
begin
  Result := FItems[0];
end;

procedure TCertificateList.SetTbsCertList(const Value: TTbscertList);
begin
  TTbscertList(FItems[0]).Assign(Value);
end;

function TCertificateList.BeforeSign(var CACert: TCertificate;
                                     SignAlg: ObjectIdentifier;
                                     out Params: TASNCustomWrapper): Boolean;
var
  SExt, Ext: TExtension;
  Ext0: TCrlExtension;
begin
  Result := True;
  TBSCertList.SignatureAlgorithm.Algorithm := SignAlg;

  TBSCertList.Issuer := CACert.TbsCertificate.Subject;

  SExt := CACert.TbsCertificate.Extensions.UniqueItem[eveIdCeSubjectAltName];
  if Assigned(SExt) then begin
    Ext0 := TBSCertList.CrlExtensions.AddUniqueItem(cveIdCeIssuerAltName);
    Ext0.ExtnValue.AsCe_IssuerAltName := SExt.ExtnValue.AsCe_SubjectAltName;
  end;

  Ext := CACert.TbsCertificate.Extensions.UniqueItem[eveIdCeSubjectKeyIdentifier];
  if Assigned(Ext) then begin
    Ext0 := TBSCertList.CrlExtensions.AddUniqueItem(cveIdCeAuthorityKeyIdentifier);
    Ext0.ExtnValue.AsCe_AuthorityKeyIdentifier.KeyIdentifier :=
      Ext.ExtnValue.AsCe_SubjectKeyIdentifier;
  end;

  FData.SortSET;
end;

function TCertificateList.CheckSignAlgCoherence: Boolean;
begin
  Result := SignatureAlgorithm.Algorithm = TbsCertList.SignatureAlgorithm.Algorithm;
end;

function TCertificateList.IsAllegedIssuer(CACert: TCertificate): Boolean;
var
  UKI, SKI: TExtension;
  AKI: TCrlExtension;
  A, S: TOctetString;
begin
  Result := False;

  UKI := CACert.TBSCertificate.Extensions.UniqueItem[eveIdCeKeyUsage];
  if Assigned(UKI) then
    if not UKI.ExtnValue.AsCe_KeyUsage.Bits[keyCertSignVal] then
      Exit;

  AKI := TBSCertList.CrlExtensions.UniqueItem[cveIdCeAuthorityKeyIdentifier];
  if Assigned(AKI) then begin
    A := AKI.ExtnValue.AsCe_AuthorityKeyIdentifier.KeyIdentifier;
    SKI := CACert.TbsCertificate.Extensions.UniqueItem[eveIdCeSubjectKeyIdentifier];
    Result := Assigned(SKI);
    if Result then begin
      S := SKI.ExtnValue.AsCe_SubjectKeyIdentifier;
      Result := (A.Length = S.Length) and
                (A.AsString = S.AsString);
      if Result then begin
        Result := TBSCertList.Issuer.Compare(CACert.TbsCertificate.Subject);
        if Result then begin
          AKI := TBSCertList.CrlExtensions.UniqueItem[cveIdCeIssuerAltName];
          SKI := CACert.TbsCertificate.Extensions.UniqueItem[eveIdCeSubjectAltName];
          Result := Assigned(AKI) = Assigned(SKI);
          if Result and Assigned(AKI) then
            Result := AKI.GetExtnValue.AsCe_IssuerAltName.Compare(SKI.ExtnValue.AsCe_SubjectAltName);
        end;
      end;
    end;
  end else begin
    Result := TBSCertList.Issuer.Compare(CACert.TbsCertificate.Subject);
    if Result then begin
      AKI := TBSCertList.CrlExtensions.UniqueItem[cveIdCeIssuerAltName];
      SKI := CACert.TbsCertificate.Extensions.UniqueItem[eveIdCeSubjectAltName];
      Result := Assigned(AKI) = Assigned(SKI);
      if Result and Assigned(AKI) then
        Result := AKI.GetExtnValue.AsCe_IssuerAltName.Compare(SKI.ExtnValue.AsCe_SubjectAltName);
    end;
  end;
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
