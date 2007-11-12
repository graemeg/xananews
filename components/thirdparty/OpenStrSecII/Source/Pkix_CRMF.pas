{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     PKIX_CRMF Unit                                    }
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
unit Pkix_CRMF;

interface

uses
  SysUtils, Classes, Asn1, Pkix_Cert, Cms, Pkix_CRL, SecUtils, MpIF, MpDL, MpEC;

const
  crlf = #13#10;
  ASNModule =
    'PKIX-CRMF DEFINITIONS ::=' + crlf +
    'BEGIN' + crlf + crlf +
    'IMPORTS EnvelopedData, DigestAlgorithmIdentifier, MessageAuthenticationCodeAlgorithm FROM CryptographicMessageSyntax {  iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-9(9) smime(16) modules(0) cms(1) };' + crlf +
    'IMPORTS AlgorithmIdentifier, Name, Time, SubjectPublicKeyInfo, Extensions FROM PKIX-Cert;' + crlf + crlf +
    'CertReqMessages ::= SEQUENCE OF CertReqMsg' + crlf + crlf +
    'CertReqMsg ::= SEQUENCE {' + crlf +
    '     certReq'#9'CertRequest,' + crlf +
    '     pop'#9'ProofOfPossession OPTIONAL,' + crlf +
    '     regInfo'#9'SEQUENCE OF RegInfo OPTIONAL}' + crlf + crlf +
    'CertRequest ::= SEQUENCE {' + crlf +
    '     certReqId'#9'INTEGER,' + crlf +
    '     certTemplate'#9'CertTemplate,' + crlf +
    '     controls'#9'Controls OPTIONAL}' + crlf + crlf +
    'CertTemplate ::= SEQUENCE {' + crlf +
    '     version'#9'[0] IMPLICIT INTEGER OPTIONAL,' + crlf +
    '     serialNumber'#9'[1] IMPLICIT INTEGER OPTIONAL,' + crlf +
    '     signingAlg'#9'[2] IMPLICIT AlgorithmIdentifier OPTIONAL,' + crlf +
    '     issuer'#9'[3] Name OPTIONAL,' + crlf +
    '     validity'#9'[4] IMPLICIT OptionalValidity OPTIONAL,' + crlf +
    '     subject'#9'[5] Name OPTIONAL,' + crlf +
    '     publicKey'#9'[6] IMPLICIT SubjectPublicKeyInfo OPTIONAL,' + crlf +
    '     issuerUID'#9'[7] IMPLICIT BIT STRING OPTIONAL,' + crlf +
    '     subjectUID'#9'[8] IMPLICIT BIT STRING OPTIONAL,' + crlf +
    '     extensions'#9'[9] IMPLICIT Extensions OPTIONAL}' + crlf + crlf +
    'OptionalValidity ::= SEQUENCE {' + crlf +
    '     notBefore'#9'[0] Time OPTIONAL,' + crlf +
    '     notAfter'#9'[1] Time OPTIONAL}' + crlf + crlf +
    'Controls ::= SEQUENCE OF ControlTypeAndValue' + crlf + crlf +
    'ControlTypeAndValue ::= SEQUENCE {' + crlf + 
    '     controlID'#9'OBJECT,' + crlf + 
    '     value'#9'ControlValue.&Type(&controlID)}' + crlf + crlf + 
    'ControlValue ::= TYPE-IDENTIFIER {' + crlf +
    '     {UTF8STRING'#9'IDENTIFIED BY id-regCtrl-regToken}|' + crlf +
    '     {UTF8STRING'#9'IDENTIFIED BY id-regCtrl-authenticator}|' + crlf + 
    '     {PKIPublicationInfo'#9'IDENTIFIED BY id-regCtrl-pkiPublicationInfo}|' + crlf + 
    '     {PKIArchiveOptions'#9'IDENTIFIED BY id-regCtrl-pkiArchiveOptions}|' + crlf + 
    '     {SubjectPublicKeyInfo'#9'IDENTIFIED BY id-regCtrl-protocolEncrKey}|' + crlf + 
    '     {OldCertId'#9'IDENTIFIED BY id-regCtrl-oldCertID}}' + crlf + crlf + 
    'id-regCtrl OBJECT IDENTIFIER ::= {  iso(1) org(3) dod(6) internet(1) security(5) mechanisms(5) id-pkix(7) id-pkip(5) 1 }' + crlf + crlf +
    'id-regCtrl-regToken            OBJECT IDENTIFIER ::= { id-regCtrl 1 }' + crlf + crlf +
    'id-regCtrl-authenticator OBJECT IDENTIFIER ::= {  iso(1) org(3) dod(6) internet(1) security(5) mechanisms(5) id-pkix(7) id-pkip(5) id-regCtrl(1) 2 }' + crlf + crlf +
    'PKIPublicationInfo ::= SEQUENCE {' + crlf + 
    '     action'#9'INTEGER,' + crlf + 
    '     pubInfos'#9'SEQUENCE OF SinglePubInfo OPTIONAL}' + crlf + crlf + 
    'SinglePubInfo ::= SEQUENCE {' + crlf + 
    '     pubMethod'#9'INTEGER,' + crlf + 
    '     pubLocation'#9'GeneralName OPTIONAL}' + crlf + crlf + 
    'id-regCtrl-pkiPublicationInfo OBJECT IDENTIFIER ::= {  iso(1) org(3) dod(6) internet(1) security(5) mechanisms(5) id-pkix(7) id-pkip(5) id-regCtrl(1) 3 }' + crlf + crlf + 
    'PKIArchiveOptions ::= CHOICE {' + crlf +
    '     encryptedPrivKey'#9'[0] EncryptedKey,' + crlf +
    '     keyGenParameters'#9'[1] IMPLICIT OCTET STRING,' + crlf + 
    '     archiveRemGenPrivKey'#9'[2] IMPLICIT BOOLEAN}' + crlf + crlf + 
    'EncryptedKey ::= CHOICE {' + crlf + 
    '     encryptedValue'#9'EncryptedValue,' + crlf + 
    '     envelopedData'#9'[0] IMPLICIT EnvelopedData}' + crlf + crlf + 
    'EncryptedValue ::= SEQUENCE {' + crlf + 
    '     intendedAlg'#9'[0] IMPLICIT AlgorithmIdentifier OPTIONAL,' + crlf + 
    '     symmAlg'#9'[1] IMPLICIT AlgorithmIdentifier OPTIONAL,' + crlf + 
    '     encSymmKey'#9'[2] IMPLICIT BIT STRING OPTIONAL,' + crlf + 
    '     keyAlg'#9'[3] IMPLICIT AlgorithmIdentifier OPTIONAL,' + crlf + 
    '     valueHint'#9'[4] IMPLICIT OCTET STRING OPTIONAL,' + crlf + 
    '     encValue'#9'BIT STRING}' + crlf + crlf + 
    'id-regCtrl-pkiArchiveOptions OBJECT IDENTIFIER ::= {  iso(1) org(3) dod(6) internet(1) security(5) mechanisms(5) id-pkix(7) id-pkip(5) id-regCtrl(1) 4 }' + crlf + crlf + 
    'id-regCtrl-protocolEncrKey OBJECT IDENTIFIER ::= {  iso(1) org(3) dod(6) internet(1) security(5) mechanisms(5) id-pkix(7) id-pkip(5) id-regCtrl(1) 6 }' + crlf + crlf + 
    'OldCertId ::= CertId' + crlf + crlf + 
    'CertId ::= SEQUENCE {' + crlf + 
    '     issuer'#9'GeneralName,' + crlf + 
    '     serialNumber'#9'INTEGER}' + crlf + crlf + 
    'id-regCtrl-oldCertID OBJECT IDENTIFIER ::= {  iso(1) org(3) dod(6) internet(1) security(5) mechanisms(5) id-pkix(7) id-pkip(5) id-regCtrl(1) 5 }' + crlf + crlf + 
    'ProofOfPossession ::= CHOICE {' + crlf + 
    '     raVerified'#9'[0] IMPLICIT NULL,' + crlf + 
    '     signature'#9'[1] IMPLICIT POPOSigningKey,' + crlf +
    '     keyEncipherment'#9'[2] POPOPrivKey,' + crlf +
    '     keyAgreement'#9'[3] POPOPrivKey}' + crlf + crlf +
    'POPOSigningKey ::= SEQUENCE {' + crlf + 
    '     poposkInput'#9'[0] IMPLICIT POPOSigningKeyInput OPTIONAL,' + crlf + 
    '     algorithmIdentifier'#9'AlgorithmIdentifier,' + crlf + 
    '     signature'#9'BIT STRING}' + crlf + crlf + 
    'POPOSigningKeyInput ::= SEQUENCE {' + crlf + 
    '     authInfo'#9'AuthInfo,' + crlf + 
    '     publicKey'#9'SubjectPublicKeyInfo}' + crlf + crlf + 
    'AuthInfo ::= CHOICE {' + crlf + 
    '     publicKeyMAC'#9'PKMACValue,' + crlf + 
    '     sender'#9'[0] IMPLICIT GeneralName}' + crlf + crlf + 
    'PKMACValue ::= SEQUENCE {' + crlf + 
    '     algId'#9'PBMIdentifier,' + crlf + 
    '     value'#9'BIT STRING}' + crlf + crlf + 
    'PBMIdentifier ::= SEQUENCE {' + crlf + 
    '     algorithm'#9'OBJECT,' + crlf + 
    '     parameters'#9'PBMType.&Type(&algorithm)}' + crlf + crlf + 
    'PBMType ::= TYPE-IDENTIFIER {' + crlf + 
    '     {PBMParameter'#9'IDENTIFIED BY PasswordBasedMac}}' + crlf + crlf + 
    'PBMParameter ::= SEQUENCE {' + crlf + 
    '     salt'#9'OCTET STRING,' + crlf + 
    '     owf'#9'DigestAlgorithmIdentifier,' + crlf + 
    '     iterationCount'#9'INTEGER,' + crlf + 
    '     mac'#9'MessageAuthenticationCodeAlgorithm}' + crlf + crlf + 
    'PasswordBasedMac OBJECT IDENTIFIER ::= {  iso(1) member-body(2) us(840) (113533) (7) 66 }' + crlf + crlf + 
    'POPOPrivKey ::= CHOICE {' + crlf + 
    '     thisMessage'#9'[0] IMPLICIT BIT STRING,' + crlf + 
    '     subsequentMessage'#9'[1] IMPLICIT INTEGER,' + crlf + 
    '     dhMAC'#9'[2] IMPLICIT BIT STRING}' + crlf + crlf + 
    'RegInfo ::= SEQUENCE {' + crlf + 
    '     regID'#9'OBJECT,' + crlf + 
    '     regValue'#9'RegValue.&Type(&regID)}' + crlf + crlf + 
    'RegValue ::= TYPE-IDENTIFIER {' + crlf + 
    '     {UTF8Pairs'#9'IDENTIFIED BY id-regInfo-utf8Pairs}|' + crlf + 
    '     {CertRequest'#9'IDENTIFIED BY id-regInfo-certReq}}' + crlf + crlf + 
    'UTF8Pairs ::= UTF8STRING' + crlf + crlf + 
    'id-regInfo-utf8Pairs OBJECT IDENTIFIER ::= {  iso(1) org(3) dod(6) internet(1) security(5) mechanisms(5) id-pkix(7) id-pkip(5) id-regInfo(2) 1 }' + crlf + crlf + 
    'id-regInfo-certReq OBJECT IDENTIFIER ::= {  iso(1) org(3) dod(6) internet(1) security(5) mechanisms(5) id-pkix(7) id-pkip(5) id-regInfo(2) 2 }' + crlf + crlf + crlf + 
    'END' + crlf + crlf;

type
{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TCertRequest
       TCertTemplate
        TOptionalValidity}

  TOptionalValidity = class(TASNConstructedWrapper)
  private
    function GetNotBefore: TTime;
    procedure SetNotBefore(const Value: TTime);
    function GetNotAfter: TTime;
    procedure SetNotAfter(const Value: TTime);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property NotBefore: TTime read GetNotBefore write SetNotBefore;
    property NotAfter: TTime read GetNotAfter write SetNotAfter;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TCertRequest
       TCertTemplate}

  TCertTemplate = class(TASNConstructedWrapper)
  private
    function GetVersion: TIntegerWrapper;
    procedure SetVersion(const Value: TIntegerWrapper);
    function GetSerialNumber: TIntegerWrapper;
    procedure SetSerialNumber(const Value: TIntegerWrapper);
    function GetSigningAlg: TAlgorithmIdentifier;
    procedure SetSigningAlg(const Value: TAlgorithmIdentifier);
    function GetIssuer: TName;
    procedure SetIssuer(const Value: TName);
    function GetValidity: TOptionalValidity;
    procedure SetValidity(const Value: TOptionalValidity);
    function GetSubject: TName;
    procedure SetSubject(const Value: TName);
    function GetPublicKey: TSubjectPublicKeyInfo;
    procedure SetPublicKey(const Value: TSubjectPublicKeyInfo);
    function GetIssuerUID: TBitString;
    procedure SetIssuerUID(const Value: TBitString);
    function GetSubjectUID: TBitString;
    procedure SetSubjectUID(const Value: TBitString);
    function GetExtensions: TExtensions;
    procedure SetExtensions(const Value: TExtensions);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Version: TIntegerWrapper read GetVersion write SetVersion;
    property SerialNumber: TIntegerWrapper read GetSerialNumber write SetSerialNumber;
    property SigningAlg: TAlgorithmIdentifier read GetSigningAlg write SetSigningAlg;
    property Issuer: TName read GetIssuer write SetIssuer;
    property Validity: TOptionalValidity read GetValidity write SetValidity;
    property Subject: TName read GetSubject write SetSubject;
    property PublicKey: TSubjectPublicKeyInfo read GetPublicKey write SetPublicKey;
    property IssuerUID: TBitString read GetIssuerUID write SetIssuerUID;
    property SubjectUID: TBitString read GetSubjectUID write SetSubjectUID;
    property Extensions: TExtensions read GetExtensions write SetExtensions;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TCertRequest
       TControls
        TControlTypeAndValue
         TControlValue
          TPkipublicationInfo
           TSequenceOfSinglePubInfo
            TSinglePubInfo}

  TSinglePubInfo = class(TASNConstructedWrapper)
  private
    function GetPubMethod: TIntegerWrapper;
    procedure SetPubMethod(const Value: TIntegerWrapper);
    function GetPubLocation: TGeneralName;
    procedure SetPubLocation(const Value: TGeneralName);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property PubMethod: TIntegerWrapper read GetPubMethod write SetPubMethod;
    property PubLocation: TGeneralName read GetPubLocation write SetPubLocation;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TCertRequest
       TControls
        TControlTypeAndValue
         TControlValue
          TPkipublicationInfo
           TSequenceOfSinglePubInfo}

  TSequenceOfSinglePubInfo = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TSinglePubInfo;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TSinglePubInfo;
    property Items[Index: Integer]: TSinglePubInfo read GetItems; default;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TCertRequest
       TControls
        TControlTypeAndValue
         TControlValue
          TPkipublicationInfo}

  TPkipublicationInfo = class(TASNConstructedWrapper)
  private
    function GetAction: TIntegerWrapper;
    procedure SetAction(const Value: TIntegerWrapper);
    function GetPubInfos: TSequenceOfSinglePubInfo;
    procedure SetPubInfos(const Value: TSequenceOfSinglePubInfo);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Action: TIntegerWrapper read GetAction write SetAction;
    property PubInfos: TSequenceOfSinglePubInfo read GetPubInfos write SetPubInfos;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TCertRequest
       TControls
        TControlTypeAndValue
         TControlValue
          TPkiarchiveOptions
           TEncryptedKey
            TEncryptedValue}

  TEncryptedValue = class(TASNConstructedWrapper)
  private
    function GetIntendedAlg: TAlgorithmIdentifier;
    procedure SetIntendedAlg(const Value: TAlgorithmIdentifier);
    function GetSymmAlg: TAlgorithmIdentifier;
    procedure SetSymmAlg(const Value: TAlgorithmIdentifier);
    function GetEncSymmKey: TBitString;
    procedure SetEncSymmKey(const Value: TBitString);
    function GetKeyAlg: TAlgorithmIdentifier;
    procedure SetKeyAlg(const Value: TAlgorithmIdentifier);
    function GetValueHint: TOctetString;
    procedure SetValueHint(const Value: TOctetString);
    function GetEncValue: TBitString;
    procedure SetEncValue(const Value: TBitString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property IntendedAlg: TAlgorithmIdentifier read GetIntendedAlg write SetIntendedAlg;
    property SymmAlg: TAlgorithmIdentifier read GetSymmAlg write SetSymmAlg;
    property EncSymmKey: TBitString read GetEncSymmKey write SetEncSymmKey;
    property KeyAlg: TAlgorithmIdentifier read GetKeyAlg write SetKeyAlg;
    property ValueHint: TOctetString read GetValueHint write SetValueHint;
    property EncValue: TBitString read GetEncValue write SetEncValue;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TCertRequest
       TControls
        TControlTypeAndValue
         TControlValue
          TPkiarchiveOptions
           TEncryptedKey}

  TEncryptedKeyEnum = (
    ekeUndefined, ekeEncryptedValue, ekeEnvelopedData);

  TEncryptedKey = class(TASNChoiceWrapper)
  private
    function GetAsEncryptedValue: TEncryptedValue;
    procedure SetAsEncryptedValue(const Value: TEncryptedValue);
    function GetAsEnvelopedData: TEnvelopedData;
    procedure SetAsEnvelopedData(const Value: TEnvelopedData);
    function GetChoice: TEncryptedKeyEnum;
    procedure SetChoice(const Value: TEncryptedKeyEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsEncryptedValue: TEncryptedValue read GetAsEncryptedValue write SetAsEncryptedValue;
    property AsEnvelopedData: TEnvelopedData read GetAsEnvelopedData write SetAsEnvelopedData;
  published
    property Choice: TEncryptedKeyEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TCertRequest
       TControls
        TControlTypeAndValue
         TControlValue
          TPkiarchiveOptions}

  TPkiarchiveOptionsEnum = (
    poeUndefined, poeEncryptedPrivKey, poeKeyGenParameters,
    poeArchiveRemGenPrivKey);

  TPkiarchiveOptions = class(TASNChoiceWrapper)
  private
    function GetAsEncryptedPrivKey: TEncryptedKey;
    procedure SetAsEncryptedPrivKey(const Value: TEncryptedKey);
    function GetAsKeyGenParameters: TOctetString;
    procedure SetAsKeyGenParameters(const Value: TOctetString);
    function GetAsArchiveRemGenPrivKey: Boolean;
    procedure SetAsArchiveRemGenPrivKey(const Value: Boolean);
    function GetChoice: TPkiarchiveOptionsEnum;
    procedure SetChoice(const Value: TPkiarchiveOptionsEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsEncryptedPrivKey: TEncryptedKey read GetAsEncryptedPrivKey write SetAsEncryptedPrivKey;
    property AsKeyGenParameters: TOctetString read GetAsKeyGenParameters write SetAsKeyGenParameters;
    property AsArchiveRemGenPrivKey: Boolean read GetAsArchiveRemGenPrivKey write SetAsArchiveRemGenPrivKey;
  published
    property Choice: TPkiarchiveOptionsEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TCertRequest
       TControls
        TControlTypeAndValue
         TControlValue
          TCertId}

  TCertId = class(TASNConstructedWrapper)
  private
    function GetIssuer: TGeneralName;
    procedure SetIssuer(const Value: TGeneralName);
    function GetSerialNumber: TIntegerWrapper;
    procedure SetSerialNumber(const Value: TIntegerWrapper);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Issuer: TGeneralName read GetIssuer write SetIssuer;
    property SerialNumber: TIntegerWrapper read GetSerialNumber write SetSerialNumber;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TCertRequest
       TControls
        TControlTypeAndValue
         TControlValue}

  TControlValueEnum = (
    cveUndefined, cveIdRegCtrl, cveIdRegCtrlAuthenticator,
    cveIdRegCtrlPkiPublicationInfo, cveIdRegCtrlPkiArchiveOptions,
    cveIdRegCtrlProtocolEncrKey, cveIdRegCtrlOldCertID);

  TControlValue = class(TASNChoiceWrapper)
  private
    function GetAsRegCtrl: WideString;
    procedure SetAsRegCtrl(const Value: WideString);
    function GetAsRegCtrl_Authenticator: WideString;
    procedure SetAsRegCtrl_Authenticator(const Value: WideString);
    function GetAsRegCtrl_PkiPublicationInfo: TPkipublicationInfo;
    procedure SetAsRegCtrl_PkiPublicationInfo(const Value: TPkipublicationInfo);
    function GetAsRegCtrl_PkiArchiveOptions: TPkiarchiveOptions;
    procedure SetAsRegCtrl_PkiArchiveOptions(const Value: TPkiarchiveOptions);
    function GetAsRegCtrl_ProtocolEncrKey: TSubjectPublicKeyInfo;
    procedure SetAsRegCtrl_ProtocolEncrKey(const Value: TSubjectPublicKeyInfo);
    function GetAsRegCtrl_OldCertID: TCertId;
    procedure SetAsRegCtrl_OldCertID(const Value: TCertId);
    function GetChoice: TControlValueEnum;
    procedure SetChoice(const Value: TControlValueEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsRegCtrl: WideString read GetAsRegCtrl write SetAsRegCtrl;
    property AsRegCtrl_Authenticator: WideString read GetAsRegCtrl_Authenticator write SetAsRegCtrl_Authenticator;
    property AsRegCtrl_PkiPublicationInfo: TPkipublicationInfo read GetAsRegCtrl_PkiPublicationInfo write SetAsRegCtrl_PkiPublicationInfo;
    property AsRegCtrl_PkiArchiveOptions: TPkiarchiveOptions read GetAsRegCtrl_PkiArchiveOptions write SetAsRegCtrl_PkiArchiveOptions;
    property AsRegCtrl_ProtocolEncrKey: TSubjectPublicKeyInfo read GetAsRegCtrl_ProtocolEncrKey write SetAsRegCtrl_ProtocolEncrKey;
    property AsRegCtrl_OldCertID: TCertId read GetAsRegCtrl_OldCertID write SetAsRegCtrl_OldCertID;
  published
    property Choice: TControlValueEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TCertRequest
       TControls
        TControlTypeAndValue}

  TControlTypeAndValue = class(TASNConstructedWrapper)
  private
    function GetControlID: ObjectIdentifier;
    procedure SetControlID(const Value: ObjectIdentifier);
    function GetValue: TControlValue;
    procedure SetValue(const Value: TControlValue);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TControlValueEnum): Boolean;
    procedure SetType(Id: TControlValueEnum);
  published
    property ControlID: ObjectIdentifier read GetControlID write SetControlID;
    property Value: TControlValue read GetValue write SetValue;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TCertRequest
       TControls}

  TControls = class(TASNCustomOFFieldWrapper)
  private
    function GetUniqueItem(Id: TControlValueEnum): TControlTypeAndValue;
    function GetItems(Index: Integer): TControlTypeAndValue;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function AddUniqueItem(Id: TControlValueEnum): TControlTypeAndValue;
    function Add: TControlTypeAndValue;
    property Items[Index: Integer]: TControlTypeAndValue read GetItems;
    property UniqueItem[Id: TControlValueEnum]: TControlTypeAndValue read GetUniqueItem; default;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TCertRequest}

  TCertRequest = class(TASNConstructedWrapper)
  private
    function GetCertReqId: TIntegerWrapper;
    procedure SetCertReqId(const Value: TIntegerWrapper);
    function GetCertTemplate: TCertTemplate;
    procedure SetCertTemplate(const Value: TCertTemplate);
    function GetControls: TControls;
    procedure SetControls(const Value: TControls);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property CertReqId: TIntegerWrapper read GetCertReqId write SetCertReqId;
    property CertTemplate: TCertTemplate read GetCertTemplate write SetCertTemplate;
    property Controls: TControls read GetControls write SetControls;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TProofOfPossession
       TPoposigningKey
        TPoposigningKeyInput
         TAuthInfo
          TPkmacvalue
           TPbmidentifier
            TPbmtype
             TPbmparameter}

  TPbmparameter = class(TASNConstructedWrapper)
  private
    function GetSalt: TOctetString;
    procedure SetSalt(const Value: TOctetString);
    function GetOwf: TDigestAlgorithmIdentifier;
    procedure SetOwf(const Value: TDigestAlgorithmIdentifier);
    function GetIterationCount: TIntegerWrapper;
    procedure SetIterationCount(const Value: TIntegerWrapper);
    function GetMac: TMessageAuthenticationCodeAlgorithm;
    procedure SetMac(const Value: TMessageAuthenticationCodeAlgorithm);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property Salt: TOctetString read GetSalt write SetSalt;
    property Owf: TDigestAlgorithmIdentifier read GetOwf write SetOwf;
    property IterationCount: TIntegerWrapper read GetIterationCount write SetIterationCount;
    property Mac: TMessageAuthenticationCodeAlgorithm read GetMac write SetMac;
  end;

{ Declaration tree:
    TCertReqMessages
     TCertReqMsg
      TProofOfPossession
       TPoposigningKey
        TPoposigningKeyInput
         TAuthInfo
          TPkmacvalue
           TPbmidentifier
            TPbmtype}

  TPbmtypeEnum = (
    peUndefined, pePasswordBasedMac);

  TPbmtype = class(TASNChoiceWrapper)
  private
    function GetAsPasswordBasedMac: TPbmparameter;
    procedure SetAsPasswordBasedMac(const Value: TPbmparameter);
    function GetChoice: TPbmtypeEnum;
    procedure SetChoice(const Value: TPbmtypeEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsPasswordBasedMac: TPbmparameter read GetAsPasswordBasedMac write SetAsPasswordBasedMac;
  published
    property Choice: TPbmtypeEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TProofOfPossession
       TPoposigningKey
        TPoposigningKeyInput
         TAuthInfo
          TPkmacvalue
           TPbmidentifier}

  TPbmidentifier = class(TASNConstructedWrapper)
  private
    function GetAlgorithm: ObjectIdentifier;
    procedure SetAlgorithm(const Value: ObjectIdentifier);
    function GetParameters: TPbmtype;
    procedure SetParameters(const Value: TPbmtype);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TPbmtypeEnum): Boolean;
    procedure SetType(Id: TPbmtypeEnum);
  published
    property Algorithm: ObjectIdentifier read GetAlgorithm write SetAlgorithm;
    property Parameters: TPbmtype read GetParameters write SetParameters;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TProofOfPossession
       TPoposigningKey
        TPoposigningKeyInput
         TAuthInfo
          TPkmacvalue}

  TPkmacvalue = class(TASNConstructedWrapper)
  private
    function GetAlgId: TPbmidentifier;
    procedure SetAlgId(const Value: TPbmidentifier);
    function GetValue: TBitString;
    procedure SetValue(const Value: TBitString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property AlgId: TPbmidentifier read GetAlgId write SetAlgId;
    property Value: TBitString read GetValue write SetValue;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TProofOfPossession
       TPoposigningKey
        TPoposigningKeyInput
         TAuthInfo}

  TAuthInfoEnum = (
    aieUndefined, aiePublicKeyMAC, aieSender);

  TAuthInfo = class(TASNChoiceWrapper)
  private
    function GetAsPublicKeyMAC: TPkmacvalue;
    procedure SetAsPublicKeyMAC(const Value: TPkmacvalue);
    function GetAsSender: TGeneralName;
    procedure SetAsSender(const Value: TGeneralName);
    function GetChoice: TAuthInfoEnum;
    procedure SetChoice(const Value: TAuthInfoEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsPublicKeyMAC: TPkmacvalue read GetAsPublicKeyMAC write SetAsPublicKeyMAC;
    property AsSender: TGeneralName read GetAsSender write SetAsSender;
  published
    property Choice: TAuthInfoEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TProofOfPossession
       TPoposigningKey
        TPoposigningKeyInput}

  TPoposigningKeyInput = class(TASNConstructedWrapper)
  private
    function GetAuthInfo: TAuthInfo;
    procedure SetAuthInfo(const Value: TAuthInfo);
    function GetPublicKey: TSubjectPublicKeyInfo;
    procedure SetPublicKey(const Value: TSubjectPublicKeyInfo);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property AuthInfo: TAuthInfo read GetAuthInfo write SetAuthInfo;
    property PublicKey: TSubjectPublicKeyInfo read GetPublicKey write SetPublicKey;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TProofOfPossession
       TPoposigningKey}

  TPoposigningKey = class(TASNConstructedWrapper)
  private
    function GetPoposkInput: TPoposigningKeyInput;
    procedure SetPoposkInput(const Value: TPoposigningKeyInput);
    function GetAlgorithmIdentifier: TAlgorithmIdentifier;
    procedure SetAlgorithmIdentifier(const Value: TAlgorithmIdentifier);
    function GetSignature: TBitString;
    procedure SetSignature(const Value: TBitString);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
  published
    property PoposkInput: TPoposigningKeyInput read GetPoposkInput write SetPoposkInput;
    property AlgorithmIdentifier: TAlgorithmIdentifier read GetAlgorithmIdentifier write SetAlgorithmIdentifier;
    property Signature: TBitString read GetSignature write SetSignature;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TProofOfPossession
       TPopoprivKey}

  TPopoprivKeyEnum = (
    pkeUndefined, pkeThisMessage, pkeSubsequentMessage, pkeDhMAC);

  TPopoprivKey = class(TASNChoiceWrapper)
  private
    function GetAsThisMessage: TBitString;
    procedure SetAsThisMessage(const Value: TBitString);
    function GetAsSubsequentMessage: TIntegerWrapper;
    procedure SetAsSubsequentMessage(const Value: TIntegerWrapper);
    function GetAsDhMAC: TBitString;
    procedure SetAsDhMAC(const Value: TBitString);
    function GetChoice: TPopoprivKeyEnum;
    procedure SetChoice(const Value: TPopoprivKeyEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsThisMessage: TBitString read GetAsThisMessage write SetAsThisMessage;
    property AsSubsequentMessage: TIntegerWrapper read GetAsSubsequentMessage write SetAsSubsequentMessage;
    property AsDhMAC: TBitString read GetAsDhMAC write SetAsDhMAC;
  published
    property Choice: TPopoprivKeyEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TProofOfPossession}

  TProofOfPossessionEnum = (
    popeUndefined, popeRaVerified, popeSignature, popeKeyEncipherment,
    popeKeyAgreement);

  TProofOfPossession = class(TASNChoiceWrapper)
  private
    function GetAsSignature: TPoposigningKey;
    procedure SetAsSignature(const Value: TPoposigningKey);
    function GetAsKeyEncipherment: TPopoprivKey;
    procedure SetAsKeyEncipherment(const Value: TPopoprivKey);
    function GetAsKeyAgreement: TPopoprivKey;
    procedure SetAsKeyAgreement(const Value: TPopoprivKey);
    function GetChoice: TProofOfPossessionEnum;
    procedure SetChoice(const Value: TProofOfPossessionEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsSignature: TPoposigningKey read GetAsSignature write SetAsSignature;
    property AsKeyEncipherment: TPopoprivKey read GetAsKeyEncipherment write SetAsKeyEncipherment;
    property AsKeyAgreement: TPopoprivKey read GetAsKeyAgreement write SetAsKeyAgreement;
  published
    property Choice: TProofOfPossessionEnum read GetChoice write SetChoice;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TSequenceOfRegInfo
       TRegInfo
        TRegValue}

  TRegValueEnum = (
    rveUndefined, rveIdRegInfoUtf8Pairs, rveIdRegInfoCertReq);

  TRegValue = class(TASNChoiceWrapper)
  private
    function GetAsRegInfo_Utf8Pairs: WideString;
    procedure SetAsRegInfo_Utf8Pairs(const Value: WideString);
    function GetAsRegInfo_CertReq: TCertRequest;
    procedure SetAsRegInfo_CertReq(const Value: TCertRequest);
    function GetChoice: TRegValueEnum;
    procedure SetChoice(const Value: TRegValueEnum);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    property AsRegInfo_Utf8Pairs: WideString read GetAsRegInfo_Utf8Pairs write SetAsRegInfo_Utf8Pairs;
    property AsRegInfo_CertReq: TCertRequest read GetAsRegInfo_CertReq write SetAsRegInfo_CertReq;
  published
    property Choice: TRegValueEnum read GetChoice write SetChoice;
  end;

{ Declaration tree:
    TCertReqMessages
     TCertReqMsg
      TSequenceOfRegInfo
       TRegInfo}

  TRegInfo = class(TASNConstructedWrapper)
  private
    function GetRegID: ObjectIdentifier;
    procedure SetRegID(const Value: ObjectIdentifier);
    function GetRegValue: TRegValue;
    procedure SetRegValue(const Value: TRegValue);
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function IsType(Id: TRegValueEnum): Boolean;
    procedure SetType(Id: TRegValueEnum);
  published
    property RegID: ObjectIdentifier read GetRegID write SetRegID;
    property RegValue: TRegValue read GetRegValue write SetRegValue;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg
      TSequenceOfRegInfo}

  TSequenceOfRegInfo = class(TASNCustomOFFieldWrapper)
  private
    function GetUniqueItem(Id: TRegValueEnum): TRegInfo;
    function GetItems(Index: Integer): TRegInfo;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function AddUniqueItem(Id: TRegValueEnum): TRegInfo;
    function Add: TRegInfo;
    property Items[Index: Integer]: TRegInfo read GetItems;
    property UniqueItem[Id: TRegValueEnum]: TRegInfo read GetUniqueItem; default;
  end;

{ Declaration tree: 
    TCertReqMessages
     TCertReqMsg}

  TCertReqMsg = class(TASNConstructedWrapper,
                      ISigned)
  private
    function GetCertReq: TCertRequest;
    procedure SetCertReq(const Value: TCertRequest);
    function GetPop: TProofOfPossession;
    procedure SetPop(const Value: TProofOfPossession);
    function GetRegInfo: TSequenceOfRegInfo;
    procedure SetRegInfo(const Value: TSequenceOfRegInfo);
  protected                           
    function BeforeSign(var CACert: TCertificate;
                        SignAlg: ObjectIdentifier;
                        out Params: TASNCustomWrapper): Boolean;
    function InternalCheckSignature(CACert: IReadOnlyPublicKeyInfo): Boolean; 
    function InterpretRSASignatureAlgorithm(var HA: THashAlgorithm;
                                            var MHA: THashAlgorithm;
                                            var EM: TSignEncoding): Boolean;
    function IsAllegedIssuer(CACert: TCertificate): Boolean;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function CheckSignature(CACert: TCertificate): Boolean; overload;
    function CheckSignature(PublicKey: IReadOnlyPublicKeyInfo): Boolean; overload;
    function RSASign(const RSAKey: TIFPrivateKey;
                     HA: THashAlgorithm;
                     CACert: TCertificate): Boolean; overload;
    function RSASSA_PSSSign(const RSAKey: TIFPrivateKey;
                            CACert: TCertificate): Boolean; overload;
    function DSASign(const DSAKey: TDLPrivateKey;
                     CACert: TCertificate): Boolean; overload;
    function ECDSASign(const ECDSAKey: TECPrivateKey;
                       CACert: TCertificate): Boolean; overload;
    function RSASign(const RSAKey: TIFPrivateKey;
                     HA: THashAlgorithm;
                     CACert: TASN1Struct): Boolean; overload;
    function RSASSA_PSSSign(const RSAKey: TIFPrivateKey;
                            CACert: TASN1Struct): Boolean; overload;
    function DSASign(const DSAKey: TDLPrivateKey;
                     CACert: TASN1Struct): Boolean; overload;
    function ECDSASign(const ECDSAKey: TECPrivateKey;
                       CACert: TASN1Struct): Boolean; overload;
    function RSASign(const RSAKey: TIFPrivateKey;
                     HA: THashAlgorithm): Boolean; overload;
    function RSASSA_PSSSign(const RSAKey: TIFPrivateKey): Boolean; overload;
    function DSASign(const DSAKey: TDLPrivateKey): Boolean; overload;
    function ECDSASign(const ECDSAKey: TECPrivateKey): Boolean; overload;
    procedure SetPBMac(const Password: string; Iter: Integer);
    function VerifyPBMac(const Password: string): Boolean;
  published
    property CertReq: TCertRequest read GetCertReq write SetCertReq;
    property Pop: TProofOfPossession read GetPop write SetPop;
    property RegInfo: TSequenceOfRegInfo read GetRegInfo write SetRegInfo;
  end;

{ Declaration tree:
    TCertReqMessages}

  TCertReqMessages = class(TASNCustomOFFieldWrapper)
  private
    function GetItems(Index: Integer): TCertReqMsg;
  public
    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
    destructor Destroy; override;
    function Add: TCertReqMsg;
    property Items[Index: Integer]: TCertReqMsg read GetItems; default;
  end;

{$IFNDEF INIT_SECTIONS}
procedure Initialize;
{$ENDIF}
{$IFNDEF FINI_SECTIONS}
procedure Finalize;
{$ENDIF}

implementation

uses
  ReadStrm, MpYarrow, Pkix;

var
  GlobalObject: TASN1Struct;

procedure InitGlobalObject;
var
  SS: TStringStream;
begin             
  Assert(PKIX_Cert.ASNModule <> '');
  Assert(PKIX_CRL.ASNModule <> '');
  Assert(CMS.ASNModule <> '');
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

{ TOptionalValidity }

constructor TOptionalValidity.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template;
    F := F.FindField('/certReq/certTemplate/validity')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TTime,nil,FData.Items[0]^.Items[0]^,nil^);
  FieldFactory(TTime,nil,FData.Items[1]^.Items[0]^,nil^);
end;

destructor TOptionalValidity.Destroy;
begin
  inherited Destroy;
end;

function TOptionalValidity.GetNotBefore: TTime;
begin
  Result := FItems[0];
end;

procedure TOptionalValidity.SetNotBefore(const Value: TTime);
begin
  TTime(FItems[0]).Assign(Value);
end;

function TOptionalValidity.GetNotAfter: TTime;
begin
  Result := FItems[1];
end;

procedure TOptionalValidity.SetNotAfter(const Value: TTime);
begin
  TTime(FItems[1]).Assign(Value);
end;

{ TCertTemplate }

constructor TCertTemplate.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template;
    F := F.FindField('/certReq/certTemplate')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[1]^,nil^);
  FieldFactory(TAlgorithmIdentifier,nil,FData.Items[2]^,nil^);
  FieldFactory(TName,nil,FData.Items[3]^.Items[0]^,nil^);
  FieldFactory(TOptionalValidity,nil,FData.Items[4]^,nil^);
  FieldFactory(TName,nil,FData.Items[5]^.Items[0]^,nil^);
  FieldFactory(TSubjectPublicKeyInfo,nil,FData.Items[6]^,nil^);
  FieldFactory(TBitString,nil,FData.Items[7]^,nil^);
  FieldFactory(TBitString,nil,FData.Items[8]^,nil^);
  FieldFactory(TExtensions,nil,FData.Items[9]^,nil^);
end;

destructor TCertTemplate.Destroy;
begin
  inherited Destroy;
end;

function TCertTemplate.GetVersion: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TCertTemplate.SetVersion(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TCertTemplate.GetSerialNumber: TIntegerWrapper;
begin
  Result := FItems[1];
end;

procedure TCertTemplate.SetSerialNumber(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[1]).Assign(Value);
end;

function TCertTemplate.GetSigningAlg: TAlgorithmIdentifier;
begin
  Result := FItems[2];
end;

procedure TCertTemplate.SetSigningAlg(const Value: TAlgorithmIdentifier);
begin
  TAlgorithmIdentifier(FItems[2]).Assign(Value);
end;

function TCertTemplate.GetIssuer: TName;
begin
  Result := FItems[3];
end;

procedure TCertTemplate.SetIssuer(const Value: TName);
begin
  TName(FItems[3]).Assign(Value);
end;

function TCertTemplate.GetValidity: TOptionalValidity;
begin
  Result := FItems[4];
end;

procedure TCertTemplate.SetValidity(const Value: TOptionalValidity);
begin
  TOptionalValidity(FItems[4]).Assign(Value);
end;

function TCertTemplate.GetSubject: TName;
begin
  Result := FItems[5];
end;

procedure TCertTemplate.SetSubject(const Value: TName);
begin
  TName(FItems[5]).Assign(Value);
end;

function TCertTemplate.GetPublicKey: TSubjectPublicKeyInfo;
begin
  Result := FItems[6];
end;

procedure TCertTemplate.SetPublicKey(const Value: TSubjectPublicKeyInfo);
begin
  TSubjectPublicKeyInfo(FItems[6]).Assign(Value);
end;

function TCertTemplate.GetIssuerUID: TBitString;
begin
  Result := FItems[7];
end;

procedure TCertTemplate.SetIssuerUID(const Value: TBitString);
begin
  TBitString(FItems[7]).Assign(Value);
end;

function TCertTemplate.GetSubjectUID: TBitString;
begin
  Result := FItems[8];
end;

procedure TCertTemplate.SetSubjectUID(const Value: TBitString);
begin
  TBitString(FItems[8]).Assign(Value);
end;

function TCertTemplate.GetExtensions: TExtensions;
begin
  Result := FItems[9];
end;

procedure TCertTemplate.SetExtensions(const Value: TExtensions);
begin
  TExtensions(FItems[9]).Assign(Value);
end;

{ TSinglePubInfo }

constructor TSinglePubInfo.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template.FindField('/certReq/controls')^.Template.FindField('value')^.Choices[2]^.FindField('pubInfos')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TGeneralName,nil,FData.Items[1]^,nil^);
end;

destructor TSinglePubInfo.Destroy;
begin
  inherited Destroy;
end;

function TSinglePubInfo.GetPubMethod: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TSinglePubInfo.SetPubMethod(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TSinglePubInfo.GetPubLocation: TGeneralName;
begin
  Result := FItems[1];
end;

procedure TSinglePubInfo.SetPubLocation(const Value: TGeneralName);
begin
  TGeneralName(FItems[1]).Assign(Value);
end;

{ TSequenceOfSinglePubInfo }

constructor TSequenceOfSinglePubInfo.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  FFieldType := TSinglePubInfo;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template.FindField('/certReq/controls')^.Template.FindField('value')^.Choices[2]^;
    F := F.FindField('pubInfos')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TSequenceOfSinglePubInfo.Destroy;
begin
  inherited Destroy;
end;

function TSequenceOfSinglePubInfo.Add: TSinglePubInfo;
begin
  Result := TSinglePubInfo(InternalAdd);
end;

function TSequenceOfSinglePubInfo.GetItems(Index: Integer): TSinglePubInfo;
begin
  Result := TSinglePubInfo(InternalGetItems(Index));
end;

{ TPkipublicationInfo }

constructor TPkipublicationInfo.Create;
var
  F: TASN1Struct;
begin
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template.FindField('/certReq/controls')^.Template.FindField('value')^.Choices[2]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TSequenceOfSinglePubInfo,nil,FData.Items[1]^,nil^);
end;

destructor TPkipublicationInfo.Destroy;
begin
  inherited Destroy;
end;

function TPkipublicationInfo.GetAction: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TPkipublicationInfo.SetAction(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TPkipublicationInfo.GetPubInfos: TSequenceOfSinglePubInfo;
begin
  Result := FItems[1];
end;

procedure TPkipublicationInfo.SetPubInfos(const Value: TSequenceOfSinglePubInfo);
begin
  TSequenceOfSinglePubInfo(FItems[1]).Assign(Value);
end;

{ TEncryptedValue }

constructor TEncryptedValue.Create;
var
  F: TASN1Struct;
begin                 
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template.FindField('/certReq/controls')^.Template.FindField('value')^.Choices[3]^.Choices[0]^.Choices[0]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TAlgorithmIdentifier,nil,FData.Items[0]^,nil^);
  FieldFactory(TAlgorithmIdentifier,nil,FData.Items[1]^,nil^);
  FieldFactory(TBitString,nil,FData.Items[2]^,nil^);
  FieldFactory(TAlgorithmIdentifier,nil,FData.Items[3]^,nil^);
  FieldFactory(TOctetString,nil,FData.Items[4]^,nil^);
  FieldFactory(TBitString,nil,FData.Items[5]^,nil^);
end;

destructor TEncryptedValue.Destroy;
begin
  inherited Destroy;
end;

function TEncryptedValue.GetIntendedAlg: TAlgorithmIdentifier;
begin
  Result := FItems[0];
end;

procedure TEncryptedValue.SetIntendedAlg(const Value: TAlgorithmIdentifier);
begin
  TAlgorithmIdentifier(FItems[0]).Assign(Value);
end;

function TEncryptedValue.GetSymmAlg: TAlgorithmIdentifier;
begin
  Result := FItems[1];
end;

procedure TEncryptedValue.SetSymmAlg(const Value: TAlgorithmIdentifier);
begin
  TAlgorithmIdentifier(FItems[1]).Assign(Value);
end;

function TEncryptedValue.GetEncSymmKey: TBitString;
begin
  Result := FItems[2];
end;

procedure TEncryptedValue.SetEncSymmKey(const Value: TBitString);
begin
  TBitString(FItems[2]).Assign(Value);
end;

function TEncryptedValue.GetKeyAlg: TAlgorithmIdentifier;
begin
  Result := FItems[3];
end;

procedure TEncryptedValue.SetKeyAlg(const Value: TAlgorithmIdentifier);
begin
  TAlgorithmIdentifier(FItems[3]).Assign(Value);
end;

function TEncryptedValue.GetValueHint: TOctetString;
begin
  Result := FItems[4];
end;

procedure TEncryptedValue.SetValueHint(const Value: TOctetString);
begin
  TOctetString(FItems[4]).Assign(Value);
end;

function TEncryptedValue.GetEncValue: TBitString;
begin
  Result := FItems[5];
end;

procedure TEncryptedValue.SetEncValue(const Value: TBitString);
begin
  TBitString(FItems[5]).Assign(Value);
end;

{ TEncryptedKey }

constructor TEncryptedKey.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template.FindField('/certReq/controls')^.Template.FindField('value')^.Choices[3]^.Choices[0]^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TEncryptedValue);
  FChoiceList.Add(TEnvelopedData);
end;

destructor TEncryptedKey.Destroy;
begin
  inherited Destroy;
end;

function TEncryptedKey.GetAsEncryptedValue: TEncryptedValue;
begin
  Result := (FSelected as TEncryptedValue);
end;

procedure TEncryptedKey.SetAsEncryptedValue(const Value: TEncryptedValue);
begin
  InternalSelectChoice(0);
  (FSelected as TEncryptedValue).Assign(Value);
end;

function TEncryptedKey.GetAsEnvelopedData: TEnvelopedData;
begin
  Result := (FSelected as TEnvelopedData);
end;

procedure TEncryptedKey.SetAsEnvelopedData(const Value: TEnvelopedData);
begin
  InternalSelectChoice(1);
  (FSelected as TEnvelopedData).Assign(Value);
end;

function TEncryptedKey.GetChoice: TEncryptedKeyEnum;
begin
  Result := TEncryptedKeyEnum(InternalGetChoice);
end;

procedure TEncryptedKey.SetChoice(const Value: TEncryptedKeyEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TPkiarchiveOptions }

constructor TPkiarchiveOptions.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template.FindField('/certReq/controls')^.Template.FindField('value')^.Choices[3]^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TEncryptedKey);
  FChoiceList.Add(TOctetString);
  FChoiceList.Add(TBooleanWrapper);
end;

destructor TPkiarchiveOptions.Destroy;
begin
  inherited Destroy;
end;

function TPkiarchiveOptions.GetAsEncryptedPrivKey: TEncryptedKey;
begin
  Result := (FSelected as TEncryptedKey);
end;

procedure TPkiarchiveOptions.SetAsEncryptedPrivKey(const Value: TEncryptedKey);
begin
  InternalSelectChoice(0);
  (FSelected as TEncryptedKey).Assign(Value);
end;

function TPkiarchiveOptions.GetAsKeyGenParameters: TOctetString;
begin
  Result := (FSelected as TOctetString);
end;

procedure TPkiarchiveOptions.SetAsKeyGenParameters(const Value: TOctetString);
begin
  InternalSelectChoice(1);
  (FSelected as TOctetString).Assign(Value);
end;

function TPkiarchiveOptions.GetAsArchiveRemGenPrivKey: Boolean;
begin
  Result := (FSelected as TBooleanWrapper).Value;
end;

procedure TPkiarchiveOptions.SetAsArchiveRemGenPrivKey(const Value: Boolean);
begin
  InternalSelectChoice(2);
  (FSelected as TBooleanWrapper).Value := Value;
end;

function TPkiarchiveOptions.GetChoice: TPkiarchiveOptionsEnum;
begin
  Result := TPkiarchiveOptionsEnum(InternalGetChoice);
end;

procedure TPkiarchiveOptions.SetChoice(const Value: TPkiarchiveOptionsEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TCertId }

constructor TCertId.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template.FindField('/certReq/controls')^.Template.FindField('value')^.Choices[5]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TGeneralName,nil,FData.Items[0]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[1]^,nil^);
end;

destructor TCertId.Destroy;
begin
  inherited Destroy;
end;

function TCertId.GetIssuer: TGeneralName;
begin
  Result := FItems[0];
end;

procedure TCertId.SetIssuer(const Value: TGeneralName);
begin
  TGeneralName(FItems[0]).Assign(Value);
end;

function TCertId.GetSerialNumber: TIntegerWrapper;
begin
  Result := FItems[1];
end;

procedure TCertId.SetSerialNumber(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[1]).Assign(Value);
end;

{ TControlValue }

constructor TControlValue.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template.FindField('/certReq/controls')^.Template;
    F := F.FindField('value')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TWideStringWrapper);
  FChoiceList.Add(TWideStringWrapper);
  FChoiceList.Add(TPkipublicationInfo);
  FChoiceList.Add(TPkiarchiveOptions);
  FChoiceList.Add(TSubjectPublicKeyInfo);
  FChoiceList.Add(TCertId);
end;

destructor TControlValue.Destroy;
begin
  inherited Destroy;
end;

function TControlValue.GetAsRegCtrl: WideString;
begin
  Result := (FSelected as TWideStringWrapper).Value;
end;

procedure TControlValue.SetAsRegCtrl(const Value: WideString);
begin
  InternalSelectChoice(0);
  (FSelected as TWideStringWrapper).Value := Value;
end;

function TControlValue.GetAsRegCtrl_Authenticator: WideString;
begin
  Result := (FSelected as TWideStringWrapper).Value;
end;

procedure TControlValue.SetAsRegCtrl_Authenticator(const Value: WideString);
begin
  InternalSelectChoice(1);
  (FSelected as TWideStringWrapper).Value := Value;
end;

function TControlValue.GetAsRegCtrl_PkiPublicationInfo: TPkipublicationInfo;
begin
  Result := (FSelected as TPkipublicationInfo);
end;

procedure TControlValue.SetAsRegCtrl_PkiPublicationInfo(const Value: TPkipublicationInfo);
begin
  InternalSelectChoice(2);
  (FSelected as TPkipublicationInfo).Assign(Value);
end;

function TControlValue.GetAsRegCtrl_PkiArchiveOptions: TPkiarchiveOptions;
begin
  Result := (FSelected as TPkiarchiveOptions);
end;

procedure TControlValue.SetAsRegCtrl_PkiArchiveOptions(const Value: TPkiarchiveOptions);
begin
  InternalSelectChoice(3);
  (FSelected as TPkiarchiveOptions).Assign(Value);
end;

function TControlValue.GetAsRegCtrl_ProtocolEncrKey: TSubjectPublicKeyInfo;
begin
  Result := (FSelected as TSubjectPublicKeyInfo);
end;

procedure TControlValue.SetAsRegCtrl_ProtocolEncrKey(const Value: TSubjectPublicKeyInfo);
begin
  InternalSelectChoice(4);
  (FSelected as TSubjectPublicKeyInfo).Assign(Value);
end;

function TControlValue.GetAsRegCtrl_OldCertID: TCertId;
begin
  Result := (FSelected as TCertId);
end;

procedure TControlValue.SetAsRegCtrl_OldCertID(const Value: TCertId);
begin
  InternalSelectChoice(5);
  (FSelected as TCertId).Assign(Value);
end;

function TControlValue.GetChoice: TControlValueEnum;
begin
  Result := TControlValueEnum(InternalGetChoice);
end;

procedure TControlValue.SetChoice(const Value: TControlValueEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TControlTypeAndValue }

constructor TControlTypeAndValue.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template.FindField('/certReq/controls')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TControlValue,nil,FData.Items[1]^,nil^);
end;

destructor TControlTypeAndValue.Destroy;
begin
  inherited Destroy;
end;

function TControlTypeAndValue.GetControlID: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TControlTypeAndValue.SetControlID(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TControlTypeAndValue.GetValue: TControlValue;
begin
  Result := FItems[1];
end;

procedure TControlTypeAndValue.SetValue(const Value: TControlValue);
begin
  TControlValue(FItems[1]).Assign(Value);
end;

function TControlTypeAndValue.IsType(Id: TControlValueEnum): Boolean;
begin
  Result := TControlValue(FItems[1]).Choice = Id;
end;

procedure TControlTypeAndValue.SetType(Id: TControlValueEnum);
begin
  TControlValue(FItems[1]).Choice := Id;
end;

{ TControls }

constructor TControls.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  FFieldType := TControlTypeAndValue;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template;
    F := F.FindField('/certReq/controls')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TControls.Destroy;
begin
  inherited Destroy;
end;

function TControls.GetUniqueItem(Id: TControlValueEnum): TControlTypeAndValue;
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

function TControls.AddUniqueItem(Id: TControlValueEnum): TControlTypeAndValue;
begin
  Result := GetUniqueItem(Id);
  if Result = nil then begin
    Result := Add;
    Result.SetType(Id);
  end;
end;

function TControls.Add: TControlTypeAndValue;
begin
  Result := TControlTypeAndValue(InternalAdd);
end;

function TControls.GetItems(Index: Integer): TControlTypeAndValue;
begin
  Result := TControlTypeAndValue(InternalGetItems(Index));
end;

{ TCertRequest }

constructor TCertRequest.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template;
    F := F.FindField('certReq')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TIntegerWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TCertTemplate,nil,FData.Items[1]^,nil^);
  FieldFactory(TControls,nil,FData.Items[2]^,nil^);
end;

destructor TCertRequest.Destroy;
begin
  inherited Destroy;
end;

function TCertRequest.GetCertReqId: TIntegerWrapper;
begin
  Result := FItems[0];
end;

procedure TCertRequest.SetCertReqId(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[0]).Assign(Value);
end;

function TCertRequest.GetCertTemplate: TCertTemplate;
begin
  Result := FItems[1];
end;

procedure TCertRequest.SetCertTemplate(const Value: TCertTemplate);
begin
  TCertTemplate(FItems[1]).Assign(Value);
end;

function TCertRequest.GetControls: TControls;
begin
  Result := FItems[2];
end;

procedure TCertRequest.SetControls(const Value: TControls);
begin
  TControls(FItems[2]).Assign(Value);
end;

{ TPbmparameter }

constructor TPbmparameter.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template.FindField('pop')^.Choices[2]^.FindField('/poposkInput/authInfo')^.Choices[0]^.FindField('/algId/parameters')^.Choices[0]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TOctetString,nil,FData.Items[0]^,nil^);
  FieldFactory(TDigestAlgorithmIdentifier,nil,FData.Items[1]^,nil^);
  FieldFactory(TIntegerWrapper,nil,FData.Items[2]^,nil^);
  FieldFactory(TMessageAuthenticationCodeAlgorithm,nil,FData.Items[3]^,nil^);
end;

destructor TPbmparameter.Destroy;
begin
  inherited Destroy;
end;

function TPbmparameter.GetSalt: TOctetString;
begin
  Result := FItems[0];
end;

procedure TPbmparameter.SetSalt(const Value: TOctetString);
begin
  TOctetString(FItems[0]).Assign(Value);
end;

function TPbmparameter.GetOwf: TDigestAlgorithmIdentifier;
begin
  Result := FItems[1];
end;

procedure TPbmparameter.SetOwf(const Value: TDigestAlgorithmIdentifier);
begin
  TDigestAlgorithmIdentifier(FItems[1]).Assign(Value);
end;

function TPbmparameter.GetIterationCount: TIntegerWrapper;
begin
  Result := FItems[2];
end;

procedure TPbmparameter.SetIterationCount(const Value: TIntegerWrapper);
begin
  TIntegerWrapper(FItems[2]).Assign(Value);
end;

function TPbmparameter.GetMac: TMessageAuthenticationCodeAlgorithm;
begin
  Result := FItems[3];
end;

procedure TPbmparameter.SetMac(const Value: TMessageAuthenticationCodeAlgorithm);
begin
  TMessageAuthenticationCodeAlgorithm(FItems[3]).Assign(Value);
end;

{ TPbmtype }

constructor TPbmtype.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template.FindField('pop')^.Choices[2]^.FindField('/poposkInput/authInfo')^.Choices[0]^;
    F := F.FindField('/algId/parameters')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TPbmparameter);
end;

destructor TPbmtype.Destroy;
begin
  inherited Destroy;
end;

function TPbmtype.GetAsPasswordBasedMac: TPbmparameter;
begin
  Result := (FSelected as TPbmparameter);
end;

procedure TPbmtype.SetAsPasswordBasedMac(const Value: TPbmparameter);
begin
  InternalSelectChoice(0);
  (FSelected as TPbmparameter).Assign(Value);
end;

function TPbmtype.GetChoice: TPbmtypeEnum;
begin
  Result := TPbmtypeEnum(InternalGetChoice);
end;

procedure TPbmtype.SetChoice(const Value: TPbmtypeEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TPbmidentifier }

constructor TPbmidentifier.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template.FindField('pop')^.Choices[2]^.FindField('/poposkInput/authInfo')^.Choices[0]^;
    F := F.FindField('algId')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TPbmtype,nil,FData.Items[1]^,nil^);
end;

destructor TPbmidentifier.Destroy;
begin
  inherited Destroy;
end;

function TPbmidentifier.GetAlgorithm: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TPbmidentifier.SetAlgorithm(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TPbmidentifier.GetParameters: TPbmtype;
begin
  Result := FItems[1];
end;

procedure TPbmidentifier.SetParameters(const Value: TPbmtype);
begin
  TPbmtype(FItems[1]).Assign(Value);
end;

function TPbmidentifier.IsType(Id: TPbmtypeEnum): Boolean;
begin
  Result := TPbmtype(FItems[1]).Choice = Id;
end;

procedure TPbmidentifier.SetType(Id: TPbmtypeEnum);
begin
  TPbmtype(FItems[1]).Choice := Id;
end;

{ TPkmacvalue }

constructor TPkmacvalue.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template.FindField('pop')^.Choices[2]^.FindField('/poposkInput/authInfo')^.Choices[0]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TPbmidentifier,nil,FData.Items[0]^,nil^);
  FieldFactory(TBitString,nil,FData.Items[1]^,nil^);
end;

destructor TPkmacvalue.Destroy;
begin
  inherited Destroy;
end;

function TPkmacvalue.GetAlgId: TPbmidentifier;
begin
  Result := FItems[0];
end;

procedure TPkmacvalue.SetAlgId(const Value: TPbmidentifier);
begin
  TPbmidentifier(FItems[0]).Assign(Value);
end;

function TPkmacvalue.GetValue: TBitString;
begin
  Result := FItems[1];
end;

procedure TPkmacvalue.SetValue(const Value: TBitString);
begin
  TBitString(FItems[1]).Assign(Value);
end;

{ TAuthInfo }

constructor TAuthInfo.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template.FindField('pop')^.Choices[2]^;
    F := F.FindField('/poposkInput/authInfo')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TPkmacvalue);
  FChoiceList.Add(TGeneralName);
end;

destructor TAuthInfo.Destroy;
begin
  inherited Destroy;
end;

function TAuthInfo.GetAsPublicKeyMAC: TPkmacvalue;
begin
  Result := (FSelected as TPkmacvalue);
end;

procedure TAuthInfo.SetAsPublicKeyMAC(const Value: TPkmacvalue);
begin
  InternalSelectChoice(0);
  (FSelected as TPkmacvalue).Assign(Value);
end;

function TAuthInfo.GetAsSender: TGeneralName;
begin
  Result := (FSelected as TGeneralName);
end;

procedure TAuthInfo.SetAsSender(const Value: TGeneralName);
begin
  InternalSelectChoice(1);
  (FSelected as TGeneralName).Assign(Value);
end;

function TAuthInfo.GetChoice: TAuthInfoEnum;
begin
  Result := TAuthInfoEnum(InternalGetChoice);
end;

procedure TAuthInfo.SetChoice(const Value: TAuthInfoEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TPoposigningKeyInput }

constructor TPoposigningKeyInput.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template.FindField('pop')^.Choices[2]^;
    F := F.FindField('poposkInput')^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TAuthInfo,nil,FData.Items[0]^,nil^);
  FieldFactory(TSubjectPublicKeyInfo,nil,FData.Items[1]^,nil^);
end;

destructor TPoposigningKeyInput.Destroy;
begin
  inherited Destroy;
end;

function TPoposigningKeyInput.GetAuthInfo: TAuthInfo;
begin
  Result := FItems[0];
end;

procedure TPoposigningKeyInput.SetAuthInfo(const Value: TAuthInfo);
begin
  TAuthInfo(FItems[0]).Assign(Value);
end;

function TPoposigningKeyInput.GetPublicKey: TSubjectPublicKeyInfo;
begin
  Result := FItems[1];
end;

procedure TPoposigningKeyInput.SetPublicKey(const Value: TSubjectPublicKeyInfo);
begin
  TSubjectPublicKeyInfo(FItems[1]).Assign(Value);
end;

{ TPoposigningKey }

constructor TPoposigningKey.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template.FindField('pop')^.Choices[2]^;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TPoposigningKeyInput,nil,FData.Items[0]^,nil^);
  FieldFactory(TAlgorithmIdentifier,nil,FData.Items[1]^,nil^);
  FieldFactory(TBitString,nil,FData.Items[2]^,nil^);
end;

destructor TPoposigningKey.Destroy;
begin
  inherited Destroy;
end;

function TPoposigningKey.GetPoposkInput: TPoposigningKeyInput;
begin
  Result := FItems[0];
end;

procedure TPoposigningKey.SetPoposkInput(const Value: TPoposigningKeyInput);
begin
  TPoposigningKeyInput(FItems[0]).Assign(Value);
end;

function TPoposigningKey.GetAlgorithmIdentifier: TAlgorithmIdentifier;
begin
  Result := FItems[1];
end;

procedure TPoposigningKey.SetAlgorithmIdentifier(const Value: TAlgorithmIdentifier);
begin
  TAlgorithmIdentifier(FItems[1]).Assign(Value);
end;

function TPoposigningKey.GetSignature: TBitString;
begin
  Result := FItems[2];
end;

procedure TPoposigningKey.SetSignature(const Value: TBitString);
begin
  TBitString(FItems[2]).Assign(Value);
end;

{ TPopoprivKey }

constructor TPopoprivKey.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template.FindField('pop')^.Choices[4]^.Items[0]^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TBitString);
  FChoiceList.Add(TIntegerWrapper);
  FChoiceList.Add(TBitString);
end;

destructor TPopoprivKey.Destroy;
begin
  inherited Destroy;
end;

function TPopoprivKey.GetAsThisMessage: TBitString;
begin
  Result := (FSelected as TBitString);
end;

procedure TPopoprivKey.SetAsThisMessage(const Value: TBitString);
begin
  InternalSelectChoice(0);
  (FSelected as TBitString).Assign(Value);
end;

function TPopoprivKey.GetAsSubsequentMessage: TIntegerWrapper;
begin
  Result := (FSelected as TIntegerWrapper);
end;

procedure TPopoprivKey.SetAsSubsequentMessage(const Value: TIntegerWrapper);
begin
  InternalSelectChoice(1);
  (FSelected as TIntegerWrapper).Assign(Value);
end;

function TPopoprivKey.GetAsDhMAC: TBitString;
begin
  Result := (FSelected as TBitString);
end;

procedure TPopoprivKey.SetAsDhMAC(const Value: TBitString);
begin
  InternalSelectChoice(2);
  (FSelected as TBitString).Assign(Value);
end;

function TPopoprivKey.GetChoice: TPopoprivKeyEnum;
begin
  Result := TPopoprivKeyEnum(InternalGetChoice);
end;

procedure TPopoprivKey.SetChoice(const Value: TPopoprivKeyEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TProofOfPossession }

constructor TProofOfPossession.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template;
    F := F.FindField('pop')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(nil);
  FChoiceList.Add(TPoposigningKey);
  FChoiceList.Add(TPopoprivKey);
  FChoiceList.Add(TPopoprivKey);
end;

destructor TProofOfPossession.Destroy;
begin
  inherited Destroy;
end;

function TProofOfPossession.GetAsSignature: TPoposigningKey;
begin
  Result := (FSelected as TPoposigningKey);
end;

procedure TProofOfPossession.SetAsSignature(const Value: TPoposigningKey);
begin
  InternalSelectChoice(1);
  (FSelected as TPoposigningKey).Assign(Value);
end;

function TProofOfPossession.GetAsKeyEncipherment: TPopoprivKey;
begin
  Result := (FSelected as TPopoprivKey);
end;

procedure TProofOfPossession.SetAsKeyEncipherment(const Value: TPopoprivKey);
begin
  InternalSelectChoice(2);
  (FSelected as TPopoprivKey).Assign(Value);
end;

function TProofOfPossession.GetAsKeyAgreement: TPopoprivKey;
begin
  Result := (FSelected as TPopoprivKey);
end;

procedure TProofOfPossession.SetAsKeyAgreement(const Value: TPopoprivKey);
begin
  InternalSelectChoice(3);
  (FSelected as TPopoprivKey).Assign(Value);
end;

function TProofOfPossession.GetChoice: TProofOfPossessionEnum;
begin
  Result := TProofOfPossessionEnum(InternalGetChoice);
end;

procedure TProofOfPossession.SetChoice(const Value: TProofOfPossessionEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TRegValue }

constructor TRegValue.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template.FindField('regInfo')^.Template;
    F := F.FindField('regValue')^;
    FData.CopyTypeInfo(F);
  end;
  FChoiceList.Add(TWideStringWrapper);
  FChoiceList.Add(TCertRequest);
end;

destructor TRegValue.Destroy;
begin
  inherited Destroy;
end;

function TRegValue.GetAsRegInfo_Utf8Pairs: WideString;
begin
  Result := (FSelected as TWideStringWrapper).Value;
end;

procedure TRegValue.SetAsRegInfo_Utf8Pairs(const Value: WideString);
begin
  InternalSelectChoice(0);
  (FSelected as TWideStringWrapper).Value := Value;
end;

function TRegValue.GetAsRegInfo_CertReq: TCertRequest;
begin
  Result := (FSelected as TCertRequest);
end;

procedure TRegValue.SetAsRegInfo_CertReq(const Value: TCertRequest);
begin
  InternalSelectChoice(1);
  (FSelected as TCertRequest).Assign(Value);
end;

function TRegValue.GetChoice: TRegValueEnum;
begin
  Result := TRegValueEnum(InternalGetChoice);
end;

procedure TRegValue.SetChoice(const Value: TRegValueEnum);
begin
  InternalSelectChoice(Ord(Value)-1);
end;

{ TRegInfo }

constructor TRegInfo.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template.FindField('regInfo')^.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TStringWrapper,nil,FData.Items[0]^,nil^);
  FieldFactory(TRegValue,nil,FData.Items[1]^,nil^);
end;

destructor TRegInfo.Destroy;
begin
  inherited Destroy;
end;

function TRegInfo.GetRegID: ObjectIdentifier;
begin
  Result := ObjectIdentifier(TStringWrapper(FItems[0]).Value);
end;

procedure TRegInfo.SetRegID(const Value: ObjectIdentifier);
begin
  TStringWrapper(FItems[0]).Value := Value;
end;

function TRegInfo.GetRegValue: TRegValue;
begin
  Result := FItems[1];
end;

procedure TRegInfo.SetRegValue(const Value: TRegValue);
begin
  TRegValue(FItems[1]).Assign(Value);
end;

function TRegInfo.IsType(Id: TRegValueEnum): Boolean;
begin
  Result := TRegValue(FItems[1]).Choice = Id;
end;

procedure TRegInfo.SetType(Id: TRegValueEnum);
begin
  TRegValue(FItems[1]).Choice := Id;
end;

{ TSequenceOfRegInfo }

constructor TSequenceOfRegInfo.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  FFieldType := TRegInfo;
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template;
    F := F.FindField('regInfo')^;
    FData.CopyTypeInfo(F);
  end;
end;

destructor TSequenceOfRegInfo.Destroy;
begin
  inherited Destroy;
end;

function TSequenceOfRegInfo.GetUniqueItem(Id: TRegValueEnum): TRegInfo;
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

function TSequenceOfRegInfo.AddUniqueItem(Id: TRegValueEnum): TRegInfo;
begin
  Result := GetUniqueItem(Id);
  if Result = nil then begin
    Result := Add;
    Result.SetType(Id);
  end;
end;

function TSequenceOfRegInfo.Add: TRegInfo;
begin
  Result := TRegInfo(InternalAdd);
end;

function TSequenceOfRegInfo.GetItems(Index: Integer): TRegInfo;
begin
  Result := TRegInfo(InternalGetItems(Index));
end;

{ TCertReqMsg }

constructor TCertReqMsg.Create;
var
  F: TASN1Struct;
begin                
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  inherited;
  if FData.TypeName = '' then begin
    F := GlobalObject.Template;
    FData.CopyTypeInfo(F);
  end;
  FieldFactory(TCertRequest,nil,FData.Items[0]^,nil^);
  FieldFactory(TProofOfPossession,nil,FData.Items[1]^,nil^);
  FieldFactory(TSequenceOfRegInfo,nil,FData.Items[2]^,nil^);
end;

destructor TCertReqMsg.Destroy;
begin
  inherited Destroy;
end;

function TCertReqMsg.GetCertReq: TCertRequest;
begin
  Result := FItems[0];
end;

procedure TCertReqMsg.SetCertReq(const Value: TCertRequest);
begin
  TCertRequest(FItems[0]).Assign(Value);
end;

function TCertReqMsg.GetPop: TProofOfPossession;
begin
  Result := FItems[1];
end;

procedure TCertReqMsg.SetPop(const Value: TProofOfPossession);
begin
  TProofOfPossession(FItems[1]).Assign(Value);
end;

function TCertReqMsg.GetRegInfo: TSequenceOfRegInfo;
begin
  Result := FItems[2];
end;

procedure TCertReqMsg.SetRegInfo(const Value: TSequenceOfRegInfo);
begin
  TSequenceOfRegInfo(FItems[2]).Assign(Value);
end;

function TCertReqMsg.CheckSignature(CACert: TCertificate): Boolean;
begin
  if CACert = nil then begin
    if not CertReq.CertTemplate.PublicKey.Data.IsEmpty then
      Result := InternalCheckSignature(CertReq.CertTemplate.PublicKey)
    else
      Result := InternalCheckSignature(nil);
  end else begin
    Result := IsAllegedIssuer(CACert);
    if Result then
      Result := InternalCheckSignature(CACert);
  end;
end;

function TCertReqMsg.CheckSignature(
  PublicKey: IReadOnlyPublicKeyInfo): Boolean;
begin
  Result := InternalCheckSignature(PublicKey);
end;

function TCertReqMsg.DSASign(const DSAKey: TDLPrivateKey): Boolean;
begin
  Result := DSASign(DSAKey,TCertificate(nil));
end;

function TCertReqMsg.DSASign(const DSAKey: TDLPrivateKey;
  CACert: TASN1Struct): Boolean;
var
  Crt: TCertificate;
begin
  Crt := TCertificate.Create(nil,nil);
  try
    Crt.Data.ReadOnly := False;
    Crt.AssignStruct(CACert);
    Result := DSASign(DSAkey,Crt);
  finally
    Crt.Free;
  end;
end;

function TCertReqMsg.DSASign(const DSAKey: TDLPrivateKey;
  CACert: TCertificate): Boolean;
{$IFDEF SHA1}
var
  OID, D: string;
  MS: TSecureMemoryStream;
  Len: Integer;
  Params: TASNCustomWrapper;
{$ENDIF SHA1}
begin
{$IFDEF SHA1}
  OID := id_dsa_with_sha1;
  Result := BeforeSign(CACert,OID,Params);
  if Result then begin
    MS := TSecureMemoryStream.Create;
    try
      FData.Items[0].SaveToStream(MS,fmtDER);
      SetLength(D,8192);
      Len := DLSSASignatureGeneration(DSAKey,MS.Memory^,MS.Size,haSHA1,True,D[1],Length(D));
      SetLength(D,Len);
      D := #0 + D;
      Pop.AsSignature.Signature.SetBinary(D[1],Length(D));
    finally
      MS.Free;
    end;
  end;
{$ELSE  SHA1}
  Result := False;
{$ENDIF SHA1}
end;

function TCertReqMsg.ECDSASign(const ECDSAKey: TECPrivateKey;
  CACert: TCertificate): Boolean;
{$IFDEF SHA1}
var
  OID, D: string;
  MS: TSecureMemoryStream;
  Len: Integer;
  Params: TASNCustomWrapper;
{$ENDIF}
begin
{$IFDEF SHA1}
  OID := ecdsa_with_sha1;
  Result := BeforeSign(CACert,OID,Params);
  if Result then begin
    MS := TSecureMemoryStream.Create;
    try
      FData.Items[0].SaveToStream(MS,fmtDER);
      SetLength(D,8192);
      Len := ECSSASignatureGeneration(ECDSAKey,MS.Memory^,MS.Size,haSHA1,True,D[1],Length(D));
      SetLength(D,Len);
      D := #0 + D;
      Pop.AsSignature.Signature.SetBinary(D[1],Length(D));
    finally
      MS.Free;
    end;
  end;
{$ELSE  SHA1}
  Result := False;
{$ENDIF SHA1}
end;

function TCertReqMsg.ECDSASign(const ECDSAKey: TECPrivateKey): Boolean;
begin
  Result := ECDSASign(ECDSAKey,TCertificate(nil));
end;

function TCertReqMsg.ECDSASign(const ECDSAKey: TECPrivateKey;
  CACert: TASN1Struct): Boolean;
var
  Crt: TCertificate;
begin
  Crt := TCertificate.Create(nil,nil);
  try
    Crt.Data.ReadOnly := False;
    Crt.AssignStruct(CACert);
    Result := ECDSASign(ECDSAkey,Crt);
  finally
    Crt.Free;
  end;
end;

function TCertReqMsg.RSASign(const RSAKey: TIFPrivateKey;
  HA: THashAlgorithm; CACert: TASN1Struct): Boolean;
var
  Crt: TCertificate;
begin
  Crt := TCertificate.Create(nil,nil);
  try
    Crt.Data.ReadOnly := False;
    Crt.AssignStruct(CACert);
    Result := RSASign(RSAkey,HA,Crt);
  finally
    Crt.Free;
  end;
end;

function TCertReqMsg.RSASign(const RSAKey: TIFPrivateKey;
  HA: THashAlgorithm; CACert: TCertificate): Boolean;
var
  OID, D: string;
  MS: TSecureMemoryStream;
  Len: Integer;
  Params: TASNCustomWrapper;
begin
  OID := MpIF.SignatureAlgEMSA3(HA);
  if OID = '' then
    Result := False
  else begin
    Result := BeforeSign(CACert,OID,Params);
    if Result then begin
      MS := TSecureMemoryStream.Create;
      try
        FData.CalculateLength;
        FData.Items[0].SaveToStream(MS,fmtDER);
        SetLength(D,8192);
        Len := IFSSASignatureGeneration(RSAKey,
                                        MS.Memory^,MS.Size,
                                        HA,HA,seEMSA3,
                                        D[1],Length(D));
        SetLength(D,Len);
        D := #0 + D;
        Pop.AsSignature.Signature.SetBinary(D[1],Length(D));
      finally
        MS.Free;
      end;
      FData.CalculateLength;
    end;
  end;
end;

function TCertReqMsg.RSASign(const RSAKey: TIFPrivateKey;
  HA: THashAlgorithm): Boolean;
begin
  Result := RSASign(RSAKey,HA,TCertificate(nil));
end;

function TCertReqMsg.RSASSA_PSSSign(const RSAKey: TIFPrivateKey;
  CACert: TCertificate): Boolean;
var
  OID, D: string;
  HA, MGFHA: THashAlgorithm;
  MS: TSecureMemoryStream;
  Len: Integer;
  Params: TASNCustomWrapper;
begin
  OID := id_RSASSA_PSS;
  Result := BeforeSign(CACert,OID,Params);
  if Result then begin
    Result := OIDToHashAlgorithm(Pop.AsSignature.AlgorithmIdentifier.Parameters.AsRsassaPss.HashFunc.Algorithm,HA);
{$IFDEF SHA1}
    if not Result then HA := haSHA1;
    Result := True;
{$ENDIF SHA1}
    if Result then begin
      Result := OIDToHashAlgorithm(Pop.AsSignature.AlgorithmIdentifier.Parameters.AsRsassaPss.MgfFunc.Parameters.AsMgf1.Algorithm,MGFHA);
{$IFDEF SHA1}
      if not Result then MGFHA := haSHA1;
      Result := True;
{$ENDIF SHA1}
    end;
  end;
  if Result then begin
    MS := TSecureMemoryStream.Create;
    try
      FData.Items[0].SaveToStream(MS,fmtDER);
      SetLength(D,8192);
      Len := IFSSASignatureGeneration(RSAKey,
                                      MS.Memory^,MS.Size,
                                      HA,MGFHA,seEMSA4,
                                      D[1],Length(D));
      SetLength(D,Len);
      D := #0 + D;
      Pop.AsSignature.Signature.SetBinary(D[1],Length(D));
    finally
      MS.Free;
    end;
    FData.CalculateLength;
  end;
end;

function TCertReqMsg.RSASSA_PSSSign(const RSAKey: TIFPrivateKey;
  CACert: TASN1Struct): Boolean;
var
  Crt: TCertificate;
begin
  Crt := TCertificate.Create(nil,nil);
  try
    Crt.Data.ReadOnly := False;
    Crt.AssignStruct(CACert);
    Result := RSASSA_PSSSign(RSAkey,Crt);
  finally
    Crt.Free;
  end;
end;

function TCertReqMsg.RSASSA_PSSSign(const RSAKey: TIFPrivateKey): Boolean;
begin
  Result := RSASSA_PSSSign(RSAKey,TCertificate(nil));
end;

function TCertReqMsg.InternalCheckSignature(
  CACert: IReadOnlyPublicKeyInfo): Boolean;
var
  RSAKey: TIFPublicKey;
  DLKey: TDLPublicKey;
  ECKey: TECPublicKey;
  MS: TSecureMemoryStream;
  HA,MHA: THashAlgorithm;
  EM: TSignEncoding;
  Sign: Pointer;
  SignLen: Integer;
begin
  Result := Pop.Choice = popeSignature;
  if not Result then
    Exit;

  MS := TSecureMemoryStream.Create;
  try
    if CertReq.CertTemplate.PublicKey.Data.IsEmpty and
       CertReq.CertTemplate.Subject.Data.IsEmpty then begin
      Result := not (Pop.AsSignature.PoposkInput.PublicKey.Data.IsEmpty or
                     Pop.AsSignature.PoposkInput.AuthInfo.Data.IsEmpty);
      if Result then begin
        CACert := Pop.AsSignature.PoposkInput.PublicKey;
        Pop.AsSignature.PoposkInput.SaveToStream(MS);
      end;
    end else if CACert <> nil then
      CertReq.SaveToStream(MS);

    Result := Result and (CACert <> nil);
    if Result then begin

      EM := seEMSA3;
{$IFDEF SHA1}
      HA := haSHA1;
      MHA := haSHA1;
{$ENDIF SHA1}
      if (Pop.AsSignature.AlgorithmIdentifier.Algorithm = id_DSA_with_SHA1) or
         (Pop.AsSignature.AlgorithmIdentifier.Algorithm = ecdsa_with_SHA1) then
{$IFDEF SHA1}
        Result := True
{$ELSE SHA1}
        Result := False
{$ENDIF SHA1}
      else
        Result := InterpretRSASignatureAlgorithm(HA,MHA,EM);
      if Result then begin
        FillChar(RSAKey,SizeOf(RSAKey),0);
        FillChar(DLKey,SizeOf(DLKey),0);
        FillChar(ECKey,SizeOf(ECKey),0);
        if CACert.ExtractSubjectRSAPublicKey(RSAKey) then try
          Sign := Pop.AsSignature.Signature.Data.Content;
          SignLen := Pop.AsSignature.Signature.Data.Length;
          Result := IFSSASignatureVerification(RSAKey,MS.Memory^,MS.Size,
                                               Sign^,
                                               SignLen,
                                               HA,MHA,EM);
        finally
          DisposeIFPublicKey(RSAKey);
        end else if CACert.ExtractSubjectDSAPublicKey(DLKey) then try
          Sign := @Pop.AsSignature.Signature.Data.Content[1];
          SignLen := Pop.AsSignature.Signature.Data.Length - 1;
          Result := DLSSASignatureVerification(DLKey,MS.Memory^,MS.Size,
                                               Sign^,
                                               SignLen,
                                               True,HA);
        finally
          DisposeDLPublicKey(DLKey);
        end else if CACert.ExtractSubjectECPublicKey(ECKey) then try
          Sign := @Pop.AsSignature.Signature.Data.Content[1];
          SignLen := Pop.AsSignature.Signature.Data.Length - 1;
          Result := ECSSASignatureVerification(ECKey,MS.Memory^,MS.Size,
                                               Sign^,
                                               SignLen,
                                               True,HA);
        finally
          DisposeECPublicKey(ECKey);
        end else
          Result := False;
      end;

    end;
  finally
    MS.Free;
  end;
end;

function TCertReqMsg.InterpretRSASignatureAlgorithm(var HA,
  MHA: THashAlgorithm; var EM: TSignEncoding): Boolean;
begin
  Result := True;
  if MpIF.ExtractHashAlgEMSA3(Pop.AsSignature.AlgorithmIdentifier.Algorithm,HA) then
    EM := seEMSA3
  else if Pop.AsSignature.AlgorithmIdentifier.Algorithm = id_RSASSA_PSS then begin
    EM := seEMSA4;
    Result := OIDToHashAlgorithm(Pop.AsSignature.AlgorithmIdentifier.Parameters.AsRsassaPss.HashFunc.Algorithm,HA);
{$IFDEF SHA1}
    if not Result then HA := haSHA1;
    Result := True;
{$ENDIF SHA1}
    if Result then begin
      Result := OIDToHashAlgorithm(Pop.AsSignature.AlgorithmIdentifier.Parameters.AsRsassaPss.MgfFunc.Parameters.AsMgf1.Algorithm,MHA);
{$IFDEF SHA1}
      if not Result then HA := haSHA1;
      Result := True;
{$ENDIF SHA1}
    end;
  end;
end;

function TCertReqMsg.IsAllegedIssuer(CACert: TCertificate): Boolean;
begin
  Result := Pop.Choice = popeSignature;
  if Result then begin
    Result := Pop.AsSignature.PoposkInput.AuthInfo.Choice = aieSender;
    if Result then begin
      Result := Pop.AsSignature.PoposkInput.AuthInfo.AsSender.Choice = gneDirectoryName;
      if Result then
        Result := Pop.AsSignature.PoposkInput.AuthInfo.AsSender.AsDirectoryName.Compare(CACert.Subject);
    end;
  end;
end;

function TCertReqMsg.VerifyPBMac(const Password: string): Boolean;
{$IFDEF SHA1}
var
  HA: THashAlgorithm;
  Dig: string;
  I: Integer;
{$ENDIF SHA1}
begin
  if Pop.Choice = popeSignature then begin
{$IFDEF SHA1}
    Result := Pop.AsSignature.PoposkInput.AuthInfo.Choice <> aiePublicKeyMAC;
    if not Result then begin
      Result := Password <> '';
      if Result then begin
        Result := OIDToHashAlgorithm(Pop.AsSignature.PoposkInput.AuthInfo.AsPublicKeyMAC.AlgId.Parameters.AsPasswordBasedMac.Owf.Algorithm,HA);
        if Result then begin
          Dig := DigestString(HA,Password +
                              Pop.AsSignature.PoposkInput.AuthInfo.AsPublicKeyMAC.AlgId.Parameters.AsPasswordBasedMac.Salt.AsString);
          for I := 1 to Pop.AsSignature.PoposkInput.AuthInfo.AsPublicKeyMAC.AlgId.Parameters.AsPasswordBasedMac.IterationCount.AsInteger - 1 do
            Dig := DigestString(HA,Dig);
          Dig := HMacString(haSHA1,Dig,20,Pop.AsSignature.PoposkInput.PublicKey.Data.ContentAsOctetString);
          Result := CompareMem(Pointer(Dig),
                               Pop.AsSignature.PoposkInput.AuthInfo.AsPublicKeyMAC.Value.Data.Content,
                               Pop.AsSignature.PoposkInput.AuthInfo.AsPublicKeyMAC.Value.Data.Length);
        end;
      end;
    end;
{$ELSE  SHA1}
    Result := False;
{$ENDIF SHA1}
  end else
    Result := True;
end;

procedure TCertReqMsg.SetPBMac(const Password: string; Iter: Integer);
{$IFDEF SHA1}
var
  PW, Dig, Salt: string;
  I: Integer;
{$ENDIF SHA1}
begin
  if Pop.Choice = popeSignature then begin
{$IFDEF SHA1}
    Pop.AsSignature.PoposkInput.AuthInfo.Choice := aiePublicKeyMAC;
    Pop.AsSignature.PoposkInput.AuthInfo.AsPublicKeyMAC.AlgId.Algorithm := '1.2.840.113533.7.66.13';
    Pop.AsSignature.PoposkInput.AuthInfo.AsPublicKeyMAC.AlgId.Parameters.Choice := pePasswordBasedMac;
    Pop.AsSignature.PoposkInput.AuthInfo.AsPublicKeyMAC.AlgId.Parameters.AsPasswordBasedMac.Owf.Algorithm := id_sha1;
    SetLength(Salt,20);
    Pop.AsSignature.PoposkInput.AuthInfo.AsPublicKeyMAC.AlgId.Parameters.AsPasswordBasedMac.Salt.AsString := Salt;
    SetLength(PW,Length(Password) + 20);
    try
      Move(Pointer(Password)^,Pointer(PW)^,Length(Password));
      Move(Pointer(Salt)^,PW[1 + Length(Password)],20);
      Dig := DigestString(haSHA1,PW);
    finally
      ProtectClear(Pointer(PW)^,Length(PW));
    end;
    if Iter < 1 then Iter := 1;
    Pop.AsSignature.PoposkInput.AuthInfo.AsPublicKeyMAC.AlgId.Parameters.AsPasswordBasedMac.IterationCount.AsInteger := Iter;
    for I := 1 to Iter - 1 do
      Dig := DigestString(haSHA1,Dig);
    Dig := HMacString(haSHA1,Dig,20,Pop.AsSignature.PoposkInput.PublicKey.Data.ContentAsOctetString);
    Pop.AsSignature.PoposkInput.AuthInfo.AsPublicKeyMAC.Value.SetBinary(Pointer(Dig)^,Length(Dig));
{$ELSE  SHA1}
    raise Exception.Create('TCertReqMsg.SetPBMac: Action requires SHA-1');
{$ENDIF SHA1}
  end;
end;

function TCertReqMsg.BeforeSign(var CACert: TCertificate;
  SignAlg: ObjectIdentifier; out Params: TASNCustomWrapper): Boolean;
var
  Ext: TExtension;
begin
  Result := True;
  if Assigned(CACert) then begin
    Ext := CACert.TBSCertificate.Extensions.UniqueItem[eveIdCeKeyUsage];
    if Assigned(Ext) then
      if not Ext.ExtnValue.AsCe_KeyUsage.Bits[digitalSignatureVal] then begin
        Result := False;
        Exit;
      end;

    Pop.Choice := popeSignature;
    Pop.AsSignature.AlgorithmIdentifier.Algorithm := SignAlg;
    Params := Pop.AsSignature.AlgorithmIdentifier.Parameters;
    if SignAlg = id_RSASSA_PSS then begin
      if CACert.SubjectPublicKeyInfo.Algorithm.Parameters.Choice = aideIdRsaesOaep then begin
        TRsassaPssParams(Params).HashFunc.Algorithm :=
          CACert.SubjectPublicKeyInfo.Algorithm.Parameters.AsRsaesOaep.HashFunc.Algorithm;
        TRsassaPssParams(Params).MgfFunc.Algorithm := id_mgf1;
        TRsassaPssParams(Params).MgfFunc.Parameters.AsMgf1.Algorithm;
          CACert.SubjectPublicKeyInfo.Algorithm.Parameters.AsRsaesOaep.MgfFunc.Parameters.AsMgf1.Algorithm;
      end else if CACert.SubjectPublicKeyInfo.Algorithm.Parameters.Choice = aideIdRsassaPss then begin
        TRsassaPssParams(Params).HashFunc.Algorithm :=
          CACert.SubjectPublicKeyInfo.Algorithm.Parameters.AsRsassaPss.HashFunc.Algorithm;
        TRsassaPssParams(Params).MgfFunc.Algorithm := id_mgf1;
        TRsassaPssParams(Params).MgfFunc.Parameters.AsMgf1.Algorithm;
          CACert.SubjectPublicKeyInfo.Algorithm.Parameters.AsRsassaPss.MgfFunc.Parameters.AsMgf1.Algorithm;
      end else begin
        Result := False;
        Exit;
      end;
    end;

    Pop.AsSignature.PoposkInput.AuthInfo.Choice := aieSender;
    Pop.AsSignature.PoposkInput.AuthInfo.AsSender.Choice := gneDirectoryName;
    Pop.AsSignature.PoposkInput.AuthInfo.AsSender.AsDirectoryName.Assign(CACert.Subject);
  end;
end;

{ TCertReqMessages }

constructor TCertReqMessages.Create;
begin                      
  Assert(Assigned(GlobalObject),'PKIX_CRMF not initialized: Call StrSecInit.InitPKIX_CRMF to correct.');
  FFieldType := TCertReqMsg;
  inherited;
  FData.Assign(GlobalObject);
end;

destructor TCertReqMessages.Destroy;
begin
  inherited Destroy;
end;

function TCertReqMessages.Add: TCertReqMsg;
begin
  Result := TCertReqMsg(InternalAdd);
end;

function TCertReqMessages.GetItems(Index: Integer): TCertReqMsg;
begin
  Result := TCertReqMsg(InternalGetItems(Index));
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
