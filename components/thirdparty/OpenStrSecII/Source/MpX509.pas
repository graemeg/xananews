{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     MPX509 Unit                                       }
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
{$B-}
{$I ver.inc}
unit MpX509;

interface

uses
  Controls,
  Classes, SysUtils, SyncObjs, SecUtils, ResourceFile, Asn1, Pkix, Tls,
  X509Base, MpPK, MpIF, MpDL, MpEC, Pkix_Cert, Pkix_CRL;

const
  Jan1st2050: TDateTime = 54789;

type
  TMPX501Name = class(TPersistent)
  private
    FData: TX501Name;
    FOwner: TPersistent;
    FReadOnly: Boolean;
    function GetcommonName: WideString;
    function GetcountryName: WideString;
    function GetdnQualifier: WideString;
    function GetemailAddress: WideString;
    function GetgenerationQualifier: WideString;
    function GetgivenName: WideString;
    function Getinitials: WideString;
    function GetlocalityName: WideString;
    function Getname: WideString;
    function GetorganizationalUnitName: WideString;
    function GetorganizationName: WideString;
    function GetstateOrProvinceName: WideString;
    function Getsurname: WideString;
    function Gettitle: WideString;
    procedure SetcommonName(const Value: WideString);
    procedure SetcountryName(const Value: WideString);
    procedure SetdnQualifier(const Value: WideString);
    procedure SetemailAddress(const Value: WideString);
    procedure SetgenerationQualifier(const Value: WideString);
    procedure SetgivenName(const Value: WideString);
    procedure Setinitials(const Value: WideString);
    procedure SetlocalityName(const Value: WideString);
    procedure Setname(const Value: WideString);
    procedure SetorganizationalUnitName(const Value: WideString);
    procedure SetorganizationName(const Value: WideString);
    procedure SetstateOrProvinceName(const Value: WideString);
    procedure Setsurname(const Value: WideString);
    procedure Settitle(const Value: WideString);
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure ChangeOwner;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    property Data: TX501Name read FData;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
  published
    property commonName: WideString read GetcommonName write SetcommonName;
    property surname: WideString read Getsurname write Setsurname;
    property countryName: WideString read GetcountryName write SetcountryName;
    property localityName: WideString read GetlocalityName write SetlocalityName;
    property stateOrProvinceName: WideString read GetstateOrProvinceName write SetstateOrProvinceName;
    property organizationName: WideString read GetorganizationName write SetorganizationName;
    property organizationalUnitName: WideString read GetorganizationalUnitName write SetorganizationalUnitName;
    property title: WideString read Gettitle write Settitle;
    property name: WideString read Getname write Setname;
    property givenName: WideString read GetgivenName write SetgivenName;
    property initials: WideString read Getinitials write Setinitials;
    property generationQualifier: WideString read GetgenerationQualifier write SetgenerationQualifier;
    property dnQualifier: WideString read GetdnQualifier write SetdnQualifier;
    property emailAddress: WideString read GetemailAddress write SetemailAddress;
  end;

  TMPX509Validity = class(TPersistent)
  private
    FData: TX509Validity;
    FReadOnly: Boolean;
    function GetnotAfter: TDateTime;
    function GetnotBefore: TDateTime;
    procedure SetnotAfter(const Value: TDateTime);
    procedure SetnotBefore(const Value: TDateTime);
    procedure SetReadOnly(const Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
  published
    property notAfter: TDateTime read GetnotAfter write SetnotAfter;
    property notBefore: TDateTime read GetnotBefore write SetnotBefore;
  end;

  TipMask = 0..4;
  TMPX509GeneralName = class;

  TMPX509ediPartyName = class(TPersistent)
  private
    FnameAssigner: TMPX501Name;
    FOwner: TMPX509GeneralName;
    FpartyName: TMPX501Name;
    procedure SetnameAssigner(const Value: TMPX501Name);
    procedure SetpartyName(const Value: TMPX501Name);
  protected
    procedure ChangeOwner;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TMPX509GeneralName);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
  published
    property nameAssigner: TMPX501Name read FnameAssigner write SetnameAssigner;
    property partyName: TMPX501Name read FpartyName write SetpartyName;
  end;

  TMPX509GeneralName = class(TCollectionItem)
  private
    FData: TX509GeneralName;
    FediPartyName: TMPX509ediPartyName;
    FdirectoryName: TMPX501Name;
    procedure SetdirectoryName(const Value: TMPX501Name);
    procedure SetdNSName(const Value: string);
    procedure SetediPartyName(const Value: TMPX509ediPartyName);
    procedure SetipAddress(const Value: string);
    procedure SetipMask(const Value: TipMask);
    procedure SetregistredID(const Value: string);
    procedure Setrfc822Name(const Value: string);
    procedure SetuniformResourceIdentifier(const Value: string);
    function GetdNSName: string;
    function GetipAddress: string;
    function GetipMask: TipMask;
    function GetregistredID: string;
    function Getrfc822Name: string;
    function GetuniformResourceIdentifier: string;
    function GetTag: Integer;
    procedure SetTag(const Value: Integer);
  protected
    procedure Change(NewTag: Integer);
    function GetDisplayName: string; override;
    procedure SetData(const Data: TX509GeneralName);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property rfc822Name: string read Getrfc822Name write Setrfc822Name;
    property dNSName: string read GetdNSName write SetdNSName;
    property directoryName: TMPX501Name read FdirectoryName write SetdirectoryName;
    property ediPartyName: TMPX509ediPartyName read FediPartyName write SetediPartyName;
    property uniformResourceIdentifier: string read GetuniformResourceIdentifier write SetuniformResourceIdentifier;
    property ipAddress: string read GetipAddress write SetipAddress;
    property ipMask: TipMask read GetipMask write SetipMask;
    property registredID: string read GetregistredID write SetregistredID;
    property Tag: Integer read GetTag write SetTag;
  end;

  TMPX509GeneralNames = class(TOwnedCollection)
  private
    FData: TX509GeneralNames;
  protected
    procedure Compose;
    procedure Interpret;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    procedure Assign(Source: TPersistent); override;
  end;

  TMPASN1Structs = class;

  TMPASN1Struct = class(TCollectionItem)
  private
    FData: PASN1Struct;
    FContents: TMPASN1Structs;
    function GetCls: Byte;
    function GetConstructed: Boolean;
    function GetContent: string;
    function GetTag: Cardinal;
    procedure SetCls(const Value: Byte);
    procedure SetConstructed(const Value: Boolean);
    procedure SetContent(const Value: string);
    procedure SetContents(const Value: TMPASN1Structs);
    procedure SetTag(const Value: Cardinal);
    function GetValue: string;
    procedure SetValue(const Value: string);
  protected
    procedure Compose;
    function DataAssigned: Boolean;
    function GetDisplayName: string; override;
    procedure Interpret;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Cls: Byte read GetCls write SetCls;
    property Constructed: Boolean read GetConstructed write SetConstructed;
    property Tag: Cardinal read GetTag write SetTag;
    property Content: string read GetContent write SetContent;
    property Contents: TMPASN1Structs read FContents write SetContents;
    property Value: string read GetValue write SetValue;
  end;

  TMPASN1Structs = class(TOwnedCollection)
  private
    FData: PASN1Struct;
    function GetCls: Byte;
    function GetTag: Cardinal;
    procedure SetCls(const Value: Byte);
    procedure SetTag(const Value: Cardinal);
  protected
    procedure Compose;
    procedure InternalInterpret;
    procedure Interpret;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TMPASN1Struct;
    procedure Assign(Source: TPersistent); override;
    property Cls: Byte read GetCls write SetCls;
    property Tag: Cardinal read GetTag write SetTag;
  end;

  TMPX509Algorithm = class(TPersistent)
  private
    FOwner: TPersistent;
    FData: TX509AlgorithmIdentifier;
    FParameters: TMPASN1Structs;
    function GetAlgID: string;
    function GetAlgName: string;
    procedure SetAlgID(const Value: string);
    procedure SetAlgName(const Value: string);
    procedure SetParameters(const Value: TMPASN1Structs);
  protected
    function GetOwner: TPersistent; override;
    procedure SetData(const Data: TX509AlgorithmIdentifier);
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    property Parameters: TMPASN1Structs read FParameters write SetParameters;
  published
    property AlgorithmID: string read GetAlgID write SetAlgID;
    property AlgorithmName: string read GetAlgName write SetAlgName;
  end;

  TMPX509Extension = class(TCollectionItem)
  private
    FData: TX509Extension;
    FextnValue: TMPASN1Structs;
    function GetCritical: Boolean;
    function GetExtnID: string;
    function GetExtnName: string;
    procedure SetCritical(const Value: Boolean);
    procedure SetExtnID(const Value: string);
    procedure SetExtnName(const Value: string);
    procedure SetextnValue(const Value: TMPASN1Structs);
    function GetExtnValueCnt: string;
    procedure SetExtnValueCnt(const Value: string);
  protected    
    function GetDisplayName: string; override;
    procedure SetData(const Data: TX509Extension);
  public        
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Critical: Boolean read GetCritical write SetCritical;
    property extnID: string read GetExtnID write SetExtnID;
    property extnName: string read GetExtnName write SetExtnName;
    property extnValue: TMPASN1Structs read FextnValue write SetextnValue;
    property extnValueContent: string read GetExtnValueCnt write SetExtnValueCnt;
  end;

  TMPX509Extensions = class(TOwnedCollection)
  private
    FCert: PASN1Struct;
  protected
    procedure Compose;
    procedure Interpret;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    procedure Assign(Source: TPersistent); override;
  end;

  TCertStatusCode = (crcOK,crcExpired,crcInvalidSignature,crcCANotTrusted,
                     crcRevoked,crcCAExpiredOrRevoked,crcConstraintFault,
                     crcInvalidKeyUsage,crcUnsupportedExtension,crcSuperseeded,
                     crcSyntax,crcBaseNotFound,crcTrusted,crcTooSmallKey,
                     crcPolicyNotAccepted,crcDuplicateKeyIdentifier);
  TCertStatusCodes = set of TCertStatusCode;

  TBeforeImportTLSCertEvent = procedure (Sender: TObject;
                                         Cert: TASN1Struct;
                                         var ExplicitTrust: Boolean;
                                         var AllowExpired: Boolean) of object;
  TCertNotTrustedEvent = procedure (Sender: TObject;
                                    Cert: TASN1Struct;
                                    var ExplicitTrust: Boolean) of object;
  TCertNotAcceptedEvent = procedure (Sender: TObject;
                                     Cert: TASN1Struct;
                                     Status: TCertStatusCode) of object;
  TPolicyEvent = procedure (Sender: TObject;
                            Cert: TASN1Struct;
                            AcceptedPolicy: TStrings;
                            var Accept: Boolean) of object;


  IThreadIdentifier = interface
    function CheckThreadID: Boolean;
  end;

  TPublicKeyAlg = (pkaRSA,pkaDSA,pkaECDSA,pkaDH,pkaECDH,pkaOther);

  TCertificateItem = class(TCollectionItem)
  private
    FCert: TASN1Struct;
    FPublicKeyAlg: TPublicKeyAlg;
    FKeyUsage: TKeyUsage;
    FExtKeyUsage: TStrings;
    FCA: Boolean;
    FPathLenConstraint: Integer;
    FGroupIndex: Integer;
    FCertIndex: Integer;
    FCertId: string;
    FSubject: TMPX501Name;
    FIssuer: TMPX501Name;
    FSerial: string;
    FValidity: TMPX509Validity;
    FAuthorityKeyIdentifier: string;
    FSubjectKeyIdentifier: string;
    procedure SetCert(const Value: TASN1Struct);
    function GetKeyUsage: TKeyUsage;
    procedure SetKeyUsage(const Value: TKeyUsage);
    procedure SetExtKeyUsage(const Value: TStrings);
    procedure SetCA(const Value: Boolean);
    procedure SetPathLenConstraint(const Value: Integer);
    procedure SetGroupIndex(const Value: Integer);
    function GetCertIndex: Integer;
    procedure SetCertIndex(const Value: Integer);
    procedure SetCertId(const Value: string);
    function GetPublicKeyAlg: TPublicKeyAlg;
    procedure SetPublicKeyAlg(const Value: TPublicKeyAlg);
    procedure SetSubject(const Value: TMPX501Name);
    procedure SetIssuer(const Value: TMPX501Name);
    procedure SetSerial(const Value: string);
    procedure SetValidity(const Value: TMPX509Validity);
    procedure SetAuthorityKeyIdentifier(const Value: string);
    procedure SetSubjectKeyIdentifier(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
    procedure Loaded;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Cert: TASN1Struct read FCert write SetCert;
  published                                                                
    property Serial: string read FSerial write SetSerial stored False;
    property Subject: TMPX501Name read FSubject write SetSubject stored False;
    property Issuer: TMPX501Name read FIssuer write SetIssuer stored False;
    property Validity: TMPX509Validity read FValidity write SetValidity stored False;
    property PublicKeyAlg: TPublicKeyAlg read GetPublicKeyAlg write SetPublicKeyAlg stored False;
    property KeyUsage: TKeyUsage read GetKeyUsage write SetKeyUsage stored False;
    property ExtKeyUsage: TStrings read FExtKeyUsage write SetExtKeyUsage stored False;
    property BasicCA: Boolean read FCA write SetCA stored False;
    property BasicPathLenConstraint: Integer read FPathLenConstraint write SetPathLenConstraint stored False;
    property SubjectKeyIdentifier: string read FSubjectKeyIdentifier write SetSubjectKeyIdentifier;
    property AuthorityKeyIdentifier: string read FAuthorityKeyIdentifier write SetAuthorityKeyIdentifier;
    property CertIndex: Integer read GetCertIndex write SetCertIndex default -1;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property CertId: string read FCertId write SetCertId;
  end;

  TX509TrustedCertificates = class;

  TCertificateCollection = class(TOwnedCollection)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetAttrCount: Integer; override;
    function GetAttr(Index: Integer): string; override;
    function GetItemAttr(Index, ItemIndex: Integer): string; override;
    procedure Loaded;
    procedure SetItemName(Item: TCollectionItem); override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TX509TrustedCertificates);
  end;

  TCustomX509TrustedCertificates = class(TComponent)
  private
    FCertSource: TCustomX509TrustedCertificates;
    FGroupIndex: Integer;
    procedure SetCertSource(const Value: TCustomX509TrustedCertificates);
    procedure SetGroupIndex(const Value: Integer);
  protected
    FHijackers: TList;
    procedure AddHijacker(AHijacker: TCustomX509TrustedCertificates);
    procedure CertListLoaded; virtual; abstract;
    procedure Clear; virtual; abstract; // Called by ClearHijacker
    function ClearHijacker(AHijacker: TCustomX509TrustedCertificates): Integer;
    function GetCertificateCollectionCount: Integer; virtual;
    function GetCertificateCollections(
      Index: Integer): TCertificateCollection; virtual;
    procedure SetCertificateCollections(Index: Integer;
      const Value: TCertificateCollection); virtual;
    function IsHijacker(ASource: TCustomX509TrustedCertificates): Boolean;
    function Hijacks(ASource: TCustomX509TrustedCertificates): Boolean; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RemoveHijacker(AHijacker: TCustomX509TrustedCertificates);
    procedure UpdateHijacker(AHijacker: TCustomX509TrustedCertificates); virtual; abstract;
    procedure UpdateHijackers;
    property CertSource: TCustomX509TrustedCertificates read FCertSource write SetCertSource;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddCertificate(const Cert: TASN1Struct;
                            ExplicitTrust: Boolean;
                            var Status: TCertStatusCode;
                            AllowExpired: Boolean = False): Integer; virtual; abstract;
    function AddCRL(const CRL: TASN1Struct;
                    var Status: TCertStatusCode): Integer; virtual; abstract;
    function FindCRLs(const CACert: TASN1Struct;
                      var CRLs: TASN1Struct): Integer; virtual; abstract;
    function FindRevocation(const Cert: TASN1Struct;
                            var RC: TX509RevokedCertificate): Boolean; virtual; abstract;                
    function FindSMIMECapabilities(Cert: TASN1Struct;
                                   var Capabilities: PASN1Struct): Boolean; virtual; abstract;
    function RevokeCertificate(const Cert: TASN1Struct;
                               Reason: CRLReason;
                               HoldInstruction: string;
                               InvalidityDate: TDateTime;
                               NextCRLUpdate: TDateTime;
                               PrivKey: IMPPrivateKey = nil;
                               CACert: PASN1Struct = nil): Boolean; overload; virtual; abstract;
    property CertificateCollectionCount: Integer read GetCertificateCollectionCount;
    property CertificateCollections[Index: Integer]: TCertificateCollection read GetCertificateCollections write SetCertificateCollections;
  end;

  TX509TrustedCertificates = class(TCustomX509TrustedCertificates, IThreadIdentifier)
  private
    FCerts: TASN1Struct;
    FModified: Boolean;
    FHoursOffsetFromGMT: Double;
    FTrustedCACertificates: TX509TrustedCertificates;
    FLeastKeyBitSize: Integer;
    FOnCertNotTrusted: TCertNotTrustedEvent;
    FLock: TCriticalSection;
    FOnCertPolicy: TPolicyEvent;
    FRequiredPolicies: TStrings;
    FAcceptedPolicies: TStrings;
    FOnCertNotAccepted: TCertNotAcceptedEvent;
    FSCLFileName: TFileName;
    FSCLFile: TResourceFile;
    FCertList: TCertificateCollection;
    FBeforeImportTLSCert: TBeforeImportTLSCertEvent;
    function GetCerts(index: Integer): TASN1Struct;
    function GetCount: Integer;
    procedure SetCerts(index: Integer; const Value: TASN1Struct);
    function GetSignatureCert: TASN1Struct;
    function GetCRLCount: Integer;
    function GetCRLs(index: Integer): TASN1Struct;
    function GetPCerts(index: Integer): PASN1Struct;
    procedure SetCRLs(index: Integer; const Value: TASN1Struct);
    procedure SetHoursOffsetFromGMT(const Value: Double);
    procedure SetTrustedCACertificates(
      const Value: TX509TrustedCertificates);
    function GetPCRLs(index: Integer): PASN1Struct;
    procedure SetSignatureCert(const Value: TASN1Struct);
    procedure SetLeastKeyBitSize(const Value: Integer);
    procedure SetOnCertNotTrusted(const Value: TCertNotTrustedEvent);
    procedure SetOnCertPolicy(const Value: TPolicyEvent);
    procedure SetRequiredPolicies(const Value: TStrings);
    procedure SetAcceptedPolicies(const Value: TStrings);
    procedure SetOnCertNotAccepted(const Value: TCertNotAcceptedEvent);
    procedure SetSCLFile(const Value: TResourceFile);
    procedure SetSCLFileName(const Value: TFileName);
    procedure SetCertList(const Value: TCertificateCollection);
    procedure SetBeforeImportTLSCert(
      const Value: TBeforeImportTLSCertEvent);
  protected
    FCreationThreadID: LongWord;
    FIndex: TStrings;
    FSubjectIndex: TStrings;
    procedure AddToIndex(Index: Integer);
    procedure CertListLoaded; override;
    function CheckPolicies(const Cert: TASN1Struct;
                           PolicyMappings: TStrings;
                           AcceptedPolicies: TStrings): Boolean;
    function CheckThreadID: Boolean;
    procedure DoLoadSCLFile(Sender: TObject);
    function GetCertificateCollectionCount: Integer; override;
    function GetCertificateCollections(
      Index: Integer): TCertificateCollection; override;
    procedure SetCertificateCollections(Index: Integer;
      const Value: TCertificateCollection); override;
    procedure InitStructure;
    function InternalCheckSignatureCert(const Cert, CACert: TASN1Struct): Boolean;
    function InternalVerifyCertificate(const Cert: TASN1Struct;
                                       ExplicitTrust: Boolean;
                                       var Status: TCertStatusCode;
                                       AllowExpired: Boolean = False): Integer; 
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DeleteIndex(Index: Integer);
    procedure UpdateHijacker(AHijacker: TCustomX509TrustedCertificates); override;
    procedure UpdatePolicies;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddCertificate(const Cert: TASN1Struct;
                            ExplicitTrust: Boolean;
                            var Status: TCertStatusCode;
                            AllowExpired: Boolean = False): Integer; override;
    function AddCRL(const CRL: TASN1Struct;
                    var Status: TCertStatusCode): Integer; override;
    function AddSMIMECapabilities(Cert, Capabilities: TASN1Struct): Boolean;
    procedure Clear; override;
    function DeleteCertificate(index: Integer): Boolean;
    procedure DSASignSaveToFile(AFileName: TFileName;
                                const MyCert: TASN1Struct;
                                const MyPrivKey: TDLPrivateKey);
    procedure DSASignSaveToStream(AStream: TStream;
                                  const MyCert: TASN1Struct;
                                  const MyPrivKey: TDLPrivateKey);
    procedure ECDSASignSaveToFile(AFileName: TFileName;
                                  const MyCert: TASN1Struct;
                                  const MyPrivKey: TECPrivateKey);
    procedure ECDSASignSaveToStream(AStream: TStream;
                                    const MyCert: TASN1Struct;
                                    const MyPrivKey: TECPrivateKey);
    procedure ExportAllToCMS(var ContentInfo: TASN1Struct;
                             NoSyntaxCheck: Boolean = False);
    procedure ExportAllToCMSFile(AFileName: TFileName);
    procedure ExportAllToCMSStream(AStream: TStream);
    procedure ExportChainToCMS(var ContentInfo: TASN1Struct;
                               EndIndex: Integer;
                               Sort: Boolean;
                               IncludeRoot: Boolean);
    procedure ExportChainToCMSFile(AFileName: TFileName;
                                   EndIndex: Integer;
                                   Sort: Boolean;
                                   IncludeRoot: Boolean);
    procedure ExportChainToCMSStream(AStream: TStream;
                                     EndIndex: Integer;
                                     Sort: Boolean;
                                     IncludeRoot: Boolean);  
{$IFDEF SHA1_AND_MD5}
    procedure ExportChainToTLS(var Certificate: PTLSHandshake;
                               var Len: Integer;
                               EndIndex: Integer); overload;
    function ExportChainToTLS(var Certificate: PTLSHandshake;
                              var Len: Integer;
                              var Cert: PASN1Struct;
                              CertTypes: TCertificateTypes;
                              Issuers: TX501Names): Boolean; overload;
{$ENDIF SHA1_AND_MD5}
    function ExtractConstraintsFromChain(CACert: TASN1Struct;
                                         BC: TBasicConstraintsSyntax;
                                         NC: TNameConstraintsSyntax;
                                         PolicyMappings: TStrings;
                                         Policies: TStrings;
                                         var RequirePolicy,
                                             AllowMapping: Boolean): Boolean;
    function ExtractSubjectDHPublicKey(const Cert: TASN1Struct;
                                       var PublKey: TDLPublicKey): Boolean;
    function ExtractSubjectDSAPublicKey(const Cert: TASN1Struct;
                                        var PublKey: TDLPublicKey): Boolean;
    function ExtractSubjectECPublicKey(const Cert: TASN1Struct;
                                       var PublKey: TECPublicKey): Boolean;
    function FindCACert(const Cert: TASN1Struct;
                        var CACert: PASN1Struct): Boolean;
    function FindCert(const KeyIdentifier: string;
                      var Cert: PASN1Struct): Boolean; overload;
    function FindCert(const Serial: string;
                      const Issuer: TX501Name;
                      var Cert: PASN1Struct): Boolean; overload;    
    function FindCert(const Serial: string;
                      const Issuer: TASN1Struct;
                      var Cert: PASN1Struct): Boolean; overload;
    function FindCert(const SignatureAlgorithms: array of string;
                      const PKAlgorithms: array of string;
                      const KeyUsage: TKeyUsage;
                      var StartIndex: Integer;
                      var Cert: PASN1Struct): Boolean; overload;
    function FindCert(const SignatureAlgorithms: array of string;
                      const PKAlgorithms: array of string;
                      const KeyUsage: TKeyUsage;
                      const ExtKeyUsage: array of string;
                      var StartIndex: Integer;
                      var Cert: PASN1Struct;
                      ReverseSearch: Boolean = False): Boolean; overload;
    function FindCRL(const CACert: TASN1Struct;
                     const issuingDistPoint: TX509issuingDistributionPoint;
                     var CRL: PASN1Struct): Boolean; overload;  
    function FindCRL(const CACert: TASN1Struct;
                     const Reasons: TX509ReasonFlags;
                     var CRL: PASN1Struct): Boolean; overload;
    function FindCRLCACert(const CRL: TASN1Struct;
                           var CACert: PASN1Struct): Boolean;
    function FindCRLs(const CACert: TASN1Struct;
                      var CRLs: TASN1Struct): Integer; override;
    function FindRevocation(const Cert: TASN1Struct;
                            var RC: TX509RevokedCertificate): Boolean; override;
    function FindServerCert(const SignatureAlgorithms: array of string;
                            const PKAlgorithms: array of string;
                            const KeyUsage: TKeyUsage; 
                            const URI: string;
                            const IPAddress: string;
                            const DNSName: string;
                            var StartIndex: Integer; 
                            var Cert: PASN1Struct): Boolean;                
    function FindSMIMECapabilities(Cert: TASN1Struct;
                                   var Capabilities: PASN1Struct): Boolean; override;
{$IFDEF SHA1_AND_MD5}
    function ImportChainFromTLS(Certificate: PCertificate;
                                Len: Integer;
                                var EndCert: PASN1Struct;
                                var Status: TCertStatusCodes): Boolean;
{$ENDIF SHA1_AND_MD5}
    function ImportFromCMS(var ContentInfo: TASN1Struct;
                           ExplicitTrust: Boolean;
                           var Status: TCertStatusCodes;
                           AllowExpired: Boolean = False): Boolean;
    function ImportFromCMSFile(AFileName: TFileName;
                               ExplicitTrust: Boolean;
                               var Status: TCertStatusCodes;
                               AllowExpired: Boolean = False): Boolean;
    function ImportFromCMSStream(AStream: TStream;
                                 ExplicitTrust: Boolean;
                                 var Status: TCertStatusCodes;
                                 AllowExpired: Boolean = False): Boolean;
    function IndexOfCert(const Cert: TASN1Struct): Integer;
    function IndexOfCRL(const CRL: TASN1Struct): Integer;
    function IsRevoked(const Cert: TASN1Struct): Boolean;
    procedure LoadFromFile(AFileName: TFileName);
    procedure LoadFromStream(AStream: TStream);
    function NextSerialNumber(const CACert: TASN1Struct): string;
    function RemoveCertificate(const Cert: TASN1Struct): Boolean;
    function RevokeCertificate(const Cert: TASN1Struct;
                               Reason: CRLReason;
                               HoldInstruction: string;
                               InvalidityDate: TDateTime;
                               NextCRLUpdate: TDateTime;
                               const PrivKey: TIFPrivateKey): Boolean; overload;
    function RevokeCertificate(const Cert: TASN1Struct;
                               Reason: CRLReason;
                               HoldInstruction: string;
                               InvalidityDate: TDateTime;
                               NextCRLUpdate: TDateTime;
                               const PrivKey: TDLPrivateKey): Boolean; overload;
    function RevokeCertificate(const Cert: TASN1Struct;
                               Reason: CRLReason;
                               HoldInstruction: string;
                               InvalidityDate: TDateTime;
                               NextCRLUpdate: TDateTime;
                               PrivKey: IMPPrivateKey;
                               CACert: PASN1Struct = nil): Boolean; overload; override;
    procedure RSASignSaveToFile(AFileName: TFileName;
                                const MyCert: TASN1Struct;
                                const MyPrivKey: TIFPrivateKey);
    procedure RSASignSaveToStream(AStream: TStream;
                                  const MyCert: TASN1Struct;
                                  const MyPrivKey: TIFPrivateKey);
    procedure SaveToFile(AFileName: TFileName);
    procedure SaveToStream(AStream: TStream);
    procedure SignSaveToFile(AFileName: TFileName;
                             const MyCert: TASN1Struct;
                             MyPrivKey: IMPPrivateKey);
    procedure SignSaveToStream(AStream: TStream;
                               const MyCert: TASN1Struct;
                               MyPrivKey: IMPPrivateKey);
    function ValidateMyCert(const MyPrivKey: TDLPrivateKey): Boolean; overload;
    function ValidateMyCert(const MyPrivKey: TIFPrivateKey): Boolean; overload;
    function VerifyCAChain(const Cert: TASN1Struct;
                           var Code: TCertStatusCode;
                           PolicyMappings: TStrings = nil;
                           AcceptedPolicies: TStrings = nil;
                           NC: TNameConstraintsSyntax = nil;
                           AllowExpired: Boolean = False): Boolean; overload;
    function VerifyCAChain(const Cert, TopCert: TASN1Struct;
                           var Code: TCertStatusCode;
                           PolicyMappings: TStrings = nil;
                           AcceptedPolicies: TStrings = nil;
                           NC: TNameConstraintsSyntax = nil;
                           AllowExpired: Boolean = False): Boolean; overload;
    function VerifyCertificate(const Cert: TASN1Struct;
                               ExplicitTrust: Boolean;
                               var Status: TCertStatusCode): Integer;
    property Count: Integer read GetCount;
    property CRLCount: Integer read GetCRLCount;
    property Certs[index: Integer]: TASN1Struct read GetCerts write SetCerts;
    property CRLs[index: Integer]: TASN1Struct read GetCRLs write SetCRLs;
    property Modified: Boolean read FModified;
    property SignatureCert: TASN1Struct read GetSignatureCert
                                        write SetSignatureCert;
  published
    property AcceptedPolicies: TStrings read FAcceptedPolicies write SetAcceptedPolicies;
    property CertList: TCertificateCollection read FCertList write SetCertList;
    property CertSource;
    property GroupIndex default -1;
    property HoursOffsetFromGMT: Double read FHoursOffsetFromGMT write SetHoursOffsetFromGMT{$IFDEF MSWINDOWS} stored False{$ENDIF};
    property LeastKeyBitSize: Integer read FLeastKeyBitSize write SetLeastKeyBitSize default 1024;
    property RequiredPolicies: TStrings read FRequiredPolicies write SetRequiredPolicies;
    property SCLFile: TResourceFile read FSCLFile write SetSCLFile;
    property SCLFileName: TFileName read FSCLFileName write SetSCLFileName;
    property TrustedCACertificates: TX509TrustedCertificates read FTrustedCACertificates write SetTrustedCACertificates;
    property BeforeImportTLSCert: TBeforeImportTLSCertEvent read FBeforeImportTLSCert write SetBeforeImportTLSCert;
    property OnCertNotTrusted: TCertNotTrustedEvent read FOnCertNotTrusted write SetOnCertNotTrusted;
    property OnCertNotAccepted: TCertNotAcceptedEvent read FOnCertNotAccepted write SetOnCertNotAccepted;
    property OnCertPolicy: TPolicyEvent read FOnCertPolicy write SetOnCertPolicy;
  end;

  TX509Certificate = class(TComponent, IThreadIdentifier)
  private
    FCert: TASN1Struct;
    FIssuer: TMPX501Name;
    FSubject: TMPX501Name;
    FFileName: TFileName;
    FAutoLoad: Boolean;
    FSubjectAltName: TMPX509GeneralNames;
    FIssuerAltName: TMPX509GeneralNames;
    FSignature: TMPX509Algorithm;
    FASN1Struct: TMPASN1Structs;
    FSubjectPublicKey: TMPX509Algorithm;
    FPublicKey: string;
    FExtensions: TMPX509Extensions;
    FKeyUsage: TKeyUsage;
    FExtendedKeyUsage: TStrings;
    FValidity: TMPX509Validity;
    FTrustedCertificates: TX509TrustedCertificates;
    procedure SetIssuer(const Value: TMPX501Name);
    procedure SetSubject(const Value: TMPX501Name);
    procedure SetFileName(const Value: TFileName);
    procedure SetAutoLoad(const Value: Boolean);
    procedure SetIssuerAltName(const Value: TMPX509GeneralNames);
    procedure SetSubjectAltName(const Value: TMPX509GeneralNames);
    function GetSerial: string;
    procedure SetSerial(const Value: string);
    procedure SetSignature(const Value: TMPX509Algorithm);
    procedure SetASN1Struct(const Value: TMPASN1Structs);
    function GetSignParams: TMPASN1Structs;
    procedure SetSignParams(const Value: TMPASN1Structs);
    procedure SetSignatureValue(const Value: string);
    function GetSignatureValue: string;
    function GetPublicKey: string;
    function GetSPKParams: TMPASN1Structs;
    procedure SetPublicKey(const Value: string);
    procedure SetSPKParams(const Value: TMPASN1Structs);
    procedure SetSubjectPublicKey(const Value: TMPX509Algorithm);
    procedure SetExtensions(const Value: TMPX509Extensions);
    function GetKeyUsage: TKeyUsage;
    procedure SetExtendedKeyUsage(const Value: TStrings);
    procedure SetKeyUsage(const Value: TKeyUsage);
    procedure SetCert(const Value: TASN1Struct);
    procedure SetValidity(const Value: TMPX509Validity);
    procedure SetTrustedCertificates(
      const Value: TX509TrustedCertificates);
  protected
    FCreationThreadID: LongWord;
    function CheckThreadID: Boolean;
    procedure DataFromStream(AStream: TStream);
    procedure DataToStream(AStream: TStream);
    procedure DefineProperties(Filer: TFiler); override;
    procedure Interpret;
    function NoLoad: Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CheckExtendedKeyUsage(const UID: string): Boolean;
    procedure ComposeCertificate(const CACert: TASN1Struct);
    procedure ComposeCACertificate(const CRLDistributionPoints: TX509CRLDistPoints;
                                   PathLenConstraint: Cardinal);
    procedure ComposeCertRequest(var CertReq: TASN1Struct;
                                 const ChallengePassword: string);
    procedure ComposeCACertRequest(var CertReq: TASN1Struct;
                                   const ChallengePassword: string;
                                   const CRLDistributionPoints: TX509CRLDistPoints;
                                   PathLenConstraint: Cardinal);

    procedure DSASignCertificate(const CACert: TASN1Struct;
                                 const PrivKey: TDLPrivateKey);
    procedure DSASignCACertificate(const PrivKey: TDLPrivateKey);
    procedure ExtractCRLDistributionPoints(var DPs: TX509CRLDistPoints);
    procedure ImposeExtension(const Ext: TX509Extension);
    procedure ImposePublicKey(const PK: TX509PublicKey);
    procedure LoadFromFile(AFileName: TFileName; Format: Byte = fmtDER);
    procedure LoadFromStream(AStream: TStream; Format: Byte = fmtDER);
    procedure RSASignCertificate(const CACert: TASN1Struct;
                                 const PrivKey: TIFPrivateKey;
                                 HashAlgorithm: THashAlgorithm;
                                 MGFHashAlgorithm: THashAlgorithm;
                                 Encoding: TSignEncoding);
    procedure RSASignCACertificate(const PrivKey: TIFPrivateKey;
                                   HashAlgorithm: THashAlgorithm;
                                   MGFHashAlgorithm: THashAlgorithm;
                                   Encoding: TSignEncoding);
    procedure SaveToFile(AFileName: TFileName; Format: Byte = fmtDER);
    procedure SaveToStream(AStream: TStream; Format: Byte = fmtDER);
    function VerifyCertificate(var Status: TCertStatusCode): Boolean;
    property Cert: TASN1Struct read FCert write SetCert;
  published
    property ASN1Struct: TMPASN1Structs read FASN1Struct write SetASN1Struct stored False;
    property AutoLoad: Boolean read FAutoLoad write SetAutoLoad;
    property ExtendedKeyUsage: TStrings read FExtendedKeyUsage write SetExtendedKeyUsage;
    property Extensions: TMPX509Extensions read FExtensions write SetExtensions stored False;
    property FileName: TFileName read FFileName write SetFileName;
    property Issuer: TMPX501Name read FIssuer write SetIssuer stored NoLoad;
    property IssuerAltName: TMPX509GeneralNames read FIssuerAltName write SetIssuerAltName stored NoLoad;
    property KeyUsage: TKeyUsage read GetKeyUsage write SetKeyUsage stored NoLoad;
    property Serial: string read GetSerial write SetSerial stored False;
    property Signature: TMPX509Algorithm read FSignature write SetSignature stored NoLoad;
    property SignatureParameters: TMPASN1Structs read GetSignParams write SetSignParams stored False;
    property SignatureValue: string read GetSignatureValue write SetSignatureValue stored False;
    property Subject: TMPX501Name read FSubject write SetSubject stored NoLoad;
    property SubjectAltName: TMPX509GeneralNames read FSubjectAltName write SetSubjectAltName stored NoLoad;
    property SubjectPublicKey: TMPX509Algorithm read FSubjectPublicKey write SetSubjectPublicKey stored NoLoad;
    property SubjectPublicKeyParams: TMPASN1Structs read GetSPKParams write SetSPKParams stored False;
    property SubjectPublicKeyValue: string read GetPublicKey write SetPublicKey stored NoLoad;
    property TrustedCertificates: TX509TrustedCertificates read FTrustedCertificates write SetTrustedCertificates;
    property Validity: TMPX509Validity read FValidity write SetValidity stored NoLoad;
  end;

{$IFNDEF D5UP}
{ Interface support routines }
function Supports(const Instance: IUnknown; const IID: TGUID; out Intf): Boolean; overload;
function Supports(const Instance: TObject; const IID: TGUID; out Intf): Boolean; overload;
{$ENDIF}
{$IFNDEF D6UP}
function Supports(const Instance: IUnknown; const IID: TGUID): Boolean; overload;
function Supports(const Instance: TObject; const IID: TGUID): Boolean; overload;
function Supports(const AClass: TClass; const IID: TGUID): Boolean; overload;
{$ENDIF}

{$IFDEF MSWINDOWS}
function OffsetFromUTC: Double;
{$ENDIF}

{$IFNDEF INIT_SECTIONS}
procedure Initialize;
procedure Finalize;
{$ENDIF}

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF}
  SsBase64, ReadStrm, MpArithTypes, MpArith, Pkcs10, Cms, MpECArith;

{$IFNDEF D5UP}
{ Interface support routines }

function Supports(const Instance: IUnknown; const IID: TGUID; out Intf): Boolean;
begin
  Result := (Instance <> nil) and (Instance.QueryInterface(IID, Intf) = 0);
end;

function Supports(const Instance: TObject; const IID: TGUID; out Intf): Boolean;
var
  LUnknown: IUnknown;
begin
  Result := (Instance <> nil) and
            ((Instance.GetInterface(IUnknown, LUnknown) and Supports(LUnknown, IID, Intf)) or
             Instance.GetInterface(IID, Intf));
end;
{$ENDIF}

{$IFNDEF D6UP}
function Supports(const Instance: IUnknown; const IID: TGUID): Boolean;
var
  Temp: IUnknown;
begin
  Result := Supports(Instance, IID, Temp);
end;

function Supports(const Instance: TObject; const IID: TGUID): Boolean;
var
  Temp: IUnknown;
begin
  Result := Supports(Instance, IID, Temp);
end;

function Supports(const AClass: TClass; const IID: TGUID): Boolean;
begin
  Result := AClass.GetInterfaceEntry(IID) <> nil;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function OffsetFromUTC: Double;
var
  Bias: Integer;
  TZI: TTimeZoneInformation;
begin
  Bias := 0;
  case GetTimeZoneInformation(TZI) of
    TIME_ZONE_ID_UNKNOWN:
      Bias := TZI.Bias;
    TIME_ZONE_ID_DAYLIGHT:
      Bias := TZI.Bias + TZI.DaylightBias;
    TIME_ZONE_ID_STANDARD:
      Bias := TZI.Bias + TZI.StandardBias;
  end;
  Result := -(Bias / 60);
end;
{$ENDIF}

type
  TCertStore = class(TSigned)
  protected
    function BeforeSign(var CACert: TCertificate;
                        SignAlg: ObjectIdentifier;
                        out Params: TASNCustomWrapper): Boolean; override;
    function CheckSignAlgCoherence: Boolean; override;
    function IsAllegedIssuer(CACert: TCertificate): Boolean; override;
  public
   constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;
   destructor Destroy; override;
  end;                                                    

const
  crlf = #13#10;
  ASNModule =
    'CertStore DEFINITIONS ::=' + crlf +
    'BEGIN' + crlf + crlf +
    'IMPORTS AlgorithmIdentifier FROM PKIX-Cert;' + crlf + crlf +
    'CertStore ::= SEQUENCE {' + crlf +
    '     certList'#9'ANY,' + crlf +
    '     signatureAlgorithm'#9'AlgorithmIdentifier,' + crlf +
    '     signature'#9'BIT STRING}' + crlf + crlf +
    'END';

var
  GlobalObject: TASN1Struct;

procedure InitGlobalObject;
var
  SS: TStringStream;
begin
  Assert(ASNModule <> '');
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
  if GlobalObject = nil then
    InitGlobalObject;
end;

procedure Finalize;
begin
  if Assigned(GlobalObject) then
    ASN1.ReleaseASN1Struct(GlobalObject);
end;

{ TMPX501Name }

procedure TMPX501Name.Assign(Source: TPersistent);
begin
  if Source is TMPX501Name then begin
    FData := TMPX501Name(Source).FData;
    ChangeOwner;
  end else
    inherited Assign(Source);
end;

procedure TMPX501Name.ChangeOwner;
begin
  if FOwner is TMPX509ediPartyName then
    TMPX509ediPartyName(FOwner).ChangeOwner
  else if FOwner is TMPX509GeneralName then
    TMPX509GeneralName(FOwner).Change(4);
end;

procedure TMPX501Name.Clear;
begin
  System.Finalize(FData);
end;

constructor TMPX501Name.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  FData.commonName.Tag := V_ASN1_UTF8STRING;
  FData.surname.Tag := V_ASN1_UTF8STRING;
  FData.countryName.Tag := V_ASN1_UTF8STRING;
  FData.localityName.Tag := V_ASN1_UTF8STRING;
  FData.stateOrProvinceName.Tag := V_ASN1_UTF8STRING;
  FData.organizationName.Tag := V_ASN1_UTF8STRING;
  FData.organizationalUnitName.Tag := V_ASN1_UTF8STRING;
  FData.title.Tag := V_ASN1_UTF8STRING;
  FData.name.Tag := V_ASN1_UTF8STRING;
  FData.givenName.Tag := V_ASN1_UTF8STRING;
  FData.initials.Tag := V_ASN1_UTF8STRING;
  FData.generationQualifier.Tag := V_ASN1_UTF8STRING;
  FData.dnQualifier.Tag := V_ASN1_UTF8STRING;
  FData.emailAddress.Tag := V_ASN1_UTF8STRING;
end;

destructor TMPX501Name.Destroy;
begin

  inherited;
end;

function TMPX501Name.GetcommonName: WideString;
begin
  Result := FData.commonName.Str;
end;

function TMPX501Name.GetcountryName: WideString;
begin
  Result := FData.countryName.Str;
end;

function TMPX501Name.GetdnQualifier: WideString;
begin
  Result := FData.dnQualifier.Str;
end;

function TMPX501Name.GetemailAddress: WideString;
begin
  Result := FData.emailAddress.Str;
end;

function TMPX501Name.GetgenerationQualifier: WideString;
begin
  Result := FData.generationQualifier.Str;
end;

function TMPX501Name.GetgivenName: WideString;
begin
  Result := FData.givenName.Str;
end;

function TMPX501Name.Getinitials: WideString;
begin
  Result := FData.initials.Str;
end;

function TMPX501Name.GetlocalityName: WideString;
begin
  Result := FData.localityName.Str;
end;

function TMPX501Name.Getname: WideString;
begin
  Result := FData.name.Str;
end;

function TMPX501Name.GetorganizationalUnitName: WideString;
begin
  Result := FData.organizationalUnitName.Str;
end;

function TMPX501Name.GetorganizationName: WideString;
begin
  Result := FData.organizationName.Str;
end;

function TMPX501Name.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TMPX501Name.GetstateOrProvinceName: WideString;
begin
  Result := FData.stateOrProvinceName.Str;
end;

function TMPX501Name.Getsurname: WideString;
begin
  Result := FData.surname.Str;
end;

function TMPX501Name.Gettitle: WideString;
begin
  Result := FData.title.Str;
end;

procedure TMPX501Name.SetcommonName(const Value: WideString);
begin
  if not ReadOnly then begin
    FData.commonName.Str := Value;
    if Value <> '' then ChangeOwner;
  end;
end;

procedure TMPX501Name.SetcountryName(const Value: WideString);
begin
  if not ReadOnly then begin
    FData.countryName.Str := Value;
    if Value <> '' then ChangeOwner;
  end;
end;

procedure TMPX501Name.SetdnQualifier(const Value: WideString);
begin
  if not ReadOnly then begin
    FData.dnQualifier.Str := Value;
    if Value <> '' then ChangeOwner;
  end;
end;

procedure TMPX501Name.SetemailAddress(const Value: WideString);
begin
  if not ReadOnly then begin
    FData.emailAddress.Str := Value;
    if Value <> '' then ChangeOwner;
  end;
end;

procedure TMPX501Name.SetgenerationQualifier(const Value: WideString);
begin
  if not ReadOnly then begin
    FData.generationQualifier.Str := Value;
    if Value <> '' then ChangeOwner;
  end;
end;

procedure TMPX501Name.SetgivenName(const Value: WideString);
begin
  if not ReadOnly then begin
    FData.givenName.Str := Value;
    if Value <> '' then ChangeOwner;
  end;
end;

procedure TMPX501Name.Setinitials(const Value: WideString);
begin
  if not ReadOnly then begin
    FData.initials.Str := Value;
    if Value <> '' then ChangeOwner;
  end;
end;

procedure TMPX501Name.SetlocalityName(const Value: WideString);
begin
  if not ReadOnly then begin
    FData.localityName.Str := Value;
    if Value <> '' then ChangeOwner;
  end;
end;

procedure TMPX501Name.Setname(const Value: WideString);
begin
  if not ReadOnly then begin
    FData.name.Str := Value;
    if Value <> '' then ChangeOwner;
  end;
end;

procedure TMPX501Name.SetorganizationalUnitName(const Value: WideString);
begin
  if not ReadOnly then begin
    FData.organizationalUnitName.Str := Value;
    if Value <> '' then ChangeOwner;
  end;
end;

procedure TMPX501Name.SetorganizationName(const Value: WideString);
begin
  if not ReadOnly then begin
    FData.organizationName.Str := Value;
    if Value <> '' then ChangeOwner;
  end;
end;

procedure TMPX501Name.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TMPX501Name.SetstateOrProvinceName(const Value: WideString);
begin
  if not ReadOnly then begin
    FData.stateOrProvinceName.Str := Value;
    if Value <> '' then ChangeOwner;
  end;
end;

procedure TMPX501Name.Setsurname(const Value: WideString);
begin
  if not ReadOnly then begin
    FData.surname.Str := Value;
    if Value <> '' then ChangeOwner;
  end;
end;

procedure TMPX501Name.Settitle(const Value: WideString);
begin
  if not ReadOnly then begin
    FData.title.Str := Value;
    if Value <> '' then ChangeOwner;
  end;
end;

{ TMPX509ediPartyName }

procedure TMPX509ediPartyName.Assign(Source: TPersistent);
begin
  if Source = Self then
    Exit;
  if Source is TMPX509ediPartyName then begin
    nameAssigner := TMPX509ediPartyName(Source).nameAssigner;
    partyName := TMPX509ediPartyName(Source).partyName;
  end else
    inherited Assign(Source);
end;

procedure TMPX509ediPartyName.ChangeOwner;
begin
  FOwner.Change(5);
end;

procedure TMPX509ediPartyName.Clear;
begin
  System.Finalize(FnameAssigner.FData);
  System.Finalize(FpartyName.FData);
end;

constructor TMPX509ediPartyName.Create(AOwner: TMPX509GeneralName);
begin
  FOwner := AOwner;
  FnameAssigner := TMPX501Name.Create(Self);
  FpartyName := TMPX501Name.Create(Self);
end;

destructor TMPX509ediPartyName.Destroy;
begin
  FnameAssigner.Free;
  FpartyName.Free;
  inherited;
end;

function TMPX509ediPartyName.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TMPX509ediPartyName.SetnameAssigner(const Value: TMPX501Name);
begin
  FnameAssigner.Assign(Value);
end;

procedure TMPX509ediPartyName.SetpartyName(const Value: TMPX501Name);
begin
  FpartyName.Assign(Value);
end;

{ TMPX509GeneralName }

procedure TMPX509GeneralName.Assign(Source: TPersistent);
begin
  if Source is TMPX509GeneralName then
    SetData(TMPX509GeneralName(Source).FData)
  else
    inherited Assign(Source);
end;

procedure TMPX509GeneralName.Change(NewTag: Integer);
begin
  FData.Tag := Byte(NewTag);
  if NewTag = 5 then begin
    FData.ediPartyName.nameAssigner := FediPartyName.FnameAssigner.FData;
    FData.ediPartyName.partyName := FediPartyName.FpartyName.FData;
  end else if NewTag = 4 then
    FData.directoryName := FdirectoryName.FData;
end;

constructor TMPX509GeneralName.Create(Collection: TCollection);
begin
  inherited;
  FediPartyName := TMPX509ediPartyName.Create(Self);
  FdirectoryName := TMPX501Name.Create(Self);
end;

destructor TMPX509GeneralName.Destroy;
begin
  FediPartyName.Free;
  FdirectoryName.Free;
  inherited;
end;

function TMPX509GeneralName.GetDisplayName: string;
begin
  case Tag of
    1: Result := 'rfc822Name';
    2: Result := 'dNSName';
    4: Result := 'directoryName';
    5: Result := 'ediPartyName';
    6: Result := 'uniformResourceIdentifier';
    7: Result := 'ipAddress';
    8: Result := 'registredID';
  else
    Result := inherited GetDisplayName;
  end;
end;

function TMPX509GeneralName.GetdNSName: string;
begin
  Result := FData.dNSName;
end;

function TMPX509GeneralName.GetipAddress: string;
begin
  Result := IntToStr(FData.ipAddress[0]) + '.' +
            IntToStr(FData.ipAddress[1]) + '.' +
            IntToStr(FData.ipAddress[2]) + '.' +
            IntToStr(FData.ipAddress[3]);
end;

function TMPX509GeneralName.GetipMask: TipMask;
begin
  Result := FData.ipMask;
end;

function TMPX509GeneralName.GetregistredID: string;
begin
  Result := FData.registredID;
end;

function TMPX509GeneralName.Getrfc822Name: string;
begin
  Result := FData.rfc822Name;
end;

function TMPX509GeneralName.GetTag: Integer;
begin
  Result := FData.Tag;
end;

function TMPX509GeneralName.GetuniformResourceIdentifier: string;
begin
  Result := FData.uniformResourceIdentifier;
end;

procedure TMPX509GeneralName.SetData(const Data: TX509GeneralName);
begin
  FData := Data;
  FediPartyName.FnameAssigner.FData := Data.ediPartyName.nameAssigner;
  FediPartyName.FpartyName.FData := Data.ediPartyName.partyName;
  FdirectoryName.FData := Data.directoryName;
end;

procedure TMPX509GeneralName.SetdirectoryName(const Value: TMPX501Name);
begin
  FdirectoryName.Assign(Value);
end;

procedure TMPX509GeneralName.SetdNSName(const Value: string);
begin
  FData.dNSName := Value;
  Change(2);
end;

procedure TMPX509GeneralName.SetediPartyName(
  const Value: TMPX509ediPartyName);
begin
  FediPartyName.Assign(Value);
end;

procedure TMPX509GeneralName.SetipAddress(const Value: string);
var
  I, J: Integer;
  S: string;
begin
  J := 0;
  for I := 1 to Length(Value) do begin
    if Value[I] in ['0'..'9'] then
      S := S + Value[I]
    else if Value[I] = '.' then begin
      FData.ipAddress[J] := StrToInt(S);
      Inc(J);
      S := '';
    end;
    if J = 4 then Break;
  end;
  Change(7);
end;

procedure TMPX509GeneralName.SetipMask(const Value: TipMask);
begin
  FData.ipMask := Value;
  Change(7);
end;

procedure TMPX509GeneralName.SetregistredID(const Value: string);
begin
  FData.registredID := Value;
  Change(8);
end;

procedure TMPX509GeneralName.Setrfc822Name(const Value: string);
begin
  FData.rfc822Name := Value;
  Change(1);
end;

procedure TMPX509GeneralName.SetTag(const Value: Integer);
begin
  FData.Tag := Byte(Value);
  if Tag <> 1 then
    FData.rfc822Name := '';
  if Tag <> 2 then
    FData.dNSName := '';
  if Tag <> 4 then begin
    FdirectoryName.Clear;
    System.Finalize(FData.directoryName);
  end;
  if Tag <> 5 then begin
    FediPartyName.Clear;
    System.Finalize(FData.ediPartyName);
  end;
  if Tag <> 6 then
    FData.uniformResourceIdentifier := '';
  if Tag <> 7 then begin
    FillChar(FData.ipAddress,4,0);
    FData.ipMask := 0;
  end;
  if Tag <> 8 then
    FData.registredID := '';
end;

procedure TMPX509GeneralName.SetuniformResourceIdentifier(
  const Value: string);
begin
  FData.uniformResourceIdentifier := Value;
  Change(6);
end;

{ TMPX509GeneralNames }

procedure TMPX509GeneralNames.Assign(Source: TPersistent);
begin
  if Source is TMPX509GeneralNames then begin
    FData := TMPX509GeneralNames(Source).FData;
    Interpret;
  end else
    inherited Assign(Source);
end;

procedure TMPX509GeneralNames.Compose;
var
  I: Integer;
begin
  SetLength(FData,Count);
  for I := 0 to Count - 1 do
    FData[I] := TMPX509GeneralName(Items[I]).FData;
end;

constructor TMPX509GeneralNames.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner,TMPX509GeneralName);
end;

procedure TMPX509GeneralNames.Interpret;
var
  Item: TMPX509GeneralName;
  I: Integer;
begin
  BeginUpdate;
  try
    Clear;
    for I := 0 to Length(FData) - 1 do begin
      Item := TMPX509GeneralName(Add);
      Item.FData := FData[I];
    end;
  finally
    EndUpdate;
  end;
end;

procedure TMPX509GeneralNames.Update(Item: TCollectionItem);
begin
  if Item = nil then
    Compose
  else
    FData[Item.Index] := TMPX509GeneralName(Item).FData;
end;

{ TX509Certificate }

function TX509Certificate.CheckExtendedKeyUsage(
  const UID: string): Boolean;
var
  Ext: TX509Extension;
  I: Integer;
begin
  FillChar(Ext,SizeOf(Ext),0);
  if ExtractNamedExtension(FCert,id_ce_extKeyUsage,Ext) = E_OK then begin
    Result := False;
    for I := 0 to Ext.extnValue.Contents^.ItemCount - 1 do begin
      Result := Ext.extnValue.Contents^.Items[I].ContentAsOID = UID;
      if Result then
        Break;
    end;
  end else
    Result := False;
  DisposeASN1Struct(Ext.extnValue);
end;

function TX509Certificate.CheckThreadID: Boolean;
var
  CTID: LongWord;
begin
  CTID := GetCurrentThreadID;
  Result := CTID = FCreationThreadID;
  if (FCreationThreadID <> MainThreadID) and not Result then
    raise Exception.Create('TX509Certificate.CheckThreadID: Called from the wrong thread.');
end;

procedure TX509Certificate.ComposeCACertificate(
  const CRLDistributionPoints: TX509CRLDistPoints;
  PathLenConstraint: Cardinal);
var
  PK: TX509PublicKey;
  SS: TStringStream;
  KE: TKeyUsage;
begin
  PK := TX509PublicKey.Create;
  PK.Algorithm.Algorithm := FSubjectPublicKey.FData.Algorithm;
  CopyASN1Struct(PK.Algorithm.Parameters,FSubjectPublicKey.FData.Parameters);
  SS := TStringStream.Create(FPublicKey);
  try
    ASN1FromStream(SS,PK.PublicKey);
  finally
    SS.Free;
  end;
  KE := KeyUsage;
  X509Base.ComposeCACertificate2(FCert,FSubject.FData,PK,FValidity.FData,KE,
                                 FExtendedKeyUsage,FSubjectAltName.FData,
                                 CRLDistributionPoints,PathLenConstraint);
  PK.Free;
end;

procedure TX509Certificate.ComposeCACertRequest(var CertReq: TASN1Struct;
  const ChallengePassword: string;
  const CRLDistributionPoints: TX509CRLDistPoints;
  PathLenConstraint: Cardinal);
var
  PK: TX509PublicKey;
  SS: TStringStream;
  KE: TKeyUsage;
begin
  PK := TX509PublicKey.Create;
  PK.Algorithm.Algorithm := FSubjectPublicKey.FData.Algorithm;
  CopyASN1Struct(PK.Algorithm.Parameters,FSubjectPublicKey.FData.Parameters);
  SS := TStringStream.Create(FPublicKey);
  try
    ASN1FromStream(SS,PK.PublicKey);
  finally
    SS.Free;
  end;
  KE := KeyUsage;
  Pkcs10.ComposeCACertRequest(CertReq,FSubject.FData,PK,ChallengePassword,KE,
                              FExtendedKeyUsage,FSubjectAltName.FData,
                              CRLDistributionPoints,PathLenConstraint);
  PK.Free;
end;

procedure TX509Certificate.ComposeCertificate(const CACert: TASN1Struct);
var
  PK: TX509PublicKey;
  SS: TStringStream;
  KE: TKeyUsage;
begin
  FillChar(PK,SizeOf(PK),0);
  PK.Algorithm.Algorithm := FSubjectPublicKey.FData.Algorithm;
  CopyASN1Struct(PK.Algorithm.Parameters,FSubjectPublicKey.FData.Parameters);
  SS := TStringStream.Create(FPublicKey);
  try
    ASN1FromStream(SS,PK.PublicKey);
  finally
    SS.Free;
  end;
  KE := KeyUsage;
  X509Base.ComposeCertificate(FCert,FSubject.FData,PK,CACert,FValidity.FData,KE,
                              FExtendedKeyUsage,FSubjectAltName.FData);
  DisposeASN1Struct(PK.Algorithm.Parameters);
  DisposeASN1Struct(PK.PublicKey);
end;

procedure TX509Certificate.ComposeCertRequest(var CertReq: TASN1Struct;
                                              const ChallengePassword: string);
var
  PK: TX509PublicKey;
  SS: TStringStream;
  KE: TKeyUsage;
begin                             
  FillChar(PK,SizeOf(PK),0);
  PK.Algorithm.Algorithm := FSubjectPublicKey.FData.Algorithm;
  CopyASN1Struct(PK.Algorithm.Parameters,FSubjectPublicKey.FData.Parameters);
  SS := TStringStream.Create(FPublicKey);
  try
    ASN1FromStream(SS,PK.PublicKey);
  finally
    SS.Free;
  end;
  KE := KeyUsage;                 
  PKCS10.ComposeCertRequest(CertReq,
                            FSubject.FData,
                            PK,
                            ChallengePassword,
                            KE,
                            FExtendedKeyUsage,
                            FSubjectAltName.FData);
  DisposeASN1Struct(PK.Algorithm.Parameters);
  DisposeASN1Struct(PK.PublicKey);
end;

constructor TX509Certificate.Create(AOwner: TComponent);
begin
  FCreationThreadID := GetCurrentThreadID;
  inherited;
  FIssuer := TMPX501Name.Create(Self);
  FSubject := TMPX501Name.Create(Self);
  FSubjectAltName := TMPX509GeneralNames.Create(Self);
  FIssuerAltName := TMPX509GeneralNames.Create(Self);
  FSignature := TMPX509Algorithm.Create(Self);
  FSubjectPublicKey := TMPX509Algorithm.Create(Self);
  FExtensions := TMPX509Extensions.Create(Self);
  FExtensions.FCert := @FCert;
  FExtendedKeyUsage := TStringList.Create;
  FValidity := TMPX509Validity.Create;
  FASN1Struct := TMPASN1Structs.Create(Self);
  NewComposeASN1Struct(FCert,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  FASN1Struct.FData := @FCert;
  FAutoLoad := True;
end;

procedure TX509Certificate.DataFromStream(AStream: TStream);
begin
  LoadFromStream(AStream,fmtDER);
end;

procedure TX509Certificate.DataToStream(AStream: TStream);
begin
  SaveToStream(AStream,fmtDER);
end;

procedure TX509Certificate.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Cert',DataFromStream,DataToStream,NoLoad);
end;

destructor TX509Certificate.Destroy;
begin
  FIssuer.Free;
  FSubject.Free;
  FSubjectAltName.Free;
  FIssuerAltName.Free;
  FSignature.Free;
  FSubjectPublicKey.Free;
  FExtensions.Free;
  FExtendedKeyUsage.Free;
  FValidity.Free;
  FASN1Struct.Free;
  DisposeASN1Struct(FCert);
  inherited;
end;

procedure TX509Certificate.DSASignCACertificate(
  const PrivKey: TDLPrivateKey);
begin
{$IFDEF SHA1}
  X509Base.DSASignCertificate(FCert,FCert,PrivKey);
{$ELSE  SHA1}
  raise Exception.Create('TX509Certificate.DSASignCACertificate: Unsupported algorithm');
{$ENDIF SHA1}
end;

procedure TX509Certificate.DSASignCertificate(const CACert: TASN1Struct;
  const PrivKey: TDLPrivateKey);
begin        
{$IFDEF SHA1}
  X509Base.DSASignCertificate(FCert,CACert,PrivKey);
{$ELSE  SHA1}
  raise Exception.Create('TX509Certificate.DSASignCertificate: Unsupported algorithm');
{$ENDIF SHA1}
end;

procedure TX509Certificate.ExtractCRLDistributionPoints(
  var DPs: TX509CRLDistPoints);
begin
  if X509Base.ExtractCRLDistributionPoints(FCert,DPs) = E_SYNTAX then
    raise Exception.Create('Syntax error');
end;

function TX509Certificate.GetKeyUsage: TKeyUsage;
begin
  Result := FKeyUsage;
end;

function TX509Certificate.GetPublicKey: string;
begin
  Result := OSToHex(FPublicKey);
end;

function TX509Certificate.GetSerial: string;
begin
  ExtractSerial(FCert,Result);
  Result := OSToHex(Result);
end;

function TX509Certificate.GetSignatureValue: string;
begin
  if (FCert.Contents <> nil) and (FCert.Contents^.ItemCount = 3) and
     (FCert.Contents^.Items[2].Tag = V_ASN1_BIT_STRING) and
     not FCert.Contents^.Items[2].Constructed then begin
    SetLength(Result,FCert.Contents^.Items[2].Length);
    Move(FCert.Contents^.Items[2].Content^,Result[1],Length(Result));
    Delete(Result,1,1);
    Result := OSToHex(Result);
  end else
    Result := '(empty)';
end;

function TX509Certificate.GetSignParams: TMPASN1Structs;
begin
  Result := FSignature.Parameters;
end;

function TX509Certificate.GetSPKParams: TMPASN1Structs;
begin
  Result := FSubjectPublicKey.Parameters;
end;

procedure TX509Certificate.ImposeExtension(const Ext: TX509Extension);
begin
  X509Base.ImposeExtension(Ext,FCert);
end;

procedure TX509Certificate.ImposePublicKey(const PK: TX509PublicKey);
var
  SS: TStringStream;
begin
  FSubjectPublicKey.FData.Algorithm := PK.Algorithm.Algorithm;
  FSubjectPublicKey.FData.AlgorithmName := PK.Algorithm.AlgorithmName;
  FSubjectPublicKey.FData.Parameters.Free;
  FSubjectPublicKey.FData.Parameters := nil;
  FSubjectPublicKey.FParameters.FData := nil;
  FSubjectPublicKey.FParameters.Clear;
  CopyASN1Struct(FSubjectPublicKey.FData.Parameters,PK.Algorithm.Parameters);
  FSubjectPublicKey.FParameters.FData := @FSubjectPublicKey.FData.Parameters;
  FSubjectPublicKey.FParameters.Interpret;
  SS := TStringStream.Create('');
  try
    ASN1ToStream(PK.PublicKey,SS);
    FPublicKey := SS.DataString;
  finally
    SS.Free;
  end;
end;

procedure TX509Certificate.Interpret;
var
  Ext: TX509Extension;
  Alg: TX509AlgorithmIdentifier;
  PK: TX509PublicKey;
  SS: TStringStream;
  S: string;
  I: Integer;
begin
  if ExtractIssuer(FCert,FIssuer.FData) <> E_OK then Exit;
  ExtractSubject(FCert,FSubject.FData);
  ExtractValidity(FCert,FValidity.FData);
  FillChar(Ext,SizeOf(Ext),0);
  if ExtractNamedExtension(FCert,id_ce_issuerAltName,Ext) = E_OK then begin
    if InterpretGeneralNames(Ext.extnValue,FIssuerAltName.FData) = E_OK then
      FIssuerAltName.Interpret
    else
      FIssuerAltName.Clear;
  end else
    FIssuerAltName.Clear;
  if ExtractNamedExtension(FCert,id_ce_subjectAltName,Ext) = E_OK then begin
    if InterpretGeneralNames(Ext.extnValue,FSubjectAltName.FData) = E_OK then
      FSubjectAltName.Interpret
    else
      FSubjectAltName.Clear;
  end else
    FSubjectAltName.Clear;
  FExtendedKeyUsage.Clear;
  if ExtractNamedExtension(FCert,id_ce_extKeyUsage,Ext) = E_OK then begin
    for I := 0 to Ext.extnValue.Contents^.ItemCount - 1 do begin
      FExtendedKeyUsage.Add('-- ' + GetObjectName(Ext.extnValue.Contents^.Items[I].ContentAsOID));
      FExtendedKeyUsage.Add(Ext.extnValue.Contents^.Items[I].ContentAsOID);
      FExtendedKeyUsage.Add('');
    end;
  end;
  FKeyUsage := [];
  Ext := nil;
  if ExtractNamedExtension(FCert,id_ce_keyUsage,Ext) = E_OK then
    Move(Ext.extnValue.Content[1],FKeyUsage,Ext.extnValue.Length - 1)
  else
    FKeyUsage := [Low(TKeyUsageItem)..High(TKeyUsageItem)];
  Ext.Free;

  Alg := nil;
  ExtractSignatureAlgorithm(FCert,Alg);
  FSignature.SetData(Alg);
  Alg.Free;
  SetLength(S,FCert.Contents^.Items[2].Length - 1);
  Move(FCert.Contents^.Items[2].Content[1],S[1],Length(S));

  PK := nil;
  ExtractSubjectPublicKey(FCert,PK);
  FSubjectPublicKey.SetData(PK.Algorithm);
  if ASN1StructAssigned(PK.PublicKey) then begin
    SS := TStringStream.Create('');
    try
      ASN1ToStream(PK.PublicKey,SS);
      FPublicKey := SS.DataString;
    finally
      SS.Free;
    end;
  end else
    FPublicKey := '';
  PK.Free;

  FExtensions.Interpret;

  FASN1Struct.FData := @FCert;
  FASN1Struct.Interpret;
end;

procedure TX509Certificate.LoadFromFile(AFileName: TFileName; Format: Byte);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName,fmOpenRead or fmShareExclusive);
  try
    LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TX509Certificate.LoadFromStream(AStream: TStream; Format: Byte);
begin
  FASN1Struct.FData := nil;
  FASN1Struct.Clear;
  FExtensions.FCert := nil;
  FExtensions.Clear;
  FCert.LoadFromStream(AStream,Format);
  Interpret;
end;

function TX509Certificate.NoLoad: Boolean;
begin
  Result := (FCert.Length > 0) and not FAutoLoad;
end;

procedure TX509Certificate.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FTrustedCertificates then
      FTrustedCertificates := nil;
  end;
end;

procedure TX509Certificate.RSASignCACertificate(
  const PrivKey: TIFPrivateKey; HashAlgorithm: THashAlgorithm;
  MGFHashAlgorithm: THashAlgorithm; Encoding: TSignEncoding);
begin
  X509Base.RSASignCertificate(FCert,FCert,PrivKey,HashAlgorithm,MGFHashAlgorithm,Encoding);
end;

procedure TX509Certificate.RSASignCertificate(const CACert: TASN1Struct;
  const PrivKey: TIFPrivateKey; HashAlgorithm: THashAlgorithm;
  MGFHashAlgorithm: THashAlgorithm; Encoding: TSignEncoding);
begin
  X509Base.RSASignCertificate(FCert,CACert,PrivKey,HashAlgorithm,MGFHashAlgorithm,Encoding);
end;

procedure TX509Certificate.SaveToFile(AFileName: TFileName; Format: Byte);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName,fmCreate or fmShareExclusive);
  try
    SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TX509Certificate.SaveToStream(AStream: TStream; Format: Byte);
begin
  if ASN1StructAssigned(FCert) then
    FCert.SaveToStream(AStream,Format);
end;

procedure TX509Certificate.SetASN1Struct(const Value: TMPASN1Structs);
begin
  FASN1Struct.Assign(Value);
end;

procedure TX509Certificate.SetAutoLoad(const Value: Boolean);
begin
  if Value = FAutoLoad then Exit;
  FAutoLoad := Value;
  if Value and FileExists(FFileName) then
    LoadFromFile(FFileName);
end;

procedure TX509Certificate.SetCert(const Value: TASN1Struct);
begin              
  FASN1Struct.Clear;
  FExtensions.Clear;
  FCert.Assign(Value);
  Interpret;
end;

procedure TX509Certificate.SetExtendedKeyUsage(const Value: TStrings);
begin
  FExtendedKeyUsage.Assign(Value);
end;

procedure TX509Certificate.SetExtensions(const Value: TMPX509Extensions);
begin
  FExtensions.Assign(Value);
end;

procedure TX509Certificate.SetFileName(const Value: TFileName);
begin
  if Value = FFileName then Exit;
  FFileName := Value;
  if FAutoLoad and FileExists(Value) then
    LoadFromFile(Value);
end;

procedure TX509Certificate.SetIssuer(const Value: TMPX501Name);
begin
  FIssuer.Assign(Value);
end;

procedure TX509Certificate.SetIssuerAltName(
  const Value: TMPX509GeneralNames);
begin
  FIssuerAltName.Assign(Value);
end;

procedure TX509Certificate.SetKeyUsage(const Value: TKeyUsage);
begin
  FKeyUsage := Value;
end;

procedure TX509Certificate.SetPublicKey(const Value: string);
begin
  FPublicKey := HexToOS(Value);
end;

procedure TX509Certificate.SetSerial(const Value: string);
begin
  // Not implemented
end;

procedure TX509Certificate.SetSignature(
  const Value: TMPX509Algorithm);
begin
  FSignature.Assign(Value);
end;

procedure TX509Certificate.SetSignatureValue(const Value: string);
var
  S: string;
begin
  S := #0 + HexToOS(Value);
  FCert.Contents^.Items[2].SetContent(S[1],Length(S));
end;

procedure TX509Certificate.SetSignParams(const Value: TMPASN1Structs);
begin
  FSignature.Parameters := Value;
end;

procedure TX509Certificate.SetSPKParams(const Value: TMPASN1Structs);
begin
  FSubjectPublicKey.Parameters := Value;
end;

procedure TX509Certificate.SetSubject(const Value: TMPX501Name);
begin
  FSubject.Assign(Value);
end;

procedure TX509Certificate.SetSubjectAltName(
  const Value: TMPX509GeneralNames);
begin
  FSubjectAltName.Assign(Value);
end;

procedure TX509Certificate.SetSubjectPublicKey(
  const Value: TMPX509Algorithm);
begin
  FSubjectPublicKey.Assign(Value);
end;

procedure TX509Certificate.SetTrustedCertificates(
  const Value: TX509TrustedCertificates);
begin
  FTrustedCertificates := Value;
end;

procedure TX509Certificate.SetValidity(const Value: TMPX509Validity);
begin
  FValidity.Assign(Value);
end;

function TX509Certificate.VerifyCertificate(
  var Status: TCertStatusCode): Boolean;
begin
  Result := False;
  if Assigned(FTrustedCertificates) then begin
    if FTrustedCertificates.VerifyCertificate(FCert,False,Status) < 0 then
      Result := Status = crcOK
    else begin
      Result := FTrustedCertificates.VerifyCAChain(FCert,Status);
      if Status = crcCANotTrusted then begin
        Status := crcTrusted;
        Result := True;
      end;
    end;
  end else
    Status := crcCANotTrusted;
end;

{ TMPASN1Struct }

function TMPASN1Structs.Add: TMPASN1Struct;
begin
  Result := TMPASN1Struct(inherited Add);
end;

procedure TMPASN1Structs.Assign(Source: TPersistent);
begin
  if FData = nil then
    raise Exception.Create('Invalid operation');
  if Source is TMPASN1Struct then begin
    if FData <> TMPASN1Struct(Source).FData then begin
      Clear;
      FData^.Assign(TMPASN1Struct(Source).FData^);
    end;
    Interpret;
  end else
    inherited Assign(Source);
end;

procedure TMPASN1Structs.Compose;
begin

end;

constructor TMPASN1Structs.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner,TMPASN1Struct);
  if AOwner is TMPASN1Struct then
    FData := TMPASN1Struct(AOwner).FData;
end;

function TMPASN1Structs.GetCls: Byte;
begin
  if Assigned(FData) and ASN1StructAssigned(FData^) then
    Result := FData^.Cls
  else
    Result := 0;
end;

function TMPASN1Structs.GetTag: Cardinal;
begin
  if Assigned(FData) and ASN1StructAssigned(FData^) then
    Result := FData^.Tag
  else
    Result := 0;
end;

procedure TMPASN1Structs.InternalInterpret;
var
  Item: TMPASN1Struct;
  LowCount, LowLow, I: Integer;
begin
  if Assigned(FData) and ASN1StructAssigned(FData^) and
     FData.Constructed and (FData^.Contents <> nil) then begin
    if FData^.ItemCount < 0 then
      FData^.AllocContents(0);
    LowCount := Count;
    if LowCount > FData^.ItemCount then
      LowLow := FData^.ItemCount
    else
      LowLow := LowCount;
    for I := 0 to LowLow - 1 do begin
      Item := TMPASN1Struct(Items[I]);
      Item.FData := FData^.Items[I];
      Item.Interpret;
    end;
    for I := LowLow to FData^.ItemCount - 1 do begin
      Item := Add;
      Item.FData := FData^.Items[I];
      Item.Interpret;
    end;
    while Count > FData^.ItemCount do begin
      Item := Add;
      Item.FData := FData.AddField;
      Item.Interpret;
    end;
  end;
end;

procedure TMPASN1Structs.Interpret;
begin
  BeginUpdate;
  try
    Clear;
    InternalInterpret;
  finally
    EndUpdate;
  end;
end;

procedure TMPASN1Structs.SetCls(const Value: Byte);
begin
  if Assigned(FData) and ASN1StructAssigned(FData^) then
    FData^.Cls := Value;
end;

procedure TMPASN1Structs.SetTag(const Value: Cardinal);
begin
  if Assigned(FData) and ASN1StructAssigned(FData^) then
    FData^.Tag := Value;
end;

procedure TMPASN1Structs.Update(Item: TCollectionItem);
begin
  if Item = nil then
    Compose;
  InternalInterpret;
end;

{ TMPASN1Struct }

procedure TMPASN1Struct.Assign(Source: TPersistent);
begin
  if Source is TMPASN1Struct then begin
    FData := TMPASN1Struct(Source).FData;
  end else
    inherited Assign(Source);
end;

procedure TMPASN1Struct.Compose;
begin

end;

constructor TMPASN1Struct.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  if Collection is TMPASN1Structs then begin
    if (TMPASN1Structs(Collection).FData <> nil) and
       ASN1StructAssigned(TMPASN1Structs(Collection).FData^) and
       TMPASN1Structs(Collection).FData^.Constructed then begin
      if (TMPASN1Structs(Collection).FData^.Contents = nil) or
         (TMPASN1Structs(Collection).FData^.Contents^.ItemCount <= Index) then
        TMPASN1Structs(Collection).FData^.AllocContents(Index + 1);
      FData := @TMPASN1Structs(Collection).FData^.Contents^.Items[Index];
    end else
      raise Exception.Create('Invalid data');
  end;
  FContents := TMPASN1Structs.Create(Self);
  Interpret;
end;

function TMPASN1Struct.DataAssigned: Boolean;
begin
  Result := Assigned(FData) and ASN1StructAssigned(FData^);
end;

destructor TMPASN1Struct.Destroy;
begin
  FContents.Free;
  inherited Destroy;
end;

function TMPASN1Struct.GetCls: Byte;
begin
  if DataAssigned then
    Result := FData^.Cls
  else
    Result := 0;
end;

function TMPASN1Struct.GetConstructed: Boolean;
begin
  if DataAssigned then
    Result := FData^.Constructed
  else
    Result := False;
end;

function TMPASN1Struct.GetContent: string;
begin
  if DataAssigned and not FData^.Constructed then begin
    SetLength(Result,FData.Length);
    Move(FData.Content^,Result[1],Length(Result));
    Result := OSToHex(Result);
  end else
    Result := '';
end;

function TMPASN1Struct.GetDisplayName: string;
begin
  if Cls = V_ASN1_UNIVERSAL then begin
    case Tag of
      0: Result := 'EOC';
      1: Result := 'BOOLEAN';
      2: Result := 'INTEGER';
      3: Result := 'BIT STRING';
      4: Result := 'OCTET STRING';
      5: Result := 'NULL';
      6: Result := 'OBJECT';
      7: Result := 'OBJECT DESCRIPTOR';
      8: Result := 'EXTERNAL';
      9: Result := 'REAL';
      10: Result := 'ENUMERATED';
      12: Result := 'UTF8STRING';
      16: Result := 'SEQUENCE';
      17: Result := 'SET';
      19: Result := 'PRINTABLESTRING';
      22: Result := 'IA5STRING';
      23: Result := 'UTCTIME';
      24: Result := 'GENERALIZEDTIME';
      30: Result := 'BMPSTRING';
    else
      Result := inherited GetDisplayName;
    end;
  end else if Cls = V_ASN1_APPLICATION then
    Result := Format('[APPLICATION %d]',[Tag])
  else if Cls = V_ASN1_CONTEXT_SPECIFIC then
    Result := Format('[%d]',[Tag])
  else if Cls = V_ASN1_PRIVATE then
    Result := Format('[PRIVATE %d]',[Tag]);
end;

function TMPASN1Struct.GetTag: Cardinal;
begin
  if DataAssigned then
    Result := FData^.Tag
  else
    Result := 0;
end;

function TMPASN1Struct.GetValue: string;
begin
  if DataAssigned and not Constructed then begin
    Result := FData^.DisplayContent;
  end else
    Result := '';
end;

procedure TMPASN1Struct.Interpret;
begin
  if Constructed then begin
    FContents.FData := FData;
    FContents.Interpret;
  end;
end;

procedure TMPASN1Struct.SetCls(const Value: Byte);
begin
  if DataAssigned then
    FData^.Cls := Value;
end;

procedure TMPASN1Struct.SetConstructed(const Value: Boolean);
begin
  if DataAssigned and Value <> Constructed then begin
    FData^.DisposeContent;
    FData^.Constructed := Value;
    Interpret;
  end;
end;

procedure TMPASN1Struct.SetContent(const Value: string);
var
  S: string;
begin
  if DataAssigned and not Constructed then begin
    S := HexToOS(Value);
    FData^.SetContent(S[1],Length(S));
  end;
end;

procedure TMPASN1Struct.SetContents(const Value: TMPASN1Structs);
begin
  FContents.Assign(Value);
end;

procedure TMPASN1Struct.SetTag(const Value: Cardinal);
begin
  if DataAssigned then
    FData^.Tag := Value;
end;

procedure TMPASN1Struct.SetValue(const Value: string);
begin
  if DataAssigned and not Constructed then
    FData^.EditContent(Value);
end;

{ TMPX509Algorithm }

constructor TMPX509Algorithm.Create(AOwner: TPersistent);
begin
  FParameters := TMPASN1Structs.Create(Self);
  FData := TX509AlgorithmIdentifier.Create;
end;

destructor TMPX509Algorithm.Destroy;
begin
  FData.Free;
  FParameters.Free;
  inherited;
end;

function TMPX509Algorithm.GetAlgID: string;
begin
  Result := FData.Algorithm;
end;

function TMPX509Algorithm.GetAlgName: string;
begin
  Result := FData.AlgorithmName;
end;

function TMPX509Algorithm.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TMPX509Algorithm.SetAlgID(const Value: string);
begin
  FData.Algorithm := Value;
  FData.AlgorithmName := GetObjectName(Value);
end;

procedure TMPX509Algorithm.SetAlgName(const Value: string);
begin
  // Not supported
end;

procedure TMPX509Algorithm.SetData(const Data: TX509AlgorithmIdentifier);
begin
  DisposeASN1Struct(FData.Parameters);
  FData.Algorithm := Data.Algorithm;
  FData.AlgorithmName := Data.AlgorithmName;
  if ASN1StructAssigned(Data.Parameters) then begin
    CopyASN1Struct(FData.Parameters,Data.Parameters);
  end else begin
    DisposeASN1Struct(FData.Parameters);
    NewComposeASN1Struct(FData.Parameters,V_ASN1_UNIVERSAL,False,V_ASN1_NULL);
  end;
  FParameters.FData := @FData.Parameters;
  FParameters.Interpret;
end;

procedure TMPX509Algorithm.SetParameters(const Value: TMPASN1Structs);
begin
  FParameters.Assign(Value);
end;

{ TMPX509Extension }

procedure TMPX509Extension.Assign(Source: TPersistent);
begin
  if Source is TMPX509Extension then begin
    FextnValue.Clear;
    FData.extnID := TMPX509Extension(Source).FData.extnID;
    FData.extnIDName := TMPX509Extension(Source).FData.extnIDName;
    FData.Critical := TMPX509Extension(Source).FData.Critical;
    CopyASN1Struct(FData.extnValue,TMPX509Extension(Source).FData.extnValue);
    if FData.extnValue.Constructed then
      FextnValue.Interpret;
  end else
    inherited Assign(Source);
end;

constructor TMPX509Extension.Create(Collection: TCollection);
begin
  inherited;
  FData := TX509Extension.Create;
  FextnValue := TMPASN1Structs.Create(Self);
  FextnValue.FData := @FData.extnValue;
end;

destructor TMPX509Extension.Destroy;
begin
  FData.Free;
  FextnValue.Free;
  inherited;
end;

function TMPX509Extension.GetCritical: Boolean;
begin
  Result := FData.Critical;
end;

function TMPX509Extension.GetDisplayName: string;
begin
  Result := FData.extnIDName;
end;

function TMPX509Extension.GetExtnID: string;
begin
  Result := FData.extnID;
end;

function TMPX509Extension.GetExtnName: string;
begin
  Result := FData.extnIDName;
end;

function TMPX509Extension.GetExtnValueCnt: string;
begin
  if ASN1StructAssigned(FData.extnValue) and
     not FData.extnValue.Constructed then begin
    SetLength(Result,FData.extnValue.Length);
    Move(FData.extnValue.Content^,Result[1],Length(Result));
    Result := OStoHex(Result);
  end else
    Result := '';
end;

procedure TMPX509Extension.SetCritical(const Value: Boolean);
begin
  FData.Critical := Value;
end;

procedure TMPX509Extension.SetData(const Data: TX509Extension);
begin
  FData.extnID := Data.extnID;
  FData.extnIDName := Data.extnIDName;
  FData.Critical := Data.Critical;
  CopyASN1Struct(FData.extnValue,Data.extnValue);
  if FData.extnValue.Constructed then
    FextnValue.Interpret;
end;

procedure TMPX509Extension.SetExtnID(const Value: string);
begin
  FData.extnID := Value;
end;

procedure TMPX509Extension.SetExtnName(const Value: string);
begin
  FData.extnIDName := Value;
end;

procedure TMPX509Extension.SetextnValue(const Value: TMPASN1Structs);
begin
  FextnValue.Assign(Value);
end;

procedure TMPX509Extension.SetExtnValueCnt(const Value: string);
var
  S: string;
begin
  if ASN1StructAssigned(FData.extnValue) and
     not FData.extnValue.Constructed then begin
    S := HexToOS(Value);
    FData.extnValue.SetContent(S[1],Length(S));
  end;
end;

{ TMPX509Extensions }

procedure TMPX509Extensions.Assign(Source: TPersistent);
begin
  if Source <> Self then begin
    if Source is TMPX509Extensions then begin
      Clear;
      CopyASN1Struct(FCert^,TMPX509Extensions(Source).FCert^);
      Interpret;
    end else
      inherited Assign(Source);
  end;
end;

procedure TMPX509Extensions.Compose; 
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Update(Items[I]);
end;

constructor TMPX509Extensions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner,TMPX509Extension);
end;

procedure TMPX509Extensions.Interpret;
var
  R, I: Integer;
  Ext: TX509Extension;
  Item: TMPX509Extension;
begin
  if Assigned(FCert) and ASN1StructAssigned(FCert^) then begin
    BeginUpdate;
    try
      Clear;
      Ext := nil;
      I := 0;
      repeat
        R := ExtractExtension(FCert^,I,Ext);
        if R = E_OK then begin
          Item := TMPX509Extension(Add);
          Item.SetData(Ext);
        end;
        Inc(I);
      until (R <> E_OK);
      Ext.Free;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TMPX509Extensions.Update(Item: TCollectionItem);
begin
  if Item = nil then
    Compose
  else if Assigned(FCert) and ASN1StructAssigned(FCert^) and
          ASN1StructAssigned(TMPX509Extension(Item).FData.extnValue) then
    ImposeExtension(TMPX509Extension(Item).FData,FCert^);
end;

{ TMPX509Validity }

procedure TMPX509Validity.Assign(Source: TPersistent);
begin
  if Source is TMPX509Validity then begin
    FData := TMPX509Validity(Source).FData;
  end else
    inherited Assign(Source);
end;

function TMPX509Validity.GetnotAfter: TDateTime;
begin
  Result := FData.notAfter;
end;

function TMPX509Validity.GetnotBefore: TDateTime;
begin
  Result := FData.notBefore;
end;

procedure TMPX509Validity.SetnotAfter(const Value: TDateTime);
begin
  if not ReadOnly then
    FData.notAfter := Value;
end;

procedure TMPX509Validity.SetnotBefore(const Value: TDateTime);
begin
  if not ReadOnly then
    FData.notBefore := Value;
end;

procedure TMPX509Validity.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

{ TX509TrustedCertificates }

function TX509TrustedCertificates.AddCertificate(const Cert: TASN1Struct;
  ExplicitTrust: Boolean; var Status: TCertStatusCode;
  AllowExpired: Boolean): Integer;
begin
  FLock.Acquire;
  try
    Result := InternalVerifyCertificate(Cert,ExplicitTrust,Status,AllowExpired);
    if (Result < 0) and (Status = crcOK) then begin
      Result := Count;
      if Result < 0 then Result := 0;
      FCerts.Contents^.Items[0].Contents^.Items[1].AllocContents(Result + 1);
      Certs[Result] := Cert;
      AddToIndex(Result);
      FModified := True;
      if not (csUpdating in ComponentState) then begin
        CertListLoaded;
        UpdateHijackers;
      end;
    end;
    if Status <> crcOK then
      if Assigned(FOnCertNotAccepted) then
        FOnCertNotAccepted(Self,Cert,Status);
  finally
    FLock.Release;
  end;
end;

function TX509TrustedCertificates.AddCRL(const CRL: TASN1Struct;
  var Status: TCertStatusCode): Integer;
var
  CACert: PASN1Struct;
  IDP, IDP0: TX509issuingDistributionPoint;
  I, R, R0, DR, DR0: Integer;
  BaseFound, Delta, Delta0: Boolean;
begin
  FLock.Acquire;
  try
    Result := IndexOfCRL(CRL);
    if Result >= 0 then begin
      Status := crcOK;
      Exit;
    end;

    if FindCRLCACert(CRL,CACert) then begin
      if IsRevoked(CACert^) then
        Status := crcRevoked
      else if not CheckValidity(CACert^,FHoursOffsetFromGMT) then
        Status := crcExpired
      else
        VerifyCAChain(CACert^,Status);
    end else
      Status := crcCANotTrusted;
    if Status in [crcOK,crcCANotTrusted] then begin
      Result := CRLCount;
      ExtractIssuingDistributionPoint(CRL,IDP);
      R := ExtractCRLNumber(CRL);
      DR := ExtractBaseCRLNumber(CRL);
      if DR > R then begin
        Status := crcSyntax
      end else begin
        if (DR >= 0) and (DR < R) then
          BaseFound := False
        else
          BaseFound := True;
        Status := crcOK;
        Delta := not BaseFound;
        for I := 0 to CRLCount - 1 do begin
          if IsAllegedCRLIssuer(CRLs[I],CACert^) then begin
            R0 := ExtractCRLNumber(CRLs[I]);
            DR0 := ExtractBaseCRLNumber(CRLs[I]);

            Delta0 := (DR0 > -1) and (R0 < DR0);
            if not Delta0 then begin
              BaseFound := BaseFound or (R0 = DR);
              if R0 >= R then begin
                Result := I;
                Status := crcSuperseeded;
                Break;
              end;
            end else if Delta then begin
              if R <= R0 then begin
                Result := I;
                Status := crcSuperseeded;
                Break;
              end;
            end;
            if Delta = Delta0 then begin
              ExtractIssuingDistributionPoint(CRLs[I],IDP0);
              if (R0 < R) and
                 (CompareIssuingDistributionPoints(IDP0,IDP) >= 0) then begin
                Result := I;
                Break;
              end;
            end;
          end;
        end;
        if (Status <> crcSuperseeded) and Delta and not BaseFound then begin
          Result := -1;
          Status := crcBaseNotFound;
        end;
      end;
      if Status = crcOK then begin
        if Result = CRLCount then
          FCerts.Contents^.Items[0].Contents^.Items[2].AllocContents(Result + 1);
        CRLs[Result] := CRL;
        FModified := True;
        if not (csUpdating in ComponentState) then begin
          CertListLoaded;
          UpdateHijackers;
        end;
      end;
    end else
      Result := -1;
  finally
    FLock.Release;
  end;
end;

constructor TX509TrustedCertificates.Create(AOwner: TComponent);
begin
  FCreationThreadID := GetCurrentThreadID;
  FLock := TCriticalSection.Create;
  inherited;
  InitStructure;
  FIndex := TStringList.Create;
  TStringList(FIndex).Sorted := True;
  TStringList(FIndex).Duplicates := dupAccept;
  FSubjectIndex := TStringList.Create;
  TStringList(FSubjectIndex).Sorted := True;
  TStringList(FSubjectIndex).Duplicates := dupAccept;
  FRequiredPolicies := TStringList.Create;
  FAcceptedPolicies := TStringList.Create;
{$IFDEF MSWINDOWS}
  SetHoursOffsetFromGMT(OffsetFromUTC);
{$ENDIF}
  FLeastKeyBitSize := 1024;
  FCertList := TCertificateCollection.Create(Self);
end;

destructor TX509TrustedCertificates.Destroy;
begin
  SetCertSource(nil);
  DisposeASN1Struct(FCerts);
  FLock.Free;
  FIndex.Free;
  FSubjectIndex.Free;
  FRequiredPolicies.Free;
  FAcceptedPolicies.Free;
  inherited;
  FCertList.Free;
end;

function TX509TrustedCertificates.FindCACert(const Cert: TASN1Struct;
  var CACert: PASN1Struct): Boolean;
var
  I: Integer;
  N: PASN1Struct;
  F: TASN1Struct;
  SS: TStringStream;
  S: string;
begin
  Result := False;
  if ExtractIssuerStruct(Cert,N) = E_OK then begin
    SS := TStringStream.Create('');
    try
      N^.SaveToStream(SS,fmtDER);
      S := SS.DataString;
    finally
      SS.Free;
    end;
    I := FSubjectIndex.IndexOf(S);
    if I > -1 then begin
      while (I < FSubjectIndex.Count) and (FSubjectIndex[I] = S) do begin
        F := Certs[Integer(FSubjectIndex.Objects[I])];
        if IsAllegedIssuer(Cert,F) then begin
          if not CompareCertificate(Cert,F) then begin
            Result := True;
            I := Integer(FSubjectIndex.Objects[I]);
            CACert := GetPCerts(I);
          end;
          Break;
        end;
        Inc(I);
      end;
    end;
  end;
  if Assigned(FTrustedCACertificates) and not Result then
    Result := FTrustedCACertificates.FindCACert(Cert,CACert);
end;

function TX509TrustedCertificates.FindCRL(const CACert: TASN1Struct;
  const issuingDistPoint: TX509issuingDistributionPoint;
  var CRL: PASN1Struct): Boolean;
var
  I: Integer;
  IDP: TX509issuingDistributionPoint;
begin
  Result := False;
  for I := 0 to CRLCount - 1 do
    if IsAllegedCRLIssuer(CRLs[I],CACert) then begin
      System.Finalize(IDP);
      if ExtractIssuingDistributionPoint(CRLs[I],IDP) = E_OK then begin
        if CompareIssuingDistributionPoints(issuingDistPoint,IDP) = 0 then begin
          Result := True;
          CRL := GetPCRLs(I);
          Exit;
        end;
      end;
    end;
end;

function TX509TrustedCertificates.FindCRLCACert(const CRL: TASN1Struct;
  var CACert: PASN1Struct): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if IsAllegedCRLIssuer(CRL,Certs[I]) then begin
      Result := True;
      CACert := GetPCerts(I);
      Exit;
    end;
end;

function TX509TrustedCertificates.FindCRLs(const CACert: TASN1Struct;
  var CRLs: TASN1Struct): Integer;
var
  I: Integer;
begin
  if ASN1StructAssigned(CRLs) then
    DisposeASN1Struct(CRLs);
  NewComposeASN1Struct(CRLs,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  Result := 0;
  for I := 0 to CRLCount - 1 do
    if IsAllegedCRLIssuer(Self.CRLs[I],CACert) then begin
      Inc(Result);
      AddASN1Field(CRLs,Self.CRLs[I]);
    end;
end;

function TX509TrustedCertificates.FindRevocation(const Cert: TASN1Struct;
  var RC: TX509RevokedCertificate): Boolean;
var
  I, Idx, J, K: Integer;
  CACert, CRLCACert, N: PASN1Struct;
  S: string;
  CL: TCertificateList;
  EExt: TEntryExtension;
  Ext: TX509Extension;
  CRLDist: TCrldistributionPointsSyntax;
begin
  Result := False;
  if FindCACert(Cert,CACert) then begin
    ExtractSerial(Cert,S);
    for I := 0 to CRLCount - 1 do begin
      Idx := CertIsRevoked(Cert,CRLs[I]);
      if Idx >= 0 then
        if FindCRLCACert(CRLs[I],CRLCACert) then begin
          Result := ASN1StructCompare(CRLCACert^,CACert^);
          if not Result then begin
            CL := TCertificateList.Create(nil,nil);
            try
              CL.Data.ReadOnly := False;
              CL.AssignStruct(CRLs[I]);
              ExtractIssuerStruct(Cert,N);
              EExt := CL.TbsCertList.RevokedCertificates[Idx].CrlEntryExtensions.UniqueItem[eeveIdCeCertificateIssuer];
              if Assigned(EExt) then
                for J := 0 to EExt.ExtnValue.AsCe_CertificateIssuer.Count - 1 do
                  if EExt.ExtnValue.AsCe_CertificateIssuer[J].Choice = gneDirectoryName then
                    Result := ASN1StructCompare(N^,EExt.ExtnValue.AsCe_CertificateIssuer[J].AsDirectoryName.Data);
              if Result then begin
                Ext := nil;
                try
                  Result := ExtractNamedExtension(CACert^,id_ce_cRLDistributionPoints,Ext) = E_OK;
                  if Result then begin
                    CRLDist := TCrldistributionPointsSyntax.Create(nil,nil);
                    try
                      CRLDist.Data.ReadOnly := False;
                      CRLDist.AssignStruct(Ext.extnValue);
                      ExtractSubjectStruct(CRLCACert^,N);
                      for J := 0 to CRLDist.Count - 1 do begin
                        for K := 0 to CRLDist[J].CRLIssuer.Count - 1 do
                          if CRLDist[J].CRLIssuer[K].Choice = gneDirectoryName then begin
                            Result := ASN1StructCompare(N^,CRLDist[J].CRLIssuer[K].Data);
                            if Result then Break;
                          end;
                        if Result then Break;
                      end;
                    finally
                      CRLDist.Free;
                    end;
                  end;
                finally
                  Ext.Free;
                end;
              end;
            finally
              CL.Free;
            end;
          end;
          if Result then begin
            ExtractRevokedCertificate(CRLs[I],Idx,RC);
            Exit;
          end;
        end;
    end;
  end;
end;

function TX509TrustedCertificates.GetCerts(index: Integer): TASN1Struct;
begin
  Result := nil;
  if (index >= 0) and (index <= Count) then
    Result := FCerts.Contents^.Items[0].Contents^.Items[1].Contents^.Items[index];
end;

function TX509TrustedCertificates.GetCount: Integer;
begin
  Result := 0;
  if Assigned(FCerts) and
     Assigned(FCerts.Contents^.Items[0]) and
     Assigned(FCerts.Contents^.Items[0].Contents^.Items[1]) then
    Result := FCerts.Contents^.Items[0].Contents^.Items[1].ItemCount;
end;

function TX509TrustedCertificates.GetCRLCount: Integer;
begin
  Result := 0;
  if Assigned(FCerts) and
     Assigned(FCerts.Contents^.Items[0]) and
     Assigned(FCerts.Contents^.Items[0].Contents^.Items[2]) then
    Result := FCerts.Contents^.Items[0].Contents^.Items[2].ItemCount;
end;

function TX509TrustedCertificates.GetCRLs(index: Integer): TASN1Struct;
begin
  Result := nil;
  if (index >= 0) and (index <= CRLCount) then
    Result := FCerts.Contents^.Items[0].Contents^.Items[2].Items[index]^;
end;

function TX509TrustedCertificates.GetPCerts(index: Integer): PASN1Struct;
begin
  Result := @FCerts.Contents^.Items[0].Contents^.Items[1].Contents^.Items[index];
end;

function TX509TrustedCertificates.GetPCRLs(index: Integer): PASN1Struct;
begin
  Result := @FCerts.Contents^.Items[0].Contents^.Items[2].Contents^.Items[index];
end;

function TX509TrustedCertificates.GetSignatureCert: TASN1Struct;
begin
  Result := FCerts.Contents^.Items[0].Contents^.Items[0].Contents^.Items[0];
end;

function TX509TrustedCertificates.IndexOfCert(
  const Cert: TASN1Struct): Integer;
var
  I: Integer;  
{$IFDEF MD5}
  SS: TStringStream;
  S: string;
{$ENDIF MD5}
begin
  Result := -1;
{$IFDEF MD5}
    SS := TStringStream.Create('');
    try
      Cert.SaveToStream(SS,fmtDER);
      S := DigestString(haMD5,SS.DataString);
    finally
      SS.Free;
    end;
    I := FIndex.IndexOf(S);
    if I > -1 then begin
      while (I < FIndex.Count) and (FIndex[I] = S) and
            not ASN1StructCompare(Cert,Certs[Integer(FIndex.Objects[I])]) do
        Inc(I);
     if (I < FIndex.Count) and (FIndex[I] = S) then
       Result := Integer(FIndex.Objects[I]);
    end;
{$ELSE  MD5}
    for I := 0 to Count - 1 do
      if ASN1StructCompare(Cert,Certs[I]) then begin
        Result := I;
        Exit;
      end;
{$ENDIF MD5}
end;

function TX509TrustedCertificates.IndexOfCRL(
  const CRL: TASN1Struct): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to CRLCount - 1 do
    if ASN1StructCompare(CRL,CRLs[I]) then begin
      Result := I;
      Exit;
    end;
end;

procedure TX509TrustedCertificates.InitStructure;
var
  F: PASN1Struct;
begin
  Assert(Assigned(GlobalObject),'MPX509 not initialized: Call StrSecInit.InitMPX509 to correct.');
  NewComposeASN1Struct(FCerts,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  FCerts._AddRef;
  AddComposeASN1Field(FCerts,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  AddComposeASN1Field(FCerts.Contents^.Items[0],V_ASN1_CONTEXT_SPECIFIC,True,0);
  AddComposeASN1Field(FCerts.Contents^.Items[0].Contents^.Items[0],
                      V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  AddComposeASN1Field(FCerts.Contents^.Items[0],V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  AddComposeASN1Field(FCerts.Contents^.Items[0],V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  F := AddComposeASN1Field(FCerts.Items[0]^,V_ASN1_CONTEXT_SPECIFIC,True,1);
  F^.Implicit := True;
  F^.ImplicitTag := V_ASN1_SEQUENCE;
  F^.Optional := True;
  F := AddComposeASN1Field(FCerts.Items[0]^,V_ASN1_CONTEXT_SPECIFIC,True,2);
  F^.Implicit := True;
  F^.ImplicitTag := V_ASN1_SEQUENCE;
  F^.Optional := True;
  F := AddComposeASN1Field(FCerts.Items[0]^,V_ASN1_CONTEXT_SPECIFIC,True,3);
  F^.Implicit := True;
  F^.ImplicitTag := V_ASN1_SEQUENCE;
  F^.Optional := True;
  F^.CreateOFTemplate;
  F^.Template.Tag := V_ASN1_SEQUENCE;
  F^.Template.Constructed := True;
  F^.Template.AddField('md5Fingerprint','OCTET STRING',nil);
  // SET OF AttributeValue:
  F^.Template.AddField('smimeCapabilities','SET OF AlgorithmIdentifier',nil);
  FCerts.AddField('signatureAlgorithm',
                  'AlgorithmIdentifier',
                  GlobalObject.FindField('signatureAlgorithm')^);
  AddComposeASN1Field(FCerts,V_ASN1_UNIVERSAL,False,V_ASN1_BIT_STRING);
  FCerts.Persistent := True;
  FCerts.Items[0]^.Persistent := True;
end;

function TX509TrustedCertificates.IsRevoked(const Cert: TASN1Struct): Boolean;
var
  I, Idx, J, K: Integer;
  CACert, CRLCACert, N: PASN1Struct;
  S: string;
  CL: TCertificateList;
  EExt: TEntryExtension;
  Ext: TX509Extension;
  CRLDist: TCrldistributionPointsSyntax;
begin
  Result := False;
  if FindCACert(Cert,CACert) then begin
    ExtractSerial(Cert,S);
    for I := 0 to CRLCount - 1 do begin
      Idx := CertIsRevoked(Cert,CRLs[I]);
      if Idx >= 0 then
        if FindCRLCACert(CRLs[I],CRLCACert) then begin
          Result := ASN1StructCompare(CRLCACert^,CACert^);
          if not Result then begin
            CL := TCertificateList.Create(nil,nil);
            try
              CL.Data.ReadOnly := False;
              CL.AssignStruct(CRLs[I]);
              ExtractIssuerStruct(Cert,N);
              EExt := CL.TbsCertList.RevokedCertificates[Idx].CrlEntryExtensions.UniqueItem[eeveIdCeCertificateIssuer];
              if Assigned(Ext) then
                for J := 0 to EExt.ExtnValue.AsCe_CertificateIssuer.Count - 1 do
                  if EExt.ExtnValue.AsCe_CertificateIssuer[J].Choice = gneDirectoryName then
                    Result := ASN1StructCompare(N^,EExt.ExtnValue.AsCe_CertificateIssuer[J].AsDirectoryName.Data);
              if Result then begin
                Ext := nil;
                try
                  Result := ExtractNamedExtension(Cert,id_ce_cRLDistributionPoints,Ext) = E_OK;
                  if not Result then
                    Result := ExtractNamedExtension(CACert^,id_ce_cRLDistributionPoints,Ext) = E_OK;
                  if Result then begin
                    CRLDist := TCrldistributionPointsSyntax.Create(nil,nil);
                    try
                      CRLDist.Data.ReadOnly := False;
                      CRLDist.AssignStruct(Ext.extnValue);
                      ExtractSubjectStruct(CRLCACert^,N);
                      for J := 0 to CRLDist.Count - 1 do begin
                        for K := 0 to CRLDist[J].CRLIssuer.Count - 1 do
                          if CRLDist[J].CRLIssuer[K].Choice = gneDirectoryName then begin
                            Result := ASN1StructCompare(N^,CRLDist[J].CRLIssuer[K].Data);
                            if Result then Break;
                          end;
                        if Result then Break;
                      end;
                    finally
                      CRLDist.Free;
                    end;
                  end;
                finally
                  Ext.Free;
                end;
              end;
            finally
              CL.Free;
            end;
          end;
          if Result then Break;
        end;
    end;
  end;
end;

procedure TX509TrustedCertificates.LoadFromFile(AFileName: TFileName);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName,fmOpenRead or fmShareExclusive);
  try
    LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TX509TrustedCertificates.LoadFromStream(AStream: TStream);
var
  I: Integer;
  IsLoading: Boolean;
  O: TComponent;
begin
  IsLoading := csLoading in ComponentState;
  O := Owner;
  while Assigned(O) and not IsLoading do begin
    IsLoading := csLoading in O.ComponentState;
    O := O.Owner;
  end;
  if not IsLoading then
    CertList.Clear;
  FCerts.Clear;
  ASN1FromStream(AStream,FCerts);
  if CheckSignature(FCerts,SignatureCert,False) then begin
    FModified := False;
    FIndex.Clear;
    for I := 0 to Count - 1 do
      AddToIndex(I);
    for I := 0 to FCerts.Items[0].Items[3].ItemCount - 1 do
      FRequiredPolicies.Add(FCerts.Items[0].Items[3].Items[I].ContentAsOID);
    for I := 0 to FCerts.Items[0].Items[4].ItemCount - 1 do
      FAcceptedPolicies.Add(FCerts.Items[0].Items[4].Items[I].ContentAsOID);
    CertListLoaded;
    UpdateHijackers;
  end else begin
    FModified := True;
    raise Exception.Create('Invalid format / No valid signature');
  end;
end;

procedure TX509TrustedCertificates.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FTrustedCACertificates then
      FTrustedCACertificates := nil;
    if AComponent = FCertSource then
      SetCertSource(nil);
    if AComponent = FSCLFile then
      FSCLFile := nil;
    while FHijackers.Remove(AComponent) >= 0 do;
  end;
end;

function TX509TrustedCertificates.RevokeCertificate(
  const Cert: TASN1Struct; Reason: CRLReason;
  HoldInstruction: string; InvalidityDate: TDateTime;
  NextCRLUpdate: TDateTime; PrivKey: IMPPrivateKey; CACert: PASN1Struct): Boolean;
var
  vCACert, CRL: PASN1Struct;
  Res: Boolean;
  IDPs: TX509IssuingDistributionPoints;
  CRLs: TASN1Struct;
  CRLNumber, BaseCRLNumber, N, I, R: Integer;
  RCList: TX509RevokedCertificates;
  RC: TX509RevokedCertificate;
  Serial: string;
  Status: TCertStatusCode;
  NewCRL: Boolean;
  ReasonFlags: TX509ReasonFlags;
  oCRL: TCertificateList;
  iCRL: ISigned;
begin
  if Assigned(Cert) then begin
    Res := (ExtractSerial(Cert,Serial) = E_OK) and
           (Assigned(CACert) or FindCACert(Cert,CACert));
    Assert(Res);
    vCACert := @Cert;
  end else begin
    Assert(Assigned(CACert));
    vCACert := CACert;
  end;
  ReasonFlags := [];
  case Reason of
    keyCompromise:        ReasonFlags := [rfKeyCompromise];
    cACompromise:         ReasonFlags := [rfCACompromise];
    affiliationChanged:   ReasonFlags := [rfAffilationChanged];
    superseded:           ReasonFlags := [rfSuperseeded];
    cessationOfOperation: ReasonFlags := [rfcessationOfOperation];
    certificateHold:      ReasonFlags := [rfcertificateHold];
    removeFromCRL:        ReasonFlags := [LowReasonFlag..High(TX509ReasonFlag)] - [rfUnused7];
  end;
  R := SuggestIssuingDistributionPoints(vCACert^,ReasonFlags,IDPs);
  Result := R > 0;
  if R < 0 then
    if Assigned(Cert) and FindCACert(Cert,vCACert) then
      if SuggestIssuingDistributionPoints(vCACert^,ReasonFlags,IDPs) >= 0 then
        Result := True;
  if Result then begin
    CRLNumber := 1;
    CRLs := nil;
    try
      if FindCRLs(SignatureCert,CRLs) >= 0 then begin
        for I := 0 to CRLs.ItemCount - 1 do begin
          N := ExtractCRLNumber(CRLs.Items[I]^);
          if N + 1 > CRLNumber then
            CRLNumber := N + 1;
        end;
      end;
    finally
      CRLs.Free;
    end;
    for I := 0 to Length(IDPs) - 1 do begin
      NewCRL := False;
      BaseCRLNumber := CRLNumber;
      try
        RCList := TX509RevokedCertificates.Create;
        try
          if FindCRL(CACert^,IDPs[I],CRL) then begin
            BaseCRLNumber := ExtractBaseCRLNumber(CRL^);
            if BaseCRLNumber < 0 then
              BaseCRLNumber := CRLNumber;
            ExtractRevokedCertificates(CRL^,RCList);
            NewCRL := False;
          end else begin
            New(CRL);
            FillChar(CRL^,SizeOf(TASN1Struct),0);
            NewCRL := True;
          end;
          if Assigned(Cert) then begin
            RC := RCList.Add;
            RC.userCertificate := Serial;
            RC.revocationDate := Now - FHoursOffsetFromGMT/24;
            RC.ReasonCode := Reason;
            RC.InvalidityDate := InvalidityDate - FHoursOffsetFromGMT/24;
            RC.HoldInstruction := HoldInstruction;
          end;
          ComposeCRL(CRL^,
                     CACert^,
                     Now - FHoursOffsetFromGMT/24,
                     NextCRLUpdate - FHoursOffsetFromGMT/24,
                     RCList,CRLNumber,BaseCRLNumber,IDPs[I]);
        finally
          RCList.Free;
        end;
        oCRL := TCertificateList.Create(nil,nil);
        try
          oCRL.AssignStruct(CRL^);
          iCRL := oCRL;
          PrivKey.SignSigned(iCRL,CACert^);
          CRL^.Assign(oCRL.Data);
        finally
          iCRL := nil;
          oCRL.Free;
        end;
      finally
        if NewCRL then begin
          AddCRL(CRL^,Status);
          DisposeASN1Struct(CRL^);
          Dispose(CRL);
          CRL := nil;
        end;
      end;
      Inc(CRLNumber);
    end;
  end;
end;

function TX509TrustedCertificates.RevokeCertificate(
  const Cert: TASN1Struct; Reason: CRLReason;
  HoldInstruction: string; InvalidityDate: TDateTime;
  NextCRLUpdate: TDateTime; const PrivKey: TIFPrivateKey): Boolean;
var
  Res: Boolean;
  CACert, CRL: PASN1Struct;
  IDPs: TX509IssuingDistributionPoints;
  CRLs: TASN1Struct;
  CRLNumber, BaseCRLNumber, N, I: Integer;
  RCList: TX509RevokedCertificates;
  RC: TX509RevokedCertificate;
  Serial: string;
  Status: TCertStatusCode;
  NewCRL: Boolean;
  ReasonFlags: TX509ReasonFlags;
begin
  Result := False;
  Res := ExtractSerial(Cert,Serial) = E_OK;
  Assert(Res);
  FillChar(CACert,SizeOf(CACert),0);
  if FindCACert(Cert,CACert) then begin
    ReasonFlags := [];
    case Reason of
      keyCompromise:        ReasonFlags := [rfKeyCompromise];
      cACompromise:         ReasonFlags := [rfCACompromise];
      affiliationChanged:   ReasonFlags := [rfAffilationChanged];
      superseded:           ReasonFlags := [rfSuperseeded];
      cessationOfOperation: ReasonFlags := [rfcessationOfOperation];
      certificateHold:      ReasonFlags := [rfcertificateHold];
      removeFromCRL:        ReasonFlags := [LowReasonFlag..High(TX509ReasonFlag)] - [rfUnused7];
    end;
    if SuggestIssuingDistributionPoints(CACert^,ReasonFlags,IDPs) >= 0 then begin
      FillChar(CRLs,SizeOf(CRLs),0);
      CRLNumber := 1;
      if FindCRLs(SignatureCert,CRLs) >= 0 then begin
        for I := 0 to CRLs.ItemCount - 1 do begin
          N := ExtractCRLNumber(CRLs.Items[I]^);
          if N + 1 > CRLNumber then
            CRLNumber := N + 1;
        end;
      end;
      CRLs.Free;
      for I := 0 to Length(IDPs) - 1 do begin
        RCList := TX509RevokedCertificates.Create;
        BaseCRLNumber := CRLNumber;
        if FindCRL(CACert^,IDPs[I],CRL) then begin
          BaseCRLNumber := ExtractBaseCRLNumber(CRL^);
          if BaseCRLNumber < 0 then
            BaseCRLNumber := CRLNumber;
          ExtractRevokedCertificates(CRL^,RCList);
          NewCRL := False;
        end else begin
          New(CRL);
          FillChar(CRL^,SizeOf(TASN1Struct),0);
          NewCRL := True;
        end;
        RC := RCList.Add;
        RC.userCertificate := Serial;
        RC.revocationDate := Now - FHoursOffsetFromGMT/24;
        RC.ReasonCode := Reason;
        RC.InvalidityDate := InvalidityDate - FHoursOffsetFromGMT/24;
        RC.HoldInstruction := HoldInstruction;
        ComposeCRL(CRL^,
                   SignatureCert,
                   Now - FHoursOffsetFromGMT/24,
                   NextCRLUpdate - FHoursOffsetFromGMT/24,
                   RCList,CRLNumber,BaseCRLNumber,IDPs[I]);
        RCList.Free;
        {$IFDEF SHA1}
        RSASignCRL(CRL^,SignatureCert,PrivKey,haSHA1,haSHA1,seEMSA3);
        Result := True;
        {$ELSE  SHA1}
        Result := False;
        {$ENDIF SHA1}
        if NewCRL then begin
          AddCRL(CRL^,Status);
          DisposeASN1Struct(CRL^);
          Dispose(CRL);
        end;
        Inc(CRLNumber);
      end;
    end;
  end;
end;

procedure TX509TrustedCertificates.RSASignSaveToFile(AFileName: TFileName;
  const MyCert: TASN1Struct; const MyPrivKey: TIFPrivateKey);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName,fmCreate or fmShareExclusive);
  try
    RSASignSaveToStream(FS,MyCert,MyPrivKey);
  finally
    FS.Free;
  end;
end;

procedure TX509TrustedCertificates.RSASignSaveToStream(AStream: TStream;
  const MyCert: TASN1Struct; const MyPrivKey: TIFPrivateKey);
{$IFDEF SHA512}
{$IFDEF SHA256}
var
  MS: TMemoryStream;
{$ENDIF SHA256}
{$ENDIF SHA512}
begin
{$IFDEF SHA512}
{$IFDEF SHA256}
  if not VerifyRSAPrivateKey(MyCert,MyPrivKey) then
    raise Exception.Create('Invalid Certificate / Private key');
  UpdatePolicies;
  CopyASN1Struct(FCerts.Contents^.Items[0].Contents^.Items[0].Contents^.Items[0],MyCert);
  MS := TMemoryStream.Create;
  try
    RSASignStruct(FCerts,{MyCert,}MyPrivKey,haSHA256,haSHA512,seEMSA4);
    FModified := False;
    ASN1ToStream(FCerts,AStream);
  finally
    MS.Free;
  end;
{$ELSE  SHA256}
  raise Exception.Create('TX509TrustedCertificates.RSASignSaveToStream: Action requires SHA-256');
{$ENDIF SHA256}
{$ELSE  SHA512}
  raise Exception.Create('TX509TrustedCertificates.RSASignSaveToStream: Action requires SHA-512');
{$ENDIF SHA512}
end;

procedure TX509TrustedCertificates.SaveToFile(AFileName: TFileName);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName,fmCreate or fmShareExclusive);
  try
    SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TX509TrustedCertificates.SaveToStream(AStream: TStream);
begin
  if Modified then
    raise Exception.Create('Cannot save modified list: Invalid signature');  
  FLock.Acquire;
  try
    ASN1ToStream(FCerts,AStream);
  finally
    FLock.Release;
  end;
end;

procedure TX509TrustedCertificates.SetCerts(index: Integer;
  const Value: TASN1Struct);
begin
  if ASN1StructCompare(Value,FCerts.Items[0].Items[1].Items[index]^) then
    Exit;
  FCerts.Items[0].Items[1].Contents^.Items[index].ReadOnly := True;
  FCerts.Items[0].Items[1].Contents^.Items[index].Assign(Value);
  FModified := True;
end;

procedure TX509TrustedCertificates.SetCRLs(index: Integer;
  const Value: TASN1Struct);
begin
  if ASN1StructCompare(Value,FCerts.Contents^.Items[0].Contents^.Items[2].Contents^.Items[index]) then
    Exit;
  CopyASN1Struct(FCerts.Contents^.Items[0].Contents^.Items[2].Contents^.Items[index],Value);
  FModified := True;
end;

procedure TX509TrustedCertificates.SetHoursOffsetFromGMT(
  const Value: Double);
begin
  FHoursOffsetFromGMT := Value;
end;

procedure TX509TrustedCertificates.SetTrustedCACertificates(
  const Value: TX509TrustedCertificates);
begin
{$IFDEF D5UP}
  if Assigned(FTrustedCACertificates) then
    RemoveFreeNotification(FTrustedCACertificates);
{$ENDIF D5UP}
  FTrustedCACertificates := Value;
  if Assigned(FTrustedCACertificates) then
    FreeNotification(FTrustedCACertificates);
end;

function TX509TrustedCertificates.ValidateMyCert(
  const MyPrivKey: TIFPrivateKey): Boolean;
var
  PK: TIFPublicKey;
begin
  FillChar(PK,SizeOf(PK),0);
  ExtractSubjectRSAPublicKey(FCerts.Contents^.Items[0].Contents^.Items[0],PK);
  Result := ValidateIFPrivateKey(MyPrivKey,PK);
  DisposeIFPublicKey(PK);
end;

function TX509TrustedCertificates.VerifyCAChain(const Cert: TASN1Struct;
  var Code: TCertStatusCode;
  PolicyMappings, AcceptedPolicies: TStrings;
  NC: TNameConstraintsSyntax; AllowExpired: Boolean): Boolean;
var
  CACert: PASN1Struct;
  aCert: PASN1Struct;
  Level, MaxLevel, I, NCLevel: Integer;
  Ext: TX509Extension;
  S: ^TASN1Struct;
  KeyUsage: TKeyUsage;
  NetscapeKeyUsage, NetscapeKeyUsageCA: TNetscapeKeyUsage;
  PolMapLevel: Integer;
  OIDs: TStringList;
  OID: string;
  vNC: TNameConstraintsSyntax;
begin
  aCert := @Cert;
  Level := 0;
  NCLevel := 0;
  PolMapLevel := -1;
  repeat
    if not FindCACert(aCert^,CACert) then begin
      if Assigned(FTrustedCACertificates) then
        Result := FTrustedCACertificates.VerifyCAChain(aCert^,Code)
      else
        Result := False;
      if not Result then begin
        Result := Level > 0;
        if Result then
          Code := crcOK
        else
          Code := crcCANotTrusted;
      end;
      Break;
    end else if ASN1StructCompare(aCert^,CACert^) then begin
      Result := True;
      Code := crcOK;
      Break;
    end else begin
      Result := (AllowExpired or CheckValidity(CACert^,FHoursOffsetFromGMT)) and
                not IsRevoked(CACert^);
      if not Result then
        Code := crcCAExpiredOrRevoked
      else begin
        Ext := nil;
        if ExtractNamedExtension(CACert^,id_ce_basicConstraints,Ext) = E_OK then begin
          if ASN1StructAssigned(Ext.extnValue) then begin
            if Ext.extnValue.Contents.ItemCount > 0 then begin
              if Ext.extnValue.Contents^.Items[0].Tag = V_ASN1_INTEGER then
                S := @Ext.extnValue.Contents^.Items[0]
              else if Ext.extnValue.Contents^.Items[0].Tag = V_ASN1_BOOLEAN then begin
                Result := (Ext.extnValue.Contents^.Items[0].Length = 1) and
                          (Ext.extnValue.Contents^.Items[0].Content[0] = #$FF);
                if not Result then
                  Code := crcConstraintFault;
                if Result and (Ext.extnValue.Contents.ItemCount > 1) and
                   (Ext.extnValue.Contents^.Items[1].Tag = V_ASN1_INTEGER) then
                  S := @Ext.extnValue.Contents^.Items[1]
                else
                  S := nil;
              end else
                S := nil;
              if Assigned(S) and (S^.Length <= 4) then begin
                MaxLevel := 0;
                for I := 0 to S^.Length -1 do
                  MaxLevel := (MaxLevel shl 8) or Byte(S^.Content[I]);
                Result := Level <= MaxLevel;
                if not Result then
                  Code := crcConstraintFault;
              end;
            end else begin
              Result := False;
              Code := crcConstraintFault;
            end;
          end;
        end;
        if Result and Assigned(NC) and
           (ExtractNamedExtension(CACert^,id_ce_nameConstraints,Ext) = E_OK) then begin
          if NCLevel = 0 then
            NC.AssignStruct(Ext.extnValue)
          else begin
            vNC := TNameConstraintsSyntax.Create(nil,nil);
            try
              vNC.AssignStruct(Ext.extnValue);
              vNC.Combine(NC);
            finally
              vNC.Free;
            end;
          end;
          Inc(NCLevel);
        end;
        if Result and
           (ExtractNamedExtension(CACert^,id_ce_keyUsage,Ext) = E_OK) then begin
          KeyUsage := [];
          Move(Ext.extnValue.Content[1],KeyUsage,Ext.extnValue.Length - 1);
          Result := keyCertSign in KeyUsage;
          if not Result then
            Code := crcInvalidKeyUsage;
        end;
        if Result and
           (ExtractNamedExtension(aCert^,netscape_cert_type,Ext) = E_OK) then begin
          NetscapeKeyUsage := [];
          Move(Ext.extnValue.Content[1],NetscapeKeyUsage,1);
          if NetscapeKeyUsage*[nkuSslClient,nkuSslServer,nkuSMime,nkuObjectSigning] <> [] then begin
            if ExtractNamedExtension(CACert^,netscape_cert_type,Ext) = E_OK then begin
              NetscapeKeyUsageCA := [];
              Move(Ext.extnValue.Content[1],NetscapeKeyUsageCA,1);
              if nkuSslClient in NetscapeKeyUsage then
                Result := Result and (nkuSslCA in NetscapeKeyUsageCA);
              if nkuSslServer in NetscapeKeyUsage then
                Result := Result and (nkuSslCA in NetscapeKeyUsageCA);
              if nkuSMime in NetscapeKeyUsage then
                Result := Result and (nkuSMimeCA in NetscapeKeyUsageCA);
              if nkuObjectSigning in NetscapeKeyUsage then
                Result := Result and (nkuObjectSigningCA in NetscapeKeyUsageCA);
              if not Result then
                Code := crcInvalidKeyUsage;
            end;
          end;
        end;
        if Result and Assigned(PolicyMappings) and
           (ExtractNamedExtension(CACert^,id_ce_policyMappings,Ext) = E_OK) then begin
          for I := 0 to Ext.extnValue.ItemCount - 1 do begin
            Result := Ext.extnValue.Items[I].ItemCount = 2;
            if not Result then begin
              Code := crcSyntax;
              Break;
            end;
            PolicyMappings.Values[Ext.extnValue.Items[I].Items[1].ContentAsOID] :=
              Ext.extnValue.Items[I].Items[0].ContentAsOID;
            if PolMapLevel < 0 then
              PolMapLevel := Level;
          end;
        end;
        if Result and Assigned(AcceptedPolicies) and
           (ExtractNamedExtension(CACert^,id_ce_certificatePolicies,Ext) = E_OK) then begin
          if (AcceptedPolicies.Count = 1) and (AcceptedPolicies[0] = anyPolicy) then begin
            AcceptedPolicies.Clear;
            for I := 0 to Ext.extnValue.ItemCount - 1 do
              AcceptedPolicies.Add(Ext.extnValue.Items[I]^.Items[0]^.ContentAsOID);
          end else begin
            OIDs := TStringList.Create;
            try
              for I := 0 to Ext.extnValue.ItemCount - 1 do
                OIDs.Add(Ext.extnValue.Items[I]^.Items[0]^.ContentAsOID);
              for I := AcceptedPolicies.Count - 1 downto 0 do begin
                OID := AcceptedPolicies[0];
                if OIDs.IndexOf(OID) < 0 then begin
                  repeat
                    OID := PolicyMappings.Values[OID];
                  until (OID = '') or (OIDs.IndexOf(OID) >= 0);
                  if OID = '' then
                    AcceptedPolicies.Delete(I)
                  else
                    AcceptedPolicies[I] := OID;
                end;
              end;
            finally
              OIDs.Free;
            end;
            Result := AcceptedPolicies.Count > 0;
          end;
        end;
        if Result and Assigned(PolicyMappings) and
           (ExtractNamedExtension(CACert^,id_ce_policyConstraints,Ext) = E_OK) then begin
          if Ext.extnValue.ItemCount = 1 then begin
            if Ext.extnValue.Items[0].Tag = 1 then begin
              if PolMapLevel > -1 then
                if Level - PolMapLevel > Ext.extnValue.Items[0].ContentAsInteger then begin
                  Result := False;
                  Code := crcConstraintFault;
                end;
            end;
          end else if Ext.extnValue.ItemCount = 2 then begin
            if Ext.extnValue.Items[1].Tag = 1 then begin
              if PolMapLevel > -1 then
                if Level - PolMapLevel > Ext.extnValue.Items[1].ContentAsInteger then begin
                  Result := False;
                  Code := crcConstraintFault;
                end;
            end else if Ext.Critical then begin
              Result := False;
              Code := crcSyntax;
            end;
          end else if Ext.Critical then begin
            Result := False;
            Code := crcSyntax;
          end;
        end;
        Ext.Free;
        aCert := CACert;
      end;
    end;
    Inc(Level);
  until not Result;
end;

function TX509TrustedCertificates.ValidateMyCert(
  const MyPrivKey: TDLPrivateKey): Boolean;
var
  PK: TDLPublicKey;
begin
  FillChar(PK,SizeOf(PK),0);
  ExtractSubjectDSAPublicKey(FCerts.Contents^.Items[0].Contents^.Items[0],PK);
  Result := ValidateDLPrivateKey(MyPrivKey,PK);
  DisposeDLPublicKey(PK);
end;

function TX509TrustedCertificates.VerifyCAChain(const Cert,
  TopCert: TASN1Struct; var Code: TCertStatusCode;
  PolicyMappings, AcceptedPolicies: TStrings;
  NC: TNameConstraintsSyntax; AllowExpired: Boolean): Boolean;
var
  CACert: PASN1Struct;
  aCert: PASN1Struct;
  Level, MaxLevel, MaxLevel2, I, NCLevel: Integer;
  Ext: TX509Extension;
  S: ^TASN1Struct;
  KeyUsage: TKeyUsage;
  PolMapLevel: Integer;
  OID: string;
  OIDs: TStringList;
  vNC: TNameConstraintsSyntax;
begin
  aCert := @Cert;
  Level := 0;
  NCLevel := 0;
  Result := False;
  PolMapLevel := -1;
  repeat
    if CompareCertificate(aCert^,TopCert) then begin
      Code := crcTrusted;
      Result := True;
      Break;
    end;
    if not FindCACert(aCert^,CACert) then begin
      if Assigned(FTrustedCACertificates) then
        Result := FTrustedCACertificates.VerifyCAChain(aCert^,Code)
      else
        Result := False;
      if not Result then begin
        Result := Level > 0;
        if Result then
          Code := crcOK
        else
          Code := crcCANotTrusted;
      end;
      Break;
    end else if CompareCertificate(aCert^,CACert^) then begin
      Result := True;
      Code := crcOK;
      Break;
    end else begin
      Result := (AllowExpired or CheckValidity(CACert^,FHoursOffsetFromGMT)) and
                not IsRevoked(CACert^);
      if not Result then
        Code := crcCAExpiredOrRevoked
      else begin
        Ext := nil;
        if ExtractNamedExtension(CACert^,id_ce_basicConstraints,Ext) = E_OK then begin
          if ASN1StructAssigned(Ext.extnValue) then begin
            if Ext.extnValue.Contents.ItemCount > 0 then begin
              if Ext.extnValue.Contents^.Items[0].Tag = V_ASN1_INTEGER then
                S := @Ext.extnValue.Contents^.Items[0]
              else if Ext.extnValue.Contents^.Items[0].Tag = V_ASN1_BOOLEAN then begin
                Result := (Ext.extnValue.Contents^.Items[0].Length = 1) and
                          (Ext.extnValue.Contents^.Items[0].Content[0] = #$FF);
                if not Result then
                  Code := crcConstraintFault;
                if Result and (Ext.extnValue.Contents.ItemCount > 1) and
                   (Ext.extnValue.Contents^.Items[1].Tag = V_ASN1_INTEGER) then
                  S := @Ext.extnValue.Contents^.Items[1]
                else
                  S := nil;
              end else
                S := nil;
              if Assigned(S) and (S^.Length <= 4) then begin
                MaxLevel := 0;
                for I := 0 to S^.Length -1 do
                  MaxLevel := (MaxLevel shl 8) or Byte(S^.Content[I]);
                Result := Level <= MaxLevel;
                if not Result then
                  Code := crcConstraintFault
                else begin
                  // Verify that the pathLenConstraint, if present, doesn't
                  // exceed the max set by the issuer:
                  if ExtractNamedExtension(aCert^,id_ce_basicConstraints,Ext) = E_OK then begin
                    if ASN1StructAssigned(Ext.extnValue) then begin
                      if Ext.extnValue.Contents.ItemCount > 0 then begin
                        if Ext.extnValue.Contents^.Items[0].Tag = V_ASN1_INTEGER then
                          S := @Ext.extnValue.Contents^.Items[0]
                        else if Ext.extnValue.Contents^.Items[0].Tag = V_ASN1_BOOLEAN then begin
                          if (Ext.extnValue.Contents^.Items[0].Length = 1) and
                             (Ext.extnValue.Contents^.Items[0].Content[0] = #$FF) and
                             (Ext.extnValue.Contents.ItemCount > 1) and
                             (Ext.extnValue.Contents^.Items[1].Tag = V_ASN1_INTEGER) then
                            S := @Ext.extnValue.Contents^.Items[1]
                          else
                            S := nil;
                        end else
                          S := nil;
                        if Assigned(S) and (S^.Length <= 4) then begin
                          MaxLevel2 := 0;
                          for I := 0 to S^.Length -1 do
                            MaxLevel2 := (MaxLevel2 shl 8) or Byte(S^.Content[I]);
                          Result := MaxLevel2 < MaxLevel;
                          if not Result then
                            Code := crcConstraintFault;
                        end;
                          // else the certificate is not a CA certificate
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        if Result and Assigned(NC) and
           (ExtractNamedExtension(CACert^,id_ce_nameConstraints,Ext) = E_OK) then begin
          vNC := TNameConstraintsSyntax.Create(nil,nil);
          try
            if NCLevel = 0 then
              vNC.AssignStruct(Ext.extnValue)
            else begin
              vNC.AssignStruct(Ext.extnValue);
              vNC.Combine(NC);
            end;
          finally
            vNC.Free;
          end;
          Inc(NCLevel);
        end;
        if Result and
           (ExtractNamedExtension(CACert^,id_ce_keyUsage,Ext) = E_OK) then begin
          KeyUsage := [];
          Move(Ext.extnValue.Content[1],KeyUsage,Ext.extnValue.Length - 1);
          Result := [keyCertSign,cRLSign] <= KeyUsage;
          if not Result then
            Code := crcInvalidKeyUsage;
        end;
        if Result and Assigned(PolicyMappings) and
           (ExtractNamedExtension(CACert^,id_ce_policyMappings,Ext) = E_OK) then begin
          for I := 0 to Ext.extnValue.ItemCount - 1 do begin
            Result := Ext.extnValue.Items[I].ItemCount = 2;
            if not Result then begin
              Code := crcSyntax;
              Break;
            end;
            PolicyMappings.Values[Ext.extnValue.Items[I].Items[1].ContentAsOID] :=
              Ext.extnValue.Items[I].Items[0].ContentAsOID;
            if PolMapLevel < 0 then
              PolMapLevel := Level;
          end;
        end;
        if Result and Assigned(AcceptedPolicies) and
           (ExtractNamedExtension(CACert^,id_ce_certificatePolicies,Ext) = E_OK) then begin
          if (AcceptedPolicies.Count = 1) and (AcceptedPolicies[0] = anyPolicy) then begin
            AcceptedPolicies.Clear;
            for I := 0 to Ext.extnValue.ItemCount - 1 do
              AcceptedPolicies.Add(Ext.extnValue.Items[I]^.Items[0]^.ContentAsOID);
          end else begin
            OIDs := TStringList.Create;
            try
              for I := 0 to Ext.extnValue.ItemCount - 1 do
                OIDs.Add(Ext.extnValue.Items[I]^.Items[0]^.ContentAsOID);
              for I := AcceptedPolicies.Count - 1 downto 0 do begin
                OID := AcceptedPolicies[0];
                if OIDs.IndexOf(OID) < 0 then begin
                  repeat
                    OID := PolicyMappings.Values[OID];
                  until (OID = '') or (OIDs.IndexOf(OID) >= 0);
                  if OID = '' then
                    AcceptedPolicies.Delete(I)
                  else
                    AcceptedPolicies[I] := OID;
                end;
              end;
            finally
              OIDs.Free;
            end;
            Result := AcceptedPolicies.Count > 0;
          end;
        end;
        if Result and Assigned(PolicyMappings) and
           (ExtractNamedExtension(CACert^,id_ce_policyConstraints,Ext) = E_OK) then begin
          if Ext.extnValue.ItemCount = 1 then begin
            if Ext.extnValue.Items[0].Tag = 1 then begin
              if PolMapLevel > -1 then
                if Level - PolMapLevel > Ext.extnValue.Items[0].ContentAsInteger then begin
                  Result := False;
                  Code := crcConstraintFault;
                end;
            end;
          end else if Ext.extnValue.ItemCount = 2 then begin
            if Ext.extnValue.Items[1].Tag = 1 then begin
              if PolMapLevel > -1 then
                if Level - PolMapLevel > Ext.extnValue.Items[1].ContentAsInteger then begin
                  Result := False;
                  Code := crcConstraintFault;
                end;
            end else if Ext.Critical then begin
              Result := False;
              Code := crcSyntax;
            end;
          end else if Ext.Critical then begin
            Result := False;
            Code := crcSyntax;
          end;
        end;
        Ext.Free;
        aCert := CACert;
      end;
    end;
    Inc(Level);
  until not Result;
end;

function TX509TrustedCertificates.VerifyCertificate(
  const Cert: TASN1Struct; ExplicitTrust: Boolean;
  var Status: TCertStatusCode): Integer;
begin
  FLock.Acquire;
  try
    Result := InternalVerifyCertificate(Cert,ExplicitTrust,Status,False);
  finally
    FLock.Release;
  end;
end;

procedure TX509TrustedCertificates.DSASignSaveToFile(AFileName: TFileName;
  const MyCert: TASN1Struct; const MyPrivKey: TDLPrivateKey);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName,fmOpenWrite or fmShareExclusive);
  try
    DSASignSaveToStream(FS,MyCert,MyPrivKey);
  finally
    FS.Free;
  end;
end;

procedure TX509TrustedCertificates.DSASignSaveToStream(AStream: TStream;
  const MyCert: TASN1Struct; const MyPrivKey: TDLPrivateKey);
{$IFDEF SHA1}
var
  MS: TMemoryStream;
{$ENDIF SHA1}
begin
{$IFDEF SHA1}
  if not VerifyDLPrivateKey(MyCert,MyPrivKey) then
    raise Exception.Create('Invalid Certificate / Private key');
  UpdatePolicies;
  CopyASN1Struct(FCerts.Contents^.Items[0].Contents^.Items[0].Contents^.Items[0],MyCert);
  MS := TMemoryStream.Create;
  try
    DSASignStruct(FCerts,{MyCert,}MyPrivKey);
    FModified := False;
    ASN1ToStream(FCerts,AStream);
  finally
    MS.Free;
  end;
{$ELSE  SHA1}
  raise Exception.Create('TX509TrustedCertificates.DSASignSaveToStream: Unsupported algorithm');
{$ENDIF SHA1}
end;

function TX509TrustedCertificates.RevokeCertificate(
  const Cert: TASN1Struct; Reason: CRLReason;
  HoldInstruction: string; InvalidityDate: TDateTime;
  NextCRLUpdate: TDateTime; const PrivKey: TDLPrivateKey): Boolean;
var
  Res: Boolean;
  CACert, CRL: PASN1Struct;
  IDPs: TX509IssuingDistributionPoints;
  CRLs: TASN1Struct;
  CRLNumber, BaseCRLNumber, N, I: Integer;
  RCList: TX509RevokedCertificates;
  RC: TX509RevokedCertificate;
  Serial: string;
  Status: TCertStatusCode;
  NewCRL: Boolean;
  ReasonFlags: TX509ReasonFlags;
begin
  Result := False;
  Res := ExtractSerial(Cert,Serial) = E_OK;
  Assert(Res);
  CACert := nil;
  if FindCACert(Cert,CACert) then begin
    ReasonFlags := [];
    case Reason of
      keyCompromise:        ReasonFlags := [rfKeyCompromise];
      cACompromise:         ReasonFlags := [rfCACompromise];
      affiliationChanged:   ReasonFlags := [rfAffilationChanged];
      superseded:           ReasonFlags := [rfSuperseeded];
      cessationOfOperation: ReasonFlags := [rfcessationOfOperation];
      certificateHold:      ReasonFlags := [rfcertificateHold];
      removeFromCRL:        ReasonFlags := [LowReasonFlag..High(TX509ReasonFlag)];
    end;
    if SuggestIssuingDistributionPoints(CACert^,ReasonFlags,IDPs) >= 0 then begin
      FillChar(CRLs,SizeOf(CRLs),0);
      CRLNumber := 1;
      if FindCRLs(SignatureCert,CRLs) >= 0 then begin
        for I := 0 to CRLs.ItemCount - 1 do begin
          N := ExtractCRLNumber(CRLs.Items[I]^);
          if N + 1 > CRLNumber then
            CRLNumber := N + 1;
        end;
      end;
      CRLs.Free;
      for I := 0 to Length(IDPs) - 1 do begin
        RCList := TX509RevokedCertificates.Create;
        BaseCRLNumber := CRLNumber;
        if FindCRL(SignatureCert,IDPs[I],CRL) then begin
          BaseCRLNumber := ExtractBaseCRLNumber(CRL^);
          if BaseCRLNumber < 0 then
            BaseCRLNumber := CRLNumber;
          ExtractRevokedCertificates(CRL^,RCList);
          NewCRL := False;
        end else begin
          New(CRL);
          FillChar(CRL^,SizeOf(TASN1Struct),0);
          NewCRL := True;
        end;
        RC := RCList.Add;
        RC.userCertificate := Serial;
        RC.revocationDate := Now - FHoursOffsetFromGMT/24;
        RC.ReasonCode := Reason;
        RC.InvalidityDate := InvalidityDate - FHoursOffsetFromGMT/24;
        RC.HoldInstruction := HoldInstruction;
        ComposeCRL(CRL^,
                   SignatureCert,
                   Now - FHoursOffsetFromGMT/24,
                   NextCRLUpdate - FHoursOffsetFromGMT/24,
                   RCList,CRLNumber,BaseCRLNumber,IDPs[I]);
        RCList.Free;
{$IFDEF SHA1}
        DSASignCRL(CRL^,SignatureCert,PrivKey);
        Result := True;
{$ELSE  SHA1}
        Result := False;
{$ENDIF SHA1}
        if NewCRL then begin
          AddCRL(CRL^,Status);
          DisposeASN1Struct(CRL^);
          Dispose(CRL);
        end;
        Inc(CRLNumber);
      end;
    end;
  end;
end;

function TX509TrustedCertificates.FindCert(const KeyIdentifier: string;
  var Cert: PASN1Struct): Boolean;
var
  I: Integer;
  S: string;
begin
  Result := False;
  if KeyIdentifier <> '' then begin
    Cert := nil;
    for I := 0 to Count - 1 do begin
      Cert := GetPCerts(I);
      S := ExtractSubjectKeyIdentifier(Cert^);
      if (Length(KeyIdentifier) = Length(S)) and
         CompareMem(Pointer(KeyIdentifier),Pointer(S),Length(S)) then begin
        Result := True;
        Exit;
      end;
    end;
  end;
  Cert := nil;
end;

procedure TX509TrustedCertificates.SetSignatureCert(
  const Value: TASN1Struct);
begin
  FCerts.Contents^.Items[0].Contents^.Items[0].Contents^.Items[0].Assign(Value);
  FCerts.Contents^.Items[0].Contents^.Items[0].Contents^.Items[0].CalculateLength;
end;

function TX509TrustedCertificates.FindCert(const Serial: string;
  const Issuer: TX501Name; var Cert: PASN1Struct): Boolean;
var
  I: Integer;
  S: string;
  N: TX501Name;
begin
  Result := False;
  Cert := nil;
  for I := 0 to Count - 1 do begin
    Cert := GetPCerts(I);
    System.Finalize(N);
    FillChar(N,SizeOf(N),0);
    if ExtractSerial(Cert^,S) = E_OK then
      if ExtractIssuer(Cert^,N) = E_OK then
        if (Length(Serial) = Length(S)) and
           CompareMem(Pointer(Serial),Pointer(S),Length(S)) and
           CompareName(Issuer,N) then begin
          Result := True;
          Exit;
        end;
  end;
  Cert := nil;
end;

function TX509TrustedCertificates.FindCert(const Serial: string;
  const Issuer: TASN1Struct; var Cert: PASN1Struct): Boolean;
var
  I: Integer;
  S: string;
  N: PASN1Struct;
begin
  Result := False;
  Cert := nil;
  for I := 0 to Count - 1 do begin
    Cert := GetPCerts(I);
    N := nil;
    if ExtractSerial(Cert^,S) = E_OK then
      if ExtractIssuerStruct(Cert^,N) = E_OK then
        if (Length(Serial) = Length(S)) and
           CompareMem(Pointer(Serial),Pointer(S),Length(S)) and
           ASN1StructCompare(Issuer,N^) then begin
          Result := True;
          Exit;
        end;
  end;
  Cert := nil;
end;

type
  THack = class(TASN1Struct);

function TX509TrustedCertificates.ImportFromCMS(
  var ContentInfo: TASN1Struct; ExplicitTrust: Boolean;
  var Status: TCertStatusCodes; AllowExpired: Boolean): Boolean;
var
  ContentType: TPKCS7DataType;
  C, Crt, F: PASN1Struct;
  I, J, PrevCount: Integer;
  S: TCertStatusCode;
  Added: TList;
begin
  Status := [];
  Result := CheckCMSSyntax(ContentInfo,
                           ContentType,
                           [pSignedData,pSignedAndEnvelopedData]);
  if Result then begin
    Updating;
    LockInstanceList;
    try
      PreAllocInstances(THack(ContentInfo).TotalCount);
      C := ContentInfo.FindField('/content//certificates');
      if Assigned(C) then begin
        Added := TList.Create;
        try
          Added.Capacity := C^.ItemCount;
          for I := 0 to C^.ItemCount - 1 do begin
            F := C^.Items[I];
            if ExplicitTrust or IsAllegedIssuer(F^,F^) then
              J := Added.Count
            else begin
              J := 0;
              while J < Added.Count do begin
                Crt := Added[J];
                if IsAllegedIssuer(F^,Crt^) then
                  Break;
                Inc(J);
              end;
            end;
            Added.Insert(J,F);
          end;
          while Added.Count > 0 do begin
            PrevCount := Added.Count;
            I := PrevCount - 1;
            while I >= 0 do begin
              F := Added[I];
              if F.ActualTag = V_ASN1_EOC then
                // SPC bug
                Added.Delete(I)
              else if AddCertificate(F^,ExplicitTrust,S,AllowExpired) >= 0 then begin
                Added.Delete(I);
                Include(Status,S);
              end else if S <> crcCANotTrusted then begin
                Include(Status,S);
                Added.Delete(I);
              end;
              Dec(I);
            end;
            if (Added.Count = PrevCount) and not ExplicitTrust then begin
              Include(Status,crcCANotTrusted);
              Break;
            end;
          end;
        finally
          Added.Free;
        end;
      end;
      C := ContentInfo.FindField('/content//crls');
      if Assigned(C) then begin
        for I := 0 to C^.ItemCount - 1 do begin
          F := C^.Items[I];
          AddCRL(F^,S);
          Include(Status,S);
        end;
      end;
    finally
      UnlockInstanceList;
      Updated;
    end;
    CertListLoaded;
    UpdateHijackers;
  end;
end;

function TX509TrustedCertificates.ImportFromCMSFile(
  AFileName: TFileName; ExplicitTrust: Boolean;
  var Status: TCertStatusCodes; AllowExpired: Boolean): Boolean;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName,fmOpenRead);
  try
    Result := ImportFromCMSStream(FS,ExplicitTrust,Status,AllowExpired);
  finally
    FS.Free;
  end;
end;

function TX509TrustedCertificates.ImportFromCMSStream(
  AStream: TStream; ExplicitTrust: Boolean;
  var Status: TCertStatusCodes; AllowExpired: Boolean): Boolean;
var
  C: TASN1Struct;
  B: Byte;
  MS: TMemoryStream;
  Buf, S, BD, ED: string;
begin
  C := nil;
  try
    NewSignedDataStruct(C);
    try                 
      with C.FindField('/content//certificates')^.Template do begin
        DisposeContent;
        ChoiceCount := 0;
        Persistent := False;
        TypeName := 'SEQUENCE';
      end;
      C.ReadOnly := True;
      Result := AStream.Read(B,1) = 1;
      if Result then begin
        AStream.Seek(-1,soFromCurrent);
        if B = $30 then
          C.LoadFromStream(AStream,fmtDER)
        else begin
          MS := TMemoryStream.Create;
          try
            SetLength(Buf,AStream.Size);
            AStream.Read(Pointer(Buf)^,Length(Buf));
            S := MIME64ToStrEx(BD,ED,Buf);
            if S = '' then
              S := MIME64ToStr(Buf);
            MS.Write(Pointer(S)^,Length(S));
            MS.Position := 0;
            C.LoadFromStream(MS,fmtDER);
          finally
            MS.Free;
          end;
        end;
        Result := ImportFromCMS(C,ExplicitTrust,Status,AllowExpired);
      end;
    except
      Result := False;
    end;
  finally
    C.Free;
  end;
end;

procedure TX509TrustedCertificates.ExportChainToCMS(
  var ContentInfo: TASN1Struct; EndIndex: Integer; Sort: Boolean;
  IncludeRoot: Boolean);
var
  Res: Boolean;
  ContentType: TPKCS7DataType;
  I: Integer;
  C: TASN1Struct;
  CL, CRLs, CA: PASN1Struct;
begin
  C := Certs[EndIndex];
  Res := True;
  if ContentInfo = nil then
    NewSignedDataStruct(ContentInfo)
  else
    Res := CheckCMSSyntax(ContentInfo,
                          ContentType,
                          [pSignedData,pSignedAndEnvelopedData]);
  Assert(Res);
  with ContentInfo.FindField('/content//certificates')^.Template do begin
    DisposeContent;
    ChoiceCount := 0;
    Persistent := False;
    TypeName := 'SEQUENCE';
  end;
  CL := ContentInfo.FindField('/content//certificates');
  CRLs := ContentInfo.FindField('/content//crls');
  if IncludeRoot then begin
    CL.AddField('','',C);
    CA := nil;
    while FindCACert(C,CA) do begin
      CL.AddField('','',CA^);
      C := nil;
      try
        if FindCRLs(CA^,C) > 0 then
          for I := 0 to C.ItemCount - 1 do
            CRLs.AddField('','',C.Items[I]^);
      finally
        C.Free;
      end;
      C := CA^;
    end;
  end else begin
    CA := nil;
    while FindCACert(C,CA) and not IsAllegedIssuer(C,C) do begin
      CL.AddField('','',C);
      C := nil;
      try
        if FindCRLs(CA^,C) > 0 then
          for I := 0 to C.ItemCount - 1 do
            CRLs.AddField('','',C.Items[I]^);
      finally
        C.Free;
      end;
      C := CA^;
    end;
    if CA = nil then
      CL.AddField('','',C);
  end;
  if Sort then begin
    CL.SortSET;
    CRLs.SortSET;
  end;
  ContentInfo.CalculateLength;
end;

procedure TX509TrustedCertificates.ExportChainToCMSFile(
  AFileName: TFileName; EndIndex: Integer; Sort: Boolean; IncludeRoot: Boolean);
var
  FS: TFileStream;
begin
  if FileExists(AFileName) then                 
    FS := TFileStream.Create(AFileName,fmOpenWrite)
  else
    FS := TFileStream.Create(AFileName,fmCreate);
  try
    FS.Size := 0;
    ExportChainToCMSStream(FS,EndIndex,Sort,IncludeRoot);
  finally
    FS.Free;
  end;
end;

procedure TX509TrustedCertificates.ExportChainToCMSStream(AStream: TStream;
  EndIndex: Integer; Sort: Boolean; IncludeRoot: Boolean);
var
  C: TASN1Struct;
begin
  C := nil;
  try
    ExportChainToCMS(C,EndIndex,Sort,IncludeRoot);
    C.SaveToStream(AStream,fmtDER);
  finally
    C.Free;
  end;
end;

procedure TX509TrustedCertificates.ExportAllToCMS(
  var ContentInfo: TASN1Struct; NoSyntaxCheck: Boolean);
var
  Res: Boolean;
  ContentType: TPKCS7DataType;
  I: Integer;
  CL: PASN1Struct;
  SL: TStringList;
  SS: TStringStream;
begin
  Res := True;
  if not NoSyntaxCheck then begin
    if ContentInfo = nil then
      NewSignedDataStruct(ContentInfo)
    else
      Res := CheckCMSSyntax(ContentInfo,
                            ContentType,
                            [pSignedData,pSignedAndEnvelopedData]);
    Assert(Res);
  end;

  FLock.Acquire;
  try
    CL := ContentInfo.FindField('/content//certificates');
    SL := TStringList.Create;
    try
      for I := 0 to Count - 1 do begin
        SS := TStringStream.Create('');
        try
          Certs[I].SaveToStream(SS,fmtDER);
          SL.Add(SS.DataString);
        finally
          SS.Free;
        end;
      end;
      SL.Sort;
      for I := 0 to SL.Count - 1 do begin
        SS := TStringStream.Create(SL[I]);
        try
          CL.AddField^.LoadFromStream(SS,fmtDER);
        finally
          SS.Free;
        end;
      end;
    finally
      SL.Free;
    end;

    CL := ContentInfo.FindField('/content//cRLs');
    SL := TStringList.Create;
    try
      for I := 0 to CRLCount - 1 do begin
        SS := TStringStream.Create('');
        try
          CRLs[I].SaveToStream(SS,fmtDER);
          SL.Add(SS.DataString);
        finally
          SS.Free;
        end;
      end;
      SL.Sort;
      for I := 0 to SL.Count - 1 do begin
        SS := TStringStream.Create(SL[I]);
        try
          CL.AddField^.LoadFromStream(SS,fmtDER);
        finally
          SS.Free;
        end;
      end;
    finally
      SL.Free;
    end;
  finally
    FLock.Release;
  end;

  ContentInfo.CalculateLength;
end;

procedure TX509TrustedCertificates.ExportAllToCMSFile(
  AFileName: TFileName);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName,fmCreate);
  try
    ExportAllToCMSStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TX509TrustedCertificates.ExportAllToCMSStream(AStream: TStream);
var
  C: TASN1Struct;
begin
  C := nil;
  try
    ExportAllToCMS(C);
    C.SaveToStream(AStream,fmtDER);
  finally
    C.Free;
  end;
end;

{$IFDEF SHA1_AND_MD5}
procedure TX509TrustedCertificates.ExportChainToTLS(
  var Certificate: PTLSHandshake; var Len: Integer; EndIndex: Integer);
var
  C: TASN1Struct;
begin
  C := nil;
  try
    ExportChainToCMS(C,EndIndex,False,True);
    Len := TLS_EncodeCertificate(C.FindField('/content//certificates')^,Certificate);
  finally
    C.Free;
  end;
end;

function TX509TrustedCertificates.ImportChainFromTLS(
  Certificate: PCertificate; Len: Integer; var EndCert: PASN1Struct;
  var Status: TCertStatusCodes): Boolean;
var
  L: Cardinal;
  RS: TReadStream;
  CL: TASN1Struct;
  C: PASN1Struct;
  I, Idx: Integer;
  S: TCertStatusCode;
  ELen: LongWord;
  ExplicitTrust, AllowExpired: Boolean;
begin
  Status := [];
  L := TLS_DecodeCertificateLength(Certificate,Len);
  Result := L > 0;
  if Result then try
    RS := TReadStream.Create(Certificate^.certificate_list[3],L);
    try
      Updating;
      CL := nil;
      try
        NewComposeASN1Struct(CL,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
        while RS.Position < RS.Size-3 do begin
          C := CL.AddField;
          ELen := 0;
          RS.Read(ELen,3);
          L := ByteSwap(ELen) shr 8 + Cardinal(RS.Position);
          C.LoadFromStream(RS,fmtDER);
          Result := Cardinal(RS.Position) = L;
          if not Result then Break;
        end;
        Idx := -1;
        if Result then begin
          ExplicitTrust := False;
          AllowExpired := False;
          if Assigned(FBeforeImportTLSCert) then
            FBeforeImportTLSCert(Self,CL.Items[0]^,ExplicitTrust,AllowExpired);
          Idx := IndexOfCert(CL.Items[0]^);
          Result := (Idx >= 0) and
                    (AllowExpired or CheckValidity(CL.Items[0]^,FHoursOffsetFromGMT));
          if Result then
            Status := [crcTrusted]
          else begin
            Idx := AddCertificate(CL.Items[0]^,ExplicitTrust,S,AllowExpired);
            Result := Idx >= 0;
            if not Result then begin
              Result := True;
              for I := CL.ItemCount - 1 downto 0 do begin
                Idx := AddCertificate(CL.Items[I]^,ExplicitTrust,S,AllowExpired);
                Result := Result and (Idx >= 0);
                Include(Status,S);
              end
            end else
              Include(Status,S);
          end;
        end;
        if Result and (Idx >= 0) then
          EndCert := GetPCerts(Idx)
        else
          EndCert := nil;
      finally
        CL.Free;
        Updated;
      end;
      CertListLoaded;
      UpdateHijackers;
    finally
      RS.Free;
    end;
  except
    Result := False;
    Status := [crcSyntax];
  end;
end;
{$ENDIF SHA1_AND_MD5}

procedure TX509TrustedCertificates.SetLeastKeyBitSize(
  const Value: Integer);
begin
  FLeastKeyBitSize := Value;
end;

procedure TX509TrustedCertificates.ECDSASignSaveToFile(
  AFileName: TFileName; const MyCert: TASN1Struct;
  const MyPrivKey: TECPrivateKey);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName,fmOpenWrite or fmShareExclusive);
  try
    ECDSASignSaveToStream(FS,MyCert,MyPrivKey);
  finally
    FS.Free;
  end;
end;

procedure TX509TrustedCertificates.ECDSASignSaveToStream(AStream: TStream;
  const MyCert: TASN1Struct; const MyPrivKey: TECPrivateKey);
{$IFDEF SHA1}
var
  MS: TMemoryStream;
{$ENDIF SHA1}
begin
{$IFDEF SHA1}
  if not VerifyECPrivateKey(MyCert,MyPrivKey) then
    raise Exception.Create('Invalid Certificate / Private key');
  UpdatePolicies;
  CopyASN1Struct(FCerts.Contents^.Items[0].Contents^.Items[0].Contents^.Items[0],MyCert);
  MS := TMemoryStream.Create;
  try
    ECDSASignStruct(FCerts,{MyCert,}MyPrivKey);
    FModified := False;
    ASN1ToStream(FCerts,AStream);
  finally
    MS.Free;
  end;
{$ELSE  SHA1}
  raise Exception.Create('TX509TrustedCertificates.ECDSASignSaveToStream: Unsupported algorithm');
{$ENDIF SHA1}
end;

function TX509TrustedCertificates.FindCert(const SignatureAlgorithms,
  PKAlgorithms: array of string; const KeyUsage: TKeyUsage;
  var StartIndex: Integer; var Cert: PASN1Struct): Boolean;
begin
  Result := FindCert(SignatureAlgorithms,PKAlgorithms,KeyUsage,[],StartIndex,Cert);
end;

function TX509TrustedCertificates.ExtractSubjectDHPublicKey(
  const Cert: TASN1Struct; var PublKey: TDLPublicKey): Boolean;
var
  CACert: PASN1Struct;
  CAKey: TDLPublicKey;
begin
  DisposeDLPublicKey(PublKey);
  Result := X509Base.ExtractSubjectDHPublicKey(Cert,PublKey) = E_OK;
  if Result then
    if not Assigned(PublKey.Params.P) then begin
      FillChar(CAKey,SizeOf(CAKey),0);
      try
        Result := FindCACert(Cert,CACert);
        if Result then begin
          Result := Self.ExtractSubjectDSAPublicKey(CACert^,CAKey);
          if Result then
            CopyDLSystemParams(PublKey.Params,CAKey.Params);
        end;
      finally
        DisposeDLPublicKey(CAKey);
      end;
    end;
end;

function TX509TrustedCertificates.ExtractSubjectDSAPublicKey(
  const Cert: TASN1Struct; var PublKey: TDLPublicKey): Boolean;
var
  CACert: PASN1Struct;
  CAKey: TDLPublicKey;
begin
  DisposeDLPublicKey(PublKey);
  Result := X509Base.ExtractSubjectDSAPublicKey(Cert,PublKey) = E_OK;
  if Result then
    if not Assigned(PublKey.Params.P) then begin
      FillChar(CAKey,SizeOf(CAKey),0);
      try
        Result := FindCACert(Cert,CACert);
        if Result then begin
          Result := Self.ExtractSubjectDSAPublicKey(CACert^,CAKey);
          if Result then
            CopyDLSystemParams(PublKey.Params,CAKey.Params);
        end;
      finally
        DisposeDLPublicKey(CAKey);
      end;
    end;
end;

function TX509TrustedCertificates.InternalCheckSignatureCert(const Cert,
  CACert: TASN1Struct): Boolean;
var
  Alg: TX509AlgorithmIdentifier;
  RSAKey: TIFPublicKey;
  DLKey: TDLPublicKey;  
  ECKey: TECPublicKey;
  MS: TSecureMemoryStream;
  HA,MHA: THashAlgorithm;
  EM: TSignEncoding;
  Val, ValCA: TX509Validity;
begin
  Result := IsAllegedIssuer(Cert,CACert);
  if not Result then Exit;
  ExtractValidity(Cert,Val);
  ExtractValidity(CACert,ValCA);
{ Note: The CA certificate might have been reissued. This was the old code:
  Result := (Val.NotBefore >= ValCA.NotBefore) and
            (Val.NotBefore <= ValCA.NotAfter);
}
  Result := Val.NotBefore <= ValCA.NotAfter;
  if not Result then Exit;
  Alg := nil;
  EM := seEMSA3;
{$IFDEF SHA1}
  HA := haSHA1;
  MHA := haSHA1;
{$ENDIF SHA1}
  if ExtractSignatureAlgorithmCertOrCRL(Cert,Alg) = E_OK then begin
    if (Alg.Algorithm = id_DSA_with_SHA1) or
       (Alg.Algorithm = ecdsa_with_SHA1) then                      
{$IFDEF SHA1}
      Result := True
{$ELSE  SHA1}
      Result := False
{$ENDIF SHA1}
    else
      Result := InterpretRSASignatureAlgorithm(Alg,HA,MHA,EM);
    Alg.Free;
  end else
    Result := False;
  if Result then begin
    FillChar(RSAKey,SizeOf(RSAKey),0);
    FillChar(DLKey,SizeOf(DLKey),0);   
    FillChar(ECKey,SizeOf(ECKey),0);
    if ExtractSubjectRSAPublicKey(CACert,RSAKey,True) = E_OK then begin
      MS := TSecureMemoryStream.Create;
      try
        Cert.Items[0]^.SaveToStream(MS,fmtDER);
        MS.Position := 0;
        Result := IFSSASignatureVerification(RSAKey,MS.Memory^,MS.Size,
                                             Cert.Contents^.Items[2].Content^,
                                             Cert.Contents^.Items[2].Length,
                                             HA,MHA,EM);
      finally
        MS.Free;
      end;
      DisposeIFPublicKey(RSAKey);
    end else if ExtractSubjectDSAPublicKey(CACert,DLKey) then begin
      MS := TSecureMemoryStream.Create;
      try
        ASN1ToStream(Cert.Contents^.Items[0],MS);
        MS.Position := 0;
        Result := DLSSASignatureVerification(DLKey,MS.Memory^,MS.Size,
                                             Cert.Contents^.Items[2].Content[1],
                                             Cert.Contents^.Items[2].Length - 1,
                                             True,HA);
      finally
        MS.Free;
      end;
      DisposeDLPublicKey(DLKey);
    end else if ExtractSubjectECPublicKey(CACert,ECKey) then begin
      MS := TSecureMemoryStream.Create;
      try
        ASN1ToStream(Cert.Contents^.Items[0],MS);
        MS.Position := 0;
        Result := ECSSASignatureVerification(ECKey,MS.Memory^,MS.Size,
                                             Cert.Contents^.Items[2].Content[1],
                                             Cert.Contents^.Items[2].Length - 1,
                                             True,HA);
      finally
        MS.Free;
      end;
      DisposeECPublicKey(ECKey);
    end
  end;
end;

function TX509TrustedCertificates.ExtractSubjectECPublicKey(
  const Cert: TASN1Struct; var PublKey: TECPublicKey): Boolean;
var
  Res: Integer;
  CACert: PASN1Struct;
  CAKey: TECPublicKey;
begin
  DisposeECPublicKey(PublKey);
  Res := X509Base.ExtractSubjectECPublicKey(Cert,PublKey);
  Result := Res = E_OK;
  if Res = E_NO_PARAMETERS then begin
    FillChar(CAKey,SizeOf(CAKey),0);
    try
      Result := FindCACert(Cert,CACert);
      if Result then begin
        Result := Self.ExtractSubjectECPublicKey(CACert^,CAKey);
        if Result then begin
          ECCopy2(CAKey.Params,PublKey.Params);
          Res := X509Base.ExtractSubjectECPublicKey(Cert,PublKey);
          Result := Res = E_OK;
        end;
      end;
    finally
      DisposeECPublicKey(CAKey);
    end;
  end;
end;

{$IFDEF SHA1_AND_MD5}
function TX509TrustedCertificates.ExportChainToTLS(
  var Certificate: PTLSHandshake; var Len: Integer;
  var Cert: PASN1Struct;
  CertTypes: TCertificateTypes; Issuers: TX501Names): Boolean;
var
  I, J: Integer;
  C, CA: PASN1Struct;
  N: TX501Name;
  PK: TX509PublicKey;
  OK: Boolean;
  KeyUsage, CertKeyUsage: TKeyUsage;
  Alg: TX509AlgorithmIdentifier;
begin
  KeyUsage := [];
  for I := Low(CertTypes) to High(CertTypes) do
    if CertTypes[I] in [1,2,5] then
      Include(KeyUsage,DigitalSignature)
    else if CertTypes[I] in [3,4,6,7] then
      Include(KeyUsage,KeyAgreement);
  Result := KeyUsage <> [];
  if not Result then
    Exit;
  for I := Count - 1 downto 0 do begin
    C := GetPCerts(I);
    if not CheckValidity(C^,FHoursOffsetFromGMT) then Continue;
    CertKeyUsage := ExtractKeyUsage(C^) * KeyUsage;
    if CertKeyUsage = [] then
      Continue;
    PK := nil;
    try
      OK := ExtractSubjectPublicKey(C^,PK) = E_OK;
      if OK then begin
        if (PK.Algorithm.Algorithm = dhPublicNumber) and
           (KeyAgreement in CertKeyUsage) then begin
          CertKeyUsage := [KeyAgreement];
          for J := Low(CertTypes) to High(CertTypes) do begin
            OK := CertTypes[J] in [3,4];
            if OK then Break;
          end;
        end else if (PK.Algorithm.Algorithm = id_dsa) and
                    (DigitalSignature in CertKeyUsage) then begin
          CertKeyUsage := [DigitalSignature];
          for J := Low(CertTypes) to High(CertTypes) do begin
            OK := CertTypes[J] = 2;
            if OK then Break;
          end;
        end else if (PK.Algorithm.Algorithm = rsaEncryption) and
                    (DigitalSignature in CertKeyUsage) then begin
          CertKeyUsage := [DigitalSignature];
          for J := Low(CertTypes) to High(CertTypes) do begin
            OK := CertTypes[J] = 1;
            if OK then Break;
          end;
        end else if PK.Algorithm.Algorithm = id_ecPublicKey then begin
          for J := Low(CertTypes) to High(CertTypes) do begin
            OK := CertTypes[J] in [5,6,7];
            if OK then Break;
          end;
        end else
          OK := False;
      end;
    finally
      PK.Free;
    end;
    if not OK then
      Continue;
    if CertKeyUsage = [KeyAgreement] then begin
      Alg := nil;
      try
        OK := ExtractSignatureAlgorithmCertOrCRL(C^,Alg) = E_OK;
        if OK then begin
          if Alg.Algorithm = id_dsa_with_sha1 then
            for J := Low(CertTypes) to High(CertTypes) do begin
              OK := CertTypes[J] = 4;
              if OK then Break;
            end
          else if (Alg.Algorithm = md5WithRSAEncryption) or
                  (Alg.Algorithm = sha1WithRSAEncryption) then
            for J := Low(CertTypes) to High(CertTypes) do begin
              OK := CertTypes[J] in [3,6];
              if OK then Break;
            end
          else if Alg.Algorithm = ecdsa_with_sha1 then
            for J := Low(CertTypes) to High(CertTypes) do begin
              OK := CertTypes[J] = 7;
              if OK then Break;
            end
          else
            OK := False;
        end;
      finally
        Alg.Free;
      end;
    end;
    if not OK then
      Continue;
    OK := False;
    Cert := C;
    while FindCACert(C^,CA) and not CompareCertificate(C^,CA^) do begin
      C := CA;
      System.Finalize(N);
      FillChar(N,SizeOf(N),0);
      ExtractSubject(C^,N);
      for J := Low(Issuers) to High(Issuers) do
        if CompareName(N,Issuers[J]) then begin
          OK := True;
          Break;
        end;
    end;
    if OK then begin
      Result := True;
      ExportChainToTLS(Certificate,Len,I);
      Break;
    end else
      Cert := nil;
  end;
end;                           
{$ENDIF SHA1_AND_MD5}

procedure TX509TrustedCertificates.SetOnCertNotTrusted(
  const Value: TCertNotTrustedEvent);
begin
  FOnCertNotTrusted := Value;
end;

function TX509TrustedCertificates.CheckThreadID: Boolean;
var
  CTID: LongWord;
begin
  CTID := GetCurrentThreadID;
  Result := CTID = FCreationThreadID;
  if (FCreationThreadID <> MainThreadID) and not Result then
    raise Exception.Create('TX509TrustedCertificates.CheckThreadID: Called from the wrong thread.');
end;

procedure TX509TrustedCertificates.AddToIndex(Index: Integer);
var
  F: PASN1Struct;
  SS: TStringStream;
begin
  if ExtractSubjectStruct(Certs[Index],F) = E_OK then begin
    SS := TStringStream.Create('');
    try
      F.SaveToStream(SS,fmtDER);
      FSubjectIndex.AddObject(SS.DataString,TObject(Index));
    finally
      SS.Free;
    end;
  end;
{$IFDEF MD5}
  SS := TStringStream.Create('');
  try
    Certs[Index].SaveToStream(SS,fmtDER);
    FIndex.AddObject(DigestString(haMD5,SS.DataString),TObject(Index));
  finally
    SS.Free;
  end;
{$ENDIF MD5}
end;

function TX509TrustedCertificates.CheckPolicies(const Cert: TASN1Struct;
                                                PolicyMappings,
                                                AcceptedPolicies: TStrings): Boolean;
var
  Ext: TX509Extension;
  I, J, Res: Integer;
  OIDs: TStringList;
  OID, ReqOID: string;
begin
  Ext := nil;
  try
    Res := ExtractNamedExtension(Cert,id_ce_certificatePolicies,Ext);
    if Res = E_OK then begin
      Result := True;
      if FRequiredPolicies.Count > 0 then begin
        OIDs := TStringList.Create;
        try
          for J := 0 to Ext.extnValue.ItemCount - 1 do
            OIDs.Add(Ext.extnValue.Items[J].Items[0].ContentAsOID);
          for I := 0 to FRequiredPolicies.Count - 1 do begin
            Result := False;
            ReqOID := FRequiredPolicies[I];
            for J := 0 to OIDs.Count - 1 do begin
              OID := OIDs[J];
              Result := OID = ReqOID;
              if not Result then
                repeat
                  OID := PolicyMappings.Values[OID];
                  Result := OID = ReqOID;
                until (OID = '') or Result;
              if Result then Break;
            end;
            if not Result then Break;
          end;
          if (AcceptedPolicies.Count <> 1) or
             (AcceptedPolicies[0] <> anyPolicy) then begin
            Result := False;
            for I := 0 to OIDs.Count - 1 do begin
              OID := OIDs[I];
              repeat
                Result := AcceptedPolicies.IndexOf(OID) >= 0;
                if Result then Break;
                OID := PolicyMappings.Values[OID];
              until (OID = '') or Result;
              if Result then Break;
            end;
          end;
        finally
          OIDs.Free;
        end;
      end;
      if Result then begin
        Result := not Ext.Critical;
        if Assigned(FOnCertPolicy) then
          FOnCertPolicy(Self,Cert,PolicyMappings,Result);
      end;
    end else if Res = E_NOT_SUPPORTED then
      Result := FRequiredPolicies.Count = 0
    else
      Result := Res <> E_SYNTAX;
  finally
    Ext.Free;
  end;
end;

procedure TX509TrustedCertificates.SetOnCertPolicy(
  const Value: TPolicyEvent);
begin
  FOnCertPolicy := Value;
end;

procedure TX509TrustedCertificates.SetRequiredPolicies(
  const Value: TStrings);
begin
  FRequiredPolicies.Assign(Value);
end;

procedure TX509TrustedCertificates.UpdatePolicies;
var
  I: Integer;
begin
  FCerts.Items[0].Items[3].DisposeContent;
  FCerts.Items[0].Items[3].CreateOFTemplate;
  FCerts.Items[0].Items[3].Template.Tag := V_ASN1_OBJECT;
  for I := 0 to FRequiredPolicies.Count - 1 do
    FCerts.Items[0].Items[3].AddField.EditContent(FRequiredPolicies[I]);
  FCerts.Items[0].Items[4].DisposeContent;
  FCerts.Items[0].Items[4].CreateOFTemplate;
  FCerts.Items[0].Items[4].Template.Tag := V_ASN1_OBJECT;
  for I := 0 to FRequiredPolicies.Count - 1 do
    FCerts.Items[0].Items[4].AddField.EditContent(FAcceptedPolicies[I]);
end;

procedure TX509TrustedCertificates.SetAcceptedPolicies(
  const Value: TStrings);
begin
  FAcceptedPolicies.Assign(Value);
end;

function TX509TrustedCertificates.NextSerialNumber(
  const CACert: TASN1Struct): string;
var
  X, Y: PMPInteger;
  Serial: string;
  I: Integer;
begin
  X := IntToMPInt(0);
  try
    Y := nil;
    try
      for I := 0 to Count - 1 do
        if IsAllegedIssuer(Certs[I],CACert) then
          if ExtractSerial(Certs[I],Serial) = E_OK then begin
            Base256ToMPInt(Y,Serial);
            if MPCmpOffset(X,Y) <= 0 then
              MPInc2(Y,X);
          end;
      if Assigned(FTrustedCACertificates) then begin
        Serial := FTrustedCACertificates.NextSerialNumber(CACert);
        Base256ToMPInt(Y,Serial);
        if MPCmpOffset(X,Y) <= 0 then
          MPCopy2(Y,X);
      end;
    finally
      MPDealloc(Y);
    end;
    Result := MPIntToBase256(X);
  finally
    MPDealloc(X);
  end;
end;

function TX509TrustedCertificates.ExtractConstraintsFromChain(
  CACert: TASN1Struct; BC: TBasicConstraintsSyntax;
  NC: TNameConstraintsSyntax; PolicyMappings, Policies: TStrings;
  var RequirePolicy, AllowMapping: Boolean): Boolean;
var
  Level, NCLevel: Integer;
  CAACert: PASN1Struct;
  Ext: TX509Extension;
  vBC: TBasicConstraintsSyntax;
  vNC: TNameConstraintsSyntax;
  vPM: TPolicyMappingsSyntax;
  vCP: TCertificatePolicies;
  vPC: TPolicyConstraintsSyntax;
  I: Integer;
  OIDs: TStringList;
  OID: string;
begin
  Result := True;
  Level := 1;
  NCLevel := 0;
  BC.CA := True;
  Ext := nil;
  try
    CAACert := @CACert;
    repeat
      CACert := CAACert^;
      if Assigned(BC) and
         (ExtractNamedExtension(CACert,id_ce_BasicConstraints,Ext) = E_OK) then begin
        vBC := TBasicConstraintsSyntax.Create(nil,nil);
        try
          vBC.Data.ReadOnly := False;
          vBC.AssignStruct(Ext.extnValue);
          Result := vBC.CA;
          if vBC.PathLenConstraint.Length > 0 then begin
            Result := Result and (vBC.PathLenConstraint.AsInteger >= Level - 1);
            if Result then begin
              if vBC.PathLenConstraint.AsInteger < Level then
                BC.CA := False;
              if BC.CA then
                if (BC.PathLenConstraint.Length = 0) or
                   (vBC.PathLenConstraint.AsInteger - Level <
                    BC.PathLenConstraint.AsInteger) then
                  BC.PathLenConstraint.AsInteger :=
                    vBC.PathLenConstraint.AsInteger - Level;
            end;
          end;
        finally
          vBC.Free;
        end;
      end;
      if Assigned(NC) and
         (ExtractNamedExtension(CACert,id_ce_NameConstraints,Ext) = E_OK) then begin
        if NCLevel = 0 then
          NC.AssignStruct(Ext.extnValue)
        else begin
          vNC := TNameConstraintsSyntax.Create(nil,nil);
          try
            vNC.AssignStruct(Ext.extnValue);
            vNC.Combine(NC);
          finally
            vNC.Free;
          end;
        end;
        Inc(NCLevel);
      end;
      if Assigned(PolicyMappings) and
         (ExtractNamedExtension(CACert,id_ce_policyMappings,Ext) = E_OK) then begin
        vPM := TPolicyMappingsSyntax.Create(nil,nil);
        try
          vPM.AssignStruct(Ext.extnValue);
          for I := 0 to vPM.Count - 1 do
            PolicyMappings.Values[vPM[I].SubjectDomainPolicy] :=
              vPM[I].IssuerDomainPolicy;
        finally
          vPM.Free;
        end;
      end;
      if Assigned(AcceptedPolicies) and
         (ExtractNamedExtension(CACert,id_ce_certificatePolicies,Ext) = E_OK) then begin
        vCP := TCertificatePolicies.Create(nil,nil);
        try
          vCP.AssignStruct(Ext.extnValue);
          if (AcceptedPolicies.Count = 1) and (AcceptedPolicies[0] = anyPolicy) then begin
            AcceptedPolicies.Clear;
            for I := 0 to vCP.Count - 1 do
              AcceptedPolicies.Add(vCP[I].PolicyIdentifier);
          end else begin
            OIDs := TStringList.Create;
            try
              for I := 0 to vCP.Count - 1 do
                OIDs.Add(vCP[I].PolicyIdentifier);
              for I := AcceptedPolicies.Count - 1 downto 0 do begin
                OID := AcceptedPolicies[I];
                if OIDs.IndexOf(OID) < 0 then begin
                  repeat
                    OID := PolicyMappings.Values[OID];
                  until (OID = '') or (OIDs.IndexOf(OID) >= 0);
                  if OID = '' then
                    AcceptedPolicies.Delete(I)
                  else
                    AcceptedPolicies[I] := OID;
                end;
              end;
            finally
              OIDs.Free;
            end;
            Result := AcceptedPolicies.Count > 0;
          end;
        finally
          vCP.Free;
        end;
      end;
      if ExtractNamedExtension(CACert,id_ce_policyConstraints,Ext) = E_OK then begin
        vPC := TPolicyConstraintsSyntax.Create(nil,nil);
        try
          vPC.AssignStruct(Ext.extnValue);
          if (vPC.RequireExplicitPolicy.Length > 0) and
             (vPC.RequireExplicitPolicy.AsInteger < Level) then
            RequirePolicy := True;
          if (vPC.InhibitPolicyMapping.Length > 0) and
             (vPC.InhibitPolicyMapping.AsInteger < Level) then
            AllowMapping := False;
        finally
          vPC.Free;
        end;
      end;
      Inc(Level);
    until not (Result and FindCACert(CACert,CAACert));
  finally
    Ext.Free;
  end;
end;

function TX509TrustedCertificates.FindCRL(const CACert: TASN1Struct;
  const Reasons: TX509ReasonFlags; var CRL: PASN1Struct): Boolean;
var
  IDPs: TX509IssuingDistributionPoints;
  I: Integer;
begin
  Result := False;
  if SuggestIssuingDistributionPoints(CACert,Reasons,IDPs) >= 0 then
    for I := 0 to Length(IDPs) - 1 do
      if FindCRL(CACert,IDPs[I],CRL) then begin
        Result := True;
        Exit;
      end;
end;

procedure TX509TrustedCertificates.SignSaveToFile(AFileName: TFileName;
  const MyCert: TASN1Struct; MyPrivKey: IMPPrivateKey);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName,fmCreate or fmShareExclusive);
  try
    SignSaveToStream(FS,MyCert,MyPrivKey);
  finally
    FS.Free;
  end;
end;

procedure TX509TrustedCertificates.SignSaveToStream(AStream: TStream;
  const MyCert: TASN1Struct; MyPrivKey: IMPPrivateKey);
var
  S: TCertStore;
  Signed: ISigned;
begin
  FLock.Acquire;
  try
    UpdatePolicies;
    CopyASN1Struct(FCerts.Contents^.Items[0].Contents^.Items[0].Contents^.Items[0],MyCert);
    S := TCertStore.Create(nil,FCerts);
    try
      Signed := S;
      MyPrivKey.SignSigned(Signed,MyCert);
    finally
      Signed := nil;
      S.Free;
    end;
    FModified := False;
    ASN1ToStream(FCerts,AStream);
  finally
    FLock.Release;
  end;
end;

function TX509TrustedCertificates.FindCert(const SignatureAlgorithms,
  PKAlgorithms: array of string; const KeyUsage: TKeyUsage;
  const ExtKeyUsage: array of string; var StartIndex: Integer;
  var Cert: PASN1Struct; ReverseSearch: Boolean): Boolean;
var
  I: Integer;

  function VerifyPKAlgorithm(Cert: TASN1Struct): Boolean;
  var
    I: Integer;
    PublKey: TX509PublicKey;
  begin
    if Length(PKAlgorithms) = 0 then
      Result := True
    else begin
      Result := False;
      PublKey := nil;
      try
        ExtractSubjectPublicKey(Cert,PublKey);
        for I := 0 to Length(PKAlgorithms) - 1 do
          if PublKey.Algorithm.Algorithm = PKAlgorithms[I] then begin
            Result := True;
            Break;
          end;
      finally
        PublKey.Free;
      end;
    end;
  end;

  function VerifySignatureAlgorithm(Cert: TASN1Struct): Boolean;
  var
    I: Integer;
    SignAlg: TX509AlgorithmIdentifier;
  begin
    if Length(SignatureAlgorithms) = 0 then
      Result := True
    else begin
      Result := False;
      SignAlg := nil;
      try
        ExtractSignatureAlgorithmCertOrCRL(Cert,SignAlg);
        for I := 0 to Length(SignatureAlgorithms) - 1 do
          if SignAlg.Algorithm = SignatureAlgorithms[I] then begin
            Result := True;
            Break;
          end;
      finally
        SignAlg.Free;
      end;
    end;
  end;

  function VerifyExtKeyUsage(Cert: TASN1Struct): Boolean;
  var
    I, J: Integer;
    Ext: TX509Extension;
    PublKey: TX509PublicKey;
  begin
    if Length(ExtKeyUsage) = 0 then
      Result := True
    else begin
      PublKey := nil;
      Ext := nil;
      try
        Result := ExtractNamedExtension(Cert,id_ce_extKeyUsage,Ext) = E_OK;
        if Result then for I := 0 to Length(ExtKeyUsage) - 1 do begin
          Result := False;
          for J := 0 to Ext.extnValue.ItemCount - 1 do
            if Ext.extnValue.Items[J].ContentAsOID = ExtKeyUsage[I] then begin
              if (KeyUsage = []) and
                 (ExtKeyUsage[I] = id_kp_serverAuth) then begin
                if PublKey = nil then
                  ExtractSubjectPublicKey(Cert,PublKey);
                if PublKey.Algorithm.Algorithm = rsaEncryption then
                  Result := [digitalSignature,keyEncipherment]*ExtractKeyUsage(Cert) <> []
                else if PublKey.Algorithm.Algorithm = dhPublicNumber then
                  Result := keyAgreement in ExtractKeyUsage(Cert)
                else if PublKey.Algorithm.Algorithm = id_dsa then
                  Result := digitalSignature in ExtractKeyUsage(Cert)
                else if PublKey.Algorithm.Algorithm = id_ecPublicKey then
                  Result := keyAgreement in ExtractKeyUsage(Cert);
              end else
                Result := True;
              if Result then begin
                Ext.extnValue.DeleteItem(J);
                Break;
              end;
            end;
          if not Result then Break;
        end;
      finally
        PublKey.Free;
        Ext.Free;
      end;
    end;
  end;

begin
  Result := False;
  if ReverseSearch then begin
    if StartIndex < 0 then Exit;
    for I := StartIndex downto 0 do begin
      Cert := GetPCerts(I);
      Assert(Assigned(Cert) and Assigned(Cert^));
      if not CheckValidity(Cert^,HoursOffsetFromGMT) then Continue;
      if IsRevoked(Cert^) then Continue;
      if VerifyPKAlgorithm(Cert^) then
        if VerifySignatureAlgorithm(Cert^) then
          if KeyUsage <= ExtractKeyUsage(Cert^) then
            if VerifyExtKeyUsage(Cert^) then begin
              StartIndex := I;
              Result := True;
              Break;
            end;
      Cert := nil;
    end;
    if not Result then StartIndex := -1;
  end else begin
    if Count <= StartIndex then Exit;
    for I := StartIndex to Count - 1 do begin
      Cert := GetPCerts(I);
      Assert(Assigned(Cert) and Assigned(Cert^));
      if not CheckValidity(Cert^,HoursOffsetFromGMT) then Continue;
      if IsRevoked(Cert^) then Continue;
      if VerifyPKAlgorithm(Cert^) then
        if VerifySignatureAlgorithm(Cert^) then
          if KeyUsage <= ExtractKeyUsage(Cert^) then
            if VerifyExtKeyUsage(Cert^) then begin
              StartIndex := I;
              Result := True;
              Break;
            end;
      Cert := nil;
    end;
    if not Result then StartIndex := Count;
  end;
end;

procedure TX509TrustedCertificates.SetOnCertNotAccepted(
  const Value: TCertNotAcceptedEvent);
begin
  FOnCertNotAccepted := Value;
end;

function TX509TrustedCertificates.RemoveCertificate(
  const Cert: TASN1Struct): Boolean;
var
  Idx: Integer;
begin
  FLock.Acquire;
  try
    Result := False;
    repeat
      Idx := IndexOfCert(Cert);
      if Idx >= 0 then begin
        DeleteIndex(Idx);
        FCerts.Contents^.Items[0].Contents^.Items[1].DeleteItem(Idx);
        FModified := True;
        Result := True;
      end;
    until Idx < 0;
  finally
    FLock.Release;
  end;
end;

procedure TX509TrustedCertificates.SetSCLFile(const Value: TResourceFile);
begin
  try
    if Value <> FSCLFile then begin
      if Assigned(FSCLFile) then
        FSCLFile.RemoveLoadNotification(Self,DoLoadSCLFile);
      FSCLFile := Value;
      if Assigned(FSCLFile) then begin
        FreeNotification(FSCLFile);
        FSCLFile.AddLoadNotification(Self,DoLoadSCLFile);
        DoLoadSCLFile(FSCLFile);
        if FSCLFile <> nil then begin
          FSCLFileName := '';
          CertSource := nil;
        end;
      end;
    end;
  except
    FSCLFile := nil;
  end;
end;

procedure TX509TrustedCertificates.SetSCLFileName(const Value: TFileName);
begin
  if Value <> FSCLFileName then begin
    FSCLFileName := Value;
    if (Value <> '') and not (csLoading in ComponentState) then
      try
        LoadFromFile(Value);
        SetSCLFile(nil);
        SetCertSource(nil);
      except
        FSCLFileName := '';
      end;
  end;
end;

procedure TX509TrustedCertificates.DoLoadSCLFile(Sender: TObject);
begin
  if Sender = FSCLFile then
    try
      LoadFromStream(FSCLFile.DataStream);
    except
      SetSCLFile(nil);
    end;
end;

procedure TX509TrustedCertificates.Loaded;
begin
  inherited;
  if FSCLFileName <> '' then
    LoadFromFile(FSCLFileName)
  else if Assigned(FSCLFile) and
          Assigned(FSCLFile.DataStream) then
    DoLoadSCLFile(FSCLFile)
  else if Assigned(FCertSource) then
    FCertList.Loaded; // csLoading in ComponentState might be excluded or out of synch.
end;

function TX509TrustedCertificates.DeleteCertificate(
  index: Integer): Boolean;
begin
  FLock.Acquire;
  try
    Result := False;
    if (index >= 0) and (index < Count) then begin
      DeleteIndex(index);
      FCerts.Contents^.Items[0].Contents^.Items[1].DeleteItem(index);
      FModified := True;
      Result := True;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TX509TrustedCertificates.Clear;
begin
  FLock.Acquire;
  try
    if Count > 0 then
      FCerts.Items[0].Items[1].DisposeContent;
    Assert(Count <= 0);
    if CRLCount > 0 then
      FCerts.Items[0].Items[2].DisposeContent;
    Assert(CRLCount <= 0);
    FModified := False;
    FIndex.Clear;
    FSubjectIndex.Clear;
    FGroupIndex := -1;
  finally
    FLock.Release;
  end;
end;

procedure TX509TrustedCertificates.UpdateHijacker(
  AHijacker: TCustomX509TrustedCertificates);
var
  GroupIdx, Idx: Integer;
  Cert: TASN1Struct;
  Status: TCertStatusCode;
begin
  if Assigned(AHijacker) and AHijacker.Hijacks(Self) then begin
    GroupIdx := ClearHijacker(AHijacker);
    for Idx := 0 to FCertList.Count - 1 do begin
      Cert := TCertificateItem(FCertList.Items[Idx]).Cert;
      if Cert = nil then Continue;
      if TCertificateItem(FCertList.Items[Idx]).GroupIndex = GroupIdx then
        AHijacker.AddCertificate(Cert,True,Status);
    end;
    for Idx := 0 to CRLCount - 1 do
      AHijacker.AddCRL(CRLs[Idx],Status);
    AHijacker.CertListLoaded;
    AHijacker.UpdateHijackers;
  end;
end;

procedure TX509TrustedCertificates.SetCertList(
  const Value: TCertificateCollection);
begin
  FCertList.Assign(Value);
end;

procedure TX509TrustedCertificates.CertListLoaded;
begin
  FCertList.Loaded;
end;

function TX509TrustedCertificates.FindServerCert(const SignatureAlgorithms,
  PKAlgorithms: array of string; const KeyUsage: TKeyUsage; const URI,
  IPAddress, DNSName: string; var StartIndex: Integer;
  var Cert: PASN1Struct): Boolean;
begin
  repeat
    Result := FindCert(SignatureAlgorithms,PKAlgorithms,KeyUsage,
                       [id_kp_serverAuth],StartIndex,Cert);
    if Result then begin
      if URI <> '' then
        Result := X509Base.CheckURI(Cert^,URI);
      if Result then begin
        if IPAddress <> '' then
          Result := X509Base.CheckIP(Cert^,IPAddress);
        if Result and (DNSName <> '') then
          Result := X509Base.CheckDNSName(Cert^,DNSName);
      end;
    end else
      Break;
    if not Result then
      Inc(StartIndex);
  until Result;
end;

procedure TX509TrustedCertificates.DeleteIndex(Index: Integer);
var
  Idx: Integer;
begin
  FIndex.Delete(FIndex.IndexOfObject(TObject(index)));
  for Idx := 0 to FIndex.Count - 1 do
    if Integer(FIndex.Objects[Idx]) > Index then
      FIndex.Objects[Idx] := TObject(Integer(FIndex.Objects[Idx]) - 1);
  FSubjectIndex.Delete(FSubjectIndex.IndexOfObject(TObject(index)));
  for Idx := 0 to FSubjectIndex.Count - 1 do
    if Integer(FSubjectIndex.Objects[Idx]) > Index then
      FSubjectIndex.Objects[Idx] := TObject(Integer(FSubjectIndex.Objects[Idx]) - 1);
end;

function TX509TrustedCertificates.AddSMIMECapabilities(Cert,
  Capabilities: TASN1Struct): Boolean;
{$IFDEF MD5}
var
  C: PASN1Struct;
  FingerPrint: OctetString;
{$ENDIF MD5}
begin  
{$IFDEF MD5}
  if FindSMIMECapabilities(Cert,C) then begin
    if not Asn1.ASN1StructCompare(C^,Capabilities) then begin
      C^.Clear;
      CopyASN1Struct(C^,Capabilities);
      FModified := True;
    end;
  end else begin
    FingerPrint := DigestString(haMD5,Cert.ContentAsOctetString);
    with FCerts.Items[0]^.Items[5]^.AddField^ do begin
      Items[0]^.SetContent(Pointer(FingerPrint)^,System.Length(FingerPrint));
      CopyASN1Struct(Items[1]^,Capabilities);
    end;
    FModified := True;
  end;
  Result := True;
{$ELSE  MD5}
  Result := False;
{$ENDIF MD5}
end;

function TX509TrustedCertificates.FindSMIMECapabilities(Cert: TASN1Struct;
  var Capabilities: PASN1Struct): Boolean;
{$IFDEF MD5}
var
  FingerPrint: OctetString;
  I: Integer;
{$ENDIF MD5}
begin       
  Result := False;
{$IFDEF MD5}
  FingerPrint := DigestString(haMD5,Cert.ContentAsOctetString);
  with FCerts.Items[0]^.Items[5]^ do
    for I := 0 to ItemCount - 1 do
      if Items[I]^.Items[0]^.ContentAsOctetString = FingerPrint then begin
        Capabilities := Items[I]^.Items[1];
        Result := True;
        Break;
      end;
{$ENDIF MD5}
end;

function TX509TrustedCertificates.GetCertificateCollectionCount: Integer;
begin
  Result := 1;
end;

function TX509TrustedCertificates.GetCertificateCollections(
  Index: Integer): TCertificateCollection;
begin
  if Index = 0 then
    Result := FCertList
  else
    Result := nil;
end;

procedure TX509TrustedCertificates.SetCertificateCollections(
  Index: Integer; const Value: TCertificateCollection);
begin
  if Index = 0 then
    FCertList.Assign(Value);
end;

procedure TX509TrustedCertificates.SetBeforeImportTLSCert(
  const Value: TBeforeImportTLSCertEvent);
begin
  FBeforeImportTLSCert := Value;
end;

function TX509TrustedCertificates.InternalVerifyCertificate(
  const Cert: TASN1Struct; ExplicitTrust: Boolean;
  var Status: TCertStatusCode; AllowExpired: Boolean): Integer;
var
  CACert, P: PASN1Struct;
  Ext: TX509Extension;
  KS: Integer;
  Trust: Boolean;
  PolicyMappings,
  AcceptedPolicies: TStringList;
  NC: TNameConstraintsSyntax;
  I: Integer;
  vSubject: TName;
  vExt: TExtension;
begin
  Result := IndexOfCert(Cert);
  if Result >= 0 then begin
    if not CheckValidity(Cert,FHoursOffsetFromGMT) then
      Status := crcExpired
    else
      Status := crcOK;
    if IsRevoked(Cert) then
      Status := crcRevoked
    else if AllowExpired or (Status = crcOK) then begin
      if IsAllegedIssuer(Cert,Cert) then
        Status := crcOK
      else begin
        VerifyCAChain(Cert,Status,nil,nil,nil,AllowExpired);
        if (Status = crcCANotTrusted) then begin
          if ExplicitTrust then
            Status := crcOK
          else if Assigned(FOnCertNotTrusted) then begin
            Trust := False;
            FOnCertNotTrusted(Self,Cert,Trust);
            if Trust then
              Status := crcOK
            else
              Status := crcCANotTrusted;
          end;
        end;
      end;
    end;
    Exit;
  end;

  if FindCert(ExtractSubjectKeyIdentifier(Cert),P) then begin
    Status := crcDuplicateKeyIdentifier;
    if Assigned(FOnCertNotAccepted) then
      FOnCertNotAccepted(Self,Cert,Status);
    Exit;
  end;

  if ExtractSubjectPublicKeySize(Cert,KS) <> E_OK then begin
    Status := crcSyntax;
    if Assigned(FOnCertNotAccepted) then
      FOnCertNotAccepted(Self,Cert,Status);
    Exit;
  end else if KS < FLeastKeyBitSize then begin
    Status := crcTooSmallKey;
    if Assigned(FOnCertNotAccepted) then
      FOnCertNotAccepted(Self,Cert,Status);
    Exit;
  end;

  FillChar(Ext,SizeOf(Ext),0);
  if CheckCriticalExtensions(Cert,Ext) = E_NOT_SUPPORTED then begin
    Status := crcUnsupportedExtension;
    Ext.Free;
    if Assigned(FOnCertNotAccepted) then
      FOnCertNotAccepted(Self,Cert,Status);
    Exit;
  end;

  PolicyMappings := TStringList.Create;
  AcceptedPolicies := TStringList.Create;
  try
    NC := TNameConstraintsSyntax.Create(nil,nil);
  except
    NC := nil;
  end;
  try
    if Assigned(NC) then
      NC.Data.ReadOnly := False;
    AcceptedPolicies.Add(anyPolicy);
    if CheckValidity(Cert,FHoursOffsetFromGMT) then
      Status := crcOK
    else
      Status := crcExpired;
    if AllowExpired or (Status = crcOK) then begin
      if IsAllegedIssuer(Cert,Cert) then begin
        if CheckSignatureCert(Cert,Cert) then begin
          if ExplicitTrust then
            Status := crcOK
          else begin
            if IndexOfCert(Cert) >= 0 then
              Status := crcOK
            else if Assigned(FTrustedCACertificates) and
                    (FTrustedCACertificates.IndexOfCert(Cert) >= 0) then
              Status := crcOK
            else if Assigned(FOnCertNotTrusted) then begin
              Trust := False;
              FOnCertNotTrusted(Self,Cert,Trust);
              if Trust then
                Status := crcOK
              else
                Status := crcCANotTrusted;
            end else
              Status := crcCANotTrusted;
          end;
        end else
          Status := crcInvalidSignature;
      end else begin
        if IsRevoked(Cert) then
          Status := crcRevoked
        else if FindCACert(Cert,CACert) then begin
          if VerifyCAChain(Cert,Status,PolicyMappings,AcceptedPolicies,NC) then begin
            if InternalCheckSignatureCert(Cert,CACert^) then begin
              Status := crcOK;
            end else
              Status := crcInvalidSignature;
          end;
        end else if ExplicitTrust then
          Status := crcOK
        else if Assigned(FOnCertNotTrusted) then begin
          Trust := False;
          FOnCertNotTrusted(Self,Cert,Trust);
          if Trust then
            Status := crcOK
          else
            Status := crcCANotTrusted;
        end else
        Status := crcCANotTrusted;
      end;
    end else
      Status := crcExpired;

    if Status = crcOK then
      if Assigned(NC) and (NC.ExcludedSubtrees.Count > 0) then begin
        vExt := TExtension.Create(nil,nil);
        try
          vSubject := TName.Create(nil,nil);
          try
            if ExtractSubjectStruct(Cert,P) = E_OK then
              vSubject.AssignStruct(P^);
            Ext := nil;
            if ExtractNamedExtension(Cert,id_ce_SubjectAltName,Ext) = E_OK then
              try
                vExt.extnID := id_ce_SubjectAltName;
                vExt.ExtnValue.AsCe_SubjectAltName.AssignStruct(Ext.extnValue);
              finally
                Ext.Free;
              end
            else
              vExt.ExtnValue.Choice := eveUndefined;
            if vExt.ExtnValue.Choice = eveIdCeSubjectAltName then begin
              if not NC.Verify(vSubject,vExt.ExtnValue.AsCe_SubjectAltName) then
                Status := crcConstraintFault;
            end else
              if not NC.Verify(vSubject,nil) then
                Status := crcConstraintFault;
          finally
            vSubject.Free;
          end;
        finally
          vExt.Free;
        end;
      end;
    if Status = crcOK then begin
      if (FAcceptedPolicies.Count > 0) and
         (FAcceptedPolicies[0] <> anyPolicy) then begin
        for I := AcceptedPolicies.Count - 1 downto 0 do
          if FAcceptedPolicies.IndexOf(AcceptedPolicies[I]) < 0 then
            AcceptedPolicies.Delete(I);
        if AcceptedPolicies.Count = 0 then
          Status := crcPolicyNotAccepted;
      end;
    end;
    if Status = crcOK then begin
      if not CheckPolicies(Cert,PolicyMappings,AcceptedPolicies) then begin
        Status := crcPolicyNotAccepted;
        Result := -1;
        Exit;
      end;
    end else
      Result := -1;
  finally
    PolicyMappings.Free;
    AcceptedPolicies.Free;
    NC.Free;
  end;
end;

{ TCertStore }

function TCertStore.BeforeSign(var CACert: TCertificate;
  SignAlg: ObjectIdentifier; out Params: TASNCustomWrapper): Boolean;
begin
  Result := True;
  Params := nil;
end;

function TCertStore.CheckSignAlgCoherence: Boolean;
begin
  Result := True;
end;

constructor TCertStore.Create(AOwner: TPersistent; AData,
  ATemplate: TASN1Struct);
begin
  Assert(Assigned(GlobalObject),'MPX509 not initialized: Call StrSecInit.InitMPX509 to correct.');
  inherited Create(AOwner,AData,ATemplate);
  if FData.TypeName = '' then
    FData.Assign(GlobalObject);
  FItems.Add(nil);
  FieldFactory(TAlgorithmIdentifier,nil,FData.Items[1]^,nil^);
  FieldFactory(TBitString,nil,FData.Items[2]^,nil^);
end;

destructor TCertStore.Destroy;
begin
  inherited Destroy;
end;

function TCertStore.IsAllegedIssuer(CACert: TCertificate): Boolean;
begin
  Result := True;
end;

{ TCertificateItem }

procedure TCertificateItem.AssignTo(Dest: TPersistent);
var
  D: TCertificateItem;
begin
  if Dest is TCertificateItem then begin
    D := TCertificateItem(Dest);
    D.CertIndex := CertIndex;
    D.GroupIndex := GroupIndex;
  end else
    inherited;
end;

constructor TCertificateItem.Create(Collection: TCollection);
begin
  FExtKeyUsage := TStringList.Create;
  FSubject := TMPX501Name.Create(Self);
  FSubject.ReadOnly := True;
  FIssuer := TMPX501Name.Create(Self);
  FIssuer.ReadOnly := True;
  FValidity := TMPX509Validity.Create;
  FValidity.ReadOnly := True;
  inherited;
end;

destructor TCertificateItem.Destroy;
begin
  SetCert(nil);
  FSubject.Free;
  FIssuer.Free;
  FValidity.Free;
  FExtKeyUsage.Free;
  inherited;
end;

function TCertificateItem.GetCertIndex: Integer;
var
  GreatOwner: TPersistent;
begin
  GreatOwner := TCertificateCollection(Collection).GetOwner;
  if Assigned(FCert) and
     (GreatOwner <> nil) and
     (GreatOwner is TX509TrustedCertificates) then
    Result := TX509TrustedCertificates(GreatOwner).IndexOfCert(FCert)
  else
    Result := -1;
end;

function TCertificateItem.GetDisplayName: string;
begin
  Result := '';
  if FSubject.FData.commonName.Str <> '' then
    Result := 'CN="' + FSubject.FData.commonName.Str + '" ';
  if FSubject.FData.organizationName.Str <> '' then
    Result := Result + 'O="' + FSubject.FData.organizationName.Str + '" ';
  if FSubject.FData.organizationalUnitName.Str <> '' then
    Result := Result + 'OU="' + FSubject.FData.organizationalUnitName.Str + '" ';
end;

function TCertificateItem.GetKeyUsage: TKeyUsage;
begin
  Result := FKeyUsage;
end;

function TCertificateItem.GetPublicKeyAlg: TPublicKeyAlg;
begin
  Result := FPublicKeyAlg;
end;

procedure TCertificateItem.Loaded;
var
  Idx: Integer;
  CrtId: string;
  GreatOwner: TPersistent;
begin
  if FCert = nil then begin
    if FCertId <> '' then begin
      if Length(FCertId) = 16 then begin
        // Backward compatibility. This shouldn't happen.
        CrtId := FCertId;
        FCertId := OSToHex(CrtId,True);
      end else
        CrtId := HexToOS(FCertId);
      GreatOwner := TCertificateCollection(Collection).GetOwner;
      Idx := TX509TrustedCertificates(GreatOwner).FIndex.IndexOf(CrtId);
      if Idx >= 0 then
        FCertIndex := Integer(TX509TrustedCertificates(GreatOwner).FIndex.Objects[Idx]);
    end;
    CertIndex := FCertIndex;
  end;
end;

procedure TCertificateItem.SetAuthorityKeyIdentifier(const Value: string);
begin
  // Read only
end;

procedure TCertificateItem.SetCA(const Value: Boolean);
begin
  // Read only
end;

procedure TCertificateItem.SetCert(const Value: TASN1Struct);
var
  S, PKOID: string;
  Ext: TX509Extension;
  I: Integer;
begin
  if Assigned(FCert) then
    FCert._Release;
  FCert := Value;
  if Assigned(FCert) then begin
    FCert._AddRef; 
{$IFDEF MD5}
    FCertId := OSToHex(DigestString(haMD5,FCert.ContentAsOctetString),True);
{$ELSE  MD5}
    FCertId := '(not supported)';
{$ENDIF MD5}

    ExtractSubject(FCert,FSubject.FData,False,'/');
    ExtractIssuer(FCert,FIssuer.FData,False,'/');
    ExtractSerial(FCert,S);
    FSerial := OSToHex(S);
    ExtractValidity(FCert,FValidity.FData);

    FKeyUsage := ExtractKeyUsage(FCert);

    ExtractSubjectPublicKeyAlg(FCert,PKOID);
    if PKOID = rsaEncryption then
      FPublicKeyAlg := pkaRSA
    else if PKOID = id_dsa then
      FPublicKeyAlg := pkaDSA
    else if PKOID = dhPublicNumber then
      FPublicKeyAlg := pkaDH
    else if (PKOID = id_ecPublicKey) and
            ([digitalSignature,keyCertSign,cRLSign,nonRepudiation]*FKeyUsage <>
             []) then
      FPublicKeyAlg := pkaECDSA
    else if (PKOID = id_ecPublicKey) and (keyAgreement in FKeyUsage) then
      FPublicKeyAlg := pkaECDH
    else
      FPublicKeyAlg := pkaOther;

    FExtKeyUsage.Clear;
    Ext := nil;
    try
      if ExtractNamedExtension(FCert,id_ce_extKeyUsage,Ext) = E_OK then begin
        for I := 0 to Ext.extnValue.ItemCount - 1 do
          FExtKeyUsage.Add(Ext.extnValue.Items[I].DisplayContent);
      end;
    finally
      Ext.Free;
    end;
    FPathLenConstraint := ExtractPathLenConstraint(FCert);
    FCA := FPathLenConstraint <> -1;

    FAuthorityKeyIdentifier := OSToHex(ExtractIssuerKeyIdentifier(FCert),True);
    FSubjectKeyIdentifier := OSToHex(ExtractSubjectKeyIdentifier(FCert),True);
  end;
end;

procedure TCertificateItem.SetCertId(const Value: string);
var
  GreatOwner: TPersistent;
begin
  GreatOwner := TCertificateCollection(Collection).GetOwner;
  if (csLoading in TComponent(GreatOwner).ComponentState) then
    FCertId := Value;
end;

procedure TCertificateItem.SetCertIndex(const Value: Integer);
var
  GreatOwner: TPersistent;
begin
  GreatOwner := TCertificateCollection(Collection).GetOwner;
  if (Value < 0) or (GreatOwner = nil) or
     not (GreatOwner is TX509TrustedCertificates) then begin
    if FCertId = '' then
      SetCert(nil);
  end else if (csLoading in TComponent(GreatOwner).ComponentState) or
              (TX509TrustedCertificates(GreatOwner).Count <= Value) then
    FCertIndex := Value
  else begin
    SetCert(TX509TrustedCertificates(GreatOwner).Certs[Value]);
    Changed(False);
  end;
end;

procedure TCertificateItem.SetExtKeyUsage(const Value: TStrings);
begin
  // Read only
end;

procedure TCertificateItem.SetGroupIndex(const Value: Integer);
begin
  FGroupIndex := Value;
  Changed(False);
end;

procedure TCertificateItem.SetIssuer(const Value: TMPX501Name);
begin
  // Read only
end;

procedure TCertificateItem.SetKeyUsage(const Value: TKeyUsage);
begin
  // Read only
end;

procedure TCertificateItem.SetPathLenConstraint(const Value: Integer);
begin
  // Read only
end;

procedure TCertificateItem.SetPublicKeyAlg(const Value: TPublicKeyAlg);
begin
  // Read only
end;

procedure TCertificateItem.SetSerial(const Value: string);
begin
  // Read only
end;

procedure TCertificateItem.SetSubject(const Value: TMPX501Name);
begin
  // Read only
end;

procedure TCertificateItem.SetSubjectKeyIdentifier(const Value: string);
begin
  // Read only
end;

procedure TCertificateItem.SetValidity(const Value: TMPX509Validity);
begin
  // Read only
end;

{ TCertificateCollection }

procedure TCertificateCollection.AssignTo(Dest: TPersistent);
var
  D: TCertificateCollection;
  Idx: Integer;
begin
  if (Dest is TCertificateCollection) and (Dest <> Self) then begin
    if Dest <> Self then begin
      D := TCertificateCollection(Dest);
      D.Clear;
      for Idx := 0 to Count - 1 do
        D.Add.Assign(Items[Idx]);
    end;
  end else
    inherited;
end;

constructor TCertificateCollection.Create(
  AOwner: TX509TrustedCertificates);
begin
  inherited Create(AOwner,TCertificateItem);
end;

function TCertificateCollection.GetAttr(Index: Integer): string;
begin
  case Index of
    0: Result := 'GroupIndex';
    1: Result := 'CertIndex';
    2: Result := 'commonName';
    3: Result := 'organizationName';
    4: Result := 'organizationalUnitName';
  else
    Result := '';
  end;
end;

function TCertificateCollection.GetAttrCount: Integer;
begin
  Result := 5;
end;

function TCertificateCollection.GetItemAttr(Index,
  ItemIndex: Integer): string;
begin
  case Index of
    0: Result := IntToStr(TCertificateItem(Items[ItemIndex]).GroupIndex);
    1: Result := IntToStr(TCertificateItem(Items[ItemIndex]).CertIndex);
    2: Result := TCertificateItem(Items[ItemIndex]).Subject.GetcommonName;
    3: Result := TCertificateItem(Items[ItemIndex]).Subject.GetorganizationName;
    4: Result := TCertificateItem(Items[ItemIndex]).Subject.GetorganizationalUnitName;
  else
    Result := Items[ItemIndex].DisplayName;
  end;
end;

procedure TCertificateCollection.Loaded;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TCertificateItem(Items[I]).Loaded;
end;

procedure TCertificateCollection.SetItemName(Item: TCollectionItem);
begin
  if Item.Index >= TX509TrustedCertificates(GetOwner).Count then
    TCertificateItem(Item).CertIndex := Item.Index
  else
    TCertificateItem(Item).Cert := TX509TrustedCertificates(GetOwner).Certs[Item.Index];
  inherited SetItemName(Item);
end;

procedure TCertificateCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if (GetOwner <> nil) and (GetOwner is TX509TrustedCertificates) then
    TX509TrustedCertificates(GetOwner).UpdateHijackers;
end;

{ TCustomX509TrustedCertificates }

procedure TCustomX509TrustedCertificates.AddHijacker(
  AHijacker: TCustomX509TrustedCertificates);
begin
  FHijackers.Add(AHijacker);
  if not (csLoading in ComponentState) then
    UpdateHijacker(AHijacker);
end;

function TCustomX509TrustedCertificates.ClearHijacker(
  AHijacker: TCustomX509TrustedCertificates): Integer;
begin
  Result := AHijacker.FGroupIndex;
  AHijacker.Clear;
  AHijacker.FGroupIndex := Result;
end;

constructor TCustomX509TrustedCertificates.Create(AOwner: TComponent);
begin
  inherited;
  FHijackers := TList.Create;
  FGroupIndex := -1;
end;

destructor TCustomX509TrustedCertificates.Destroy;
begin
  SetCertSource(nil);
  inherited;
  FHijackers.Free;
  FHijackers := nil;
end;

function TCustomX509TrustedCertificates.GetCertificateCollectionCount: Integer;
begin
  Result := 0;
end;

function TCustomX509TrustedCertificates.GetCertificateCollections(
  Index: Integer): TCertificateCollection;
begin
  Result := nil;
end;

function TCustomX509TrustedCertificates.Hijacks(
  ASource: TCustomX509TrustedCertificates): Boolean;
begin
  Result := (CertSource = ASource) and (GroupIndex >= 0);
end;

function TCustomX509TrustedCertificates.IsHijacker(
  ASource: TCustomX509TrustedCertificates): Boolean;
var
  Idx: Integer;
begin
  Result := ASource = Self;
  if not Result then
    for Idx := 0 to FHijackers.Count - 1 do begin
      Result := TCustomX509TrustedCertificates(FHijackers[Idx]).IsHijacker(ASource);
      if Result then Exit;
    end;
end;

procedure TCustomX509TrustedCertificates.Loaded;
begin
  inherited Loaded;

end;

procedure TCustomX509TrustedCertificates.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FCertSource then
      SetCertSource(nil);
    while FHijackers.Remove(AComponent) >= 0 do;
  end;
end;

procedure TCustomX509TrustedCertificates.RemoveHijacker(
  AHijacker: TCustomX509TrustedCertificates);
begin
  if Assigned(FHijackers) then
    while FHijackers.Remove(AHijacker) >= 0 do;
end;

procedure TCustomX509TrustedCertificates.SetCertificateCollections(
  Index: Integer; const Value: TCertificateCollection);
begin

end;

procedure TCustomX509TrustedCertificates.SetCertSource(
  const Value: TCustomX509TrustedCertificates);
var
  GroupIdx: Integer;
begin
  if Value <> FCertSource then begin
    if Assigned(FCertSource) then
      FCertSource.RemoveHijacker(Self);
    if Assigned(Value) and not IsHijacker(Value) then begin
      GroupIdx := FGroupIndex;
      Clear;
      FGroupIndex := GroupIdx;
      FCertSource := Value;
      if Assigned(Value) then
        Value.AddHijacker(Self);
    end else
      FCertSource := nil;
  end;
end;

procedure TCustomX509TrustedCertificates.SetGroupIndex(
  const Value: Integer);
begin
  if Value <> FGroupIndex then begin
    if Value < 0 then
      Clear
    else begin
      FGroupIndex := Value;
      if Assigned(FCertSource) and not (csLoading in ComponentState) then
        FCertSource.UpdateHijacker(Self);
    end;
  end;
end;

procedure TCustomX509TrustedCertificates.UpdateHijackers;
var
  I: Integer;
begin
  if Assigned(FHijackers) then
    for I := 0 to FHijackers.Count - 1 do
      UpdateHijacker(FHijackers[I]);
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
