{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     TLSInternalServer Unit                            }
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
unit TlsInternalServer;

interface

uses
  {$IFDEF LINUX}
  Libc,
  {$IFDEF GUI_APP}
  QForms,
  {$ENDIF}
  {$ELSE}
  Windows, Messages,
  {$IFDEF GUI_APP}
  Forms,
  {$ENDIF}
  {$ENDIF}
  SysUtils, Classes, SyncObjs,
  ResourceFile, SecUtils, ReadStrm, Pkix, Asn1, X509Base, MpPK, MpIF, Tls,
  TlsClass,
  {$IFDEF LINUX}
  Pkix_Cert,
  {$ENDIF}
  StreamSecII, MpX509, MpYarrow;

type
  TCustomTLSInternalServer = class;

  ITLSSocket = interface
    function GetTLSServer: TCustomTLSInternalServer;
    procedure SetSocketID(const Value: Pointer);
    procedure SetTLSServer(const Value: TCustomTLSInternalServer);
    property TLSServer: TCustomTLSInternalServer read GetTLSServer write SetTLSServer;
  end;

{$IFDEF SHA1_AND_MD5}
  TAbstractTLSSocket = class;

  ISocketRefCounter = interface
  ['{84180293-ADB3-4F10-9C2E-199D000F228E}']
    procedure Reset;
  end;

  TKeepAlive = class(TInterfacedObject,ISocketRefCounter)
  private
    FSocket: TAbstractTLSSocket;
  public
    constructor Create(ASocket: TAbstractTLSSocket);
    destructor Destroy; override;
    procedure Reset;
  end;

  TRefCounter = class(TInterfacedObject,ISocketRefCounter)
  private
    FSocket: TAbstractTLSSocket;
  public
    constructor Create(ASocket: TAbstractTLSSocket);
    destructor Destroy; override;
    procedure Reset;
  end;

  TAbstractTLSSocket = class(TObject,ITLSSocket)
  private
    FRefCounter: ISocketRefCounter;
    FKeepAlive: ISocketRefCounter;
    FSocketID: Pointer;
    FLock: TCriticalSection;
    FInData: TSecureMemoryStream;
    FInDataReadPos: Integer;
    FOutData: TSecureMemoryStream;
    FTLSServer: TCustomTLSInternalServer;
    FSending: Boolean;
    FEncrypted: Boolean;
    FConnected: Boolean;
    FSessionID: TSessionID;
    FSleepInterval: Cardinal;
    FClientCertDNSName: string;
    FClientCertIP: string;
    FClientCertURI: string;
    FServerCertDNSName: string;
    FServerCertIP: string;
    FServerCertURI: string;
    FIPToCheck: string;
    FURIToCheck: string;
    FDNSNameToCheck: string;
    FEntity: TConnectionEnd;
    FCipherSuite: TCipherSuites;
    FConnectTimeOut: Cardinal;
    FTLSPeer: TCustomTLS_ContentLayer;
    function GetSending: Boolean;
    function GetTLSServer: TCustomTLSInternalServer;
    procedure SetSocketID(const Value: Pointer);
    procedure SetTLSServer(const Value: TCustomTLSInternalServer);
    procedure SetDNSNameToCheck(const Value: string);
    procedure SetIPToCheck(const Value: string);
    procedure SetURIToCheck(const Value: string);
    procedure SetConnectTimeOut(const Value: Cardinal);
  protected
    FErrorCode: Integer;
    procedure DoSleep(Milliseconds: Cardinal);
    procedure DoTLSCreate(APeer: TCustomTLS_ContentLayer);
    function GetPeer: TCustomTLS_ContentLayer;
    // TLS layer methods:
    procedure InternalCheckDisconnected;
    procedure InternalConnect;
    procedure InternalDisconnect;
    procedure InternalRead;
    procedure InternalSetConnected(Value: Boolean);
    procedure InternalSend;
    // Socket layer methods:
    procedure RawClose; virtual; abstract;
    procedure RawConnect; virtual; abstract;
    function RawReceive: TStream; virtual; abstract;
    procedure RawSend(Strm: TCustomMemoryStream); virtual; abstract;
    // Application-TLS interface methods:
    procedure CheckAbort;
    procedure Connect; virtual; abstract;
    procedure DoConnected(APeer: TCustomTLS_ContentLayer); virtual;
    procedure Disconnect; virtual; abstract;
    procedure Receive; virtual; abstract;
    procedure Send; virtual; abstract;
    procedure InternalLock; virtual;
    procedure InternalUnlock; virtual;
    procedure SetEncrypted(const Value: Boolean);
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // Protected Properties
    property Encrypted: Boolean read FEncrypted;
    property Sending: Boolean read GetSending;
    property SocketID: Pointer read FSocketID write SetSocketID;
    property SessionID: TSessionID read FSessionID write FSessionID;
    property CipherSuite: TCipherSuites read FCipherSuite write FCipherSuite;
    property TLSServer: TCustomTLSInternalServer read GetTLSServer write SetTLSServer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
    procedure Open;
    procedure Close;
    function ReceiveLength: Integer;
    function ReceiveBuf(var Buf; Count: Integer): Integer;
    procedure ReceiveStream(AStream: TStream;
                            AByteCount: LongInt = -1;
                            const AReadUntilDisconnect: Boolean = False);
    procedure ReceiveStruct(Struct: TASN1Struct);
    procedure SendBuf(const Buf; Count: Integer; SendNow: Boolean = True);
    procedure SendStream(AStream: TStream;
                         const AAll: Boolean = True;
                         const ASize: Integer = 0;
                         SendNow: Boolean = True);
    procedure SendStruct(AStruct: TASN1Struct; SendNow: Boolean = True);
    procedure CopySession(ASource: TAbstractTLSSocket);
    procedure Release;
    function GetClientCert: PASN1Struct;
    function GetClientCertDNSName: string;
    function GetClientCertIP: string;
    function GetClientCertURI: string;
    function GetServerCert: PASN1Struct;
    function GetServerCertDNSName: string;
    function GetServerCertIP: string;
    function GetServerCertURI: string;
    property URIToCheck: string read FURIToCheck write SetURIToCheck;
    property IPToCheck: string read FIPToCheck write SetIPToCheck;
    property DNSNameToCheck: string read FDNSNameToCheck write SetDNSNameToCheck;
    property Entity: TConnectionEnd read FEntity;
    property ErrorCode: Integer read FErrorCode;
    property Connected: Boolean read FConnected;
    property ConnectTimeOut: Cardinal read FConnectTimeOut write SetConnectTimeOut;
    property SleepInterval: Cardinal read FSleepInterval write FSleepInterval;
  end;
{$ENDIF SHA1_AND_MD5}

  ETLSAlert = class(Exception)
  private
    FFatal: Boolean;
    FAlertCode: Integer;
    procedure SetAlertCode(const Value: Integer);
    procedure SetFatal(const Value: Boolean);
  public
    constructor CreateAlert(AAlertCode: Integer; AFatal: Boolean);
    property AlertCode: Integer read FAlertCode write SetAlertCode;
    property Fatal: Boolean read FFatal write SetFatal;
  end;
  EIncomingTLSAlert = class(ETLSAlert);
  EOutgoingTLSAlert = class(ETLSAlert);

  {$IFNDEF BCB}
  TPublicKeyAlg = MPX509.TPublicKeyAlg;
  {$ENDIF}
  TPublicKeyAlgs = set of TPublicKeyAlg;

  TClientOrServer = (cosServerSide,cosClientSide);

  ITLSInternalServer = interface
  ['{0CA5F330-1D29-4BFD-972E-5BC7E2DD6459}']
    procedure DoNewSocket(Socket: ITLSSocket);
    procedure DoRemoveSocket(Socket: ITLSSocket);
  end;

  TCustomTLSInternalServer = class(TCustomX509TrustedCertificates,
                                   ITLSInternalServer)
  private
    FStreamSecII: TStreamSecII;
    FLoaded: Boolean;
{$IFDEF SHA1_AND_MD5}
    FTLSSetupServerWhenLoaded: Boolean;
    FTLSOptions: TTLSOptions;
{$ENDIF SHA1_AND_MD5}
    FMyCerts: TX509TrustedCertificates;
    FRootCerts: TX509TrustedCertificates;
    FTrustedCerts: TX509TrustedCertificates;
    FYarrow: TMPYarrow;
    FOnPassword: TPasswordEvent;
{$IFDEF SHA1_AND_MD5}
    FOnOutgoingAlert: TTLSAlertEvent;
    FOnIncomingAlert: TTLSAlertEvent;
    FOnTLSChangeCipherSpec: TTLSChangeCipherSpec;
    FOnCompress: TTLSCompressEvent;
    FOnDecompress: TTLSDecompressEvent;
    FOnRenegotiate: TTLSRenegotiateEvent;
    FOnSelectCompression: TTLSSelectCompressionEvent;
    FSocketList: TThreadList;
    FNextSocketID: Cardinal;
{$ENDIF SHA1_AND_MD5}
    FOnCertNotAccepted: TCertNotAcceptedEvent;
    FOnCertNotTrusted: TCertNotTrustedEvent;
    FOnCertPolicy: TPolicyEvent;
    FExternalIFPrivateKeyClass: TExternalIFPrivateKeyClass;
    FPrivateKeyRingLoaded: Boolean;
    FMyCertsLoaded: Boolean;
    FMyCertsFileName: TFileName;
    FPublicKeyAlgorithms: TPublicKeyAlgs;
    FRootCertsFileName: TFileName;
    FClientOrServer: TClientOrServer;
    FMyCertsSCLFile: TResourceFile;
    FRootCertsSCLFile: TResourceFile;
    FTrustedCertsSCLFile: TResourceFile;
    FTrustedCertsFileName: TFileName;
    FKeyDerivationBits: Integer;
    FRegisteredPathName: string;
{$IFDEF SHA1_AND_MD5}
    function GetOptions: TTLSOptions;
    procedure SetOptions(const Value: TTLSOptions);
{$ENDIF SHA1_AND_MD5}
    function GetLeastKeyBitSize: Integer;
    procedure SetLeastKeyBitSize(const Value: Integer);
{$IFDEF SHA1_AND_MD5}
    procedure SetOnCompress(const Value: TTLSCompressEvent);
    procedure SetOnDecompress(const Value: TTLSDecompressEvent);
    procedure SetOnIncomingAlert(const Value: TTLSAlertEvent);
    procedure SetOnOutgoingAlert(const Value: TTLSAlertEvent);
{$ENDIF SHA1_AND_MD5}
    procedure SetOnPassword(const Value: TPasswordEvent);
{$IFDEF SHA1_AND_MD5}
    procedure SetOnRenegotiate(const Value: TTLSRenegotiateEvent);
    procedure SetOnSelectCompression(
      const Value: TTLSSelectCompressionEvent);
    procedure SetOnTLSChangeCipherSpec(const Value: TTLSChangeCipherSpec);
{$ENDIF SHA1_AND_MD5}
    function GetAcceptedPolicies: TStrings;
    function GetRequiredPolicies: TStrings;
    procedure SetAcceptedPolicies(const Value: TStrings);
    procedure SetOnCertNotAccepted(const Value: TCertNotAcceptedEvent);
    procedure SetOnCertNotTrusted(const Value: TCertNotTrustedEvent);
    procedure SetOnCertPolicy(const Value: TPolicyEvent);
    procedure SetRequiredPolicies(const Value: TStrings);
    function GetOffsetFromGMT: Double;
    procedure SetOffsetFromGMT(const Value: Double);
    function GetSessionKeyCount: Integer;
    function GetSessionKeys(index: Integer): TASN1Struct;
{$IFDEF SHA1_AND_MD5}
    function GetTLSSessionCount: Integer;
    function GetTLSSessions(index: Integer): TCustomTLS_ContentLayer;
{$ENDIF SHA1_AND_MD5}
    procedure SetExternalIFPrivateKeyClass(
      const Value: TExternalIFPrivateKeyClass);
    procedure SetSessionKeys(index: Integer; const Value: TASN1Struct);
    procedure SetPrivateKeyRingFileName(const Value: TFileName);
    procedure SetMyCertsFileName(const Value: TFileName);
    procedure SetPublicKeyAlgorithms(const Value: TPublicKeyAlgs);
    procedure SetRootCertsFileName(const Value: TFileName);
    function GetSessionKeyLifeSpan: {$IFDEF MSWINDOWS}TStreamSecIITime;{$ELSE}TTime;{$ENDIF}
    procedure SetRegisteredPathName(const Value: string);
    function GetBeforeImportTLSCert: TBeforeImportTLSCertEvent;
    procedure SetBeforeImportTLSCert(
      const Value: TBeforeImportTLSCertEvent);
    function GetTrustedCertsSCLFile: TResourceFile;
    procedure SetTrustedCertsFileName(const Value: TFileName);
    procedure SetTrustedCertsSCLFile(const Value: TResourceFile);
    function GetTrustedCertList: TCertificateCollection;
    function GetTrustedCertsGroupIndex: Integer;
    function GetTrustedCertsSource: TCustomX509TrustedCertificates;
    procedure SetTrustedCertList(const Value: TCertificateCollection);
    procedure SetTrustedCertsGroupIndex(const Value: Integer);
    procedure SetTrustedCertsSource(
      const Value: TCustomX509TrustedCertificates);
    function GetIsGlobalServer: Boolean;
    procedure SetIsGlobalServer(const Value: Boolean);
    procedure SetKeyDerivationBits(const Value: Integer);
    procedure SetSessionKeyLifeSpan(const Value: {$IFDEF MSWINDOWS}TStreamSecIITime);{$ELSE}TTime);{$ENDIF}
    procedure SetClientOrServer(const Value: TClientOrServer);
    procedure SetMyCertsSCLFile(const Value: TResourceFile);
    procedure SetPrivateKeyRingFile(const Value: TResourceFile);
    procedure SetRootCertsSCLFile(const Value: TResourceFile);
    function GetPrivateKeyRingFile: TResourceFile;
    function GetPrivateKeyRingFileName: TFileName;
    function GetPrivateKeyRing: TSsPrivateKeyRingComponent;
    procedure SetPrivateKeyRing(const Value: TSsPrivateKeyRingComponent);
    function GetMyCertList: TCertificateCollection;
    function GetMyCertsGroupIndex: Integer;
    function GetMyCertsSource: TCustomX509TrustedCertificates;
    function GetRootCertList: TCertificateCollection;
    function GetRootCertsGroupIndex: Integer;
    function GetRootCertsSource: TCustomX509TrustedCertificates;
    procedure SetMyCertList(const Value: TCertificateCollection);
    procedure SetMyCertsGroupIndex(const Value: Integer);
    procedure SetMyCertsSource(const Value: TCustomX509TrustedCertificates);
    procedure SetRootCertList(const Value: TCertificateCollection);
    procedure SetRootCertsGroupIndex(const Value: Integer);
    procedure SetRootCertsSource(const Value: TCustomX509TrustedCertificates);
    function GetMyCertsSCLFile: TResourceFile;
    function GetRootCertsSCLFile: TResourceFile;
  protected
    procedure CertListLoaded; override;
    procedure Clear; override;
    procedure DoCertNotTrusted(Sender: TObject;
                               Cert: TASN1Struct;
                               var ExplicitTrust: Boolean);
    procedure DoCertNotAccepted(Sender: TObject;
                                Cert: TASN1Struct;
                                Status: TCertStatusCode);
    procedure DoCertPolicy(Sender: TObject;
                           Cert: TASN1Struct;
                           AcceptedPolicy: TStrings;
                           var Accept: Boolean);
    procedure DoSetRegisteredPathName(const Value: string); virtual;
    procedure DoLoadMyCertsSCLFile(Sender: TObject);
    procedure DoLoadRootCertsSCLFile(Sender: TObject);
    procedure DoLoadTrustedCertsSCLFile(Sender: TObject);
    procedure DoNewSocket(Socket: ITLSSocket);
    procedure DoRemoveSocket(Socket: ITLSSocket);
{$IFDEF SHA1_AND_MD5}
    procedure DoTLSChangeCipherSpec(Sender: TObject;
                                    Client: TCustomTLS_ContentLayer);
    procedure DoTLSCompress(Sender: TObject;
                            Client: TCustomTLS_ContentLayer;
                            Src: PTLSPlainText; PTLen: Integer;
                            Method: Byte;
                            var Dst: PTLSCompressed; var CLen: Integer;
                            var Error: Integer);
    procedure DoTLSDecompress(Sender: TObject;
                              Client: TCustomTLS_ContentLayer;
                              Src: PTLSCompressed; CLen: Integer;
                              Method: Byte;
                              var Dst: PTLSPlainText; var PTLen: Integer;
                              var Error: Integer);
    procedure DoTLSIncomingAlert(Sender: TObject;
                                 Client: TCustomTLS_ContentLayer;
                                 var Fatal: Boolean;
                                 AlertCode: Integer);
    procedure DoTLSOutgoingAlert(Sender: TObject;
                                 Client: TCustomTLS_ContentLayer;
                                 var Fatal: Boolean;
                                 AlertCode: Integer);
    procedure DoTLSRenegotiate(Sender: TObject;
                               Client: TCustomTLS_ContentLayer;
                               const SessionID: TSessionID;
                               var Allow: Boolean);
    procedure DoTLSSelectCompression(Sender: TObject;
                                     Client: TCustomTLS_ContentLayer;
                                     const CompMethods: array of TCompressionMethod;
                                     var CompMethod: TCompressionMethod);
    procedure FreeSocketList;
    procedure HandleCipherSuitesChange(Sender: TObject); virtual;
{$ENDIF SHA1_AND_MD5}
    function GetCertificateCollectionCount: Integer; override;
    function GetCertificateCollections(
      Index: Integer): TCertificateCollection; override;
    procedure SetCertificateCollections(Index: Integer;
      const Value: TCertificateCollection); override;
    function HasPrivateCACert(var ACACert: TASN1Struct): Boolean;
    function HasPrivateSignCert(var ACert: TASN1Struct): Boolean;
    function Hijacks(ASource: TCustomX509TrustedCertificates): Boolean; override;
    procedure InternalLoaded; virtual;
    procedure Loaded; override;
    procedure LoadedResolveResourceFiles; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const NewName: TComponentName); override;
    procedure UpdateHijacker(AHijacker: TCustomX509TrustedCertificates); override;
    procedure ValidatePublicKeyPairs;
    property StreamSecIIComp: TStreamSecII read FStreamSecII;
    property MyCerts: TX509TrustedCertificates read FMyCerts;
    property RootCerts: TX509TrustedCertificates read FRootCerts;
    property TrustedCerts: TX509TrustedCertificates read FTrustedCerts;
    property YarrowComp: TMPYarrow read FYarrow;
    // To be published:
    property MyCertsSCLFile: TResourceFile read GetMyCertsSCLFile write SetMyCertsSCLFile;
    property MyCertsFileName: TFileName read FMyCertsFileName write SetMyCertsFileName;
    property PrivateKeyRingFile: TResourceFile read GetPrivateKeyRingFile write SetPrivateKeyRingFile;
    property PrivateKeyRingFileName: TFileName read GetPrivateKeyRingFileName write SetPrivateKeyRingFileName;
    property RootCertsSCLFile: TResourceFile read GetRootCertsSCLFile write SetRootCertsSCLFile;
    property RootCertsFileName: TFileName read FRootCertsFileName write SetRootCertsFileName;
    property TrustedCertsSCLFile: TResourceFile read GetTrustedCertsSCLFile write SetTrustedCertsSCLFile;
    property TrustedCertsFileName: TFileName read FTrustedCertsFileName write SetTrustedCertsFileName;
    property TrustedCertList: TCertificateCollection read GetTrustedCertList write SetTrustedCertList;
    property TrustedCertsGroupIndex: Integer read GetTrustedCertsGroupIndex write SetTrustedCertsGroupIndex;
    property TrustedCertsSource: TCustomX509TrustedCertificates read GetTrustedCertsSource write SetTrustedCertsSource;
    property OnCertPolicy: TPolicyEvent read FOnCertPolicy write SetOnCertPolicy;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AccumulateEntropy;
    function AddCertificate(const Cert: TASN1Struct;
                            ExplicitTrust: Boolean;
                            var Status: TCertStatusCode;
                            AllowExpired: Boolean = False): Integer; override;
    function AddCRL(const CRL: TASN1Struct;
                    var Status: TCertStatusCode): Integer; override;
    procedure CheckLoaded;
    procedure ClearCertificates; dynamic;
    procedure ClearExpiredSessionKeys;
    procedure ExportToPFX(AFileName: string;
                          Password: ISecretKey = nil;
                          ExportCert: TASN1Struct = nil;
                          FriendlyName: WideString = '';
                          ProviderName: WideString = '');
    function FindCRLs(const CACert: TASN1Struct;
                      var CRLs: TASN1Struct): Integer; override;
    function FindRevocation(const Cert: TASN1Struct;
                            var RC: TX509RevokedCertificate): Boolean; override;                
    function FindSMIMECapabilities(Cert: TASN1Struct;
                                   var Capabilities: PASN1Struct): Boolean; override;
{$IFDEF SHA1_AND_MD5}
    function FindTLSMasterSecret(const SessionID: TSessionID;
                                 var MasterSecret: TMasterSecret;
                                 var Index: Integer): Boolean; overload;
    function FindTLSMasterSecret(const SessionID: TSessionID;
                                 var Index: Integer): ISecretKey; overload;
    function FindTLSSession(const SessionID: TSessionID; var Client: TCustomTLS_ContentLayer): Integer; overload;
    function FindTLSSession(UserData: Pointer; var Client: TCustomTLS_ContentLayer): Integer; overload;
{$ENDIF SHA1_AND_MD5}
    procedure ImportFromPFX(AFileName: TFileName; Password: ISecretKey);
    function LoadMyCertsFromFile(AFileName: TFileName): Boolean;
    function LoadMyCertsFromStream(AStream: TStream): Boolean;
    procedure LoadPrivateKeyRingFromFile(AFileName: TFileName; Password: ISecretKey);
    procedure LoadPrivateKeyRingFromStream(AStream: TStream; Password: ISecretKey);
    procedure LoadRootCertsFromFile(AFileName: TFileName);
    procedure LoadRootCertsFromStream(AStream: TStream);
    procedure LoadP7BRootCertsFromStream(AStream: TStream);
    procedure LoadTrustedCertsFromFile(AFileName: TFileName);
    procedure LoadTrustedCertsFromStream(AStream: TStream);
{$IFDEF SHA1_AND_MD5}
    function RenewEphemeralDHKey(DHBitSize, DHQBitSize: Integer): Boolean;
{$ENDIF SHA1_AND_MD5}
    function RevokeCertificate(const Cert: TASN1Struct;
                               Reason: CRLReason;
                               HoldInstruction: string;
                               InvalidityDate: TDateTime;
                               NextCRLUpdate: TDateTime;
                               PrivKey: IMPPrivateKey = nil;
                               CACert: PASN1Struct = nil): Boolean; overload; override;
    procedure SavePrivateKeyRing(AFileName: TFileName); virtual;
    procedure SavePrivateKeyRingToStream(AStream: TStream); virtual;   
{$IFDEF SHA1_AND_MD5}
    procedure StartEphemeralKeyGen(const RSABitSizes: array of Integer;
                                   const DHBitSizes: array of Integer;
                                   const DHQBitSizes: array of Integer;
                                   RunOnce: Boolean);
    procedure StopEphemeralKeyGen;
    function TLSAddClientSession: TCustomTLS_ContentLayer;
    function TLSAddServerSession: TCustomTLS_ContentLayer;
    function TLSClose(const SessionID: TSessionID;
                      Response: TStream;
                      ErrorCode: PInteger = nil): Integer; overload;
    function TLSDecodeData(var SessionID: TSessionID;
                           Src, Data, Response: TStream;
                           ErrorCode: PInteger = nil): Integer; overload;
    function TLSEncodeData(const SessionID: TSessionID;
                           Data, Response: TStream;
                           ErrorCode: PInteger = nil): Integer; overload;
    function TLSAccept(UserData: Pointer;
                       Src, Response: TStream;
                       ErrorCode: PInteger = nil): Integer;
    function TLSClose(UserData: Pointer;
                      Response: TStream;
                      ErrorCode: PInteger = nil): Integer; overload;
    function TLSConnect(UserData: Pointer;
                        Response: TStream;
                        ErrorCode: PInteger = nil): Integer; overload;
    function TLSConnect(const SessionID: TSessionID;
                        Response: TStream;
                        ErrorCode: PInteger = nil): Integer; overload;
    function TLSConnect(UserData: Pointer;
                        const SessionID: TSessionID;
                        Response: TStream;
                        ErrorCode: PInteger = nil): Integer; overload;
    function TLSConnect(UserData: Pointer;
                        const SessionID: TSessionID;
                        const CipherSuites: TCipherSuites;
                        Response: TStream;
                        ErrorCode: PInteger): Integer; overload;
    function TLSConnectAsServer(UserData: Pointer;
                                Response: TStream;
                                ErrorCode: PInteger = nil): Integer;
    function TLSDecodeData(UserData: Pointer;
                           Src, Data, Response: TStream;
                           ErrorCode: PInteger = nil): Integer; overload;
    function TLSEncodeData(UserData: Pointer;
                           Data, Response: TStream;
                           ErrorCode: PInteger = nil): Integer; overload;
    procedure TLSSetupServer;
{$ENDIF SHA1_AND_MD5}
    function ValidateMyCert(const Cert: TASN1Struct): Boolean;
    function ValidateSCLSignatureCerts: Boolean;
    property ExternalIFPrivateKeyClass: TExternalIFPrivateKeyClass read FExternalIFPrivateKeyClass write SetExternalIFPrivateKeyClass;
    property SessionKeyCount: Integer read GetSessionKeyCount;
    property SessionKeys[index: Integer]: TASN1Struct read GetSessionKeys write SetSessionKeys;
{$IFDEF SHA1_AND_MD5}
    property TLSSessionCount: Integer read GetTLSSessionCount;
    property TLSSessions[index: Integer]: TCustomTLS_ContentLayer read GetTLSSessions;
{$ENDIF SHA1_AND_MD5}
    // To be published:
    property AcceptedCertPolicies: TStrings read GetAcceptedPolicies write SetAcceptedPolicies;
    property ClientOrServer: TClientOrServer read FClientOrServer write SetClientOrServer;
    property HoursOffsetFromGMT: Double read GetOffsetFromGMT write SetOffsetFromGMT;
{$IFDEF SHA1_AND_MD5}
    property Options: TTLSOptions read GetOptions write SetOptions;
{$ENDIF SHA1_AND_MD5}
    property RequiredCertPolicies: TStrings read GetRequiredPolicies write SetRequiredPolicies;
{$IFDEF SHA1_AND_MD5}
    property OnTLSChangeCipherSpec: TTLSChangeCipherSpec read FOnTLSChangeCipherSpec write SetOnTLSChangeCipherSpec;
    property OnTLSCompress: TTLSCompressEvent read FOnCompress write SetOnCompress;
    property OnTLSDecompress: TTLSDecompressEvent read FOnDecompress write SetOnDecompress;
    property OnTLSIncomingAlert: TTLSAlertEvent read FOnIncomingAlert write SetOnIncomingAlert;
    property OnTLSOutgoingAlert: TTLSAlertEvent read FOnOutgoingAlert write SetOnOutgoingAlert;
    property OnTLSRenegotiate: TTLSRenegotiateEvent read FOnRenegotiate write SetOnRenegotiate;
    property OnTLSSelectCompression: TTLSSelectCompressionEvent read FOnSelectCompression write SetOnSelectCompression;
{$ENDIF SHA1_AND_MD5}
    property KeyDerivationBits: Integer read FKeyDerivationBits write SetKeyDerivationBits default 10;
  published
    property IsGlobalServer: Boolean read GetIsGlobalServer write SetIsGlobalServer default False;
    property LeastKeyBitSize: Integer read GetLeastKeyBitSize write SetLeastKeyBitSize default 1024;
    property MyCertList: TCertificateCollection read GetMyCertList write SetMyCertList;
    property MyCertsGroupIndex: Integer read GetMyCertsGroupIndex write SetMyCertsGroupIndex default -1;
    property MyCertsSource: TCustomX509TrustedCertificates read GetMyCertsSource write SetMyCertsSource;
    property PrivateKeyRing: TSsPrivateKeyRingComponent read GetPrivateKeyRing write SetPrivateKeyRing;
    property PublicKeyAlgorithms: TPublicKeyAlgs read FPublicKeyAlgorithms write SetPublicKeyAlgorithms;
    property RegisteredPathName: string read FRegisteredPathName write SetRegisteredPathName;
    property RootCertList: TCertificateCollection read GetRootCertList write SetRootCertList;
    property RootCertsGroupIndex: Integer read GetRootCertsGroupIndex write SetRootCertsGroupIndex default -1;
    property RootCertsSource: TCustomX509TrustedCertificates read GetRootCertsSource write SetRootCertsSource;
    property SessionKeyLifeSpan: {$IFDEF MSWINDOWS}TStreamSecIITime{$ELSE}TTime{$ENDIF} read GetSessionKeyLifeSpan write SetSessionKeyLifeSpan;
    property BeforeImportTLSCert: TBeforeImportTLSCertEvent read GetBeforeImportTLSCert write SetBeforeImportTLSCert;
    property OnCertNotTrusted: TCertNotTrustedEvent read FOnCertNotTrusted write SetOnCertNotTrusted;
    property OnCertNotAccepted: TCertNotAcceptedEvent read FOnCertNotAccepted write SetOnCertNotAccepted;
    property OnPassword: TPasswordEvent read FOnPassword write SetOnPassword;
  end;

  TSleepProc = procedure (Milliseconds: Cardinal);

  TTLSInternalServer = class(TCustomTLSInternalServer)
  published
    property AcceptedCertPolicies;
    property ClientOrServer default cosServerSide;
    property HoursOffsetFromGMT stored False;
    property MyCertsSCLFile;
    property MyCertsFileName;
{$IFDEF SHA1_AND_MD5}
    property Options;
{$ENDIF SHA1_AND_MD5}
    property PrivateKeyRingFile;
    property PrivateKeyRingFileName;
    property RequiredCertPolicies;
    property RootCertsSCLFile;
    property RootCertsFileName;
    property TrustedCertList;
    property TrustedCertsGroupIndex default -1;
    property TrustedCertsSource;     
    property TrustedCertsSCLFile;
    property TrustedCertsFileName;
    property OnCertPolicy;         
{$IFDEF SHA1_AND_MD5}
    property OnTLSChangeCipherSpec;
    property OnTLSCompress;
    property OnTLSDecompress;
    property OnTLSIncomingAlert;
    property OnTLSOutgoingAlert;
    property OnTLSRenegotiate;
    property OnTLSSelectCompression;
{$ENDIF SHA1_AND_MD5}
  end;

{$IFDEF GUI_APP}
  TGetFormClassEvent = procedure (Sender: TObject; var AFormClass: TFormClass) of object;
{$ENDIF}
  TGetNewCertDlgEvent = procedure (Sender: TObject;
                                   ACreateType: TNewCertCreateEnum;  
                                   var ANewCertDlg: INewCertDlg) of object;

  TCustomSimpleTLSInternalServer = class(TCustomTLSInternalServer, ITLSServer)
  private
    FMyCertsFile: TResourceFile;
    FRootCertsFile: TResourceFile;
    FTrustedCertsFile: TResourceFile;
    FPrivateKeyRingFile: TResourceFile;
    FBeforeCreateNewCertDlg: TGetNewCertDlgEvent;
    FUserList: TComponent;
{$IFDEF GUI_APP}
    FOnGetNewCertDlg: TGetFormClassEvent;
    procedure SetOnGetNewCertDlg(const Value: TGetFormClassEvent);
{$ENDIF GUI_APP}
    procedure ReadMyCerts(AStream: TStream);
    procedure ReadPrivateKeyRing(AStream: TStream);
    procedure ReadRootCerts(AStream: TStream);
    procedure ReadTrustedCerts(AStream: TStream);
    procedure WriteMyCerts(AStream: TStream);
    procedure WritePrivateKeyRing(AStream: TStream);
    procedure WriteRootCerts(AStream: TStream);
    procedure WriteTrustedCerts(AStream: TStream);
    procedure SetBeforeCreateNewCertDlg(const Value: TGetNewCertDlgEvent);
    procedure SetUserList(const Value: TComponent);
  protected
    procedure DefineProperties(AFiler: TFiler); override;
{$IFDEF SHA1_AND_MD5}
    procedure HandleCipherSuitesChange(Sender: TObject); override;
{$ENDIF SHA1_AND_MD5}
    procedure InternalLoaded; override;
    function InternalWriteMyCerts: Boolean;
    function InternalWriteRootCerts: Boolean;
    function InternalWriteTrustedCerts: Boolean;
    property InternalPrivateKeyRingFile: TResourceFile read FPrivateKeyRingFile;
    property InternalMyCertsFile: TResourceFile read FMyCertsFile;
    property InternalRootCertsFile: TResourceFile read FRootCertsFile;
    property InternalTrustedCertsFile: TResourceFile read FTrustedCertsFile;
    procedure LoadedResolveResourceFiles; override;
    procedure KeyAndSign(ADialog: INewCertDlg);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CreateNewCertDlg(ACreateType: TNewCertCreateEnum): INewCertDlg;
{$IFDEF GUI_APP}
    function GetNewCertDlg: TFormClass;
    property OnGetNewCertDlg: TGetFormClassEvent read FOnGetNewCertDlg write SetOnGetNewCertDlg;
{$ENDIF}
    property BeforeCreateNewCertDlg: TGetNewCertDlgEvent read FBeforeCreateNewCertDlg write SetBeforeCreateNewCertDlg;
    property UserList: TComponent read FUserList write SetUserList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function NewCACertDlg(AExportData: TStream): Boolean;
    function NewCACertReqDlg(AExportData: TStream): Boolean;
    function NewClientCertDlg(AExportData: TStream): Boolean;
    function NewClientCertReqDlg(AExportData: TStream): Boolean;
    function NewP2PCertDlg(AExportData: TStream): Boolean;
    function NewP2PCertReqDlg(AExportData: TStream): Boolean;
    function NewServerCertDlg(AExportData: TStream): Boolean;
    function NewServerCertReqDlg(AExportData: TStream): Boolean;
    function SaveMyCertsToSCLFile(AFileName: TFileName): Boolean;
    function SaveMyCertsToSCLStream(AStream: TStream): Boolean;
    procedure SavePrivateKeyRing(AFileName: TFileName); override;
    function SaveRootCertsToSCLFile(AFileName: TFileName): Boolean;
    function SaveRootCertsToSCLStream(AStream: TStream): Boolean;
    function SaveTrustedCertsToSCLFile(AFileName: TFileName): Boolean;
    function SaveTrustedCertsToSCLStream(AStream: TStream): Boolean;
  published
    property ClientOrServer default cosServerSide;
    property KeyDerivationBits default 10;
  end;

  TSimpleTLSInternalServer = class(TCustomSimpleTLSInternalServer)
  published
{$IFDEF SHA1_AND_MD5}
    property Options;
{$ENDIF SHA1_AND_MD5}
    property TrustedCertList;
    property TrustedCertsGroupIndex default -1;
    property TrustedCertsSource;
    property UserList;
    property OnCertPolicy; 
    property BeforeCreateNewCertDlg;
{$IFDEF SHA1_AND_MD5}
    property OnTLSChangeCipherSpec;
    property OnTLSCompress;
    property OnTLSDecompress;
    property OnTLSIncomingAlert;
    property OnTLSOutgoingAlert;
    property OnTLSRenegotiate;
    property OnTLSSelectCompression;
{$ENDIF SHA1_AND_MD5}
{$IFDEF GUI_APP}
    property OnGetNewCertDlg;
{$ENDIF}
  end;

var
  SleepProc: TSleepProc = nil;
  GlobalServer: TCustomTLSInternalServer;

function TLSSocketInstanceCount: Integer;

implementation

uses
{$IFDEF GUI_APP}
{$IFDEF MSWINDOWS}
  NewCertDlg, Controls, Dialogs,
{$ENDIF}
{$IFDEF LINUX}
  QNewCertDlg, QControls, QDialogs,
{$ENDIF}
  PasswordMain, ExportDlg,
{$ENDIF GUI_APP}
  Pkcs12, TlsUtils, TlsConst, SsRijndael, SsCertMgrUtils,
{$IFNDEF LINUX}
  Pkix_Cert,
{$ENDIF LINUX}
  Pkcs_10, SsBase64, MpIFConversion;

var
  GTLSSocketInstanceCount: Integer = 0;

function TLSSocketInstanceCount: Integer;
begin
  Result := GTLSSocketInstanceCount;
end;

type
  TCustomX509TrustedCertificatesHack = class(TCustomX509TrustedCertificates);

{ TCustomTLSInternalServer }

procedure TCustomTLSInternalServer.AccumulateEntropy;
begin
  FYarrow.Accumulate;
end;

function TCustomTLSInternalServer.AddCertificate(const Cert: TASN1Struct;
  ExplicitTrust: Boolean; var Status: TCertStatusCode;
  AllowExpired: Boolean): Integer;
var
  IsPersonal: Boolean;
begin
  try
    IsPersonal := StreamSecIIComp.ValidateMyCert(Cert);
  except
    IsPersonal := False;
  end;
  if IsPersonal then
    Result := MyCerts.AddCertificate(Cert,ExplicitTrust,Status,AllowExpired)
  else if ExtractPathLenConstraint(Cert) <> -1 then
    Result := RootCerts.AddCertificate(Cert,ExplicitTrust,Status,AllowExpired)
  else if ExplicitTrust then begin
    Result := TrustedCerts.AddCertificate(Cert,True,Status,AllowExpired);
    if Result >= 0 then
      StreamSecIIComp.CollectedCertificates.AddCertificate(Cert,True,Status,AllowExpired);
  end else begin
    Status := crcInvalidKeyUsage;
    Result := -1;
  end;
  if Result >= 0 then
    Result := 0;
end;

function TCustomTLSInternalServer.AddCRL(const CRL: TASN1Struct;
  var Status: TCertStatusCode): Integer;
begin
  Result := RootCerts.AddCRL(CRL,Status);
end;

procedure TCustomTLSInternalServer.CertListLoaded;
begin
  // No action
end;

procedure TCustomTLSInternalServer.Clear;
begin
  { No action. This method will only be called by the internal
    TX509TrustedCertificates components, prior to a call to UpdateHijackers. 
  }
end;

procedure TCustomTLSInternalServer.ClearCertificates;
begin
  MyCerts.Clear;
  RootCerts.Clear;
  StreamSecIIComp.CollectedCertificates.Clear;
end;

procedure TCustomTLSInternalServer.ClearExpiredSessionKeys;
begin
  FStreamSecII.ClearExpiredSessionKeys;
end;

constructor TCustomTLSInternalServer.Create(AOwner: TComponent);
begin
  inherited;
  FStreamSecII := TStreamSecII.Create(Self);
{$IFDEF SHA1_AND_MD5}
  FTLSOptions := TTLSOptions.Create(Self);  
{$ENDIF SHA1_AND_MD5}
  FMyCerts := TX509TrustedCertificates.Create(Self);
  TCustomX509TrustedCertificatesHack(FMyCerts).AddHijacker(Self);
  FRootCerts := TX509TrustedCertificates.Create(Self);
  TCustomX509TrustedCertificatesHack(FRootCerts).AddHijacker(Self);
  FTrustedCerts := TX509TrustedCertificates.Create(Self);
  TCustomX509TrustedCertificatesHack(FTrustedCerts).AddHijacker(Self);
  FMyCerts.TrustedCACertificates := FRootCerts;
  FTrustedCerts.TrustedCACertificates := FRootCerts;
  FStreamSecII.MyCertificates := FMyCerts;
  FStreamSecII.CACertificates := FRootCerts;
  FStreamSecII.AllowPlainTextKeys := True;
  FStreamSecII.CacheKeyInterfaces := True;
{$IFDEF SHA1_AND_MD5}
  FStreamSecII.OnTLSChangeCipherSpec := DoTLSChangeCipherSpec;
  FStreamSecII.OnTLSCompress := DoTLSCompress;
  FStreamSecII.OnTLSDecompress := DoTLSDecompress;
  FStreamSecII.OnTLSIncomingAlert := DoTLSIncomingAlert;
  FStreamSecII.OnTLSOutgoingAlert := DoTLSOutgoingAlert;
  FStreamSecII.OnTLSRenegotiate := DoTLSRenegotiate;
  FStreamSecII.OnTLSSelectCompression := DoTLSSelectCompression;
{$ENDIF SHA1_AND_MD5}
  FStreamSecII.CollectedCertificates.OnCertNotTrusted := DoCertNotTrusted;
  FStreamSecII.CollectedCertificates.OnCertNotAccepted := DoCertNotAccepted;
  FStreamSecII.CollectedCertificates.OnCertPolicy := DoCertPolicy;
  FStreamSecII.TrustedCertificates := FTrustedCerts;
  FMyCerts.OnCertNotTrusted := DoCertNotTrusted;
  FMyCerts.OnCertNotAccepted := DoCertNotAccepted;
  FMyCerts.OnCertPolicy := DoCertPolicy;
  FRootCerts.OnCertNotTrusted := DoCertNotTrusted;
  FRootCerts.OnCertNotAccepted := DoCertNotAccepted;
  FRootCerts.OnCertPolicy := DoCertPolicy;
  FTrustedCerts.OnCertNotTrusted := DoCertNotTrusted;
  FTrustedCerts.OnCertNotAccepted := DoCertNotAccepted;
  FTrustedCerts.OnCertPolicy := DoCertPolicy;
  FYarrow := TMPYarrow.Create(Self);
  FYarrow.ForceSlowReseed(nil^,0,0);
  // Default properties:
  LeastKeyBitSize := 1024;
{$IFDEF SHA1_AND_MD5}
  Options.Export40Bit := prNotAllowed;
  Options.Export56Bit := prNotAllowed;
{$ENDIF SHA1_AND_MD5}
{$IFDEF MSWINDOWS}
  SetOffsetFromGMT(OffsetFromUTC);
{$ENDIF}     
{$IFDEF SHA1_AND_MD5}
  FSocketList := TThreadList.Create;
{$ENDIF SHA1_AND_MD5}
  if (AOwner = nil) or not (csLoading in AOwner.ComponentState) then begin
    FStreamSecII.Name := Name + 'StreamSecII';
    FMyCerts.Name := Name + 'MyCerts';
    FRootCerts.Name := Name + 'RootCerts';
    FTrustedCerts.Name := Name + 'TrustedCerts';
  end;
end;

destructor TCustomTLSInternalServer.Destroy;
var
  Debug: string;
begin
  try
{$IFDEF SHA1_AND_MD5}
    Debug := 'StopEphemeralKeyGen';
    StopEphemeralKeyGen;
    Debug := 'FreeSocketList';
    FreeSocketList;  
{$ENDIF SHA1_AND_MD5}
    Debug := 'FRootCertsSCLFile.RemoveLoadNotification';
    if Assigned(FRootCertsSCLFile) then
      FRootCertsSCLFile.RemoveLoadNotification(Self,DoLoadRootCertsSCLFile);
    Debug := 'FMyCertsSCLFile.RemoveLoadNotification';
    if Assigned(FMyCertsSCLFile) then
      FMyCertsSCLFile.RemoveLoadNotification(Self,DoLoadMyCertsSCLFile);
    Debug := 'FYarrow.Free';
    FYarrow.Free;
    Debug := 'FMyCerts.Free';
    FMyCerts.Free;
    Debug := 'FRootCerts.Free';
    FRootCerts.Free;
    Debug := 'FTrustedCerts.Free';
    FTrustedCerts.Free;   
{$IFDEF SHA1_AND_MD5}
    Debug := 'FTLSOptions.Free';
    FTLSOptions.Free;
{$ENDIF SHA1_AND_MD5}
    Debug := 'FStreamSecII.Free';
    FStreamSecII.Free;
    Debug := 'inherited Destroy';
    inherited Destroy;
  except
    on E: Exception do
      raise Exception.Create('TCustomTLSInternalServer.Destroy: ' + Debug + #13#10 +
                              E.Message);
  end;
end;

procedure TCustomTLSInternalServer.DoCertNotAccepted(Sender: TObject;
  Cert: TASN1Struct; Status: TCertStatusCode);
begin
  if Assigned(FOnCertNotAccepted) then
    FOnCertNotAccepted(Sender,Cert,Status);
end;

procedure TCustomTLSInternalServer.DoCertNotTrusted(Sender: TObject;
  Cert: TASN1Struct; var ExplicitTrust: Boolean);
begin
  if Assigned(FOnCertNotTrusted) then
    FOnCertNotTrusted(Sender,Cert,ExplicitTrust);
end;

procedure TCustomTLSInternalServer.DoCertPolicy(Sender: TObject;
  Cert: TASN1Struct; AcceptedPolicy: TStrings; var Accept: Boolean);
begin
  if Assigned(FOnCertPolicy) then
    FOnCertPolicy(Sender,Cert,AcceptedPolicy,Accept);
end;    

type
  TStreamSecIIHack = class(TStreamSecII);

procedure TCustomTLSInternalServer.DoLoadMyCertsSCLFile(Sender: TObject);
begin
  if Sender = FMyCertsSCLFile then begin
    try
      if Assigned(FMyCertsSCLFile) and
         Assigned(FMyCertsSCLFile.DataStream) then begin
        LoadMyCertsFromStream(FMyCertsSCLFile.DataStream);
        FMyCertsFileName := '';
      end;
    except
      FMyCertsSCLFile.RemoveLoadNotification(Self,DoLoadMyCertsSCLFile);
      FMyCertsSCLFile := nil;
    end;
  end;
end;

procedure TCustomTLSInternalServer.DoLoadRootCertsSCLFile(Sender: TObject);
begin
  if Sender = FRootCertsSCLFile then begin
    try
      if Assigned(FRootCertsSCLFile) and
         Assigned(FRootCertsSCLFile.DataStream) then begin
        LoadRootCertsFromStream(FRootCertsSCLFile.DataStream);
        FRootCertsFileName := '';
      end;
    except
      FRootCertsSCLFile.RemoveLoadNotification(Self,DoLoadRootCertsSCLFile);
      FRootCertsSCLFile := nil;
    end;
  end;
end;

procedure TCustomTLSInternalServer.DoLoadTrustedCertsSCLFile(
  Sender: TObject);
begin
  if Sender = FTrustedCertsSCLFile then begin
    try
      if Assigned(FTrustedCertsSCLFile) and
         Assigned(FTrustedCertsSCLFile.DataStream) then begin
        LoadTrustedCertsFromStream(FTrustedCertsSCLFile.DataStream);
        FTrustedCertsFileName := '';
      end;
    except
      FTrustedCertsSCLFile.RemoveLoadNotification(Self,DoLoadRootCertsSCLFile);
      FTrustedCertsSCLFile := nil;
    end;
  end;
end;

procedure TCustomTLSInternalServer.DoNewSocket(Socket: ITLSSocket);
{$IFDEF SHA1_AND_MD5}
var
  List: TList;       
{$ENDIF SHA1_AND_MD5}
begin
{$IFDEF SHA1_AND_MD5}
  TStreamSecIIHack(FStreamSecII).TLSLock;
  try
    Socket.SetSocketID(Ptr(FNextSocketID));
    InterlockedIncrement(Integer(FNextSocketID));
    List := FSocketList.LockList;
    try
      if List.IndexOf(Pointer(Socket)) < 0 then
        List.Add(Pointer(Socket));
    finally
      FSocketList.UnlockList;
    end;
  finally
    TStreamSecIIHack(FStreamSecII).TLSUnlock;
  end;
{$ENDIF SHA1_AND_MD5}
end;

procedure TCustomTLSInternalServer.DoRemoveSocket(Socket: ITLSSocket);
begin                  
{$IFDEF SHA1_AND_MD5}
  if Assigned(FSocketList) then
    FSocketList.Remove(Pointer(Socket));
{$ENDIF SHA1_AND_MD5}
end;
                                    
{$IFDEF SHA1_AND_MD5}
procedure TCustomTLSInternalServer.DoTLSChangeCipherSpec(Sender: TObject;
  Client: TCustomTLS_ContentLayer);
begin
  if Assigned(FOnTLSChangeCipherSpec) then
    FOnTLSChangeCipherSpec(Sender,Client);
end;

procedure TCustomTLSInternalServer.DoTLSCompress(Sender: TObject;
  Client: TCustomTLS_ContentLayer; Src: PTLSPlainText; PTLen: Integer;
  Method: Byte; var Dst: PTLSCompressed; var CLen, Error: Integer);
begin
  if Assigned(FOnCompress) then
    FOnCompress(Sender,Client,Src,PTLen,Method,Dst,CLen,Error);
end;

procedure TCustomTLSInternalServer.DoTLSDecompress(Sender: TObject;
  Client: TCustomTLS_ContentLayer; Src: PTLSCompressed; CLen: Integer;
  Method: Byte; var Dst: PTLSPlainText; var PTLen, Error: Integer);
begin
  if Assigned(FOnDecompress) then
    FOnDecompress(Sender,Client,Src,CLen,Method,Dst,PTLen,Error);
end;

procedure TCustomTLSInternalServer.DoTLSIncomingAlert(Sender: TObject;
  Client: TCustomTLS_ContentLayer; var Fatal: Boolean; AlertCode: Integer);
begin
  if Assigned(FOnIncomingAlert) then
    FOnIncomingAlert(Sender,Client,Fatal,AlertCode)
  else if AlertCode <> 0 then
    raise EIncomingTLSAlert.CreateAlert(AlertCode,Fatal);
end;

procedure TCustomTLSInternalServer.DoTLSOutgoingAlert(Sender: TObject;
  Client: TCustomTLS_ContentLayer; var Fatal: Boolean; AlertCode: Integer);
begin
  if Assigned(FOnOutgoingAlert) then
    FOnOutgoingAlert(Sender,Client,Fatal,AlertCode)
  else if AlertCode <> 0 then
    raise EOutgoingTLSAlert.CreateAlert(AlertCode,Fatal);
end;

procedure TCustomTLSInternalServer.DoTLSRenegotiate(Sender: TObject;
  Client: TCustomTLS_ContentLayer; const SessionID: TSessionID;
  var Allow: Boolean);
begin
  if Assigned(FOnRenegotiate) then
    FOnRenegotiate(Sender,Client,SessionID,Allow);
end;

procedure TCustomTLSInternalServer.DoTLSSelectCompression(Sender: TObject;
  Client: TCustomTLS_ContentLayer;
  const CompMethods: array of TCompressionMethod;
  var CompMethod: TCompressionMethod);
begin
  if Assigned(FOnSelectCompression) then
    FOnSelectCompression(Sender,Client,CompMethods,CompMethod);
end;

function TCustomTLSInternalServer.FindTLSMasterSecret(
  const SessionID: TSessionID; var MasterSecret: TMasterSecret;
  var Index: Integer): Boolean;
begin
  Result := FStreamSecII.FindTLSMasterSecret(SessionID,MasterSecret,Index);
end;                 
{$ENDIF SHA1_AND_MD5}

procedure TCustomTLSInternalServer.ExportToPFX(AFileName: string;
  Password: ISecretKey; ExportCert: TASN1Struct; FriendlyName: WideString;
  ProviderName: WideString);
var
  RSAPrivateKey: TASN1Struct;
  PrivateKeyInfo: TASN1Struct;
  SafeBag: TASN1Struct;
  SafeContents: TASN1Struct;
  ContentInfo: TASN1Struct;
  AuthenticatedSafe: TASN1Struct;
  PDU: TASN1Struct;
  Priv: TMPIFPrivateKey;
  Cert: TASN1Struct;
  CACert: PASN1Struct;
  SelCert: PASN1Struct;
  Idx: Integer;
  CRLs: TASN1Struct;
  I: Integer;
begin
  SelCert := nil;
  Idx := 0;
  if Assigned(ExportCert) then
    SelCert := @ExportCert
  else if not FMyCerts.FindCert([md2WithRSAEncryption,
                                 {$IFNDEF DISABLEMD5SIGN}md5WithRSAEncryption,{$ENDIF DISABLEMD5SIGN}
                                 sha1WithRSAEncryption,
                                 sha256WithRSAEncryption,
                                 sha384WithRSAEncryption,
                                 sha512WithRSAEncryption],
                                [rsaEncryption],
                                [digitalSignature,keyEncipherment],
                                [id_kp_serverAuth],Idx,SelCert) then begin
    Idx := 0;
    if not FMyCerts.FindCert([md2WithRSAEncryption,
                              {$IFNDEF DISABLEMD5SIGN}md5WithRSAEncryption,{$ENDIF DISABLEMD5SIGN}
                              sha1WithRSAEncryption,
                              sha256WithRSAEncryption,
                              sha384WithRSAEncryption,
                              sha512WithRSAEncryption],
                             [rsaEncryption],
                             [digitalSignature,keyEncipherment],
                             [id_kp_clientAuth],Idx,SelCert) then
      raise Exception.Create('Unable to find a rsaEncryption server or client certificate');
  end;
  if Assigned(Password)
{$IFDEF GUI_APP}
     or PasswordMain.NewPasswordDlg(AFileName,Password,True)
{$ENDIF GUI_APP}
     then begin
    PDU := nil;
    AuthenticatedSafe := nil;
    ContentInfo := nil;
    SafeContents := nil;
    try
      NewComposeASN1Struct(AuthenticatedSafe,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      NewComposeASN1Struct(SafeContents,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      SafeBag := nil;
      try
        PrivateKeyInfo := nil;
        try
          RSAPrivateKey := nil;
          try
            Priv := StreamSecIIComp.FindCreatePrivateKey(SelCert^) as TMPIFPrivateKey;
            if not Priv.ExportToRSAPrivateKey(RSAPrivateKey,False) then
              if not (StreamSecIIComp.AdminLogin and
                      Priv.ExportToRSAPrivateKey(RSAPrivateKey,True)) then
                raise Exception.Create('Could not export private key');
            Pkcs12.ImposeRSAPrivateKey(RSAPrivateKey,
                                       ExtractKeyUsage(SelCert^),
                                       PrivateKeyInfo);
          finally
            RSAPrivateKey.Free;
          end;
          if ProviderName = '' then
            ProviderName := 'Microsoft RSA SChannel Cryptographic Provider';
          Pkcs12.ImposePrivateKeyInfo(PrivateKeyInfo,
                                      Password,
                                      OSToHex(ExtractSubjectKeyIdentifier(SelCert^)),
                                      #1#0#0#0,
                                      ProviderName,
                                      2000,pbe3DES_192,
                                      SafeBag);
        finally
          PrivateKeyInfo.Free;
        end;
        SafeContents.AddField('','',Safebag);
        SafeContents.CalculateLength;
        Pkcs12.ImposeSafeContents(SafeContents,Password,2000,pbeNone,ContentInfo);
        AuthenticatedSafe.AddField('','',ContentInfo);

        NewComposeASN1Struct(SafeContents,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
        if FriendlyName = '' then
          FriendlyName := 'StrSecII CertMgr ' + DateTimeToStr(Now);
        Pkcs12.ImposeCertificate(SelCert^,FriendlyName,#1#0#0#0,SafeBag);
        SafeContents.AddField('','',Safebag);
        Cert := SelCert^;                             
        CRLs := nil;
        try
          if MyCerts.FindCRLs(Cert,CRLs) > 0 then begin
            for I := 0 to CRLs.ItemCount - 1 do begin
              Pkcs12.ImposeCRL(CRLs.Items[I]^,SafeBag);
              SafeContents.AddField('','',Safebag);
            end;
          end;
          while MyCerts.FindCACert(Cert,CACert) do begin
            Pkcs12.ImposeCertificate(CACert^,'','',SafeBag);
            SafeContents.AddField('','',Safebag);
            Cert := CACert^;
            if (MyCerts.FindCRLs(Cert,CRLs) > 0) or
               (RootCerts.FindCRLs(Cert,CRLs) > 0) then begin
              for I := 0 to CRLs.ItemCount - 1 do begin
                Pkcs12.ImposeCRL(CRLs.Items[I]^,SafeBag);
                SafeContents.AddField('','',Safebag);
              end;
            end;
          end;
        finally
          CRLs.Free;
        end;
        SafeContents.CalculateLength;
        Pkcs12.ImposeSafeContents(SafeContents,Password,2000,pbeRC2_40,ContentInfo);
        AuthenticatedSafe.AddField('','',ContentInfo);
      finally
        SafeBag.Free;
        SafeContents.Free;
        ContentInfo.Free;
      end;
      AuthenticatedSafe.CalculateLength;
      Pkcs12.ImposeAuthSafeContent(AuthenticatedSafe,Password,2000,PDU);
      PDU.SaveToFile(AFileName,fmtDER);
    finally
      AuthenticatedSafe.Free;
      PDU.Free;
    end;
  end;
end;

function TCustomTLSInternalServer.FindCRLs(const CACert: TASN1Struct;
  var CRLs: TASN1Struct): Integer;
begin
  Result := FRootCerts.FindCRLs(CACert,CRLs);
  if Result <= 0 then begin
    Result := FMyCerts.FindCRLs(CACert,CRLs);
    if Result <= 0 then
      Result := FTrustedCerts.FindCRLs(CACert,CRLs);
  end;
end;

function TCustomTLSInternalServer.FindRevocation(const Cert: TASN1Struct;
  var RC: TX509RevokedCertificate): Boolean;
begin
  Result := True;
  if not FRootCerts.FindRevocation(Cert,RC) then
    if not FMyCerts.FindRevocation(Cert,RC) then
      Result := FTrustedCerts.FindRevocation(Cert,RC);
end;
                     
function TCustomTLSInternalServer.FindSMIMECapabilities(Cert: TASN1Struct;
  var Capabilities: PASN1Struct): Boolean;
begin
  Result := True;
  if not FTrustedCerts.FindSMIMECapabilities(Cert,Capabilities) then
    if not FMyCerts.FindSMIMECapabilities(Cert,Capabilities) then
      Result := FRootCerts.FindSMIMECapabilities(Cert,Capabilities);
end;

{$IFDEF SHA1_AND_MD5}
function TCustomTLSInternalServer.FindTLSMasterSecret(
  const SessionID: TSessionID; var Index: Integer): ISecretKey;
begin
  Result := FStreamSecII.FindTLSMasterSecret(SessionID,Index);
end;

function TCustomTLSInternalServer.FindTLSSession(const SessionID: TSessionID;
  var Client: TCustomTLS_ContentLayer): Integer;
begin
  Result := FStreamSecII.FindTLSSession(SessionID,Client);
end;

function TCustomTLSInternalServer.FindTLSSession(UserData: Pointer;
  var Client: TCustomTLS_ContentLayer): Integer;
begin
  Result := FStreamSecII.FindTLSSession(UserData,Client);
end;                 
{$ENDIF SHA1_AND_MD5}
               
{$IFDEF SHA1_AND_MD5}
procedure TCustomTLSInternalServer.FreeSocketList;
var
  SList: TThreadList;
  List: TList;
  Socket: ITLSSocket;
  I: Integer;
begin
  SList := FSocketList;
  FSocketList := nil;
  List := SList.LockList;
  try
    for I := List.Count - 1 downto 0 do begin
      Socket := ITLSSocket(List[I]);
      Socket.TLSServer := nil;
    end;
  finally
    SList.UnlockList;
    SList.Free;
  end;
end;
{$ENDIF SHA1_AND_MD5}

function TCustomTLSInternalServer.GetAcceptedPolicies: TStrings;
begin
  Result := FStreamSecII.CollectedCertificates.AcceptedPolicies;
end;

function TCustomTLSInternalServer.GetLeastKeyBitSize: Integer;
begin
  Result := FStreamSecII.CollectedCertificates.LeastKeyBitSize;
end;

function TCustomTLSInternalServer.GetMyCertList: TCertificateCollection;
begin
  Result := FMyCerts.CertList;
end;

function TCustomTLSInternalServer.GetMyCertsGroupIndex: Integer;
begin
  Result := FMyCerts.GroupIndex;
end;

function TCustomTLSInternalServer.GetMyCertsSCLFile: TResourceFile;
begin
  Result := FMyCertsSCLFile;
end;

function TCustomTLSInternalServer.GetMyCertsSource: TCustomX509TrustedCertificates;
begin
  Result := FMyCerts.CertSource;
end;

function TCustomTLSInternalServer.GetOffsetFromGMT: Double;
begin
  Result := FStreamSecII.HoursOffsetFromGMT;
end;
                          
{$IFDEF SHA1_AND_MD5}
function TCustomTLSInternalServer.GetOptions: TTLSOptions;
begin
  Result := FTLSOptions;
end;                 
{$ENDIF SHA1_AND_MD5}

function TCustomTLSInternalServer.GetPrivateKeyRing: TSsPrivateKeyRingComponent;
begin
  Result := FStreamSecII.PrivateKeyRing;
end;

function TCustomTLSInternalServer.GetPrivateKeyRingFile: TResourceFile;
begin
  Result := FStreamSecII.PrivateKeyRingFile;
end;

function TCustomTLSInternalServer.GetPrivateKeyRingFileName: TFileName;
begin
  Result := FStreamSecII.PrivateKeyRingFileName;
end;

function TCustomTLSInternalServer.GetRequiredPolicies: TStrings;
begin
  Result := FStreamSecII.CollectedCertificates.RequiredPolicies;
end;

function TCustomTLSInternalServer.GetRootCertList: TCertificateCollection;
begin
  Result := FRootCerts.CertList;
end;

function TCustomTLSInternalServer.GetRootCertsGroupIndex: Integer;
begin
  Result := FRootCerts.GroupIndex;
end;

function TCustomTLSInternalServer.GetRootCertsSCLFile: TResourceFile;
begin
  Result := FRootCertsSCLFile;
end;

function TCustomTLSInternalServer.GetRootCertsSource: TCustomX509TrustedCertificates;
begin
  Result := FRootCerts.CertSource;
end;

function TCustomTLSInternalServer.GetSessionKeyCount: Integer;
begin
  Result := FStreamSecII.SessionKeyCount;
end;

function TCustomTLSInternalServer.GetSessionKeyLifeSpan: {$IFDEF MSWINDOWS}TStreamSecIITime{$ELSE}TTime{$ENDIF};
begin
  Result := FStreamSecII.SessionKeyLifeSpan;
end;

function TCustomTLSInternalServer.GetSessionKeys(index: Integer): TASN1Struct;
begin
  Result := FStreamSecII.SessionKeys[index];
end;
                
{$IFDEF SHA1_AND_MD5}
function TCustomTLSInternalServer.GetTLSSessionCount: Integer;
begin
  Result := FStreamSecII.TLSSessionCount;
end;

function TCustomTLSInternalServer.GetTLSSessions(
  index: Integer): TCustomTLS_ContentLayer;
begin
  Result := FStreamSecII.TLSSessions[index];
end;                 
{$ENDIF SHA1_AND_MD5}

function TCustomTLSInternalServer.GetTrustedCertList: TCertificateCollection;
begin
  Result := FTrustedCerts.CertList;
end;

function TCustomTLSInternalServer.GetTrustedCertsGroupIndex: Integer;
begin
  Result := FTrustedCerts.GroupIndex;
end;

function TCustomTLSInternalServer.GetTrustedCertsSCLFile: TResourceFile;
begin
  Result := FTrustedCertsSCLFile;
end;

function TCustomTLSInternalServer.GetTrustedCertsSource: TCustomX509TrustedCertificates;
begin
  Result := FTrustedCerts.CertSource;
end;
                                         
{$IFDEF SHA1_AND_MD5}
procedure TCustomTLSInternalServer.HandleCipherSuitesChange(Sender: TObject);
begin
  Options.BeginUpdate;
  try
    if Options.KeyAgreementDH <> prNotAllowed then
      if not (pkaDH in PublicKeyAlgorithms) then
        Options.KeyAgreementDH := prNotAllowed;
    if Options.KeyAgreementECDH <> prNotAllowed then
      if not (pkaECDH in PublicKeyAlgorithms) then
        Options.KeyAgreementECDH := prNotAllowed;
    if Options.KeyAgreementRSA <> prNotAllowed then
      if not (pkaRSA in PublicKeyAlgorithms) then
        Options.KeyAgreementRSA := prNotAllowed;

    if Options.SignatureDSS <> prNotAllowed then
      if not (pkaDSA in PublicKeyAlgorithms) then
        Options.SignatureDSS := prNotAllowed;
    if Options.SignatureECDSA <> prNotAllowed then
      if not (pkaECDSA in PublicKeyAlgorithms) then
        Options.SignatureECDSA := prNotAllowed;
    if Options.SignatureRSA <> prNotAllowed then
      if not (pkaRSA in PublicKeyAlgorithms) then
        Options.SignatureRSA := prNotAllowed;
  finally
    Options.EndUpdate;
  end;
  FStreamSecII.TLSOptions := Options;
end;
{$ENDIF SHA1_AND_MD5}

function TCustomTLSInternalServer.HasPrivateCACert(
  var ACACert: TASN1Struct): Boolean;
var
  I: Integer;
  F: PASN1Struct;
begin
  I := 0;
  F := nil;
  repeat
    try
      Result := FMyCerts.FindCert([],[],[keyCertSign],I,F);
    except
      raise Exception.Create('HasPrivateCACert: Exception in RootCerts.FindCert');
    end;
    try
      if Result and Assigned(F) then begin
        if FStreamSecII.ValidateMyCert(F^) then
          Break;
      end;
    except
      on E: Exception do
        raise Exception.Create('HasPrivateCACert: Exception in StreamSecIIComp.ValidateMyCert'#13#10 +
                               E.Message);
    end;
  until not Result;
  if Result then begin
    Assert(Assigned(F));
    if not Assigned(ACACert) then
      ACACert := TASN1Struct.Create;
    ACACert.Assign(F^);
  end;
end;

function TCustomTLSInternalServer.HasPrivateSignCert(
  var ACert: TASN1Struct): Boolean;
var
  I: Integer;
  F: PASN1Struct;
begin
  I := 0;
  F := nil;
  repeat
    try
      Result := FMyCerts.FindCert([],[],[digitalSignature],I,F);
    except
      raise Exception.Create('HasPrivateSignCert: Exception in RootCerts.FindCert');
    end;
    try
      if Result and Assigned(F) then begin
        if FStreamSecII.ValidateMyCert(F^) then
          Break;
      end;
    except
      on E: Exception do
        raise Exception.Create('HasPrivateSignCert: Exception in StreamSecIIComp.ValidateMyCert'#13#10 +
                               E.Message);
    end;
  until not Result;
  if Result then begin
    Assert(Assigned(F));
    if not Assigned(ACert) then
      ACert := TASN1Struct.Create;
    ACert.Assign(F^);
  end;
end;

function TCustomTLSInternalServer.Hijacks(
  ASource: TCustomX509TrustedCertificates): Boolean;
begin
  { The internal components have individual sources, so this component will
    only hijack the internal components. }
  Result := (ASource = MyCerts) or (ASource = RootCerts);
end;

procedure TCustomTLSInternalServer.ImportFromPFX(AFileName: TFileName;
  Password: ISecretKey);
var
  RSAPrivateKey: TASN1Struct;
  CRL: TASN1Struct;
  Cert: TASN1Struct;
  PrivateKeyInfo: TASN1Struct;
  SafeContents, SafeContentsList: TASN1Struct;
  AuthenticatedSafe: TASN1Struct;
  PDU: TASN1Struct;
  OK, MoreToGo, CertAdded: Boolean;
  I, J, K: Integer;
  FriendlyName, LocalKeyId, CertLocalKeyId: string;
  Status: TCertStatusCode;
  CACert: PASN1Struct;
  Priv: TIFPrivateKey;
begin
  if Assigned(Password)
{$IFDEF GUI_APP}
     or PasswordMain.PasswordDlg(AFileName,Password,True)
{$ENDIF GUI_APP}
     then begin
    SafeContents := nil;
    AuthenticatedSafe := nil;
    PrivateKeyInfo := nil;
    RSAPrivateKey := nil;
    Cert := nil;
    CRL := nil;
    PDU := TASN1Struct.Create;
    try
      PDU.LoadFromFile(AFileName,fmtDER);
      OK := Pkcs12.ExtractAuthSafeContent(PDU,Password,AuthenticatedSafe);
      if not OK then
        raise Exception.Create(Format('Could not open %s. Either the file is damaged or not a valid PKCS#12 file or the password is incorrect.',[AFileName]))
      else begin
        SafeContentsList := nil;
        NewComposeASN1Struct(SafeContentsList,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
        try
          for I := 0 to AuthenticatedSafe.ItemCount - 1 do begin
            OK := Pkcs12.ExtractSafeContents(AuthenticatedSafe.Items[I]^,Password,SafeContents);
            if OK then
              for J := 0 to SafeContents.ItemCount - 1 do
                SafeContentsList.AddField('','ANY',SafeContents.Items[J]^);
          end;
          if SafeContentsList.ItemCount > 0 then begin
            for J := 0 to SafeContentsList.ItemCount - 1 do begin
              OK := Pkcs12.ExtractPrivateKeyInfo(SafeContentsList.Items[J]^,Password,PrivateKeyInfo,FriendlyName,LocalKeyId);
              if OK then begin
                for K := 0 to SafeContentsList.ItemCount - 1 do begin
                  OK := Pkcs12.ExtractCertificate(SafeContentsList.Items[K]^,Cert,CertLocalKeyId);
                  if OK then
                    if CertLocalKeyId = LocalKeyId then begin
                      OK := not StreamSecIIComp.ValidateMyCert(Cert);
                      if OK then
//                        FriendlyName := ExtractSubjectKeyIdentifier(Cert);
                        with TCertificate.Create(nil,nil) do try
                          AssignStruct(Cert);
                          FriendlyName := PublicKeyIdentifier;
                          if FriendlyName = '' then
                            FriendlyName := SubjectPublicKeyInfo.PublicKeyIdentifier;
                        finally
                          Free;
                        end;
                      Break;
                    end;
                  OK := True;
                end;
                if OK and (FriendlyName <> '') then begin
                  OK := Pkcs12.ExtractRSAPrivateKey(PrivateKeyInfo,RSAPrivateKey);
                  if OK then begin
                    FillChar(Priv,SizeOf(Priv),0);
                    try
                      OK := ASN1StructToRSAPrivateKey(RSAPrivateKey,Priv);
                      if OK then
                        if StreamSecIIComp.AddPrivateRSAKey(Priv,Pointer(FriendlyName),Length(FriendlyName),True) < 0 then
                          if StreamSecIIComp.AdminLogin then
                            if StreamSecIIComp.AddPrivateRSAKey(Priv,Pointer(FriendlyName),Length(FriendlyName),True) < 0 then
                              raise Exception.Create('The import failed');
                    finally
                      DisposeIFPrivateKey(Priv);
                    end;
                  end;
                end else if FriendlyName = '' then
                  raise Exception.Create('Import failure: No SubjectKeyIdentifier');
              end;
            end;
            // My certs:
            for J := 0 to SafeContentsList.ItemCount - 1 do begin
              OK := Pkcs12.ExtractCertificate(SafeContentsList.Items[J]^,Cert,LocalKeyId);
              if OK then
                try
                  OK := StreamSecIIComp.ValidateMyCert(Cert);
                except
                  OK := False;
                end;
                if OK then begin
                  if MyCerts.AddCertificate(Cert,True,Status,True) >= 0 then
                    FMyCertsLoaded := True;
                  FPrivateKeyRingLoaded := FPrivateKeyRingLoaded or FMyCertsLoaded;
                end;
            end;
            // Root certs:
            for J := 0 to SafeContentsList.ItemCount - 1 do begin
              OK := Pkcs12.ExtractCertificate(SafeContentsList.Items[J]^,Cert,LocalKeyId);
              if OK then
                if IsAllegedIssuer(Cert,Cert) then
                  RootCerts.AddCertificate(Cert,csDesigning in ComponentState,Status,True);
            end;
            // Other CA Certs:
            repeat
              CertAdded := False;
              MoreToGo := False;
              for J := 0 to SafeContentsList.ItemCount - 1 do begin
                OK := Pkcs12.ExtractCertificate(SafeContentsList.Items[J]^,Cert,LocalKeyId);
                if OK then
                  if RootCerts.IndexOfCert(Cert) < 0 then
                    if X509Base.ExtractPathLenConstraint(Cert) <> -1 then begin
                      if RootCerts.FindCACert(Cert,CACert) then begin
                        RootCerts.AddCertificate(Cert,False,Status,True);
                        CertAdded := CertAdded or (Status in [crcOK,crcTrusted]);
                      end else
                        MoreToGo := True;
                    end;
              end;
            until not (CertAdded and MoreToGo);
            // Other certs:
            for J := 0 to SafeContentsList.ItemCount - 1 do begin
              OK := Pkcs12.ExtractCertificate(SafeContentsList.Items[J]^,Cert,LocalKeyId);
              if OK then
                if (MyCerts.IndexOfCert(Cert) < 0) and
                   (RootCerts.IndexOfCert(Cert) < 0) then
                  StreamSecIIComp.CollectedCertificates.AddCertificate(Cert,False,Status,True);
            end;
            // CRLs:
            for J := 0 to SafeContentsList.ItemCount - 1 do begin
              OK := Pkcs12.ExtractCRL(SafeContentsList.Items[J]^,CRL);
              if OK then
                if MyCerts.AddCRL(CRL,Status) < 0 then
                  if RootCerts.AddCRL(CRL,Status) < 0 then
                    StreamSecIIComp.CollectedCertificates.AddCRL(CRL,Status);
            end;
          end;
        finally
          SafeContentsList.Free;
        end;
      end;
      ValidatePublicKeyPairs;
    finally
      PDU.Free;
      AuthenticatedSafe.Free;
      SafeContents.Free;
      PrivateKeyInfo.Free;
      Cert.Free;
      CRL.Free;
      RSAPrivateKey.Free;
    end;
  end;
end;

procedure TCustomTLSInternalServer.Loaded;
begin
  if not FLoaded then begin
    FLoaded := True; 
    inherited Loaded;
    InternalLoaded;
  end;
end;

procedure TCustomTLSInternalServer.LoadedResolveResourceFiles;
begin
  TStreamSecIIHack(FStreamSecII).Loaded;
  if not FMyCertsLoaded then begin
    SetMyCertsFileName(FMyCertsFileName);
    SetMyCertsSCLFile(FMyCertsSCLFile);
  end;              
  FStreamSecII.TrustedCertificates := FTrustedCerts;
end;

function TCustomTLSInternalServer.LoadMyCertsFromFile(AFileName: TFileName): Boolean;
var
  Ext: string;
  FS: TFileStream;
  Status: TCertStatusCodes;

  function AddCert(Strm: TStream): Boolean;
  var
    F: TASN1Struct;
    StatusCode: TCertStatusCode;
  begin
    F := TASN1Struct.Create;
    try
      F.LoadFromStream(FS,fmtDER);
      try
        Result := FStreamSecII.ValidateMyCert(F) and
                  (FMyCerts.AddCertificate(F,True,StatusCode) >= 0);
        FMyCertsLoaded := FMyCerts.Count > 0;
      except
        Result := False;
      end;
      if Result then begin
        Result := FStreamSecII.ValidateMyCert(F);
        FPrivateKeyRingLoaded := FPrivateKeyRingLoaded or Result;
      end;
    finally
      F.Free;
    end;
  end;

begin
  Ext := LowerCase(ExtractFileExt(AFileName));
  if (Ext = '.cer') or (Ext = '.crt') then begin
    FS := TFileStream.Create(AFileName,fmOpenRead);
    try
      Result := AddCert(FS);
    finally
      FS.Free;
    end;
  end else if (Ext = '.p7b') or (Ext = '.p7c') then begin
    Result := FMyCerts.ImportFromCMSFile(AFileName,False,Status);
    FMyCertsLoaded := FMyCerts.Count > 0;
    FPrivateKeyRingLoaded := FPrivateKeyRingLoaded or Result;
  end else begin
    FMyCerts.LoadFromFile(AFileName);
    FMyCertsLoaded := FMyCerts.Count > 0;
    Result := True;
  end;
  ValidatePublicKeyPairs;
end;

function TCustomTLSInternalServer.LoadMyCertsFromStream(AStream: TStream): Boolean;
var
  OldPos: Integer;
  F: TASN1Struct;
  Status: TCertStatusCodes;
  StatusCode: TCertStatusCode;
begin
  OldPos := AStream.Position;
  try
    FMyCerts.LoadFromStream(AStream);
  except
    FMyCerts.Clear;
    F := TASN1Struct.Create;
    try
      F._AddRef;
      AStream.Position := OldPos;
      F.LoadFromStream(AStream,fmtDER);
      if not FMyCerts.ImportFromCMS(F,False,Status) then begin
        FMyCerts.Clear;
        if FStreamSecII.ValidateMyCert(F) and
           (FMyCerts.AddCertificate(F,True,StatusCode) < 0) then
          raise Exception.Create('TTLSInternalServer.LoadRootCertsFromStream: Failure');
      end;
    finally
      F._Release;
    end;
  end;
  Result := True;
  FMyCertsLoaded := FMyCerts.Count > 0;
  ValidatePublicKeyPairs;
end;

procedure TCustomTLSInternalServer.LoadP7BRootCertsFromStream(AStream: TStream);
var
  Status: TCertStatusCodes;
begin
  FRootCerts.ImportFromCMSStream(AStream,True,Status);
end;

procedure TCustomTLSInternalServer.LoadPrivateKeyRingFromFile(
  AFileName: TFileName; Password: ISecretKey);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName,fmOpenRead);
  try
    LoadPrivateKeyRingFromStream(FS,Password);
  finally
    FS.Free;
  end;
end;

procedure TCustomTLSInternalServer.LoadPrivateKeyRingFromStream(AStream: TStream;
  Password: ISecretKey);
begin
  try
    if not FStreamSecII.LoadPrivateKeyRingFromStream(AStream,Password) then
      raise Exception.Create('Unable to load private key ring');
    FPrivateKeyRingLoaded := True;
  finally
    ValidatePublicKeyPairs;
  end;
end;

procedure TCustomTLSInternalServer.LoadRootCertsFromFile(AFileName: TFileName);
var
  Ext: string;
  FS: TFileStream;
  F: TASN1Struct;
  StatusCode: TCertStatusCode;
  Status: TCertStatusCodes;
begin
  Ext := LowerCase(ExtractFileExt(AFileName));
  if (Ext = '.cer') or (Ext = '.crt') then begin
    FS := TFileStream.Create(AFileName,fmOpenRead);
    try
      F := TASN1Struct.Create;
      try
        F.LoadFromStream(FS,fmtDER);
        FRootCerts.AddCertificate(F,True,StatusCode);
      finally
        F.Free;
      end;
    finally
      FS.Free;
    end;
  end else if (Ext = '.p7b') or (Ext = '.p7c') then
    FRootCerts.ImportFromCMSFile(AFileName,True,Status)
  else
    FRootCerts.LoadFromFile(AFileName);
end;

procedure TCustomTLSInternalServer.LoadRootCertsFromStream(AStream: TStream);
var
  OldPos: Integer;
  F: TASN1Struct;
  Status: TCertStatusCodes;
  StatusCode: TCertStatusCode;
begin
  OldPos := AStream.Position;
  try
    FRootCerts.LoadFromStream(AStream);
  except
    FRootCerts.Clear;
    AStream.Position := OldPos;
    if not FRootCerts.ImportFromCMSStream(AStream,True,Status) then begin
      FRootCerts.Clear;
      F := TASN1Struct.Create;
      try
        AStream.Position := OldPos;
        F.LoadFromStream(AStream,fmtDER);
        if FRootCerts.AddCertificate(F,True,StatusCode) < 0 then
          raise Exception.Create('TTLSInternalServer.LoadRootCertsFromStream: Failure');
      finally
        F.Free;
      end;
    end;
  end;
end;

procedure TCustomTLSInternalServer.LoadTrustedCertsFromFile(
  AFileName: TFileName);
var
  Ext: string;
  FS: TFileStream;
  F: TASN1Struct;
  StatusCode: TCertStatusCode;
  Status: TCertStatusCodes;
begin
  Ext := LowerCase(ExtractFileExt(AFileName));
  if (Ext = '.cer') or (Ext = '.crt') then begin
    FS := TFileStream.Create(AFileName,fmOpenRead);
    try
      F := TASN1Struct.Create;
      try
        F.LoadFromStream(FS,fmtDER);
        FTrustedCerts.AddCertificate(F,True,StatusCode);
      finally
        F.Free;
      end;
    finally
      FS.Free;
    end;
  end else if (Ext = '.p7b') or (Ext = '.p7c') then begin
    FTrustedCerts.ImportFromCMSFile(AFileName,True,Status);
    FStreamSecII.TrustedCertificates := FTrustedCerts;
  end else begin
    FTrustedCerts.LoadFromFile(AFileName);
    FStreamSecII.TrustedCertificates := FTrustedCerts;
  end;
end;

procedure TCustomTLSInternalServer.LoadTrustedCertsFromStream(
  AStream: TStream);
var
  OldPos: Integer;
  F: TASN1Struct;
  Status: TCertStatusCodes;
  StatusCode: TCertStatusCode;
begin
  OldPos := AStream.Position;
  try
    FTrustedCerts.LoadFromStream(AStream);
  except 
    FTrustedCerts.Clear;
    AStream.Position := OldPos;
    if not FTrustedCerts.ImportFromCMSStream(AStream,True,Status) then begin
      FTrustedCerts.Clear;
      F := TASN1Struct.Create;
      try
        AStream.Position := OldPos;
        F.LoadFromStream(AStream,fmtDER);
        if FTrustedCerts.AddCertificate(F,True,StatusCode) < 0 then
          raise Exception.Create('TTLSInternalServer.LoadTrustedCertsFromStream: Failure');
      finally
        F.Free;
      end;
    end;
  end;
  FStreamSecII.TrustedCertificates := FTrustedCerts;
end;

procedure TCustomTLSInternalServer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FMyCertsSCLFile then
      FMyCertsSCLFile := nil;
    if AComponent = FRootCertsSCLFile then
      FRootCertsSCLFile := nil;
    if AComponent = FTrustedCertsSCLFile then
      FTrustedCertsSCLFile := nil;
    if AComponent = FStreamSecII then
      FStreamSecII := nil;
    if Assigned(FStreamSecII) then begin
      if AComponent = FStreamSecII.PrivateKeyRingFile then
        FStreamSecII.PrivateKeyRingFile := nil;
      if AComponent = FStreamSecII.PrivateKeyRing then
        FStreamSecII.PrivateKeyRing := nil;
    end;
  end;
end;

function TCustomTLSInternalServer.RevokeCertificate(
  const Cert: TASN1Struct; Reason: CRLReason; HoldInstruction: string;
  InvalidityDate, NextCRLUpdate: TDateTime; PrivKey: IMPPrivateKey;
  CACert: PASN1Struct): Boolean;
begin
  Result := Assigned(CACert);
  if not Result then
    Result := RootCerts.FindCACert(Cert,CACert);
  if Result then begin
    if PrivKey = nil then
      PrivKey := StreamSecIIComp.FindCreatePrivateKey(Cert);
    Result := Assigned(PrivKey);
    if Result then
      Result := RootCerts.RevokeCertificate(Cert,
                                            Reason,
                                            HoldInstruction,
                                            InvalidityDate,
                                            NextCRLUpdate,
                                            PrivKey,
                                            CACert);
    if Result then begin
    end;
  end;
end;

procedure TCustomTLSInternalServer.SetAcceptedPolicies(const Value: TStrings);
begin
  FStreamSecII.CollectedCertificates.AcceptedPolicies.Assign(Value);
end;

procedure TCustomTLSInternalServer.SetClientOrServer(
  const Value: TClientOrServer);
begin
  FClientOrServer := Value;
  ValidatePublicKeyPairs;
end;

procedure TCustomTLSInternalServer.SetExternalIFPrivateKeyClass(
  const Value: TExternalIFPrivateKeyClass);
begin
  FExternalIFPrivateKeyClass := Value;
end;

procedure TCustomTLSInternalServer.SetLeastKeyBitSize(const Value: Integer);
begin
  FMyCerts.LeastKeyBitSize := Value;
  FRootCerts.LeastKeyBitSize := Value;
  FTrustedCerts.LeastKeyBitSize := Value;
  FStreamSecII.CollectedCertificates.LeastKeyBitSize := Value;
end;

procedure TCustomTLSInternalServer.SetMyCertList(
  const Value: TCertificateCollection);
begin
  FMyCerts.CertList.Assign(Value);
end;

procedure TCustomTLSInternalServer.SetMyCertsFileName(const Value: TFileName);
begin
  try
    FMyCertsFileName := Value;
    if Value <> '' then begin
      LoadMyCertsFromFile(Value);
      SetMyCertsSCLFile(nil);
    end;
  except
    FMyCertsFileName := '';
  end;
end;

procedure TCustomTLSInternalServer.SetMyCertsGroupIndex(const Value: Integer);
begin
  FMyCerts.GroupIndex := Value;
end;

procedure TCustomTLSInternalServer.SetMyCertsSCLFile(const Value: TResourceFile);
var
  Loading: Boolean;
begin
  try
    if Assigned(FMyCertsSCLFile) then begin
      FMyCertsSCLFile.RemoveLoadNotification(Self,DoLoadMyCertsSCLFile);
    end;
    FMyCertsSCLFile := Value;
    if Assigned(Value) then begin
      FreeNotification(Value);
      Value.FreeNotification(Self);
      Value.AddLoadNotification(Self,DoLoadMyCertsSCLFile);
      Loading := csLoading in ComponentState;
      if Assigned(Owner) and not Loading then
        Loading := csLoading in Owner.ComponentState;
      if (not Loading) and
         Assigned(Value.DataStream) and (Value.DataStream.Size > 0) then begin
        LoadMyCertsFromStream(Value.DataStream);
        FMyCertsFileName := '';
        MyCertsSource := nil;
      end;
    end;
  except
    FMyCertsSCLFile := nil;
  end;
end;

procedure TCustomTLSInternalServer.SetMyCertsSource(
  const Value: TCustomX509TrustedCertificates);
begin
  if Value <> Self then begin
    FMyCerts.CertSource := Value;
    FMyCertsLoaded := FMyCerts.Count > 0;
    ValidatePublicKeyPairs;
  end;
end;

procedure TCustomTLSInternalServer.SetOffsetFromGMT(const Value: Double);
begin
  FStreamSecII.HoursOffsetFromGMT := Value;
  FStreamSecII.CollectedCertificates.HoursOffsetFromGMT := Value;
  FRootCerts.HoursOffsetFromGMT := Value;
  FMyCerts.HoursOffsetFromGMT := Value;
end;

procedure TCustomTLSInternalServer.SetOnCertNotAccepted(
  const Value: TCertNotAcceptedEvent);
begin
  FOnCertNotAccepted := Value;
end;

procedure TCustomTLSInternalServer.SetOnCertNotTrusted(
  const Value: TCertNotTrustedEvent);
begin
  FOnCertNotTrusted := Value;
end;

procedure TCustomTLSInternalServer.SetOnCertPolicy(const Value: TPolicyEvent);
begin
  FOnCertPolicy := Value;
end;
                 
{$IFDEF SHA1_AND_MD5}
procedure TCustomTLSInternalServer.SetOnCompress(const Value: TTLSCompressEvent);
begin
  FOnCompress := Value;
end;

procedure TCustomTLSInternalServer.SetOnDecompress(
  const Value: TTLSDecompressEvent);
begin
  FOnDecompress := Value;
end;

procedure TCustomTLSInternalServer.SetOnIncomingAlert(
  const Value: TTLSAlertEvent);
begin
  FOnIncomingAlert := Value;
end;

procedure TCustomTLSInternalServer.SetOnOutgoingAlert(
  const Value: TTLSAlertEvent);
begin
  FOnOutgoingAlert := Value;
end;                 
{$ENDIF SHA1_AND_MD5}

procedure TCustomTLSInternalServer.SetOnPassword(const Value: TPasswordEvent);
begin
  FOnPassword := Value;
  FStreamSecII.OnPassword := Value;
end;
            
{$IFDEF SHA1_AND_MD5}
procedure TCustomTLSInternalServer.SetOnRenegotiate(
  const Value: TTLSRenegotiateEvent);
begin
  FOnRenegotiate := Value;
end;

procedure TCustomTLSInternalServer.SetOnSelectCompression(
  const Value: TTLSSelectCompressionEvent);
begin
  FOnSelectCompression := Value;
end;

procedure TCustomTLSInternalServer.SetOnTLSChangeCipherSpec(
  const Value: TTLSChangeCipherSpec);
begin
  FOnTLSChangeCipherSpec := Value;
end;

procedure TCustomTLSInternalServer.SetOptions(const Value: TTLSOptions);
begin
  FTLSOptions.Assign(Value);
end;                 
{$ENDIF SHA1_AND_MD5}

procedure TCustomTLSInternalServer.SetPrivateKeyRing(
  const Value: TSsPrivateKeyRingComponent);
begin
  FStreamSecII.PrivateKeyRing := Value;
  FPrivateKeyRingLoaded := Assigned(FStreamSecII.PrivateKeyRing) or
                           Assigned(FStreamSecII.PrivateKeyRingFile) or
                           (FStreamSecII.PrivateKeyRingFileName <> '');
  if not (csDestroying in ComponentState) then
    ValidatePublicKeyPairs;
end;

procedure TCustomTLSInternalServer.SetPrivateKeyRingFile(
  const Value: TResourceFile);
begin
  FStreamSecII.PrivateKeyRingFile := Value;
  FPrivateKeyRingLoaded := Assigned(FStreamSecII.PrivateKeyRing) or
                           Assigned(FStreamSecII.PrivateKeyRingFile) or
                           (FStreamSecII.PrivateKeyRingFileName <> '');
  if not (csDestroying in ComponentState) then
    ValidatePublicKeyPairs;
end;

procedure TCustomTLSInternalServer.SetPrivateKeyRingFileName(
  const Value: TFileName);
begin
  FStreamSecII.PrivateKeyRingFileName := Value;
  FPrivateKeyRingLoaded := Assigned(FStreamSecII.PrivateKeyRing) or
                           Assigned(FStreamSecII.PrivateKeyRingFile) or
                           (FStreamSecII.PrivateKeyRingFileName <> '');
  if not (csDestroying in ComponentState) then
    ValidatePublicKeyPairs;
end;

procedure TCustomTLSInternalServer.SetPublicKeyAlgorithms(
  const Value: TPublicKeyAlgs);
begin
  // Read only;
end;

procedure TCustomTLSInternalServer.SetRequiredPolicies(const Value: TStrings);
begin
  FStreamSecII.CollectedCertificates.RequiredPolicies.Assign(Value);
end;

procedure TCustomTLSInternalServer.SetRootCertList(
  const Value: TCertificateCollection);
begin
  FRootCerts.CertList.Assign(Value);
end;

procedure TCustomTLSInternalServer.SetRootCertsFileName(const Value: TFileName);
begin
  try
    FRootCertsFileName := Value;
    if Value <> '' then begin
      LoadRootCertsFromFile(Value);
      SetRootCertsSCLFile(nil);
    end;
  except
    FRootCertsFileName := '';
  end;
end;

procedure TCustomTLSInternalServer.SetRootCertsGroupIndex(const Value: Integer);
begin
  FRootCerts.GroupIndex := Value;
end;

procedure TCustomTLSInternalServer.SetRootCertsSCLFile(const Value: TResourceFile);
begin
  try
    if Assigned(FRootCertsSCLFile) then begin
      FRootCertsSCLFile.RemoveLoadNotification(Self,DoLoadRootCertsSCLFile);
    end;
    FRootCertsSCLFile := Value;
    if Assigned(Value) then begin
      FreeNotification(Value);
      Value.FreeNotification(Self);
      Value.AddLoadNotification(Self,DoLoadRootCertsSCLFile);
      if Assigned(Value.DataStream) then begin
        LoadRootCertsFromStream(Value.DataStream);
        FRootCertsFileName := '';
        RootCertsSource := nil;
      end;
    end;
  except
    FRootCertsSCLFile := nil;
  end;
end;

procedure TCustomTLSInternalServer.SetRootCertsSource(
  const Value: TCustomX509TrustedCertificates);
begin
  if Value <> Self then begin
    FRootCerts.CertSource := Value;
    ValidatePublicKeyPairs;
  end;
end;

procedure TCustomTLSInternalServer.SetSessionKeyLifeSpan(
  const Value: {$IFDEF MSWINDOWS}TStreamSecIITime{$ELSE}TTime{$ENDIF});
begin
  FStreamSecII.SessionKeyLifeSpan := Value;
end;

procedure TCustomTLSInternalServer.SetSessionKeys(index: Integer;
  const Value: TASN1Struct);
begin
  FStreamSecII.SessionKeys[index] := Value;
end;

procedure TCustomTLSInternalServer.SetTrustedCertList(
  const Value: TCertificateCollection);
begin
  FTrustedCerts.CertList := Value;
end;

procedure TCustomTLSInternalServer.SetTrustedCertsFileName(
  const Value: TFileName);
begin
  try
    FTrustedCertsFileName := Value;
    if Value <> '' then begin
      LoadTrustedCertsFromFile(Value);
      SetTrustedCertsSCLFile(nil);
    end;
  except
    FTrustedCertsFileName := '';
  end;
end;

procedure TCustomTLSInternalServer.SetTrustedCertsGroupIndex(
  const Value: Integer);
begin
  FTrustedCerts.GroupIndex := Value;
end;

procedure TCustomTLSInternalServer.SetTrustedCertsSCLFile(
  const Value: TResourceFile);
begin
  try
    if Assigned(FTrustedCertsSCLFile) then begin
      FTrustedCertsSCLFile.RemoveLoadNotification(Self,DoLoadTrustedCertsSCLFile);
    end;
    FTrustedCertsSCLFile := Value;
    if Assigned(Value) then begin
      FreeNotification(Value);
      Value.FreeNotification(Self);
      Value.AddLoadNotification(Self,DoLoadTrustedCertsSCLFile);
      if Assigned(Value.DataStream) then begin
        LoadTrustedCertsFromStream(Value.DataStream);
        FTrustedCertsFileName := '';
        TrustedCertsSource := nil;
      end;
    end;
  except
    FTrustedCertsSCLFile := nil;
  end;
end;

procedure TCustomTLSInternalServer.SetTrustedCertsSource(
  const Value: TCustomX509TrustedCertificates);
begin
  if Value <> Self then
    FTrustedCerts.CertSource := Value;
end;
     
{$IFDEF SHA1_AND_MD5}
procedure TCustomTLSInternalServer.StartEphemeralKeyGen(const RSABitSizes,
  DHBitSizes, DHQBitSizes: array of Integer; RunOnce: Boolean);
begin
  FStreamSecII.StartEphemeralKeyGen(RSABitSizes,DHBitSizes,DHQBitSizes,RunOnce);
end;

procedure TCustomTLSInternalServer.StopEphemeralKeyGen;
begin
  FStreamSecII.StopEphemeralKeyGen;
end;

function TCustomTLSInternalServer.TLSAccept(UserData: Pointer; Src,
  Response: TStream; ErrorCode: PInteger): Integer;
begin
  Result := FStreamSecII.TLSAccept(UserData,Src,Response,ErrorCode);
end;

function TCustomTLSInternalServer.TLSAddClientSession: TCustomTLS_ContentLayer;
begin
  FStreamSecII.TLSOptions.Assign(FTLSOptions);
  Result := FStreamSecII.TLSAddClientSession;
end;

function TCustomTLSInternalServer.TLSAddServerSession: TCustomTLS_ContentLayer;
begin
  Result := FStreamSecII.TLSAddServerSession;
end;

function TCustomTLSInternalServer.TLSClose(const SessionID: TSessionID;
  Response: TStream; ErrorCode: PInteger): Integer;
begin
  Result := FStreamSecII.TLSClose(SessionID,Response,ErrorCode);
end;

function TCustomTLSInternalServer.TLSClose(UserData: Pointer;
  Response: TStream; ErrorCode: PInteger): Integer;
begin
  Result := FStreamSecII.TLSClose(UserData,Response,ErrorCode);
end;

function TCustomTLSInternalServer.TLSConnect(UserData: Pointer;
  Response: TStream; ErrorCode: PInteger): Integer;
begin
  FStreamSecII.TLSOptions.Assign(FTLSOptions);
  Result := FStreamSecII.TLSConnect(UserData,Response,ErrorCode);
end;

function TCustomTLSInternalServer.TLSConnect(const SessionID: TSessionID;
  Response: TStream; ErrorCode: PInteger): Integer;
begin
  Result := FStreamSecII.TLSConnect(SessionID,Response,ErrorCode);
end;

function TCustomTLSInternalServer.TLSConnect(UserData: Pointer;
  const SessionID: TSessionID; Response: TStream; ErrorCode: PInteger): Integer;
begin
  Result := FStreamSecII.TLSConnect(UserData,SessionID,Response,ErrorCode);
end;

function TCustomTLSInternalServer.TLSConnect(UserData: Pointer;
  const SessionID: TSessionID; const CipherSuites: TCipherSuites;
  Response: TStream; ErrorCode: PInteger): Integer;
begin
  Result := FStreamSecII.TLSConnect(UserData,SessionID,CipherSuites,Response,ErrorCode);
end;

function TCustomTLSInternalServer.TLSConnectAsServer(UserData: Pointer;
  Response: TStream; ErrorCode: PInteger): Integer;
begin
  Result := FStreamSecII.TLSConnectAsServer(UserData,Response,ErrorCode);
end;

function TCustomTLSInternalServer.TLSDecodeData(UserData: Pointer; Src, Data,
  Response: TStream; ErrorCode: PInteger): Integer;
begin
  Result := FStreamSecII.TLSDecodeData(UserData,Src,Data,Response,ErrorCode);
end;

function TCustomTLSInternalServer.TLSDecodeData(var SessionID: TSessionID; Src,
  Data, Response: TStream; ErrorCode: PInteger): Integer;
begin
  Result := FStreamSecII.TLSDecodeData(SessionID,Src,Data,Response,ErrorCode);
end;

function TCustomTLSInternalServer.TLSEncodeData(UserData: Pointer; Data,
  Response: TStream; ErrorCode: PInteger): Integer;
begin
  Result := FStreamSecII.TLSEncodeData(UserData,Data,Response,ErrorCode);
end;

function TCustomTLSInternalServer.TLSEncodeData(const SessionID: TSessionID;
  Data, Response: TStream; ErrorCode: PInteger): Integer;
begin
  Result := FStreamSecII.TLSEncodeData(SessionID,Data,Response,ErrorCode);
end;

procedure TCustomTLSInternalServer.TLSSetupServer;
begin
  FTLSSetupServerWhenLoaded := not (csDesigning in ComponentState);
  FStreamSecII.StopEphemeralKeyGen;
  FStreamSecII.TLSOptions.Assign(FTLSOptions);
  FStreamSecII.TLSSetupServer;
  if Options.KeyAgreementDHE <> prNotAllowed then
    FStreamSecII.StartEphemeralKeyGen([],
                                      [Options.EphemeralDHKeySize],
                                      [Options.EphemeralDHQSize],
                                      False);
  if (Options.KeyAgreementRSA <> prNotAllowed) and
     (Options.EphemeralRSAKeySize > 0) and
     ((Options.Export40Bit <> prNotAllowed) or (Options.Export56Bit <> prNotAllowed)) then
    FStreamSecII.StartEphemeralKeyGen([Options.EphemeralRSAKeySize],
                                      [],
                                      [],
                                      False);
end;
{$ENDIF SHA1_AND_MD5}

procedure TCustomTLSInternalServer.UpdateHijacker(
  AHijacker: TCustomX509TrustedCertificates);
var
  GroupIdx: Integer;

  procedure AddCerts(ASource: TX509TrustedCertificates);
  var
    Idx: Integer;
    Cert: TASN1Struct;
    Status: TCertStatusCode;
  begin
    for Idx := 0 to ASource.CertList.Count - 1 do begin
      Cert := TCertificateItem(ASource.CertList.Items[Idx]).Cert;
      if Cert = nil then Continue;
      if TCertificateItem(ASource.CertList.Items[Idx]).GroupIndex = GroupIdx then
        AHijacker.AddCertificate(Cert,True,Status);
    end;
  end;

  procedure AddCRLs(ASource: TX509TrustedCertificates); 
  var
    Idx: Integer;
    Status: TCertStatusCode;
  begin                 
    for Idx := 0 to ASource.CRLCount - 1 do
      AHijacker.AddCRL(ASource.CRLs[Idx],Status);
  end;

begin
  if Assigned(AHijacker) and
     (TCustomX509TrustedCertificatesHack(AHijacker).CertSource = Self) and
     (TCustomX509TrustedCertificatesHack(AHijacker).GroupIndex >= 0) then begin
    GroupIdx := ClearHijacker(AHijacker);
    AddCerts(MyCerts);
    AddCerts(RootCerts);
    AddCRLs(MyCerts);
    AddCRLs(RootCerts);
    TCustomX509TrustedCertificatesHack(AHijacker).CertListLoaded;
    TCustomX509TrustedCertificatesHack(AHijacker).UpdateHijackers;
  end;
end;

function TCustomTLSInternalServer.ValidateMyCert(
  const Cert: TASN1Struct): Boolean;
begin
  Result := FStreamSecII.ValidateMyCert(Cert);
end;

procedure TCustomTLSInternalServer.ValidatePublicKeyPairs;
{$IFDEF SHA1_AND_MD5}
var
  Idx: Integer;
  C: PASN1Struct;
  ExtKeyUsage: ObjectIdentifier;
{$ENDIF SHA1_AND_MD5}
begin
{$IFDEF SHA1_AND_MD5}
  if FClientOrServer = cosClientSide then begin
    FPublicKeyAlgorithms := [Low(TPublicKeyAlg)..High(TPublicKeyAlg)];
    Exclude(FPublicKeyAlgorithms,pkaOther);
    HandleCipherSuitesChange(nil);
  end else if FPrivateKeyRingLoaded and
              FMyCertsLoaded and
              (FClientOrServer = cosServerSide) then begin
    ExtKeyUsage := id_kp_serverAuth;
    Idx := 0;
    FPublicKeyAlgorithms := [];
    while FMyCerts.FindCert([md2WithRSAEncryption,
                             {$IFNDEF DISABLEMD5SIGN}md5WithRSAEncryption,{$ENDIF DISABLEMD5SIGN}
                             sha1WithRSAEncryption,
                             sha256WithRSAEncryption,
                             sha384WithRSAEncryption,
                             sha512WithRSAEncryption],
                            [rsaEncryption],
                            [digitalSignature,keyEncipherment],
                            [ExtKeyUsage],Idx,C) do
      if FStreamSecII.FindCreatePrivateKeyTLS(C^) <> nil then begin
        Include(FPublicKeyAlgorithms,pkaRSA);
        Break;
      end;
    Idx := 0;
    while FMyCerts.FindCert([],
                            [id_dsa],
                            [digitalSignature],
                            [ExtKeyUsage],Idx,C) do
      if FStreamSecII.FindCreatePrivateKeyTLS(C^) <> nil then begin
        Include(FPublicKeyAlgorithms,pkaDSA);
        Break;
      end;
    Idx := 0;
    while FMyCerts.FindCert([],
                            [id_ecPublicKey],
                            [digitalSignature],
                            [ExtKeyUsage],Idx,C) do
      if FStreamSecII.FindCreatePrivateKeyTLS(C^) <> nil then begin
        Include(FPublicKeyAlgorithms,pkaECDSA);
        Break;
      end;
    Idx := 0;
    while FMyCerts.FindCert([],
                            [dhPublicNumber],
                            [keyAgreement],
                            [ExtKeyUsage],Idx,C) do
      if FStreamSecII.FindCreatePrivateKeyTLS(C^) <> nil then begin
        Include(FPublicKeyAlgorithms,pkaDH);
        Break;
      end;
    Idx := 0;
    while FMyCerts.FindCert([],
                            [id_ecPublicKey],
                            [keyAgreement],
                            [ExtKeyUsage],Idx,C) do
      if FStreamSecII.FindCreatePrivateKeyTLS(C^) <> nil then begin
        Include(FPublicKeyAlgorithms,pkaECDH);
        Break;
      end;
    HandleCipherSuitesChange(nil);
  end else
    FPublicKeyAlgorithms := [];
{$ENDIF SHA1_AND_MD5}
end;

function TCustomTLSInternalServer.ValidateSCLSignatureCerts: Boolean;
var
  ErrMsg: string;
begin
  ErrMsg := '';
  if MyCerts.SignatureCert = nil then
    ErrMsg := 'TCustomTLSInternalServer.ValidateSCLSignatureCerts: No Signature Certificate for MyCerts'
  else begin
    if not ValidateMyCert(MyCerts.SignatureCert) then
      ErrMsg := 'TCustomTLSInternalServer.ValidateSCLSignatureCerts: Invalid Signature Certificate for MyCerts';
  end;
  if RootCerts.SignatureCert = nil then
    ErrMsg := 'TCustomTLSInternalServer.ValidateSCLSignatureCerts: No Signature Certificate for RootCerts'
  else begin
    if not ValidateMyCert(RootCerts.SignatureCert) then
      ErrMsg := 'TCustomTLSInternalServer.ValidateSCLSignatureCerts: Invalid Signature Certificate for RootCerts';
  end;
  Result := ErrMsg = '';
  if not Result then
    raise Exception.Create(ErrMsg);
end;

function TCustomTLSInternalServer.GetIsGlobalServer: Boolean;
begin
  Result := Self = GlobalServer;
end;

procedure TCustomTLSInternalServer.SetIsGlobalServer(const Value: Boolean);
begin
  if Value then
    GlobalServer := Self
  else if (Self = GlobalServer) and not (csLoading in ComponentState) then
    GlobalServer := nil;
end;

procedure TCustomTLSInternalServer.SavePrivateKeyRing(AFileName: TFileName);
var
  Password: ISecretKey;
begin
  if ExtractFileExt(AFileName) = '' then
    AFileName := AFileName + '.pkr';
  Password := TSecretKey.Create('');
  StreamSecIIComp.DoPassword(Password);
  if not StreamSecIIComp.SavePrivateKeyRingToFile(AFileName,
                                                  Password,
                                                  kdfWPv2SHA1,
                                                  1 shl KeyDerivationBits,
                                                  True,
                                                  id_aes256_wrap,
                                                  TRijndael_ABC) then
    raise Exception.Create(Format('Unable to save private key ring to file %s',[AFileName]));
end;

procedure TCustomTLSInternalServer.SetKeyDerivationBits(
  const Value: Integer);
begin
  FKeyDerivationBits := Value;
end;
                               
{$IFDEF SHA1_AND_MD5}
function TCustomTLSInternalServer.RenewEphemeralDHKey(DHBitSize,
  DHQBitSize: Integer): Boolean;
begin
  Result := StreamSecIIComp.RenewEphemeralDHKey(DHBitSize,DHQBitSize);
end;                 
{$ENDIF SHA1_AND_MD5}

procedure TCustomTLSInternalServer.SavePrivateKeyRingToStream(
  AStream: TStream);
var
  Password: ISecretKey;
begin
  Password := TSecretKey.Create('');
  StreamSecIIComp.DoPassword(Password);
  StreamSecIIComp.SavePrivateKeyRingToStream(AStream,
                                             Password,
                                             kdfWPv2SHA1,
                                             1 shl KeyDerivationBits,
                                             True,
                                             id_aes256_wrap,
                                             TRijndael_ABC);
end;

procedure TCustomTLSInternalServer.SetName(const NewName: TComponentName);
begin
  inherited;
  FStreamSecII.Name := Name + 'StreamSecII';
  FMyCerts.Name := Name + 'MyCerts';
  FRootCerts.Name := Name + 'RootCerts';
  FTrustedCerts.Name := Name + 'TrustedCerts';
end;

function TCustomTLSInternalServer.GetCertificateCollectionCount: Integer;
begin
  Result := 3;
end;

function TCustomTLSInternalServer.GetCertificateCollections(
  Index: Integer): TCertificateCollection;
begin
  case Index of
    0: Result := GetMyCertList;
    1: Result := GetRootCertList;
    2: Result := GetTrustedCertList;
  else
    Result := nil;
  end;
end;

procedure TCustomTLSInternalServer.SetCertificateCollections(
  Index: Integer; const Value: TCertificateCollection);
begin
  case Index of
    0: GetMyCertList.Assign(Value);
    1: GetRootCertList.Assign(Value);
    2: GetTrustedCertList.Assign(Value);
  end;
end;

procedure TCustomTLSInternalServer.SetBeforeImportTLSCert(
  const Value: TBeforeImportTLSCertEvent);
begin
  FStreamSecII.CollectedCertificates.BeforeImportTLSCert := Value;
end;

function TCustomTLSInternalServer.GetBeforeImportTLSCert: TBeforeImportTLSCertEvent;
begin
  Result := FStreamSecII.CollectedCertificates.BeforeImportTLSCert;
end;

procedure TCustomTLSInternalServer.SetRegisteredPathName(
  const Value: string);
begin
  if Value <> FRegisteredPathName then begin
    FRegisteredPathName := Value;
    DoSetRegisteredPathName(Value);
  end;
end;

procedure TCustomTLSInternalServer.DoSetRegisteredPathName(
  const Value: string);
var
  Path: string;
begin
  if (csDesigning in ComponentState) and (Value <> '') then begin
    Path := TSsCertMgrPaths.GetPath(PChar(Value));
    if Path <> '' then begin
      PrivateKeyRingFileName := Path + 'User.pkr';
      RootCertsFileName := Path + 'Root.scl';
      MyCertsFileName := Path + 'User.scl';
      TrustedCertsFileName := Path + 'Collected.scl';
    end else begin
      SetRegisteredPathName('');
      raise Exception.Create('The file path for ' + Value + ' could not be found');
    end;
  end else if Value <> '' then begin
    SetRegisteredPathName('');
    if not (csLoading in ComponentState) then
      raise Exception.Create('This property can be set at design time only');
  end;
end;

procedure TCustomTLSInternalServer.CheckLoaded;
begin
  Loaded;
end;

procedure TCustomTLSInternalServer.InternalLoaded;
begin
  FStreamSecII.Name := Name + 'StreamSecII';
  FMyCerts.Name := Name + 'MyCerts';
  FRootCerts.Name := Name + 'RootCerts';
  FTrustedCerts.Name := Name + 'TrustedCerts';
{$IFDEF SHA1_AND_MD5}
  FTLSOptions.OnChange := HandleCipherSuitesChange;
{$ENDIF SHA1_AND_MD5}
  LoadedResolveResourceFiles;
  FPrivateKeyRingLoaded := Assigned(FStreamSecII.PrivateKeyRing) or
                           Assigned(FStreamSecII.PrivateKeyRingFile) or
                           (FStreamSecII.PrivateKeyRingFileName <> '');
  ValidatePublicKeyPairs;
{$IFDEF SHA1_AND_MD5}
  if FTLSSetupServerWhenLoaded then
    TLSSetupServer;
{$ENDIF SHA1_AND_MD5}
end;

{$IFDEF SHA1_AND_MD5}
{ TAbstractTLSSocket }

procedure TAbstractTLSSocket.Close;
var
  vRefCounter: IUnknown;
begin
  vRefCounter := FRefCounter;
  FKeepAlive := nil;
  // Close might be called from another thread and the socket thread might be
  // stuck waiting for data. The following code will close the socket without
  // a proper TLS close notification:
  DoSleep(1);
  if FConnected then begin
    try
      RawClose;                        // Close the socket
    except
    end;
    InternalSetConnected(False);     // Set FConnected to False
  end;
end;

procedure TAbstractTLSSocket.CopySession(ASource: TAbstractTLSSocket);
begin
  URIToCheck := ASource.URIToCheck;
  DNSNameToCheck := ASource.DNSNameToCheck;
  IPToCheck := ASource.IPToCheck;
  SessionID := ASource.SessionID;
  CipherSuite := ASource.CipherSuite;
end;

constructor TAbstractTLSSocket.Create;
begin
  InterlockedIncrement(GTLSSocketInstanceCount);
  FRefCounter := TRefCounter.Create(Self);
  inherited;
  FInData := TSecureMemoryStream.Create;
  FOutData := TSecureMemoryStream.Create;
  FLock := TCriticalSection.Create;
  FConnectTimeOut := 20000;
  FSleepInterval := 1;
end;

destructor TAbstractTLSSocket.Destroy;
begin
  if Assigned(FKeepAlive) then FKeepAlive.Reset;
  if Assigned(FRefCounter) then FRefCounter.Reset;
  Disconnect;
  FOutData.Free;
  FInData.Free;
  FLock.Free;
  inherited;
  InterlockedDecrement(GTLSSocketInstanceCount);
end;

procedure TAbstractTLSSocket.DoConnected(APeer: TCustomTLS_ContentLayer);
begin
  DoTLSCreate(APeer);
  if Assigned(APeer) then begin
    FEntity := APeer.Context.entity;
    FCipherSuite := APeer.CipherPreference;
    FSessionID := APeer.SessionID;
  end else begin
    FSessionID := '';
    SetLength(FCipherSuite,0);
  end;
end;

procedure TAbstractTLSSocket.DoSleep(Milliseconds: Cardinal);
begin
  if Assigned(SleepProc) then
    SleepProc(Milliseconds)
  else
    Sleep(Milliseconds);
end;

procedure TAbstractTLSSocket.DoTLSCreate(APeer: TCustomTLS_ContentLayer);
begin
  if FTLSPeer <> APeer then begin
    if Assigned(FTLSPeer) then
      FTLSPeer.Release;
    FTLSPeer := APeer;
  end;
end;

function TAbstractTLSSocket.GetSending: Boolean;
begin
  Result := FSending;
end;

procedure TAbstractTLSSocket.InternalCheckDisconnected;
var
  vSelf: ITLSSocket;
  Peer: TCustomTLS_ContentLayer;
begin
  vSelf := Self;
  Peer := nil;
  if Assigned(FTLSPeer) then
    Peer := FTLSPeer
  else if Assigned(TLSServer) then begin
    try
      TLSServer.FStreamSecII.FindTLSSession(FSocketID,Peer,True);
      if Peer = nil then Abort;
      DoTLSCreate(Peer);
    except
      FTLSPeer := nil;
      Peer.Release;
      raise;
    end;
  end;
  if (Peer = nil) or not Peer.Active then begin
    InternalSetConnected(False);
    RawClose;
  end else begin
    InternalSetConnected(True);
    FEncrypted := Peer.Encrypted;
    FSessionID := Peer.SessionID;
  end;
end;

procedure TAbstractTLSSocket.InternalConnect;
var
  OutStrm: TSecureMemoryStream;
  InStrm: TStream;
  Done: Boolean;
  Peer: TCustomTLS_ContentLayer;
  vKeepAlive: IUnknown;
  vRefCounter: IUnknown;
  Ticks: Cardinal;
begin
  vKeepAlive := FKeepAlive;
  vRefCounter := FRefCounter;
  FTLSPeer := nil;
  RawConnect;
  if FConnected then begin
    OutStrm := TSecureMemoryStream.Create;
    try
      if (FSessionID = '') or (Length(FCipherSuite) <> 1) then
        TLSServer.TLSConnect(FSocketID,OutStrm,@FErrorCode)
      else
        TLSServer.TLSConnect(FSocketID,SessionID,FCipherSuite,OutStrm,@FErrorCode);
      if (IPToCheck <> '') or (DNSNameToCheck <> '') or (URIToCheck <> '') then begin
        TLSServer.StreamSecIIComp.FindTLSSession(FSocketId,Peer,True);
        if Assigned(Peer) then begin
          DoTLSCreate(Peer);
          if vsnIP in TLSServer.Options.VerifyServerName then
            Peer.IPToCheck := IPToCheck
          else
            Peer.IPToCheck := '';
          if vsnDNS in TLSServer.Options.VerifyServerName then
            Peer.DNSNameToCheck := DNSNameToCheck
          else
            Peer.DNSNameToCheck := '';
          if vsnURI in TLSServer.Options.VerifyServerName then
            Peer.URIToCheck := URIToCheck
          else
            Peer.URIToCheck := '';
        end;
      end;
      RawSend(OutStrm);
      Ticks := GetTickCount;
      Done := False;
      while Connected and not Done do begin
        OutStrm.Size := 0;
        InStrm := RawReceive;
        if Assigned(InStrm) then begin
          try
            try
              InternalLock;
              try
                if Assigned(FTLSPeer) then begin
                  FTLSPeer.DecodeData(InStrm,FInData,OutStrm);
                  FErrorCode := FTLSPeer.LastAlertCode;
                end else
                  TLSServer.TLSDecodeData(FSocketID,InStrm,FInData,OutStrm,@FErrorCode);
              finally
                InternalUnlock;
              end;
              if OutStrm.Size > 0 then begin
                Done := FErrorCode <> 0;
                OutStrm.Position := 0;
                RawSend(OutStrm);
              end else
                Done := False;
              InternalCheckDisconnected;
            except
              InternalSetConnected(False);
              raise;
            end;
            Done := Done or Encrypted or not Connected;
          finally
            InStrm.Free;
          end;
        end else
          DoSleep(FSleepInterval);
        if Cardinal(GetTickCount - Ticks) > FConnectTimeOut then begin
          RawClose;
          InternalSetConnected(False);
          SetTLSServer(nil);
        end;
      end;
      if Connected then begin
        Peer := FTLSPeer;
        if Assigned(Peer) then begin
          SetEncrypted(Peer.Encrypted);
          if Encrypted then
            DoConnected(Peer);
        end else
          SetEncrypted(False);
      end;
      if Connected and (FErrorCode <> 0) then begin
        DoConnected(nil);
        InternalSetConnected(False);
        SetTLSServer(nil);
        RawClose;
      end;
    finally
      OutStrm.Free;
    end;
  end;
end;

procedure TAbstractTLSSocket.InternalDisconnect;
var
  OutStrm: TSecureMemoryStream;
  vRefCounter: IUnknown;
  Peer: TCustomTLS_ContentLayer;
begin
  vRefCounter := FRefCounter;
  if FConnected and Assigned(TLSServer) then begin
    // Socket is open.
    OutStrm := TSecureMemoryStream.Create;
    try
      InternalLock;
      try
        Peer := FTLSPeer;
        FTLSPeer := nil;
        if Assigned(Peer) then begin
          Peer.Close(OutStrm);
          FErrorCode := Peer.LastAlertCode;
          Peer.Release;
        end else
          TLSServer.TLSClose(FSocketID,OutStrm,@FErrorCode);
        try
          try
            RawSend(OutStrm);           // Send a Close Notification
          finally
            InternalSetConnected(False);// Set Connected to False
            RawClose;                   // Close the socket
          end;
        finally
          TLSServer := nil;
        end;
      finally
        InternalUnlock;
      end;
    finally
      OutStrm.Free;
    end;
  end else
    SetTLSServer(nil); // Destroy the TTLS_ContentLayer if not done
end;

procedure TAbstractTLSSocket.InternalLock;
begin
//  FLock.Acquire;
end;

procedure TAbstractTLSSocket.InternalRead;
var
  InStrm: TStream;
  OutStrm: TSecureMemoryStream;
  vKeepAlive: IUnknown;
  vRefCounter: IUnknown;
begin
  vKeepAlive := FKeepAlive;
  vRefCounter := FRefCounter;
  InStrm := RawReceive;
  if Assigned(InStrm) then try
    CheckAbort;
    try
      OutStrm := TSecureMemoryStream.Create;
      try
        InternalLock;
        try
          if Assigned(FTLSPeer) then begin
            FTLSPeer.DecodeData(InStrm,FInData,OutStrm);
            FErrorCode := FTLSPeer.LastAlertCode;
          end else
            TLSServer.TLSDecodeData(FSocketID,InStrm,FInData,OutStrm,@FErrorCode);
        finally
          InternalUnlock;
        end;
        if OutStrm.Size > 0 then begin
          OutStrm.Position := 0;
          RawSend(OutStrm);
        end;
      finally
        OutStrm.Free;
      end;
      InternalCheckDisconnected;
    except
      InternalSetConnected(False);
      raise;
    end;
  finally
    InStrm.Free;
  end else
    DoSleep(FSleepInterval);
end;

procedure TAbstractTLSSocket.InternalSend;
var
  OutStrm: TSecureMemoryStream;
  CheckDisconnect: Boolean;
  vKeepAlive: IUnknown;
  vRefCounter: IUnknown;

  function DataToSend: Boolean;
  begin
    if not Connected then
      Result := False
    else begin
      InternalLock;
      try
        Result := FOutData.Size > FOutData.Position;
        if not Result then begin
          FOutData.Size := 0;
          FSending := False;
        end;
      finally
        InternalUnlock;
      end;
    end;
  end;

begin
  vKeepAlive := FKeepAlive;
  vRefCounter := FRefCounter;
  CheckDisconnect := False;
  if DataToSend then begin
    OutStrm := TSecureMemoryStream.Create;
    try
      try
        repeat
          CheckAbort;
          CheckDisconnect := True;
          InternalLock;
          try
            if Assigned(FTLSPeer) and FTLSPeer.Active and FTLSPeer.Encrypted then begin
              FTLSPeer.EncodeData(FOutData,OutStrm);
              FErrorCode := FTLSPeer.LastAlertCode;
            end else
              TLSServer.TLSEncodeData(FSocketID,FOutData,OutStrm,@FErrorCode);
          finally
            InternalUnlock;
          end;
          if OutStrm.Size > 0 then begin
            OutStrm.Position := 0;
            RawSend(OutStrm);
            OutStrm.SetSize(0);
          end else
            InternalSetConnected(False);
        until not DataToSend;
      except
        InternalSetConnected(False);
        raise;
      end;
    finally
      OutStrm.Free;
    end;
  end;

  if CheckDisconnect then
    InternalCheckDisconnected;
end;

procedure TAbstractTLSSocket.InternalSetConnected(Value: Boolean);
begin
  if Value <> FConnected then begin
    if Assigned(FKeepAlive) then FKeepAlive.Reset;
    if Value then
      FKeepAlive := TKeepAlive.Create(Self)
    else begin
      FEncrypted := False;
      FKeepAlive := nil;
    end;
    FConnected := Value;
  end;
  if not Value then
    SetTLSServer(nil);
end;

procedure TAbstractTLSSocket.InternalUnlock;
begin
//  FLock.Release;
end;

procedure TAbstractTLSSocket.Lock;
begin
  FLock.Acquire;
end;

procedure TAbstractTLSSocket.Open;
begin
  Connect;
end;

function TAbstractTLSSocket.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TAbstractTLSSocket.ReceiveBuf(var Buf; Count: Integer): Integer;
var
  vKeepAlive: IUnknown;
  vRefCounter: IUnknown;
begin
  vKeepAlive := FKeepAlive;
  vRefCounter := FRefCounter;
  InternalLock;
  try
    Result := FInData.Size - FInDataReadPos;
    FInData.Position := FInDataReadPos;
    if Result > Count then begin
      Result := FIndata.Read(Buf,Count);
      Inc(FInDataReadPos,Result);
      FInData.Seek(0,soFromEnd);
    end else begin
      Result := FInData.Read(Buf,Count);
      FInData.Size := 0;
      FInDataReadPos := 0;
    end;
  finally
    InternalUnlock;
  end;
end;

function TAbstractTLSSocket.ReceiveLength: Integer;
var
  vKeepAlive: IUnknown;
  vRefCounter: IUnknown;

  function AlreadyRead: Integer;
  begin
    InternalLock;
    try
      Result := FInData.Size - FInDataReadPos;
    finally
      InternalUnlock;
    end;
  end;

begin
  vKeepAlive := FKeepAlive;
  vRefCounter := FRefCounter;
  Result := AlreadyRead;
  if (Result = 0) and Assigned(FTLSServer) then begin
    Receive;
    Result := AlreadyRead;
  end;
end;

procedure TAbstractTLSSocket.ReceiveStream(AStream: TStream;
  AByteCount: Integer; const AReadUntilDisconnect: Boolean);
var
  Len, InDataSize: Integer;
  Buf: PChar;
var
  vKeepAlive: IUnknown;
  vRefCounter: IUnknown;
begin
  vKeepAlive := FKeepAlive;
  vRefCounter := FRefCounter;
  if AReadUntilDisconnect then AByteCount := High(AByteCount);
  repeat
    Receive;
    InternalLock;
    try
      InDataSize := FInData.Size;
      Len := InDataSize - FInDataReadPos;
      if AByteCount < 0 then AByteCount := Len;
      Buf := FInData.Memory;
      if Len > AByteCount then begin
        Len := AStream.Write(Buf[FInDataReadPos],AByteCount);
        Inc(FInDataReadPos,Len);
        FInData.Seek(0,soFromEnd);
      end else begin
        Len := AStream.Write(Buf[FInDataReadPos],Len);
        FInData.Size := 0;
        FInDataReadPos := 0;
      end;
      Dec(AByteCount,Len);
    finally
      InternalUnlock;
    end;
  until (AByteCount = 0) or not Connected;
end;

procedure TAbstractTLSSocket.ReceiveStruct(Struct: TASN1Struct);
var
  Len, InDataSize: Integer;
var
  vKeepAlive: IUnknown;
  vRefCounter: IUnknown;
begin
  vKeepAlive := FKeepAlive;
  vRefCounter := FRefCounter;
  Struct.DisposeContent;
  Struct.AllowResume := True;
  repeat
    Receive;
    InternalLock;
    try
      InDataSize := FInData.Size;
      Len := InDataSize - FInDataReadPos;
      if Len > 0 then begin
        FInData.Position := FInDataReadPos;
        ASN1FromStream(FInData,Struct);
        FInDataReadPos := FInData.Position;
        if FInDataReadPos = InDataSize then begin
          FInData.Size := 0;
          FInDataReadPos := 0;
        end else
          FInData.Seek(0,soFromEnd);
      end;
    finally
      InternalUnlock;
    end;
  until Struct.DoneLoading or not Connected;
end;

procedure TAbstractTLSSocket.SendBuf(const Buf; Count: Integer; SendNow: Boolean);
var
  OldPos, OldSize: Integer;
  vKeepAlive: IUnknown;
  vRefCounter: IUnknown;
begin
  vKeepAlive := FKeepAlive;
  vRefCounter := FRefCounter;
  InternalLock;
  try
    OldPos := FOutData.Position;
    OldSize := FOutData.Seek(0,soFromEnd);
    if OldPos = OldSize then begin
      OldPos := 0;
      FOutData.Seek(0,soFromBeginning);
      FOutData.Size := 0;
    end;
    FOutData.Write(Buf,Count);
    FOutData.Position := OldPos;
    FSending := FSending or SendNow;
  finally
    InternalUnlock;
  end;
  if SendNow then Send;
end;

procedure TAbstractTLSSocket.SendStream(AStream: TStream; const AAll: Boolean;
  const ASize: Integer; SendNow: Boolean);
var
  OldPos, OldSize: Integer;
  Len, DLen: Integer;
  Buf: PChar;
  vKeepAlive: IUnknown;
  vRefCounter: IUnknown;
begin
  vKeepAlive := FKeepAlive;
  vRefCounter := FRefCounter;
  Len := AStream.Size - AStream.Position;
  if (Len > ASize) and not AAll then
    Len := ASize;
  repeat
    InternalLock;
    try
      OldPos := FOutData.Position;
      OldSize := FOutData.Size;
      if OldPos = OldSize then begin
        OldPos := 0;
        OldSize := 0;
      end;
      DLen := Len;
      if (DLen + OldSize - OldPos > MaxPlainTextLength) and SendNow then
        DLen := MaxPlainTextLength - (OldSize - OldPos);
      if DLen > 0 then begin
        FOutData.Size := OldSize + DLen;

        Buf := FOutData.Memory;
        DLen := AStream.Read(Buf[OldSize],DLen);
        Dec(Len,DLen);

        FOutData.Position := OldPos;
        FSending := FSending or SendNow;
      end;
    finally
      InternalUnlock;
    end;
    if SendNow then Send;
  until (Len = 0) or not Connected;
end;

procedure TAbstractTLSSocket.SendStruct(AStruct: TASN1Struct;
  SendNow: Boolean);
var
  OldPos, OldSize: Integer;
  vKeepAlive: IUnknown;
  vRefCounter: IUnknown;
begin
  vKeepAlive := FKeepAlive;
  vRefCounter := FRefCounter;
  InternalLock;
  try
    OldPos := FOutData.Position;
    OldSize := FOutData.Seek(0,soFromEnd);
    if OldPos = OldSize then begin
      OldPos := 0;
      FOutData.Seek(0,soFromBeginning);
      FOutData.Size := 0;
    end;
    AStruct.SaveToStream(FOutData,fmtDER);
    FOutData.Position := OldPos;
    FSending := FSending or SendNow;
  finally
    InternalUnlock;
  end;
  if SendNow then Send;
end;

procedure TAbstractTLSSocket.SetEncrypted(const Value: Boolean);
begin
  FEncrypted := Value;
end;

procedure TAbstractTLSSocket.SetSocketID(const Value: Pointer);
begin
  FSocketID := Value;
end;

procedure TAbstractTLSSocket.SetTLSServer(const Value: TCustomTLSInternalServer);
var
  Peer: TCustomTLS_ContentLayer;
begin
  Peer := FTLSPeer;
  FTLSPeer := nil;
  if Value <> FTLSServer then begin
    FEncrypted := False;
    if Assigned(FTLSServer) then begin     
      FTLSServer.DoRemoveSocket(Self);
      if Peer = nil then
        FTLSServer.TLSClose(FSocketID,nil);
    end else
      FSocketID := nil;
    FTLSServer := Value;
    if Assigned(Value) then
      Value.DoNewSocket(Self);
  end;
  if Assigned(Peer) then
    Peer.Release;
end;

procedure TAbstractTLSSocket.Unlock;
begin
  FLock.Release;
end;

function TAbstractTLSSocket._AddRef: Integer;
begin
  Result := -1;
end;

function TAbstractTLSSocket._Release: Integer;
begin
  Result := -1;
end;

function TAbstractTLSSocket.GetTLSServer: TCustomTLSInternalServer;
begin
  Result := FTLSServer;
end;

function TAbstractTLSSocket.GetClientCertDNSName: string;
var
  Cert: PASN1Struct;
begin
  Result := FClientCertDNSName;
  if Result = '' then begin
    Cert := GetClientCert;
    if Assigned(Cert) then
      Result := ExtractDNSName(Cert^)
    else
      Result := '';
    FClientCertDNSName := Result;
  end;
end;

function TAbstractTLSSocket.GetClientCertIP: string;
var
  Cert: PASN1Struct;
begin
  Result := FClientCertIP;
  if Result = '' then begin
    Cert := GetClientCert;
    if Assigned(Cert) then
      Result := ExtractIP(Cert^)
    else
      Result := '';
    FClientCertIP := Result;
  end;
end;

function TAbstractTLSSocket.GetClientCertURI: string;
var
  Cert: PASN1Struct;
begin
  Result := FClientCertURI;
  if Result = '' then begin
    Cert := GetClientCert;
    if Assigned(Cert) then
      Result := ExtractURI(Cert^)
    else
      Result := '';
    FClientCertURI := Result;
  end;
end;

function TAbstractTLSSocket.GetClientCert: PASN1Struct;
var
  Peer: TCustomTLS_ContentLayer;
begin
  Result := nil;
  Peer := nil;
  if Assigned(FTLSPeer) then
    Peer := FTLSPeer
  else if Assigned(FTLSServer) then
    if FTLSServer.FindTLSSession(FSocketID,Peer) < 0 then
      Peer := nil;
  if Assigned(Peer) then
    Result := Peer.ClientCertificate;
end;

function TAbstractTLSSocket.GetServerCert: PASN1Struct;
var
  Peer: TCustomTLS_ContentLayer;
begin
  Peer := GetPeer;
  if Assigned(Peer) then
    Result := Peer.ServerCertificate
  else
    Result := nil;
end;

function TAbstractTLSSocket.GetServerCertDNSName: string;
var
  Cert: PASN1Struct;
begin
  Result := FServerCertDNSName;
  if Result = '' then begin
    Cert := GetServerCert;
    if Assigned(Cert) then
      Result := ExtractDNSName(Cert^)
    else
      Result := '';
    FServerCertDNSName := Result;
  end;
end;

function TAbstractTLSSocket.GetServerCertIP: string;
var
  Cert: PASN1Struct;
begin
  Result := FServerCertIP;
  if Result = '' then begin
    Cert := GetServerCert;
    if Assigned(Cert) then
      Result := ExtractIP(Cert^)
    else
      Result := '';
    FServerCertIP := Result;
  end;
end;

function TAbstractTLSSocket.GetServerCertURI: string;
var
  Cert: PASN1Struct;
begin
  Result := FServerCertURI;
  if Result = '' then begin
    Cert := GetServerCert;
    if Assigned(Cert) then
      Result := ExtractURI(Cert^)
    else
      Result := '';
    FServerCertURI := Result;
  end;
end;

procedure TAbstractTLSSocket.SetDNSNameToCheck(const Value: string);
begin
  FDNSNameToCheck := Value;
end;

procedure TAbstractTLSSocket.SetIPToCheck(const Value: string);
begin
  FIPToCheck := Value;
end;

procedure TAbstractTLSSocket.SetURIToCheck(const Value: string);
begin
  FURIToCheck := Value;
end;

procedure TAbstractTLSSocket.Release;
begin
  if Self <> nil then begin
    FKeepAlive := nil;
    FRefCounter := nil;
  end;
end;

procedure TAbstractTLSSocket.CheckAbort;
begin
  if (FRefCounter = nil) or (FKeepAlive = nil) then
//    Abort;
end;

procedure TAbstractTLSSocket.SetConnectTimeOut(const Value: Cardinal);
begin
  FConnectTimeOut := Value;
end;

function TAbstractTLSSocket.GetPeer: TCustomTLS_ContentLayer;
begin
  Result := nil;
  if Assigned(FTLSPeer) then
    Result := FTLSPeer
  else if Assigned(FTLSServer) then
    if FTLSServer.FindTLSSession(FSocketID,Result) < 0 then
      Result := nil;
end;
{$ENDIF SHA1_AND_MD5}

{ TSimpleTLSInternalServer }

constructor TCustomSimpleTLSInternalServer.Create(AOwner: TComponent);
begin
  inherited;               
  FStreamSecII.AllowPlainTextKeys := False;
  FMyCertsFile := TResourceFile.Create(Self);
  FMyCertsFile.Name := GetNamePath + 'MyCertsFile';
  FRootCertsFile := TResourceFile.Create(Self);
  FRootCertsFile.Name := GetNamePath + 'RootCertsFile';
  FTrustedCertsFile := TResourceFile.Create(Self);
  FTrustedCertsFile.Name := GetNamePath + 'TrustedCertsFile';
  FPrivateKeyRingFile := TResourceFile.Create(Self);
  FPrivateKeyRingFile.Name := GetNamePath + 'PrivateKeyRingFile';
  FKeyDerivationBits := 10;    
{$IFDEF SHA1_AND_MD5}
  Options.BulkCipherAES128 := prPrefer;
  Options.BulkCipherAES256 := prAllowed;
  Options.KeyAgreementDHE := prPrefer;
  Options.KeyAgreementECDHE := prAllowed;
  Options.EphemeralECDHKeySize := ecs256;  
{$ENDIF SHA1_AND_MD5}
end;

function TCustomSimpleTLSInternalServer.CreateNewCertDlg(
  ACreateType: TNewCertCreateEnum): INewCertDlg;
var
  Intf: IUserList;
begin
  Result := nil;
  if Assigned(FBeforeCreateNewCertDlg) then
    FBeforeCreateNewCertDlg(Self,ACreateType,Result);
{$IFDEF GUI_APP}
  if not Assigned(Result) then
    Supports(GetNewCertDlg.Create(nil),INewCertDlg,Result);
{$ENDIF GUI_APP}
  if not Assigned(Result) then
    raise Exception.Create('Unable to create a NewCertDlg');
  if Assigned(FUserList) then begin
    Supports(FUserList,IUserList,Intf);
    Result.UserList := Intf;
  end;
end;

procedure TCustomSimpleTLSInternalServer.DefineProperties(AFiler: TFiler);
begin
  inherited;
  AFiler.DefineBinaryProperty('InternalRootCerts',ReadRootCerts,WriteRootCerts,RootCertsSource = nil);
  AFiler.DefineBinaryProperty('InternalPrivateKeyRing',ReadPrivateKeyRing,WritePrivateKeyRing,PrivateKeyRing = nil);
  AFiler.DefineBinaryProperty('InternalMyCerts',ReadMyCerts,WriteMyCerts,MyCertsSource = nil);                      
  AFiler.DefineBinaryProperty('InternalTrustedCerts',ReadTrustedCerts,WriteTrustedCerts,TrustedCertsSource = nil);
end;

destructor TCustomSimpleTLSInternalServer.Destroy;
begin
  MyCertsSCLFile := nil;
  RootCertsSCLFile := nil;
  PrivateKeyRingFile := nil;
  TrustedCertsSCLFile := nil;
  FMyCertsFile.Free;
  FRootCertsFile.Free;
  FTrustedCertsFile.Free;
  FPrivateKeyRingFile.Free;
  inherited Destroy;
end;

{$IFDEF GUI_APP}
function TCustomSimpleTLSInternalServer.GetNewCertDlg: TFormClass;
begin
  Result := TfrmNewCertDlg;
  if Assigned(FOnGetNewCertDlg) then
    FOnGetNewCertDlg(Self,Result);
  if (Result = nil) or (Result.GetInterfaceEntry(INewCertDlg) = nil) then
    raise Exception.Create('A NewCertDlg class must implement StreamSecII.INewCertDlg');
end;
{$ENDIF}

{$IFDEF SHA1_AND_MD5}
procedure TCustomSimpleTLSInternalServer.HandleCipherSuitesChange(
  Sender: TObject);
begin
  { This method will try to set some KeyAgreement and Signature options to
    prAllowed. They will be changed to prNotAllowed if there is no corresponding
    certificate.

    Since the method operates by changing Options we must prevent that it is
    called recursively. Set Options.OnChange to nil: }
  Options.OnChange := nil;
  Options.BeginUpdate;
  try
    with Options do begin
      SignatureAnon := prNotAllowed;
      if SignatureDSS = prNotAllowed then
        SignatureDSS := prAllowed;
      if SignatureECDSA = prNotAllowed then
        SignatureECDSA := prAllowed;
      if SignatureRSA = prNotAllowed then
        SignatureRSA := prPrefer;
    end;
    inherited;
  finally
    Options.EndUpdate;
    Options.OnChange := HandleCipherSuitesChange;
  end;
end;
{$ENDIF SHA1_AND_MD5}

procedure TCustomSimpleTLSInternalServer.InternalLoaded;
begin
  inherited InternalLoaded;
  FPrivateKeyRingFile.Name := Name + 'PrivateKeyRingFile';
  FMyCertsFile.Name := Name + 'MyCertsFile';
  FRootCertsFile.Name := Name + 'RootCertsFile';
  FTrustedCertsFile.Name := Name + 'TrustedCertsFile';
end;

function TCustomSimpleTLSInternalServer.InternalWriteMyCerts: Boolean;
var
  Password: ISecretKey;
  CC: TCipherClass;
  SignCert: TASN1Struct;
  SignKey: IMPPrivateKey;
begin
  Result := True;
  if MyCerts.Modified then begin
    StreamSecIIComp.DoPassword(Password);
    CC := FindCipherClass(caRijndael,cmABC);
    if CC = nil then
      CC := FindCipherClass(caTwoFish,cmABC);
    Assert(Assigned(CC),'Rijndael-ABC or TwoFish-ABC must be enabled');
    InternalPrivateKeyRingFile.CheckCreateDataStream;
    StreamSecIIComp.SavePrivateKeyRingToStream(InternalPrivateKeyRingFile.DataStream,
                                               Password,
                                               kdfWPv2SHA1,
                                               1 shl FKeyDerivationBits,
                                               True,
                                               id_aes256_wrap,
                                               CC);
//    PrivateKeyRingFile := InternalPrivateKeyRingFile;

    SignCert := nil;
    try
      if not HasPrivateCACert(SignCert) then
        HasPrivateSignCert(SignCert);
      if Assigned(SignCert) then begin
        SignKey := StreamSecIIComp.FindCreatePrivateKey(SignCert);
        if Assigned(SignKey) then begin
          InternalMyCertsFile.CheckCreateDataStream;
          MyCerts.SignSaveToStream(InternalMyCertsFile.DataStream,
                                   SignCert,
                                   SignKey);
          MyCertsSCLFile := InternalMyCertsFile;
        end else
          Result := False;
      end else
        Result := False;
    finally
      SignCert.Free;
    end;
  end;
end;

function TCustomSimpleTLSInternalServer.InternalWriteRootCerts: Boolean;
var
  SignCert: TASN1Struct;
  SignKey: IMPPrivateKey;
begin
  Result := True;
  if RootCerts.Modified then begin
    SignCert := nil;
    try
      if not HasPrivateCACert(SignCert) then
        HasPrivateSignCert(SignCert);
      if Assigned(SignCert) then begin
        SignKey := StreamSecIIComp.FindCreatePrivateKey(SignCert);
        InternalRootCertsFile.CheckCreateDataStream;
        RootCerts.SignSaveToStream(InternalRootCertsFile.DataStream,
                                   SignCert,
                                   SignKey);
        RootCertsSCLFile := InternalRootCertsFile;
      end else
        Result := False;
    finally
      SignCert.Free;
    end;
  end;
end;

function TCustomSimpleTLSInternalServer.InternalWriteTrustedCerts: Boolean;
var
  SignCert: TASN1Struct;
  SignKey: IMPPrivateKey;
begin
  Result := True;
  if TrustedCerts.Modified then begin
    SignCert := nil;
    try
      if not HasPrivateCACert(SignCert) then
        HasPrivateSignCert(SignCert);
      if Assigned(SignCert) then begin
        SignKey := StreamSecIIComp.FindCreatePrivateKey(SignCert);
        InternalTrustedCertsFile.CheckCreateDataStream;
        TrustedCerts.SignSaveToStream(InternalTrustedCertsFile.DataStream,
                                      SignCert,
                                      SignKey);
        TrustedCertsSCLFile := InternalTrustedCertsFile;
      end else
        Result := False;
    finally
      SignCert.Free;
    end;
  end;
end;

procedure TCustomSimpleTLSInternalServer.KeyAndSign(
  ADialog: INewCertDlg);
var
  Cert: TCertificate;
  P10: TCertificationRequest;
  OID: ObjectIdentifier;
  Ext: TExtension;
  Serial: string;
  Status: TCertStatusCode;
  Res: Boolean;
{$IFDEF GUI_APP}
  ExportDlg: TfrmExportDlg;
{$ENDIF GUI_APP}
begin
  if ADialog.CreateOpt <> nccCertReq then begin
    Cert := ADialog.Cert;
    try
      Cert.Version := 3;

      // Subject Public Key (incl. Subject Key Identifier)
      if ADialog.CreateOpt in [nccRoot,nccCert] then begin
        StreamSecIIComp.DefaultHashAlgorithm := ADialog.SignatureDigestAlg;
        Res := False;
        OID := ADialog.PrivateKeyAlg;
        if CompareStr(OID,id_ecPublicKey) <> 0 then
          Res := StreamSecIIComp.CreateKeyPair(Cert,
                                               OID,
                                               ADialog.KeySize,'',
                                               ADialog.ExportableRSA)
        else case ADialog.KeySize of
          192: Res := PrivateKeyRing.CreateKeyPair(Cert,
                                                   OID,
                                                   -1,
                                                   prime192v1);
          224: Res := PrivateKeyRing.CreateKeyPair(Cert,
                                                   OID,
                                                   -1,
                                                   prime224);
          239: Res := PrivateKeyRing.CreateKeyPair(Cert,
                                                   OID,
                                                   -1,
                                                   prime239v1);
          256: Res := PrivateKeyRing.CreateKeyPair(Cert,
                                                   OID,
                                                   -1,
                                                   prime256v1);
          384: Res := PrivateKeyRing.CreateKeyPair(Cert,
                                                   OID,
                                                   -1,
                                                   prime384);
          521: Res := PrivateKeyRing.CreateKeyPair(Cert,
                                                   OID,
                                                   -1,
                                                   prime521);
        end;
        Assert(Res);
      end else if ADialog.CreateOpt = nccCertFromReq then begin
        Cert.SubjectPublicKeyInfo.AssignStruct(ADialog.SubjectPublicKeyInfo.Data);
        Ext := Cert.TbsCertificate.Extensions.AddUniqueItem(eveIdCeSubjectKeyIdentifier);
        Ext.ExtnValue.AsCe_SubjectKeyIdentifier.AsString := Cert.SubjectPublicKeyInfo.PublicKeyIdentifier;
      end;

      if ADialog.CreateOpt <> nccTemplate then begin
        if ADialog.CreateOpt = nccRoot then
          ADialog.CACert := Cert.Data;

        // Serial
        if Cert.SerialNumber.Length = 0 then begin
          if Assigned(ADialog.IssuedCerts) then
            Serial := ADialog.IssuedCerts.NextSerialNumber(ADialog.CACert)
          else
            Serial := MyCerts.NextSerialNumber(ADialog.CACert);
          Cert.SerialNumber.SetBinary(Pointer(Serial)^,Length(Serial));
        end;

        // Signature (incl. Issuer, Issuer Alt Name, Authority Key Identifier)
{$IFDEF SHA1}
        Res := StreamSecIIComp.SignCertificate(Cert,ADialog.CACert);
{$ELSE  SHA1}
        Res := StreamSecIIComp.SignCertificate(Cert,ADialog.CACert,
          StreamSecIIComp.FindCreatePrivateKey(ADialog.CACert).SignatureDigestAlg);
{$ENDIF SHA1}
        Assert(Res);
      end;

      if ADialog.CreateOpt = nccTemplate then begin
        if Assigned(ADialog.ExportData) then
          Cert.SaveToStream(ADialog.ExportData)
        else begin
{$IFDEF GUI_APP}
          ExportDlg := TfrmExportDlg.Create(nil);
          try
            ExportDlg.memImport.Text := StrToMIME64('-----BEGIN CERTIFICATE TEMPLATE-----',
                                                    '-----END CERTIFICATE TEMPLATE-----',
                                                    Cert.Data.ContentAsOctetString);
            case ExportDlg.ShowModal of
              mrAbort:
                with TSaveDialog.Create(nil) do try
                  Filter := 'Certificate (*.cer)|*.cer';
                  DefaultExt := 'cer';
                  Title := 'Save Certificate';
                  if Execute then
                    Cert.SaveToFile(FileName);
                finally
                  Free;
                end;
            end;
          finally
            ExportDlg.Free;
          end;
{$ELSE  GUI_APP}
          Assert(False,'ExportData must be assigned in non-GUI applications');
{$ENDIF GUI_APP}
        end;
      end else if ADialog.CreateOpt = nccCertFromReq then begin
        Assert(Assigned(ADialog.IssuedCerts),'No IssuedCerts component assigned');
        ADialog.IssuedCerts.AddCertificate(Cert.Data,False,Status);
        if Assigned(ADialog.ExportData) then
          Cert.SaveToStream(ADialog.ExportData)
        else begin
{$IFDEF GUI_APP}
          ExportDlg := TfrmExportDlg.Create(nil);
          try
            ExportDlg.memImport.Text := StrToMIME64('-----BEGIN CERTIFICATE-----',
                                                    '-----END CERTIFICATE-----',
                                                    Cert.Data.ContentAsOctetString);
            case ExportDlg.ShowModal of
              mrAbort:
                with TSaveDialog.Create(nil) do try
                  Filter := 'Certificate (*.cer)|*.cer';
                  DefaultExt := 'cer';
                  Title := 'Save Certificate';
                  if Execute then
                    Cert.SaveToFile(FileName);
                finally
                  Free;
                end;
            end;
          finally
            ExportDlg.Free;
          end;
{$ELSE  GUI_APP}
          Assert(False,'ExportData must be assigned in non-GUI applications');
{$ENDIF GUI_APP}
        end;
      end else begin
        MyCerts.AddCertificate(Cert.Data,True,Status);
        if Assigned(ADialog.ExportData) then
          Cert.SaveToStream(ADialog.ExportData);
      end;

      Assert(Status = crcOK);
    finally
//      Cert := nil;
    end;
  end else begin
    P10 := ADialog.P10;
    try
      P10.CertificationRequestInfo.Version.AsInteger := 3 - 1;

      // Subject Public Key (incl. Subject Key Identifier)
      StreamSecIIComp.DefaultHashAlgorithm := ADialog.SignatureDigestAlg;
      Res := False;
      OID := ADialog.PrivateKeyAlg;
      if CompareStr(OID,id_ecPublicKey) <> 0 then
        Res := StreamSecIIComp.CreateKeyPair(P10,
                                             OID,
                                             ADialog.KeySize,'',
                                             ADialog.ExportableRSA)
      else case ADialog.KeySize of
        192: Res := PrivateKeyRing.CreateKeyPair(P10,
                                                 OID,
                                                 -1,
                                                 prime192v1);
        224: Res := PrivateKeyRing.CreateKeyPair(P10,
                                                 OID,
                                                 -1,
                                                 prime224);
        239: Res := PrivateKeyRing.CreateKeyPair(P10,
                                                 OID,
                                                 -1,
                                                 prime239v1);
        256: Res := PrivateKeyRing.CreateKeyPair(P10,
                                                 OID,
                                                 -1,
                                                 prime256v1);
        384: Res := PrivateKeyRing.CreateKeyPair(P10,
                                                 OID,
                                                 -1,
                                                 prime384);
        521: Res := PrivateKeyRing.CreateKeyPair(P10,
                                                 OID,
                                                 -1,
                                                 prime521);
      end;
      Assert(Res);
      // Sign Certificate Request
      Res := StreamSecIIComp.SignSigned(P10,P10,ADialog.SignatureDigestAlg);
      Assert(Res);
      if Assigned(ADialog.ExportData) then
        P10.SaveToStream(ADialog.ExportData)
      else begin
{$IFDEF GUI_APP}
        ExportDlg := TfrmExportDlg.Create(nil);
        try
          ExportDlg.memImport.Text := StrToMIME64('-----BEGIN CERTIFICATE REQUEST-----',
                                                  '-----END CERTIFICATE REQUEST-----',
                                                  P10.Data.ContentAsOctetString);
          case ExportDlg.ShowModal of
            mrAbort:
              with TSaveDialog.Create(nil) do try
                Filter := 'PKCS#10 Request (*.p10)|*.p10';
                DefaultExt := 'p10';
                Title := 'Save Certificate Request';
                if Execute then
                  P10.SaveToFile(FileName);
              finally
                Free;
              end;
          end;
        finally
          ExportDlg.Free;
        end;
{$ELSE  GUI_APP}
        Assert(False,'ExportData must be assigned in non-GUI applications');
{$ENDIF GUI_APP}
      end;
    finally
//      P10 := nil;
    end;
  end;
end;

procedure TCustomSimpleTLSInternalServer.LoadedResolveResourceFiles;
begin
  inherited;
  RootCertsSCLFile := InternalRootCertsFile;
  MyCertsSCLFile := InternalMyCertsFile;
end;

function TCustomSimpleTLSInternalServer.NewCACertDlg(
  AExportData: TStream): Boolean;
var
  Dlg: INewCertDlg;
  Idx: Integer;
  Status: TCertStatusCode;
begin
  AccumulateEntropy;
  StreamSecIIComp.Exceptions := True;
  if MyCerts.Count = 0 then         
    Dlg := CreateNewCertDlg(nccRoot)
  else
    Dlg := CreateNewCertDlg(nccCert);
  try
    Dlg.PrivateKeyRing := StreamSecIIComp;
    Dlg.MyCerts := MyCerts;
    Dlg.RootCerts := RootCerts;
    Dlg.ExportData := AExportData;
    Idx := MyCerts.Count;
    if Idx < 0 then
      Idx := 0;
    Dlg.SelectWizard(ncwCA);
    Result := Dlg.Execute(Self);
    if MyCerts.Count > Idx then
      RootCerts.AddCertificate(MyCerts.Certs[Idx],True,Status);
  finally
    StreamSecIIComp.Exceptions := False;
  end;

  if Result then begin
    InternalWriteRootCerts;
    InternalWriteMyCerts;
  end;
end;

function TCustomSimpleTLSInternalServer.NewCACertReqDlg(
  AExportData: TStream): Boolean;
var
  Dlg: INewCertDlg;
begin
  AccumulateEntropy;
  StreamSecIIComp.Exceptions := True;
  Dlg := CreateNewCertDlg(nccCertReq);
  try
    Dlg.PrivateKeyRing := StreamSecIIComp;
    Dlg.MyCerts := MyCerts;
    Dlg.RootCerts := RootCerts;
    Dlg.ExportData := AExportData;
    Dlg.SelectWizard(ncwCA,True);
    Result := Dlg.Execute(Self);
  finally
    StreamSecIIComp.Exceptions := False;
  end;
end;

function TCustomSimpleTLSInternalServer.NewClientCertDlg(
  AExportData: TStream): Boolean;
var
  Dlg: INewCertDlg;
begin
  AccumulateEntropy;
  StreamSecIIComp.Exceptions := True;
  Dlg := CreateNewCertDlg(nccCert);
  try
    Dlg.PrivateKeyRing := StreamSecIIComp;
    Dlg.MyCerts := MyCerts;
    Dlg.RootCerts := RootCerts;
    Dlg.ExportData := AExportData;
    Dlg.SelectWizard(ncwClient);
    Result := Dlg.Execute(Self);
  finally
    StreamSecIIComp.Exceptions := False;
  end;

  if Result then begin
    InternalWriteMyCerts;
    ValidatePublicKeyPairs;
  end;
end;

function TCustomSimpleTLSInternalServer.NewClientCertReqDlg(
  AExportData: TStream): Boolean;
var
  Dlg: INewCertDlg;
begin
  AccumulateEntropy;
  StreamSecIIComp.Exceptions := True;
  Dlg := CreateNewCertDlg(nccCertReq);
  try
    Dlg.PrivateKeyRing := StreamSecIIComp;
    Dlg.MyCerts := MyCerts;
    Dlg.RootCerts := RootCerts;
    Dlg.ExportData := AExportData;
    Dlg.SelectWizard(ncwClient,True);
    Result := Dlg.Execute(Self);
  finally
    StreamSecIIComp.Exceptions := False;
  end;
end;

function TCustomSimpleTLSInternalServer.NewP2PCertDlg(
  AExportData: TStream): Boolean;
var
  Dlg: INewCertDlg;
begin
  AccumulateEntropy;
  StreamSecIIComp.Exceptions := True;
  Dlg := CreateNewCertDlg(nccCert);
  try
    Dlg.PrivateKeyRing := StreamSecIIComp;
    Dlg.MyCerts := MyCerts;
    Dlg.RootCerts := RootCerts;
    Dlg.ExportData := AExportData;
    Dlg.SelectWizard(ncwPeerToPeer);
    Result := Dlg.Execute(Self);
  finally
    StreamSecIIComp.Exceptions := False;
  end;

  if Result then begin
    InternalWriteMyCerts;
    ValidatePublicKeyPairs;
  end;
end;

function TCustomSimpleTLSInternalServer.NewP2PCertReqDlg(
  AExportData: TStream): Boolean;
var
  Dlg: INewCertDlg;
begin
  AccumulateEntropy;
  StreamSecIIComp.Exceptions := True;
  Dlg := CreateNewCertDlg(nccCertReq);
  try
    Dlg.PrivateKeyRing := StreamSecIIComp;
    Dlg.MyCerts := MyCerts;
    Dlg.RootCerts := RootCerts;
    Dlg.ExportData := AExportData;
    Dlg.SelectWizard(ncwPeerToPeer,True);
    Result := Dlg.Execute(Self);
  finally
    StreamSecIIComp.Exceptions := False;
  end;
end;

function TCustomSimpleTLSInternalServer.NewServerCertDlg(
  AExportData: TStream): Boolean;
var
  Dlg: INewCertDlg;
begin
  AccumulateEntropy;
  StreamSecIIComp.Exceptions := True;
  Dlg := CreateNewCertDlg(nccCert);
  try
    Dlg.PrivateKeyRing := StreamSecIIComp;
    Dlg.MyCerts := MyCerts;
    Dlg.RootCerts := RootCerts;
    Dlg.ExportData := AExportData;
    Dlg.SelectWizard(ncwServer);
    Result := Dlg.Execute(Self);
  finally
    StreamSecIIComp.Exceptions := False;
  end;

  if Result then begin
    InternalWriteMyCerts;
    ValidatePublicKeyPairs;
  end;
end;

function TCustomSimpleTLSInternalServer.NewServerCertReqDlg(
  AExportData: TStream): Boolean;
var
  Dlg: INewCertDlg;
begin
  AccumulateEntropy;
  StreamSecIIComp.Exceptions := True;
  Dlg := CreateNewCertDlg(nccCertReq);
  try
    Dlg.PrivateKeyRing := StreamSecIIComp;
    Dlg.MyCerts := MyCerts;
    Dlg.RootCerts := RootCerts;
    Dlg.ExportData := AExportData;
    Dlg.SelectWizard(ncwServer,True);
    Result := Dlg.Execute(Self);
  finally
    StreamSecIIComp.Exceptions := False;
  end;
end;

procedure TCustomSimpleTLSInternalServer.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FUserList) and (Operation = opRemove) then
    SetUserList(nil);
end;

procedure TCustomSimpleTLSInternalServer.ReadMyCerts(AStream: TStream);
begin
  AStream.ReadComponent(FMyCertsFile);
end;

procedure TCustomSimpleTLSInternalServer.ReadPrivateKeyRing(AStream: TStream);
begin
  AStream.ReadComponent(FPrivateKeyRingFile);
  StreamSecIIComp.PrivateKeyRingFile := FPrivateKeyRingFile;
end;

procedure TCustomSimpleTLSInternalServer.ReadRootCerts(AStream: TStream);
begin
  RootCertsSCLFile := FRootCertsFile;
  AStream.ReadComponent(FRootCertsFile);
end;

procedure TCustomSimpleTLSInternalServer.ReadTrustedCerts(
  AStream: TStream);
begin
  TrustedCertsSCLFile := FTrustedCertsFile;
  AStream.ReadComponent(FTrustedCertsFile);
end;

function TCustomSimpleTLSInternalServer.SaveMyCertsToSCLFile(
  AFileName: TFileName): Boolean;
var
  FS: TFileStream;
begin
  Result := InternalWriteMyCerts;
  if Result and Assigned(InternalMyCertsFile.DataStream) then begin
    FS := TFileStream.Create(AFileName,fmCreate);
    try
      Result := SaveMyCertsToSCLStream(FS);
    finally
      FS.Free;
    end;
  end else
    Result := False;
end;

function TCustomSimpleTLSInternalServer.SaveMyCertsToSCLStream(
  AStream: TStream): Boolean;
begin
  Result := InternalWriteMyCerts;
  if Result and Assigned(InternalMyCertsFile.DataStream) then
    AStream.CopyFrom(InternalMyCertsFile.DataStream,0)
  else
    Result := False;
end;

procedure TCustomSimpleTLSInternalServer.SavePrivateKeyRing(
  AFileName: TFileName);
begin
  inherited;
  if csDesigning in ComponentState then begin
    InternalPrivateKeyRingFile.FileName := AFileName;
    PrivateKeyRingFile := InternalPrivateKeyRingFile;
  end;
end;

function TCustomSimpleTLSInternalServer.SaveRootCertsToSCLFile(
  AFileName: TFileName): Boolean;
var
  FS: TFileStream;
begin
  Result := InternalWriteRootCerts;
  if Result and Assigned(InternalRootCertsFile.DataStream) then begin
    FS := TFileStream.Create(AFileName,fmCreate);
    try
      Result := SaveRootCertsToSCLStream(FS);
    finally
      FS.Free;
    end;
  end else
    Result := False;
end;

function TCustomSimpleTLSInternalServer.SaveRootCertsToSCLStream(
  AStream: TStream): Boolean;
begin
  Result := InternalWriteRootCerts;
  if Result and Assigned(InternalRootCertsFile.DataStream) then
    AStream.CopyFrom(InternalRootCertsFile.DataStream,0)
  else
    Result := False;
end;

function TCustomSimpleTLSInternalServer.SaveTrustedCertsToSCLFile(
  AFileName: TFileName): Boolean;
var
  FS: TFileStream;
begin
  Result := InternalWriteTrustedCerts;
  if Result and Assigned(InternalTrustedCertsFile.DataStream) then begin
    FS := TFileStream.Create(AFileName,fmCreate);
    try
      Result := SaveTrustedCertsToSCLStream(FS);
    finally
      FS.Free;
    end;
  end else
    Result := False;
end;

function TCustomSimpleTLSInternalServer.SaveTrustedCertsToSCLStream(
  AStream: TStream): Boolean;
begin
  Result := InternalWriteTrustedCerts;
  if Result and Assigned(InternalTrustedCertsFile.DataStream) then
    AStream.CopyFrom(InternalTrustedCertsFile.DataStream,0)
  else
    Result := False;
end;

procedure TCustomSimpleTLSInternalServer.SetBeforeCreateNewCertDlg(
  const Value: TGetNewCertDlgEvent);
begin
  FBeforeCreateNewCertDlg := Value;
end;

{$IFDEF GUI_APP}
procedure TCustomSimpleTLSInternalServer.SetOnGetNewCertDlg(
  const Value: TGetFormClassEvent);
begin
  FOnGetNewCertDlg := Value;
end;            
{$ENDIF GUI_APP}

procedure TCustomSimpleTLSInternalServer.SetUserList(
  const Value: TComponent);
begin
{$IFDEF D5UP}
  if Assigned(FUserList) then
    FUserList.RemoveFreeNotification(Self);
{$ENDIF}
  if Assigned(Value) and not Supports(Value,IUserList) then
    FUserList := nil
  else
    FUserList := Value;
  if Assigned(FUserList) then
    FUserList.FreeNotification(Self);
end;

procedure TCustomSimpleTLSInternalServer.WriteMyCerts(AStream: TStream);
begin
  InternalWriteMyCerts;
  AStream.WriteComponent(FMyCertsFile);
end;

procedure TCustomSimpleTLSInternalServer.WritePrivateKeyRing(AStream: TStream);
begin
  InternalWriteMyCerts;
  AStream.WriteComponent(FPrivateKeyRingFile);
end;

procedure TCustomSimpleTLSInternalServer.WriteRootCerts(AStream: TStream);
begin
  InternalWriteRootCerts;
  AStream.WriteComponent(FRootCertsFile);
end;

procedure TCustomSimpleTLSInternalServer.WriteTrustedCerts(
  AStream: TStream);
begin
  InternalWriteTrustedCerts;
  AStream.WriteComponent(FTrustedCertsFile);
end;

{ TKeepAlive }
                    
{$IFDEF SHA1_AND_MD5}
constructor TKeepAlive.Create(ASocket: TAbstractTLSSocket);
begin
  inherited Create;
  FSocket := ASocket;
end;

destructor TKeepAlive.Destroy;
var
  vSocket: TAbstractTLSSocket;
begin                            
  vSocket := FSocket;            
  FSocket := nil;
  if Assigned(vSocket) then begin
    vSocket.FKeepAlive := nil;
    vSocket.Disconnect;
  end;
  inherited;
end;

procedure TKeepAlive.Reset;
begin
  if Self <> nil then
    FSocket := nil;
end;

{ TRefCounter }

constructor TRefCounter.Create(ASocket: TAbstractTLSSocket);
begin
  inherited Create;
  FSocket := ASocket;
end;

destructor TRefCounter.Destroy;
var
  vSocket: TAbstractTLSSocket;
begin                            
  vSocket := FSocket;
  FSocket := nil;
  if Assigned(vSocket) then begin
    vSocket.FRefCounter := nil;
    vSocket.Free;
  end;
  inherited;
end;

procedure TRefCounter.Reset;
begin
  if Self <> nil then
    FSocket := nil;
end;   
{$ENDIF SHA1_AND_MD5}

{ ETLSAlert }

constructor ETLSAlert.CreateAlert(AAlertCode: Integer; AFatal: Boolean);
begin
  FAlertCode := AAlertCode;
  FFatal := AFatal;
  Create(TlsConst.AlertMsg(AAlertCode));
end;

procedure ETLSAlert.SetAlertCode(const Value: Integer);
begin
  FAlertCode := Value;
end;

procedure ETLSAlert.SetFatal(const Value: Boolean);
begin
  FFatal := Value;
end;

end.
