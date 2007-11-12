{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     MPCMS Unit                                        }
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
unit MpCms;

interface

uses
  SysUtils, Classes, SyncObjs, SecUtils, SsArc2,
  Asn1, Cms, MpX509, StreamSecII, Pkix_Cert, TlsInternalServer;

type
  TSignatureEvent = procedure (Sender: TObject; SignerCert: TCertificate) of object;
  TCounterSignatureEvent = procedure (Sender: TObject;
                                      SignerCert: TCertificate;
                                      CounterSignerCert: TCertificate;
                                      TimeStamp: TDateTime) of object;
  TAddCertificateEvent = procedure (Sender: TObject;
                                    Index: Integer;
                                    var Cert: TCertificate;
                                    var Done: Boolean) of object;
  TEmailEvent = procedure (Sender: TObject; AEmailAddress: string) of object;
  TGetDetachedMessageEvent = procedure (Sender: TObject;
                                        var AMessage: TCustomMemoryStream) of object;

  TSMIMECapabilityItem = class(TCollectionItem)
  private
    FCipherAlg: TCipherAlg;
    FCipherMode: TCipherMode;
    FKeyLength: Word;
    FKeyedIV: Boolean;
    procedure SetCipherAlg(const Value: TCipherAlg);
    procedure SetCipherMode(const Value: TCipherMode);
    procedure SetKeyLength(const Value: Word);
    function GetOID: ObjectIdentifier;
    procedure SetKeyedIV(const Value: Boolean);
    procedure SetOID(const Value: ObjectIdentifier);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
    function GetIsValid(var CC: TCipherClass): Boolean;
  published
    property CipherAlg: TCipherAlg read FCipherAlg write SetCipherAlg;
    property CipherMode: TCipherMode read FCipherMode write SetCipherMode;
    property KeyedIV: Boolean read FKeyedIV write SetKeyedIV;
    property KeyLength: Word read FKeyLength write SetKeyLength;
    property OID: ObjectIdentifier read GetOID write SetOID stored False;
  end;

  TSMIMECapabilities = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TSMIMECapabilityItem;
    procedure SetItem(Index: Integer; const Value: TSMIMECapabilityItem);
  protected
    function GetAttrCount: Integer; override;
    function GetAttr(Index: Integer): string; override;
    function GetItemAttr(Index, ItemIndex: Integer): string; override;
  public
    function AssignStruct(Struct: TASN1Struct; Intersect: Boolean = True): Boolean;
    function HasCapability(CC: TCipherClass; AKeyLength: Integer; AKeyedIV: Boolean): Boolean;
    property Items[Index: Integer]: TSMIMECapabilityItem read GetItem write SetItem;
  end;

  TBaseMPCMSComponent = class(TComponent)
  private
    FLock: TCriticalSection;
    FIllegalFormat: Boolean;
    FCertificateNotFound: Boolean;
    FAllowExpiredCertificates: Boolean;
    FKeysAndCerts: TCustomTLSInternalServer;
    FContentType: TPKCS7DataType;
    FPrivateKeyRing: TStreamSecII;
    FCollectedCertificates: TX509TrustedCertificates;
    FMyCertificates: TX509TrustedCertificates;
    FCert: TCertificate;
    FFile: TStream;
    FSMIMECapabilities: TSMIMECapabilities;
    FCollectedSMIMECapabilities: TSMIMECapabilities;
    procedure SetAllowExpiredCertificates(const Value: Boolean);
    procedure SetCollectedCertificates(
      const Value: TX509TrustedCertificates);
    procedure SetContentType(const Value: TPKCS7DataType);
    procedure SetKeysAndCerts(const Value: TCustomTLSInternalServer);
    procedure SetMyCertificates(const Value: TX509TrustedCertificates);
    procedure SetPrivateKeyRing(const Value: TStreamSecII);
    procedure SetSMIMECapabilities(const Value: TSMIMECapabilities);
    procedure SetCollectedSMIMECapabilities(
      const Value: TSMIMECapabilities);
  protected
    // Common:
    procedure DoChangeComponentProperty; virtual;
    procedure FillSMIMECapabilities;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetIllegalFormat(Value: Boolean);
    property FileStream: TStream read FFile;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    // To be published:
    property AllowExpiredCertificates: Boolean read FAllowExpiredCertificates write SetAllowExpiredCertificates;
    property CertificateNotFound: Boolean read FCertificateNotFound;
    property CollectedCertificates: TX509TrustedCertificates read FCollectedCertificates write SetCollectedCertificates;
    property ContentType: TPKCS7DataType read FContentType write SetContentType;
    property IllegalFormat: Boolean read FIllegalFormat;
    property KeysAndCerts: TCustomTLSInternalServer read FKeysAndCerts write SetKeysAndCerts;
    property MyCertificates: TX509TrustedCertificates read FMyCertificates write SetMyCertificates;
    property PrivateKeyRing: TStreamSecII read FPrivateKeyRing write SetPrivateKeyRing;
    property SMIMECapabilities: TSMIMECapabilities read FSMIMECapabilities write SetSMIMECapabilities;
    property CollectedSMIMECapabilities: TSMIMECapabilities read FCollectedSMIMECapabilities write SetCollectedSMIMECapabilities;
  end;

  TCustomMPCMSBroker = class; // forward

  TCustomMPSignedData = class(TBaseMPCMSComponent)
  private
    FHashClass: THashClass;
    FSigStruct: TASN1Struct;
    FContentDigestAlg: THashAlgorithm;
    FContentDigest: packed array [0..63] of Byte;
    FIncludeRootCertificate: Boolean;
    FSigningDescription: WideString;
    FSigningTime: TDateTime;
    FOnGoodSignature: TSignatureEvent;
    FOnBadSignature: TSignatureEvent;
    FDetachedSignature: Boolean;
    FOnGetDetachedMessage: TGetDetachedMessageEvent;
    FContentOnly: Boolean;
    FOnAddSigner: TAddCertificateEvent;
    FExtractedCapabilities: PASN1Struct;
    FExtKeyCompatibility: Boolean;
    FOnGoodCounterSignature: TCounterSignatureEvent;
    FOrgFilePosition: Integer;
    FBroker: TCustomMPCMSBroker;
    FMD5Override: Boolean;
    FIncludeSigningTime: Boolean;
    procedure SetHashClass(const Value: THashClass);
    procedure SetIncludeRootCertificate(const Value: Boolean);
    procedure SetSigningDescription(const Value: WideString);
    procedure SetOnBadSignature(const Value: TSignatureEvent);
    procedure SetOnGoodSignature(const Value: TSignatureEvent);
    procedure SetDetachedSignature(const Value: Boolean);
    procedure SetOnGetDetachedMessage(
      const Value: TGetDetachedMessageEvent);
    procedure SetContentOnly(const Value: Boolean);
    procedure SetOnAddSigner(const Value: TAddCertificateEvent);
    procedure SetExtKeyCompatibility(const Value: Boolean);
    procedure SetOnGoodCounterSignature(
      const Value: TCounterSignatureEvent);
    procedure SetMD5Override(const Value: Boolean);
    procedure SetIncludeSigningTime(const Value: Boolean);
  protected
    // Verification:
    procedure DoGoodCounterSignature(OuterCert, Cert: TCertificate);    
    procedure DoGoodSignature(Cert: TCertificate);
    procedure ExtractCertificates; virtual;
    procedure Final(AContent: TStream); virtual;
    procedure Init; virtual;
    function LoadSigStruct: Boolean; virtual;
    function VerifyAttributes(SignerInfo: TASN1Struct;
                              CounterSign: Boolean = False): Boolean; virtual;
    function VerifyUnsignedAttributes(SignerInfo: TASN1Struct): Boolean; virtual;
    function VerifySignature(AContent: TStream): Boolean;
    // Signing:
    procedure DoAddSigner(Index: Integer;
                          var Cert: TCertificate;
                          var Done: Boolean); virtual;
    procedure FinalSign; virtual;
    procedure GenerateSignature(AContent: TStream);
    procedure GenerateCounterSignature;
    procedure ImposeCertificates;
    procedure ImposeAttributes(SignerInfo: TASN1Struct;
                               AContentCopy: TStream = nil;
                               CounterSign: Boolean = False); virtual;
    procedure InitSign; virtual;
    procedure LoadContents(AContent: TStream); virtual;
    procedure ResetFileStream; virtual;
    // Common:
    procedure CalcContentDigest(AContent: TStream; AContentCopy: TStream = nil);
    procedure CreateSigStruct; virtual;
    property ContentOnly: Boolean read FContentOnly write SetContentOnly;
    property HashClass: THashClass read FHashClass write SetHashClass;
    property SigStruct: TASN1Struct read FSigStruct;
    property DetachedSignature: Boolean read FDetachedSignature write SetDetachedSignature;
    property OnGetDetachedMessage: TGetDetachedMessageEvent read FOnGetDetachedMessage write SetOnGetDetachedMessage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CheckTrust(const FileName: string): Boolean;
    function SignData(Cert: TASN1Struct;
                      AContent, ADest: TStream): Boolean; overload;
    function CounterSignData(Cert: TASN1Struct;
                             AStream, ADest: TStream): Boolean; overload;
    function VerifySignedData(AStream, ADest: TStream): Boolean;
  protected
    // To be published:
    property ExtKeyCompatibility: Boolean read FExtKeyCompatibility write SetExtKeyCompatibility;
    property IncludeRootCertificate: Boolean read FIncludeRootCertificate write SetIncludeRootCertificate;
    property SigningDescription: WideString read FSigningDescription write SetSigningDescription;
    property MD5Override: Boolean read FMD5Override write SetMD5Override;
    property IncludeSigningTime: Boolean read FIncludeSigningTime write SetIncludeSigningTime;
    property OnAddSigner: TAddCertificateEvent read FOnAddSigner write SetOnAddSigner;
    property OnBadSignature: TSignatureEvent read FOnBadSignature write SetOnBadSignature;
    property OnGoodSignature: TSignatureEvent read FOnGoodSignature write SetOnGoodSignature;
    property OnGoodCounterSignature: TCounterSignatureEvent read FOnGoodCounterSignature write SetOnGoodCounterSignature;
  end;

  TMPSignedData = class(TCustomMPSignedData)
  public
    // Run-time result values:
    property CertificateNotFound;
    property ContentOnly;
    property IllegalFormat;
  published
    property AllowExpiredCertificates;
    property ExtKeyCompatibility;
    property IncludeRootCertificate;
    property IncludeSigningTime;
    property SMIMECapabilities;
    // Per-message values:
    property ContentType;
    property SigningDescription;
    property DetachedSignature;
    // Associated components:
    property CollectedCertificates;
    property KeysAndCerts;
    property MyCertificates;
    property PrivateKeyRing;
    // Events:
    property OnAddSigner;
    property OnBadSignature;
    property OnGoodSignature;
    property OnGoodCounterSignature;
    property OnGetDetachedMessage;
  end;

  TCustomMPEnvelopedData = class(TBaseMPCMSComponent)
  private
    FEnvStruct: TASN1Struct;
    FCipherClass: TCipherClass;
    FContentEncryptionKey: ISecretKey;
    FKeyLength: Integer;
    FKeyedIV: Boolean;
    FIV: ISecretKey;
    FOnAddRecipient: TAddCertificateEvent;
    FRecipientEMailList: TStrings;
    FOnRecipientCertNotFound: TEmailEvent;
    FContentOnly: Boolean;
    procedure SetCipherClass(const Value: TCipherClass);
    procedure SetOnAddRecipient(const Value: TAddCertificateEvent);
    procedure SetOnRecipientCertNotFound(const Value: TEmailEvent);
    procedure SetContentOnly(const Value: Boolean);
  protected
    // Decryption:
    procedure ExtractCertificates; virtual;
    procedure ExtractContentEncryptionAlgorithm;
    procedure Final(AContent: TStream); virtual;
    procedure Init; virtual;
    function LoadEnvStruct: Boolean; virtual;
    function DecryptKey: Boolean;
    // Encryption:
    procedure DoAddRecipient(Index: Integer;
                             var Cert: TCertificate;
                             var Done: Boolean); virtual;
    procedure FinalEnv; virtual;
    procedure ImposeCertificates; virtual;
    procedure ImposeRecipient;
    procedure InitEnv; virtual;
    procedure LoadContents(AContent: TStream); virtual;
    // Common:
    procedure CreateEnvStruct; virtual;
    property CipherClass: TCipherClass read FCipherClass write SetCipherClass;
    property ContentOnly: Boolean read FContentOnly write SetContentOnly;
    property EnvStruct: TASN1Struct read FEnvStruct;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Decrypt(AStream, ADest: TStream): Boolean;
    function Encrypt(Cert: TASN1Struct; AContent, ADest: TStream): Boolean; overload;
    function Encrypt(ARecipientEMail: string; AContent, ADest: TStream): Boolean; overload;
    function Encrypt(ARecipientEMailList: TStrings; AContent, ADest: TStream): Boolean; overload;
  protected
    property OnAddRecipient: TAddCertificateEvent read FOnAddRecipient write SetOnAddRecipient;
    property OnRecipientCertNotFound: TEmailEvent read FOnRecipientCertNotFound write SetOnRecipientCertNotFound;
  end;

  TMPEnvelopedData = class(TCustomMPEnvelopedData)
  public
    // Run-time result values:
    property CertificateNotFound;
    property ContentOnly;
    property IllegalFormat;
  published
    property SMIMECapabilities;
    // Per-message values:
    property ContentType;
    // Associated components:
    property CollectedCertificates;
    property KeysAndCerts;
    property MyCertificates;
    property PrivateKeyRing;
    // Events:
    property OnAddRecipient;
    property OnRecipientCertNotFound;
  end;

  TContentTypeArray = array of TPKCS7DataType;

  TCustomMPCMSBroker = class(TBaseMPCMSComponent)
  private
    FEnvelopedData: TCustomMPEnvelopedData;
    FSignedData: TCustomMPSignedData;
    FOnAddSigner: TAddCertificateEvent;
    FOnAddRecipient: TAddCertificateEvent;
    FActionDescriptions: TStrings;
    FIdx: Integer;
    procedure SetEnvelopedData(const Value: TCustomMPEnvelopedData);
    procedure SetSignedData(const Value: TCustomMPSignedData);
    procedure SetOnAddRecipient(const Value: TAddCertificateEvent);
    procedure SetOnAddSigner(const Value: TAddCertificateEvent);
  protected
    procedure AddCertName(Cert: TCertificate);
    function DecodeContent(Action: TPKCS7DataType;
                           AStream, ADest: TStream;
                           var Actions: TContentTypeArray): Boolean;
    procedure DoChangeComponentProperty; override;
    procedure DoGoodCounterSignature(OuterCert: TCertificate;
                                     Cert: TCertificate;
                                     SigningTime: TDateTime);
    procedure DoGoodSignature(Cert: TCertificate);
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function Decode(AStream, ADest: TStream;
                    var Actions: TContentTypeArray): Boolean; overload;
    function Decode(AStream, ADest: TStream;
                    ActionDescriptions: TStrings): Boolean; overload;
    function Encode(Actions: TContentTypeArray;
                    AStream, ADest: TStream;
                    DataType: TPKCS7DataType = pData): Boolean; overload;
  protected
    // To be published:
    property EnvelopedData: TCustomMPEnvelopedData read FEnvelopedData write SetEnvelopedData;
    property SignedData: TCustomMPSignedData read FSignedData write SetSignedData;
    property OnAddRecipient: TAddCertificateEvent read FOnAddRecipient write SetOnAddRecipient;
    property OnAddSigner: TAddCertificateEvent read FOnAddSigner write SetOnAddSigner;
  end;

  TMPCMSBroker = class(TCustomMPCMSBroker)
  published
    property AllowExpiredCertificates;
    property SMIMECapabilities;
    // Associated components:
    property CollectedCertificates;
    property KeysAndCerts;
    property MyCertificates;
    property PrivateKeyRing;
    property EnvelopedData;
    property SignedData;
    property OnAddRecipient;
    property OnAddSigner;
  end;

function MakeContentTypeArray(Actions: array of TPKCS7DataType): TContentTypeArray;

implementation

uses
  SsBase64, ReadStrm, Pkix, X509Base, MpPK, MpIF, MpDL, MpEC, MpYarrow;

function MakeContentTypeArray(Actions: array of TPKCS7DataType): TContentTypeArray;
var
  I: Integer;
begin
  SetLength(Result,High(Actions) - Low(Actions) + 1);
  for I := Low(Actions) to High(Actions) do
    Result[I - Low(Actions)] := Actions[I];
end;

{ TBaseMPCMSComponent }

constructor TBaseMPCMSComponent.Create(AOwner: TComponent);
begin
  inherited;
  FLock := TCriticalSection.Create;
  FContentType := pData;
  FAllowExpiredCertificates := True;
  FSMIMECapabilities := TSMIMECapabilities.Create(Self,TSMIMECapabilityItem);
  FillSMIMECapabilities;                                                     
  FCollectedSMIMECapabilities := TSMIMECapabilities.Create(Self,TSMIMECapabilityItem);
end;

destructor TBaseMPCMSComponent.Destroy;
begin
  FLock.Free;
  FCert.Free;
  FSMIMECapabilities.Free;
  FCollectedSMIMECapabilities.Free;
  FFile.Free;
  inherited;
end;

procedure TBaseMPCMSComponent.DoChangeComponentProperty;
begin
  // Implement in descendants
end;

procedure TBaseMPCMSComponent.FillSMIMECapabilities;
var
  SL: TStringList;
  I: Integer;
  CC: TCipherClass;
  vKeySize: Integer;
  vKeyedIV: Boolean;
begin
  SL := TStringList.Create;
  try
    try
      SecUtils.GetSMIMECapabilities(SL);
      for I := 0 to SL.Count - 1 do begin
        try
          vKeySize := Integer(SL.Objects[I]);
          vKeyedIV := False;
          CC := SecUtils.FindCipherClass(SL[I],vKeySize,vKeyedIV);
          if Assigned(CC) then begin
            with SMIMECapabilities.Add as TSMIMECapabilityItem do begin
              CipherAlg := CC.Algorithm;
              CipherMode := CC.Mode;
              KeyLength := vKeySize;
              KeyedIV := vKeyedIV;
            end;
          end;
        except
        end;
      end;
    except
    end;
  finally
    SL.Free;
  end;
end;

procedure TBaseMPCMSComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FCollectedCertificates then
      FCollectedCertificates := nil;
    if AComponent = FMyCertificates then
      FMyCertificates := nil;
    if AComponent = FPrivateKeyRing then
      FPrivateKeyRing := nil;
    if AComponent = FKeysAndCerts then
      SetKeysAndCerts(nil);
  end;
end;

procedure TBaseMPCMSComponent.SetAllowExpiredCertificates(
  const Value: Boolean);
begin
  FAllowExpiredCertificates := Value;
end;

procedure TBaseMPCMSComponent.SetCollectedCertificates(
  const Value: TX509TrustedCertificates);
begin
  FCollectedCertificates := Value;
  if Assigned(Value) then begin
    FreeNotification(Value);
    DoChangeComponentProperty;
  end;
end;

procedure TBaseMPCMSComponent.SetCollectedSMIMECapabilities(
  const Value: TSMIMECapabilities);
begin
  FCollectedSMIMECapabilities := Value;
end;

procedure TBaseMPCMSComponent.SetContentType(const Value: TPKCS7DataType);
begin
  FContentType := Value;
end;

type
  THack = class(TCustomTLSInternalServer);

procedure TBaseMPCMSComponent.SetIllegalFormat(Value: Boolean);
begin
  FIllegalFormat := Value;
end;

procedure TBaseMPCMSComponent.SetKeysAndCerts(
  const Value: TCustomTLSInternalServer);
begin
  if FKeysAndCerts <> Value then begin
    if Assigned(FKeysAndCerts) then begin
      if THack(FKeysAndCerts).StreamSecIIComp = PrivateKeyRing then
        PrivateKeyRing := nil;
      if THack(FKeysAndCerts).MyCerts = MyCertificates then
        MyCertificates := nil;
      if THack(FKeysAndCerts).TrustedCerts = CollectedCertificates then
        CollectedCertificates := nil;
    end;
    FKeysAndCerts := Value;
    if Assigned(Value) then begin
      FreeNotification(Value);
      PrivateKeyRing := THack(Value).StreamSecIIComp;
      MyCertificates := THack(Value).MyCerts;
      CollectedCertificates := THack(Value).TrustedCerts;
      DoChangeComponentProperty;
    end;
  end;
end;

procedure TBaseMPCMSComponent.SetMyCertificates(
  const Value: TX509TrustedCertificates);
begin
  FMyCertificates := Value;
  if Assigned(Value) then begin
    FreeNotification(Value);
    DoChangeComponentProperty;
  end;
end;

procedure TBaseMPCMSComponent.SetPrivateKeyRing(const Value: TStreamSecII);
begin
  FPrivateKeyRing := Value;
  if Assigned(Value) then begin
    FreeNotification(Value);   
    DoChangeComponentProperty;
  end;
end;

procedure TBaseMPCMSComponent.SetSMIMECapabilities(
  const Value: TSMIMECapabilities);
begin
  if Value = nil then
    FSMIMECapabilities.Clear
  else
    FSMIMECapabilities.Assign(Value);
end;

{ TCustomMPSignedData }

procedure TCustomMPSignedData.CalcContentDigest(AContent: TStream;
  AContentCopy: TStream);
var
  MS: TMemoryStream;
  Strm: TCustomMemoryStream;
  I: Integer;
  Struct, P: PASN1Struct;
  OID: ObjectIdentifier;
  Dummy: Integer;
begin
  MS := TSecureMemoryStream.Create;
  try
    Struct := FSigStruct.FindField('/content//encapContentInfo');
    P := Struct^.FindField('eContentType');
    OID := P^.ContentAsOID;
    if OID <> '' then OIDToContentType(OID,FContentType);

    P := Struct^.FindField('/eContent/');
    if P = nil then
      // Detached signature
      MS.Size := 0
    else if not P^.Constructed then
      MS.Write(P^.Content^,P^.Length)  // <--- The standard way
    else
      for I := 0 to P^.ItemCount - 1 do
        P^.Items[I].SaveToStream(MS,fmtDER); // <--- Early PKCS#7
    FDetachedSignature := MS.Size = 0;
    Dummy := SizeOf(FContentDigest);
    FillChar(FContentDigest,Dummy,0);
    if FDetachedSignature and Assigned(AContent) then begin
      AContent.Position := 0;
      if Assigned(AContentCopy) then begin
        AContentCopy.CopyFrom(AContent,0);
        AContent.Position := 0;
      end;
      DigestStream(FContentDigestAlg,AContent,@FContentDigest,Dummy);
    end else begin
      Strm := MS;
      if FDetachedSignature and Assigned(FOnGetDetachedMessage) then
        FOnGetDetachedMessage(Self,Strm);
      Strm.Position := 0;
      if Assigned(AContentCopy) then begin
        AContentCopy.CopyFrom(Strm,0);
        Strm.Position := 0;
      end;
      DigestStream(FContentDigestAlg,Strm,@FContentDigest,Dummy);
    end;
  finally
    MS.Free;
  end;
end;

function TCustomMPSignedData.CheckTrust(const FileName: string): Boolean;
var
  F: TStream;
begin
  Result := FileExists(FileName);
  if Result then begin
    F := TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
    try
      Result := VerifySignedData(F,nil);
    finally
      F.Free;
    end;
  end;
end;

function TCustomMPSignedData.CounterSignData(Cert: TASN1Struct; AStream,
  ADest: TStream): Boolean;     
var
  vCert: TCertificate;
  Done: Boolean;
begin
  FLock.Acquire;
  try
    FFile := AStream;
    try
      Init; // <-- TMPSignCode will check the PE format here.
      Result := not FIllegalFormat;
      if Result then begin
        FContentOnly := False;
        Result := LoadSigStruct; // <-- TMPSignCode will check the PE digest here.
        if Result then begin
            Result := True;
            vCert := FCert;
            FCert := nil;
            vCert.Free;
            FCert := TCertificate.Create(nil,nil);
            if Assigned(Cert) then
              FCert.AssignStruct(Cert)
            else begin
              Done := True;
              DoAddSigner(0,FCert,Done);
              Result := Assigned(FCert) and (FCert.Data.Length > 0);
            end;
            if Result then begin
              FFile := ADest;
              try
                InitSign;
                Result := not FIllegalFormat;
                if Result then begin
                  ImposeCertificates;
                  GenerateCounterSignature;
                  Result := not FCertificateNotFound;
                  if Result then
                    FinalSign;
                end;
              finally
                FFile := nil;
              end;
            end;
        end;
      end;
    finally
      FFile := nil;
    end;
  finally
    FLock.Release;
  end;
end;

constructor TCustomMPSignedData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF SHA1}
  FHashClass := TSHA1;
{$ENDIF SHA1}
  FIncludeSigningTime := True;
end;

procedure TCustomMPSignedData.CreateSigStruct;
begin
  NewSignedDataStruct(FSigStruct);
  FSigStruct._AddRef;
  {$IFDEF ALLOW_CMS_3}
  if (FCert = nil) or
     (X509Base.ExtractSubjectKeyIdentifier(FCert.Data) = '') then
  {$ENDIF ALLOW_CMS_3}
    FSigStruct.EditField('/content//version',Integer(1))
  {$IFDEF ALLOW_CMS_3}
  else
    FSigStruct.EditField('/content//version',Integer(3));
  {$ENDIF ALLOW_CMS_3}
end;

destructor TCustomMPSignedData.Destroy;
begin
  FSigStruct.Free;
  inherited;
end;

procedure TCustomMPSignedData.DoAddSigner(Index: Integer;
  var Cert: TCertificate; var Done: Boolean);
begin
  if Assigned(FOnAddSigner) then
    FOnAddSigner(Self,Index,Cert,Done);
end;

procedure TCustomMPSignedData.DoGoodCounterSignature(OuterCert,
  Cert: TCertificate);
begin
  if Assigned(FOnGoodCounterSignature) then
    FOnGoodCounterSignature(Self,OuterCert,Cert,FSigningTime);
  if Assigned(FBroker) then
    FBroker.DoGoodCounterSignature(OuterCert,Cert,FSigningTime);
end;

procedure TCustomMPSignedData.DoGoodSignature(Cert: TCertificate);
begin
  if Assigned(FOnGoodSignature) then
    FOnGoodSignature(Self,Cert);
  if Assigned(FBroker) then
    FBroker.DoGoodSignature(Cert);
end;

procedure TCustomMPSignedData.ExtractCertificates;
var
  Status: TCertStatusCodes;
begin
  if Assigned(FCollectedCertificates) then
    FCollectedCertificates.ImportFromCMS(FSigStruct,
                                         False,
                                         Status,
                                         FAllowExpiredCertificates);
end;

procedure TCustomMPSignedData.Final(AContent: TStream);
var
  P: PASN1Struct;
begin
  P := FSigStruct.FindField('/content//encapContentInfo/eContent/');
  if P^.ActualTag = V_ASN1_OCTET_STRING then
    AContent.Write(P^.Content^,P^.Length)  // <--- The standard way
end;

procedure TCustomMPSignedData.FinalSign;
begin
  if Assigned(FFile) then begin
    ResetFileStream;
    if FContentOnly then
      FSigStruct.FindField('/content/').SaveToStream(FFile,fmtDER)
    else
      FSigStruct.SaveToStream(FFile,fmtDER);
  end;
end;

procedure TCustomMPSignedData.GenerateCounterSignature;
var
  P, E, A, F, G, S, U: PASN1Struct;
  {$IFDEF ALLOW_CMS_3}
  PKI: string;
  {$ENDIF ALLOW_CMS_3}
  OID, Serial, Sign: string;
  Priv: IMPPrivateKey;
  Res: Integer;
  ContentCopy: TSecureMemoryStream;
  Dummy, I: Integer;
  OuterContentType: TPKCS7DataType;
begin
  OuterContentType := FContentType;
  ContentCopy := TSecureMemoryStream.Create;
  try
    FDetachedSignature := True;
    FContentType := pData; // <-- This is actually not what the standard says
    P := FSigStruct.FindField('/content//signerInfos');

    for I := 0 to P^.ItemCount - 1 do begin
      S := P^.Items[I];
      ContentCopy.Clear;
      E := S^.FindField('signature');
      ContentCopy.Write(E.Content^,E.Length);
      ContentCopy.Position := 0;
      U := S^.FindField('unsignedAttrs')^.AddField;
      U^.Items[0].EditContent(pkcs_9_at_counterSignature);
      A := U^.Items[1].AddField;
      A^.TypeIdentify;

      {$IFDEF ALLOW_CMS_3}
      PKI := ExtractSubjectKeyIdentifier(FCert.Data);
      if PKI = '' then
      {$ENDIF ALLOW_CMS_3}
        A^.EditField('version',Integer(1))
      {$IFDEF ALLOW_CMS_3}
      else
        A^.EditField('version',Integer(3))
      {$ENDIF ALLOW_CMS_3}
      ;

      OID := HashAlgorithmToOID(FContentDigestAlg);
      {
      // This part is not necessary, since FContentDigestAlg will be the same
      // as the one used by the outer signature.
      D := FSigStruct.FindField('/content//digestAlgorithms')^.AddField;
      D^.EditField('algorithm',OID);
      }
      A^.EditField('/digestAlgorithm/algorithm',OID);
      Dummy := SizeOf(FContentDigest);
      DigestStream(FContentDigestAlg,ContentCopy,@FContentDigest,Dummy);
      ImposeAttributes(A^,ContentCopy,True);

      F := A^.FindField('sid');
      {$IFDEF ALLOW_CMS_3}
      if PKI = '' then begin
      {$ENDIF ALLOW_CMS_3}
        F^.TrySelectChoice(V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
        ExtractIssuerStruct(FCert.Data,G);
        F^.FindField('issuer')^.Assign(G^);
        ExtractSerial(FCert.Data,Serial);
        F^.FindField('serialNumber')^.SetContent(Pointer(Serial)^,Length(Serial));
      {$IFDEF ALLOW_CMS_3}
      end else begin
        F^.TrySelectChoice(V_ASN1_CONTEXT_SPECIFIC,True,0);
        F^.Items[0]^.SetContent(Pointer(PKI)^,Length(PKI));
      end;
      {$ENDIF ALLOW_CMS_3}

      Assert(Assigned(FPrivateKeyRing));

      Priv := FPrivateKeyRing.FindCreatePrivateKey(FCert.Data);

      if Priv = nil then
        FCertificateNotFound := True
      else begin
        Priv.PublKey := FCert;
        try
          A^.EditField('/signatureAlgorithm/algorithm',Priv.Algorithm);
          SetLength(Sign,Priv.SignatureLength);
          Res := Priv.SignDig(FContentDigest,FHashClass.DigestSize,Pointer(Sign)^,Length(Sign),haNull);
          if Res <= 0 then
            FCertificateNotFound := True
          else
            A^.FindField('signature')^.SetContent(Pointer(Sign)^,Res);
        finally
          Priv.PublKey := nil;
        end;
      end;
      A^.CalculateLength;
      FSigStruct.CalculateLength;
    end;
  finally
    FContentType := OuterContentType;
    ContentCopy.Free;
  end;
end;

procedure TCustomMPSignedData.GenerateSignature;
var
  P, A, D, F, G: PASN1Struct;
  {$IFDEF ALLOW_CMS_3}
  PKI: string;
  {$ENDIF ALLOW_CMS_3}
  OID, Serial, Sign: string;
  Priv: IMPPrivateKey;
  Res: Integer;
  ContentCopy: TSecureMemoryStream;
begin
  if ExtKeyCompatibility then
    ContentCopy := TSecureMemoryStream.Create
  else
    ContentCopy := nil;
  try
    P := FSigStruct.FindField('/content//signerInfos');

    A := P^.AddField;

    {$IFDEF ALLOW_CMS_3}
    PKI := ExtractSubjectKeyIdentifier(FCert.Data);
    if PKI = '' then
    {$ENDIF ALLOW_CMS_3}
      A^.EditField('version',Integer(1))
    {$IFDEF ALLOW_CMS_3}
    else
      A^.EditField('version',Integer(3))
    {$ENDIF ALLOW_CMS_3}
    ;

    OID := HashAlgorithmToOID(FContentDigestAlg);
    D := FSigStruct.FindField('/content//digestAlgorithms')^.AddField;
    D^.EditField('algorithm',OID);
    A^.EditField('/digestAlgorithm/algorithm',OID);
    CalcContentDigest(AContent,ContentCopy);
    ImposeAttributes(A^,ContentCopy);

    F := A^.FindField('sid');
    {$IFDEF ALLOW_CMS_3}
    if (FContentType = pNone) or (PKI = '') then begin
    {$ENDIF ALLOW_CMS_3}
      F^.TrySelectChoice(V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      ExtractIssuerStruct(FCert.Data,G);
      F^.FindField('issuer')^.Assign(G^);
      ExtractSerial(FCert.Data,Serial);
      F^.FindField('serialNumber')^.SetContent(Pointer(Serial)^,Length(Serial));
    {$IFDEF ALLOW_CMS_3}
    end else begin
      F^.TrySelectChoice(V_ASN1_CONTEXT_SPECIFIC,True,0);
      F^.Items[0]^.SetContent(Pointer(PKI)^,Length(PKI));
    end;
    {$ENDIF ALLOW_CMS_3}

    Assert(Assigned(FPrivateKeyRing));

    Priv := FPrivateKeyRing.FindCreatePrivateKey(FCert.Data);

    if Priv = nil then
      FCertificateNotFound := True
    else begin
      Priv.PublKey := FCert;
      try
        A^.EditField('/signatureAlgorithm/algorithm',Priv.Algorithm);
        SetLength(Sign,Priv.SignatureLength);
        if Assigned(ContentCopy) then
          Res := Priv.SignBuf(ContentCopy.Memory^,ContentCopy.Size,Pointer(Sign)^,Length(Sign))
        else
          Res := Priv.SignDig(FContentDigest,FHashClass.DigestSize,Pointer(Sign)^,Length(Sign),FContentDigestAlg);
        if Res <= 0 then
          FCertificateNotFound := True
        else
          A^.FindField('signature')^.SetContent(Pointer(Sign)^,Res);
      finally
        Priv.PublKey := nil;
      end;
    end;
  finally
    ContentCopy.Free;
  end;
end;

procedure TCustomMPSignedData.ImposeAttributes(SignerInfo: TASN1Struct;
  AContentCopy: TStream; CounterSign: Boolean);
var
  F, G, H, E: PASN1Struct;
  Dummy: Integer;
  S: string;
  I: Integer;
  OID: string;
  CC: TCipherClass;
  HA: THashAlgorithm;
begin
  // IMPORTANT:
  // If this method is overridden, any additional signed attributes must be
  // added before the call to the inherited method.

  F := SignerInfo.FindField('signedAttrs');

  if CounterSign or
     (FContentType <> pData) or
     FDetachedSignature or
     FIncludeSigningTime or
     (FSigningDescription <> '') or
     not F^.IsEmpty then begin

    if FContentType <> pNone then begin
      G := F^.AddField;
      G^.EditField('attrType',pkcs_9_at_contentType);
      G := G^.Items[1].AddField;
      G^.TypeIdentify;
      G^.EditContent(ContentTypeToOID(FContentType));
    end;

    if FIncludeSigningTime then begin
      G := F^.AddField;
      G^.EditField('attrType',pkcs_9_at_signingTime);
      G := G^.Items[1].AddField;
      G^.TypeIdentify;
      G^.TrySelectChoice(V_ASN1_UNIVERSAL,False,V_ASN1_UTCTIME);
      G^.EditContent(Now - FMyCertificates.HoursOffsetFromGMT / 24);
    end;

    G := F^.AddField;
    G^.EditField('attrType',pkcs_9_at_messageDigest);
    G := G^.Items[1].AddField;
    G^.TypeIdentify;
    G^.SetContent(FContentDigest,FHashClass.DigestSize);

    if FSigningDescription <> '' then begin
      G := F^.AddField;
      G^.EditField('attrType',pkcs_9_at_signingDescription);
      G := G^.Items[1].AddField;
      G^.TypeIdentify;
      G^.TrySelectChoice(V_ASN1_UNIVERSAL,False,V_ASN1_UTF8STRING);
      G^.EditContent(FSigningDescription);
    end;

    // S/MIME capabilities:
    if (FDetachedSignature or (FSMIMECapabilities.Count > 0))
       and not (CounterSign or (ContentType in [pAuthenticode,pNone])) then begin
      G := F^.AddField;
      G^.EditField('attrType',pkcs_9 + '.15');
      for I := 0 to FSMIMECapabilities.Count - 1 do
        if FSMIMECapabilities.Items[I].GetIsValid(CC) then begin
          OID := Trim(FSMIMECapabilities.Items[I].OID);
          if OID <> '' then begin
            H := G^.Items[1].AddField;
            H^.TypeName := 'SEQUENCE';
            E := H^.AddField('','OBJECT',nil);
            E^.EditContent(OID);
{$IFDEF ARCTWO}
            if CC.InheritsFrom(TARC2_ECB) then begin
              H := H^.AddField('','SEQUENCE',nil);
              E := H^.AddField('','INTEGER',nil);
              if FSMIMECapabilities.Items[I].KeyLength < 32 then
                E^.EditContent(Integer(EKBTable[FSMIMECapabilities.Items[I].KeyLength*8]))
              else
                E^.EditContent(Integer(FSMIMECapabilities.Items[I].KeyLength*8));
            end;
{$ENDIF ARCTWO}
            H^.AddField('','OCTET STRING',nil);
          end;
        end;
      if FDetachedSignature then
        for HA := Low(HA) to High(HA) do begin
          OID := HashAlgorithmToOID(HA);
          if OID <> '' then begin
            H := G^.Items[1].AddField;
            H^.TypeName := 'SEQUENCE';
            H:= H^.AddField('','OBJECT',nil);
            H^.EditContent(OID);
          end;
        end;
    end;

//    F^.SortSET;

    F.CalculateLength;
    S := F^.ContentAsOctetString;
    S[1] := Char(V_ASN1_UNIVERSAL or V_ASN1_CONSTRUCTED or V_ASN1_SET);
    Dummy := SizeOf(FContentDigest);
    if Assigned(AContentCopy) then begin
      AContentCopy.Size := 0;
      AContentCopy.Write(Pointer(S)^,Length(S));
    end;
    Dummy := SizeOf(FContentDigest);
    DigestString(FContentDigestAlg,S,@FContentDigest,Dummy);
  end;
end;

procedure TCustomMPSignedData.ImposeCertificates;
var
  Idx: Integer;
begin
  if Assigned(FMyCertificates) then begin
    Idx := FMyCertificates.IndexOfCert(FCert.Data);
    if Idx >= 0 then
      FMyCertificates.ExportChainToCMS(FSigStruct,Idx,False,FIncludeRootCertificate);
  end;
end;

procedure TCustomMPSignedData.Init;
begin
  FIllegalFormat := False;
  FCertificateNotFound := False;
end;

procedure TCustomMPSignedData.InitSign;
var
  Priv: IMPPrivateKey;
begin
  FIllegalFormat := False;
  FCertificateNotFound := False;
{$IFNDEF DISABLEMD5SIGN}
  if FMD5Override then
    FContentDigestAlg := haMD5
  else begin
{$ENDIF DISABLEMD5SIGN}
    Priv := FPrivateKeyRing.FindCreatePrivateKey(FCert.Data);
    if Assigned(Priv) then
      FContentDigestAlg := Priv.SignatureDigestAlg;
{$IFNDEF DISABLEMD5SIGN}
  end;
{$ENDIF DISABLEMD5SIGN}
  FHashClass := FindHashClass(FContentDigestAlg);
end;

procedure TCustomMPSignedData.LoadContents(AContent: TStream);
var
  P: PASN1Struct;
  Cnt: string;
begin
  if Assigned(FSigStruct) then FSigStruct._Release;
  FSigStruct := nil;
  CreateSigStruct;
  ImposeCertificates;
  if Assigned(AContent) or FDetachedSignature then begin
    if FContentType <> pNone then begin
      P := FSigStruct.FindField('/content//encapContentInfo/eContentType');
      P^.EditContent(Cms.ContentTypeToOID(FContentType));
    end;
    P := FSigStruct.FindField('/content//encapContentInfo/eContent/');
    if FDetachedSignature then
      P^.Length := 0
    else begin
      SetLength(Cnt,AContent.Size);
      AContent.Read(Pointer(Cnt)^,Length(Cnt));
      P^.SetContent(Pointer(Cnt)^,Length(Cnt));
    end;
  end;
end;

function TCustomMPSignedData.LoadSigStruct: Boolean;
var
  OldPos: Int64;
  B: Byte;
  Buf: string;
  Strm: TStream;
begin
  Result := False;
  if Assigned(FSigStruct) then FSigStruct._Release;
  FSigStruct := nil;
  CreateSigStruct;
  try
    OldPos := FFile.Position;
    B := 0;
    FFile.Read(B,1);
    FFile.Position := OldPos;
    Strm := FFile;
    try
      if B = Ord('M') then begin
        SetLength(Buf,FFile.Size - OldPos);
        FFile.Read(Pointer(Buf)^,Length(Buf));
        Strm := TStringStream.Create(MIME64ToStr(Buf));
      end else
        while B <> 48 do begin
          if FFile <> Strm then
            Strm.Free;
          FFile.Position := OldPos + 1;
          repeat
            OldPos := FFile.Position;
            if FFile.Read(B,1) < 1 then
              raise Exception.Create('Unable to decode input data');
          until B in [48,Ord('M')];
          FFile.Position := OldPos;
          if B = Ord('M') then begin
            SetLength(Buf,FFile.Size - OldPos);
            FFile.Read(Pointer(Buf)^,Length(Buf));
            Strm := TStringStream.Create(MIME64ToStr(Buf));
            if Copy(TStringStream(Strm).DataString,1,1) = Chr(48) then
              B := 48;
          end else
            Strm := FFile;
        end;
      if FContentOnly then begin
        FSigStruct.FindField('/content/').ReadOnly := True;
        FSigStruct.FindField('/content/').LoadFromStream(Strm,fmtDER);
      end else begin
        FSigStruct.ReadOnly := True;
        FSigStruct.LoadFromStream(Strm,fmtDER);
      end;
      ExtractCertificates;
      Result := True;
    finally
      if Strm <> FFile then
        Strm.Free;
    end;
  except
    Result := False;
  end;
end;

procedure TCustomMPSignedData.ResetFileStream;
begin
  FFile.Position := FOrgFilePosition;
end;

procedure TCustomMPSignedData.SetContentOnly(const Value: Boolean);
begin
  FContentOnly := Value;
end;

procedure TCustomMPSignedData.SetDetachedSignature(const Value: Boolean);
begin
  FDetachedSignature := Value;
end;

procedure TCustomMPSignedData.SetExtKeyCompatibility(const Value: Boolean);
begin
  FExtKeyCompatibility := Value;
end;

procedure TCustomMPSignedData.SetHashClass(const Value: THashClass);
begin
  FHashClass := Value;
end;

procedure TCustomMPSignedData.SetIncludeRootCertificate(
  const Value: Boolean);
begin
  FIncludeRootCertificate := Value;
end;

procedure TCustomMPSignedData.SetIncludeSigningTime(const Value: Boolean);
begin
  FIncludeSigningTime := Value;
end;

procedure TCustomMPSignedData.SetMD5Override(const Value: Boolean);
begin
  FMD5Override := Value;
end;

procedure TCustomMPSignedData.SetOnAddSigner(
  const Value: TAddCertificateEvent);
begin
  FOnAddSigner := Value;
end;

procedure TCustomMPSignedData.SetOnBadSignature(
  const Value: TSignatureEvent);
begin
  FOnBadSignature := Value;
end;

procedure TCustomMPSignedData.SetOnGetDetachedMessage(
  const Value: TGetDetachedMessageEvent);
begin
  FOnGetDetachedMessage := Value;
end;

procedure TCustomMPSignedData.SetOnGoodCounterSignature(
  const Value: TCounterSignatureEvent);
begin
  FOnGoodCounterSignature := Value;
end;

procedure TCustomMPSignedData.SetOnGoodSignature(
  const Value: TSignatureEvent);
begin
  FOnGoodSignature := Value;
end;

procedure TCustomMPSignedData.SetSigningDescription(
  const Value: WideString);
begin
  FSigningDescription := Value;
end;

function TCustomMPSignedData.SignData(Cert: TASN1Struct; AContent,
  ADest: TStream): Boolean;
var
  vCert: TCertificate;
  Done: Boolean;
begin
  FLock.Acquire;
  try
    vCert := FCert;
    FCert := nil;
    vCert.Free;
    FCert := TCertificate.Create(nil,nil);

    Result := True;
    if Assigned(Cert) then
      FCert.AssignStruct(Cert)
    else begin
      Done := True;
      DoAddSigner(0,FCert,Done);
      Result := Assigned(FCert) and (FCert.Data.Length > 0);
    end;
    if Result then begin
      FFile := ADest;
      FOrgFilePosition := ADest.Position; 
      try
        InitSign; // <-- TMPSignCode will check the PE format here.
        Result := not FIllegalFormat;
        if Result then begin
          LoadContents(AContent);
          if Result then begin
            GenerateSignature(AContent);
            Result := not FCertificateNotFound;
            if Result then
              FinalSign;
          end;
        end;
      finally
        FFile := nil;
      end;
    end;
  finally
    FLock.Release;
  end;
end;

function TCustomMPSignedData.VerifyAttributes(SignerInfo: TASN1Struct;
                                              CounterSign: Boolean): Boolean;
var
  F, G, P: PASN1Struct;
  I, Dummy: Integer;
  S: string;
  OK1, OK2: Boolean;
begin
  FExtractedCapabilities := nil;
  Result := True;
  F := SignerInfo.FindField('signedAttrs');
  FSigningTime := 0;
  if not F^.IsEmpty then begin
    OK1 := False;
    OK2 := CounterSign;
    for I := 0 to F^.ItemCount - 1 do begin
      G := F^.Items[I];
      if G^.ItemCount > 1 then
        if (G^.Items[0].ActualTag = V_ASN1_OBJECT) and
           (G^.Items[0]^.ContentAsOID = pkcs_9_at_messageDigest) then begin
          G := G^.Items[1]^.Items[0];
          OK1 := (G^.Length = FHashClass.DigestSize) and
                 CompareMem(@FContentDigest,G^.Content,FHashClass.DigestSize);
        end else if (G^.Items[0].ActualTag = V_ASN1_OBJECT) and
                    (G^.Items[0]^.ContentAsOID = pkcs_9_at_contentType) then begin
          if CounterSign then
            OK2 := G^.Items[1]^.Items[0].ContentAsOID = id_data
          else begin
            G := G^.Items[1]^.Items[0];
            P := FSigStruct.FindField('/content//encapContentInfo/eContentType');
            OK2 := (G^.ContentAsOID = P^.ContentAsOID);
          end;
        end else if (G^.Items[0].ActualTag = V_ASN1_OBJECT) and
                    (G^.Items[0]^.ContentAsOID = pkcs_9 + '.15') then begin
          FExtractedCapabilities := G^.Items[1];
        end else if (G^.Items[0].ActualTag = V_ASN1_OBJECT) and
                    (G^.Items[0]^.ContentAsOID = pkcs_9 + '.5') then begin
          FSigningTime := G^.Items[1]^.Items[0].ContentAsDateTime;
        end;
    end;
    Result := OK1 and OK2;
    if Result then begin
      S := F^.ContentAsOctetString;
      S[1] := Char(V_ASN1_UNIVERSAL or V_ASN1_CONSTRUCTED or V_ASN1_SET);
      Dummy := SizeOf(FContentDigest);
      DigestString(FContentDigestAlg,S,@FContentDigest,Dummy);
    end;
  end;
end;

function TCustomMPSignedData.VerifySignature(AContent: TStream): Boolean;
var
  P, A, F, G: PASN1Struct;
  I: Integer;
  OID: string;
  vCert: TCertificate;
  PK: IMPPublicKey;
begin
  P := FSigStruct.FindField('/content//signerInfos');
  I := 0;
  Result := Assigned(FCollectedCertificates) and Assigned(P);
  while Result and (I < P^.ItemCount) do begin
    A := P^.Items[I];
    OID := A^.FindField('/digestAlgorithm/algorithm')^.ContentAsOID;
    Result := OIDToHashAlgorithm(OID,FContentDigestAlg);
    if Result then begin
      FHashClass := FindHashClass(FContentDigestAlg);
      CalcContentDigest(AContent);
      Result := VerifyAttributes(A^);
      if Result then begin
        F := A^.FindField('sid');
        G := nil;
        // By retrieving the certificate from the certificate store, we are
        // assured that it passed all tests when it was added to it:
        if F^.Tag = V_ASN1_SEQUENCE then
          FCollectedCertificates.FindCert(F^.FindField('serialNumber')^.ContentAsOctetString,
                                          F^.FindField('issuer')^,
                                          G)
        else if F^.Tag = 0 then
          FCollectedCertificates.FindCert(F^.Items[0]^.ContentAsOctetString,G);
        Result := Assigned(G) and not FCollectedCertificates.IsRevoked(G^);
        if Result then begin
          if Assigned(FExtractedCapabilities) then
            FCollectedCertificates.AddSMIMECapabilities(G^,FExtractedCapabilities^);
          vCert := FCert;
          FCert := nil;
          vCert.Free;
          FCert := TCertificate.Create(nil,nil);
          FCert.AssignStruct(G^);
          PK := CreateMPPublicKey(FCert);
          try
            Result := Assigned(PK);
            if Result then begin
              F := A^.FindField('signature');
              Result := Assigned(F);
              if Result and (F^.Length > 0) then begin
                PK.SignatureDigestAlg := FContentDigestAlg;
                Result := PK.VerDig(FContentDigest,FHashClass.DigestSize,
                                    F^.Content^,F^.Length);
              end else
                Result := False;
            end;
            if Result then begin
              DoGoodSignature(FCert);   
              VerifyUnsignedAttributes(A^);
            end else
              if Assigned(FOnBadSignature) then
                FOnBadSignature(Self,FCert);
          finally
            PK := nil;
          end;
        end else
          FCertificateNotFound := True;
      end;
    end;
    Inc(I);
  end;
end;

function TCustomMPSignedData.VerifySignedData(AStream, ADest: TStream): Boolean;
begin
  FLock.Acquire;
  try
    FFile := AStream;
    try
      Init; // <-- TMPSignCode will check the PE format here.
      Result := not FIllegalFormat;
      if Result then begin
        Result := LoadSigStruct; // <-- TMPSignCode will check the PE digest here.
        if Result then begin
          Result := VerifySignature(ADest);
          if Result and Assigned(ADest) and not DetachedSignature then
            Final(ADest);
        end;
      end;
    finally
      FFile := nil;
    end;
  finally
    FLock.Release;
  end;
end;

function TCustomMPSignedData.VerifyUnsignedAttributes(
  SignerInfo: TASN1Struct): Boolean;
var
  P, Q, A, F, G: PASN1Struct;
  I, Dummy: Integer;
  OID: string;
  OuterCert, vCert: TCertificate;
  PK: IMPPublicKey;
  OuterSignature: string;
begin
  OuterCert := FCert;
  try
    vCert := nil;
    OuterSignature := SignerInfo.FindField('signature').ContentAsOctetString;
    P := SignerInfo.FindField('unsignedAttrs');
    I := 0;
    Result := Assigned(FCollectedCertificates) and Assigned(P);
    while Result and (I < P^.ItemCount) do try
      Q := P^.Items[I];
      // Just checking for counter signatures:
      if Q^.Items[0].ContentAsOID <> pkcs_9_at_counterSignature then
        Continue;
      A := Q^.Items[1]^.Items[0];
      OID := A^.FindField('/digestAlgorithm/algorithm')^.ContentAsOID;
      Result := OIDToHashAlgorithm(OID,FContentDigestAlg);
      if Result then begin
        FHashClass := FindHashClass(FContentDigestAlg);
        Dummy := SizeOf(FContentDigest);
        DigestString(FContentDigestAlg,OuterSignature,@FContentDigest,Dummy);
        Result := VerifyAttributes(A^,True);
        if Result then begin
          F := A^.FindField('sid');
          G := nil;
          // By retrieving the certificate from the certificate store, we are
          // assured that it passed all tests when it was added to it:
          if F^.Tag = V_ASN1_SEQUENCE then
            FCollectedCertificates.FindCert(F^.FindField('serialNumber')^.ContentAsOctetString,
                                            F^.FindField('issuer')^,
                                            G)
          else if F^.Tag = 0 then
            FCollectedCertificates.FindCert(F^.Items[0]^.ContentAsOctetString,G);
          Result := Assigned(G) and not FCollectedCertificates.IsRevoked(G^);
          if Result then begin
            if Assigned(FExtractedCapabilities) then
              FCollectedCertificates.AddSMIMECapabilities(G^,FExtractedCapabilities^);
            vCert.Free;
            vCert := TCertificate.Create(nil,nil);
            vCert.AssignStruct(G^);
            PK := CreateMPPublicKey(vCert);
            try
              Result := Assigned(PK);
              if Result then begin
                F := A^.FindField('signature');
                Result := Assigned(F);
                if Result and (F^.Length > 0) then begin
                  PK.SignatureDigestAlg := haNull;
                  Result := PK.VerDig(FContentDigest,FHashClass.DigestSize,
                                      F^.Content^,F^.Length);
                end else
                  Result := False;
              end;
              if Result then begin
                DoGoodCounterSignature(OuterCert,vCert);
                FCert := vCert;
                VerifyUnsignedAttributes(A^);
              end else
                if Assigned(FOnBadSignature) then
                  FOnBadSignature(Self,vCert);
            finally
              PK := nil;
            end;
          end else
            FCertificateNotFound := True;
        end;
      end;
      Inc(I);
    finally
      vCert.Free;
      vCert := nil;
    end;
  finally
    FCert := OuterCert;
  end;
end;

{ TSMIMECapabilityItem }

procedure TSMIMECapabilityItem.AssignTo(Dest: TPersistent);
var
  D: TSMIMECapabilityItem;
begin
  if Dest is TSMIMECapabilityItem then begin
    D := TSMIMECapabilityItem(Dest);
    D.FCipherAlg := FCipherAlg;
    D.FCipherMode := FCipherMode;
    D.FKeyLength := FKeyLength;
    D.FKeyedIV := FKeyedIV;
    D.Changed(False);
  end else
    inherited;
end;

function TSMIMECapabilityItem.GetDisplayName: string;
var
  CC: TCipherClass;
begin
  if GetIsValid(CC) then begin
    Result := CC.AlgorithmName;
    Result := Result + ' (' + IntToStr(FKeyLength * 8) + ' Bits)';
    if KeyedIV then
      Result := Result + ' Keyed-IV';
    case CC.Mode of
      cmECB: Result := Result + ' ECB-Mode';
      cmCFB: Result := Result + ' CFB-Mode';
      cmCBC: Result := Result + ' CBC-Mode';
      cmOFB: Result := Result + ' OFB-Mode';
      cmCTR: Result := Result + ' CTR-Mode';
      cmABC: Result := Result + ' ABC-Mode';
      cmPCFB: Result := Result + ' PCFB-Mode';
      cmPipedPCFB: Result := Result + ' Piped PCFB-Mode';
    else
      Result := Result + ' (invalid mode)';
    end;
  end else
    Result := '(invalid)';
end;

function TSMIMECapabilityItem.GetIsValid(var CC: TCipherClass): Boolean;
begin
  CC := SecUtils.FindCipherClass(FCipherAlg,FCipherMode);
  Result := Assigned(CC);
  if Result and CC.InheritsFrom(TBlockCipher) then
    Result := (TBlockCipherClass(CC).MinKeySize <= FKeyLength) and
              (TBlockCipherClass(CC).MaxKeySize >= FKeyLength);
end;

function TSMIMECapabilityItem.GetOID: ObjectIdentifier;
var
  CC: TCipherClass;
begin
  CC := FindCipherClass(CipherAlg,CipherMode);
  if Assigned(CC) then begin
    if FKeyedIV then
      Result := CC.KeyedIVOID(FKeyLength)
    else
      Result := CC.OID(FKeyLength);
  end else
    Result := '';
end;

procedure TSMIMECapabilityItem.SetCipherAlg(const Value: TCipherAlg);
begin
  FCipherAlg := Value;
end;

procedure TSMIMECapabilityItem.SetCipherMode(const Value: TCipherMode);
begin
  FCipherMode := Value;
end;

procedure TSMIMECapabilityItem.SetKeyedIV(const Value: Boolean);
begin
  FKeyedIV := Value;
end;

procedure TSMIMECapabilityItem.SetKeyLength(const Value: Word);
begin
  FKeyLength := Value;
end;

procedure TSMIMECapabilityItem.SetOID(const Value: ObjectIdentifier);
begin
  Changed(False);
end;

{ TCustomMPEnvelopedData }

procedure TCustomMPEnvelopedData.CreateEnvStruct;
begin
  NewEnvelopedDataStruct(FEnvStruct);
  FEnvStruct._AddRef;
  // NOTE: CMSVersion = 0 here means:
  // * Key establishment through RSA key transport. Recipient cert must have a
  //   rsaEncryption subject public key.
  // * Only X.509 certificates.
  FEnvStruct.EditField('/content//version',Integer(0));
end;

function TCustomMPEnvelopedData.Decrypt(AStream, ADest: TStream): Boolean;
begin
  FLock.Acquire;
  try
    FFile := AStream;
    try
      Init;
      Result := not FIllegalFormat;
      if Result then begin
        Result := LoadEnvStruct; 
        if Result then begin
          Result := DecryptKey;
          if Result and Assigned(ADest) then
            Final(ADest);
        end;
      end;
    finally
      FFile := nil;
    end;
  finally
    FLock.Release;
  end;
end;

function TCustomMPEnvelopedData.DecryptKey: Boolean;
var
  P, A, F, G: PASN1Struct;
  I: Integer;
  BCC: TBlockCipherClass;
  PK: IMPPrivateKey;
begin
  P := FEnvStruct.FindField('/content//recipientInfos');
  I := 0;
  Result := Assigned(FMyCertificates);
  FCertificateNotFound := True;
  while FCertificateNotFound and (I < P^.ItemCount) do begin
    A := P^.Items[I];
    F := A^.FindField('rid');
    G := nil;
    if F^.Tag = V_ASN1_SEQUENCE then
      FMyCertificates.FindCert(F^.FindField('serialNumber')^.ContentAsOctetString,
                               F^.FindField('issuer')^,
                               G)
    else if F^.Tag = 0 then
      FMyCertificates.FindCert(F^.Items[0]^.ContentAsOctetString,G);
    Result := Assigned(G) and not FMyCertificates.IsRevoked(G^);
    if Result then begin
      PK := FPrivateKeyRing.FindCreatePrivateKey(G^);
      Result := Assigned(PK);
      if Result then begin
        F := A^.FindField('encryptedKey');
        Result := Assigned(F);
        if Result and (F^.Length > 0) then begin
          FContentEncryptionKey := TSecretKey.Create('');
          if FCipherClass.InheritsFrom(TBlockCipher) then
            BCC := TBlockCipherClass(FCipherClass)
          else
            BCC := nil;
          if FKeyedIV then begin
            FContentEncryptionKey.SetLength(FKeyLength + BCC.BlockVectorSize * BCC.BlockSize);
            FContentEncryptionKey.VectorSize := BCC.BlockVectorSize;
          end else
            FContentEncryptionKey.SetLength(FKeyLength);
          Result := PK.DecryptKeyTransport(F^.Content^,F^.Length,
                                           FContentEncryptionKey.Key^,
                                           FContentEncryptionKey.KeyLen);
        end;
      end;
      FCertificateNotFound := False;
    end;
    Inc(I);
  end;
end;

function TCustomMPEnvelopedData.Encrypt(Cert: TASN1Struct;
  AContent, ADest: TStream): Boolean;
var
  vCert: TCertificate;
  Done: Boolean;
begin
  FLock.Acquire;
  try
    FCert := TCertificate.Create(nil,nil);
    try
      Result := True;
      if Assigned(Cert) then
        FCert.AssignStruct(Cert)
      else begin
        Done := True;
        DoAddRecipient(0,FCert,Done);
        Result := Assigned(FCert) and (FCert.Data.Length > 0);
      end;
      if Result then begin
        FFile := ADest;
        try
          InitEnv;
          Result := not FIllegalFormat;
          if Result then begin
            LoadContents(AContent);
            if Result then begin
              ImposeRecipient;
              Result := not FCertificateNotFound;
              if Result then
                FinalEnv;
            end;
          end;
        finally
          FFile := nil;
        end;
      end;
    finally
      vCert := FCert;
      FCert := nil;
      vCert.Free;
    end;
  finally
    FLock.Release;
  end;
end;

function TCustomMPEnvelopedData.Encrypt(ARecipientEMail: string; AContent,
  ADest: TStream): Boolean;
var
  vCert: TCertificate;
  Idx: Integer;
  RCert: PASN1Struct;
begin
  FLock.Acquire;
  try
    FCert := TCertificate.Create(nil,nil);
    try
      Idx := 0;
      repeat
        RCert := nil;
        CollectedCertificates.FindCert([],[rsaEncryption],[keyEncipherment],[id_kp_emailProtection],Idx,RCert);
        Inc(Idx);
      until (RCert = nil) or X509Base.CheckEmailAddress(RCert^,ARecipientEMail);
      if RCert = nil then begin
        Result := False;
        FCertificateNotFound := True;
        if Assigned(FOnRecipientCertNotFound) then
          FOnRecipientCertNotFound(Self,ARecipientEmail);
      end else begin
        FCert.AssignStruct(RCert^);
        FFile := ADest;
        try
          InitEnv;
          Result := not FIllegalFormat;
          if Result then begin
            LoadContents(AContent);
            if Result then begin
              ImposeRecipient;
              Result := not FCertificateNotFound;
              if Result then
                FinalEnv;
            end;
          end;
        finally
          FFile := nil;
        end;
      end;
    finally
      vCert := FCert;
      FCert := nil;
      vCert.Free;
    end;
  finally
    FLock.Release;
  end;
end;

function TCustomMPEnvelopedData.Encrypt(ARecipientEMailList: TStrings;
  AContent, ADest: TStream): Boolean;
var
  vCert: TCertificate;
  Idx, I: Integer;
  RCert: PASN1Struct;
  EMail: string;
begin
  FLock.Acquire;
  try
    FCert := TCertificate.Create(nil,nil);
    try
      FRecipientEMailList.Assign(ARecipientEMailList);
      Idx := 0;
      repeat
        RCert := nil;
        CollectedCertificates.FindCert([],[rsaEncryption],[keyEncipherment],[id_kp_emailProtection],Idx,RCert);
        if Assigned(RCert) then begin
          EMail := X509Base.ExtractEmailAddress(RCert^);
          if EMail <> '' then begin
            I := FRecipientEMailList.IndexOf(EMail);
            if I > -1 then
              FRecipientEMailList.Objects[I] := RCert^;
          end;
        end;
        Inc(Idx);
      until RCert = nil;
      if Assigned(MyCertificates) then begin
        Idx := 0;
        repeat
          RCert := nil;
          MyCertificates.FindCert([],[rsaEncryption],[keyEncipherment],[id_kp_emailProtection],Idx,RCert);
          if Assigned(RCert) then begin
            EMail := X509Base.ExtractEmailAddress(RCert^);
            if EMail <> '' then begin
              I := FRecipientEMailList.IndexOf(EMail);
              if I > -1 then
                FRecipientEMailList.Objects[I] := RCert^;
            end;
          end;
          Inc(Idx);
        until RCert = nil;
      end;
      if Assigned(MyCertificates.TrustedCACertificates) then begin
        Idx := 0;
        repeat
          RCert := nil;
          MyCertificates.TrustedCACertificates.FindCert([],[rsaEncryption],[keyEncipherment],[id_kp_emailProtection],Idx,RCert);
          if Assigned(RCert) then begin
            EMail := X509Base.ExtractEmailAddress(RCert^);
            if EMail <> '' then begin
              I := FRecipientEMailList.IndexOf(EMail);
              if I > -1 then
                FRecipientEMailList.Objects[I] := RCert^;
            end;
          end;
          Inc(Idx);
        until RCert = nil;
      end;
      for I := FRecipientEMailList.Count - 1 downto 0 do
        if FRecipientEMailList.Objects[I] = nil then begin
          if Assigned(FOnRecipientCertNotFound) then
            FOnRecipientCertNotFound(Self,FRecipientEMailList[I]);
          FRecipientEMailList.Delete(I);
        end;
      if FRecipientEMailList.Count = 0 then begin
        Result := False;
        FCertificateNotFound := True;
      end else begin
        FCert.AssignStruct(TASN1Struct(FRecipientEMailList.Objects[0]));
        FFile := ADest;
        try
          InitEnv;
          Result := not FIllegalFormat;
          if Result then begin
            LoadContents(AContent);
            if Result then begin
              ImposeRecipient;
              Result := not FCertificateNotFound;
              if Result then
                FinalEnv;
            end;
          end;
        finally
          FFile := nil;
          FRecipientEMailList.Clear;
        end;
      end;
    finally
      vCert := FCert;
      FCert := nil;
      vCert.Free;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TCustomMPEnvelopedData.ExtractCertificates;
begin
  // Implement extraction of DH and ECDH certificates (not currectly supported).
end;

procedure TCustomMPEnvelopedData.ExtractContentEncryptionAlgorithm;
var
  P: PASN1Struct; 
{$IFDEF ARCTWO}
  EKBParam, KeySize: Integer;
{$ENDIF ARCTWO}
begin
  P := FEnvStruct.FindField('/content//encryptedContentInfo/contentType');
  OIDToContentType(P^.ContentAsOID,FContentType);

  P := FEnvStruct.FindField('/content//encryptedContentInfo/contentEncryptionAlgorithm/algorithm');
  FCipherClass := FindCipherClass(P^.ContentAsOID,FKeyLength,FKeyedIV);
  P := FEnvStruct.FindField('/content//encryptedContentInfo/contentEncryptionAlgorithm/parameters');
{$IFDEF ARCTWO}
  if Assigned(P) and (P^.ChoiceTypeName = 'RC2CBCParameter') then begin
    EKBParam := P^.Items[0]^.ContentAsInteger;
    if (EKBParam < 256) and (EKBParam >= 0) then begin
      KeySize := 0;
      while SsArc2.EKBTable[KeySize] <> EKBParam do
        Inc(KeySize);
    end else
      KeySize := EKBParam;
    FKeyLength := KeySize shr 3;
    P := P^.Items[1];
  end;
{$ENDIF ARCTWO}

  if Assigned(P) and (P^.Length > 0) then begin
    FIV := TSecretKey.Create('');
    FIV.SetKey(P^.Content,P^.Length,0);
  end else
    FIV := nil;

  FIllegalFormat := not FSMIMECapabilities.HasCapability(FCipherClass,FKeyLength,FKeyedIV);
end;

procedure TCustomMPEnvelopedData.Final(AContent: TStream);
var
  P: PASN1Struct;
  C: TCipher;
  Cnt: ISecretKey;
  I: Integer;

  procedure DecryptStruct(Struct: PASN1Struct; Padding: Boolean);
  var
    Pad: Byte;
    CTSize, PTSize: Integer;
  begin
    Cnt := TSecretKey.CreateStr(P.Content,P.Length);
    C.DecryptIntf(Cnt);
    if Padding then begin
      CTSize := Cnt.KeyLen;
      Pad := Cnt.KeyBytes[CTSize-1];
      if (Pad > 0) and (Pad <= C.BlockSize) then
        PTSize := CTSize - Pad
      else
        PTSize := CTSize;
    end else
      PTSize := Cnt.KeyLen;
    if PTSize > 0 then
      AContent.Write(Cnt.Key^,PTSize);
  end;

begin
  P := FEnvStruct.FindField('/content//encryptedContentInfo/encryptedContent');
  C := FCipherClass.CreateIntf(FContentEncryptionKey);
  try
    if Assigned(FIV) then
      C.SetVectorBuf(FIV.Key^,FIV.KeyLen);
    if P.Constructed then begin
      for I := 0 to P^.ItemCount - 2 do
        DecryptStruct(P^.Items[I],False);
      DecryptStruct(P^.Items[P^.ItemCount - 1],C.BlockSize > 0);
    end else
      DecryptStruct(P,C.BlockSize > 0);
  finally
    C.Free;
  end;
end;

procedure TCustomMPEnvelopedData.FinalEnv;
begin
  if Assigned(FFile) then begin
    if FContentOnly then
      FEnvStruct.FindField('/content/').SaveToStream(FFile,fmtDER)
    else
      FEnvStruct.SaveToStream(FFile,fmtDER);
  end;
end;

procedure TCustomMPEnvelopedData.ImposeCertificates;
begin
  // Implement inclusion of DH and ECDH certificates (not currectly supported).
end;

procedure TCustomMPEnvelopedData.ImposeRecipient;
var
  P, A, F, G: PASN1Struct;
  {$IFDEF ALLOW_CMS_3}
  PKI: string;
  {$ENDIF ALLOW_CMS_3}
  Serial: string;
  Publ: IMPPublicKey;
  I: Integer;
  Cert: TCertificate;
  Done: Boolean;
  Capabilities: PASN1Struct;
begin
  P := FEnvStruct.FindField('/content//recipientInfos');

  FCollectedSMIMECapabilities.Assign(FSMIMECapabilities);

  I := 0;
  Cert := FCert;
  repeat
    FCert := Cert;
    A := P^.AddField;
    if Cert.SubjectPublicKeyInfo.IsType(epkeRsaEncryption) then begin
      if FCollectedCertificates.FindSMIMECapabilities(Cert.Data,Capabilities) then begin
        if not FCollectedSMIMECapabilities.AssignStruct(Capabilities^) then begin
          FCertificateNotFound := True;
          Break;
        end;
      end;

      A.TrySelectChoice(V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);

      {$IFDEF ALLOW_CMS_3}
      PKI := ExtractSubjectKeyIdentifier(Cert.Data);
      if PKI = '' then
      {$ENDIF ALLOW_CMS_3}
        A^.EditField('version',Integer(0))
      {$IFDEF ALLOW_CMS_3}
      else
        A^.EditField('version',Integer(2))
      {$ENDIF ALLOW_CMS_3}
      ;

      F := A^.FindField('rid');
      {$IFDEF ALLOW_CMS_3}
      if (FContentType = pNone) // Probably older format, use issuer + serialNumber
         or (PKI = '') then begin
      {$ENDIF ALLOW_CMS_3}
        F^.TrySelectChoice(V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
        ExtractIssuerStruct(Cert.Data,G);
        F^.FindField('issuer')^.Assign(G^);
        ExtractSerial(Cert.Data,Serial);
        F^.FindField('serialNumber')^.SetContent(Pointer(Serial)^,Length(Serial));
      {$IFDEF ALLOW_CMS_3}
      end else begin
        F^.TrySelectChoice(V_ASN1_CONTEXT_SPECIFIC,True,0);
        F^.Items[0]^.SetContent(Pointer(PKI)^,Length(PKI));
      end;
      {$ENDIF ALLOW_CMS_3}

      F := A^.FindField('keyEncryptionAlgorithm');
      F^.EditField('algorithm',rsaEncryption);

      F := A^.FindField('encryptedKey');
      Publ := TMPIFPublicKey.Create(Cert);
      try
        F^.Length := Publ.SignatureLength;
        FCertificateNotFound :=
          not Publ.EncryptKeyTransport(FContentEncryptionKey.Key^,
                                       FContentEncryptionKey.KeyLen,
                                       F^.Content^,F^.Length);
      finally
        Publ := nil;
      end;
    end else
      FCertificateNotFound := True;

    Done := True;
    Inc(I);
    DoAddRecipient(I,Cert,Done);
  until Done;
  if not FCertificateNotFound then
    FCertificateNotFound := not FCollectedSMIMECapabilities.HasCapability(FCipherClass,FKeyLength,FKeyedIV);
end;

procedure TCustomMPEnvelopedData.Init;
begin
  FIllegalFormat := False;
  FCertificateNotFound := False;
end;

procedure TCustomMPEnvelopedData.InitEnv;
var
  BCC: TBlockCipherClass;
begin
  FIllegalFormat := False;
  FCertificateNotFound := False;
  FKeyLength := FSMIMECapabilities.Items[0].KeyLength;
  FKeyedIV := FSMIMECapabilities.Items[0].KeyedIV;
  FCipherClass := FindCipherClass(FSMIMECapabilities.Items[0].GetOID,FKeyLength,FKeyedIV);
  if FCipherClass.InheritsFrom(TBlockCipher) then
    BCC := TBlockCipherClass(FCipherClass)
  else
    BCC := nil;
  if FKeyedIV then begin
    MpYarrow.RawRandomIntf(FContentEncryptionKey,
                           (FKeyLength + BCC.BlockVectorSize * BCC.BlockSize)*8);
    FContentEncryptionKey.VectorSize := BCC.BlockVectorSize;
    FIV := nil;
  end else begin
    MpYarrow.RawRandomIntf(FContentEncryptionKey,FKeyLength*8);
    if Assigned(BCC) then
      MpYarrow.RawRandomIntf(FIV,BCC.BlockSize * BCC.BlockVectorSize * 8)
    else
      FIV := nil;
  end;
end;

procedure TCustomMPEnvelopedData.LoadContents(AContent: TStream);
var
  P: PASN1Struct;
  Cnt: ISecretKey;
  C: TCipher;
  PTSize, CTSize: Integer;
  I: Integer;
  PadLen: Byte;
begin
  if Assigned(FEnvStruct) then FEnvStruct._Release;
  FEnvStruct := nil;
  CreateEnvStruct;
  ImposeCertificates;
  if Assigned(AContent) then begin
    if FContentType <> pNone then begin
      P := FEnvStruct.FindField('/content//encryptedContentInfo/contentType');
      P^.EditContent(Cms.ContentTypeToOID(FContentType));
    end;
    P := FEnvStruct.FindField('/content//encryptedContentInfo/contentEncryptionAlgorithm/algorithm');
    if FKeyedIV then
      P^.EditContent(FCipherClass.KeyedIVOID(FKeyLength))
    else
      P^.EditContent(FCipherClass.OID(FKeyLength));
    P := FEnvStruct.FindField('/content//encryptedContentInfo/contentEncryptionAlgorithm/parameters');
{$IFDEF ARC2}
    if P^.ChoiceTypeName = 'RC2CBCParameter' then begin
      if FKeyLength < 32 then
        P^.Items[0].EditContent(Integer(SsArc2.EKBTable[FKeyLength*8]))
      else
        P^.Items[0].EditContent(Integer(FKeyLength*8));
      P := P^.Items[1];
    end;
{$ENDIF ARC2}
    if Assigned(FIV) then begin
      P^.TypeName := 'OCTET STRING';
      P^.SetContent(FIV.Key^,FIV.KeyLen);
    end;
    P := FEnvStruct.FindField('/content//encryptedContentInfo/encryptedContent');
    C := FCipherClass.CreateIntf(FContentEncryptionKey);
    try
      if Assigned(FIV) then
        C.SetVectorBuf(FIV.Key^,FIV.KeyLen);
      PTSize := AContent.Size;
      if C.BlockSize > 1 then
        PadLen := C.BlockSize - (PTSize mod C.BlockSize)
      else
        PadLen := 0;
      CTSize := PTSize + PadLen;
      Cnt := TSecretKey.Create('');
      Cnt.SetLength(CTSize);
      AContent.Read(Cnt.Key^,PTSize);
      for I := PTSize to CTSize - 1 do
        Cnt.KeyBytes[I] := PadLen;
      C.EncryptIntf(Cnt);
      P^.SetContent(Cnt.Key^,CTSize);
    finally
      C.Free;
    end;
  end;
end;

function TCustomMPEnvelopedData.LoadEnvStruct: Boolean;
begin
  if Assigned(FEnvStruct) then FEnvStruct._Release;
  FEnvStruct := nil;
  CreateEnvStruct;
  try
    if FContentOnly then
      FEnvStruct.FindField('/content/').LoadFromStream(FFile,fmtDER)
    else
      FEnvStruct.LoadFromStream(FFile,fmtDER);
    ExtractCertificates;
    ExtractContentEncryptionAlgorithm;
    Result := not FIllegalFormat;
  except
    Result := False;
  end;
end;

procedure TCustomMPEnvelopedData.SetCipherClass(
  const Value: TCipherClass);
begin
  FCipherClass := Value;
end;

procedure TCustomMPEnvelopedData.SetContentOnly(const Value: Boolean);
begin
  FContentOnly := Value;
end;

procedure TCustomMPEnvelopedData.SetOnAddRecipient(
  const Value: TAddCertificateEvent);
begin
  FOnAddRecipient := Value;
end;

procedure TCustomMPEnvelopedData.DoAddRecipient(Index: Integer;
  var Cert: TCertificate; var Done: Boolean);
begin
  if FRecipientEMailList.Count > 0 then begin
    if Index < FRecipientEMailList.Count then begin
      Cert.Free;
      Cert := TCertificate.Create(nil,nil);
      Cert.AssignStruct(TASN1Struct(FRecipientEMailList.Objects[Index]));
      Done := False;
    end;
  end;
  if Assigned(FOnAddRecipient) then
    FOnAddRecipient(Self,Index,Cert,Done);
end;

constructor TCustomMPEnvelopedData.Create(AOwner: TComponent);
begin
  inherited;
  FRecipientEMailList := TStringList.Create;
end;

destructor TCustomMPEnvelopedData.Destroy;
begin
  FRecipientEMailList.Free;
  inherited;
end;

procedure TCustomMPEnvelopedData.SetOnRecipientCertNotFound(
  const Value: TEmailEvent);
begin
  FOnRecipientCertNotFound := Value;
end;

{ TSMIMECapabilities }

function TSMIMECapabilities.AssignStruct(Struct: TASN1Struct;
  Intersect: Boolean): Boolean;
var
  I: Integer;
{$IFDEF ARCTWO}
  EKB: Integer;
{$ENDIF ARCTWO}
  vKeyLen: Integer;
  CC: TCipherClass;
  vKeyedIV, OK: Boolean;
  NewItems: TSMIMECapabilities;
begin
  Result := Assigned(Struct);
  if Result then begin
    Result := (Struct.ActualTag = V_ASN1_SET) and Struct.Constructed;
    if Result then begin
      NewItems := TSMIMECapabilities.Create(nil,TSMIMECapabilityItem);
      try
        for I := 0 to Struct.ItemCount - 1 do begin
          CC := FindCipherClass(Struct.Items[I].Items[0].ContentAsOID,vKeyLen,vKeyedIV);
          if Assigned(CC) then begin
{$IFDEF ARCTWO}
            if CC.InheritsFrom(TARC2_ECB) then begin
              OK := Struct.Items[I].ItemCount > 1;
              if OK then begin
                OK := Struct.Items[I].Items[1].Constructed and
                      (Struct.Items[I].Items[1].ItemCount > 0);
                if OK then begin
                  EKB := Struct.Items[I].Items[1].Items[0].ContentAsInteger;
                  if EKB < 256 then begin
                    vKeyLen := 0;
                    while vKeyLen <> EKBTable[EKB] do
                      Inc(vKeyLen);
                  end else
                    vKeyLen := EKB;
                  OK := vKeyLen and $7 = 0;
                  vKeyLen := vKeyLen shr 3;
                end;
              end;
            end else
{$ENDIF ARCTWO}
              OK := True;
            if OK and (HasCapability(CC,vKeyLen,vKeyedIV) or not Intersect) then
              with TSMIMECapabilityItem(NewItems.Add) do begin
                CipherAlg := CC.Algorithm;
                CipherMode := CC.Mode;
                KeyedIV := vKeyedIV;
                KeyLength := vKeyLen;
              end;
          end;
        end;
        Assign(NewItems);
      finally
        NewItems.Free;
      end;
      Result := Count > 0;
    end;
  end;
end;

function TSMIMECapabilities.GetAttr(Index: Integer): string;
begin
  case Index of
    0: Result := 'Algorithm';
    1: Result := 'Mode';
    2: Result := 'Key Length';
    3: Result := 'Keyed IV';
    4: Result := 'OID';
  else
    Result := '';
  end;
end;

function TSMIMECapabilities.GetAttrCount: Integer;
begin
  Result := 5;
end;

function TSMIMECapabilities.GetItem(Index: Integer): TSMIMECapabilityItem;
begin
  Result := TSMIMECapabilityItem(inherited GetItem(Index));
end;

function TSMIMECapabilities.GetItemAttr(Index, ItemIndex: Integer): string;
var
  Item: TSMIMECapabilityItem;
  CC: TCipherClass;
begin
  Item := Items[ItemIndex];
  if Item.GetIsValid(CC) then begin
    case Index of
      0: Result := CC.AlgorithmName;
      1: case CC.Mode of
           cmECB: Result := 'ECB';
           cmCFB: Result := 'CFB';
           cmCBC: Result := 'CBC';
           cmOFB: Result := 'OFB';
           cmCTR: Result := 'CTR';
           cmABC: Result := 'ABC';
           cmPCFB: Result := 'PCFB';
           cmPipedPCFB: Result := 'Piped PCFB';
         else
           Result := '(invalid)';
         end;
      2: Result := IntToStr(Item.KeyLength);
      3: if Item.KeyedIV then
           Result := '*'
         else
           Result := '';
      4: Result := Item.OID;
    else
      Result := '';
    end;
  end else
    Result := '(invalid)';
end;

function TSMIMECapabilities.HasCapability(CC: TCipherClass;
  AKeyLength: Integer; AKeyedIV: Boolean): Boolean;
var
  I: Integer;
begin
  Result := False;
  I := 0;
  while (I < Count) and not Result do begin
    with Items[I] do
      Result := (CipherAlg = CC.Algorithm) and
                (CipherMode = CC.Mode) and
                (KeyLength = AKeyLength) and
                (KeyedIV = AKeyedIV);
    Inc(I);
  end;
end;

procedure TSMIMECapabilities.SetItem(Index: Integer;
  const Value: TSMIMECapabilityItem);
begin
  inherited SetItem(Index,Value);
end;

{ TCustomMPCMSBroker }

function TCustomMPCMSBroker.Decode(AStream, ADest: TStream;
  var Actions: TContentTypeArray): Boolean;
var
  F: TASN1Struct;
  DataType: TPKCS7DataType;
  Done: Boolean;
begin
  Result := False;
  SetLength(Actions,0);
  F := nil;
  try
    Done := False;
    Asn1.ASN1IdenAndLenFromStream(AStream,F,Done);
    if Done and (F.ActualTag = V_ASN1_SEQUENCE) and F.Constructed then begin
      Asn1.ASN1IdenAndLenFromStream(AStream,F,Done);
      if Done and (F.ActualTag = V_ASN1_OBJECT) and not F.Constructed then begin
        F.Length := F.Length;
        AStream.Read(F.Content^,F.Length);
        Result := Cms.OIDToContentType(F.ContentAsOID,DataType) and
                  (DataType in [pSignedData,pEnvelopedData]);
        if Result then begin
          Asn1.ASN1IdenAndLenFromStream(AStream,F,Done);
          if Done and
             (F.Tag = 0) and
             (F.Cls = V_ASN1_CONTEXT_SPECIFIC) and
             F.Constructed then begin

            Result := DecodeContent(DataType,AStream,ADest,Actions);
          end;
        end;
      end;
    end;
  finally
    F.Free;
  end;
end;

procedure TCustomMPCMSBroker.AddCertName(Cert: TCertificate);
var
  EmailAddr: string;

  procedure AddItem(const aType, aValue: string);
  begin
    if aValue <> '' then begin
      Inc(FIdx);
      FActionDescriptions.Insert(FIdx,#9 + aType + '=' + aValue);
    end;
  end;

begin
  with Cert.Subject do begin
    LineSeparator := '/';
    AddItem('CN',CommonName);
    AddItem('T',Title);
    AddItem('GN',GivenName);
    AddItem('I',Initials);
    AddItem('S',SurName);
    AddItem('GQ',GenerationQualifier);
    AddItem('O',OrganizationName);
    AddItem('OU',OrganizationalUnitName);
    AddItem('L',LocalityName);
    AddItem('S',StateOrProvinceName);
    AddItem('C',Country);
    AddItem('DQ',DnQualifier);
  end;
  EmailAddr := X509Base.ExtractEmailAddress(Cert.Data);
  AddItem('E',EmailAddr);
end;

function TCustomMPCMSBroker.Decode(AStream, ADest: TStream;
  ActionDescriptions: TStrings): Boolean;
var
  Actions: TContentTypeArray;
  F: TASN1Struct;
  DataType: TPKCS7DataType;
  Done: Boolean;
begin
  Result := False;
  SetLength(Actions,0);
  FActionDescriptions := ActionDescriptions;
  F := nil;
  try
    Done := False;
    Asn1.ASN1IdenAndLenFromStream(AStream,F,Done);
    if Done and (F.ActualTag = V_ASN1_SEQUENCE) and F.Constructed then begin
      Asn1.ASN1IdenAndLenFromStream(AStream,F,Done);
      if Done and (F.ActualTag = V_ASN1_OBJECT) and not F.Constructed then begin
        F.Length := F.Length;
        AStream.Read(F.Content^,F.Length);
        Result := Cms.OIDToContentType(F.ContentAsOID,DataType) and
                  (DataType in [pSignedData,pEnvelopedData]);
        if Result then begin
          Asn1.ASN1IdenAndLenFromStream(AStream,F,Done);
          if Done and
             (F.Tag = 0) and
             (F.Cls = V_ASN1_CONTEXT_SPECIFIC) and
             F.Constructed then begin

            Result := DecodeContent(DataType,AStream,ADest,Actions);
          end;
        end;
      end;
    end;
  finally
    FActionDescriptions := nil;
    F.Free;
  end;
end;

function TCustomMPCMSBroker.DecodeContent(Action: TPKCS7DataType; AStream,
  ADest: TStream; var Actions: TContentTypeArray): Boolean;
var
  MS: TSecureMemoryStream;
begin
  MS := TSecureMemoryStream.Create;
  try
    case Action of
      pSignedData:
        begin
          Result := Assigned(FSignedData);
          if Result then begin
            FSignedData.FBroker := Self;
            try
              FIdx := -1;
              FSignedData.ContentOnly := True;
              Result := FSignedData.VerifySignedData(AStream,MS);
            finally
              FSignedData.FBroker := nil;
            end;
            if Result then begin
              MS.Position := 0;
              Result := DecodeContent(FSignedData.ContentType,MS,ADest,Actions);
              if Result then begin
                SetLength(Actions,Length(Actions) + 1);
                Actions[Length(Actions)-1] := pSignedData;
              end;
            end;
          end;
        end;
      pEnvelopedData:
        begin
          Result := Assigned(FEnvelopedData);
          if Result then begin
            FEnvelopedData.ContentOnly := True;
            Result := FEnvelopedData.Decrypt(AStream,MS);
            if Result then begin
              MS.Position := 0;
              Result := DecodeContent(FEnvelopedData.ContentType,MS,ADest,Actions);
              if Result then begin
                SetLength(Actions,Length(Actions) + 1);
                Actions[Length(Actions)-1] := pEnvelopedData;
                if Assigned(FActionDescriptions) then begin
                  FActionDescriptions.Append('EnvelopedData');
                  FActionDescriptions.Append(#9 + FEnvelopedData.FCipherClass.ClassName);
                  FActionDescriptions.Append(#9'KeyLength: ' + IntToStr(FEnvelopedData.FKeyLength));
                end;
              end;
            end;
          end;
        end;
      pData:
        begin
          if Assigned(ADest) then
            ADest.CopyFrom(AStream,0);
          Result := True;
        end;
    else
      Result := False;
    end;
  finally
    MS.Free;
  end;
end;

procedure TCustomMPCMSBroker.DoChangeComponentProperty;
begin
  inherited;
  if Assigned(FEnvelopedData) then begin
    if Assigned(FKeysAndCerts) then
      FEnvelopedData.KeysAndCerts := FKeysAndCerts;
    if Assigned(FPrivateKeyRing) then
      FEnvelopedData.PrivateKeyRing := FPrivateKeyRing;
    if Assigned(FMyCertificates) then
      FEnvelopedData.MyCertificates := FMyCertificates;
    if Assigned(FCollectedCertificates) then
      FEnvelopedData.CollectedCertificates := FCollectedCertificates;
    FEnvelopedData.SMIMECapabilities := SMIMECapabilities;
    FEnvelopedData.AllowExpiredCertificates := AllowExpiredCertificates;
    if Assigned(FOnAddRecipient) then
      FEnvelopedData.OnAddRecipient := FOnAddRecipient;
  end;
  if Assigned(FSignedData) then begin
    if Assigned(FKeysAndCerts) then
      FEnvelopedData.KeysAndCerts := FKeysAndCerts;
    if Assigned(FPrivateKeyRing) then
      FSignedData.PrivateKeyRing := FPrivateKeyRing;
    if Assigned(FMyCertificates) then
      FSignedData.MyCertificates := FMyCertificates;
    if Assigned(FCollectedCertificates) then
      FSignedData.CollectedCertificates := FCollectedCertificates;
    FSignedData.SMIMECapabilities := SMIMECapabilities;
    FSignedData.DetachedSignature := False;
    FSignedData.AllowExpiredCertificates := AllowExpiredCertificates;
    if Assigned(FOnAddSigner) then
      FSignedData.OnAddSigner := FOnAddSigner;
  end;
end;

procedure TCustomMPCMSBroker.DoGoodCounterSignature(OuterCert,
  Cert: TCertificate; SigningTime: TDateTime);
begin
  if Assigned(FActionDescriptions) then begin
    Inc(FIdx);
    FActionDescriptions.Insert(FIdx,'SignedData Counter Signature');
    if FSignedData.SigningDescription <> '' then begin
      Inc(FIdx);
      FActionDescriptions.Insert(FIdx,#9'Description: ' + FSignedData.SigningDescription);
    end;
    if FSignedData.FSigningTime <> 0 then begin
      Inc(FIdx);
      FActionDescriptions.Insert(FIdx,#9'SigningTime: ' + DateTimeToStr(SigningTime));
    end;
    Inc(FIdx);
    FActionDescriptions.Insert(FIdx,'Signature of');
    AddCertName(OuterCert);
    Inc(FIdx);
    FActionDescriptions.Insert(FIdx,'Counter Signed by');
    AddCertName(Cert);
  end;
end;

procedure TCustomMPCMSBroker.DoGoodSignature(Cert: TCertificate);
begin
  if Assigned(FActionDescriptions) then begin
    Inc(FIdx);
    FActionDescriptions.Insert(FIdx,'SignedData');
    if FSignedData.SigningDescription <> '' then begin
      Inc(FIdx);
      FActionDescriptions.Insert(FIdx,#9'Description: ' + FSignedData.SigningDescription);
    end;
    if FSignedData.FSigningTime <> 0 then begin
      Inc(FIdx);
      FActionDescriptions.Insert(FIdx,#9'SigningTime: ' + DateTimeToStr(FSignedData.FSigningTime));
    end;
    AddCertName(Cert);
  end;
end;

function TCustomMPCMSBroker.Encode(Actions: TContentTypeArray; AStream,
  ADest: TStream; DataType: TPKCS7DataType): Boolean;
var
  MS: TSecureMemoryStream;
  Action: TPKCS7DataType;
  Dummy: TASN1Struct;
begin
  MS := TSecureMemoryStream.Create;
  try
    if Length(Actions) = 0 then
      Action := pData
    else
      Action := Actions[0];
    case Action of
      pSignedData:
        begin
          Result := Assigned(FSignedData);
          if Result then begin
            FSignedData.ContentType := DataType;
            Dummy := nil;
            if Length(Actions) = 1 then begin
              FSignedData.ContentOnly := False;
              Result := FSignedData.SignData(Dummy,AStream,ADest);
            end else begin
              FSignedData.ContentOnly := True;
              Result := FSignedData.SignData(Dummy,AStream,MS);
              if Result then begin
                MS.Position := 0;
                Result := Encode(Copy(Actions,1,Length(Actions)-1),
                                 MS,ADest,Action);
              end;
            end;
          end;
        end;
      pEnvelopedData:
        begin
          Result := Assigned(FEnvelopedData);
          if Result then begin
            FEnvelopedData.ContentType := DataType;
            Dummy := nil;
            if Length(Actions) = 1 then begin
              FEnvelopedData.ContentOnly := False;
              Result := FEnvelopedData.Encrypt(Dummy,AStream,ADest);
            end else begin
              FEnvelopedData.ContentOnly := True;
              Result := FEnvelopedData.Encrypt(Dummy,AStream,MS);
              if Result then begin
                MS.Position := 0;
                Result := Encode(Copy(Actions,1,Length(Actions)-1),
                                 MS,ADest,Action);
              end;
            end;
          end;
        end;
    else
      Result := False;
    end;
  finally
    MS.Free;
  end;
end;

procedure TCustomMPCMSBroker.Loaded;
begin
  inherited;
  DoChangeComponentProperty;
end;

procedure TCustomMPCMSBroker.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FSignedData then
      FSignedData := nil;
    if AComponent = FEnvelopedData then
      FEnvelopedData := nil;
  end;
end;

procedure TCustomMPCMSBroker.SetEnvelopedData(
  const Value: TCustomMPEnvelopedData);
begin
  FEnvelopedData := Value;
  if Assigned(Value) then begin
    Value.FreeNotification(Self);
    DoChangeComponentProperty;
  end;
end;

procedure TCustomMPCMSBroker.SetOnAddRecipient(
  const Value: TAddCertificateEvent);
begin
  FOnAddRecipient := Value;
  if Assigned(Value) then
    DoChangeComponentProperty;
end;

procedure TCustomMPCMSBroker.SetOnAddSigner(
  const Value: TAddCertificateEvent);
begin
  FOnAddSigner := Value;      
  if Assigned(Value) then
    DoChangeComponentProperty;
end;

procedure TCustomMPCMSBroker.SetSignedData(
  const Value: TCustomMPSignedData);
begin
  FSignedData := Value;
  if Assigned(Value) then begin
    Value.FreeNotification(Self);
    DoChangeComponentProperty;
  end;
end;

end.
