{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     SsCertMgr Unit                                    }
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
unit SsCertMgr;

interface

uses
  SysUtils, Classes, ResourceFile, SecUtils, Pkix, Asn1, MpPK, Pkix_Cert,
  Pkcs_10, X509Base, MpX509, StreamSecII, TlsInternalServer;

type
  TX509CertificateAuthority = class(TCustomSimpleTLSInternalServer)
  private
    FIssuedCerts: TX509TrustedCertificates;
    FIssuedCertsFile: TResourceFile;
    FIssuedCertsLoaded: Boolean;
    FStoreIssuedCerts: Boolean;
    function GetIssuedCertList: TCertificateCollection;
    function GetIssuedCertsGroupIndex: Integer;
    function GetIssuedCertsSource: TCustomX509TrustedCertificates;
    procedure ReadIssuedCerts(AStream: TStream);
    procedure WriteIssuedCerts(AStream: TStream);
    procedure SetIssuedCertList(const Value: TCertificateCollection);
    procedure SetIssuedCertsGroupIndex(const Value: Integer);
    procedure SetIssuedCertsSource(
      const Value: TCustomX509TrustedCertificates);
    procedure SetStoreIssuedCerts(const Value: Boolean);
  protected                                     
    procedure DefineProperties(AFiler: TFiler); override;
    procedure DoLoadIssuedCertsSCLFile(Sender: TObject);
    function GetCertificateCollectionCount: Integer; override;
    function GetCertificateCollections(
      Index: Integer): TCertificateCollection; override;
    procedure SetCertificateCollections(Index: Integer;
      const Value: TCertificateCollection); override;
    procedure InternalLoaded; override;
    procedure InternalWriteIssuedCerts;
    procedure LoadedResolveResourceFiles; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetIssuedCertsSCLFile(const Value: TResourceFile);
    procedure UpdateHijacker(AHijacker: TCustomX509TrustedCertificates); override;
    property InternalIssuedCertsFile: TResourceFile read FIssuedCertsFile;
    property IssuedCerts: TX509TrustedCertificates read FIssuedCerts;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearCertificates; override;
    procedure ClearIssuedCertificates;  
    function FindCRL(const CACert: TASN1Struct;
                     const Reasons: TX509ReasonFlags;
                     var CRL: PASN1Struct): Boolean; overload;
    function FindCRLs(const CACert: TASN1Struct;
                      var CRLs: TASN1Struct): Integer; override;
    function FindRevocation(const Cert: TASN1Struct;
                            var RC: TX509RevokedCertificate): Boolean; override;
    function FindSMIMECapabilities(Cert: TASN1Struct;
                                   var Capabilities: PASN1Struct): Boolean; override;
    procedure LoadIssuedCertsFromFile(AFileName: TFileName);
    procedure LoadIssuedCertsFromStream(AStream: TStream);
    function NewCertAdvancedDlg(AExportData: TStream): Boolean;
    function NewCertFromReqDlg(AImportData, AExportData: TStream): Boolean;
    function NewCodeSigningCertDlg(AExportData: TStream): Boolean;
    function NewCodeSigningCertReqDlg(AExportData: TStream): Boolean;
    function NewEMailProtectionCertDlg(AExportData: TStream): Boolean;
    function NewEMailProtectionCertReqDlg(AExportData: TStream): Boolean;
    function NextIssuedSerialNumber(const CACert: TASN1Struct): string;
    function RevokeCertificate(const Cert: TASN1Struct;
                               Reason: CRLReason;
                               HoldInstruction: string;
                               InvalidityDate: TDateTime;
                               NextCRLUpdate: TDateTime): Boolean;
    procedure SaveIssuedCertsToSCLFile(AFileName: TFileName);
    procedure SaveIssuedCertsToSCLStream(AStream: TStream);
    function Sign(Signed: ISigned; PublicKey: IPublicKeyInfo;
                  HA: THashAlgorithm = haSHA1): Boolean;
    function SignBuf(const Buf; BufLen: Integer;
                     PKI: Pointer; PKILen: Integer;
                     HA: THashAlgorithm;
                     var Sign; var SignLen: Integer): Boolean;
    function SignCertificate(DstCert, CACert: TASN1Struct;
                             var Status: TCertStatusCode;
                             HA: THashAlgorithm = haSHA1): Boolean; overload;
    function SignCertificate(DstCert: TCertificate;
                             CACert: TASN1Struct;         
                             var Status: TCertStatusCode;
                             HA: THashAlgorithm = haSHA1): Boolean; overload;
    function SignCertificate(CertReq: TCertificationRequest;
                             DstCert: TCertificate;
                             CACert: TASN1Struct;
                             var Status: TCertStatusCode;
                             HA: THashAlgorithm = haSHA1): Boolean; overload;
    function SignSigned(Signed: TSigned; CACert: IPublicKeyInfo;
                        HA: THashAlgorithm = haSHA1): Boolean;
  published
    property IssuedCertsGroupIndex: Integer read GetIssuedCertsGroupIndex write SetIssuedCertsGroupIndex default -1;
    property IssuedCertList: TCertificateCollection read GetIssuedCertList write SetIssuedCertList;
    property IssuedCertsSource: TCustomX509TrustedCertificates read GetIssuedCertsSource write SetIssuedCertsSource;
    property StoreIssuedCerts: Boolean read FStoreIssuedCerts write SetStoreIssuedCerts default True;
    property UserList;
    property BeforeCreateNewCertDlg;
    property OnGetNewCertDlg;
    property OnCertPolicy;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  NewCertDlg, Controls, Dialogs,
{$ENDIF}
{$IFDEF LINUX}
  QNewCertDlg, QControls, QDialogs,
{$ENDIF}
  Pkcs10;

type
  TCustomX509TrustedCertificatesHack = class(TCustomX509TrustedCertificates);

{ TX509CertificateAuthority }

procedure TX509CertificateAuthority.ClearCertificates;
begin
  inherited;
  ClearIssuedCertificates;
end;

procedure TX509CertificateAuthority.ClearIssuedCertificates;
begin
  FIssuedCerts.Clear;
end;

constructor TX509CertificateAuthority.Create(AOwner: TComponent);
begin
  inherited;
  FIssuedCertsFile := TResourceFile.Create(Self);
  FIssuedCerts := TX509TrustedCertificates.Create(Self);             
  TCustomX509TrustedCertificatesHack(FIssuedCerts).AddHijacker(Self);
  FIssuedCerts.TrustedCACertificates := MyCerts;
  FStoreIssuedCerts := True;
end;

procedure TX509CertificateAuthority.DefineProperties(AFiler: TFiler);
begin
  inherited;
  AFiler.DefineBinaryProperty('InternalIssuedCerts',ReadIssuedCerts,WriteIssuedCerts,IssuedCertsSource = nil);
end;

destructor TX509CertificateAuthority.Destroy;
begin
  FIssuedCertsFile.RemoveLoadNotification(Self,DoLoadIssuedCertsSCLFile);
  FIssuedCerts.Free;
  inherited;
end;

procedure TX509CertificateAuthority.DoLoadIssuedCertsSCLFile(
  Sender: TObject);
begin
  if Sender = FIssuedCertsFile then begin
    try
      if Assigned(FIssuedCertsFile) and
         Assigned(FIssuedCertsFile.DataStream) then begin
        LoadIssuedCertsFromStream(FIssuedCertsFile.DataStream);
      end;
    except
      FIssuedCertsFile.RemoveLoadNotification(Self,DoLoadIssuedCertsSCLFile);
    end;
  end;
end;

function TX509CertificateAuthority.FindCRL(const CACert: TASN1Struct;
  const Reasons: TX509ReasonFlags; var CRL: PASN1Struct): Boolean;
begin
  Result := MyCerts.FindCRL(CACert,Reasons,CRL);
end;

function TX509CertificateAuthority.FindCRLs(const CACert: TASN1Struct;
  var CRLs: TASN1Struct): Integer;
begin
  Result := inherited FindCRLs(CACert,CRLs);
  if Result <= 0 then
    Result := FIssuedCerts.FindCRLs(CACert,CRLs);
end;

function TX509CertificateAuthority.FindRevocation(const Cert: TASN1Struct;
  var RC: TX509RevokedCertificate): Boolean;
begin
  Result := inherited FindRevocation(Cert,RC);
  if not Result then
    Result := FIssuedCerts.FindRevocation(Cert,RC);
end;

function TX509CertificateAuthority.FindSMIMECapabilities(Cert: TASN1Struct;
  var Capabilities: PASN1Struct): Boolean;
begin
  Result := inherited FindSMIMECapabilities(Cert,Capabilities);
  if not Result then
    Result := FIssuedCerts.FindSMIMECapabilities(Cert,Capabilities);
end;

function TX509CertificateAuthority.GetCertificateCollectionCount: Integer;
begin
  Result := 4;
end;

function TX509CertificateAuthority.GetCertificateCollections(
  Index: Integer): TCertificateCollection;
begin
  if Index = 3 then
    Result := GetIssuedCertList
  else
    Result := inherited GetCertificateCollections(Index);
end;

function TX509CertificateAuthority.GetIssuedCertList: TCertificateCollection;
begin
  Result := FIssuedCerts.CertList;
end;

function TX509CertificateAuthority.GetIssuedCertsGroupIndex: Integer;
begin
  Result := FIssuedCerts.GroupIndex;
end;

function TX509CertificateAuthority.GetIssuedCertsSource: TCustomX509TrustedCertificates;
begin
  Result := FIssuedCerts.CertSource;
end;

procedure TX509CertificateAuthority.InternalLoaded;
begin
  FIssuedCerts.Name := Name + 'IssuedCerts';
  FIssuedCertsFile.Name := Name + 'IssuedCertsFile';
  inherited;
end;

procedure TX509CertificateAuthority.InternalWriteIssuedCerts;
var
  Password: ISecretKey;
  CC: TCipherClass;
  SignCert: TASN1Struct;
  SignKey: IMPPrivateKey;
begin
  if IssuedCerts.Modified then begin
    StreamSecIIComp.DoPassword(Password);
    CC := FindCipherClass(caRijndael,cmABC);
    if CC = nil then
      CC := FindCipherClass(caTwoFish,cmABC);
    Assert(Assigned(CC),'Rijndael-ABC or TwoFish-ABC must be enabled');
    InternalPrivateKeyRingFile.CheckCreateDataStream;
    StreamSecIIComp.SavePrivateKeyRingToStream(InternalPrivateKeyRingFile.DataStream,
                                               Password,
                                               kdfWPv2SHA1,
                                               1 shl KeyDerivationBits,
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
          InternalIssuedCertsFile.CheckCreateDataStream;
          IssuedCerts.SignSaveToStream(InternalIssuedCertsFile.DataStream,
                                       SignCert,
                                       SignKey);
          SetIssuedCertsSCLFile(InternalIssuedCertsFile);
        end;
      end else
        raise Exception.Create('TSimpleTLSInternalServer.InternalWriteIssuedCerts: Signature Certificate not found');
    finally
      SignCert.Free;
    end;
  end;
end;

procedure TX509CertificateAuthority.LoadedResolveResourceFiles;
begin
  inherited;
  if not FIssuedCertsLoaded then
    SetIssuedCertsSCLFile(FIssuedCertsFile);
end;

procedure TX509CertificateAuthority.LoadIssuedCertsFromFile(
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
        FIssuedCertsLoaded := FIssuedCerts.AddCertificate(F,False,StatusCode) >= 0;
      finally
        F.Free;
      end;
    finally
      FS.Free;
    end;
  end else if (Ext = '.p7b') or (Ext = '.p7c') then begin
    FIssuedCerts.ImportFromCMSFile(AFileName,False,Status);
    FIssuedCertsLoaded := crcOK in Status;
  end else begin
    FIssuedCerts.LoadFromFile(AFileName);
    FIssuedCertsLoaded := FIssuedCerts.Count > 0;
  end;
end;

procedure TX509CertificateAuthority.LoadIssuedCertsFromStream(
  AStream: TStream);
begin
  FIssuedCerts.LoadFromStream(AStream);
end;

function TX509CertificateAuthority.NewCertAdvancedDlg(
  AExportData: TStream): Boolean;
var
  Dlg: INewCertDlg;
begin
  StreamSecIIComp.Exceptions := True;
  Dlg := CreateNewCertDlg(nccCert);
  try
    Dlg.PrivateKeyRing := StreamSecIIComp;
    Dlg.MyCerts := MyCerts;
    Dlg.RootCerts := RootCerts;
    Dlg.IssuedCerts := IssuedCerts;
    Dlg.ExportData := AExportData;
    Dlg.SelectWizard(ncwAdvanced);
    Result := Dlg.Execute(Self);
  finally
    StreamSecIIComp.Exceptions := False;
  end;

  if Result then
    InternalWriteMyCerts;
end;

function TX509CertificateAuthority.NewCertFromReqDlg(AImportData,
  AExportData: TStream): Boolean;
var
  Dlg: INewCertDlg;
begin
  StreamSecIIComp.Exceptions := True;
  Dlg := CreateNewCertDlg(nccCertFromReq);
  try
    Dlg.PrivateKeyRing := StreamSecIIComp;
    Dlg.MyCerts := MyCerts;
    Dlg.RootCerts := RootCerts;
    Dlg.IssuedCerts := IssuedCerts;
    Dlg.ImportData := AImportData;
    Dlg.ExportData := AExportData;
    Dlg.SelectWizard(ncwAdvanced);
    Result := Dlg.Execute(Self);
  finally
    StreamSecIIComp.Exceptions := False;
  end;
end;

function TX509CertificateAuthority.NewCodeSigningCertDlg(
  AExportData: TStream): Boolean;
var
  Dlg: INewCertDlg;
begin
  StreamSecIIComp.Exceptions := True;
  Dlg := CreateNewCertDlg(nccCert);
  try
    Dlg.PrivateKeyRing := StreamSecIIComp;
    Dlg.MyCerts := MyCerts;
    Dlg.RootCerts := RootCerts;
    Dlg.ExportData := AExportData;
    Dlg.SelectWizard(ncwCodeSigning);
    Result := Dlg.Execute(Self);
  finally
    StreamSecIIComp.Exceptions := False;
  end;

  if Result then
    InternalWriteMyCerts;
end;

function TX509CertificateAuthority.NewCodeSigningCertReqDlg(
  AExportData: TStream): Boolean;
var
  Dlg: INewCertDlg;
begin
  StreamSecIIComp.Exceptions := True;
  Dlg := CreateNewCertDlg(nccCertReq);
  try
    Dlg.PrivateKeyRing := StreamSecIIComp;
    Dlg.MyCerts := MyCerts;
    Dlg.RootCerts := RootCerts;
    Dlg.ExportData := AExportData;
    Dlg.SelectWizard(ncwCodeSigning,True);
    Result := Dlg.Execute(Self);
  finally
    StreamSecIIComp.Exceptions := False;
  end;
end;

function TX509CertificateAuthority.NewEMailProtectionCertDlg(
  AExportData: TStream): Boolean;
var
  Dlg: INewCertDlg;
begin
  StreamSecIIComp.Exceptions := True;
  Dlg := CreateNewCertDlg(nccCert);
  try
    Dlg.PrivateKeyRing := StreamSecIIComp;
    Dlg.MyCerts := MyCerts;
    Dlg.RootCerts := RootCerts;
    Dlg.ExportData := AExportData;
    Dlg.SelectWizard(ncwEmail);
    Result := Dlg.Execute(Self);
  finally
    StreamSecIIComp.Exceptions := False;
  end;

  if Result then
    InternalWriteMyCerts;
end;

function TX509CertificateAuthority.NewEMailProtectionCertReqDlg(
  AExportData: TStream): Boolean;
var
  Dlg: INewCertDlg;
begin
  StreamSecIIComp.Exceptions := True;
  Dlg := CreateNewCertDlg(nccCertReq);
  try
    Dlg.PrivateKeyRing := StreamSecIIComp;
    Dlg.MyCerts := MyCerts;
    Dlg.RootCerts := RootCerts;
    Dlg.ExportData := AExportData;
    Dlg.SelectWizard(ncwEmail,True);
    Result := Dlg.Execute(Self);
  finally
    StreamSecIIComp.Exceptions := False;
  end;
end;

function TX509CertificateAuthority.NextIssuedSerialNumber(
  const CACert: TASN1Struct): string;
begin
  Result := FIssuedCerts.NextSerialNumber(CACert);
end;

procedure TX509CertificateAuthority.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FIssuedCertsFile then begin
      FIssuedCertsFile.RemoveLoadNotification(Self,DoLoadIssuedCertsSCLFile);
      FIssuedCertsFile := nil;
    end;
  end;
end;

procedure TX509CertificateAuthority.ReadIssuedCerts(AStream: TStream);
begin
  AStream.ReadComponent(FIssuedCertsFile);
end;

function TX509CertificateAuthority.RevokeCertificate(
  const Cert: TASN1Struct; Reason: CRLReason; HoldInstruction: string;
  InvalidityDate, NextCRLUpdate: TDateTime): Boolean;
var
  CACert: PASN1Struct;
  Priv: IMPPrivateKey;
begin
  Result := MyCerts.FindCACert(Cert,CACert);
  if Result then begin
    Priv := StreamSecIIComp.FindCreatePrivateKey(CACert^);
    Result := Assigned(Priv);
    if Result then
      Result := IssuedCerts.RevokeCertificate(Cert,Reason,HoldInstruction,InvalidityDate,NextCRLUpdate,Priv,CACert);
  end;
end;

procedure TX509CertificateAuthority.SaveIssuedCertsToSCLFile(
  AFileName: TFileName);
var
  FS: TFileStream;
begin
  if IssuedCerts.Count > 0 then begin
    FS := TFileStream.Create(AFileName,fmCreate);
    try
      SaveIssuedCertsToSCLStream(FS);
    finally
      FS.Free;
    end;
  end;
end;

procedure TX509CertificateAuthority.SaveIssuedCertsToSCLStream(
  AStream: TStream);
begin
  InternalWriteIssuedCerts;
  AStream.CopyFrom(InternalIssuedCertsFile.DataStream,0);
end;

procedure TX509CertificateAuthority.SetCertificateCollections(
  Index: Integer; const Value: TCertificateCollection);
begin
  if Index = 3 then
    GetIssuedCertList.Assign(Value)
  else
    inherited SetCertificateCollections(Index,Value);
end;

procedure TX509CertificateAuthority.SetIssuedCertList(
  const Value: TCertificateCollection);
begin
  FIssuedCerts.CertList := Value;
end;

procedure TX509CertificateAuthority.SetIssuedCertsGroupIndex(
  const Value: Integer);
begin
  FIssuedCerts.GroupIndex := Value;
end;

procedure TX509CertificateAuthority.SetIssuedCertsSCLFile(
  const Value: TResourceFile);
var
  Loading: Boolean;
begin
  if Assigned(Value) then begin
    FreeNotification(Value);
    Value.AddLoadNotification(Self,DoLoadIssuedCertsSCLFile);
    Loading := csLoading in ComponentState;
    if Assigned(Owner) and not Loading then
      Loading := csLoading in Owner.ComponentState;
    if (not Loading) and
       Assigned(Value.DataStream) and (Value.DataStream.Size > 0) then begin
      LoadIssuedCertsFromStream(Value.DataStream);
      IssuedCertsSource := nil;
    end;
  end;
end;

procedure TX509CertificateAuthority.SetIssuedCertsSource(
  const Value: TCustomX509TrustedCertificates);
begin
  FIssuedCerts.CertSource := Value;
end;

procedure TX509CertificateAuthority.SetStoreIssuedCerts(
  const Value: Boolean);
begin
  FStoreIssuedCerts := Value;
end;

function TX509CertificateAuthority.Sign(Signed: ISigned;
  PublicKey: IPublicKeyInfo; HA: THashAlgorithm): Boolean;
begin
  Result := StreamSecIIComp.Sign(Signed,PublicKey,HA);
end;

function TX509CertificateAuthority.SignBuf(const Buf; BufLen: Integer;
  PKI: Pointer; PKILen: Integer; HA: THashAlgorithm; var Sign;
  var SignLen: Integer): Boolean;
begin
  Result := StreamSecIIComp.SignBuf(Buf,BufLen,PKI,PKILen,HA,Sign,SignLen);
end;

function TX509CertificateAuthority.SignCertificate(DstCert,
  CACert: TASN1Struct; var Status: TCertStatusCode;
  HA: THashAlgorithm): Boolean;
var
  Idx: Integer;
begin
  if CACert = nil then begin
    Idx := MyCerts.Count - 1;
    while Idx >= 0 do begin
      CACert := MyCerts.Certs[Idx];
      if keyCertSign in ExtractKeyUsage(CACert) then
        if StreamSecIIComp.ValidateMyCert(CACert) then
          Break;
      CACert := nil;
      Dec(Idx);
    end;
  end;
  Result := StreamSecIIComp.SignCertificate(DstCert,CACert,HA);
  if Result then begin
    Idx := IssuedCerts.AddCertificate(DstCert,False,Status);
    Result := Idx >= 0;
    if Result and not FStoreIssuedCerts then
      IssuedCerts.RemoveCertificate(DstCert);
  end else
    Status := crcInvalidSignature;
end;

function TX509CertificateAuthority.SignCertificate(DstCert: TCertificate;
  CACert: TASN1Struct; var Status: TCertStatusCode;
  HA: THashAlgorithm): Boolean;
var
  Idx: Integer;
begin
  if CACert = nil then begin
    Idx := MyCerts.Count - 1;
    while Idx >= 0 do begin
      CACert := MyCerts.Certs[Idx];
      if keyCertSign in ExtractKeyUsage(CACert) then
        if StreamSecIIComp.ValidateMyCert(CACert) then
          Break;
      CACert := nil;
      Dec(Idx);
    end;
  end;
  Result := StreamSecIIComp.SignCertificate(DstCert,CACert,HA);
  if Result then begin
    Idx := IssuedCerts.AddCertificate(DstCert.Data,False,Status);
    Result := Idx >= 0;
    if Result and not FStoreIssuedCerts then
      IssuedCerts.RemoveCertificate(DstCert.Data);
  end else
    Status := crcInvalidSignature;
end;

function TX509CertificateAuthority.SignCertificate(
  CertReq: TCertificationRequest; DstCert: TCertificate;
  CACert: TASN1Struct; var Status: TCertStatusCode;
  HA: THashAlgorithm): Boolean;
var
  Crt: TASN1Struct;
  Validity: TX509Validity;
  Ext: TX509Extension;
  Idx: Integer;
begin
  if CACert = nil then begin
    Idx := MyCerts.Count - 1;
    while Idx >= 0 do begin
      CACert := MyCerts.Certs[Idx];
      if keyCertSign in ExtractKeyUsage(CACert) then
        if StreamSecIIComp.ValidateMyCert(CACert) then
          Break;
      CACert := nil;
      Dec(Idx);
    end;
  end;
  Crt := nil;
  try
    // Validity:
    if DstCert.Validity.NotBefore.Choice <> teUndefined then
      Validity.notBefore := DstCert.Validity.NotBefore.AsUtcTime.AsDateTime
    else
      Validity.notBefore := Trunc(Now - HoursOffsetFromGMT/24);
    if DstCert.Validity.NotAfter.Choice <> teUndefined then
      Validity.notAfter := DstCert.Validity.NotAfter.AsUtcTime.AsDateTime
    else
      Validity.notAfter := Trunc(Now + 365 - HoursOffsetFromGMT/24);
    // Requested fields:
    ComposeCertificateCR(Crt,
                         CertReq.Data,
                         CACert,
                         Validity,
                         IssuedCerts.NextSerialNumber(CACert));
    // Extension override:
    Ext := nil;
    try
      if ExtractNamedExtension(DstCert.Data,id_ce_cRLDistributionPoints,Ext) <> E_OK then
        if ExtractNamedExtension(CACert,id_ce_cRLDistributionPoints,Ext) = E_OK then
          ImposeExtension(Ext,Crt);
      Idx := 0;
      while ExtractExtension(DstCert.Data,Idx,Ext) = E_OK do begin
        ImposeExtension(Ext,Crt);
        Inc(Idx);
      end;
    finally
      Ext.Free;
    end;
    DstCert.TbsCertificate.AssignStruct(Crt.Items[0]^);
  finally
    Crt.Free;
  end;
  Result := SignCertificate(DstCert,CACert,Status,HA);
end;

function TX509CertificateAuthority.SignSigned(Signed: TSigned;
  CACert: IPublicKeyInfo; HA: THashAlgorithm): Boolean;
begin
  Result := StreamSecIIComp.SignSigned(Signed,CACert,HA);
end;

procedure TX509CertificateAuthority.UpdateHijacker(
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
    AddCerts(IssuedCerts);
    AddCRLs(MyCerts);
    AddCRLs(RootCerts);
    AddCRLs(IssuedCerts);
    TCustomX509TrustedCertificatesHack(AHijacker).CertListLoaded;
    TCustomX509TrustedCertificatesHack(AHijacker).UpdateHijackers;
  end;
end;

procedure TX509CertificateAuthority.WriteIssuedCerts(AStream: TStream);
begin       
  InternalWriteIssuedCerts;
  AStream.WriteComponent(FIssuedCertsFile);
end;

end.
