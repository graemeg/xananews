{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     PKCS12 Unit                                       }
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
{$B-}
{$I ver.inc}
unit Pkcs12;

interface

uses
  SecUtils, Asn1, Kdf, Pkix;

type
  TPKCS12KDF = class(TPBKDF,IPBKDF)
  protected
{$IFDEF BCB}
    class function ID: Byte; virtual;
{$ELSE}
    class function ID: Byte; virtual; abstract;
{$ENDIF}
    procedure SetHashClass(const Value: THashClass); override;
  public
    procedure DeriveKey(Password, Salt: ISecretKey;
                        Iter, DKLen: Integer); override;
  end;

  TPKCS12KDF1 = class(TPKCS12KDF,IPBKDF)
  protected
    class function ID: Byte; override;
  end;

  TPKCS12KDF2 = class(TPKCS12KDF,IPBKDF)
  protected
    class function ID: Byte; override;
  end;

  TPKCS12KDF3 = class(TPKCS12KDF,IPBKDF)
  protected
    class function ID: Byte; override;
  end;

  TPKCS12Pbe = (pbeNone,pbeRC4_128,pbeRC4_40,pbe3DES_192,pbe3DES_128,pbeRC2_128,
                pbeRC2_40);

function ExtractAuthSafeContent(PDU: TASN1Struct; Password: ISecretKey;
  var AuthenticatedSafe: TASN1Struct): Boolean;
function ExtractSafeContents(ContentInfo: TASN1Struct; Password: ISecretKey;
  var SafeContents: TASN1Struct): Boolean;
function ExtractCertificate(SafeBag: TASN1Struct; var Cert: TASN1Struct;
  var LocalKeyId: string): Boolean;       
function ExtractCRL(SafeBag: TASN1Struct; var CRL: TASN1Struct): Boolean;
function ExtractPrivateKeyInfo(SafeBag: TASN1Struct; Password: ISecretKey;
  var PrivateKeyInfo: TASN1Struct; var FriendlyName, LocalKeyID: string): Boolean;
function ExtractRSAPrivateKey(PrivateKeyInfo: TASN1Struct;
  var RSAPrivateKey: TASN1Struct): Boolean;

procedure ImposeAuthSafeContent(AuthenticatedSafe: TASN1Struct;
  Password: ISecretKey; Iter: Integer; var PDU: TASN1Struct);
procedure ImposeSafeContents(SafeContents: TASN1Struct;
  Password: ISecretKey; Iter: Integer; PbeAlg: TPKCS12Pbe;
  var ContentInfo: TASN1Struct);
procedure ImposeCertificate(Cert: TASN1Struct; FriendlyName, LocalKeyID: string;
  var SafeBag: TASN1Struct);
procedure ImposeCRL(CRL: TASN1Struct; var SafeBag: TASN1Struct);
procedure ImposePrivateKeyInfo(PrivateKeyInfo: TASN1Struct;
  Password: ISecretKey; FriendlyName, LocalKeyID, ProviderName: string;
  Iter: Integer; PbeAlg: TPKCS12Pbe; var SafeBag: TASN1Struct);
procedure ImposeRSAPrivateKey(RSAPrivateKey: TASN1Struct;
  KeyUsage: TKeyUsage; var PrivateKeyInfo: TASN1Struct);

implementation

uses
  SysUtils, Cms, MpYarrow, SsArc2, SsArc4, SsDes;

{ TPKCS12KDF }

procedure TPKCS12KDF.DeriveKey(Password, Salt: ISecretKey; Iter,
  DKLen: Integer);
var
  P, S, D, A, B: ISecretKey;
  I, J: Integer;
  H: THash;
begin
  Assert(Assigned(Password) and Assigned(Salt) and (Iter > 0) and (DKLen > 0));
  P := TSecretKey.Create('');
  if Password.KeyLen > 0 then begin
    P.SetLength(Password.KeyLen + 64 - (Password.KeyLen and 63));
    J := 0;
    while J < P.KeyLen do begin
      P.SetTruncKeyAt(Password,J);
      Inc(J,Password.KeyLen);
    end;
  end;

  S := TSecretKey.Create('');
  if Salt.KeyLen > 0 then begin
    S.SetLength(Salt.KeyLen + 64 - (Salt.KeyLen and 63));
    J := 0;
    while J < S.KeyLen do begin
      S.SetTruncKeyAt(Salt,J);
      Inc(J,Salt.KeyLen);
    end;
  end;

  D := TSecretKey.Create('');
  D.SetLength(64);
  FillChar(D.Key^,64,ID);

  A := TSecretKey.Create('');
  A.SetLength(HashClass.DigestSize);
  B := TSecretKey.Create('');
  B.SetLength(64);

  SetLength(DKLen);

  H := HashClass.Create(nil^,0);
  try
    I := 0;
    while I < DKLen do begin
      H.SetUp;
      H.HashData(D.Key^,D.KeyLen);
      H.HashData(S.Key^,S.KeyLen);
      H.HashData(P.Key^,P.KeyLen);
      H.Done(A.Key);
      for J := 1 to Iter-1 do begin
        H.SetUp;
        H.HashData(A.Key^,A.KeyLen);
        H.Done(A.Key);
      end;
      SetTruncKeyAt(A,I);
      J := 0;
      while J < B.KeyLen do begin
        B.SetTruncKeyAt(A,J);
        Inc(J,A.KeyLen);
      end;
      J := 0;
      while J < S.KeyLen do begin
        S.AddTruncKeyAt(B,J,1);
        Inc(J,B.KeyLen);
      end;
      J := 0;
      while J < P.KeyLen do begin
        P.AddTruncKeyAt(B,J,1);
        Inc(J,B.KeyLen);
      end;
      Inc(I,HashClass.DigestSize);
    end;
    A := nil;
    B := nil;
    S := nil;
    P := nil;
    D := nil;
  finally
    H.Free;
    Assert((A = nil) and (B = nil) and (S = nil) and (P = nil) and (D = nil));
  end;
end;             

{$IFDEF BCB}
class function TPKCS12KDF.ID: Byte;
begin
  Result := 0; // Must override
end;
{$ENDIF}

procedure TPKCS12KDF.SetHashClass(const Value: THashClass);
{$IFOPT C+}
var
  OK: Boolean;
{$ENDIF C+}
begin
{$IFOPT C+}
  OK := False;
{$IFDEF SHA1}
  OK := OK or (Value = TSHA1);
{$ENDIF SHA1}
{$IFDEF MD5}
  OK := OK or (Value = TMD5);
{$ENDIF MD5}
{$IFDEF MD2}
  OK := OK or (Value = TMD2);
{$ENDIF MD2}
  Assert(OK,'Hash algorithm not support by this KDF');
{$ENDIF C+}
  inherited;
end;

{ TPKCS12KDF1 }

class function TPKCS12KDF1.ID: Byte;
begin
  Result := 1;
end;

{ TPKCS12KDF2 }

class function TPKCS12KDF2.ID: Byte;
begin
  Result := 2;
end;

{ TPKCS12KDF3 }

class function TPKCS12KDF3.ID: Byte;
begin
  Result := 3;
end;

function ExtractAuthSafeContent(PDU: TASN1Struct; Password: ISecretKey;
  var AuthenticatedSafe : TASN1Struct): Boolean;
var
  Salt: string;
  pContent, pAuthSafe, pMacData, pDigestInfo, pMac: PASN1Struct;
  Iter: Integer;
  Mac: ISecretKey;
  PBKDF: IPBKDF;
  HC: THashClass;
  ContentType: TPKCS7DataType;
  OID: string;
  HashAlg: THashAlgorithm;
begin
  HC := nil;
  pContent := nil;
  pMac := nil;
  Iter := 1;
  Result := Assigned(PDU) and PDU.Constructed and (PDU.ItemCount > 2);
  if Result then begin
    pAuthSafe := PDU.Items[1];
    Result := Cms.CheckCMSSyntax(PAuthSafe^,ContentType,[pData]);
    if Result then begin
      pContent := pAuthSafe^.Items[1]^.Items[0];
      pMacData := PDU.Items[2];
      Result := pMacData.Constructed and (pMacData.ItemCount >= 2);
      if Result then begin
        pDigestInfo := pMacData.Items[0];
        Result := pDigestInfo.Constructed and (pDigestInfo.ItemCount = 2);
        if Result then begin
          Result := (pMacData.Items[1]^.Tag = V_ASN1_OCTET_STRING) and
                    (pMacData.Items[1]^.Cls = V_ASN1_UNIVERSAL) and
                    (not pMacData.Items[1]^.Constructed) and
                    (pDigestInfo^.Items[0].Constructed) and
                    (pDigestInfo^.Items[0].ItemCount > 0) and
                    (pDigestInfo^.Items[0]^.Items[0]^.Tag = V_ASN1_OBJECT) and
                    (pDigestInfo^.Items[0]^.Items[0]^.Cls = V_ASN1_UNIVERSAL) and
                    not (pDigestInfo^.Items[0]^.Items[0]^.Constructed);
          if Result then begin
            Salt := pMacData.Items[1]^.ContentAsOctetString;
            if pMacData.ItemCount > 2 then
              Iter := pMacData.Items[2]^.ContentAsInteger;
            pMac := pDigestInfo^.Items[1];
            OID := pDigestInfo^.Items[0]^.Items[0].ContentAsOID;
            Result := Pkix.OIDToHashAlgorithm(OID,HashAlg);
            if Result then begin
              HC := SecUtils.FindHashClass(HashAlg);
              Result := Assigned(HC) and (HC.DigestSize = pMac.Length);
            end;
          end;
        end;
      end;
    end;
  end;
  if Result then begin
    PBKDF := Pkcs12.TPKCS12KDF3.Create('');
    PBKDF.HashClass := HC;
    PBKDF.DeriveKey(Password,
                    TSecretKey.CreateStr(PChar(Salt),Length(Salt)),
                    Iter,
                    HC.DigestSize);
    HMac(PBKDF,pContent.Content^,pContent.Length,HashAlg,HC.DigestSize,Mac);
    Result := CompareMem(Mac.Key,pMac.Content,HC.DigestSize);
  end;
  if Result then
    pContent.ContentAsASN1Struct(AuthenticatedSafe);
end;

procedure ImposeAuthSafeContent(AuthenticatedSafe: TASN1Struct;
  Password: ISecretKey; Iter: Integer; var PDU: TASN1Struct);
var
  pAuthSafe, pContent: PASN1Struct;
{$IFDEF SHA1}
  pMacData, pMac: PASN1Struct;
  Salt, Mac: ISecretKey;
  PBKDF: IPBKDF;
{$ENDIF SHA1}
begin
  Assert(Assigned(AuthenticatedSafe));
  PDU.Free;
  PDU := TASN1Struct.Create;
  PDU.Tag := V_ASN1_SEQUENCE;
  PDU.Cls := V_ASN1_UNIVERSAL;
  PDU.Constructed := True;
  PDU.AddField('version','INTEGER',nil).EditContent(Integer(3));
  pAuthSafe := PDU.AddField('authSafe','SEQUENCE',nil);
  pAuthSafe.AddField('contentType','OBJECT',id_data,True);
  pContent := pAuthSafe.AddField('content','[0] OCTET STRING',nil);
  AuthenticatedSafe.CalculateLength;
  pContent.EditContent(AuthenticatedSafe);
  if Assigned(Password) then begin
{$IFDEF SHA1}
    pMacData := PDU.AddField('macData','SEQUENCE',nil);
    pMac := pMacData.AddField('mac','SEQUENCE',nil);
    pMac.AddField('digestAlgorithm','SEQUENCE',nil).AddField('algorithm','OBJECT',id_sha1,True);
    RawRandomIntf(Salt,160);
    pMacData.AddField('macSalt','OCTET STRING',nil).SetContent(Salt.Key^,20);
    if Iter <> 1 then
      pMacData.AddField('iterations','INTEGER',nil).EditContent(Iter);
    PBKDF := TPKCS12KDF3.Create('');
    PBKDF.HashClass := TSHA1;
    PBKDF.DeriveKey(Password,Salt,Iter,20);
    HMac(PBKDF,pContent.Content^,pContent.Length,haSHA1,20,Mac);
    pMac.AddField('digest','OCTET STRING',nil).SetContent(Mac.Key^,Mac.KeyLen);
{$ELSE  SHA1}
    raise Exception.Create('ImposeAuthSafeContent: Action requires SHA-1')
{$ENDIF SHA1}
  end;
  PDU.CalculateLength;
end;

function PKCS12Keys(PbeAlg: TPKCS12Pbe; Password, Salt: ISecretKey; Iter: Integer;
  var CKey, IV: IPBKDF; var CC: TCipherClass; var OID: string): Boolean;
begin
  Result := True;
  CKey := TPKCS12KDF1.Create('');
  IV := TPKCS12KDF2.Create('');
  CC := nil;
  case PbeAlg of
    pbeRC4_128:
      begin
        OID := pkcs_12PbeIds + '.1';
        CC := TARC4;
        CKey.DeriveKey(Password,Salt,Iter,16);
      end;
    pbeRC4_40:
      begin
        OID := pkcs_12PbeIds + '.2';
        CC := TARC4;
        CKey.DeriveKey(Password,Salt,Iter,5);
      end;
    pbe3DES_192:
      begin
        OID := pkcs_12PbeIds + '.3';
        CC := T3DES_CBC;
        CKey.DeriveKey(Password,Salt,Iter,24);
        IV.DeriveKey(Password,Salt,Iter,8);
      end;
    pbe3DES_128:
      begin
        OID := pkcs_12PbeIds + '.4';
        CC := T3DES_CBC;
        CKey.DeriveKey(Password,Salt,Iter,16);
        IV.DeriveKey(Password,Salt,Iter,8);
      end;
{$IFDEF ARCTWO}
    pbeRC2_128:
      begin
        OID := pkcs_12PbeIds + '.5';
        CC := TARC2_CBC;
        CKey.DeriveKey(Password,Salt,Iter,16);
        IV.DeriveKey(Password,Salt,Iter,8);
      end;
    pbeRC2_40:
      begin
        OID := pkcs_12PbeIds + '.6';
        CC := TARC2_CBC;
        CKey.DeriveKey(Password,Salt,Iter,5);
        IV.DeriveKey(Password,Salt,Iter,8);
      end;
{$ENDIF ARCTWO}
  else
    Result := False;
  end;
end;

function TranslateEncryptionAlgorithmIdentifier(EncryptionAlgId: TASN1Struct;
  Password: ISecretKey; var CKey, IV: IPBKDF; var CC: TCipherClass): Boolean;
var
  EncAlgParam: PASN1Struct;
  Iter: Integer;
  OID, SubId: string;
  Salt: ISecretKey;
begin
  OID := EncryptionAlgId.Items[0].ContentAsOID;
  Result := StrLComp(pkcs_12PbeIds,PChar(OID),Length(pkcs_12PbeIds)) = 0;
  if Result then begin
    SubId := Copy(OID,1 + Length(pkcs_12PbeIds),MaxInt);
    Result := (Length(SubId) = 2);
    if Result then begin
      EncAlgParam := EncryptionAlgId.Items[1];
      Iter := 1;
      if EncAlgParam.Constructed and
         (EncAlgParam.ItemCount > 0) then begin
        Salt := TSecretKey.CreateStr(EncAlgParam.Items[0].Content,EncAlgParam.Items[0].Length);
        if EncAlgParam.ItemCount > 1 then
          Iter := EncAlgParam.Items[1].ContentAsInteger;
      end else
        Salt := TSecretKey.Create('');
      Result := PKCS12Keys(TPKCS12Pbe(StrToInt(SubId[2])),Password,Salt,Iter,CKey,IV,CC,OID);
    end;
  end;
end;

function ExtractSafeContents(ContentInfo: TASN1Struct; Password: ISecretKey;
  var SafeContents: TASN1Struct): Boolean;
var
  ContentType: TPkcs7DataType;
  EncCnt: PASN1Struct;
  CKey: IPBKDF;
  IV: IPBKDF;
  CC: TCipherClass;
begin
  Result := Assigned(ContentInfo) and
            Cms.CheckCMSSyntax(ContentInfo,ContentType,[pData,pEncryptedData]);
  if Result then begin
    if ContentType = pData then
      ContentInfo.Items[1].Items[0].ContentAsASN1Struct(SafeContents)
    else begin
      EncCnt := ContentInfo.Items[1].Items[0].Items[1];
      Result := Assigned(Password);
      if Result then begin
        Result := TranslateEncryptionAlgorithmIdentifier(EncCnt^.Items[1]^,
                                                         Password,
                                                         CKey,IV,CC);
        if Result then begin
          with CC.CreateIntf(CKey) do try
            if IV.KeyLen > 0 then
              SetVectorBuf(IV.Key^,IV.KeyLen);
            Decrypt(EncCnt.Items[2].Content^,EncCnt.Items[2].Length);
          finally
            Free;
          end;
          EncCnt.Items[2].ContentAsASN1Struct(SafeContents);
        end;
      end;
    end;
  end;
end;

procedure ImposeSafeContents(SafeContents: TASN1Struct;
  Password: ISecretKey; Iter: Integer; PbeAlg: TPKCS12Pbe;
  var ContentInfo: TASN1Struct);
var
  Salt: ISecretKey;
  EncCnt, EncAlgParam: PASN1Struct;
  OID: string;
  CKey: IPBKDF;
  IV: IPBKDF;
  CC: TCipherClass;
begin
  Assert(Assigned(SafeContents));
  if PbeAlg = pbeNone then begin
    NewDataStruct(ContentInfo);
    ContentInfo.Items[1].Items[0].EditContent(SafeContents);
  end else begin
    Assert(Assigned(Password) and (Iter > 0));
    NewEncryptedDataStruct(ContentInfo);
    ContentInfo.Items[1].Items[0].Items[0].EditContent(Integer(0));

    EncCnt := ContentInfo.Items[1].Items[0].Items[1];
    EncCnt.Items[0].EditContent(id_data);

    EncAlgParam := EncCnt.Items[1].Items[1];

    RawRandomIntf(Salt,64);

    Assert(PKCS12Keys(PbeAlg,Password,Salt,Iter,CKey,IV,CC,OID));

    EncCnt.Items[1].Items[0].EditContent(OID);

    EncAlgParam.DisposeContent;
    EncAlgParam.Tag := V_ASN1_SEQUENCE;
    EncAlgParam.Cls := V_ASN1_UNIVERSAL;
    EncAlgParam.Constructed := True;
    EncAlgParam.AddField('salt','OCTET STRING',nil).SetContent(Salt.Key^,Salt.KeyLen);
    if Iter > 1 then
      EncAlgParam.AddField('iterations','INTEGER',nil).EditContent(Iter);

    if Assigned(IV) and (IV.KeyLen > 0) then
      EncCnt.Items[2].EditContent(SafeContents,IV.KeyLen)
    else
      EncCnt.Items[2].EditContent(SafeContents);

    with CC.CreateIntf(CKey) do try
      if IV.KeyLen > 0 then
        SetVectorBuf(IV.Key^,IV.KeyLen);
      Encrypt(EncCnt.Items[2].Content^,EncCnt.Items[2].Length);
    finally
      Free;
    end;
  end;
  ContentInfo.CalculateLength;
end;

function ExtractPrivateKeyInfo(SafeBag: TASN1Struct; Password: ISecretKey;
  var PrivateKeyInfo: TASN1Struct; var FriendlyName, LocalKeyID: string): Boolean;
var
  BagId: string;
  CKey: IPBKDF;
  IV: IPBKDF;
  CC: TCipherClass;

  function TranslateAttributes: Boolean;
  var
    Attrs, Attr: PASN1Struct;
    AttrID: string;
    I: Integer;
  begin           
    LocalKeyId := '';
    FriendlyName := '';
    Result := True;
    if SafeBag.ItemCount > 2 then begin
      Attrs := SafeBag.Items[2];
      Result := Attrs.Constructed;
      if Result then
        for I := 0 to Attrs.ItemCount - 1 do begin
          Attr := Attrs.Items[I];
          Result := Attr.Constructed and (Attr.ItemCount = 2) and
                    (Attr.Items[0].Tag = V_ASN1_OBJECT) and
                    (Attr.Items[1].Tag = V_ASN1_SET);
          if not Result then Break;
          AttrID := Attr.Items[0].ContentAsOID;
          if AttrID = pkcs_9_at_friendlyName then begin
            if Attr.Items[1].ItemCount > 0 then
              FriendlyName := Attr.Items[1].Items[0].ContentAsString;
          end else if AttrID = pkcs_9_at_localKeyID then begin
            if Attr.Items[1].ItemCount > 0 then
              LocalKeyID := Attr.Items[1].Items[0].ContentAsOctetString;
          end;
        end;
    end;
  end;

begin
  Result := Assigned(SafeBag) and SafeBag.Constructed and
            (SafeBag.ItemCount >= 2) and TranslateAttributes;
  if Result then begin
    BagId := SafeBag.Items[0].ContentAsOID;
    if BagId = pkcs_12 + '.10.1.1' then begin
      SafeBag.Items[1].Items[0].ContentAsASN1Struct(PrivateKeyInfo);
    end else if BagId = pkcs_12 + '.10.1.2' then begin
      Result := TranslateEncryptionAlgorithmIdentifier(SafeBag.Items[1].Items[0].Items[0]^,
                                                       Password,
                                                       CKey,IV,CC);
      if Result then begin
        with CC.CreateIntf(CKey) do try
          if IV.KeyLen > 0 then
            SetVectorBuf(IV.Key^,IV.KeyLen);
          Decrypt(SafeBag.Items[1].Items[0].Items[1].Content^,
                  SafeBag.Items[1].Items[0].Items[1].Length);
        finally
          Free;
        end;
        SafeBag.Items[1].Items[0].Items[1].ContentAsASN1Struct(PrivateKeyInfo);
      end;
    end else
      Result := False;
  end;
end;

function ExtractCertificate(SafeBag: TASN1Struct; var Cert: TASN1Struct;
  var LocalKeyId: string): Boolean;
var
  CertBag: PASN1Struct; 

  function TranslateAttributes: Boolean;
  var
    Attrs, Attr: PASN1Struct;
    AttrID: string;
    I: Integer;
  begin
    LocalKeyId := '';
    Result := True;
    if SafeBag.ItemCount > 2 then begin
      Attrs := SafeBag.Items[2];
      Result := Attrs.Constructed;
      if Result then
        for I := 0 to Attrs.ItemCount - 1 do begin
          Attr := Attrs.Items[I];
          Result := Attr.Constructed and (Attr.ItemCount = 2) and
                    (Attr.Items[0].Tag = V_ASN1_OBJECT) and
                    (Attr.Items[1].Tag = V_ASN1_SET);
          if not Result then Break;
          AttrID := Attr.Items[0].ContentAsOID;
          if AttrID = pkcs_9_at_localKeyID then begin
            if Attr.Items[1].ItemCount > 0 then
              LocalKeyID := Attr.Items[1].Items[0].ContentAsOctetString;
          end;
        end;
    end;
  end;

begin
  Result := Assigned(SafeBag) and (SafeBag.Constructed) and (SafeBag.ItemCount >= 2);
  if Result then begin
    Result := SafeBag.Items[0].ActualTag = V_ASN1_OBJECT;
    if Result then begin
      Result := SafeBag.Items[0].ContentAsOID = pkcs_12 + '.10.1.3';
      if Result then begin
        CertBag := SafeBag.Items[1].Items[0];
        Result := CertBag.Items[0].ContentAsOID = pkcs_9_certTypes + '.1';
        if Result then begin
          CertBag.Items[1].Items[0].ContentAsASN1Struct(Cert);
          Result := TranslateAttributes;
        end;
      end;
    end;
  end;
end;

function ExtractCRL(SafeBag: TASN1Struct; var CRL: TASN1Struct): Boolean;
var
  CRLBag: PASN1Struct;
begin
  Result := Assigned(SafeBag) and (SafeBag.Constructed) and (SafeBag.ItemCount >= 2);
  if Result then begin
    Result := SafeBag.Items[0].ActualTag = V_ASN1_OBJECT;
    if Result then begin
      Result := SafeBag.Items[0].ContentAsOID = pkcs_12 + '.10.1.4';
      if Result then begin
        CRLBag := SafeBag.Items[1].Items[0];
        Result := CRLBag.Items[0].ContentAsOID = pkcs_9_certTypes + '.1';
        if Result then
          CRLBag.Items[1].Items[0].ContentAsASN1Struct(CRL);
      end;
    end;
  end;
end;

procedure ImposeCertificate(Cert: TASN1Struct; FriendlyName, LocalKeyID: string;
  var SafeBag: TASN1Struct);
var
  CertBag: PASN1Struct;

  procedure ImposeAttributes;
  var
    Attrs, Attr: PASN1Struct;
  begin
    Attrs := SafeBag.AddField('bagAttributes','SET',nil);
    if LocalKeyID <> '' then begin
      Attr := Attrs.AddField('','SEQUENCE',nil);
      Attr.AddField('attrID','OBJECT',pkcs_9_at_localKeyID,True);
      Attr.AddField('attrValue','SET',nil);
      Attr.Items[1].AddField('','OCTET STRING',nil).SetContent(Pointer(LocalKeyID)^,Length(LocalKeyID));
    end;
    if FriendlyName <> '' then begin
      Attr := Attrs.AddField('','SEQUENCE',nil);
      Attr.AddField('attrID','OBJECT',pkcs_9_at_friendlyName,True);
      Attr.AddField('attrValue','SET',nil);
      Attr.Items[1].AddField('','BMPString',friendlyName,True);
    end;
  end;

begin
  Assert(Assigned(Cert));
  NewComposeASN1Struct(SafeBag,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  SafeBag.AddField('bagId','OBJECT',pkcs_12 + '.10.1.3',True);
  CertBag := SafeBag.AddField('bagValue','[0] SEQUENCE',nil);
  CertBag.AddField('certId','OBJECT',pkcs_9_certTypes + '.1',True);
  CertBag.AddField('certValue','[0] OCTET STRING',nil).EditContent(Cert);
  ImposeAttributes;
  SafeBag.CalculateLength;
end;

procedure ImposeCRL(CRL: TASN1Struct; var SafeBag: TASN1Struct);
var
  CRLBag: PASN1Struct;
begin
  Assert(Assigned(CRL));
  NewComposeASN1Struct(SafeBag,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  SafeBag.AddField('bagId','OBJECT',pkcs_12 + '.10.1.4',True);
  CRLBag := SafeBag.AddField('bagValue','[0] SEQUENCE',nil);
  CRLBag.AddField('cRLId','OBJECT',pkcs_9_certTypes + '.1',True);
  CRLBag.AddField('cRLValue','[0] OCTET STRING',nil).EditContent(CRL);
  SafeBag.CalculateLength;
end;

procedure ImposePrivateKeyInfo(PrivateKeyInfo: TASN1Struct;
  Password: ISecretKey; FriendlyName, LocalKeyID, ProviderName: string; Iter: Integer;
  PbeAlg: TPKCS12Pbe; var SafeBag: TASN1Struct);
var
  Salt: ISecretKey;
  EncCnt, EncAlgParam: PASN1Struct;
  OID: string;
  CKey: IPBKDF;
  IV: IPBKDF;
  CC: TCipherClass;

  procedure ImposeAttributes;
  var
    Attrs, Attr: PASN1Struct;
  begin
    Attrs := SafeBag.AddField('bagAttributes','SET',nil);
    if LocalKeyID <> '' then begin
      Attr := Attrs.AddField('','SEQUENCE',nil);
      Attr.AddField('attrID','OBJECT',pkcs_9_at_localKeyID,True);
      Attr.AddField('attrValue','SET',nil);
      Attr.Items[1].AddField('','OCTET STRING',nil).SetContent(Pointer(LocalKeyID)^,Length(LocalKeyID));
    end;
    if FriendlyName <> '' then begin
      Attr := Attrs.AddField('','SEQUENCE',nil);
      Attr.AddField('attrID','OBJECT',pkcs_9_at_friendlyName,True);
      Attr.AddField('attrValue','SET',nil);
      Attr.Items[1].AddField('','BMPString',friendlyName,True);
    end;
    Attr := Attrs.AddField('','SEQUENCE',nil);
    Attr.AddField('attrID','OBJECT','1.3.6.1.4.1.311.17.1',True);
    Attr.AddField('attrValue','SET',nil);
    Attr.Items[1].AddField('','BMPString',ProviderName,True);
  end;

begin
  Assert(Assigned(PrivateKeyInfo));
  NewComposeASN1Struct(SafeBag,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  if PbeAlg = pbeNone then begin
    SafeBag.AddField('bagID','OBJECT',pkcs_12 + '.10.1.1',True);
    SafeBag.AddField('bagValue','[0] SEQUENCE',PrivateKeyInfo);
  end else begin
    SafeBag.AddField('bagID','OBJECT',pkcs_12 + '.10.1.2',True);
    EncCnt := SafeBag.AddField('bagValue','[0] SEQUENCE',nil);
    EncCnt.AddField('encryptionAlgorithm','SEQUENCE',nil);
    RawRandomIntf(Salt,64);
    PKCS12Keys(PbeAlg,Password,Salt,Iter,CKey,IV,CC,OID);
    EncCnt.Items[0].AddField('algorithm','OBJECT',OID,True);
    EncAlgParam := EncCnt.Items[0].AddField('params','SEQUENCE',nil);
    EncAlgParam.AddField('salt','OCTET STRING',nil).SetContent(Salt.Key^,Salt.KeyLen);
    EncAlgParam.AddField('iterations','INTEGER',nil).EditContent(Iter);

    EncCnt.AddField('encryptedContent','OCTET STRING',nil);
    if Assigned(IV) and (IV.KeyLen > 0) then
      EncCnt.Items[1].EditContent(PrivateKeyInfo,IV.KeyLen)
    else
      EncCnt.Items[1].EditContent(PrivateKeyInfo);
    with CC.CreateIntf(CKey) do try
      if IV.KeyLen > 0 then
        SetVectorBuf(IV.Key^,IV.KeyLen);
      Encrypt(EncCnt.Items[1].Content^,EncCnt.Items[1].Length);
    finally
      Free;
    end;
  end;
  ImposeAttributes;
  SafeBag.CalculateLength;
end;

function ExtractRSAPrivateKey(PrivateKeyInfo: TASN1Struct;
  var RSAPrivateKey: TASN1Struct): Boolean;
begin
  Result := Assigned(PrivateKeyInfo) and PrivateKeyInfo.Constructed and
            (PrivateKeyInfo.ItemCount >= 2);
  if Result then begin
    Result := PrivateKeyInfo.Items[0].ContentAsInteger = 0;
    if Result then begin
      Result := PrivateKeyInfo.Items[1].Constructed and
                (PrivateKeyInfo.Items[1].ItemCount > 0) and
                (PrivateKeyInfo.Items[1].Items[0].ContentAsOID = rsaEncryption);
      if Result then
        PrivateKeyInfo.Items[2].ContentAsASN1Struct(RSAPrivateKey);
    end;
  end;
end;

procedure ImposeRSAPrivateKey(RSAPrivateKey: TASN1Struct;
  KeyUsage: TKeyUsage; var PrivateKeyInfo: TASN1Struct);
var
  AlgId: PASN1Struct;
  Attr: PASN1Struct;
  BS: PASN1Struct;
begin
  Assert(Assigned(RSAPrivateKey));
  NewComposeASN1Struct(PrivateKeyInfo,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  PrivateKeyInfo.AddField('version','INTEGER',nil).EditContent(Integer(0));
  AlgId := PrivateKeyInfo.AddField('privateKeyAlgorithm','SEQUENCE',nil);
  AlgId.AddField('algorithm','OBJECT',rsaEncryption,True);
  AlgId.AddField('params','NULL',nil);
  PrivateKeyInfo.AddField('privateKey','OCTET STRING',nil).EditContent(RSAPrivateKey);
  Attr := PrivateKeyInfo.AddField('','[0] SEQUENCE',nil);
  Attr.AddField('attrID','OBJECT',id_ce_keyUsage,True);
  BS := Attr.AddField('attrValue','SET',nil).AddField('','BIT STRING',nil);
  BS.Length := 3;
  Move(KeyUsage,BS.Content[1],2);
  PrivateKeyInfo.CalculateLength;
end;

end.



