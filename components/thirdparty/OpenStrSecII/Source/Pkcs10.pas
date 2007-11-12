{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     PKCS10 Unit                                       }
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
unit Pkcs10;

interface

uses
  Classes, SecUtils, MpArithTypes, Pkix, Asn1, MpIF, MpDL, X509Base;

const
  pkcs_9_at_challengePassword = pkcs_9 + '.7';
  pkcs_9_at_extensionRequest  = pkcs_9 + '.14';


// End user routines:
procedure ComposeCertRequest(var CertReq: TASN1Struct;
                             const SubjectName: TX501Name;
                             const SubjectPublicKey: TX509PublicKey;
                             const ChallengePassword: string;
                             const KeyUsage: TKeyUsage;
                             const ExtKeyUsage: TStrings;
                             const SubjectAltName: TX509GeneralNames);
procedure ComposeCACertRequest(var CertReq: TASN1Struct;
                               const SubjectName: TX501Name;
                               const SubjectPublicKey: TX509PublicKey;
                               const ChallengePassword: string;
                               const KeyUsage: TKeyUsage;
                               const ExtKeyUsage: TStrings;
                               const SubjectAltName: TX509GeneralNames;
                               const CRLDistributionPoints: TX509CRLDistPoints;
                               PathLenConstraint: Cardinal);

procedure RSASignCertReq(var CertReq: TASN1Struct;
//                         const CACert: TASN1Struct;
                         const PrivKey: TIFPrivateKey;
                         HashAlgorithm: THashAlgorithm;
                         MGFHashAlgorithm: THashAlgorithm;
                         Encoding: TSignEncoding);
procedure DSASignCertReq(var CertReq: TASN1Struct;
//                         const CACert: TASN1Struct;
                         const PrivKey: TDLPrivateKey;
                         // Allow for future expansion:
                         HashAlgorithm: THashAlgorithm{$IFDEF SHA1} = haSHA1{$ENDIF});

// CA routines:
function ExtractSubjectPublicKeyCR(const CertReq: TASN1Struct; var PK: TX509PublicKey): Integer;
function ExtractSubjectRSAPublicKeyCR(const Cert: TASN1Struct; var RSAKey: TIFPublicKey): Integer;
function ExtractSubjectDHPublicKeyCR(const Cert: TASN1Struct; var DHKey: TDLPublicKey): Integer;
function ExtractSubjectDSAPublicKeyCR(const CertReq: TASN1Struct; var DSAKey: TDLPublicKey): Integer;

function ExtractSubjectCR(const CertReq: TASN1Struct; var Name: TX501Name): Integer;

function ExtractChallengePassword(const CertReq: TASN1Struct; var PW: string): Integer;

function CheckSignatureCR(const CertReq: TASN1Struct): Boolean;

procedure ComposeCertificateCR(var Cert: TASN1Struct;
                               const CertReq: TASN1Struct;
                               const CACert: TASN1Struct;
                               const Validity: TX509Validity;
                               const Serial: string = '');

implementation

uses
  SysUtils, ReadStrm, MpArith;

procedure ComposeCertRequest(var CertReq: TASN1Struct;
                             const SubjectName: TX501Name;
                             const SubjectPublicKey: TX509PublicKey;
                             const ChallengePassword: string;
                             const KeyUsage: TKeyUsage;
                             const ExtKeyUsage: TStrings;
                             const SubjectAltName: TX509GeneralNames);
var
  E, T, V: PASN1Struct;
  B: Byte;     
  D: string;
  I, J: Integer;
  Ext, Ext0: TX509Extension;
begin
  if [] = KeyUsage then
    raise Exception.Create('KeyUsage must not be empty');

  if ASN1StructAssigned(CertReq) then
    DisposeASN1Struct(CertReq);
  NewComposeASN1Struct(CertReq,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  AddComposeASN1Field(CertReq,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
  T := @CertReq.Contents^.Items[0];

  AddComposeASN1Field(T^,V_ASN1_UNIVERSAL,False,V_ASN1_INTEGER);
  B := 0;
  T^.Contents^.Items[0].SetContent(B,1);

  New(V);
  try
    FillChar(V^,SizeOf(TASN1Struct),0);
    TranslateName(SubjectName,V^);
    AddASN1Field(T^,V^);

    TranslatePublicKey(SubjectPublicKey,V^);
    AddASN1Field(T^,V^);

    DisposeASN1Struct(V^);
  finally
    Dispose(V);
  end;

  AddComposeASN1Field(T^,V_ASN1_CONTEXT_SPECIFIC,True,0);
  E := @T^.Contents^.Items[3];

  if ChallengePassword <> '' then begin
    Ext := TX509Extension.Create;
    try
      Ext.extnID := pkcs_9_at_challengePassword;
      NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_UTF8STRING);
      Ext.extnValue.SetContent(ChallengePassword[1],Length(ChallengePassword));
      AddComposeASN1Field(E^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      TranslateAttribute(Ext,E^.Contents^.Items[0]);
    finally
      Ext.Free;
    end;
  end;
  CertReq.CalculateLength;

  if (KeyUsage <> [Low(TKeyUsageItem)..High(TKeyUsageItem)]) or
     (ExtKeyUsage <> nil) or (Length(SubjectAltName) <> 0) then begin
    Ext0 := TX509Extension.Create;
    try
      NewComposeASN1Struct(Ext0.extnValue,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      if (KeyUsage <> [Low(TKeyUsageItem)..High(TKeyUsageItem)]) then begin
        Ext := TX509Extension.Create;
        try
          NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_BIT_STRING);
          SetLength(D,2);
          Move(KeyUsage,D[1],2);
          D[2] := Char(Byte(D[2]) and 1);
          D := #7 + D;
          Ext.extnValue.SetContent(D[1],3);
          Ext.extnID := id_ce_keyUsage;
          Ext.Critical := True;
          New(V);
          try
            FillChar(V^,SizeOf(TASN1Struct),0);
            TranslateExtension(Ext,V^);
            AddASN1Field(Ext0.extnValue,V^);
            DisposeASN1Struct(V^);
          finally
            Dispose(V);
          end;
        finally
          Ext.Free;
        end;
      end;
      if (ExtKeyUsage <> nil) then begin
        Ext := TX509Extension.Create;
        try
          Ext.Critical := False;
          Ext.extnID := id_ce_extKeyUsage;
          NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
          for I := 0 to ExtKeyUsage.Count - 1 do begin
            D := Trim(ExtKeyUsage[I]);
            if D <> '' then begin
              J := Pos('--',D);
              if J > 0 then
                D := Trim(Copy(D,1,J-1));
              if D <> '' then begin
                D := InterpretOIDtoBER(D);
                if Ext.extnValue.Contents <> nil then
                  J := Ext.extnValue.Contents^.ItemCount
                else
                  J := 0;
                AddComposeASN1Field(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_OBJECT);
                Ext.extnValue.Contents^.Items[J].SetContent(D[1],Length(D));
              end;
            end;
          end;
          Ext.extnValue.CalculateLength;
          New(V);
          try
            FillChar(V^,SizeOf(TASN1Struct),0);
            TranslateExtension(Ext,V^);
            AddASN1Field(Ext0.extnValue,V^);
            DisposeASN1Struct(V^);
          finally
            Dispose(V);
          end;
        finally
          Ext.Free;
        end;
      end;
      if Length(SubjectAltName) <> 0 then begin
        Ext := TX509Extension.Create;
        try
          TranslateGeneralNames(SubjectAltName,Ext.extnValue);
          if Ext.extnValue.Contents <> nil then begin
            Ext.extnID := id_ce_subjectAltName;
            Ext.Critical := False;
            New(V);
            try
              FillChar(V^,SizeOf(TASN1Struct),0);
              TranslateExtension(Ext,V^);
              AddASN1Field(Ext0.extnValue,V^);
              DisposeASN1Struct(V^);
            finally
              Dispose(V);
            end;
          end;
        finally
          Ext.Free;
        end;
      end;
      Ext0.extnID := pkcs_9_at_extensionRequest;
      AddComposeASN1Field(E^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      TranslateAttribute(Ext0,E^.Contents^.Items[E^.ItemCount - 1]);
    finally
      Ext0.Free;
    end;
  end;
end;

procedure ComposeCACertRequest(var CertReq: TASN1Struct;
                               const SubjectName: TX501Name;
                               const SubjectPublicKey: TX509PublicKey;
                               const ChallengePassword: string;
                               const KeyUsage: TKeyUsage;
                               const ExtKeyUsage: TStrings;
                               const SubjectAltName: TX509GeneralNames;
                               const CRLDistributionPoints: TX509CRLDistPoints;
                               PathLenConstraint: Cardinal);
var
  V, E, S: PASN1Struct;
  Idx, I, J: Integer;
  B: Byte;
  D: string;
  Ext, Ext0: TX509Extension;
begin
  ComposeCertRequest(CertReq,SubjectName,SubjectPublicKey,ChallengePassword,
                     KeyUsage,ExtKeyUsage,SubjectAltName);

  E := CertReq.Items[0].Items[3].Items[0];
  Ext0 := nil;
  Idx := 0;
  while Idx < E^.ItemCount do begin
    if InterpretAttribute(E^.Items[Idx]^,Ext0) = E_OK then begin
      if Ext0.extnID = pkcs_9_at_extensionRequest then
        Break;
    end else
      raise Exception.Create('Syntax error');
    Inc(Idx);
  end;
  if Ext0 = nil then
    Ext0 := TX509Extension.Create;
  try
    Ext := TX509Extension.Create;
    try
      Ext.Critical := False;
      Ext.extnID := id_ce_basicConstraints;
      NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
      AddComposeASN1Field(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_BOOLEAN);
      B := $FF;
      Ext.extnValue.Contents^.Items[0].SetContent(B,1);
      if PathLenConstraint < $FFFFFFFF then begin
        AddComposeASN1Field(Ext.extnValue,V_ASN1_UNIVERSAL,False,V_ASN1_INTEGER);
        D := '';
        while PathLenConstraint > 0 do begin
          B := pathLenConstraint and $FF;
          pathLenConstraint := pathLenConstraint shr 8;
          D := Char(B) + D;
        end;
        if D = '' then D := #0;
        Ext.extnValue.Contents^.Items[1].SetContent(D[1],Length(D));
      end;
      Ext.extnValue.CalculateLength;
      New(V);
      try
        FillChar(V^,SizeOf(TASN1Struct),0);
        TranslateExtension(Ext,V^);
        AddASN1Field(Ext0.extnValue,V^);
        DisposeASN1Struct(V^);
      finally
        Dispose(V);
      end;

      if Length(CRLDistributionPoints) > 0 then begin
        Ext.Critical := False;
        Ext.extnID := id_ce_cRLDistributionPoints;
        Ext.extnValue.Free;
        NewComposeASN1Struct(Ext.extnValue,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
        for I := 0 to Length(CRLDistributionPoints)-1 do begin
          AddComposeASN1Field(Ext.extnValue,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
          S := @Ext.extnValue.Contents^.Items[I];
          J := 0;
          if CRLDistributionPoints[I].distributionPoint.Tag = 0 then begin
            if Length(CRLDistributionPoints[I].distributionPoint.fullName) > 0 then begin
              AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,0);
              S := @S^.Contents^.Items[J];
              Inc(J);
              AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,0);
              V := @S^.Contents^.Items[0];
              AddComposeASN1Field(V^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
              TranslateGeneralNames(CRLDistributionPoints[I].distributionPoint.fullName,V^.Contents^.Items[0]);
            end;
          end else if CRLDistributionPoints[I].distributionPoint.Tag = 1 then begin
            raise Exception.Create('Not supported'); // Use PKCS_10 instead
          end;
          S := @Ext.extnValue.Contents^.Items[I];
          if CRLDistributionPoints[I].reasons <> [LowReasonFlag..High(TX509ReasonFlag)] then begin
            SetLength(D,SizeOf(TX509ReasonFlags));
            Move(CRLDistributionPoints[I].reasons,D[1],Length(D));
            D := Char(UnusedReasonFlagBits) + D;
            AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,1);
            S := @S^.Contents^.Items[J];
            Inc(J);
            AddComposeASN1Field(S^,V_ASN1_UNIVERSAL,False,V_ASN1_BIT_STRING);
            S^.Contents^.Items[0].SetContent(D[1],Length(D));
          end;
          S := @Ext.extnValue.Contents^.Items[I];
          if Length(CRLDistributionPoints[I].CRLIssuer) > 0 then begin
            AddComposeASN1Field(S^,V_ASN1_CONTEXT_SPECIFIC,True,2);
            S := @S^.Contents^.Items[J];
            AddComposeASN1Field(S^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
            TranslateGeneralNames(CRLDistributionPoints[I].CRLIssuer,S^.Contents^.Items[0]);
          end;

          Ext.extnValue.Contents^.Items[I].CalculateLength;
        end;
        New(V);
        try
          FillChar(V^,SizeOf(TASN1Struct),0);
          TranslateExtension(Ext,V^);
          AddASN1Field(Ext0.extnValue,V^);
          DisposeASN1Struct(V^);
        finally
          Dispose(V);
        end;
      end;

    finally
      Ext.Free;
    end;
    Ext0.extnID := pkcs_9_at_extensionRequest;
    if Idx = E^.ItemCount then
      AddComposeASN1Field(E^,V_ASN1_UNIVERSAL,True,V_ASN1_SEQUENCE);
    TranslateAttribute(Ext0,E^.Contents^.Items[Idx]);
  finally
    Ext0.Free;
  end;
end;

procedure RSASignCertReq(var CertReq: TASN1Struct;
                         const PrivKey: TIFPrivateKey;
                         HashAlgorithm: THashAlgorithm;
                         MGFHashAlgorithm: THashAlgorithm;
                         Encoding: TSignEncoding);
begin
  RSASignStruct(CertReq,PrivKey,HashAlgorithm,MGFHashAlgorithm,Encoding);
end;

procedure DSASignCertReq(var CertReq: TASN1Struct;
                         const PrivKey: TDLPrivateKey;
                         // Allow for future expansion:
                         HashAlgorithm: THashAlgorithm);
begin
  DSASignStruct(CertReq,PrivKey,HashAlgorithm);
end;


function ExtractSubjectPublicKeyCR(const CertReq: TASN1Struct; var PK: TX509PublicKey): Integer;
var
  S, T: ^TASN1Struct;
begin
  if PK = nil then
    PK := TX509PublicKey.Create;
  S := @CertReq;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
       (S^.Contents^.ItemCount > 2) then begin
      S := @S^.Contents^.Items[2];
      if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
         (S^.Contents^.ItemCount = 2) then begin
        S^.Contents^.Items[1].ContentAsASN1Struct(PK.PublicKey);
        S := @S^.Contents^.Items[0];
        if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
           (S^.Contents^.ItemCount > 0) then begin
          T := @S^.Contents^.Items[0];
          if T^.Tag = V_ASN1_OBJECT then begin
            PK.Algorithm.Algorithm := T^.ContentAsOID;
            PK.Algorithm.AlgorithmName := GetObjectName(PK.Algorithm.Algorithm);
            if S^.Contents^.ItemCount > 1 then
              CopyASN1Struct(PK.Algorithm.Parameters,S^.Contents^.Items[1]);
            Result := E_OK;
          end else
            Result := E_SYNTAX;
        end else
          Result := E_SYNTAX;
      end else
        Result := E_SYNTAX;
    end else
      Result := E_SYNTAX;
  end else
    Result := E_SYNTAX;
end;     

function ExtractSubjectRSAPublicKeyCR(const Cert: TASN1Struct; var RSAKey: TIFPublicKey): Integer;
var
  PK: TX509PublicKey;
begin
  PK := TX509PublicKey.Create;
  Result := ExtractSubjectPublicKeyCR(Cert,PK);
  if Result = E_OK then begin
    if PK.Algorithm.Algorithm <> rsaEncryption then
      Result := E_NOT_SUPPORTED
    else if (PK.PublicKey.Tag <> V_ASN1_SEQUENCE) or
            not PK.PublicKey.Constructed or
            (PK.PublicKey.Contents^.ItemCount <> 2) then
      Result := E_SYNTAX
    else begin
      RSAKey.Scheme := ifRSA1;
      PK.PublicKey.Contents^.Items[0].ContentAsUMPInt(RSAKey.N);
      PK.PublicKey.Contents^.Items[1].ContentAsUMPInt(RSAKey.E);
    end;
  end;
  PK.Free;
end;

function ExtractSubjectDHPublicKeyCR(const Cert: TASN1Struct; var DHKey: TDLPublicKey): Integer;
var
  PK: TX509PublicKey;
  P: PMPInteger;
begin
  PK := TX509PublicKey.Create;
  Result := ExtractSubjectPublicKeyCR(Cert,PK);
  if Result = E_OK then begin
    if PK.Algorithm.Algorithm <> dhPublicNumber then
      Result := E_NOT_SUPPORTED
    else if (PK.PublicKey.Tag <> V_ASN1_SEQUENCE) or
            not PK.PublicKey.Constructed or
            (PK.PublicKey.Contents^.ItemCount <> 2) then
      Result := E_SYNTAX
    else begin
      PK.PublicKey.Contents^.Items[0].ContentAsMPInt(DHKey.Y);
      if PK.Algorithm.Parameters.ItemCount >= 3 then begin
        PK.Algorithm.Parameters.Items[0].ContentAsUMPInt(DHKey.Params.P);
        PK.Algorithm.Parameters.Items[1].ContentAsUMPInt(DHKey.Params.G);
        PK.Algorithm.Parameters.Items[2].ContentAsUMPInt(DHKey.Params.Q);
        if PK.Algorithm.Parameters.ItemCount >= 4 then
          PK.Algorithm.Parameters.Items[3].ContentAsUMPInt(DHKey.Params.J)
        else begin
          P := MPCopy(DHKey.Params.P);
          MPDiv(P,DHKey.Params.Q,DHKey.Params.J);
          MPDealloc(P);
        end;
      end;
    end;
  end;
  PK.Free;
end;

function ExtractSubjectDSAPublicKeyCR(const CertReq: TASN1Struct; var DSAKey: TDLPublicKey): Integer;
var
  PK: TX509PublicKey;
  P: PMPInteger;
begin
  PK := TX509PublicKey.Create;
  Result := ExtractSubjectPublicKeyCR(CertReq,PK);
  if Result = E_OK then begin
    if PK.Algorithm.Algorithm <> id_dsa then
      Result := E_NOT_SUPPORTED
    else if (PK.PublicKey.Tag <> V_ASN1_SEQUENCE) or
            not PK.PublicKey.Constructed or
            (PK.PublicKey.Contents^.ItemCount <> 2) then
      Result := E_SYNTAX
    else begin
      PK.PublicKey.Contents^.Items[0].ContentAsUMPInt(DSAKey.Y);
      if PK.Algorithm.Parameters.ItemCount >= 3 then begin
        PK.Algorithm.Parameters.Items[0].ContentAsUMPInt(DSAKey.Params.P);
        PK.Algorithm.Parameters.Items[1].ContentAsUMPInt(DSAKey.Params.Q);
        PK.Algorithm.Parameters.Items[2].ContentAsUMPInt(DSAKey.Params.G);
        P := MPCopy(DSAKey.Params.P);
        MPDiv(P,DSAKey.Params.Q,DSAKey.Params.J);
        MPDealloc(P);
      end;
    end;
  end;
  PK.Free;
end;   

function CheckSignatureCR(const CertReq: TASN1Struct): Boolean;
var
  Alg, A: TX509AlgorithmIdentifier;
  RSAKey: TIFPublicKey;
  DLKey: TDLPublicKey;
  MS: TSecureMemoryStream;
  HA,MHA: THashAlgorithm;
  EM: TSignEncoding;
  S: PASN1Struct;
begin
  Result := False;
  Alg := nil;
  EM := seEMSA3;
  if ExtractSignatureAlgorithm(CertReq,Alg) = E_OK then begin
    if MpIF.ExtractHashAlgEMSA3(Alg.Algorithm,HA) then
      EM := seEMSA3
    else if Alg.Algorithm = id_RSASSA_PSS then begin
      EM := seEMSA4;
      if ASN1StructAssigned(Alg.Parameters) and
         (Alg.Parameters.Tag <> V_ASN1_NULL) then begin
        A := nil;
        S := @Alg.Parameters.Contents^.Items[0];
        S := @S^.Contents^.Items[0];
        if InterpretAlgorithmIdentifier(S^,A) = E_OK then begin
          Result := OIDToHashAlgorithm(A.Algorithm,HA);
          if Result then begin
            S := @Alg.Parameters.Contents^.Items[1];
            S := @S^.Contents^.Items[0];
            if InterpretAlgorithmIdentifier(S^,A) = E_OK then begin
              Result := A.Algorithm = id_mgf1;
              if Result and ASN1StructAssigned(Alg.Parameters) and
                 (Alg.Parameters.Tag <> V_ASN1_NULL) then begin
                A.Free;
                A := nil;
                if InterpretAlgorithmIdentifier(Alg.Parameters,A) = E_OK then begin
                  Result := OIDToHashAlgorithm(A.Algorithm,MHA);
                end;
              end else
              {$IFDEF SHA1}
                MHA := haSHA1;
              {$ELSE  SHA1}
                Result := False;
              {$ENDIF SHA1}
            end else
              Result := False;
          end;
          A.Free;
          DisposeASN1Struct(A.Parameters);
        end else
          Result := False;
      end else
      {$IFDEF SHA1}
        HA := haSHA1;
      {$ELSE  SHA1}
        Result := False;
      {$ENDIF SHA1}
{$IFDEF SHA1}
    end else if Alg.Algorithm = id_dsa_with_sha1 then begin
      HA := haSHA1;
      Result := True;
{$ENDIF SHA1}
    end else
      Result := False;
    Alg.Free;
  end else
    Result := False;
  if Result then begin
    FillChar(RSAKey,SizeOf(RSAKey),0);
    FillChar(DLKey,SizeOf(DLKey),0);
    if ExtractSubjectRSAPublicKeyCR(CertReq,RSAKey) = E_OK then begin
      MS := TSecureMemoryStream.Create;
      try
        ASN1ToStream(CertReq.Contents^.Items[0],MS);
        MS.Position := 0;
        Result := IFSSASignatureVerification(RSAKey,MS.Memory^,MS.Size,
                                             CertReq.Contents^.Items[2].Content^,
                                             CertReq.Contents^.Items[2].Length,
                                             HA,MHA,EM);
      finally
        MS.Free;
      end;
      DisposeIFPublicKey(RSAKey);
    end else if ExtractSubjectDSAPublicKeyCR(CertReq,DLKey) = E_OK then begin
      MS := TSecureMemoryStream.Create;
      try
        ASN1ToStream(CertReq.Contents^.Items[0],MS);
        MS.Position := 0;
        Result := DLSSASignatureVerification(DLKey,MS.Memory^,MS.Size,
                                             CertReq.Contents^.Items[2].Content^,
                                             CertReq.Contents^.Items[2].Length,
                                             True,HA);
      finally
        MS.Free;
      end;
      DisposeDLPublicKey(DLKey);
    // Special case:
    end else if ExtractSubjectDHPublicKeyCR(CertReq,DLKey) = E_OK then begin
      MS := TSecureMemoryStream.Create;
      try
        ASN1ToStream(CertReq.Contents^.Items[0],MS);
        MS.Position := 0;
        Result := DLSSASignatureVerification(DLKey,MS.Memory^,MS.Size,
                                             CertReq.Contents^.Items[2].Content^,
                                             CertReq.Contents^.Items[2].Length,
                                             True,HA);
      finally
        MS.Free;
      end;
      DisposeDLPublicKey(DLKey);
    end;
  end;
end;             

function ExtractSubjectCR(const CertReq: TASN1Struct; var Name: TX501Name): Integer;
var
  S: ^TASN1Struct;
begin
  Result := E_SYNTAX;
  S := @CertReq;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
       (S^.Contents^.ItemCount > 1) then begin
      S := @S^.Contents^.Items[1];
      Result := InterpretName(S^,Name);
    end;
  end;
end;

function ExtractChallengePassword(const CertReq: TASN1Struct; var PW: string): Integer;
var
  S, T: PASN1Struct;
  Idx: Integer;
begin
  Result := E_SYNTAX;
  S := @CertReq;
  if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
     (S^.Contents^.ItemCount > 0) then begin
    S := @S^.Contents^.Items[0];
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
       (S^.Contents^.ItemCount > 3) then begin
      S := @S^.Contents^.Items[3];
      if S^.Constructed and (S^.Tag = 0) and
         (S^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
         (S^.Contents^.ItemCount = 0) then begin
        S := @S^.Contents^.Items[0];
        if S^.Constructed and (S^.Tag = V_ASN1_SET) and
           (S^.Cls = V_ASN1_UNIVERSAL) and
           (S^.Contents^.ItemCount > 0) then begin
          for Idx := 0 to S^.ItemCount - 1 do begin
            T := @S^.Contents^.Items[Idx];
            if T^.Constructed and (T^.Tag = V_ASN1_SEQUENCE) and
               (T^.Cls = V_ASN1_UNIVERSAL) and
               (T^.Contents^.ItemCount > 1) then begin
              if (not T^.Items[0].Constructed) and
                 (T^.Items[0].Tag = V_ASN1_OBJECT) and
                 (T^.Items[0].ContentAsOID = pkcs_9_at_challengePassword) then begin
                T := T^.Items[1];
                if T^.Constructed and (T^.Tag = V_ASN1_SET) and
                  (T^.Cls = V_ASN1_UNIVERSAL) and
                  (T^.Contents^.ItemCount > 1) then begin
                  T := T^.Items[0];
                  PW := T^.ContentAsString;
                  Result := E_OK;
                  Break;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure ComposeCertificateCR(var Cert: TASN1Struct;
                               const CertReq: TASN1Struct;
                               const CACert: TASN1Struct;
                               const Validity: TX509Validity;
                               const Serial: string);
var
  Name: TX501Name;
  PK: TX509PublicKey;
  E, S, T: PASN1Struct;
  Idx, IdxE: Integer;
  Str: string;
  Ext: TX509Extension;
begin
  FillChar(Name,SizeOf(Name),0);
  ExtractSubjectCR(CertReq,Name);
  PK := nil;
  try
    ExtractSubjectPublicKeyCR(CertReq,PK);
    ComposeCertificate2(Cert,Name,PK,CACert,Validity,
                        [Low(TKeyUsageItem)..High(TKeyUsageItem)],nil,nil,Serial);
    E := Cert.Items[0].Items[7].Items[0];
    S := @CertReq;
    if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
       (S^.Contents^.ItemCount > 0) then begin
      S := @S^.Contents^.Items[0];
      if S^.Constructed and (S^.Tag = V_ASN1_SEQUENCE) and
         (S^.Contents^.ItemCount > 3) then begin
        S := @S^.Contents^.Items[3];
        if S^.Constructed and (S^.Tag = 0) and
           (S^.Cls = V_ASN1_CONTEXT_SPECIFIC) and
           (S^.Contents^.ItemCount > 0) then begin
            for Idx := 0 to S^.ItemCount - 1 do begin
              T := @S^.Contents^.Items[Idx];
              if T^.Constructed and (T^.Tag = V_ASN1_SEQUENCE) and
                 (T^.Cls = V_ASN1_UNIVERSAL) and
                 (T^.Contents^.ItemCount > 0) then begin
                if (not T^.Items[0].Constructed) and
                   (T^.Items[0].Tag = V_ASN1_OBJECT) and
                   (T^.Items[0].ContentAsOID = pkcs_9_at_extensionRequest) then begin
                  T := T^.Items[1];
                  if T^.Constructed and (T^.Tag = V_ASN1_SET) and
                    (T^.Cls = V_ASN1_UNIVERSAL) and
                    (T^.Contents^.ItemCount > 0) then begin
                    T := T^.Items[0];
                    Ext := nil;
                    for IdxE := 0 to T^.ItemCount - 1 do begin
                      Str := T^.Items[IdxE].Items[0].ContentAsOID;
                      if ExtractNamedExtension(Cert,Str,Ext) <> E_OK then
                        AddASN1Field(E^,T^.Items[IdxE]^);
                    end;
                    Ext.Free;
                    Break;
                  end;
                end;
              end;
            end;
        end;
      end;
    end;
  finally
    PK.Free;
  end;
end;

end.
