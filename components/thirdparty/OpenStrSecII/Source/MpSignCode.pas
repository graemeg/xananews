{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     MPSignCode Unit                                   }
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
unit MpSignCode;

interface

uses
  SysUtils, Classes, SyncObjs, SecUtils, Asn1, MpX509, StreamSecII, MpCms;

type
  TStatementType = (stNone,stIndividual,stCommercial);

  TCustomMPSignCode = class(TCustomMPSignedData)
  private
    FURL: string;
    FDescription: WideString;
    FStatementType: TStatementType;
    FNoSignature: Boolean;
    FPEDigest: packed array [0..63] of Byte;
    procedure SetDescription(const Value: WideString);
    procedure SetURL(const Value: string);
    procedure SetStatementType(const Value: TStatementType);
  protected
    // Verification:
    procedure Init; override;
    function VerifyAttributes(SignerInfo: TASN1Struct;
                              CounterSign: Boolean = False): Boolean; override;
    function VerifyPEDigest(AContent: TStream): Boolean;
    // Signing:
    procedure ImposeAttributes(SignerInfo: TASN1Struct;
                               AContentCopy: TStream = nil;
                               CounterSign: Boolean = False); override;
    procedure LoadContents(AContent: TStream); override;
    // Common:
    procedure CreateSigStruct; override;
    procedure CalcPEDigest(AContent: TStream); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    // To be published
    property Description: WideString read FDescription write SetDescription;
    property NoSignature: Boolean read FNoSignature;
    property URL: string read FURL write SetURL;
    property StatementType: TStatementType read FStatementType write SetStatementType;
  end;

  TMPSignCode = class(TCustomMPSignCode)
  private
    FChecksum: LongWord;
    FNTHeaderOffset: LongInt;
    FOldChecksum: LongWord;
    FSignatureOffset: LongInt;
    FSignatureSize: LongInt;
    FNoSignature: Boolean;
  protected
    // Verification:
    procedure ExtractCertificates; override;
    procedure Init; override;
    function LoadSigStruct: Boolean; override;
    // Signing:
    procedure CreatePadding;
    procedure FinalSign; override;
    procedure InitSign; override;
    procedure LoadContents(AContent: TStream); override;
    procedure ResetFileStream; override;
    // Common:
    procedure CalcChecksum;
    procedure CalcPEDigest(AContent: TStream); override;
    procedure FindNTHeader;
  public
    destructor Destroy; override;
    function CounterSignCode(const FileName: string): Boolean;
    function SignCode(const FileName: string): Boolean;
  published
    property Description;
    property NoSignature;
    property URL;
    property StatementType;
    property CollectedCertificates;
    property CertificateNotFound;
    property IllegalFormat;
    property IncludeSigningTime;
    property KeysAndCerts;
    property MyCertificates;
    property PrivateKeyRing;
    property OnGoodSignature;
    property OnBadSignature;
  end;

implementation

uses
  ReadStrm, Pkix, X509Base, Pkix_Cert, Cms, MpPK, MpIF, MpDL, MpEC;

{ TMPSignCode }

procedure TMPSignCode.CalcChecksum;
var
  I, L: Integer;
  Buf: packed array [0..$7FFF] of Word;
  C: packed record
       case Byte of
         0: (D: LongWord);
         1: (L,H: Word);
     end;
begin
  C.D := 0;
  FileStream.Position := 0;
  L := FileStream.Read(Buf,FNTHeaderOffset + 88);
  FileStream.Seek(4,soFromCurrent);
  repeat
    for I := 0 to L shr 1 - 1 do begin
      Inc(C.D,Buf[I]);
      if C.H > 0 then
        C.D := C.L + C.H;
    end;
    L := FileStream.Read(Buf,SizeOf(Buf));
  until L <= 0;
  Inc(C.D,FileStream.Size);
  FChecksum := C.D;
end;

procedure TMPSignCode.CalcPEDigest;
var
  H: THash;
  Buf: packed array [0..$FFFF] of Byte;
  P, L: Integer;
begin
  if FSignatureOffset = 0 then
    FNoSignature := True
  else begin
    H := HashClass.Create(nil^,0);
    try
      FileStream.Position := 0;
      L := FileStream.Read(Buf,FNTHeaderOffset + 88);
      H.HashData(Buf,L);
      FileStream.Seek(4,soFromCurrent);
      L := FileStream.Read(Buf,152 - 88 - 4);
      H.HashData(Buf,L);
      P := FileStream.Seek(8,soFromCurrent);
      while P < FSignatureOffset do begin
        L := SizeOf(Buf);
        if L + P > FSignatureOffset then
          L := FSignatureOffset - P;
        L := FileStream.Read(Buf,L);
        H.HashData(Buf,L);
        Inc(P,L);
      end;
      H.Done(@FPEDigest);
    finally
      H.Free;
    end;
  end;
end;

function TMPSignCode.CounterSignCode(const FileName: string): Boolean;
var
  Cert: PASN1Struct;
  MS: TSecureMemoryStream;
  Idx: Integer;
begin
  Result := Assigned(PrivateKeyRing) and Assigned(MyCertificates);
  if Result then begin
{$IFNDEF DISABLEMD5SIGN}
    MD5Override := True;
{$ENDIF DISABLEMD5SIGN}
    try
      Idx := 0;
      Result := MyCertificates.FindCert([],
                                        [rsaEncryption],
                                        [digitalSignature],
                                        [id_kp_timeStamping],
                                        Idx,
                                        Cert,
                                        True);
      if Result then begin
        MS := TSecureMemoryStream.Create;
        try
          MS.LoadFromFile(FileName);
          Result := CounterSignData(Cert^,MS,MS);
          if Result then
            MS.SaveToFile(FileName);
        finally
          MS.Free;
        end;
      end;
    finally
{$IFNDEF DISABLEMD5SIGN}
      MD5Override := False;
{$ENDIF DISABLEMD5SIGN}
    end;
  end;
end;

procedure TMPSignCode.CreatePadding;
var
  PadLen: Integer;
  Pad: packed array [0..15] of Byte;
begin
  if FSignatureOffset = 0 then begin
    FSignatureOffset := FileStream.Size;
    PadLen := 16 - (FSignatureOffset and $F);
    if PadLen < 16 then begin
      FileStream.Seek(0,soFromEnd);
      FillChar(Pad,SizeOf(Pad),0);
      FileStream.Write(Pad,PadLen);
      FSignatureOffset := FSignatureOffset + PadLen;
    end;
  end;
end;

destructor TMPSignCode.Destroy;
begin
  inherited;
end;

procedure TMPSignCode.ExtractCertificates;
var
  Status: TCertStatusCodes;
  Struct: TASN1Struct;
begin
  // The software must not have an expired signature:
  if Assigned(CollectedCertificates) then begin
    Struct := SigStruct;
    CollectedCertificates.ImportFromCMS(Struct,False,Status,False);
  end;
end;

procedure TMPSignCode.FinalSign;
const
  Flag: LongWord = $00020200;
var
  Pad: Byte;
begin
  FileStream.Position := FSignatureOffset;
  FileStream.Write(FSignatureSize,4);
  FileStream.Write(Flag,4);
  inherited FinalSign;
  FSignatureSize := FileStream.Position - FSignatureOffset;
  Pad := 0;
  while (FSignatureSize and $F) > 0 do begin
    FileStream.Write(Pad,1);
    Inc(FSignatureSize);
  end;
  FileStream.Size := FileStream.Position;

  FileStream.Position := FSignatureOffset;
  FileStream.Write(FSignatureSize,4);

  FileStream.Position := FNTHeaderOffset + 152;
  FileStream.Write(FSignatureOffset,4);
  FileStream.Write(FSignatureSize,4);

  CalcCheckSum;
  FileStream.Position := FNTHeaderOffset + 88;
  FileStream.Write(FCheckSum,4);
end;

procedure TMPSignCode.FindNTHeader;
var
  NTHeaderSign: LongWord;
begin
  SetIllegalFormat(FileStream.Size < 64);
  if not IllegalFormat then begin
    FileStream.Position := 60;
    FileStream.Read(FNTHeaderOffset,4);
    SetIllegalFormat((FNTHeaderOffset < 0) or
                     (FNTHeaderOffset + 160 > FileStream.Size));
    if not IllegalFormat then begin
      FileStream.Position := FNTHeaderOffset;
      FileStream.Read(NTHeaderSign,4);
      SetIllegalFormat(NTHeaderSign <> $00004550);
      if not IllegalFormat then begin
        FileStream.Position := FNTHeaderOffset + 88;
        FileStream.Read(FOldCheckSum,4);
        FileStream.Position := FNTHeaderOffset + 152;
        FileStream.Read(FSignatureOffset,4);
        FileStream.Read(FSignatureSize,4);
        if (FSignatureOffset <> 0) or (FSignatureSize <> 0) then
          SetIllegalFormat(FileStream.Size <> FSignatureOffset + FSignatureSize);
      end;
    end;
  end;
end;

procedure TMPSignCode.Init;
begin
  inherited Init;
  FindNTHeader;
  if not IllegalFormat then begin
    CalcCheckSum;
    SetIllegalFormat(FCheckSum <> FOldCheckSum);
  end;
end;

procedure TMPSignCode.InitSign;
begin
  inherited InitSign;
  FindNTHeader;
end;

procedure TMPSignCode.LoadContents(AContent: TStream);
begin
  CreatePadding;
  inherited LoadContents(nil);
end;

function TMPSignCode.LoadSigStruct: Boolean;
begin
  FileStream.Seek(FSignatureOffset + 8,soFromBeginning);
  Result := inherited LoadSigStruct;
  if Result then
    Result := VerifyPEDigest(nil);
end;

procedure TMPSignCode.ResetFileStream;
begin
  // Handled elsewhere
end;

function TMPSignCode.SignCode(const FileName: string): Boolean;
var
  Cert: PASN1Struct;
  MS: TSecureMemoryStream;
  Idx: Integer;
begin
  Result := Assigned(PrivateKeyRing) and Assigned(MyCertificates);
  if Result then begin
    Idx := MyCertificates.Count - 1;
    Result := MyCertificates.FindCert([],
                                      [rsaEncryption],
                                      [digitalSignature],
                                      [id_kp_codeSigning],
                                      Idx,
                                      Cert,
                                      True);
    if Result then begin
      MS := TSecureMemoryStream.Create;
      try
        MS.LoadFromFile(FileName);
        Result := SignData(Cert^,nil,MS);
        if Result then
          MS.SaveToFile(FileName);
      finally
        MS.Free;
      end;
    end;
  end;
end;

{ TCustomMPSignCode }

procedure TCustomMPSignCode.CalcPEDigest(AContent: TStream);
var
  Dummy: Integer;
begin
  Dummy := SizeOf(FPEDigest);
  DigestStream(HashClass.Algorithm,AContent,@FPEDigest,Dummy);
end;

constructor TCustomMPSignCode.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IncludeRootCertificate := True;
  ContentType := pAuthenticode;
end;

procedure TCustomMPSignCode.CreateSigStruct;
var
  P, A, F: PASN1Struct;
  B: Byte;
begin
  inherited CreateSigStruct;
  P := SigStruct.FindField('/content//encapContentInfo');
  P^.EditField('eContentType','1.3.6.1.4.1.311.2.1.4');
  P := P^.FindField('/eContent/');
  P^.Persistent := False;
  P^.Tag := V_ASN1_SEQUENCE;
  P^.Cls := V_ASN1_UNIVERSAL;
  P^.Constructed := True;
  P^.Persistent := True;
  A := P^.AddField('spcPelmageData','SEQUENCE',nil);
    A^.AddField('','OBJECT','1.3.6.1.4.1.311.2.1.15',True);
    A := A^.AddField('','SEQUENCE',nil);
      F := A^.AddField('','BIT STRING','',True);
      B := 0;
      F^.SetContent(B,1);
      A := A^.AddField('','[0] IMPLICIT SEQUENCE',nil);
        A := A^.AddField('','[2] IMPLICIT SEQUENCE',nil);
          A^.AddField('','[0] IMPLICIT BMPString','<<<Obsolete>>>',True);
  A := P^.AddField('digestInfo','SEQUENCE',nil);
    F := A^.AddField('algorithm','SEQUENCE',nil);
      F^.AddField('algorithm','OBJECT',id_sha1,True);
      F^.AddField('params','NULL',nil);
    F := A^.AddField('digest','OCTET STRING',nil);
    F^.SetContent(FPEDigest,20);
end;

procedure TCustomMPSignCode.ImposeAttributes(SignerInfo: TASN1Struct;
  AContentCopy: TStream; CounterSign: Boolean);
var
  F, G, H: PASN1Struct;
begin
  if not CounterSign then begin
    F := SignerInfo.FindField('signedAttrs');

    G := F^.AddField;
    G^.EditField('attrType',pkcs_9_at_contentType);
    G := G^.Items[1].AddField;
    G^.TypeIdentify;
    G^.EditContent('1.3.6.1.4.1.311.2.1.4');

    if FStatementType <> stNone then begin
      G := F^.AddField;
      G^.EditField('attrType','1.3.6.1.4.1.311.2.1.11');
      G := G^.Items[1].AddField;
      G^.Cls := V_ASN1_UNIVERSAL;
      G^.Constructed := True;
      G^.Tag := V_ASN1_SEQUENCE;
      case FStatementType of
        stIndividual:
          G^.AddField('','OBJECT','1.3.6.1.4.1.311.2.1.21',True);
        stCommercial:
          G^.AddField('','OBJECT','1.3.6.1.4.1.311.2.1.22',True);
      end;
    end;

    G := F^.AddField;
    G^.EditField('attrType','1.3.6.1.4.1.311.2.1.12');
    if (FDescription <> '') or (FURL <> '') then begin
      G := G^.Items[1].AddField;
      G^.Cls := V_ASN1_UNIVERSAL;
      G^.Constructed := True;
      G^.Tag := V_ASN1_SEQUENCE;
      if FDescription <> '' then begin
        H := G^.AddField('description','[0] IMPLICIT SEQUENCE',nil);
        H^.AddField('description','[0] IMPLICIT BMPString',FDescription,True);
      end;
      if FURL <> '' then begin
        H := G^.AddField('uRL','[1] IMPLICIT SEQUENCE',nil);
        H^.AddField('uRL','[0] IMPLICIT IA5String',FURL,True);
      end;
    end;

    ContentType := pNone;
    inherited ImposeAttributes(SignerInfo,AContentCopy,False);
    ContentType := pAuthenticode;
  end else
    inherited ImposeAttributes(SignerInfo,AContentCopy,True);
end;

procedure TCustomMPSignCode.Init;
begin
  FDescription := '';
  FURL := '';
  FNoSignature := False;
  inherited;
end;

procedure TCustomMPSignCode.LoadContents(AContent: TStream);
begin
  CalcPEDigest(AContent);
  inherited LoadContents(nil);
end;

procedure TCustomMPSignCode.SetDescription(const Value: WideString);
begin
  FDescription := Value;
end;

procedure TCustomMPSignCode.SetStatementType(const Value: TStatementType);
begin
  FStatementType := Value;
end;

procedure TCustomMPSignCode.SetURL(const Value: string);
begin
  FURL := Value;
end;

function TCustomMPSignCode.VerifyAttributes(SignerInfo: TASN1Struct;
  CounterSign: Boolean): Boolean;
var
  F, G: PASN1Struct;
  I, J: Integer;
begin
  Result := inherited VerifyAttributes(SignerInfo,CounterSign);
  if Result then begin
    F := SignerInfo.FindField('signedAttrs');
    for I := 0 to F^.ItemCount - 1 do begin
      G := F^.Items[I];
      if G^.ItemCount > 1 then
        if (G^.Items[0].ActualTag = V_ASN1_OBJECT) and
           (G^.Items[0]^.ContentAsOID = '1.3.6.1.4.1.311.2.1.12') and
           not G^.Items[1]^.IsEmpty then begin
          G := G^.Items[1]^.Items[0];
          for J := 0 to G^.ItemCount - 1 do
            case G^.Items[J]^.Tag of
              0:
                begin
                  G^.Items[J]^.Items[0]^.Persistent := True;
                  G^.Items[J]^.Items[0]^.Implicit := True;
                  G^.Items[J]^.Items[0]^.ImplicitTag := V_ASN1_BMPSTRING;
                  FDescription := G^.Items[J]^.Items[0]^.ContentAsString;
                end;
              1:
                FURL := G^.Items[J]^.Items[0]^.ContentAsString;
            end;
        end;
    end;
  end;
end;

function TCustomMPSignCode.VerifyPEDigest(AContent: TStream): Boolean;
var
  P: PASN1Struct;
  HA: THashAlgorithm;
begin
  P := SigStruct.FindField('/content//encapContentInfo/eContent//digestInfo');
  Result := OIDToHashAlgorithm(P^.FindField('/algorithm/algorithm')^.ContentAsOID,HA);
  if Result then begin
    HashClass := FindHashClass(HA);
    CalcPEDigest(AContent);
    Result := not FNoSignature;
    if Result then begin
      P := P^.FindField('digest');
      Result := Assigned(P) and (P^.Length = HashClass.DigestSize) and
                CompareMem(@FPEDigest,P^.Content,HashClass.DigestSize);
    end;
  end;
end;

end.
