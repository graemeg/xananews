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
}
unit SsSMIMEId9Broker;

interface

uses
  SysUtils, Classes, Cms, MpCms, IdMessage, IdException;

type
  TSsSMIMEIdBroker = class(TMPCMSBroker)
  public
    class function CheckIsSMIME(ASrc: TIdMessage): Boolean;
    function Decode(ASrc, ADest: TIdMessage;
                    ActionDescriptions: TStrings): Boolean; overload;
    function Encode(Actions: TContentTypeArray;
                    ASrc, ADest: TIdMessage;
                    DataType: TPKCS7DataType = pData): Boolean; overload;
  end;

implementation

uses
  SsBase64, ReadStrm, Pkix, Asn1, X509Base;

{ TSsSMIMEIdBroker }

class function TSsSMIMEIdBroker.CheckIsSMIME(ASrc: TIdMessage): Boolean;
var
  vContentType: string;                     
  P: Integer;
begin
  vContentType := LowerCase(ASrc.ContentType);
  P := Pos('application/',vContentType);
  Result := P = 1;
  if Result then begin
    P := Pos('pkcs7-mime',vContentType);
    Result := P > Length('application/');
    if Result then begin
      P := Pos('smime-type=',vContentType);
      if P > 0 then begin
        vContentType := Copy(vContentType,P + Length('smime-type='),MaxInt);
        Result := (Pos('enveloped-data',vContentType) = 1) or
                  (Pos('signed-data',vContentType) = 1);
      end;
    end;
  end;
end;

function TSsSMIMEIdBroker.Decode(ASrc, ADest: TIdMessage;
  ActionDescriptions: TStrings): Boolean;
const
  Ending = #13#10'.'#13#10;
var
  vContentType, vFileName, vFileExt: string;
  vSrc, vDst: TSecureMemoryStream;
  vTmpMsg: TIdMessage;
begin
  // First, test for "Content-Type: application/[x-]pkcs7-mime"
  Result := CheckIsSMIME(ASrc);
  vContentType := LowerCase(ASrc.ContentType);
  // Second, test for "Content-Disposition: attachment"
  if not Result then begin
    Result := ASrc.MessageParts.Count = 1;
    if Result then begin
      Result := ASrc.MessageParts.Items[0] is TIdAttachment;
      if Result then begin
        vFileName := TIdAttachment(ASrc.MessageParts.Items[0]).FileName;
        vFileExt := LowerCase(ExtractFileExt(vFilename));
        Result := (vFileExt = '.p7m') or (vFileExt = '.p7c') or (vFileExt = '.aps');
      end;
    end;
  end;

  if not Result then begin
    if ASrc <> ADest then begin
      vSrc := TSecureMemoryStream.Create;
      try
        ASrc.NoEncode := True;
        ASrc.SaveToStream(vSrc);
        vSrc.Position := 0;
        ADest.LoadFromStream(vSrc);
      finally
        vSrc.Free;
      end;
    end;
    Result := True;
  end else begin
    vSrc := TSecureMemoryStream.Create;
    try
      vSrc.LoadFromFile(TIdAttachment(ASrc.MessageParts.Items[0]).StoredPathname);
      vSrc.Position := 0;
      vDst := TSecureMemoryStream.Create;
      try
        Result := Decode(vSrc,vDst,ActionDescriptions);
        if Result then begin
          vDst.Seek(0,soFromEnd);
          vDst.Write(Ending[1],Length(Ending));
          vDst.Position := 0;
          vTmpMsg := ADest;
          try
            vTmpMsg.LoadFromStream(vDst);
          except
            on EIdException do
              ;
            on Exception do
              raise;
          end;
          Result := Decode(vTmpMsg,ADest,ActionDescriptions);
        end;
      finally
        vDst.Free;
      end;
    finally
      vSrc.Free;
    end;
  end;
end;

type
  TSignedDataHack = class(TCustomMPSignedData);

function TSsSMIMEIdBroker.Encode(Actions: TContentTypeArray; ASrc,
  ADest: TIdMessage; DataType: TPKCS7DataType): Boolean;
var
  Strm: TSecureMemoryStream;
  DstStrm: TStringStream;
  TmpMsg: TIdMessage;
  Addrs: TStringList;
  I: Integer;
  Cert: PASN1Struct;
begin
  Strm := TSecureMemoryStream.Create;
  try
    if ADest.From.Address = '' then
      ADest.From.Assign(ASrc.From);
    if ADest.Recipients.Count = 0 then
      ADest.Recipients.Assign(ASrc.Recipients);
    if ADest.CCList.Count = 0 then
      ADest.CCList.Assign(ASrc.CCList);
    if ADest.BCCList.Count = 0 then
      ADest.BCCList.Assign(ASrc.BCCList);

    TmpMsg := ADest;
    try
      if Length(Actions) = 0 then begin
        if ASrc <> ADest then begin
          ASrc.SaveToStream(Strm);
          Strm.Position := 0;
          ADest.LoadFromStream(Strm);
        end;
        Result := True;
      end else begin
        ASrc.SaveToStream(Strm);
        Strm.Position := 0;
        DstStrm := TStringStream.Create('');
        try
          case Actions[0] of
            pEnvelopedData:
              begin
                TmpMsg.ContentType := 'application/pkcs7-mime;' +
                                      #9'smime-type=enveloped-data;' +
                                      #9'name="smime.p7m"';
                TmpMsg.ContentTransferEncoding := 'base64';
                TmpMsg.ContentDisposition := 'attachment; filename="smime.p7m"';
                Result := Assigned(EnvelopedData);
                if Result then begin
                  Addrs := TStringList.Create;
                  try
                    Addrs.Sorted := True;
                    Addrs.Duplicates := dupIgnore;
                    if ADest.Sender.Address <> '' then
                      Addrs.Add(ADest.Sender.Address);
                    if ADest.From.Address <> '' then
                      Addrs.Add(ADest.From.Address);
                    for I := 0 to ADest.Recipients.Count - 1 do
                      if ADest.Recipients.Items[I].Address <> '' then
                        Addrs.Add(ADest.Recipients.Items[I].Address);
                    for I := 0 to ADest.CCList.Count - 1 do
                      if ADest.CCList.Items[I].Address <> '' then
                        Addrs.Add(ADest.CCList.Items[I].Address);
                    for I := 0 to ADest.CCList.Count - 1 do
                      if ADest.BCCList.Items[I].Address <> '' then
                        Addrs.Add(ADest.BCCList.Items[I].Address);
                    (EnvelopedData as TMPEnvelopedData).ContentType := pData;
                    (EnvelopedData as TMPEnvelopedData).ContentOnly := False;
                    Result := EnvelopedData.Encrypt(Addrs,Strm,DstStrm);
                  finally
                    Addrs.Free;
                  end;
                end;
              end;
            pSignedData:
              begin                              
                TSignedDataHack(SignedData).SigningDescription := ASrc.Subject;
                TmpMsg.ContentType := 'application/pkcs7-mime;' +
                                      #9'smime-type=signed-data;' +
                                      #9'name="smime.p7m"';
                TmpMsg.ContentTransferEncoding := 'base64';
                TmpMsg.ContentDisposition := 'attachment; filename="smime.p7m"';
                Cert := nil;
                I := 0;
                repeat
                  if not MyCertificates.FindCert([],[],[digitalSignature,keyEncipherment],[id_kp_emailProtection],I,Cert) then
                    Cert := nil;
                  Inc(I);
                until (Cert = nil) or X509Base.CheckEmailAddress(Cert^,ADest.From.Address);
                if Cert = nil then begin
                  I := 0;
                  repeat
                    if not MyCertificates.FindCert([],[],[digitalSignature],[id_kp_emailProtection],I,Cert) then
                      Cert := nil;
                    Inc(I);
                  until (Cert = nil) or X509Base.CheckEmailAddress(Cert^,ADest.From.Address);
                end;
                Result := Assigned(Cert);
                if Result then begin
                  (SignedData as TMPSignedData).ContentType := pData;
                  (SignedData as TMPSignedData).ContentOnly := False;
                  Result := SignedData.SignData(Cert^,Strm,DstStrm);
                end;
              end;
          else
            Result := False;
          end;
          if Result then begin
            TmpMsg.Headers.Values['MIME-Version'] := '1.0';
            TmpMsg.Headers.Values['Content-Type'] := TmpMsg.ContentType;
            TmpMsg.Headers.Values['Content-Transfer-Encoding'] := TmpMsg.ContentTransferEncoding;
            TmpMsg.Headers.Values['Content-Disposition'] := TmpMsg.ContentDisposition;
            TmpMsg.Headers.Values['X-Security-Provider'] := '"StreamSec StrSecII S/MIME Broker for Indy 9"';
            TmpMsg.Body.Text := StrToMIME64('','',DstStrm.DataString);
            Result := Encode(Copy(Actions,1,Length(Actions)-1),TmpMsg,ADest);
          end;
        finally
          DstStrm.Free;
        end;
      end;
    finally
//
    end;
  finally
    Strm.Free;
  end;
end;

end.
