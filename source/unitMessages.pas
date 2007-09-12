(*======================================================================*
 | unitMessages unit for NewsReader3
 |                                                                      |
 | Internet message classes                                             |
 |                                                                      |
 | A message conains body text, plus message parts.                     |
 |                                                                      |
 | Message parts have a 'Body' TStrings property and a 'Graphic'        |
 | TGraphic property.  Body can be nil, but Graphic always contains a   |
 | valid image.  By default this is just an icon.                       |
 |                                                                      |
 | Other message part-derived classes can be registered to return       |
 | different representations of 'body' (eg. for HTML MIME messages)     |
 | and 'graphic' (eg. for UUEncoded or MIME encoded images)             |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2002  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      02/08/2001  CPWW  Original                                  |
 *======================================================================*)

unit unitMessages;

interface

uses Windows, Classes, SysUtils, ConTnrs, Graphics, forms, SyncObjs, ShellAPI, StrUtils;

type

TmvMessagePart = class;
TmvMessagePartClass = class of TmvMessagePart;
TDecodeType = (ttText, ttBase64, ttUUEncode, ttYEnc, ttQuotedPrintable);

//------------------------------------------------------------------------
// TmvMessageParts
//
TmvMessageParts = class (TOwnedCollection)
private
  function GetItems(idx: Integer): TmvMessagePart;
public
  property Items [idx : Integer] : TmvMessagePart read GetItems; default;
end;

//------------------------------------------------------------------------
// TThreadsafeMemoryStream
//
TThreadsafeMemoryStream = class (TMemoryStream)
private
  fCriticalSection : TCriticalSection;
public
  constructor Create;
  destructor Destroy; override;
  function Write(const Buffer; Count: Longint): Longint; override;
  procedure Lock;
  procedure Unlock;
end;

TMimeHeader = class
private
  fContentTypeSubtype: string;
  fContentTypeType: string;
  fContentTransferEncoding: string;
  fContentID: string;
  fContentDescription: string;
  fContentTypeAttributes: TStringList;
  fContentDisposition: string;
  fContentDispositionAttributes: TStringList;
  function GetImageType: string;
  function GetDecodeType: TDecodeType;
  function GetFileName: string;
  function GetIsMultipart: boolean;
  function GetMultipartBoundary : string;
public
  class function CreateFromHeaderStrings (AHeader : TStrings; forceMIME : boolean) : TMimeHeader;
  destructor Destroy; override;

  procedure Assign (const AHeader : TMimeHeader);
  procedure HandleXRFC2646 (const st : string);

  property ContentType_Type : string read fContentTypeType;
  property ContentType_Subtype : string read fContentTypeSubtype;
  property ContentType_Attributes : TStringList read fContentTypeAttributes;
  property ContentTransferEncoding : string read fContentTransferEncoding;
  property ContentID : string read fContentID;
  property ContentDescription : string read fContentDescription;
  property ContentDisposition : string read fContentDisposition;
  property ContentDispositionAttributes : TStringList read fContentDispositionAttributes;

  property ImageType : string read GetImageType;
  property DecodeType : TDecodeType read GetDecodeType;
  property FileName : string read GetFileName;
  property IsMultipart : boolean read GetIsMultipart;
  property MultipartBoundary : string read GetMultipartBoundary;
end;

//------------------------------------------------------------------------
// TmvMessage
//
TmvMessage = class (TPersistent)
private
  fDefaultCodePage: Integer;
  fAlternateHTML: boolean;
  fAlternateMessagePartCount : Integer;
  fTruncateFrom: WideString;
  fStrictSigSeparator: boolean;
  function GetMIMEHeader: TMIMEHeader;
  function GetCodePage: Integer;
  function GetTextPart: TStrings;
  function GetXFace: TBitmap;
  procedure SetCodePage(const Value: Integer);
  function GetAlternateMessagePart(idx: Integer): TmvMessagePart;
  function GetAlternateMessagePartCount: Integer;
private
  fData : TThreadsafeMemoryStream;
  fBody : TStrings;
  fHeader : TStrings;
  fXFace : TBitmap;
  fMessageParts : TmvMessageParts;
  fUpdating : boolean;
  fDecodePos : Integer;
  fDecodeSize : Integer;
  fCurrentMessagePart : TmvMessagePart;
  fBeingDisplayed: boolean;
  fObject : TObject;
  fRawMode: boolean;
  fPartialMessage: boolean;
  fGotXFace : boolean;
  fMIMEHeader : TMIMEHeader;
  fMPStack : TStack;
  fCS : TCriticalSection;
  fCurrentLine : Integer;
  fCodePage : Integer;
  procedure SetHeader(const Value: TStrings);
  procedure Decode;
  procedure PartialDecode;
  function GetBody: TStrings;
  function GetMessageParts: TmvMessageParts;
  function GetDormant: boolean;
  procedure SetDormant(const Value: boolean);
  procedure SetRawMode(const Value: boolean);

protected
  function GetLine (var st : string) : boolean;
  function TryCreateMessagePart (const st : string; hdr : TMimeHeader) : TmvMessagePart;
public
  constructor Create (AObject : TObject);
  destructor Destroy; override;
  property MIMEHeader : TMIMEHeader read GetMIMEHeader;
  procedure DoOnProgress (Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);

  function IdentifyMessagePartBoundary (const st : string; hdr : TMimeHeader) : TmvMessagePartClass;

  procedure Clear;
  procedure AddData (data : TStream);
//  procedure LoadDataFromStream (strm : TStream; len : Integer);

  procedure BeginUpdate;
  procedure EndUpdate;

  procedure Lock;
  procedure Unlock;

  function FindMessagePartFromCID (const cid : string) : TmvMessagePart;

  property Updating : boolean read fUpdating;

  property RawData : TThreadsafeMemoryStream read fData;
  property Header : TStrings read fHeader write SetHeader;
  property Codepage : Integer read GetCodePage write SetCodePage;

  property Body : TStrings read GetBody;
  property XFace : TBitmap read GetXFace;
  property TextPart : TStrings read GetTextPart;
  procedure DecodeBody (f1 :TStream; decodeType : TDecodeType);
  property MessageParts : TmvMessageParts read GetMessageParts;
  property Dormant : boolean read GetDormant write SetDormant;
  property BeingDisplayed : boolean read fBeingDisplayed write fBeingDisplayed;
  property Obj : TObject read fObject;
  property RawMode : boolean read fRawMode write SetRawMode;
  property PartialMessage : boolean read fPartialMessage write fPartialMessage;
  property DefaultCodePage : Integer read fDefaultCodePage write fDefaultCodePage;

  property AlternateHTML : boolean read fAlternateHTML write fAlternateHTML;
  property AlternateMessagePartCount : Integer read GetAlternateMessagePartCount;
  property AlternateMessagePart [idx : Integer] : TmvMessagePart read GetAlternateMessagePart;
  property TruncateFrom : WideString read fTruncateFrom write fTruncateFrom;
  property StrictSigSeparator : boolean read fStrictSigSeparator write fStrictSigSeparator;
end;

//------------------------------------------------------------------------
// TmvMessagePart
//

TAddLine = (alOK, alEndOfMP, alNextMP, alMultipart);

TmvMessagePart = class (TCollectionItem)
private
  fOwner: TmvMessage;
  fHasRawData : Boolean;
  fParentMessagePart : TmvMessagePart;
  function GetPercentDecoded: Integer;
protected
  fData : TMemoryStream;
  fGraphic : TGraphic;
  fFileName : string;
  fInline : boolean;
  fComplete : boolean;
  fGotGraphic : Boolean;
  fDecodeType : TDecodeType;
  fStartLine : Integer;
  fOverridePercent : boolean;

  class function IsBoundary (const st : string; MIMEHeader : TMIMEHeader) : boolean; virtual;
  function IsBoundaryEnd (const st : string) : boolean; virtual;
  function ProcessHeaderLine (const st : string) : boolean; virtual;  // Return False when header is complete
  function AddLine (const st : string) : TAddLine; virtual;
  function GetGraphic: TGraphic; virtual;
  function GetBody : TStrings; virtual;
  procedure DecodeGraphic (gc : TGraphicClass);
  procedure InitMultipart (multipartHeader : TMIMEHeader); virtual;
  function GetDecodeType: TDecodeType; virtual;
  function GetFileName: string; virtual;
  function MatchesCID (const cid : string) : boolean; virtual;
  function GetMIMEContentType : string; virtual;
  function GetIsHTMLMultipart: boolean; virtual;
public
  constructor Create (AOwner : TCollection); override;
  destructor Destroy; override;
  property Owner : TmvMessage read fOwner;
  procedure GetData (s : TStream); virtual;
  property Graphic : TGraphic read GetGraphic;
  property Body : TStrings read GetBody;
  property FileName : string read GetFileName;
  property Complete : boolean read fComplete;
  property HasRawData : Boolean read fHasRawData;
  property DecodeType : TDecodeType read GetDecodeType;
  property PercentDecoded : Integer read GetPercentDecoded;
  property MIMEContentType : string read GetMIMEContentType;
  property IsHTMLMultipart : boolean read GetIsHTMLMultipart;
end;

//------------------------------------------------------------------------
// TmvTextMessagePart
//
TmvTextMessagePart = class (TmvMessagePart)
private
  fBody : TStrings;
protected
  function ProcessHeaderLine (const st : string) : boolean; override;
  function GetBody : TStrings; override;
public
  constructor Create (AOwner : TCollection); override;
  destructor Destroy; override;
end;

//------------------------------------------------------------------------
// TmvBodyContinuationMessagePart
//
TmvBodyContinuationMessagePart = class (TmvTextMessagePart)
end;


var
  gCurrentMessage : TmvMessage;

procedure RegisterMessagePart (cls : TmvMessagePartClass);
function GetGraphicClass (ext : string) : TGraphicClass;

implementation

uses GIFImage, Jpeg, PngImage, unitMessageMime, NewsGlobals, idCoder, idCoderUUE, idCoderMIME, unitCharsetMap, XFace, unitNNTPServices, unitSearchString;

var
  registeredMessageParts : array of TmvMessagePartClass;
  registeredMessagePartCount : Integer = 0;

(*----------------------------------------------------------------------*
 | procedure RegisterMessagePart                                        |
 |                                                                      |
 | Register a message part decoder for additional decoding - see        |
 | unitMessageNNTPBinary and unitMessageMime                            |
 |                                                                      |
 | Parameters:                                                          |
 |   cls : TmvMessagePartClass  The class to register.                  |
 *----------------------------------------------------------------------*)
procedure RegisterMessagePart (cls : TmvMessagePartClass);
var
  found : boolean;
  i : Integer;
begin
  found := False;
                                // Is it already registered?
  for i := 0 to registeredMessagePartCount - 1 do
    if registeredMessageParts [i] = cls then
    begin
      found := True;
      break
    end;

  if not found then             // Add it to the array of message classes
  begin
    if Length (registeredMessageParts) = registeredMessagePartCount then
      SetLength (registeredMessageParts, Length (registeredMessageParts) + 10);

    registeredMessageParts [registeredMessagePartCount] := cls;

    Inc (registeredMessagePartCount)
  end
end;

(*----------------------------------------------------------------------*
 | function GetGraphicClass                                             |
 |                                                                      |
 | Get a graphic class for a file extension.  This *should* be in       |
 | graphics.pas                                                         |
 |                                                                      |
 | Parameters:                                                          |
 |   ext : string               The extension                           |
 |                                                                      |
 | The function returns the graphics class that supports the extension  |
 *----------------------------------------------------------------------*)
function GetGraphicClass (ext : string) : TGraphicClass;
begin
  result := Nil;
  if ext <> '' then
  begin
    ext := UpperCase (ext);
    if ext [1] = '.' then
      ext := Copy (ext, 2, MaxInt);

    if ext = 'BMP' then
      result := TBitmap
    else
      if (ext = 'JPG') or (ext = 'JPEG') then
        result := TJPEGImage
      else
        if (ext = 'GIF') then
          result := TGIFImage
        else
          if (ext = 'PNG') then
            result := TPngObject;
  end
end;

{ TmvMessage }

(*----------------------------------------------------------------------*
 | procedure TmvMessage.AddData                                         |
 |                                                                      |
 | Add data to a message                                                |
 |                                                                      |
 | Parameters:                                                          |
 |   data : TStream             The data to add                         |
 *----------------------------------------------------------------------*)
procedure TmvMessage.AddData(data : TStream);
begin
  fData.Seek(0, soFromEnd);
  fData.CopyFrom(data, 0);
end;

(*----------------------------------------------------------------------*
 | procedure TmvMessage.BeginUpdate                                     |
 |                                                                      |
 | Prevent added data from being decoded until the next EndUpdate or    |
 | PartialDecode                                                        |
 *----------------------------------------------------------------------*)
procedure TmvMessage.BeginUpdate;
begin
  fDecodePos := 0;
  fUpdating := True;
end;

(*----------------------------------------------------------------------*
 | procedure TmvMessage.Clear                                           |
 |                                                                      |
 | Clear the message and all message parts                              |
 *----------------------------------------------------------------------*)
procedure TmvMessage.Clear;
begin
  fHeader.Clear;
  fData.Size := 0;
  Decode
end;

(*----------------------------------------------------------------------*
 | constructor TmvMessage.Create                                        |
 |                                                                      |
 | Constructor for TMessage                                             |
 *----------------------------------------------------------------------*)
constructor TmvMessage.Create (AObject : TObject);
begin
  inherited Create;
  fDefaultCodePage := CP_USASCII;
  fCodePage := -1;
  fCS := TCriticalSection.Create;
  fObject := AObject;
  fData := TThreadsafeMemoryStream.Create;
  fBody := TStringList.Create;
  fHeader := TStringList.Create;
  fMessageParts := TmvMessageParts.Create (self, TmvMessagePart);
  fAlternateHTML := True;
  fAlternateMessagePartCount := -1;
end;

(*----------------------------------------------------------------------*
 | procedure TmvMessage.Decode                                          |
 |                                                                      |
 | Decode the message.  Only called when the complete message has been  |
 | downloaded.  Called by GetBody and GetMessageParts                   |
 *----------------------------------------------------------------------*)
procedure TmvMessage.Decode;
var
  i : Integer;
begin
  if not fUpdating then
  begin
    fDecodePos := 0;
    PartialDecode;

    for i := 0 to fMessageParts.Count - 1 do
      fMessageParts [i].fComplete := True
  end
end;

procedure TmvMessage.DecodeBody(f1: TStream; decodeType: TDecodeType);
var
  decoder : TidDecoder;
  i : Integer;

begin
  decoder := Nil;
  case decodeType of
    ttUUEncode : decoder := TidDecoderUUE.Create(nil);
    ttBase64   : decoder := TidDecoderMIME.Create(nil)
  end;

  if Assigned (decoder) then
  try
    for i := 0 to Body.Count - 1 do
      decoder.DecodeToStream(Body [i], f1);
  finally
    decoder.Free;
  end
end;

(*----------------------------------------------------------------------*
 | destructor TmvMessage.Destroy                                        |
 |                                                                      |
 | Destructor for TmvMessage                                            |
 *----------------------------------------------------------------------*)
destructor TmvMessage.Destroy;
begin
  fMIMEHeader.Free;
  fBody.Free;
  fHeader.Free;
  fMessageParts.Free;
  fData.Free;
  fMPStack.Free;
  fCS.Free;
  fXFace.Free;

  inherited
end;

(*----------------------------------------------------------------------*
 | procedure TmvMessage.EndUpdate                                       |
 |                                                                      |
 | Finish adding data to the message.  Decode what we've got.           |
 *----------------------------------------------------------------------*)
procedure TmvMessage.DoOnProgress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
  const Msg: string);
begin
//  fmMain.StatusBar.Panels [0].Text := IntToStr (PercentDone);
end;

procedure TmvMessage.EndUpdate;
var
  i : Integer;
begin
  fUpdating := False;
//  Decode
  PartialDecode;
  fCodePage := -1;
  for i := 0 to fMessageParts.Count - 1 do
    fMessageParts [i].fComplete := True
end;

(*----------------------------------------------------------------------*
 | function TmvMessage.GetBody                                          |
 |                                                                      |
 | Get the message body                                                 |
 |                                                                      |
 | The function returns the message body                                |
 *----------------------------------------------------------------------*)
function TmvMessage.FindMessagePartFromCID(
  const cid: string): TmvMessagePart;
var
  i : Integer;
begin
  result := Nil;
  for i := 0 to MessageParts.Count - 1 do
  begin
    if MessageParts [i].MatchesCID(cid) then
    begin
      result := MessageParts [i];
      break
    end
  end
end;

function TmvMessage.GetAlternateMessagePart(idx: Integer): TmvMessagePart;
begin
  result := MessageParts [idx]
end;

function TmvMessage.GetAlternateMessagePartCount: Integer;
begin
  result := MessageParts.Count
end;

function TmvMessage.GetBody: TStrings;
begin
  if fUpdating then
    PartialDecode
  else
    Decode;

  result := fBody;
end;

{*----------------------------------------------------------------------*
 | function TmvMessage.GetCodePage                                      |
 |                                                                      |
 | Calculate the message's codepage.                                    |
 |
 |
 |                                                                      |
 | Parameters:                                                          |
 |   None
 |                                                                      |
 | The function returns Integer
 *----------------------------------------------------------------------*}
function TmvMessage.GetCodePage: Integer;
var
  s, ext : string;
  art : TArticle;
begin
  if fCodePage = -1 then
  begin
    if fUpdating then
      PartialDecode
    else
      Decode;

    if Assigned (MimeHeader) and Assigned (MimeHeader.ContentType_Attributes) then
    begin
      s := MimeHeader.ContentType_Attributes.Values ['charset'];
      if s = '' then
      begin
        if fUpdating then
        begin
          fCodePage := -1;
          result := CP_USASCII;
          exit
        end
        else
          fCodePage := fDefaultCodePage
      end
      else
        fCodePage := MIMECharsetNameToCodePage (s)
    end
    else
    begin
      fCodePage := -1;
      art := TArticle (Obj);
      if Assigned (art) and (Length (art.FromName) > 0) then
        if art.FromName [1] > #127 then
        begin
          ext := ExtractFileExt (art.FromEmail);
          fCodePage := URLSuffixToCodePage (ext)
        end;

      if fCodePage = -1 then
        if fObject is TArticleBase then
          fCodePage := TArticleBase (fObject).fCodePage // nb - must be fCodePage!
        else
          fCodePage := MIMECharsetNameToCodePage ('');

      if fCodePage = -1 then
        if fUpdating then
        begin
          result := CP_USASCII;
          Exit
        end
        else
          fCodePage := fDefaultCodePage;
    end
  end;

  result := fCodePage
end;

function TmvMessage.GetDormant: boolean;
begin
  result := (fDecodePos = 0) and (fDecodeSize = 0);
end;


(*----------------------------------------------------------------------*
 | function TmvMessage.GetLine : boolean                                |
 |                                                                      |
 | Get a line of the message text.  This is threadsafe so we can add    |
 | data to the message while decoding what we've got so far.            |
 |                                                                      |
 | Parameters:                                                          |
 |   var st : string    The string to get                               |
 |                                                                      |
 | The function returns true if we got a complete line of text          |
 *----------------------------------------------------------------------*)
function TmvMessage.GetLine(var st: string): boolean;
var
  pch, pch1 : PChar;
  p, l : Integer;
begin
  try
    pch := fData.Memory;
    l := fData.Size;
    p := fDecodePos;

    Dec (l, p);
    Inc (pch, p);

    if l > 0 then    // Kind of like StrScan with a Length parameter!
    asm
          push    edi
          mov     edi, pch
          mov     al, 10          // Search for #10 (LF)
          mov     ecx, l          // Length to search
          repne   scasb
          xor     eax, eax        // Return nil if not found
          jne     @2
          lea     eax, [edi-1]    // Otherwise return the pointer to the #10
    @2:   pop     edi
          mov     pch1, eax
    end
    else
      pch1 := Nil;

    if pch1 <> Nil then
    begin
      l := Integer (pch1) - Integer (pch);
      Inc (fDecodePos, l+1);
      Dec (pch1);
      while (l > 0) and (pch1^ = #13) do
      begin
        Dec (l);
        Dec (pch1);
      end;

      SetLength (st, l);
      Move (pch^, st [1], l);
      result := True
    end
    else
      result := False;
  except
    st := '';
    Sleep (100);
    result := False
  end
end;

(*----------------------------------------------------------------------*
 | function TmvMessage.GetMessageParts : TmvMessageParts                |
 |                                                                      |
 | Get the message parts                                                |
 |                                                                      |
 | The function returns the message parts                               |
 *----------------------------------------------------------------------*)
function TmvMessage.GetMessageParts: TmvMessageParts;
begin
  if fUpdating then
    PartialDecode
  else
    Decode;

  result := fMessageParts;
end;

function TmvMessage.GetMIMEHeader: TMIMEHeader;
begin
  if Assigned (fMPStack) and (fMPStack.Count > 0) then
    result := fMPStack.Peek
  else
    result := fMIMEHeader
end;

function TmvMessage.GetTextPart: TStrings;
var
  i : Integer;
begin
  if (Body.Count > 0) and not (Assigned (MIMEHeader) and MIMEHeader.IsMultipart) then
    result := Body
  else
  begin
    result := Nil;
    i := 0;

    while (result = Nil) and (i < MessageParts.Count) do
    begin
      if Assigned (MessageParts [i].Body) and  (MessageParts [i].Body.Count > 0) then
        result := MessageParts [i].Body;
      Inc (i)
    end
  end
end;

function TmvMessage.GetXFace: TBitmap;
var
  i : Integer;
  s: string;
  face : boolean;
  faceDecoder : TIdDecoderMIME;
  facePngStrm : TStream;
  png : TPngObject;
begin
  if not fGotXFace then
  begin
    fGotXFace := True;

    face := False;
    for i := 0 to header.Count - 1 do
    begin
      s := header [i];
      if CompareText (SplitString (':', s), 'Face') = 0 then
      try
        facePngStrm := Nil;
        png := Nil;
        faceDecoder := TIdDecoderMIME.Create(nil);
        try
          facePngStrm := TMemoryStream.Create;
          png := TPngObject.Create;
          faceDecoder.DecodeToStream(s, facePngStrm);
          facePngStrm.Seek(0, soFromBeginning);
          png.LoadFromStream(facePngStrm);
          fxFace := TBitmap.Create;
          fxFace.Width := png.Width;
          fxFace.Height := png.Height;
          fxFace.Canvas.Draw(0, 0, png);

        finally
          faceDecoder.Free;
          facePngStrm.Free;
          png.Free;
        end;
        face := True;
      except
        FreeAndNil (fXFace);
      end
    end;

    if not face then
      for i := 0 to header.Count - 1 do
      begin
        s := header [i];
        if CompareText (SplitString (':', s), 'X-Face') = 0 then
        begin
          fXFace := TBitmap.Create;
          try
            xFace.Width := 48;
            xFace.Height := 48;
            xFace.PixelFormat := pf1Bit;
            if XFaceToBitmap (s, fXFace) < 0 then
              FreeAndNil (fXFace)
          except
            FreeAndNil (fXFace)
          end;
          break;
        end
      end
  end;

  result := fXFace;

end;

function TmvMessage.IdentifyMessagePartBoundary(const st: string; hdr : TMimeHeader): TmvMessagePartClass;
var
  i : Integer;
  cls : TmvMessagePartClass;
begin
  result := Nil;
  for i := 0 to RegisteredMessagePartCount - 1 do
  begin
    cls := RegisteredMessageParts [i];
    if cls.IsBoundary(st, hdr) then
    begin
      result := cls;
      break
    end
  end
end;

procedure TmvMessage.Lock;
begin
  fCS.Enter;
end;

(*----------------------------------------------------------------------*
 | procedure TmvMessage.PartialDecode                                   |
 |                                                                      |
 | Decode what we've got                                                |
 *----------------------------------------------------------------------*)
procedure TmvMessage.PartialDecode;
var
  st : string;
  mp : TmvMessagePart;
  hdr : TMimeHeader;

  procedure AddMessagePartLine (const st : string);
  var
    prevMessagePart : TmvMessagePart;
    pmp : TmvMIMEMessagePart;
  begin
    case fCurrentMessagePart.AddLine(st) of
      alEndOfMP :                     // End of message part detected
        begin
          if Assigned (fMPStack) and (fMPStack.Count > 0) then
            fMPStack.Pop;
          fCurrentMessagePart.fComplete := True;
          fCurrentMessagePart := Nil
        end;

      alNextMP :
        begin
          prevMessagePart := fCurrentMessagePart;
          if prevMessagePart is TmvMIMEMessagePart then
          begin
            pmp := TmvMIMEMessagePart (prevMessagePart);
            hdr := pmp.MultipartHeader;
            if Assigned (pmp.MIMEHeader) then
              pmp.MimeHeader.FileName
          end
          else
            hdr := MIMEHeader;
          fCurrentMessagePart.fComplete := True;
          fCurrentMessagePart := Nil;
          mp := TryCreateMessagePart (st, hdr);
          if Assigned (mp) then
          begin
            fCurrentMessagePart := mp;
            if Assigned (Hdr) then
              fCurrentMessagePart.InitMultipart (Hdr)
            else
              if not fCurrentMessagePart.ProcessHeaderLine(st) then

                                        // If this is the first and only header
                                        // line for the message part, create the
                                        // data area straight away - eg. begin 644

                if not Assigned (fCurrentMessagePart.fData) then
                  fCurrentMessagePart.fData := TMemoryStream.Create;

            fCurrentMessagePart.fParentMessagePart := prevMessagePart.fParentMessagePart
          end
        end;
      alMultipart :
        begin
          if not Assigned (fMPStack) then
            fMPStack := TStack.Create;
          fMPStack.Push(TmvMimeMessagePart (fCurrentMessagePart).MimeHeader);
          fCurrentMessagePart := Nil;
        end
    end
  end;

begin
  if fDecodeSize = RawData.Size then
    Exit;

  Lock;
  try
    try
      fDecodeSize := RawData.Size;

      if fDecodePos = 0 then        // We're starting a decode, reset everything
      begin
        fBody.Clear;
        fMessageParts.Clear;
        fCurrentMessagePart := Nil;
        FreeAndNil (fMIMEHeader);
        FreeAndNil (fMPStack);
        fCurrentLine := 0;
        if not fRawMode then
          fMIMEHeader := TMIMEHeader.CreateFromHeaderStrings(header, False);

      end;

      while GetLine (st) do                 // For each line...
      begin
        Inc (fCurrentLine);

                                            // Add it to current message part if we're
                                            // in one
        if fCurrentMessagePart <> Nil then
          AddMessagePartLine (st)
        else

        begin                               // Not currently in a message part

          if Assigned (MIMEHeader) and not MIMEHeader.IsMultipart then
          begin
            mp := TmvMimeMessagePart.Create(fMessageParts);
            mp.fOwner := self;
            mp.fHasRawData := PartialMessage;
            TmvMimeMessagePart (mp).InitForcedMessagePart(MIMEHeader);
            fCurrentMessagePart := mp;
            fCurrentMessagePart.fFileName := MimeHeader.FileName;
            fCurrentMessagePart.fData := TMemoryStream.Create;
            AddMessagePartLine (st)
          end
          else
          begin
            if fRawMode then
              mp := Nil
            else
              mp := TryCreateMessagePart (st, MIMEHeader);
                                              // Create a message part if we're at a
                                              // start boundary.

            if mp = Nil then                  // Not at a start boundary

              if fMessageParts.Count > 0 then // If we've already had message parts, we need
                                              // to stick the line in a 'body continuation'
                                              // message part
              begin
                mp := fMessageParts [fMessageParts.Count - 1];
                if not (mp is TmvBodyContinuationMessagePart) then
                  mp := TmvBodyContinuationMessagePart.Create (fMessageParts);
                mp.AddLine (st)
              end
              else
                fBody.Add(st)                 // No message parts yet - just add the line
                                              // to the body.
            else
            begin                             // We created a new message part!
              mp.fHasRawData := PartialMessage;
              fCurrentMessagePart := mp;
              if Assigned (MimeHeader) and (MimeHeader.IsMultipart) then
                fCurrentMessagePart.InitMultipart (MIMEHeader)
              else
                if not fCurrentMessagePart.ProcessHeaderLine(st) then

                                              // If this is the first and only header
                                              // line for the message part, create the
                                              // data area straight away - eg. begin 644

                  if not Assigned (fCurrentMessagePart.fData) then
                    fCurrentMessagePart.fData := TMemoryStream.Create
            end
          end
        end
      end
    except  // 'GetLine' may occasionally throw an AV if the currently loading message
            // is being displayed.
      fDecodePos := 0
    end
  finally
    Unlock
  end
end;

procedure TmvMessage.SetCodePage(const Value: Integer);
begin
  fCodePage := Value
end;

procedure TmvMessage.SetDormant(const Value: boolean);
var
  updating : boolean;
begin
  if Value <> Dormant then
    if Value then
    begin
      fDecodeSize := 0;
      fDecodePos := 0;
      fBody.Clear;
      fMessageParts.Clear;
      fCurrentMessagePart := Nil
    end
    else
    begin
      updating := fUpdating;
      EndUpdate;
      fUpdating := updating
    end
end;

(*----------------------------------------------------------------------*
 | procedure TmvMessage.SetHeader                                       |
 |                                                                      |
 | Parameters:                                                          |
 |   const value : TStrings                                             |
 *----------------------------------------------------------------------*)
procedure TmvMessage.SetHeader(const Value: TStrings);
begin
  fHeader.Assign (Value);
end;

(*----------------------------------------------------------------------*
 | function TmvMessage.TryCreateMessagePart : TmvMessagePart            |
 |                                                                      |
 | If the string is a start boundary string for a message part type     |
 | create a message part for the required type.  Otherwise return Nil   |
 |                                                                      |
 | Parameters:                                                          |
 |   const st : string          The potential boundary string           |
 |                                                                      |
 | The function returns a newly created message part if st is a         |
 | boundary string                                                      |
 *----------------------------------------------------------------------*)
procedure TmvMessage.SetRawMode(const Value: boolean);
begin
  if fRawMode <> Value then
  begin
    fRawMode := Value;
    fDecodeSize := 0;
    fDecodePos := 0;
  end
end;

function TmvMessage.TryCreateMessagePart(const st: string; hdr : TMimeHeader): TmvMessagePart;
var
  cls : TmvMessagePartClass;
begin
  result := Nil;

  cls := IdentifyMessagePartBoundary (st, hdr);
  if Assigned (cls) then
  begin
    result := cls.Create(fMessageParts);
    result.fOwner := self;
    result.fStartLine := fCurrentLine
  end
end;

{ TmvMessagePart }

(*----------------------------------------------------------------------*
 | function TmvMessagePart.AddLine : boolean                            |
 |                                                                      |
 | Add a line to a message part                                         |
 |                                                                      |
 | Parameters:                                                          |
 |   const st : string          The line to add.                        |
 |                                                                      |
 | The function returns 'st' was the end boundary for the message part  |
 *----------------------------------------------------------------------*)
function TmvMessagePart.AddLine(const st: string): TAddLine;
var
  s : string;
  mp : TmvMIMEMessagePart;
  mpHeader : TMimeHeader;
begin
  if IsBoundaryEnd (st) then
  begin
    result := alEndOfMP;
    fComplete := True
  end
  else
  begin
    result := alOK;
    if self is TmvMimeMessagePart then
    begin
      mp := TmvMimeMessagePart (self);
      mpHeader := mp.MultipartHeader
    end
    else
    begin
      mp := Nil;
      mpHeader := Nil
    end;


    if Assigned (fData) then    // Is it in the message body ?
    begin
      if IsBoundary (st, mpHeader) then
      begin
        result := alNextMP;
        exit
      end;

      if (self is TmvMimeMessagePart) and not Assigned (mpHeader) then
        if owner.IdentifyMessagePartBoundary(st, nil) <> Nil then
        begin
          result := alNextMP;
          exit
        end;

      s := st + #13#10;
      fData.Write(s [1], Length (s))
    end
    else                        // We're still in the header

      if not ProcessHeaderLine (st) then
      begin
        if Assigned (mp) then
          mpHeader := mp.MimeHeader;

        if Assigned (mpHeader) and mpHeader.IsMultipart then
          result := alMultipart
        else
          if not Assigned (fData) then
            fData := TMemoryStream.Create
      end
  end
end;

(*----------------------------------------------------------------------*
 | constructor TmvMessagePart.Create                                    |
 |                                                                      |
 | Constructor for TmvMessagePart                                       |
 |                                                                      |
 | Parameters:                                                          |
 |   AOwner : TCollection       The owning TmvMessageParts collection   |
 *----------------------------------------------------------------------*)
constructor TmvMessagePart.Create(AOwner: TCollection);
begin
  inherited;
  fInLine := True;
end;

procedure TmvMessagePart.DecodeGraphic (gc : TGraphicClass);
type
  bmf = record
    f : TBitmapFileHeader;
    i : TBitmapInfoHeader;
  end;
  pbmf = ^bmf;
var
  DecodedData : TMemoryStream;
  bmp : TBitmap;
  icon : HICON;
  ext : string;
  shfi : TSHFileInfo;
  i : Integer;
  x : DWORD;
  jpg : TJPegImage;
  p : pbmf;
  sz : DWORD;

begin
  if not fGotGraphic then
  begin
    if not Assigned (fGraphic) then
    begin
      if not fHasRawData then
        if not Assigned (gc) then
        begin
          ext := ExtractFileExt (FileName);
          gc := GetGraphicClass (ext)
        end;

      if Assigned (gc) and not fHasRawData then
      begin
        fGraphic := gc.Create;
        fGraphic.OnProgress := Owner.DoOnProgress;
      end
      else
      begin
        fHasRawData := True;
        bmp := TBitmap.Create;
        bmp.Width := 32;
        if bmp.Canvas.TextWidth(FileName) + 20 > bmp.Width then
          bmp.Width := bmp.Canvas.TextWidth (FileName) + 20;
        bmp.Height := 30 + 20 + 32;
        bmp.Canvas.TextRect(RECT (0, 0, bmp.Width, bmp.Height), 10, bmp.Height - 30, FileName);

        ZeroMemory (@shfi, SizeOf (shfi));
        SHGetFileInfo(PChar (FileName),
          FILE_ATTRIBUTE_NORMAL,
          shfi, sizeof(shfi),
          SHGFI_ICON or SHGFI_USEFILEATTRIBUTES);

        icon := shfi.hIcon;

        if icon <> 0 then
        begin
          DrawIcon (bmp.Canvas.Handle, (bmp.Canvas.TextWidth (FileName) + 20 - 32) div 2 , 10, icon);
          DestroyIcon (icon);
        end;
        fGraphic := bmp
      end
    end;

    if Assigned (fGraphic) and not fHasRawData then
    begin
      DecodedData := TMemoryStream.Create;
      try
        GetData (DecodedData);

        DecodedData.Seek(0, soFromBeginning);

// Some people posts JPEG images with preliminary data before the JPEG header
// Try to salvage something, though you may end up with just a thumbnail.

        if fGraphic is TJPEGImage then
        begin
          i := 0;
          while i < DecodedData.Size do
          begin
            x := PDWORD (PChar (DecodedData.Memory) + i)^;
            if x = $e0ffd8ff then
            begin
              DecodedData.Seek (i, soFromBeginning);
              break
            end
            else
              Inc (i)
          end;

          jpg := TJPegImage (fGraphic);
          if not Complete then
            if jpg.ProgressiveEncoding then
              fOverridePercent := True;

          jpg.ProgressiveDisplay := jpg.ProgressiveEncoding and Complete;
          if Complete then
            jpg.Performance := jpBestQuality
          else
            jpg.Performance := jpBestSpeed
        end;

        try
          if fGraphic is TBitmap then
          begin
            p := pbmf (DecodedData.Memory);
            sz := p^.i.biWidth * p^.i.biHeight * p^.i.biBitCount div 8;
            if sz < P^.i.biSizeImage then
              p^.i.biSizeImage := 0;
          end
        except
        end;

        try
          fGraphic.LoadFromStream (DecodedData)
        except
        end
      finally
        DecodedData.Free
      end
    end;

    fGotGraphic := Complete
  end
end;

(*----------------------------------------------------------------------*
 | destructor TmvMessagePart.Destroy                                    |
 |                                                                      |
 | Destructor for TmvMessagePart                                        |
 *----------------------------------------------------------------------*)
destructor TmvMessagePart.Destroy;
begin
  fData.Free;
  fGraphic.Free;

  inherited;
end;

(*----------------------------------------------------------------------*
 | function TmvMessagePart.GetBody : TStrings                           |
 |                                                                      |
 | Get the message part body.  This may be overridden to get a text     |
 | body.  Othewise return Nil to trigger the display of the graphic     |
 *----------------------------------------------------------------------*)
function TmvMessagePart.GetBody: TStrings;
begin
  result := Nil
end;

(*----------------------------------------------------------------------*
 | procedure TmvMessagePart.GetData                                     |
 |                                                                      |
 | Get decoded data for attachments.  nb  This may well be overriden    |
 | to decode the data, etc.                                             |
 |                                                                      |
 | Parameters:                                                          |
 |   s : TStream        The stream to fill with the data                |
 *----------------------------------------------------------------------*)
procedure TmvMessagePart.GetData(s: TStream);
begin
  if Assigned (fData) then
    s.CopyFrom(fData, 0)
end;

(*----------------------------------------------------------------------*
 | function TmvMessagePart.GetGraphic : TGraphic                        |
 |                                                                      |
 | Get a graphic representation of the data.  If this is not overridden |
 | return an Icon placeholder for display                               |
 |                                                                      |
 | The function returns the icon placeholder                            |
 *----------------------------------------------------------------------*)
function TmvMessagePart.GetDecodeType: TDecodeType;
begin
  result := fDecodeType;
end;

function TmvMessagePart.GetFileName: string;
begin
  result := StringReplace (fFileName, ':', '_', [rfReplaceAll]);
  result := StringReplace (result, '\', '_', [rfReplaceAll]);
  result := StringReplace (result, '/', '_', [rfReplaceAll]);
end;

function TmvMessagePart.GetGraphic: TGraphic;
begin
  result := Application.Icon
end;

function TmvMessagePart.GetIsHTMLMultipart: boolean;
begin
  result := False
end;

function TmvMessagePart.GetMIMEContentType: string;
begin
  result := '';
end;

function TmvMessagePart.GetPercentDecoded: Integer;
var
  article : TArticle;
begin
  try
    if Complete or fOverridePercent then
      result := 100
    else
    begin
      if Assigned (Owner) then
        article := TArticle (Owner.Obj)
      else
        article := Nil;
      if Assigned (article) and (article.Lines > 0) then
      begin
        result := ((Owner.fCurrentLine - fStartLine) * 100) div article.Lines - 15;
        if result < 0 then
          result := 0
      end
      else
        result := 0
    end
  except
    result := 0
  end
end;

procedure TmvMessagePart.InitMultipart (multipartHeader : TMIMEHeader);
begin
// stub
end;

(*----------------------------------------------------------------------*
 | class function TmvMessagePart.IsBoundary : boolean                   |
 |                                                                      |
 | Stub function                                                        |
 *----------------------------------------------------------------------*)
class function TmvMessagePart.IsBoundary(const st: string; MIMEHeader : TMIMEHeader): boolean;
begin
  result := false;
end;

(*----------------------------------------------------------------------*
 | function TmvMessagePart.IsBoundaryEnd : boolean                      |
 |                                                                      |
 | Stub function                                                        |
 *----------------------------------------------------------------------*)
function TmvMessagePart.IsBoundaryEnd(const st: string): boolean;
begin
  result := False
end;

(*----------------------------------------------------------------------*
 | TmvMessagePart.ProcessHeaderLine                                     |
 |                                                                      |
 | Process a header line.  Sub.  Returns true, indicating we'e still in |
 | the header.                                                          |
 *----------------------------------------------------------------------*)
function TmvMessagePart.MatchesCID(const cid: string): boolean;
begin
  result := False
end;

function TmvMessagePart.ProcessHeaderLine(const st: string) : boolean;
begin
  result := True
end;

{ TmvMessageParts }

(*----------------------------------------------------------------------*
 | TmvMessageParts.GetItems                                             |
 |                                                                      |
 | Return the specified message part                                    |
 *----------------------------------------------------------------------*)
function TmvMessageParts.GetItems(idx: Integer): TmvMessagePart;
begin
  result := TmvMessagePart (inherited Items [idx])
end;


{ TThreadsafeMemoryStream }

(*----------------------------------------------------------------------*
 | TThreadsafeMemoryStream.Create                                       |
 |                                                                      |
 | Constructor for TThreadsafeMemoryStream                              |
 *----------------------------------------------------------------------*)
constructor TThreadsafeMemoryStream.Create;
begin
  inherited;
  fCriticalSection := TCriticalSection.Create;
end;

(*----------------------------------------------------------------------*
 | TThreadsafeMemoryStream.Destroy                                      |
 |                                                                      |
 | Destructor for TThreadsafeMemoryStream                               |
 *----------------------------------------------------------------------*)
destructor TThreadsafeMemoryStream.Destroy;
begin
  fCriticalSection.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure TThreadsafeMemoryStream.Lock                               |
 |                                                                      |
 | Lock the memory stream.  No one else can write to it                 |
 *----------------------------------------------------------------------*)
procedure TThreadsafeMemoryStream.Lock;
begin
  fCriticalSection.Enter
end;

(*----------------------------------------------------------------------*
 | procedure TThreadsafeMemoryStream.UnLock                             |
 |                                                                      |
 | UnLock the memory stream.  People can write to it again              |
 *----------------------------------------------------------------------*)
procedure TThreadsafeMemoryStream.Unlock;
begin
  fCriticalSection.Leave
end;

(*----------------------------------------------------------------------*
 | function TThreadsafeMemoryStream.Write                               |
 |                                                                      |
 | Override the default 'write' method to lock/unlock.  Prevents        |
 | other threads from reading/writing while this thread is writing      |
 *----------------------------------------------------------------------*)
function TThreadsafeMemoryStream.Write(const Buffer;
  Count: Integer): Longint;
begin
  Lock;
  try
    result := inherited Write (buffer, count)
  finally
    Unlock
  end
end;

{ TmvTextMessagePart }


(*----------------------------------------------------------------------*
 | constructor TmvTextMessagePart.Create                                |
 |                                                                      |
 | Constructor for TmvTextMessgePart                                    |
 |                                                                      |
 | Parameters:                                                          |
 |   AOwner : TCollection                                               |
 *----------------------------------------------------------------------*)
constructor TmvTextMessagePart.Create(AOwner: TCollection);
begin
  inherited;

  fBody := TStringList.Create;
end;

(*----------------------------------------------------------------------*
 | destructor TmvTextMessagePart.Destroy                                |
 |                                                                      |
 | Destructor for TmvTextMessagePart                                    |
 *----------------------------------------------------------------------*)
destructor TmvTextMessagePart.Destroy;
begin
  fBody.Free;

  inherited;
end;

(*----------------------------------------------------------------------*
 | function TmvTextMessagePart.GetBody : TStrings                       |
 |                                                                      |
 | GetBody returns the text body strings                                |
 *----------------------------------------------------------------------*)
function TmvTextMessagePart.GetBody: TStrings;
begin
  result := fBody
end;

(*----------------------------------------------------------------------*
 | function TmvTextMessagePart.ProcessHeaderLine                        |
 |                                                                      |
 | There's no 'header/data' concept in non-mime text message parts so   |
 | this is a special case.  Stay in the header, but add each line to    |
 | the body.                                                            |
 |                                                                      |
 | Parameters:                                                          |
 |   const st : string          The line to add                         |
 |                                                                      |
 | The function returns true to stay in the header                      |
 *----------------------------------------------------------------------*)
function TmvTextMessagePart.ProcessHeaderLine(const st: string): boolean;
begin
  if not Assigned (fBody) then
    fBody := TStringList.Create;
  fBody.Add(st);
  result := True
end;

{ TMimeHeader }

procedure TMimeHeader.Assign(const AHeader: TMimeHeader);
begin
  fContentTypeSubtype := AHeader.fContentTypeSubtype;
  fContentTypeType := AHeader.fContentTypeType;
  fContentTransferEncoding := AHeader.fContentTransferEncoding;
  fContentID := AHeader.fContentID;
  fContentDescription := AHeader.fContentDescription;
  fContentDisposition := AHeader.fContentDisposition;

  if Assigned (Aheader.fContentTypeAttributes) then
  begin
    if not Assigned (fContentTypeAttributes) then
      fContentTypeAttributes := TStringList.Create;
    fContentTypeAttributes.Assign(AHeader.fContentTypeAttributes)
  end
  else
    FreeAndNil (fContentTypeAttributes);

  if Assigned (Aheader.fcontentDispositionAttributes) then
  begin
    if not Assigned (fcontentDispositionAttributes) then
      fcontentDispositionAttributes := TStringList.Create;
    fcontentDispositionAttributes.Assign(AHeader.fcontentDispositionAttributes)
  end
  else
    FreeAndNil (fcontentDispositionAttributes);
end;

class function TMimeHeader.CreateFromHeaderStrings (AHeader: TStrings; forceMIME : boolean) : TMIMEHeader;
var
  i : Integer;
  s1, st : string;
  mimeHeader : TMimeHeader;
  gotVersion : boolean;
  gotContentType : boolean;

  procedure ParseMIMEVersion (var st : string);
  begin
    if SplitString (';', st) = '1.0' then
    begin
      gotVersion := True;
      if not Assigned (mimeHeader) then
        mimeHeader := TMimeHeader.Create
    end
  end;

  procedure ParseContentType (var st : string);
  var
    s1, attribName : string;
  begin
    gotContentType := True;
    if Pos ('/', st) > 0 then
    begin
      mimeHeader.fContentTypeType := SplitString ('/', st);
      mimeHeader.fContentTypeSubtype := SplitString (';', st)
    end
    else
      mimeHeader.fContentTypeType := SplitString (';', st);

    while st <> '' do
    begin
      s1 := SplitString (';', st);
      attribName := SplitString ('=', s1);

      if s1 <> '' then
      begin
        if not Assigned (mimeHeader.fContentTypeAttributes) then
        begin
          mimeHeader.fContentTypeAttributes := TStringList.Create;
          mimeHeader.fContentTypeAttributes.CaseSensitive := False
        end;

        mimeHeader.fContentTypeAttributes.Add(attribName + '=' + AnsiDequotedStr (s1, '"'))
      end
    end
  end;

  procedure ParseContentTransferEncoding (var st : string);
  begin
    mimeHeader.fContentTransferEncoding := SplitString (';', st);
  end;

  procedure ParseContentID (var st : string);
  begin
    mimeHeader.fContentID := SplitString (';', st);
  end;

  procedure ParseContentDescription (var st : string);
  begin
    mimeHeader.fContentDescription := SplitString (';', st)
  end;

  procedure ParseContentDisposition (var st : string);
  var
    s1, attribName : string;
  begin
    mimeHeader.fContentDisposition := SplitString ('/', st);

    while st <> '' do
    begin
      s1 := SplitString (';', st);
      attribName := SplitString ('=', s1);

      if s1 <> '' then
      begin
        if not Assigned (mimeHeader.fContentDispositionAttributes) then
        begin
          mimeHeader.fContentDispositionAttributes := TStringList.Create;
          mimeHeader.fContentDispositionAttributes.CaseSensitive := False
        end;

        mimeHeader.fContentDispositionAttributes.Add(attribName + '=' + AnsiDequotedStr (s1, '"'))
      end
    end
  end;

  procedure ParseAdditionalMIMEHeader (var st, s1 : string);
  begin
  end;

begin
  gotContentType := False;
  if forceMime then
  begin
    gotVersion := True;
    mimeHeader := TMimeHeader.Create
  end
  else
  begin
    gotVersion := False;
    mimeHeader := Nil
  end;

  for i := 0 to AHeader.Count  - 1 do
  begin
    st := AHeader [i];

    s1 := SplitString (':', st);

    if not Assigned (mimeHeader) and (CompareMem (PChar (s1), PChar ('Content-'), 8) or SameText (s1, 'X-RFC2646')) then
      if not Assigned (mimeHeader) then
        mimeHeader := TMimeHeader.Create;

    if CompareText (s1, 'MIME-Version') = 0 then
      ParseMIMEVersion (st)
    else
      if SameText (s1, 'X-RFC2646') then
      begin
        mimeHeader.HandleXRFC2646 (st);
        gotVersion := True
      end
      else
      if Assigned (MIMEHeader) and (CompareText (SplitString ('-', s1), 'Content') = 0) then
        if CompareText (s1, 'Type') = 0 then
          ParseContentType (st)
        else
          if CompareText (s1, 'Transfer-Encoding') = 0 then
            ParseContentTransferEncoding (st)
          else
            if CompareText (s1, 'ID') = 0 then
              ParseContentID (st)
            else
              if CompareText (s1, 'Description') = 0 then
                ParseContentDescription (st)
              else
                if CompareText (s1, 'Disposition') = 0 then
                  ParseContentDisposition (st)
                else
                  ParseAdditionalMIMEHeader (s1, st)
  end;

  if Assigned (mimeHeader) then
  begin
    if mimeHeader.fContentTypeType = '' then
    begin
      mimeHeader.fContentTypeType := 'text';
      mimeHeader.fContentTypeSubType := 'plain'
    end
  end;

  if gotVersion or gotContentType then
    result := mimeHeader
  else
  begin
    mimeHeader.Free;
    result := Nil
  end
end;

destructor TMimeHeader.Destroy;
begin
  fContentTypeAttributes.Free;
  fContentDispositionAttributes.Free;

  inherited;
end;

function TMimeHeader.GetDecodeType: TDecodeType;
begin
  result := ttText;

  if CompareText (ContentTransferEncoding, 'Base64') = 0 then
    result := ttBase64
  else
    if (CompareText (ContentTransferEncoding, 'UUENCODE') = 0) or (CompareText (ContentTransferEncoding, 'x-uuencode') = 0) then
      result := ttUUEncode
    else
      if CompareText (ContentTransferEncoding, 'quoted-printable') = 0 then
        result := ttQuotedPrintable
end;

function TMimeHeader.GetFileName: string;
begin
  result := '';
  if Assigned (fContentDispositionAttributes) then
    result := ExtractFileName (fContentDispositionAttributes.Values ['filename']);

  if result = '' then
    if Assigned (fContentTypeAttributes) then
      result := ExtractFileName (fContentTypeAttributes.Values ['filename']);

  if result = '' then
    if Assigned (fContentTypeAttributes) then
      result := ExtractFileName (fContentTypeAttributes.Values ['name']);

  if result = '' then
    if CompareText (ContentType_Type, 'Message') = 0 then
      result := '*.eml';

  if result = '' then
    if CompareText (ContentType_Type, 'audio') = 0 then
      result := '*.wav';

  if result = '' then
    if CompareText (ContentType_Type, 'Image') = 0 then
      if ContentType_Subtype <> '' then
        result := '*.' + ContentType_SubType;

  if (result <> '') and (result [1] = '*') then
    if ContentDescription <> '' then
      result := ContentDescription + Copy (result, 2, MaxInt)
    else
      result := 'inline' + Copy (result, 2, maxInt);

  if result = '' then
    result := ContentDescription;

  if result = '' then
  begin
    result := ContentType_Type;
    if ContentType_SubType <> '' then
      result := Result + '/' + ContentType_SubType
  end
end;

function TMimeHeader.GetImageType: string;
begin
  if CompareText (ContentType_Type, 'image') = 0 then
    result := ContentType_Subtype
  else
    result := ''
end;

function TMimeHeader.GetIsMultipart: boolean;
begin
  result := CompareText (ContentType_Type, 'Multipart') = 0

end;

function TMimeHeader.GetMultipartBoundary : string;
begin
  if Assigned (fContentTypeAttributes) then
    result := fContentTypeAttributes.Values ['boundary']
  else
    result := '';
end;

procedure TmvMessage.Unlock;
begin
  fCS.Leave
end;

procedure TMimeHeader.HandleXRFC2646(const st: string);
var
  s, s1 : String;
begin
  s := st;
  s1 := SplitString (';', s);
  s := SplitString ('=', s1);

  if SameText (s, 'format') and (s1 <> '') then
  begin
    if not Assigned (fContentTypeAttributes) then
      fContentTypeAttributes := TStringList.Create;

    fContentTypeAttributes.Values ['Format'] := s1;
  end
end;

end.
