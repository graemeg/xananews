(*======================================================================*
 | unitMessages unit for NewsReader3                                    |
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
 | Copyright © Colin Wilson 2002  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      02/08/2001  CPWW  Original                                  |
 *======================================================================*)

unit unitMessages;

interface

uses
  Windows, Classes, SysUtils, ConTnrs, Graphics, forms, SyncObjs, ShellAPI,
  StrUtils, XnClasses, XnRawByteStrings;

type
  TmvMessagePart = class;
  TmvMessagePartClass = class of TmvMessagePart;
  TDecodeType = (ttText, ttBase64, ttUUEncode, ttYEnc, ttQuotedPrintable);

  TmvMessageParts = class(TOwnedCollection)
  private
    function GetItems(idx: Integer): TmvMessagePart;
  public
    property Items[idx: Integer]: TmvMessagePart read GetItems; default;
  end;

  TThreadsafeMemoryStream = class(TMemoryStream)
  private
    fCriticalSection: TCriticalSection;
  protected
    function GetSize: Int64; override;
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
    function GetIsMultipart: Boolean;
    function GetMultipartBoundary: string;
  public
    class function CreateFromHeaderStrings(AHeader: TAnsiStrings; forceMIME: Boolean): TMimeHeader;
    destructor Destroy; override;

    procedure Assign(const AHeader: TMimeHeader);
    procedure HandleXRFC2646(const st: string);

    property ContentType_Type: string read fContentTypeType;
    property ContentType_Subtype: string read fContentTypeSubtype;
    property ContentType_Attributes: TStringList read fContentTypeAttributes;
    property ContentTransferEncoding: string read fContentTransferEncoding;
    property ContentID: string read fContentID;
    property ContentDescription: string read fContentDescription;
    property ContentDisposition: string read fContentDisposition;
    property ContentDispositionAttributes: TStringList read fContentDispositionAttributes;

    property ImageType: string read GetImageType;
    property DecodeType: TDecodeType read GetDecodeType;
    property FileName: string read GetFileName;
    property IsMultipart: Boolean read GetIsMultipart;
    property MultipartBoundary: string read GetMultipartBoundary;
  end;

  TmvMessage = class(TPersistent)
  private
    fDefaultCodePage: Integer;
    fAlternateHTML: Boolean;
    fAlternateMessagePartCount: Integer;
    fTruncateFrom: string;
    fStrictSigSeparator: Boolean;
    function GetMIMEHeader: TMIMEHeader;
    function GetCodePage: Integer;
    function GetTextPart: TAnsiStrings;
    function GetXFace: TBitmap;
    procedure SetCodePage(const Value: Integer);
    function GetAlternateMessagePart(idx: Integer): TmvMessagePart;
    function GetAlternateMessagePartCount: Integer;
  private
    fData: TThreadsafeMemoryStream;
    fBody: TAnsiStrings;
    fHeader: TAnsiStrings;
    fXFace: TBitmap;
    fMessageParts: TmvMessageParts;
    fUpdating: Boolean;
    fDecodePos: Integer;
    fDecodeSize: Integer;
    fCurrentMessagePart: TmvMessagePart;
    fBeingDisplayed: Boolean;
    fObject: TObject;
    fRawMode: Boolean;
    fPartialMessage: Boolean;
    fGotXFace: Boolean;
    fMIMEHeader: TMIMEHeader;
    fMPStack: TStack;
    fCS: TCriticalSection;
    fCurrentLine: Integer;
    fCodePage: Integer;
    procedure SetHeader(const Value: TAnsiStrings);
    procedure Decode;
    procedure PartialDecode;
    function GetBody: TAnsiStrings;
    function GetMessageParts: TmvMessageParts;
    function GetDormant: Boolean;
    procedure SetDormant(const Value: Boolean);
    procedure SetRawMode(const Value: Boolean);

  protected
    function GetLine(var st: RawByteString): Boolean;
    function TryCreateMessagePart(const st: string; hdr: TMimeHeader): TmvMessagePart;
  public
    constructor Create(AObject: TObject);
    destructor Destroy; override;
    property MIMEHeader: TMIMEHeader read GetMIMEHeader;
    procedure DoOnProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);

    function IdentifyMessagePartBoundary(const st: string; hdr: TMimeHeader): TmvMessagePartClass;

    procedure Clear;
    procedure AddData(data: TStream);

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Lock;
    procedure Unlock;

    function FindMessagePartFromCID(const cid: string): TmvMessagePart;

    property Updating: Boolean read fUpdating;

    property RawData: TThreadsafeMemoryStream read fData;
    property Header: TAnsiStrings read fHeader write SetHeader;
    property Codepage: Integer read GetCodePage write SetCodePage;

    property Body: TAnsiStrings read GetBody;
    property XFace: TBitmap read GetXFace;
    property TextPart: TAnsiStrings read GetTextPart;
    procedure DecodeBody(f1: TStream; decodeType: TDecodeType);
    property MessageParts: TmvMessageParts read GetMessageParts;
    property Dormant: Boolean read GetDormant write SetDormant;
    property BeingDisplayed: Boolean read fBeingDisplayed write fBeingDisplayed;
    property Obj: TObject read fObject;
    property RawMode: Boolean read fRawMode write SetRawMode;
    property PartialMessage: Boolean read fPartialMessage write fPartialMessage;
    property DefaultCodePage: Integer read fDefaultCodePage write fDefaultCodePage;

    property AlternateHTML: Boolean read fAlternateHTML write fAlternateHTML;
    property AlternateMessagePartCount: Integer read GetAlternateMessagePartCount;
    property AlternateMessagePart[idx: Integer]: TmvMessagePart read GetAlternateMessagePart;
    property TruncateFrom: string read fTruncateFrom write fTruncateFrom;
    property StrictSigSeparator: Boolean read fStrictSigSeparator write fStrictSigSeparator;
  end;


  TAddLine = (alOK, alEndOfMP, alNextMP, alMultipart);

  TmvMessagePart = class(TCollectionItem)
  private
    fOwner: TmvMessage;
    fHasRawData: Boolean;
    fParentMessagePart: TmvMessagePart;
    function GetPercentDecoded: Integer;
  protected
    fData: TMemoryStream;
    fGraphic: TGraphic;
    fFileName: string;
    fInline: Boolean;
    fComplete: Boolean;
    fGotGraphic: Boolean;
    fDecodeType: TDecodeType;
    fStartLine: Integer;
    fOverridePercent: Boolean;

    class function IsBoundary(const st: string; MIMEHeader: TMIMEHeader): Boolean; virtual;
    function IsBoundaryEnd(const st: string): Boolean; virtual;
    function ProcessHeaderLine(const st: RawByteString): Boolean; virtual; // Return False when header is complete
    function AddLine(const st: RawByteString): TAddLine; virtual;
    function GetGraphic: TGraphic; virtual;
    function GetBody: TAnsiStrings; virtual;
    procedure DecodeGraphic(gc: TGraphicClass);
    procedure InitMultipart(multipartHeader: TMIMEHeader); virtual;
    function GetDecodeType: TDecodeType; virtual;
    function GetFileName: string; virtual;
    function MatchesCID(const cid: string): Boolean; virtual;
    function GetMIMEContentType: string; virtual;
    function GetIsHTMLMultipart: Boolean; virtual;
  public
    constructor Create(AOwner: TCollection); override;
    destructor Destroy; override;
    property Owner: TmvMessage read fOwner;
    procedure GetData(s: TStream); virtual;
    property Graphic: TGraphic read GetGraphic;
    property Body: TAnsiStrings read GetBody;
    property FileName: string read GetFileName;
    property Complete: Boolean read fComplete;
    property HasRawData: Boolean read fHasRawData;
    property DecodeType: TDecodeType read GetDecodeType;
    property PercentDecoded: Integer read GetPercentDecoded;
    property MIMEContentType: string read GetMIMEContentType;
    property IsHTMLMultipart: Boolean read GetIsHTMLMultipart;
  end;

  TmvTextMessagePart = class(TmvMessagePart)
  private
    fBody: TAnsiStrings;
  protected
    function ProcessHeaderLine(const st: RawByteString): Boolean; override;
    function GetBody: TAnsiStrings; override;
  public
    constructor Create(AOwner: TCollection); override;
    destructor Destroy; override;
  end;

  TmvBodyContinuationMessagePart = class(TmvTextMessagePart)
  end;


var
  gCurrentMessage: TmvMessage;

procedure RegisterMessagePart(cls: TmvMessagePartClass);
function GetGraphicClass(ext: string): TGraphicClass;

implementation

uses
  GIFImg, Jpeg, PngImage, unitMessageMime, NewsGlobals, idCoder, idCoderUUE,
  idCoderMIME, unitCharsetMap, XnXFace, unitNNTPServices, unitSearchString, XnCoderUUE;

var
  registeredMessageParts: array of TmvMessagePartClass;
  registeredMessagePartCount: Integer = 0;

procedure RegisterMessagePart(cls: TmvMessagePartClass);
var
  found: Boolean;
  i: Integer;
begin
  // Register a message part decoder for additional decoding - see
  // unitMessageNNTPBinary and unitMessageMime                            
  found := False;
                                // Is it already registered?
  for i := 0 to registeredMessagePartCount - 1 do
    if registeredMessageParts[i] = cls then
    begin
      found := True;
      Break;
    end;

  if not found then             // Add it to the array of message classes
  begin
    if Length(registeredMessageParts) = registeredMessagePartCount then
      SetLength(registeredMessageParts, Length(registeredMessageParts) + 10);

    registeredMessageParts[registeredMessagePartCount] := cls;

    Inc(registeredMessagePartCount);
  end;
end;

function GetGraphicClass(ext: string): TGraphicClass;
begin
  Result := nil;
  if ext <> '' then
  begin
    ext := UpperCase(ext);
    if ext[1] = '.' then
      ext := Copy(ext, 2, MaxInt);

    if ext = 'BMP' then
      Result := TBitmap
    else
      if (ext = 'JPG') or (ext = 'JPEG') then
        Result := TJPEGImage
      else
        if ext = 'GIF' then
          Result := TGIFImage
        else
          if ext = 'PNG' then
            Result := TPngImage
          else
            if ext = 'ICO' then
              Result := TIcon;
  end;
end;

{ TmvMessage }

procedure TmvMessage.AddData(data: TStream);
begin
  fData.Seek(0, soEnd);
  fData.CopyFrom(data, 0);
end;

procedure TmvMessage.BeginUpdate;
begin
  fDecodePos := 0;
  fUpdating := True;
end;

procedure TmvMessage.Clear;
begin
  fHeader.Clear;
  fData.Size := 0;
  Decode;
end;

constructor TmvMessage.Create(AObject: TObject);
begin
  inherited Create;
  fDefaultCodePage := CP_USASCII;
  fCodePage := -1;
  fCS := TCriticalSection.Create;
  fObject := AObject;
  fData := TThreadsafeMemoryStream.Create;
  fBody := TAnsiStringList.Create;
  fHeader := TAnsiStringList.Create;
  fMessageParts := TmvMessageParts.Create(Self, TmvMessagePart);
  fAlternateHTML := True;
  fAlternateMessagePartCount := -1;
end;

procedure TmvMessage.Decode;
var
  i: Integer;
begin
  // Decode the message.  Only called when the complete message has been
  // downloaded.  Called by GetBody and GetMessageParts.
  if not fUpdating then
  begin
    fDecodePos := 0;
    PartialDecode;

    for i := 0 to fMessageParts.Count - 1 do
      fMessageParts[i].fComplete := True;
  end;
end;

procedure TmvMessage.DecodeBody(f1: TStream; decodeType: TDecodeType);
var
  decoder: TidDecoder;
  i: Integer;
begin
  decoder := nil;
  case decodeType of
    ttUUEncode: decoder := TXnDecoderUUE.Create(nil);
    ttBase64  : decoder := TidDecoderMIME.Create(nil)
  end;

  if Assigned(decoder) then
  try
    decoder.DecodeBegin(f1);
    for i := 0 to Body.Count - 1 do
      decoder.Decode(string(Body[i]));
  finally
    decoder.Free;
  end;
end;

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
  inherited Destroy;
end;

procedure TmvMessage.DoOnProgress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
  const Msg: string);
begin
//  fmMain.StatusBar.Panels[0].Text := IntToStr (PercentDone);
end;

procedure TmvMessage.EndUpdate;
var
  i: Integer;
begin
  fUpdating := False;
  PartialDecode;
  fCodePage := -1;
  for i := 0 to fMessageParts.Count - 1 do
    fMessageParts[i].fComplete := True;
end;

function TmvMessage.FindMessagePartFromCID(const cid: string): TmvMessagePart;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to MessageParts.Count - 1 do
    if MessageParts[i].MatchesCID(cid) then
    begin
      Result := MessageParts[i];
      Break;
    end;
end;

function TmvMessage.GetAlternateMessagePart(idx: Integer): TmvMessagePart;
begin
  Result := MessageParts[idx];
end;

function TmvMessage.GetAlternateMessagePartCount: Integer;
begin
  Result := MessageParts.Count;
end;

function TmvMessage.GetBody: TAnsiStrings;
begin
  if fUpdating then
    PartialDecode
  else
    Decode;
  Result := fBody;
end;

function TmvMessage.GetCodePage: Integer;
var
  s, ext: string;
  art: TArticle;
  I: Integer;
  mh: TMimeHeader;
begin
  if fCodePage = -1 then
  begin
    if fUpdating then
      PartialDecode
    else
      Decode;

    if Assigned(MimeHeader) and Assigned(MimeHeader.ContentType_Attributes) then
    begin
      s := MimeHeader.ContentType_Attributes.Values['charset'];
      if s = '' then
      begin
        if fUpdating then
        begin
          fCodePage := -1;
          Result := CP_USASCII;
          Exit;
        end
        else
        begin
          fCodePage := fDefaultCodePage;
          for I := 0 to fMessageParts.Count - 1 do
            if fMessageParts[I] is TmvMimeMessagePart then
            begin
              mh := TmvMimeMessagePart(fMessageParts[I]).MimeHeader;
              if Assigned(mh) and Assigned(mh.ContentType_Attributes) then
              begin
                s := mh.ContentType_Attributes.Values['charset'];
                if s <> '' then
                begin
                  fCodePage := MIMECharsetNameToCodePage(s);
                  Break;
                end;
              end;
            end;
        end;
      end
      else
        fCodePage := MIMECharsetNameToCodePage(s);
    end
    else
    begin
      fCodePage := -1;
      art := TArticle(Obj);
      if Assigned(art) and (Length(art.FromName) > 0) then
        if art.FromName[1] > #127 then
        begin
          ext := ExtractFileExt(art.FromEmail);
          fCodePage := URLSuffixToCodePage(ext);
        end;

      if fCodePage = -1 then
        if fObject is TArticleBase then
          fCodePage := TArticleBase(fObject).fCodePage // nb - must be fCodePage!
        else
          fCodePage := MIMECharsetNameToCodePage('');

      if fCodePage = -1 then
        if fUpdating then
        begin
          Result := CP_USASCII;
          Exit;
        end
        else
          fCodePage := fDefaultCodePage;
    end;
  end;

  Result := fCodePage;
end;

function TmvMessage.GetDormant: Boolean;
begin
  Result := (fDecodePos = 0) and (fDecodeSize = 0);
end;

function TmvMessage.GetLine(var st: RawByteString): Boolean;
var
  pch, pch1: PAnsiChar;
  p, l: Integer;
{$IFDEF CPUX64}
  pch2: PAnsiChar;
  i: Integer;
{$ENDIF}
begin
  // Get a line of the message text.  This is threadsafe so we can add
  // data to the message while decoding what we've got so far.
  // The function returns true if we got a complete line of text
  try
    pch := fData.Memory;
    l := fData.Size;
    p := fDecodePos;

    Dec(l, p);
    Inc(pch, p);

{$IFDEF CPUX64}
    pch1 := nil;
    pch2 := pch;
    // Kind of like StrScan with a Length parameter!
    for i := 1 to l do
    begin
      if pch2^ = #10 then
      begin
        pch1 := pch2;
        Break;
      end;
      Inc(pch2);
    end;
{$ELSE}
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
      pch1 := nil;
{$ENDIF}

    if pch1 <> nil then
    begin
      l := pch1 - pch;
      Inc(fDecodePos, l + 1);
      Dec(pch1);
      while (l > 0) and (pch1^ = #13) do
      begin
        Dec(l);
        Dec(pch1);
      end;

      SetLength(st, l);
      if l > 0 then
        Move(pch^, st[1], l);
      Result := True;
    end
    else
      Result := False;
  except
    st := '';
    Sleep(100);
    Result := False;
  end;
end;

function TmvMessage.GetMessageParts: TmvMessageParts;
begin
  if fUpdating then
    PartialDecode
  else
    Decode;
  Result := fMessageParts;
end;

function TmvMessage.GetMIMEHeader: TMIMEHeader;
begin
  if Assigned(fMPStack) and (fMPStack.Count > 0) then
    Result := fMPStack.Peek
  else
    Result := fMIMEHeader;
end;

function TmvMessage.GetTextPart: TAnsiStrings;
var
  i: Integer;
begin
  if (Body.Count > 0) and not (Assigned(MIMEHeader) and MIMEHeader.IsMultipart) then
    Result := Body
  else
  begin
    Result := nil;
    i := 0;

    while (Result = nil) and (i < MessageParts.Count) do
    begin
      if Assigned(MessageParts[i].Body) and (MessageParts[i].Body.Count > 0) then
        Result := MessageParts[i].Body;
      Inc(i);
    end;
  end;
end;

function TmvMessage.GetXFace: TBitmap;
var
  i: Integer;
  s: string;
  face: Boolean;
  faceDecoder: TIdDecoderMIME;
  facePngStrm: TStream;
  png: TPngImage;
  raw: RawByteString;
begin
  if not fGotXFace then
  begin
    fGotXFace := True;

    face := False;
    for i := 0 to header.Count - 1 do
    begin
      s := string(header[i]);
      if CompareText(SplitString(':', s), 'Face') = 0 then
      try
        s := StringReplace(s, ' ', '', [rfReplaceAll]);      // Remove folding spaces from Face headers
        s := StringReplace(s,  #9, '', [rfReplaceAll]);      // Note: older versions did not remove FWS when receiving
                                                             //       face-headers, as the current versions does.
                                                             //       So these were stored in the message.fat file.
        facePngStrm := nil;
        png := nil;
        faceDecoder := TIdDecoderMIME.Create(nil);
        try
          facePngStrm := TMemoryStream.Create;
          png := TPngImage.Create;
          faceDecoder.DecodeBegin(facePngStrm);
          faceDecoder.Decode(s);
          facePngStrm.Seek(0, soBeginning);
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
        FreeAndNil(fXFace);
      end;
    end;

    if not face then
      for i := 0 to header.Count - 1 do
      begin
        raw := header[i];
        if RawCompareText(RawSplitString(':', raw), 'X-Face') = 0 then
        begin
          fXFace := TBitmap.Create;
          try
            xFace.Width := 48;
            xFace.Height := 48;
            xFace.PixelFormat := pf1Bit;
            if XFaceToBitmap(string(raw), fXFace) < 0 then
              FreeAndNil(fXFace);
          except
            FreeAndNil(fXFace);
          end;
          Break;
        end;
      end;
  end;
  Result := fXFace;
end;

function TmvMessage.IdentifyMessagePartBoundary(const st: string; hdr: TMimeHeader): TmvMessagePartClass;
var
  i: Integer;
  cls: TmvMessagePartClass;
begin
  Result := nil;
  for i := 0 to RegisteredMessagePartCount - 1 do
  begin
    cls := RegisteredMessageParts[i];
    if cls.IsBoundary(st, hdr) then
    begin
      Result := cls;
      Break;
    end;
  end;
end;

procedure TmvMessage.Lock;
begin
  fCS.Enter;
  RawData.Lock;
end;

procedure TmvMessage.PartialDecode;
var
  st: RawByteString;
  mp: TmvMessagePart;
  hdr: TMimeHeader;

  procedure AddMessagePartLine(const st: RawByteString);
  var
    prevMessagePart: TmvMessagePart;
    pmp: TmvMIMEMessagePart;
  begin
    case fCurrentMessagePart.AddLine(st) of
      alEndOfMP:                     // End of message part detected
        begin
          if Assigned(fMPStack) and (fMPStack.Count > 0) then
            fMPStack.Pop;
          fCurrentMessagePart.fComplete := True;
          fCurrentMessagePart := nil;
        end;

      alNextMP:
        begin
          prevMessagePart := fCurrentMessagePart;
          if prevMessagePart is TmvMIMEMessagePart then
          begin
            pmp := TmvMIMEMessagePart(prevMessagePart);
            hdr := pmp.MultipartHeader;
            if Assigned(pmp.MIMEHeader) then
              pmp.MimeHeader.FileName;
          end
          else
            hdr := MIMEHeader;
          fCurrentMessagePart.fComplete := True;
          fCurrentMessagePart := nil;
          mp := TryCreateMessagePart(string(st), hdr);
          if Assigned(mp) then
          begin
            fCurrentMessagePart := mp;
            if Assigned(Hdr) then
              fCurrentMessagePart.InitMultipart(Hdr)
            else
              if not fCurrentMessagePart.ProcessHeaderLine(st) then
                                        // If this is the first and only header
                                        // line for the message part, create the
                                        // data area straight away - eg. begin 644
                if not Assigned(fCurrentMessagePart.fData) then
                  fCurrentMessagePart.fData := TMemoryStream.Create;

            fCurrentMessagePart.fParentMessagePart := prevMessagePart.fParentMessagePart;
          end;
        end;

      alMultipart:
        begin
          if not Assigned(fMPStack) then
            fMPStack := TStack.Create;
          fMPStack.Push(TmvMimeMessagePart(fCurrentMessagePart).MimeHeader);
          fCurrentMessagePart := nil;
        end;
    end;
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
        fCurrentMessagePart := nil;
        FreeAndNil(fMIMEHeader);
        FreeAndNil(fMPStack);
        fCurrentLine := 0;
        if not fRawMode then
          fMIMEHeader := TMIMEHeader.CreateFromHeaderStrings(header, False);
      end;

      while GetLine(st) do                  // For each line...
      begin
        Inc(fCurrentLine);                  // Add it to current message part if we're in one
        if fCurrentMessagePart <> nil then
          AddMessagePartLine(st)
        else
        begin                               // Not currently in a message part
          if Assigned(MIMEHeader) and not MIMEHeader.IsMultipart then
          begin
            mp := TmvMimeMessagePart.Create(fMessageParts);
            mp.fOwner := Self;
            mp.fHasRawData := PartialMessage;
            TmvMimeMessagePart(mp).InitForcedMessagePart(MIMEHeader);
            fCurrentMessagePart := mp;
            fCurrentMessagePart.fFileName := MimeHeader.FileName;
            fCurrentMessagePart.fData := TMemoryStream.Create;
            AddMessagePartLine(st);
          end
          else
          begin
            if fRawMode then
              mp := nil
            else
              mp := TryCreateMessagePart(string(st), MIMEHeader);
                                              // Create a message part if we're at a
                                              // start boundary.

            if mp = nil then                  // Not at a start boundary
              if fMessageParts.Count > 0 then // If we've already had message parts, we need
                                              // to stick the line in a 'body continuation'
                                              // message part
              begin
                mp := fMessageParts[fMessageParts.Count - 1];
                if not (mp is TmvBodyContinuationMessagePart) then
                  mp := TmvBodyContinuationMessagePart.Create(fMessageParts);
                mp.AddLine(st);
              end
              else
                fBody.Add(st)                 // No message parts yet - just add the line
                                              // to the body.
            else
            begin                             // We created a new message part!
              mp.fHasRawData := PartialMessage;
              fCurrentMessagePart := mp;
              if Assigned(MimeHeader) and (MimeHeader.IsMultipart) then
                fCurrentMessagePart.InitMultipart(MIMEHeader)
              else
                if not fCurrentMessagePart.ProcessHeaderLine(st) then
                                              // If this is the first and only header
                                              // line for the message part, create the
                                              // data area straight away - eg. begin 644
                  if not Assigned(fCurrentMessagePart.fData) then
                    fCurrentMessagePart.fData := TMemoryStream.Create;
            end;
          end;
        end;
      end;
    except  // 'GetLine' may occasionally throw an AV if the currently loading message
            // is being displayed.
      fDecodePos := 0;
    end;
  finally
    Unlock;
  end;
end;

procedure TmvMessage.SetCodePage(const Value: Integer);
begin
  fCodePage := Value;
end;

procedure TmvMessage.SetDormant(const Value: Boolean);
var
  updating: Boolean;
begin
  if Value <> Dormant then
    if Value then
    begin
      fDecodeSize := 0;
      fDecodePos := 0;
      fBody.Clear;
      fMessageParts.Clear;
      fCurrentMessagePart := nil;
    end
    else
    begin
      updating := fUpdating;
      EndUpdate;
      fUpdating := updating;
    end;
end;

procedure TmvMessage.SetHeader(const Value: TAnsiStrings);
begin
  fHeader.Assign(Value);
end;

procedure TmvMessage.SetRawMode(const Value: Boolean);
begin
  if fRawMode <> Value then
  begin
    // In case the message was displayed raw, the codepage needs to be read again
    // when raw mode is switched off.
    if fRawMode then
      fCodePage := -1;
    fRawMode := Value;
    fDecodeSize := 0;
    fDecodePos := 0;
  end;
end;

function TmvMessage.TryCreateMessagePart(const st: string; hdr: TMimeHeader): TmvMessagePart;
var
  cls: TmvMessagePartClass;
begin
  // If the string is a start boundary string for a message part type
  // create a message part for the required type.  Otherwise return Nil   
  Result := nil;

  cls := IdentifyMessagePartBoundary(st, hdr);
  if Assigned(cls) then
  begin
    Result := cls.Create(fMessageParts);
    Result.fOwner := Self;
    Result.fStartLine := fCurrentLine;
  end;
end;

{ TmvMessagePart }

function TmvMessagePart.AddLine(const st: RawByteString): TAddLine;
var
  s: RawByteString;
  mp: TmvMIMEMessagePart;
  mpHeader: TMimeHeader;
begin
  if IsBoundaryEnd(string(st)) then
  begin
    Result := alEndOfMP;
    fComplete := True;
  end
  else
  begin
    Result := alOK;
    if Self is TmvMimeMessagePart then
    begin
      mp := TmvMimeMessagePart(Self);
      mpHeader := mp.MultipartHeader;
    end
    else
    begin
      mp := nil;
      mpHeader := nil;
    end;

    if Assigned(fData) then    // Is it in the message body ?
    begin
      if IsBoundary(string(st), mpHeader) then
      begin
        Result := alNextMP;
        Exit;
      end;

      if (Self is TmvMimeMessagePart) and not Assigned(mpHeader) then
        if owner.IdentifyMessagePartBoundary(string(st), nil) <> nil then
        begin
          Result := alNextMP;
          Exit;
        end;

      s := st + #13#10;
      fData.Write(s[1], Length(s));
    end
    else                        // We're still in the header
      if not ProcessHeaderLine(st) then
      begin
        if Assigned(mp) then
          mpHeader := mp.MimeHeader;

        if Assigned(mpHeader) and mpHeader.IsMultipart then
          Result := alMultipart
        else
          if not Assigned(fData) then
            fData := TMemoryStream.Create;
      end;
  end;
end;

constructor TmvMessagePart.Create(AOwner: TCollection);
begin
  inherited Create(AOwner);
  fInLine := True;
end;

procedure TmvMessagePart.DecodeGraphic(gc: TGraphicClass);
type
  bmf = record
    f: TBitmapFileHeader;
    i: TBitmapInfoHeader;
  end;
  pbmf = ^bmf;
var
  Buffer: Word;
  DecodedData: TMemoryStream;
  bmp: TBitmap;
  icon: HICON;
  ext: string;
  shfi: TSHFileInfo;
//  i: Integer;
//  x: DWORD;
  jpg: TJPegImage;
  p: pbmf;
  sz: DWORD;
begin
  if not fGotGraphic then
  begin
    if not Assigned(fGraphic) then
    begin
      if not fHasRawData then
        if not Assigned(gc) then
        begin
          ext := ExtractFileExt(FileName);
          gc := GetGraphicClass(ext);
        end;

      if Assigned(gc) and not fHasRawData then
      begin
        fGraphic := gc.Create;
        if fGraphic is TGifImage then
          TGifImage(fGraphic).Animate := True;
        fGraphic.OnProgress := Owner.DoOnProgress;
      end
      else
      begin
        // Unknown format/extension draw an icon to represent the contents.
        fHasRawData := True;
        bmp := TBitmap.Create;
        bmp.Width := 32;
        if bmp.Canvas.TextWidth(FileName) + 20 > bmp.Width then
          bmp.Width := bmp.Canvas.TextWidth(FileName) + 20;
        bmp.Height := 30 + 20 + 32;
        bmp.Canvas.TextRect(Rect(0, 0, bmp.Width, bmp.Height), 10, bmp.Height - 30, FileName);

        ZeroMemory(@shfi, SizeOf(shfi));
        SHGetFileInfo(PChar(FileName),
          FILE_ATTRIBUTE_NORMAL,
          shfi, sizeof(shfi),
          SHGFI_ICON or SHGFI_USEFILEATTRIBUTES);

        icon := shfi.hIcon;

        if icon <> 0 then
        begin
          DrawIcon(bmp.Canvas.Handle, (bmp.Canvas.TextWidth(FileName) + 20 - 32) div 2, 10, icon);
          DestroyIcon(icon);
        end;
        fGraphic := bmp;
      end;
    end;

    if Assigned(fGraphic) and not fHasRawData then
    begin
      DecodedData := TMemoryStream.Create;
      try
        GetData(DecodedData);

        DecodedData.Seek(0, soBeginning);

        // Some people posts JPEG images with preliminary data before the JPEG header
        // Try to salvage something, though you may end up with just a thumbnail.
        if fGraphic is TJPEGImage then
        begin
// TODO: check again, removed special handling of JPeg data
//          i := 0;
//          while i < DecodedData.Size do
//          begin
//            x := PDWORD(PByte(DecodedData.Memory) + i)^;
//            if x = $E0FFD8FF then
//            begin
//              DecodedData.Seek(i, soBeginning);
//              Break;
//            end
//            else
//              Inc(i);
//          end;

          jpg := TJPegImage(fGraphic);
          if not Complete then
            if jpg.ProgressiveEncoding then
              fOverridePercent := True;

          jpg.ProgressiveDisplay := jpg.ProgressiveEncoding and Complete;
          if Complete then
            jpg.Performance := jpBestQuality
          else
            jpg.Performance := jpBestSpeed;
        end;

        try
          // "Correct" the biSizeImage member of the TBitmapInfoHeader.
          if fGraphic is TBitmap then
          begin
            p := pbmf(DecodedData.Memory);
            sz := p^.i.biWidth * p^.i.biHeight * p^.i.biBitCount div 8;
            if sz < P^.i.biSizeImage then
              p^.i.biSizeImage := 0;
          end;
        except
        end;

        try
          fGraphic.LoadFromStream(DecodedData);
        except
          try
            // hmmm, it failed... maybe the poster used the wrong extension
            // for the graphic, try again based on some file header information...
            FreeAndNil(fGraphic);

            DecodedData.Seek(0, soBeginning);
            DecodedData.Read(Buffer, SizeOf(Buffer));
            DecodedData.Seek(0, soBeginning);

            case Buffer of
              $4947: gc := TGifImage;
              $4D42: gc := TBitmap;
              $5089: gc := TPngImage;
              $D8FF: gc := TJPegImage;
            else
              gc := nil;
            end;

            if Assigned(gc) then
            begin
              fGraphic := gc.Create;
              if fGraphic is TGifImage then
                TGifImage(fGraphic).Animate := True;
              fGraphic.OnProgress := Owner.DoOnProgress;
              fGraphic.LoadFromStream(DecodedData);
            end;
          except
          end;
        end;

      finally
        DecodedData.Free;
      end;
    end;
    fGotGraphic := Complete;
  end;
end;

destructor TmvMessagePart.Destroy;
begin
  fData.Free;
  fGraphic.Free;
  inherited Destroy;
end;

function TmvMessagePart.GetBody: TAnsiStrings;
begin
  Result := nil;
end;

procedure TmvMessagePart.GetData(s: TStream);
begin
  if Assigned(fData) then
    s.CopyFrom(fData, 0);
end;

function TmvMessagePart.GetDecodeType: TDecodeType;
begin
  Result := fDecodeType;
end;

function TmvMessagePart.GetFileName: string;
begin
  Result := StringReplace(fFileName, ':', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '\', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
end;

function TmvMessagePart.GetGraphic: TGraphic;
begin
  Result := Application.Icon;
end;

function TmvMessagePart.GetIsHTMLMultipart: Boolean;
begin
  Result := False;
end;

function TmvMessagePart.GetMIMEContentType: string;
begin
  Result := '';
end;

function TmvMessagePart.GetPercentDecoded: Integer;
var
  article: TArticle;
begin
  try
    if Complete or fOverridePercent then
      Result := 100
    else
    begin
      if Assigned(Owner) then
        article := TArticle(Owner.Obj)
      else
        article := nil;
      if Assigned(article) and (article.Lines > 0) then
      begin
        Result := ((Owner.fCurrentLine - fStartLine) * 100) div article.Lines - 15;
        if Result < 0 then
          Result := 0
      end
      else
        Result := 0;
    end;
  except
    Result := 0;
  end;
end;

procedure TmvMessagePart.InitMultipart(multipartHeader: TMIMEHeader);
begin
  // Stub
end;

class function TmvMessagePart.IsBoundary(const st: string; MIMEHeader: TMIMEHeader): Boolean;
begin
  Result := False;
end;

function TmvMessagePart.IsBoundaryEnd(const st: string): Boolean;
begin
  Result := False;
end;

function TmvMessagePart.MatchesCID(const cid: string): Boolean;
begin
  Result := False;
end;

function TmvMessagePart.ProcessHeaderLine(const st: RawByteString): Boolean;
begin
  Result := True;
end;

{ TmvMessageParts }

function TmvMessageParts.GetItems(idx: Integer): TmvMessagePart;
begin
  Result := TmvMessagePart(inherited Items[idx]);
end;


{ TThreadsafeMemoryStream }

constructor TThreadsafeMemoryStream.Create;
begin
  inherited Create;
  fCriticalSection := TCriticalSection.Create;
end;

destructor TThreadsafeMemoryStream.Destroy;
begin
  fCriticalSection.Free;
  inherited Destroy;
end;

function TThreadsafeMemoryStream.GetSize: Int64;
begin
  Lock;
  try
    Result := inherited GetSize;
  finally
    Unlock;
  end;
end;

procedure TThreadsafeMemoryStream.Lock;
begin
  fCriticalSection.Enter;
end;

procedure TThreadsafeMemoryStream.Unlock;
begin
  fCriticalSection.Leave;
end;

function TThreadsafeMemoryStream.Write(const Buffer; Count: Integer): Longint;
begin
  // Override the default 'write' method to lock/unlock.  Prevents
  // other threads from reading/writing while this thread is writing
  Lock;
  try
    Result := inherited Write(buffer, count);
  finally
    Unlock;
  end;
end;

{ TmvTextMessagePart }

constructor TmvTextMessagePart.Create(AOwner: TCollection);
begin
  inherited Create(AOwner);
  fBody := TAnsiStringList.Create;
end;

destructor TmvTextMessagePart.Destroy;
begin
  fBody.Free;
  inherited Destroy;
end;

function TmvTextMessagePart.GetBody: TAnsiStrings;
begin
  Result := fBody;
end;

function TmvTextMessagePart.ProcessHeaderLine(const st: RawByteString): Boolean;
begin
  // There's no 'header/data' concept in non-mime text message parts so
  // this is a special case.  Stay in the header, but add each line to
  // the body.
  if not Assigned(fBody) then
    fBody := TAnsiStringList.Create;
  fBody.Add(st);
  Result := True;
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

  if Assigned(Aheader.fContentTypeAttributes) then
  begin
    if not Assigned(fContentTypeAttributes) then
      fContentTypeAttributes := TStringList.Create;
    fContentTypeAttributes.Assign(AHeader.fContentTypeAttributes);
  end
  else
    FreeAndNil(fContentTypeAttributes);

  if Assigned(Aheader.fcontentDispositionAttributes) then
  begin
    if not Assigned(fcontentDispositionAttributes) then
      fcontentDispositionAttributes := TStringList.Create;
    fcontentDispositionAttributes.Assign(AHeader.fcontentDispositionAttributes);
  end
  else
    FreeAndNil(fcontentDispositionAttributes);
end;

class function TMimeHeader.CreateFromHeaderStrings(AHeader: TAnsiStrings; forceMIME: Boolean): TMIMEHeader;
var
  i: Integer;
  s1, st: string;
  mimeHeader: TMimeHeader;
  gotVersion: Boolean;
  gotContentType: Boolean;
  gotContentTransferEncoding: Boolean;

  procedure ParseMIMEVersion(var st: string);
  begin
    if SplitString(';', st) = '1.0' then
    begin
      gotVersion := True;
      if not Assigned(mimeHeader) then
        mimeHeader := TMimeHeader.Create;
    end;
  end;

  procedure ParseContentType(var st: string);
  var
    s1, attribName: string;
  begin
    gotContentType := True;
    if Pos('/', st) > 0 then
    begin
      mimeHeader.fContentTypeType := SplitString('/', st);
      mimeHeader.fContentTypeSubtype := SplitString(';', st);
    end
    else
      mimeHeader.fContentTypeType := SplitString(';', st);

    while st <> '' do
    begin
      s1 := SplitString(';', st);
      attribName := SplitString('=', s1);

      if s1 <> '' then
      begin
        if not Assigned(mimeHeader.fContentTypeAttributes) then
        begin
          mimeHeader.fContentTypeAttributes := TStringList.Create;
          mimeHeader.fContentTypeAttributes.CaseSensitive := False;
        end;
        mimeHeader.fContentTypeAttributes.Add(attribName + '=' + AnsiDequotedStr(s1, '"'));
      end;
    end;
  end;

  procedure ParseContentTransferEncoding(var st: string);
  begin
    gotContentTransferEncoding := True;
    mimeHeader.fContentTransferEncoding := SplitString(';', st);
  end;

  procedure ParseContentID(var st: string);
  begin
    mimeHeader.fContentID := SplitString(';', st);
  end;

  procedure ParseContentDescription(var st: string);
  begin
    mimeHeader.fContentDescription := SplitString(';', st);
  end;

  procedure ParseContentDisposition(var st: string);
  var
    s1, attribName: string;
  begin
    mimeHeader.fContentDisposition := SplitString(';', st);

    while st <> '' do
    begin
      s1 := SplitString(';', st);
      attribName := SplitString('=', s1);

      if s1 <> '' then
      begin
        if not Assigned(mimeHeader.fContentDispositionAttributes) then
        begin
          mimeHeader.fContentDispositionAttributes := TStringList.Create;
          mimeHeader.fContentDispositionAttributes.CaseSensitive := False;
        end;
        mimeHeader.fContentDispositionAttributes.Add(attribName + '=' + AnsiDequotedStr(s1, '"'));
      end;
    end;
  end;

  procedure ParseAdditionalMIMEHeader(var st, s1: string);
  begin
  end;

begin
  gotContentType := False;
  gotContentTransferEncoding := False;
  if forceMime then
  begin
    gotVersion := True;
    mimeHeader := TMimeHeader.Create;
  end
  else
  begin
    gotVersion := False;
    mimeHeader := nil;
  end;

  for i := 0 to AHeader.Count - 1 do
  begin
    st := string(AHeader[i]);

    s1 := SplitString(':', st);

    if not Assigned(mimeHeader) and (CompareMem(PChar(s1), PChar('Content-'), 8) or SameText(s1, 'X-RFC2646')) then
      if not Assigned(mimeHeader) then
        mimeHeader := TMimeHeader.Create;

    if SameText(s1, 'MIME-Version') then
      ParseMIMEVersion(st)
    else
      if SameText(s1, 'X-RFC2646') then
      begin
        mimeHeader.HandleXRFC2646(st);
        gotVersion := True;
      end
      else
        if Assigned(MIMEHeader) and (CompareText(SplitString('-', s1), 'Content') = 0) then
          if CompareText(s1, 'Type') = 0 then
            ParseContentType(st)
          else
            if CompareText(s1, 'Transfer-Encoding') = 0 then
              ParseContentTransferEncoding(st)
            else
              if CompareText(s1, 'ID') = 0 then
                ParseContentID(st)
              else
                if CompareText(s1, 'Description') = 0 then
                  ParseContentDescription(st)
                else
                  if CompareText(s1, 'Disposition') = 0 then
                    ParseContentDisposition(st)
                  else
                    ParseAdditionalMIMEHeader(s1, st);
  end;

  if Assigned(mimeHeader) then
  begin
    if mimeHeader.fContentTypeType = '' then
    begin
      mimeHeader.fContentTypeType := 'text';
      mimeHeader.fContentTypeSubType := 'plain';
    end;
  end;

  if gotVersion or gotContentType or gotContentTransferEncoding then
    Result := mimeHeader
  else
  begin
    mimeHeader.Free;
    Result := nil;
  end
end;

destructor TMimeHeader.Destroy;
begin
  fContentTypeAttributes.Free;
  fContentDispositionAttributes.Free;
  inherited Destroy;
end;

function TMimeHeader.GetDecodeType: TDecodeType;
begin
  Result := ttText;

  if CompareText(ContentTransferEncoding, 'Base64') = 0 then
    Result := ttBase64
  else
    if (CompareText(ContentTransferEncoding, 'UUENCODE') = 0) or (CompareText(ContentTransferEncoding, 'x-uuencode') = 0) then
      Result := ttUUEncode
    else
      if CompareText(ContentTransferEncoding, 'quoted-printable') = 0 then
        Result := ttQuotedPrintable;
end;

function TMimeHeader.GetFileName: string;
begin
  Result := '';
  if Assigned(fContentDispositionAttributes) then
    Result := ExtractFileName(fContentDispositionAttributes.Values['filename']);

  if Result = '' then
    if Assigned(fContentTypeAttributes) then
      Result := ExtractFileName(fContentTypeAttributes.Values['filename']);

  if Result = '' then
    if Assigned(fContentTypeAttributes) then
      Result := ExtractFileName(fContentTypeAttributes.Values['name']);

  if Result = '' then
    if CompareText(ContentType_Type, 'message') = 0 then
      Result := '*.eml';

  if Result = '' then
    if CompareText(ContentType_Type, 'audio') = 0 then
      Result := '*.wav';

  if Result = '' then
    if CompareText(ContentType_Type, 'image') = 0 then
      if ContentType_Subtype <> '' then
        Result := '*.' + ContentType_SubType;

  if (Result <> '') and (Result[1] = '*') then
    if ContentDescription <> '' then
      Result := ContentDescription + Copy(Result, 2, MaxInt)
    else
      Result := 'inline' + Copy(Result, 2, maxInt);

  if Result = '' then
    Result := ContentDescription;

  if Result = '' then
  begin
    Result := ContentType_Type;
    if ContentType_SubType <> '' then
      Result := Result + '/' + ContentType_SubType;
  end;
end;

function TMimeHeader.GetImageType: string;
begin
  if CompareText(ContentType_Type, 'image') = 0 then
    Result := ContentType_Subtype
  else
    Result := ''
end;

function TMimeHeader.GetIsMultipart: Boolean;
begin
  Result := CompareText(ContentType_Type, 'Multipart') = 0;
end;

function TMimeHeader.GetMultipartBoundary: string;
begin
  if Assigned(fContentTypeAttributes) then
    Result := fContentTypeAttributes.Values['boundary']
  else
    Result := '';
end;

procedure TmvMessage.Unlock;
begin
  RawData.Unlock;
  fCS.Leave;
end;

procedure TMimeHeader.HandleXRFC2646(const st: string);
var
  s, s1: string;
begin
  s := st;
  s1 := SplitString(';', s);
  s := SplitString('=', s1);

  if SameText(s, 'format') and (s1 <> '') then
  begin
    if not Assigned(fContentTypeAttributes) then
      fContentTypeAttributes := TStringList.Create;

    fContentTypeAttributes.Values['Format'] := s1;
  end;
end;

end.
