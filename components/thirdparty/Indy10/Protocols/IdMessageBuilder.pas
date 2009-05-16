unit IdMessageBuilder;

interface

{$i IdCompilerDefines.inc}

uses
  Classes, IdMessage;

type
  TIdMessageBuilderAttachment = class(TCollectionItem)
  private
    FContentID: String;
    FFileName: String;
  public
    procedure Assign(Source: TPersistent); override;
    property ContentID: String read FContentID write FContentID;
    property FileName: String read FFileName write FFileName;
  end;

  TIdMessageBuilderAttachments = class(TCollection)
  private
    function GetAttachment(Index: Integer): TIdMessageBuilderAttachment;
    procedure SetAttachment(Index: Integer; Value: TIdMessageBuilderAttachment);
  public
    constructor Create; reintroduce;
    function Add(const AFileName: String; const AContentID: String = ''): TIdMessageBuilderAttachment; reintroduce;
    property Attachment[Index: Integer]: TIdMessageBuilderAttachment
      read GetAttachment write SetAttachment; default;
  end;

  TIdCustomMessageBuilder = class
  protected
    FAttachments: TStrings;
    FPlainText: TStrings;
    FPlainTextCharSet: String;
    procedure AddAttachments(AMsg: TIdMessage);
    procedure InternalFill(AMsg: TIdMessage); virtual; abstract;
    procedure SetPlainText(AValue: TStrings);
    procedure SetAttachments(AValue: TStrings);
    procedure SetContentTypeAndCharSet(AMsg: TIdMessage); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //
    procedure Clear; virtual;
    procedure FillMessage(AMsg: TIdMessage);
    function NewMessage(AOwner: TComponent = nil): TIdMessage;
    //
    property Attachments: TStrings read FAttachments write SetAttachments;
    property PlainText: TStrings read FPlainText write SetPlainText;
    property PlainTextCharSet: String read FPlainTextCharSet write FPlainTextCharSet;
  end;

  TIdMessageBuilderPlain = class(TIdCustomMessageBuilder)
  protected
    procedure InternalFill(AMsg: TIdMessage); override;
    procedure SetContentTypeAndCharSet(AMsg: TIdMessage); override;
  end;

  TIdMessageBuilderHtml = class(TIdCustomMessageBuilder)
  protected
    FHtml: TStrings;
    FHtmlCharSet: String;
    FHtmlFiles: TIdMessageBuilderAttachments;
    FHtmlViewerNeededMsg: String;
    procedure InternalFill(AMsg: TIdMessage); override;
    procedure SetContentTypeAndCharSet(AMsg: TIdMessage); override;
    procedure SetHtml(AValue: TStrings);
    procedure SetHtmlFiles(AValue: TIdMessageBuilderAttachments);
  public
    constructor Create; override;
    destructor Destroy; override;
    //
    procedure Clear; override;
    //
    property Html: TStrings read FHtml write SetHtml;
    property HtmlCharSet: String read FHtmlCharSet write FHtmlCharSet;
    property HtmlFiles: TIdMessageBuilderAttachments read FHtmlFiles write SetHtmlFiles;
    property HtmlViewerNeededMsg: String read FHtmlViewerNeededMsg write FHtmlViewerNeededMsg; 
  end;

  TIdMessageBuilderRtfType = (idMsgBldrRtfMS, idMsgBldrRtfEnriched, idMsgBldrRtfRichtext);

  TIdMessageBuilderRtf = class(TIdCustomMessageBuilder)
  protected
    FRtf: TStrings;
    FRtfCharSet: String;
    FRtfType: TIdMessageBuilderRtfType;
    FRtfViewerNeededMsg: String;
    procedure InternalFill(AMsg: TIdMessage); override;
    procedure SetContentTypeAndCharSet(AMsg: TIdMessage); override;
    procedure SetRtf(AValue: TStrings);
  public
    constructor Create; override;
    destructor Destroy; override;
    //
    procedure Clear; override;
    //
    property Rtf: TStrings read FRtf write SetRtf;
    property RtfCharSet: String read FRtfCharSet write FRtfCharSet;
    property RtfType: TIdMessageBuilderRtfType read FRtfType write FRtfType;
    property RtfViewerNeededMsg: String read FRtfViewerNeededMsg write FRtfViewerNeededMsg;
  end;

implementation

uses
  IdGlobal, IdGlobalProtocols, IdAttachmentFile, IdText, SysUtils;

const
  cTextPlain = 'text/plain'; {do not localize}
  cTextHtml = 'text/html'; {do not localize}
  cTextRtf: array[TIdMessageBuilderRtfType] of String = ('text/rtf', 'text/enriched', 'text/richtext'); {do not localize}
  cMultipartAlternative = 'multipart/alternative'; {do not localize}
  cMultipartMixed = 'multipart/mixed'; {do not localize}
  cMultipartRelatedHtml = 'multipart/related; type="text/html"'; {do not localize}

resourcestring
  rsHtmlViewerNeeded = 'An HTML viewer is required to see this message';
  rsRtfViewerNeeded = 'An RTF viewer is required to see this message';

{ TIdMessageBuilderAttachment }

procedure TIdMessageBuilderAttachment.Assign(Source: TPersistent);
begin
  if Source is TIdMessageBuilderAttachment then
  begin
    with TIdMessageBuilderAttachment(Source) do
    begin
      Self.FContentID := FContentID;
      Self.FFileName := FFileName;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

{ TIdMessageBuilderAttachments }

constructor TIdMessageBuilderAttachments.Create;
begin
  inherited Create(TIdMessageBuilderAttachment);
end;

function TIdMessageBuilderAttachments.Add(const AFileName: String; const AContentID: String = ''): TIdMessageBuilderAttachment;
begin
  Result := TIdMessageBuilderAttachment(inherited Add);
  Result.FContentID := AContentID;
  Result.FFileName := AFileName;
end;

function TIdMessageBuilderAttachments.GetAttachment(Index: Integer): TIdMessageBuilderAttachment;
begin
  Result := TIdMessageBuilderAttachment(inherited GetItem(Index));
end;

procedure TIdMessageBuilderAttachments.SetAttachment(Index: Integer; Value: TIdMessageBuilderAttachment);
begin
  inherited SetItem(Index, Value);
end;

{ TIdCustomMessageBuilder }

constructor TIdCustomMessageBuilder.Create;
begin
  inherited Create;
  FPlainText := TStringList.Create;
  FAttachments := TStringList.Create;
end;

destructor TIdCustomMessageBuilder.Destroy;
begin
  FPlainText.Free;
  FAttachments.Free;
  inherited Destroy;
end;

procedure TIdCustomMessageBuilder.AddAttachments(AMsg: TIdMessage);
var
  I: Integer;
begin
  for I := 0 to FAttachments.Count-1 do
  begin
    with TIdAttachmentFile.Create(AMsg.MessageParts, FAttachments[I]) do begin
      ContentType := GetMIMETypeFromFile(FileName);
    end;
  end;
end;

procedure TIdCustomMessageBuilder.Clear;
begin
  FAttachments.Clear;
  FPlainText.Clear;
  FPlainTextCharSet := '';
end;

procedure TIdCustomMessageBuilder.FillMessage(AMsg: TIdMessage);
begin
  if not Assigned(AMsg) then begin
    Exit;
  end;

  // Clear only the body, ContentType, and CharSet here...
  //
  AMsg.ClearBody;
  AMsg.ContentType := '';
  AMsg.CharSet := '';

  // let the message decide how to encode itself
  // based on what parts are added in InternalFill()
  //
  AMsg.Encoding := meDefault;

  // fill in type-specific content first
  //
  InternalFill(AMsg);

  // Are non-related attachments present?
  //
  AddAttachments(AMsg);

  // Determine the top-level ContentType for the message now
  //
  SetContentTypeAndCharSet(AMsg);
end;

function TIdCustomMessageBuilder.NewMessage(AOwner: TComponent = nil): TIdMessage;
begin
  Result := TIdMessage.Create(AOwner);
  try
    FillMessage(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TIdCustomMessageBuilder.SetAttachments(AValue: TStrings);
begin
  FAttachments.Assign(AValue);
end;

procedure TIdCustomMessageBuilder.SetContentTypeAndCharSet(AMsg: TIdMessage);
begin
  if FAttachments.Count > 0 then
  begin
    if AMsg.MessageParts.Count > 1 then
    begin
      // plain text and/or formatting, and at least 1 non-related attachment
      //
      AMsg.ContentType := cMultipartMixed;
      AMsg.CharSet := '';
    end else
    begin
      // no plain text or formatting, only 1 non-related attachment
      //
      with AMsg.MessageParts[0] do
      begin
        AMsg.ContentType := ContentType;
        AMsg.CharSet := CharSet;
      end;
    end;
  end else
  begin
    AMsg.ContentType := '';
    AMsg.CharSet := '';
  end;
end;

procedure TIdCustomMessageBuilder.SetPlainText(AValue: TStrings);
begin
  FPlainText.Assign(AValue);
end;

{ TIdMessageBuilderPlain }

procedure TIdMessageBuilderPlain.InternalFill(AMsg: TIdMessage);
begin
  // Is plain text present?
  //
  if FPlainText.Count > 0 then
  begin
    // Should the message contain only plain text?
    //
    if FAttachments.Count = 0 then
    begin
      AMsg.Body.Assign(FPlainText);
    end else
    begin
      // At this point, multiple pieces will be present in the message
      // body, so everything must be stored in the MessageParts collection...
      //
      with TIdText.Create(AMsg.MessageParts, FPlainText) do
      begin
        ContentType := cTextPlain;
        CharSet := FPlainTextCharSet;
      end;
    end;
  end;
end;

procedure TIdMessageBuilderPlain.SetContentTypeAndCharSet(AMsg: TIdMessage);
begin
  if (FPlainText.Count > 0) and (FAttachments.Count = 0) then
  begin
    // plain text only
    //
    AMsg.ContentType := cTextPlain;
    AMsg.CharSet := FPlainTextCharSet;
  end else
  begin
    inherited SetContentTypeAndCharSet(AMsg);
  end;
end;

{ TIdMessageBuilderHtml }

constructor TIdMessageBuilderHtml.Create;
begin
  inherited Create;
  FHtml := TStringList.Create;
  FHtmlFiles := TIdMessageBuilderAttachments.Create;
  FHtmlViewerNeededMsg := rsHtmlViewerNeeded;
end;

destructor TIdMessageBuilderHtml.Destroy;
begin
  FHtml.Free;
  FHtmlFiles.Free;
  inherited Destroy;
end;

procedure TIdMessageBuilderHtml.Clear;
begin
  FHtml.Clear;
  FHtmlCharSet := '';
  FHtmlFiles.Clear;
  inherited Clear;
end;

procedure TIdMessageBuilderHtml.InternalFill(AMsg: TIdMessage);
var
  LUsePlain, LUseHtml, LUseHtmlFiles, LUseAttachments: Boolean;
  I, LAlternativeIndex, LRelatedIndex: Integer;
  LAttachment: TIdMessageBuilderAttachment;

  function FormatContentId(Item: TIdMessageBuilderAttachment): String;
  begin
    if Item.FContentID <> '' then begin
      Result := EnsureMsgIDBrackets(Item.FContentID);
    end
    else if Item.FFileName <> '' then begin
      Result := EnsureMsgIDBrackets(ExtractFileName(Item.FFileName));
    end
    else begin
      Result := '';
    end;
  end;

begin
  // Cache these for better performance
  //
  LUsePlain := FPlainText.Count > 0;
  LUseHtml := FHtml.Count > 0;
  LUseHtmlFiles := LUseHtml and (FHtmlFiles.Count > 0);
  LUseAttachments := FAttachments.Count > 0;

  LAlternativeIndex := -1;
  LRelatedIndex := -1;

  // Is any body data present at all?
  //
  if not (LUsePlain or LUseHtml or LUseHtmlFiles or LUseAttachments) then begin
    Exit;
  end;

  // Should the message contain only plain text?
  //
  if LUsePlain and not (LUseHtml or LUseAttachments) then
  begin
    AMsg.Body.Assign(FPlainText);
    Exit;
  end;

  // Should the message contain only HTML?
  //
  if LUseHtml and not (LUsePlain or LUseHtmlFiles or LUseAttachments) then
  begin
    AMsg.Body.Assign(FHtml);
    Exit;
  end;

  // At this point, multiple pieces will be present in the message
  // body, so everything must be stored in the MessageParts collection...

  // If the message should contain both plain text and HTML, a
  // "multipart/alternative" piece is needed to wrap them if
  // non-related attachments are also present...
  //
  if LUseHtml and LUseAttachments then
  begin
    with TIdText.Create(AMsg.MessageParts, nil) do
    begin
      ContentType := cMultipartAlternative;
      LAlternativeIndex := Index;
    end;
  end;

  // Is plain text present?
  //
  if LUsePlain or LUseHtml then
  begin
    with TIdText.Create(AMsg.MessageParts, FPlainText) do
    begin
      if LUseHtml and (not LUsePlain) then
      begin
        Body.Text := FHtmlViewerNeededMsg;
      end;
      ContentType := cTextPlain;
      CharSet := FPlainTextCharSet;
      ParentPart := LAlternativeIndex;
    end;
  end;

  // Is HTML present?
  //
  if LUseHtml then
  begin
    // related attachments can't be referenced by, or used inside
    // of, plain text, so there is no point in wrapping the plain
    // text inside the same "multipart/related" part with the HTML
    // and attachments.  Some email programs don't do that as well.
    // This logic is newer and more accurate than what is described
    // in the "HTML Messages" article found on Indy's website.
    //
    if LUseHtmlFiles then
    begin
      with TIdText.Create(AMsg.MessageParts, nil) do
      begin
        ContentType := cMultipartRelatedHtml;
        ParentPart := LAlternativeIndex;
        LRelatedIndex := Index;
      end;
    end;

    // Add HTML
    //
    with TIdText.Create(AMsg.MessageParts, FHtml) do
    begin
      ContentType := cTextHtml;
      CharSet := FHtmlCharSet;
      if LRelatedIndex <> -1 then begin
        ParentPart := LRelatedIndex; // plain text and related attachments
      end else begin
        ParentPart := LAlternativeIndex; // plain text and optional non-related attachments
      end;
    end;

    // Are related attachments present?
    //
    if LUseHtmlFiles then
    begin
      for I := 0 to FHtmlFiles.Count-1 do
      begin
        LAttachment := FHtmlFiles[I];
        with TIdAttachmentFile.Create(AMsg.MessageParts, LAttachment.FileName) do
        begin
          ContentId := FormatContentId(LAttachment);
          ContentType := GetMIMETypeFromFile(FileName);
          if TextStartsWith(ContentType, 'image/') then begin {do not localize}
            ContentDisposition := 'inline'; {do not localize}
          end;
          ParentPart := LRelatedIndex;
        end;
      end;
    end;
  end;
end;

procedure TIdMessageBuilderHtml.SetContentTypeAndCharSet(AMsg: TIdMessage);
begin
  if FAttachments.Count = 0 then
  begin
    if (FPlainText.Count > 0) and (FHtml.Count = 0) then
    begin
      // plain text only
      //
      AMsg.ContentType := cTextPlain;
      AMsg.CharSet := FPlainTextCharSet;
    end
    else if FHtml.Count > 0 then
    begin
      if (FPlainText.Count = 0) and (FHtmlFiles.Count = 0) then
      begin
        // HTML only
        //
        AMsg.ContentType := cTextHtml;
        AMsg.CharSet := FHtmlCharSet;
      end else
      begin
        // plain text and HTML and no related attachments
        //
        AMsg.ContentType := cMultipartAlternative;
        AMsg.CharSet := '';
      end;
    end;
  end else
  begin
    inherited SetContentTypeAndCharSet(AMsg);
  end;
end;

procedure TIdMessageBuilderHtml.SetHtml(AValue: TStrings);
begin
  FHtml.Assign(AValue);
end;

procedure TIdMessageBuilderHtml.SetHtmlFiles(AValue: TIdMessageBuilderAttachments);
begin
  FHtmlFiles.Assign(AValue);
end;

{ TIdMessageBuilderRTF }

constructor TIdMessageBuilderRtf.Create;
begin
  inherited Create;
  FRtf := TStringList.Create;
  FRtfType := idMsgBldrRtfMS;
  FRtfViewerNeededMsg := rsRtfViewerNeeded;
end;

destructor TIdMessageBuilderRtf.Destroy;
begin
  FRtf.Free;
  inherited Destroy;
end;

procedure TIdMessageBuilderRtf.Clear;
begin
  FRtf.Clear;
  inherited Clear;
end;

procedure TIdMessageBuilderRtf.InternalFill(AMsg: TIdMessage);
var
  LUsePlain, LUseRtf, LUseAttachments: Boolean;
  LAlternativeIndex: Integer;
begin
  // Cache these for better performance
  //
  LUsePlain := FPlainText.Count > 0;
  LUseRtf := FRtf.Count > 0;
  LUseAttachments := FAttachments.Count > 0;
  LAlternativeIndex := -1;

  // Is any body data present at all?
  //
  if not (LUsePlain or LUseRtf or LUseAttachments) then begin
    Exit;
  end;

  // Should the message contain only plain text?
  //
  if LUsePlain and not (LUseRtf or LUseAttachments) then
  begin
    AMsg.Body.Assign(FPlainText);
    Exit;
  end;

  // Should the message contain only RTF?
  //
  if LUseRtf and not (LUsePlain or LUseAttachments) then
  begin
    AMsg.Body.Assign(FRtf);
    Exit;
  end;

  // At this point, multiple pieces will be present in the message
  // body, so everything must be stored in the MessageParts collection...

  // If the message should contain both plain text and RTF, a
  // "multipart/alternative" piece is needed to wrap them if
  // attachments are also present...
  //
  if LUseRtf and LUseAttachments then
  begin
    with TIdText.Create(AMsg.MessageParts, nil) do
    begin
      ContentType := cMultipartAlternative;
      LAlternativeIndex := Index;
    end;
  end;

  // Is plain text present?
  //
  if LUsePlain or LUseRtf then
  begin
    with TIdText.Create(AMsg.MessageParts, FPlainText) do
    begin
      if LUseRtf and (not LUsePlain) then
      begin
        Body.Text := FRtfViewerNeededMsg;
      end;
      ContentType := cTextPlain;
      ParentPart := LAlternativeIndex;
    end;
  end;

  // Is RTF present?
  //
  if LUseRtf then
  begin
    // Add RTF
    //
    with TIdText.Create(AMsg.MessageParts, FRtf) do
    begin
      ContentType := cTextRtf[FRtfType];
      ParentPart := LAlternativeIndex; // plain text and optional non-related attachments
    end;
  end;
end;

procedure TIdMessageBuilderRtf.SetContentTypeAndCharSet(AMsg: TIdMessage);
begin
  if FAttachments.Count = 0 then
  begin
    if (FPlainText.Count > 0) and (FRtf.Count = 0) then
    begin
      // plain text only
      //
      AMsg.ContentType := cTextPlain;
      AMsg.CharSet := FPlainTextCharSet;
    end
    else if (FRtf.Count > 0) and (FPlainText.Count = 0) then
    begin
      // RTF only
      //
      AMsg.ContentType := cTextRtf[FRtfType];
      AMsg.CharSet := FRtfCharSet;
    end else
    begin
      // plain text and RTF and no non-related attachments
      //
      AMsg.ContentType := cMultipartAlternative;
      AMsg.CharSet := '';
    end;
  end else
  begin
    inherited SetContentTypeAndCharSet(AMsg);
  end;
end;

procedure TIdMessageBuilderRtf.SetRtf(AValue: TStrings);
begin
  FRtf.Assign(AValue);
end;

end.
