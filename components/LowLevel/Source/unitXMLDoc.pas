{*======================================================================*
 | unitXMLDoc
 |                                                                      |
 |
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
 | Copyright © Colin Wilson 2004  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      31/08/2005  CPWW  Original                                  |
 *======================================================================*}

unit unitXMLDoc;

interface

uses Windows, Classes, SysUtils, StrUtils, MSXML, SyncObjs;

const
  MAX_REQUEST_THREADS = 5;

var
  gRequestDelay : Integer = 0;
  gUserAgent : string = '';
  gCacheDirectory : string = '';

type
TInternetDocState = (psInit, psQueued, psLoadingFromNet, psParsing, psLoaded, psError, psDisabled);
TInternetDoc = class
private
  fOnStateChange: TNotifyEvent;
  fURL: string;
  fDocName: string;
  fState: TInternetDocState;
  fCookie : Integer;
  procedure DoOnStateChange;
  procedure SetState(const Value: TInternetDocState);
  procedure SetDocName(const Value: string);
  procedure SetURL(const Value: string);
public
  constructor Create (const ADocName, AURL : string; ACookie : Integer = 0); virtual;
  property DocName : string read fDocName write SetDocName;
  property State : TInternetDocState read fState write SetState;
  property URL : string read fURL write SetURL;
  property Cookie : Integer read fCookie;

  property OnStateChange : TNotifyEvent read fOnStateChange write fOnStateChange;
end;

TXMLInternetDoc = class (TInternetDoc)
private
  fLocked : boolean;
  procedure Lock;
  procedure Unlock;
  function GetCachedDate: TDateTime;

protected
  fDirty : boolean;

  procedure Parse (elem : IXMLDomElement); virtual; abstract;
  procedure BeforeSave (doc : IXMLDomDocument); virtual;
  procedure AfterLoad (doc : IXMLDomDocument); virtual;
public
  property Locked : boolean read fLocked;
  procedure Update;
  procedure Load;
  procedure Save (doc : IXMLDomDocument); overload;
  procedure Save; overload;
  property Dirty : boolean read fDirty;
  property CachedDate : TDateTime read GetCachedDate;
end;

function GetFileNameFromURL (const url : string) : string;
function GetLock : TCriticalSection;
function GetIntAttr (elem : IXMLDOMElement; const attr : string; default : Integer = 0) : Integer;
function GetStrAttr (elem : IXMLDOMElement; const attr : string; default : String = '') : String;


implementation

uses ActiveX, ConTnrs, Variants;

type
TXMLDocumentGetters = class;
TRequestThread = class (TThread)
private
  fDoc : TXMLInternetDoc;
  fOwner : TXMLDocumentGetters;
protected
  procedure Execute; override;
public
  constructor Create (AOwner : TXMLDocumentGetters; ADoc : TXMLInternetDoc);
  destructor Destroy; override;
end;

TXMLDocumentGetters = class
private
  fQueue : TObjectList;
  fSync : TCriticalSection;
  fRequestDelaySync : TCriticalSection;
  fRequestThreads : TObjectList;
  fLastRequestDelay : Integer;

public
  constructor Create;
  destructor Destroy; override;
  procedure GetXMLDocument (doc : TXMLInternetDoc);
  procedure RequestThreadDone (requestThread : TRequestThread);
  procedure BeginRequestDelay;
  procedure EndRequestDelay;
end;

var
  gXMLDocumentGetters : TXMLDocumentGetters = Nil;
  gLock : TCriticalSection = Nil;

{*----------------------------------------------------------------------*
 | function GetLock : TCriticalSection                                  |
 |                                                                      |
 | Get the global singleton critical section that controls ensures that |
 | the actual HTTP requests are emitted one at a time                   |
 |                                                                      |
 | Note that this is public, so that the HTTP requests can be paused    |
 | and restarted by the calling app.                                    |
 *----------------------------------------------------------------------*}
function GetLock : TCriticalSection;
begin
  if not Assigned (gLock) then
    gLock := TCriticalSection.Create;
  result := gLock
end;

{*----------------------------------------------------------------------*
 | function XMLDocumentGetters : TXMLDocumentGetters                    |
 |                                                                      |
 | Return the global singleton XMLDocumentGetters object that handles   |
 | all the getter requests                                              |
 *----------------------------------------------------------------------*}
function XMLDocumentGetters : TXMLDocumentGetters;
begin
  if not Assigned (gXMLDocumentGetters) then
    gXMLDocumentGetters := TXMLDocumentGetters.Create;
  result := gXMLDocumentGetters;
end;

{*----------------------------------------------------------------------*
 | function GetFileNameFromURL : string                                 |
 |                                                                      |
 | Public helper function creates a file name based on a URL - for      |
 | caching etc.                                                         |
 *----------------------------------------------------------------------*}
function GetFileNameFromURL (const url : string) : string;
begin
  result := LowerCase (url);

  result := StringReplace (result, 'http://', '', [rfReplaceAll]);
  result := StringReplace (result, '/', '_', [rfReplaceAll]);
  result := StringReplace (result, '\', '_', [rfReplaceAll]);
  result := StringReplace (result, ':', '_', [rfReplaceAll]);
  result := StringReplace (result, '?', '_', [rfReplaceAll]);
  result := StringReplace (result, '*', '_', [rfReplaceAll]);
  result := StringReplace (result, '=', '_', [rfReplaceAll]);
end;

{*----------------------------------------------------------------------*
 | function GetIntAttr : Integer                                        |
 |                                                                      |
 | Get an integer attribute from an XML element, or return a default    |
 | value if the attribute doesn't exist for the element.                |
 *----------------------------------------------------------------------*}
function GetIntAttr (elem : IXMLDOMElement; const attr : string; default : Integer = 0) : Integer;
var
  v : OleVariant;
begin
  v := elem.getAttribute(attr);
  if not VarIsNull (v) then
    result := v
  else
    result := default
end;

{*----------------------------------------------------------------------*
 | function GetStrAttr : String                                         |
 |                                                                      |
 | Get a string attribute from an XML element, or return a default      |
 | value if the attribute doesn't exist for the element.                |
 *----------------------------------------------------------------------*}
function GetStrAttr (elem : IXMLDOMElement; const attr : string; default : String = '') : String;
var
  v : OleVariant;
begin
  v := elem.getAttribute(attr);
  if not VarIsNull (v) then
    result := v
  else
    result := default
end;

{ TInternetDoc }

constructor TInternetDoc.Create(const ADocName, AURL: string; ACookie : Integer);
begin
  fDocName := ADocName;
  fURL := AURL;
  fCookie := ACookie;
end;

procedure TInternetDoc.SetState(const Value: TInternetDocState);
begin
  fState := Value;
end;

procedure TInternetDoc.DoOnStateChange;
begin
  if Assigned (fOnStateChange) then
    fOnStateChange (self)
end;

procedure TInternetDoc.SetURL(const Value: string);
begin
  fURL := Value;
end;

procedure TInternetDoc.SetDocName(const Value: string);
begin
  fDocName := Value;
end;

{ TXMLInternetDoc }

procedure TXMLInternetDoc.Load;
var
  fileName : string;
  path : string;
  sl : TStringList;
  doc : IXMLDOMDocument;
begin
  fDirty := False;
  if URL = '' then Exit;
  fileName := GetFileNameFromURL (URL);

  path := gCacheDirectory + 'Cache\';

  ForceDirectories (path);


  if FileExists (path + fileName) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(path + fileName);
      doc := CoDOMDocument.Create;
      try
        doc.validateOnParse := False;
        doc.resolveExternals := False;
        doc.loadXML(sl.Text);
        Lock;
        try
          fState := psParsing;
          try
            Parse (doc.documentElement);
            fState := psLoaded;
          except
            fState := psError
          end
        finally
          Unlock;
        end;
        AfterLoad (doc);
      finally
        doc := Nil
      end;
    finally
      sl.Free
    end
  end;
  fDirty := False;
end;

procedure TXMLInternetDoc.Save;
var
  doc : IXMLDOMDocument;
  sl : TStringList;
  fileName, path : string;
begin
  if not Dirty then Exit;
  if URL = '' then Exit;
  fileName := GetFileNameFromURL (URL);

  path := gCacheDirectory + 'Cache\';

  ForceDirectories (path);

  if FileExists (path + fileName) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(path + fileName);
      doc := CoDOMDocument.Create;
      doc.validateOnParse := false;
      doc.resolveExternals := False;
      doc.loadXML(sl.Text);
      Save (doc)
    finally
      sl.Free
    end
  end
end;

procedure TXMLInternetDoc.Save(doc: IXMLDomDocument);
var
  fileName : string;
  path : string;
  sl : TStringList;
begin
  if not Dirty then Exit;
  if URL = '' then Exit;
  fileName := GetFileNameFromURL (URL);

  path := gCacheDirectory + 'Cache\';

  ForceDirectories (path);

  sl := TStringList.Create;
  try
    BeforeSave (doc);
    sl.Text := doc.XML;
    sl.SaveToFile(path + fileName)
  finally
    sl.Free
  end;
  fDirty := False;
end;

procedure TXMLInternetDoc.Lock;
begin
  GetLock.Enter;
  try
    fLocked := True;
  finally
    GetLock.Leave
  end
end;

procedure TXMLInternetDoc.Unlock;
begin
  GetLock.Enter;
  try
    fLocked := False;
  finally
    GetLock.Leave
  end
end;

procedure TXMLInternetDoc.BeforeSave(doc: IXMLDomDocument);
begin
// Stub
end;

procedure TXMLInternetDoc.AfterLoad(doc: IXMLDomDocument);
begin
// Stub
end;

procedure TXMLInternetDoc.Update;
begin
  if self.URL <> '' then
    XMLDocumentGetters.GetXMLDocument(self);
end;

function TXMLInternetDoc.GetCachedDate: TDateTime;
var
  fileName, path : string;
  f : TFileStream;
  dt : Integer;
begin
  result := -1;
  fileName := GetFileNameFromURL (URL);

  path := gCacheDirectory + 'Cache\';

  ForceDirectories (path);

  if FileExists (path + fileName) then
  begin
    f := TFileStream.Create(path+fileName, fmOpenRead or fmShareDenyWrite);
    try
      dt := FileGetDate (f.Handle);
      if dt <> -1 then
        result := FileDateToDateTime (dt)
    finally
    end
  end
end;

{ TRequestThread }

constructor TRequestThread.Create(AOwner : TXMLDocumentGetters; ADoc: TXMLInternetDoc);
begin
  fOwner := AOwner;
  inherited Create (true);
  FreeOnTerminate := False;
  fDoc := ADoc;
  Resume
end;

destructor TRequestThread.Destroy;
begin
  inherited;
end;

procedure TRequestThread.Execute;
var
  XMLHTTPRequest : IXMLHTTPRequest;
  needsUninit : boolean;
  doc, doc1 : IXMLDomDocument;
  t : string;
  ok :boolean;
begin
  needsUninit := Succeeded (CoInitializeEx (Nil, 0));
  try
    try
      XMLHTTPRequest := CoXMLHTTPRequest.Create;

      fDoc.fState := psLoadingFromNet;
      Synchronize (fDoc.DoOnStateChange);
      fOwner.BeginRequestDelay;
      try
        XMLHTTPRequest.open('GET', fDoc.URL, false, '', '');
        if gUserAgent <> '' then
          XMLHTTPRequest.setRequestHeader('User-Agent', gUserAgent);

        XMLHTTPRequest.send('');
        doc1 := Nil;
        if not Supports (XMLHTTPRequest.responseXML, IXMLDOMDocument, doc) then
        begin
          doc := CoDomDocument.Create;
          try
            doc.validateOnParse := False;
            doc.resolveExternals := False;
            doc.load(XMLHTTPRequest.ResponseXML);

            if doc.documentElement = Nil then
            begin

            end;
            
          except
            doc := Nil
          end
        end;
      finally
        fOwner.EndRequestDelay
      end;

      ok := False;
      if Assigned (doc) then
      try
        ok := True;
        fDoc.Lock;
        try
          fDoc.fState := psParsing;
          Synchronize (fDoc.DoOnStateChange);

          try
            if doc.documentElement = Nil then
            begin
              doc1 := CoDomDocument.Create;
              t := XMLHTTPRequest.responseText;
              doc1.resolveExternals := False;
              doc1.validateOnParse := False;
              doc1.LoadXML (t);
              fDoc.Parse (doc1.documentElement)
            end
            else
              fDoc.Parse (doc.documentElement);
          except
            ok := False
          end
        finally
          fDoc.Unlock;
        end;

        fDoc.fDirty := True;
        if doc1 <> nil then
          fDoc.Save(doc1)
        else
          fDoc.Save (doc)
      finally
        doc := Nil;
        doc1 := Nil;
      end;
      if ok then
        fDoc.fState := psLoaded
      else
        fDoc.State := psError;
      Synchronize (fDoc.DoOnStateChange);
    except
      fDoc.fState := psError;
      try
        Synchronize (fDoc.DoOnStateChange);
      except
      end
    end
  finally
    if needsUninit then
      CoUnInitialize;

    XMLDocumentGetters.RequestThreadDone(self);
    FreeOnTerminate := True
  end
end;

{ TXMLDocumentGetters }

constructor TXMLDocumentGetters.Create;
begin
  fQueue := TObjectList.Create;
  fQueue.OwnsObjects := False;
  fRequestThreads := TObjectList.Create;
  fRequestThreads.OwnsObjects := True;
  fRequestThreads.Capacity := MAX_REQUEST_THREADS;
  fSync := TCriticalSection.Create;
end;

procedure TXMLDocumentGetters.RequestThreadDone(requestThread: TRequestThread);
var
  obj : TObject;
begin
  fSync.Enter;
  try
    fRequestThreads.Extract(requestThread);

    while (fQueue.Count > 0) and (fRequestThreads.Count < MAX_REQUEST_THREADS) do
    begin
      obj := fQueue [0];
      fQueue.Delete(0);
      fRequestThreads.Add(TRequestThread.Create (self, TXMLInternetDoc (obj)))
    end
  finally
    fSync.Leave
  end
end;

destructor TXMLDocumentGetters.Destroy;
begin
  fSync.Enter;
  try
    fQueue.Free;
    fRequestThreads.Free;
  finally
    fSync.Leave
  end;
  fSync.Free;
  fRequestDelaySync.Free;

  inherited;
end;

procedure TXMLDocumentGetters.GetXMLDocument(doc: TXMLInternetDoc);
begin
  fSync.Enter;
  try
    doc.fState := psQueued;
    if fRequestThreads.Count >= MAX_REQUEST_THREADS then
      fQueue.Add(doc)
    else
      fRequestThreads.Add(TRequestThread.Create (self, doc))
  finally
    fSync.Leave
  end
end;

procedure TXMLDocumentGetters.BeginRequestDelay;
var
  ms : Integer;
begin
  if gRequestDelay = 0 then Exit;
  if not Assigned (fRequestDelaySync) then
    fRequestDelaySync := TCriticalSection.Create;
  fRequestDelaySync.Enter;

  if fLastRequestDelay <> 0 then
  begin
    ms := GetTickCount - DWORD (fLastRequestDelay);
    if (ms >= 0) and (ms < gRequestDelay) then
    begin
      ms := gRequestDelay - ms;
      Sleep (ms);
    end
  end
end;

procedure TXMLDocumentGetters.EndRequestDelay;
begin
  if gRequestDelay = 0 then Exit;

  fLastRequestDelay := GetTickCount;
  fRequestDelaySync.Leave;
end;

initialization
  gCacheDirectory := ExtractFilePath (ParamStr (0));
finalization
  gXMLDocumentGetters.Free;
  gLock.Free
end.
