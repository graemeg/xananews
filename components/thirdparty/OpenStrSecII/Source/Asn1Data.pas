{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     ASN1Data Unit                                     }
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
unit Asn1Data;

interface

uses
  SysUtils, Classes, Asn1,
  {$IFDEF MSWINDOWS}
  Controls, StdCtrls, ComCtrls
  {$ENDIF}
  {$IFDEF LINUX}              
  QTypes, QControls, QStdCtrls, QComCtrls
  {$ENDIF};

type
  TASNObject = class;
  TASNFieldPath = string;

  TASNDataLink = class
  private
    FData: TASNObject;
    FFieldPath: TASNFieldPath;
    FControl: TControl;
    FNoDisplay: Boolean;
    FDisplayDisabled: Boolean;
    FAutoEnable: Boolean;
    FBrowsing: Boolean;
    procedure SetData(const Value: TASNObject);
    procedure SetControl(const Value: TControl);
    procedure SetFieldPath(const Value: TASNFieldPath);
    procedure SetNoDisplay(const Value: Boolean);
    procedure SetAutoEnable(const Value: Boolean);
  protected
    procedure ClearControl;
    procedure DataEvent(Field: TASN1Struct; Changes: TChangeTypes); virtual;
    procedure DisplayContent(Field: TASN1Struct); virtual;
    procedure DisplayNodes(Field: TASN1Struct); virtual;
    procedure InternalSelectNode(Field: TASN1Struct); virtual;
    procedure InternalUpdateControl(Field: TASN1Struct); virtual;
    procedure SetContentFromControl;
    property Control: TControl read FControl write SetControl;
    property Data: TASNObject read FData write SetData;
    property FieldPath: TASNFieldPath read FFieldPath write SetFieldPath;
  public
    constructor Create(AOwner: TASNObject);
    destructor Destroy; override;
    procedure UpdateControl;
    function FieldIsDeclared: Boolean;
    property AutoEnable: Boolean read FAutoEnable write SetAutoEnable;
    property NoDisplay: Boolean read FNoDisplay write SetNoDisplay;
  end;

  TASNObject = class(TPersistent)
  private
    FOwner: TPersistent;
    FData: TASN1Struct;
    FFileName: TFileName;
    FIsAlias: Boolean;
    FLinks: TList;
    FAutoEnable: Boolean;
    FOnDataChanged: TASN1ChangeEvent;
    procedure ReadASNModule(Reader: TReader);
    procedure ReadDERData(Stream: TStream);
    procedure SetData(const Value: TASN1Struct);     
    procedure WriteASNModule(Writer: TWriter);
    procedure WriteDERData(Stream: TStream);
    procedure SetAutoEnable(const Value: Boolean);
    procedure SetOnDataChanged(const Value: TASN1ChangeEvent);
  protected
    procedure AddLink(Link: TASNDataLink);
    procedure AssignTo(Dest: TPersistent); override;
    procedure DataChanged(Sender: TObject; Field: TASN1Struct; Changes: TChangeTypes);
    procedure DefineProperties(Filer: TFiler); override;
    procedure FreeNotifyLinks;
    function  GetOwner: TPersistent; override;
    procedure RemoveLink(Link: TASNDataLink);
    procedure SetDataAsAlias(Value: TASN1Struct);
    property FileName: TFileName read FFileName;
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    procedure Change;
    procedure LoadFromFile(AFileName: TFileName);
    procedure SaveToFile(AFileName: TFileName);
    property AutoEnable: Boolean read FAutoEnable write SetAutoEnable;
    property Data: TASN1Struct read FData write SetData;
    property OnDataChanged: TASN1ChangeEvent read FOnDataChanged write SetOnDataChanged;
  end;

  TActionType = (atContent,
                 atEdit,atInsert,atCancel,atPost,atDelete,
                 atFirst,atPrior,atNext,atLast);

  TASNDataControlItem = class(TCollectionItem)
  private
    FLink: TASNDataLink;
    FFieldPath: TASNFieldPath;
    FControl: TControl;
    FAutoEnable: Boolean;
    FAutoEdit: Boolean;
    FOnControlChange: TNotifyEvent;
    FOnControlExit: TNotifyEvent;
    FOnControlClick: TNotifyEvent;
    FOnControlEnter: TNotifyEvent;
    FActionType: TActionType;
    FOnTreeViewChange: TTVChangedEvent;
    function GetBrowsing: Boolean;
    procedure SetControl(const Value: TControl);
    procedure SetFieldPath(const Value: TASNFieldPath);
    procedure SetAutoEdit(const Value: Boolean);
    procedure SetAutoEnable(const Value: Boolean);
    procedure SetOnControlChange(const Value: TNotifyEvent);
    procedure SetOnControlClick(const Value: TNotifyEvent);
    procedure SetOnControlEnter(const Value: TNotifyEvent);
    procedure SetOnControlExit(const Value: TNotifyEvent);
    procedure SetActionType(const Value: TActionType);
    procedure SetOnTreeViewChange(const Value: TTVChangedEvent);
    function GetCaption: TCaption;
    procedure SetCaption(const Value: TCaption);
    procedure SetBrowsing(Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CreateDataLink; virtual;
    procedure DoControlChange(Sender: TObject);
    procedure DoControlClick(Sender: TObject);
    procedure DoControlEnter(Sender: TObject);
    procedure DoControlExit(Sender: TObject);
    procedure DoTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure Enable;
    function GetData: TASNObject;
    function GetDisplayName: string; override;
    procedure RestoreControlEvents;
    procedure SetupControlEvents;
    property Browsing: Boolean read GetBrowsing write SetBrowsing;
  public
    destructor Destroy; override;
  published
    property ActionType: TActionType read FActionType write SetActionType;
    property AutoEdit: Boolean read FAutoEdit write SetAutoEdit;
    property AutoEnable: Boolean read FAutoEnable write SetAutoEnable;
    property Caption: TCaption read GetCaption write SetCaption;
    property Control: TControl read FControl write SetControl;
    property FieldPath: TASNFieldPath read FFieldPath write SetFieldPath;
    property OnControlChange: TNotifyEvent read FOnControlChange write SetOnControlChange;
    property OnControlClick: TNotifyEvent read FOnControlClick write SetOnControlClick;
    property OnControlEnter: TNotifyEvent read FOnControlEnter write SetOnControlEnter;
    property OnControlExit: TNotifyEvent read FOnControlExit write SetOnControlExit;
    property OnTreeViewChange: TTVChangedEvent read FOnTreeViewChange write SetOnTreeViewChange;
  end;

  TASNDataControls = class(TOwnedCollection)
  private
    FData: TASNObject;
  protected
    procedure DisableControls;
    procedure DisableDisplay;
    procedure EnableControls;
    procedure FixupEvents; dynamic;
    procedure RemoveControl(AControl: TControl);
    procedure SetData(const Value: TASNObject); virtual;
  public
    property Data: TASNObject read FData write SetData;
  end;

  TASNCustomData = class(TComponent)
  private
    FActive: Boolean;
    FData: TASNObject;
    FFileName: TFileName;
    FASNDataField: TASNFieldPath;
    FASNData: TASNCustomData;
    FControls: TASNDataControls;
    procedure SetActive(const Value: Boolean);
    procedure SetData(const Value: TASNObject);
    procedure SetFileName(const Value: TFileName);
    procedure SetASNData(const Value: TASNCustomData);
    procedure SetASNDataField(const Value: TASNFieldPath);
    procedure SetControls(const Value: TASNDataControls);
    procedure SetOnDataChanged(const Value: TASN1ChangeEvent);
    function GetOnDataChanged: TASN1ChangeEvent;
  protected
    procedure CreateControls; virtual;
    procedure DisableControls;
    procedure DisableDisplay;
    procedure EnableControls;
    procedure InternalClose; virtual;
    procedure InternalOpen; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function StoreData: Boolean;
    property Active: Boolean read FActive write SetActive;
    property Controls: TASNDataControls read FControls write SetControls;
    property ASNData: TASNCustomData read FASNData write SetASNData;
    property ASNDataField: TASNFieldPath read FASNDataField write SetASNDataField;
    property OnDataChanged: TASN1ChangeEvent read GetOnDataChanged write SetOnDataChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Data: TASNObject read FData write SetData stored StoreData;
    property FileName: TFileName read FFileName write SetFileName;
  end;

  TASNData = class(TASNCustomData)
  published
    property Active;
    property ASNData;
    property ASNDataField;
    property Controls;
    property Data;
    property FileName;
    property OnDataChanged;
  end;

  TCustomChoiceField = class;
  TCustomChoice = class;

  TASNCustomComponent = class(TComponent)
  private
    FChoiceFields: TList;
    FRegistredChoiceProperties: TStringList;
    FData: TASN1Struct;
    function GetNoChange: Boolean;
    function GetOnDataChanged: TASN1ChangeEvent;
    procedure ReadDERData(Stream: TStream);
    procedure SetNoChange(const Value: Boolean);
    procedure SetOnDataChanged(const Value: TASN1ChangeEvent);
    procedure WriteDERData(Stream: TStream);
  protected
    procedure ActivateChoiceField(Field: TCustomChoiceField);
    procedure AddChoiceField(Field: TCustomChoiceField);
    procedure CreateData; virtual; abstract;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DestroyChoiceFields;
    function GetField(const Path: string): PASN1Struct;
    procedure RegisterChoiceProperty(const Path: string; AField: TCustomChoice);
    procedure RemoveChoiceField(Field: TCustomChoiceField);
    procedure Update; virtual;
    property NoChange: Boolean read GetNoChange write SetNoChange;
    property OnDataChanged: TASN1ChangeEvent read GetOnDataChanged write SetOnDataChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: TFileName);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(AFileName: TFileName);
    procedure SaveToStream(AStream: TStream);
  end;

  TChoiceFieldClass = class of TCustomChoiceField;
  TChoiceTypeName = string;
  
  TCustomChoice = class(TPersistent)
  private
    FOwner: TASNCustomComponent;
    FPath: string;
    FField: TCustomChoiceField;
    FFields: TStringList;
    procedure SetField(const Value: TCustomChoiceField);
    procedure SetChoiceTypeName(const Value: TChoiceTypeName);
    function GetChoiceTypeName: TChoiceTypeName;
  protected
    function CreateField(const TypeName: string): TCustomChoiceField; virtual; abstract;
    procedure CreateFields;
    function GetChoiceCount: Integer;
    { design time support }
    function GetChoiceTypeNames(Index: Integer): string;
  protected
    property Owner: TASNCustomComponent read FOwner;
    property Path: string read FPath;
  public
    constructor Create(AOwner: TASNCustomComponent;
                       const Path: string);
  published
    property ChoiceTypeName: TChoiceTypeName read GetChoiceTypeName write SetChoiceTypeName stored False;
    property Field: TCustomChoiceField read FField write SetField stored False;
  end;

  TCustomChoiceField = class(TPersistent)
  private
    FOwner: TASNCustomComponent;
    FTag: Cardinal;
    FCls: Byte;
    FPath: string;
    FTypeName: string;
    FConstructed: Boolean;
    FIdentifiedBy: string;
    FIDField: string;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
  protected
    function GetField(const Path: string): PASN1Struct;
    property Active: Boolean read GetActive write SetActive;
  public
    constructor Create(AOwner: TASNCustomComponent;
                       const Path, TypeName: string;
                       ACls: Byte;
                       ATag: Cardinal;
                       AConstructed: Boolean); virtual;
    constructor CreateTypeIdentified(AOwner: TASNCustomComponent;
                                     const Path, TypeName, IdentifiedBy, IDField: string); virtual;
    destructor Destroy; override;
  end;

  TCustomOFFieldItem = class;

  TCustomOFFields = class(TOwnedCollection)
  private
    FPath: string;
    FComponent: TASNCustomComponent;
  protected
    procedure AddedItem(Item: TCustomOFFieldItem);
    procedure RemovedItem(Item: TCustomOFFieldItem);
    function GetField(const Path: string): PASN1Struct;    
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent;
                       AComponent: TASNCustomComponent;
                       ItemClass: TCollectionItemClass;
                       const APath: string);
  end;

  TCustomOFFieldItem = class(TCollectionItem)
  protected
    function GetField(const Path: string): PASN1Struct;
    function GetMyPath: string;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Dialogs
  {$ENDIF}
  {$IFDEF LINUX}
  QDialogs
  {$ENDIF};

type
  THack = class(TASN1Struct);

{ TASNData }

constructor TASNCustomData.Create(AOwner: TComponent);
begin
  inherited;
  FData := TASNObject.Create(Self);
  CreateControls;
end;

procedure TASNCustomData.CreateControls;
begin
  FControls := TASNDataControls.Create(Self,TASNDataControlItem);
  FControls.Data := FData;
end;

destructor TASNCustomData.Destroy;
begin
  inherited;
  FData.Free;
  FControls.Free;
end;

procedure TASNCustomData.DisableControls;
begin
  Controls.DisableControls;
end;

procedure TASNCustomData.DisableDisplay;
begin
  Controls.DisableDisplay;
end;

procedure TASNCustomData.EnableControls;
begin
  Controls.EnableControls;
end;

function TASNCustomData.GetOnDataChanged: TASN1ChangeEvent;
begin
  Result := FData.OnDataChanged;
end;

procedure TASNCustomData.InternalClose;
begin
  if FActive and (FFileName <> '') then
    FData.SaveToFile(FFileName);
  FData.FFileName := '';
  FData.Data := nil;
end;

procedure TASNCustomData.InternalOpen;
begin
  if FileExists(FFileName) then begin
    try
      FData.FFileName := FFileName;
      FData.LoadFromFile(FFileName);
    except
      FData.FFileName := '';
      FFileName := '';
      FActive := False;
      raise;
    end;
  end;
end;

procedure TASNCustomData.Loaded;
begin
  inherited;
  FControls.FixupEvents;
  if FActive then begin
    InternalOpen;
    EnableControls;
  end;
end;

procedure TASNCustomData.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if (AComponent = FASNData) then begin
      FASNData := nil;
      FData.SetDataAsAlias(nil);
    end else if AComponent is TControl then begin
      FControls.RemoveControl(TControl(AComponent));
    end;
  end;
end;

procedure TASNCustomData.SetActive(const Value: Boolean);
begin
  if Value then begin
    FActive := Value;
    if not (csLoading in ComponentState) then begin
      DisableDisplay;
      InternalOpen;
      EnableControls;
    end;
  end else begin
    DisableDisplay;
    DisableControls;
    InternalClose;
    FActive := Value;
  end;
end;

procedure TASNCustomData.SetASNData(const Value: TASNCustomData);
begin
  if FASNData <> Value then
    FData.SetDataAsAlias(nil);
  FASNData := Value;
end;

procedure TASNCustomData.SetASNDataField(const Value: TASNFieldPath);
var
  F: PASN1Struct;
begin
  if FASNData = nil then
    FASNDataField := ''
  else
    FASNDataField := Value;
  if FASNDataField = '' then
    FData.SetDataAsAlias(nil)
  else if FASNDataField = '(root)' then
    FData.SetDataAsAlias(FASNData.Data.Data)
  else begin
    F := FASNData.Data.Data.FindField(Value);
    if F = nil then
      FData.SetDataAsAlias(nil)
    else
      FData.SetDataAsAlias(F^);
  end;
end;

procedure TASNCustomData.SetControls(const Value: TASNDataControls);
begin
  FControls.Assign(Value);
end;

procedure TASNCustomData.SetData(const Value: TASNObject);
begin
  FData.Assign(Value);
end;

procedure TASNCustomData.SetFileName(const Value: TFileName);
begin
  SetActive(False);
  FFileName := Value;
  FData.FFileName := Value;
end;

procedure TASNCustomData.SetOnDataChanged(const Value: TASN1ChangeEvent);
begin
  FData.FOnDataChanged := Value;
end;

function TASNCustomData.StoreData: Boolean;
begin
  Result := FActive;
end;

{ TASNObject }

procedure TASNObject.AddLink(Link: TASNDataLink);
begin
  FLinks.Add(Link);
end;

procedure TASNObject.AssignTo(Dest: TPersistent);
var
  D: TASNObject;
begin
  if Dest is TASNObject then begin
    D := TASNObject(Dest);
    if Assigned(FData) then begin
      if (D.Data = nil) then
        D.Data := FData
      else if (D.Data.Owner <> nil) or
              (FData.Owner <> nil) or
              (THack(D.Data).RefCount > 1) or
              FIsAlias then
        D.Data := FData
      else
        D.Data.Assign(FData);
      D.FIsAlias := D.Data = FData;
    end else
      D.Data := nil;
  end else
    inherited;
end;

procedure TASNObject.Change;
begin
  SetData(FData);
end;

constructor TASNObject.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  FLinks := TList.Create;
end;

procedure TASNObject.DataChanged(Sender: TObject; Field: TASN1Struct;
  Changes: TChangeTypes);
var
  I: Integer;
begin
  for I := 0 to FLinks.Count - 1 do
    TASNDataLink(FLinks[I]).DataEvent(Field,Changes);
  if Assigned(FOnDataChanged) then
    FOnDataChanged(Self,Field,Changes);
end;

procedure TASNObject.DefineProperties(Filer: TFiler);

  function IsStored: Boolean;
  begin
    Result := Assigned(FData) and not FIsAlias;
  end;

  function DataIsStored: Boolean;
  begin
    Result := IsStored;
    if Result then
      Result := not FData.IsEmpty;
  end;

begin
  inherited;
  Filer.DefineProperty('ASNModule',ReadASNModule,WriteASNModule,IsStored);
  Filer.DefineBinaryProperty('DERData',ReadDERData,WriteDERData,DataIsStored);
end;

destructor TASNObject.Destroy;
begin
  if Assigned(FData) then
    THack(FData)._Release;
  FreeNotifyLinks;
  FLinks.Free;
  inherited;
end;

procedure TASNObject.FreeNotifyLinks;
var
  I: Integer;
begin
  for I := 0 to FLinks.Count - 1 do
    TASNDataLink(FLinks[I]).Data := nil;
end;

function TASNObject.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TASNObject.LoadFromFile(AFileName: TFileName);
var
  FS: TFileStream;
  Ext: string;
begin
  SetData(TASN1Struct.Create);
  FS := TFileStream.Create(AFileName,fmOpenRead);
  try
    Ext := LowerCase(ExtractFileExt(AFileName));
    if Ext = '.dom' then
      Data.LoadFromStream(FS,fmtDOM)
    else if Ext = '.asn' then
      Data.LoadFromStream(FS,fmtASN1)
    else
      Data.LoadFromStream(FS,fmtDER);
  finally
    FS.Free;
  end;
end;

procedure TASNObject.ReadASNModule(Reader: TReader);
var
  SL: TStringList;
  SS: TStringStream;
  D: TASN1Struct;
begin
  SL := TStringList.Create;
  try
    Reader.ReadListBegin;
    SL.BeginUpdate;
    try
      SL.Clear;
      while not Reader.EndOfList do
        SL.Add(Reader.ReadString);
    finally
      SL.EndUpdate;
    end;
    Reader.ReadListEnd;

    try
      D := TASN1Struct.Create;
      D._AddRef;
      SS := TStringStream.Create(SL.Text);
      try
        try
          D.LoadFromStream(SS,fmtASN1);
        except
          raise Exception.Create('Failure loading ASN.1 Module');
        end;
      finally
        SS.Free;
      end;
      SetData(D);
      D._Release;
    except
      on E: Exception do
        ShowMessage(E.Message);
    end;
  finally
    SL.Free;
  end;
end;

procedure TASNObject.ReadDERData(Stream: TStream);
begin
  try
    Data.LoadFromStream(Stream,fmtDER);
  except
    Data.DisposeContent;
  end;
end;

procedure TASNObject.RemoveLink(Link: TASNDataLink);
begin
  FLinks.Remove(Link);
end;

procedure TASNObject.SaveToFile(AFileName: TFileName);
var
  FS: TFileStream;
  Ext: string;
begin
  if (FData = nil) or (FData.IsEmpty) then Exit;
  FS := TFileStream.Create(AFileName,fmCreate);
  try
    Ext := LowerCase(ExtractFileExt(AFileName));
    if Ext = '.dom' then
      Data.SaveToStream(FS,fmtDOM)
    else if Ext = '.asn' then
      Data.SaveToStream(FS,fmtASN1)
    else
      Data.SaveToStream(FS,fmtDER);
  finally
    FS.Free;
  end;
end;

procedure TASNObject.SetAutoEnable(const Value: Boolean);
begin
  FAutoEnable := Value;
end;

procedure TASNObject.SetData(const Value: TASN1Struct);
begin
  if FData <> Value then begin
    FIsAlias := False;
    if Assigned(FData) then begin
      FData.OnChange := nil;
      THack(FData)._Release;
    end;
    FData := Value;
    if Assigned(FData) then begin
      FData.OnChange := DataChanged;
      THack(FData)._AddRef;
    end;
    DataChanged(Self,FData,[ctDecl]);
  end;
  if Assigned(FData) and (FFileName <> '') then
    SaveToFile(FFileName);
end;

procedure TASNObject.SetDataAsAlias(Value: TASN1Struct);
begin
  if Assigned(Value) or FIsAlias then
    SetData(Value);
  FIsAlias := Assigned(Value);
end;

procedure TASNObject.SetOnDataChanged(const Value: TASN1ChangeEvent);
begin
  FOnDataChanged := Value;
end;

procedure TASNObject.WriteASNModule(Writer: TWriter);
var
  SL: TStringList;
  SS: TStringStream;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    SS := TStringStream.Create('');
    try
      FData.SaveToStream(SS,fmtASN1);
      SL.Text := SS.DataString;
    finally
      SS.Free;
    end;

    Writer.WriteListBegin;
    for I := 0 to SL.Count - 1 do
      Writer.WriteString(SL[I]);
    Writer.WriteListEnd;
  finally
    SL.Free;
  end;
end;

procedure TASNObject.WriteDERData(Stream: TStream);
begin
  Data.CalculateLength;
  Data.SaveToStream(Stream,fmtDER);
end;

{ TASNDataLink }
      
type
  THackEdit = class(TCustomEdit);
  THackTreeView = class(TCustomTreeView);

procedure TASNDataLink.ClearControl;
begin
  if not FNoDisplay then begin
    if FControl is TCustomEdit then
      THackEdit(FControl).Text := ''
    else if FControl is TCustomTreeView then
      THackTreeView(FControl).Items.Clear;
  end;
end;

constructor TASNDataLink.Create(AOwner: TASNObject);
begin
  Data := AOwner;
  FNoDisplay := True;
end;

procedure TASNDataLink.DataEvent(Field: TASN1Struct;
  Changes: TChangeTypes);
var
  P: string;
begin                          
  if Assigned(Field) then begin
    if ctDecl in Changes then
      if FData.Data.FindField(FFieldPath) = nil then
        Exit;
    P := Field.GetNamePath(True);
    if ctBrowse in Changes then begin
      if FControl is TCustomTreeView then
        InternalSelectNode(Field)
      else if not NoDisplay then
        UpdateControl;
    end;
    if (FFieldPath <> '') and
       (((FFieldPath = '(root)') and (Field.Owner = nil)) or
        (StrLComp(PChar(P),PChar(FFieldPath),Length(FFieldPath)) = 0)) then
      UpdateControl;
  end;
end;

destructor TASNDataLink.Destroy;
begin
  Data := nil;
  inherited;
end;

type
  THackControl = class(TControl);
  THackMemo = class(TCustomMemo);

procedure TASNDataLink.DisplayContent(Field: TASN1Struct);
var
  vTag: Cardinal;
begin
  if NoDisplay then Exit;
  if Field = nil then Exit;
  FBrowsing := True;
  try
    if Field.Implicit then
      vTag := Field.ImplicitTag
    else
      vTag := Field.Tag;
    if FControl is TCustomTreeView then
      DisplayNodes(Field)
    else if Field.Constructed and (FControl is TCustomMemo) then
      RenderAsText(Field,THackMemo(FControl).Lines,True,True,True)
    else if Field.Constructed or (vTag = V_ASN1_BIT_STRING) then
      THackControl(FControl).Text := Field.ContentAsOctetString
    else
      THackControl(FControl).Text := Field.DisplayContent;
  finally
    FBrowsing := False;
  end;
end;

procedure TASNDataLink.DisplayNodes(Field: TASN1Struct);
var
  Node: TTreeNode;
  TV: THackTreeView;

  procedure CreateChildNodes(Node: TTreeNode);
  var
    F, E: TASN1Struct;
    I: Integer;
    SubNode: TTreeNode;
  begin
    F := Node.Data;
    if F.Constructed then begin
      for I := 0 to F.ItemCount - 1 do begin
        E := F.Items[I]^;
        SubNode := Node.Owner.AddChildObject(Node,E.VarName,E);
        CreateChildNodes(SubNode);
      end;
    end;
  end;

begin
  TV := THackTreeView(FControl);
  TV.Items.Clear;
  if Field.VarName = '' then
    Node := TV.Items.AddObject(nil,'(root)',Field)
  else
    Node := TV.Items.AddObject(nil,Field.VarName,Field);
  CreateChildNodes(Node);
  Node.Selected := True;
end;

function TASNDataLink.FieldIsDeclared: Boolean;
var
  F: PASN1Struct;
begin
  F := FData.Data.FindField(FFieldPath);
  if Assigned(F) then
    Result := F^.Tag <> Cardinal(V_ASN1_UNDEF)
  else
    Result := False;
end;

procedure TASNDataLink.InternalSelectNode(Field: TASN1Struct);
var
  TV: THackTreeView;
  Node: TTreeNode;
  BP: TASN1Struct;
begin
  if FBrowsing then
    Exit;
  TV := THackTreeView(FControl);
  BP := Field.FindBrowseableParent;
  while (Field.Owner <> nil) and (Field.Owner <> BP) do
    Field := Field.Owner;
  Node := TV.Selected;
  if (Node <> nil) and
     ((Node.Data = Field) or (Field.IsParent(Node.Data))) then
    Exit;
  Node := TV.TopItem;
  while Assigned(Node) do begin
    if Node.Data = Field then
      Break
    else if Field.IsParent(Node.Data) then
      Node := Node.getFirstChild
    else
      Node := Node.getNextSibling;
  end;
  if Assigned(Node) then begin
    TV.Items.BeginUpdate;
    try
      Node.MakeVisible;
      Node.Selected := True;
    finally
      TV.Items.EndUpdate;
    end;
  end;
end;

procedure TASNDataLink.InternalUpdateControl(Field: TASN1Struct);
begin
  if Assigned(FControl) then begin
    if Assigned(Field) then begin
      if AutoEnable then
        FControl.Enabled := True;
      DisplayContent(Field)
    end else begin
      FControl.Enabled := False;
      if not FNoDisplay then
        THackControl(FControl).Text := '';
    end;
  end;
end;

procedure TASNDataLink.SetAutoEnable(const Value: Boolean);
begin
  FAutoEnable := Value;
  if Value then
    UpdateControl
  else
    InternalUpdateControl(nil);
end;

procedure TASNDataLink.SetContentFromControl;
var
  F: PASN1Struct;
begin
  if Assigned(FControl) then begin
    F := FData.Data.FindField(FFieldPath);
    if Assigned(F) then
      if not F.Constructed then
        F.EditContent(THackControl(FControl).Text);
  end;
end;

procedure TASNDataLink.SetControl(const Value: TControl);
begin
  FControl := Value;
  if Assigned(FControl) and (FControl is TCustomMemo) then
    FControl.Enabled := False;
  UpdateControl;
end;

procedure TASNDataLink.SetData(const Value: TASNObject);
begin
  if Assigned(FData) then
    FData.RemoveLink(Self);
  if Assigned(Value) then
    Value.AddLink(Self);
  FData := Value;
  UpdateControl;
end;

procedure TASNDataLink.SetFieldPath(const Value: TASNFieldPath);
begin
  FFieldPath := Value;
  UpdateControl;
end;

procedure TASNDataLink.SetNoDisplay(const Value: Boolean);
begin
  FNoDisplay := Value;
  if not Value then
    UpdateControl;
end;

procedure TASNDataLink.UpdateControl;
var
  F: PASN1Struct;
begin
  if FDisplayDisabled then
    Exit;
  if (FData = nil) or (FData.Data = nil) then
    InternalUpdateControl(nil)
  else if FFieldPath = '(root)' then begin
    InternalUpdateControl(FData.Data);
  end else begin
    F := FData.Data.FindField(FFieldPath);
    if Assigned(F) then
      InternalUpdateControl(F^)
    else
      InternalUpdateControl(nil);
  end;
end;

{ TASNDataControlItem }

procedure TASNDataControlItem.CreateDataLink;
begin
  FLink := TASNDataLink.Create(GetData);
  FLink.FieldPath := FFieldPath;
  FLink.AutoEnable := FAutoEnable;
  FLink.Control := FControl;
end;

destructor TASNDataControlItem.Destroy;
begin
  SetControl(nil);
  FLink.Free;
  inherited;
end;

procedure TASNDataControlItem.DoControlChange(Sender: TObject);
begin
  if (ActionType = atContent) and not Browsing then
    FLink.SetContentFromControl;
  if Assigned(FOnControlChange) then
    FOnControlChange(Sender);
end;

procedure TASNDataControlItem.DoControlClick(Sender: TObject);
begin
  case ActionType of
    atEdit:   THack(FLink.FData.FData).EditItem;
    atInsert: THack(FLink.FData.FData).InsertItem;
    atCancel: THack(FLink.FData.FData).CancelItem;
    atDelete: THack(FLink.FData.FData).DeleteItem;
    atPost:   THack(FLink.FData.FData).PostItem;
    atFirst:  THack(FLink.FData.FData).BrowseFirst;
    atPrior:  THack(FLink.FData.FData).BrowsePrior;
    atNext:   THack(FLink.FData.FData).BrowseNext;
    atLast:   THack(FLink.FData.FData).BrowseLast;
  end;
  if Assigned(FOnControlClick) then
    FOnControlClick(Sender);
end;

procedure TASNDataControlItem.DoControlEnter(Sender: TObject);
begin
  if AutoEdit then
    THack(FLink.Data).EditItem;
  if Assigned(FOnControlEnter) then
    FOnControlEnter(Sender);
end;

procedure TASNDataControlItem.DoControlExit(Sender: TObject);
begin
  if Assigned(FOnControlExit) then
    FOnControlExit(Sender);
end;   

procedure TASNDataControlItem.DoTreeViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  if not Browsing then begin
    Browsing := True;
    try
      if Assigned(Node) and Node.Selected and Assigned(Node.Data) then
        TASN1Struct(Node.Data).ParentBrowseHere;
    finally
      Browsing := False;
    end;
  end;
  if Assigned(FOnTreeViewChange) then
    FOnTreeViewChange(Sender,Node);
end;

function TASNDataControlItem.GetData: TASNObject;
begin
  Result := nil;
  if Collection <> nil then
    if Collection is TASNDataControls then
      Result := TASNDataControls(Collection).Data;
end;

function TASNDataControlItem.GetDisplayName: string;
begin
  if Assigned(FControl) then begin
    Result := FControl.Name;
    if Result = '' then
      Result := FControl.ClassName;
  end else
    Result := inherited GetDisplayName;
end;

procedure TASNDataControlItem.RestoreControlEvents;
begin
  THackControl(FControl).OnClick := FOnControlClick;
  FOnControlClick := nil;
  if FControl is TCustomEdit then begin
    THackEdit(FControl).OnChange := FOnControlChange;
    THackEdit(FControl).OnEnter := FOnControlEnter;
    THackEdit(FControl).OnExit := FOnControlExit;
    FOnControlChange := nil;
    FOnControlEnter := nil;
    FOnControlExit := nil;
  end;
end;

procedure TASNDataControlItem.SetAutoEdit(const Value: Boolean);
begin
  if ActionType = atContent then
    FAutoEdit := Value;
end;

procedure TASNDataControlItem.SetAutoEnable(const Value: Boolean);
begin
  FAutoEnable := Value;
  if Assigned(FLink) then
    FLink.AutoEnable := Value;
end;

procedure TASNDataControlItem.SetControl(const Value: TControl);
begin
  if FControl = Value then Exit;
  if Assigned(FControl) then
    RestoreControlEvents;
  FControl := Value;
  if Assigned(Value) then begin
    (TASNDataControls(Collection).GetOwner as TComponent).FreeNotification(Value);
    if Assigned(FLink) then
      FLink.Free;
    CreateDataLink;
    FLink.Control := Value;
    FLink.FieldPath := FFieldPath;
    FLink.NoDisplay := FActionType <> atContent;
    SetUpControlEvents;
  end else begin
    FLink.Free;
    FLink := nil;
  end;
end;

procedure TASNDataControlItem.SetOnControlChange(const Value: TNotifyEvent);
begin
  FOnControlChange := Value;
end;

procedure TASNDataControlItem.SetOnControlClick(const Value: TNotifyEvent);
begin
  FOnControlClick := Value;
end;

procedure TASNDataControlItem.SetOnControlEnter(const Value: TNotifyEvent);
begin
  FOnControlEnter := Value;
end;

procedure TASNDataControlItem.SetOnControlExit(const Value: TNotifyEvent);
begin
  FOnControlExit := Value;
end;

procedure TASNDataControlItem.SetFieldPath(const Value: TASNFieldPath);
begin
  FFieldPath := Value;
  if Assigned(FLink) then
    FLink.FieldPath := Value;
end;

procedure TASNDataControlItem.SetupControlEvents;
begin
  if FControl = nil then Exit;
  if not Assigned(FOnControlClick) then begin
    FOnControlClick := THackControl(FControl).OnClick;
    if TMethod(FOnControlClick).Data = Self then
      FOnControlClick := nil;
  end;
  THackControl(FControl).OnClick := DoControlClick;
  if FControl is TCustomTreeView then begin
    THackTreeView(FControl).Items.Clear;
    if not Assigned(FOnTreeViewChange) then begin
      FOnTreeViewChange := THackTreeView(FControl).OnChange;
      if TMethod(FOnTreeViewChange).Data = Self then
        FOnTreeViewChange := nil;
    end;
    THackTreeView(FControl).OnChange := DoTreeViewChange;
  end else if FControl is TCustomEdit then begin
    if not Assigned(FOnControlChange) then begin
      FOnControlChange := THackEdit(FControl).OnChange;
      if TMethod(FOnControlChange).Data = Self then
        FOnControlChange := nil;
    end;
    if not Assigned(FOnControlEnter) then begin
      FOnControlEnter := THackEdit(FControl).OnEnter;
      if TMethod(FOnControlEnter).Data = Self then
        FOnControlEnter := nil;
    end;
    if not Assigned(FOnControlExit) then begin
      FOnControlExit := THackEdit(FControl).OnExit;
      if TMethod(FOnControlExit).Data = Self then
        FOnControlExit := nil;
    end;
    THackEdit(FControl).OnChange := DoControlChange;
    THackEdit(FControl).OnEnter := DoControlEnter;
    THackEdit(FControl).OnExit := DoControlExit;
  end;
end;

procedure TASNDataControlItem.SetActionType(const Value: TActionType);
begin
  FActionType := Value;
  if Assigned(FLink) then
    FLink.NoDisplay := Value <> atContent;
  if ActionType <> atContent then
    FAutoEdit := False;
end;

procedure TASNDataControlItem.SetOnTreeViewChange(
  const Value: TTVChangedEvent);
begin
  FOnTreeViewChange := Value;
end;

function TASNDataControlItem.GetCaption: TCaption;
begin
  if Assigned(FControl) then
    Result := THackControl(FControl).Caption
  else
    Result := '';
end;

procedure TASNDataControlItem.SetCaption(const Value: TCaption);
begin
  if Assigned(FControl) then
    THackControl(FControl).Caption := Value;
end;

procedure TASNDataControlItem.Enable;
begin
  if Assigned(FControl) then
    FControl.Enabled := True;
end;

procedure TASNDataControlItem.AssignTo(Dest: TPersistent);
var
  D: TASNDataControlItem;
begin
  if Dest is TASNDataControlItem then begin
    D := TASNDataControlItem(Dest);
    D.ActionType := ActionType;
    D.AutoEdit := AutoEdit;
    D.AutoEnable := AutoEnable;
    D.Control := Control;
    D.FieldPath := FieldPath;
    D.OnControlChange := OnControlChange;
    D.OnControlClick := OnControlClick;
    D.OnControlEnter := OnControlEnter;
    D.OnControlExit := OnControlExit;
    D.OnTreeViewChange := OnTreeViewChange;
  end else
    inherited;
end;

function TASNDataControlItem.GetBrowsing: Boolean;
begin
  Result := FLink.FBrowsing;
end;

procedure TASNDataControlItem.SetBrowsing(Value: Boolean);
begin
  FLink.FBrowsing := Value;
end;

{ TASNDataControls }

procedure TASNDataControls.DisableControls;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TASNDataControlItem(Items[I]).FLink.AutoEnable := False;
end;

procedure TASNDataControls.DisableDisplay;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do begin
    TASNDataControlItem(Items[I]).FLink.ClearControl;
    TASNDataControlItem(Items[I]).FLink.FDisplayDisabled := True;
  end;
end;

procedure TASNDataControls.EnableControls;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do begin
    TASNDataControlItem(Items[I]).AutoEnable := True;
    TASNDataControlItem(Items[I]).Enable;
    TASNDataControlItem(Items[I]).FLink.FDisplayDisabled := False;
    TASNDataControlItem(Items[I]).FLink.UpdateControl;
  end;
end;

procedure TASNDataControls.FixupEvents;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TASNDataControlItem(Items[I]).SetupControlEvents;
end;

procedure TASNDataControls.RemoveControl(AControl: TControl);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if TASNDataControlItem(Items[I]).Control = AControl then
      {$IFNDEF D5UP}
      Items[I].Free;
      {$ELSE}
      Delete(I);
      {$ENDIF}
end;

procedure TASNDataControls.SetData(const Value: TASNObject);
begin
  FData := Value;
end;

{ TASNCustomComponent }

procedure TASNCustomComponent.ActivateChoiceField(Field: TCustomChoiceField);
var
  Idx: Integer;
  Ptr: Pointer;
begin
  Idx := FRegistredChoiceProperties.IndexOf(Field.FPath);
  if Idx >= 0 then begin
    Ptr := FRegistredChoiceProperties.Objects[Idx];
    TCustomChoice(Ptr).FField := Field;
  end;
end;

procedure TASNCustomComponent.AddChoiceField(Field: TCustomChoiceField);
begin
  FChoiceFields.Add(Field);
end;

constructor TASNCustomComponent.Create(AOwner: TComponent);
begin
  inherited;
  FChoiceFields := TList.Create;
  CreateData;
  FRegistredChoiceProperties := TStringList.Create;
  FRegistredChoiceProperties.Sorted := True;
  FRegistredChoiceProperties.Duplicates := dupIgnore;
end;

procedure TASNCustomComponent.DefineProperties(Filer: TFiler);

  function IsStored: Boolean;
  begin
    Result := Assigned(FData);
  end;

begin
  inherited;
  Filer.DefineBinaryProperty('DERData',ReadDERData,WriteDERData,IsStored);
end;

destructor TASNCustomComponent.Destroy;
begin
  inherited;
  DestroyChoiceFields;
  FChoiceFields.Free;
  FData.Free;
  FRegistredChoiceProperties.Free;
end;

procedure TASNCustomComponent.DestroyChoiceFields;
var
  I: Integer;
begin
  for I := 0 to FRegistredChoiceProperties.Count - 1 do
    FRegistredChoiceProperties.Objects[I].Free;
  for I := FChoiceFields.Count - 1 downto 0 do
    TCustomChoiceField(FChoiceFields[I]).Free;
end;

function TASNCustomComponent.GetField(const Path: string): PASN1Struct;
begin
  if Assigned(FData) then
    Result := FData.FindField(Path)
  else
    Result := nil;
end;

function TASNCustomComponent.GetNoChange: Boolean;
begin
  if Assigned(FData) then
    Result := FData.NoChangeEvent
  else
    Result := True;
end;

function TASNCustomComponent.GetOnDataChanged: TASN1ChangeEvent;
begin
  if Assigned(FData) then
    Result := FData.OnChange
  else
    Result := nil;
end;

procedure TASNCustomComponent.LoadFromFile(AFileName: TFileName);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName,fmOpenRead);
  try
    LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TASNCustomComponent.LoadFromStream(AStream: TStream);
begin
  if Assigned(FData) then begin
    FData.LoadFromStream(AStream,fmtDER);
    Update;
  end;
end;

procedure TASNCustomComponent.ReadDERData(Stream: TStream);
begin
  FData.LoadFromStream(Stream,fmtDER);
  Update;
end;

procedure TASNCustomComponent.RegisterChoiceProperty(const Path: string;
  AField: TCustomChoice);
begin
  FRegistredChoiceProperties.AddObject(Path,AField);
end;

procedure TASNCustomComponent.RemoveChoiceField(Field: TCustomChoiceField);
begin
  FChoiceFields.Remove(Field);
end;

procedure TASNCustomComponent.SaveToFile(AFileName: TFileName);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName,fmCreate);
  try
    SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TASNCustomComponent.SaveToStream(AStream: TStream);
begin
  if Assigned(FData) then
    FData.SaveToStream(AStream,fmtDER);
end;

procedure TASNCustomComponent.SetNoChange(const Value: Boolean);
begin
  if Assigned(FData) then
    FData.NoChangeEvent := Value;
end;

procedure TASNCustomComponent.SetOnDataChanged(
  const Value: TASN1ChangeEvent);
begin
  if Assigned(FData) then
    FData.OnChange := Value;
  NoChange := not Assigned(Value);
end;

procedure TASNCustomComponent.Update;
var
  Idx: Integer;
begin
  for Idx := 0 to FRegistredChoiceProperties.Count - 1 do
    TCustomChoice(FregistredChoiceProperties.Objects[Idx]).GetChoiceTypeName;
end;

procedure TASNCustomComponent.WriteDERData(Stream: TStream);
begin
  FData.SaveToStream(Stream,fmtDER);
end;

{ TCustomChoiceField }

constructor TCustomChoiceField.Create(AOwner: TASNCustomComponent;
                                      const Path, TypeName: string;
                                      ACls: Byte;
                                      ATag: Cardinal;
                                      AConstructed: Boolean);
begin
  if Assigned(AOwner) then
    AOwner.AddChoiceField(Self);
  FOwner := AOwner;
  FPath := Path;
  FTypeName := TypeName;
  FCls := ACls;
  FTag := ATag;
  FConstructed := AConstructed;
end;

constructor TCustomChoiceField.CreateTypeIdentified(
  AOwner: TASNCustomComponent; const Path, TypeName, IdentifiedBy, IdField: string);
begin
  if Assigned(AOwner) then
    AOwner.AddChoiceField(Self);
  FOwner := AOwner;
  FPath := Path;
  FTypeName := TypeName;
  FIdentifiedBy := IdentifiedBy;
  FIDField := IDField;
end;

destructor TCustomChoiceField.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.RemoveChoiceField(Self);
  inherited;
end;

function TCustomChoiceField.GetActive: Boolean;
var
  Fld: PASN1Struct;
begin
  if (FIDField <> '') then begin
    Fld := GetField(FIDField);
    Result := Assigned(Fld);
    if Result then
      Result := Fld^.ContentAsOID = FIdentifiedBy;
  end else begin
    Fld := GetField(FPath);
    Result := Assigned(Fld);
    if Result then
      Result := (Fld^.Cls = FCls) and
                (Fld^.Tag = FTag) and
                (Fld^.Constructed = FConstructed);
  end;
end;

function TCustomChoiceField.GetField(const Path: string): PASN1Struct;
begin
  if Assigned(FOwner) then
    Result := FOwner.GetField(Path)
  else
    Result := nil;
end;

procedure TCustomChoiceField.SetActive(const Value: Boolean);
var
  Fld, IDField: PASN1Struct;
begin
  if Value then begin
    Fld := GetField(FPath);
    if Assigned(Fld) then begin
      if FIdentifiedBy <> '' then begin
        IDField := THack(Fld^).FindIDField;
        if Assigned(IDField) then
          IDField^.EditContent(FIdentifiedBy);
      end else begin
        if (Fld^.Cls <> FCls) or (Fld^.Tag <> FTag) or
           (Fld^.Constructed xor FConstructed) then
          Assert(Fld^.TrySelectChoice(FCls,FConstructed,FTag));
        if Assigned(FOwner) then
          FOwner.ActivateChoiceField(Self);
      end;
    end;
  end;
end;

{ TCustomOFFieldItem }

constructor TCustomOFFieldItem.Create(Collection: TCollection);
begin
  inherited;
  TCustomOFFields(Collection).AddedItem(Self);
end;

destructor TCustomOFFieldItem.Destroy;
begin
  TCustomOFFields(Collection).RemovedItem(Self);
  inherited;
end;

function TCustomOFFieldItem.GetField(const Path: string): PASN1Struct;
begin
  Result := TCustomOFFields(Collection).GetField(Path);
end;

function TCustomOFFieldItem.GetMyPath: string;
begin
  Result := TCustomOFFields(Collection).FPath + Format('/[%d]',[Index]);
end;

{ TCustomOFFields }

procedure TCustomOFFields.AddedItem(Item: TCustomOFFieldItem);
var
  FldOwner: PASN1Struct;
begin
  FldOwner := GetField(FPath);
  if Count < FldOwner^.ItemCount then
    FldOwner^.AddField;
end;

constructor TCustomOFFields.Create(AOwner: TPersistent;
                                   AComponent: TASNCustomComponent;
                                   ItemClass: TCollectionItemClass;
                                   const APath: string);
begin
  FPath := APath;
  FComponent := AComponent;
  inherited Create(AOwner,ItemClass);
end;

function TCustomOFFields.GetField(const Path: string): PASN1Struct;
begin
  if Assigned(FComponent) then
    Result := FComponent.GetField(Path)
  else
    Result := nil;
end;

procedure TCustomOFFields.RemovedItem(Item: TCustomOFFieldItem);
var
  FldOwner: PASN1Struct;
begin
  FldOwner := GetField(FPath);
  if Item.Index < FldOwner^.ItemCount then
    FldOwner^.DeleteItem(Item.Index);
end;

procedure TCustomOFFields.Update(Item: TCollectionItem);
var
  Fld: PASN1Struct;
begin
  inherited;
  if Item = nil then begin
    Fld := GetField(FPath);
    if Count <> Fld^.ItemCount then begin
      while Count < Fld^.ItemCount do
        Add;
      while Count > Fld^.ItemCount do
        Items[Count-1].Free;
    end;
  end;
end;

{ TCustomChoice }

constructor TCustomChoice.Create(AOwner: TASNCustomComponent;
  const Path: string);
begin
  FOwner := AOwner;
  FPath := Path;
  FOwner.RegisterChoiceProperty(FPath,Self);
  FFields := TStringList.Create;
  CreateFields;
end;

procedure TCustomChoice.CreateFields;
var
  I: Integer;
  Fld: PASN1Struct;
  T: string;
begin
  Fld := FOwner.GetField(FPath);
  if Assigned(Fld) then
    for I := 0 to Fld^.ChoiceCount - 1 do begin
      T := Fld^.Choices[I]^.TypeName;
      FFields.AddObject(T,CreateField(T));
    end;
end;

function TCustomChoice.GetChoiceCount: Integer;
var
  Fld: PASN1Struct;
begin
  Result := 0;
  Fld := FOwner.GetField(FPath);
  if Assigned(Fld) then
    Result := Fld^.ChoiceCount;
end;

function TCustomChoice.GetChoiceTypeName: TChoiceTypeName;         
var
  Fld: PASN1Struct;
  Idx: Integer;
begin
  Result := '';
  Fld := FOwner.GetField(FPath);
  if Assigned(Fld) then begin
    Result := Fld^.ChoiceTypeName;
    Idx := FFields.IndexOf(Result);
    if Idx >= 0 then
      FOwner.ActivateChoiceField(TCustomChoiceField(FFields.Objects[Idx]))
    else begin
      FField := nil;
      Result := '';
    end;
  end;
  Result := Result + #0;
end;

function TCustomChoice.GetChoiceTypeNames(Index: Integer): string;
var
  Fld: PASN1Struct;
begin
  Result := #0;
  Fld := FOwner.GetField(FPath);
  if Assigned(Fld) then
    if (Fld^.ItemCount > Index) and (Index >= 0) then
      Result := Fld^.Choices[Index]^.TypeName + #0;
end;

procedure TCustomChoice.SetChoiceTypeName(const Value: TChoiceTypeName);        
var
  T: string;
  Fld: PASN1Struct;
  Idx: Integer;
begin
  T := Value;
  while (Length(T) > 0) and (T[Length(T)] = #0) do
    Delete(T,Length(T),1);
  Fld := FOwner.GetField(FPath);
  if Assigned(Fld) then begin
    Idx := FFields.IndexOf(Value);
    if Idx >= 0 then
      TCustomChoiceField(FFields.Objects[Idx]).SetActive(True);
  end;
end;

procedure TCustomChoice.SetField(const Value: TCustomChoiceField);
begin
  SetChoiceTypeName(Value.FTypeName);
end;

end.
