{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     FastHelpMain Unit                                 }
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
unit QFastHelpMain;

interface

uses
  Libc, QForms, QActnList, QMenus, QTypes, QImgList, QStdCtrls, QComCtrls,
  QControls, QExtCtrls, SysUtils, {$IFDEF D6UP} Variants,{$ENDIF} Classes,
  ASN1Data, ToolsAPI, ResourceFile, PasToHTMLADS;

type
  TfrmStrSecIIDoc = class(TForm)
    ImageList1: TImageList;
    Splitter1: TSplitter;
    Panel1: TPanel;
    Label1: TLabel;
    Panel2: TPanel;
    btnClose: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TreeView1: TTreeView;
    Label2: TLabel;
    edtIndexSearch: TEdit;
    Label3: TLabel;
    lvIndex: TListView;
    CheckBox1: TCheckBox;
    DERData: TResourceFile;
    PopupMenu1: TPopupMenu;
    Back1: TMenuItem;
    ActionList1: TActionList;
    actBack: TAction;
    TabSheet3: TTabSheet;
    Label4: TLabel;
    edtSearch: TEdit;
    Label5: TLabel;
    lvSearch: TListView;
    DERDataManual: TResourceFile;
    TextBrowser1: TTextBrowser;
    procedure FormCreate(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Collapsed(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Expanded(Sender: TObject; Node: TTreeNode);
    procedure FormDestroy(Sender: TObject);
    procedure edtIndexSearchChange(Sender: TObject);
    procedure lvIndexClick(Sender: TObject);
    procedure lvIndexData(Sender: TObject; Item: TListItem);
    procedure RichEdit1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckBox1Click(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure actBackExecute(Sender: TObject);
    procedure actBackUpdate(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure lvSearchData(Sender: TObject; Item: TListItem);
    procedure lvSearchClick(Sender: TObject);
    procedure TextBrowser1HighlightText(Sender: TObject;
      const HighlightedText: WideString);
  private
    ADS: TASNClientData1Records;
    ADSManual: TASNClientData1Records;
    FInfoNodes: TStrings;
    FDocInfoNodes: TStrings;
    FFullTextIndex: TStrings;
    FFoundItems: TStrings;
    FHighlightLink: WideString;
    FHistory: TList;
    function CreateChildNodes(Node: TTreeNode; Legacy: Boolean): Boolean;
    function CreateChildNodesManual(Node: TTreeNode): Boolean;
    procedure CreateIndex;
    procedure CreateNodes;
    procedure FillIndex;
    procedure FillSearchResult;
  public
    { Public declarations }
  end;

  EStrSecIIDocServices = class(Exception);

  TStrSecIIDocServices = class({$IFNDEF D5UP}TInterfacedObject,{$ELSE}
                                TNotifierObject,{$ENDIF} IOTAWizard, IOTAMenuWizard)
  private
    FIndex: Integer;
    procedure SetIndex(const Value: Integer);
  public
    {$IFNDEF D5UP}
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    {$ENDIF}
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    function GetMenuText: string;
    property Index: Integer read FIndex write SetIndex;
  end;

var
  frmStrSecIIDoc: TfrmStrSecIIDoc;

implementation

uses
  Qt, ASN1, FastHelpHTML;

{$R *.xfm}

var
  StrSecIIDocServices: TStrSecIIDocServices;

function TfrmStrSecIIDoc.CreateChildNodes(Node: TTreeNode; Legacy: Boolean): Boolean;
var
  F, G: TMembers;
  E, ES: TMember;
  I, J, P: Integer;
  Child, SubChild: TTreeNode;
  S, CName: string;
  UnDoc: Boolean;

begin
  F := Node.Data;
  Result := False;
  for I := 0 to F.ItemCount - 1 do begin
    E := TMember(F.Items[I]);
    if (E.IsHidden and E.IsDeprecated and E.IsBeta) and
       not ((E is TASNClientData1Record) or (E is TClassesRecord)) then begin
      E.IsHidden := False;
      E.IsDeprecated := False;
      E.IsBeta := False;
    end;
    if E.IsHidden then Continue;
    if (E is TASNClientData1Record) and (E.IsDeprecated xor Legacy) then Continue;
    CName := E.Name;
    if E.IsDeprecated then
      CName := CName + ' (Deprecated)'
    else if E.IsBeta then
      CName := CName + ' (Beta)';
    Child := Node.Owner.AddChild(Node,CName);

    UnDoc := E.DescriptionEmpty;
    UnDoc := UnDoc or E.IsDeprecated or E.IsBeta;

    FInfoNodes.AddObject(E.Name,TObject(Child.Handle));

    P := Pos('.',CName);
    if P > 0 then begin
      S := E.Name;
      S := Copy(S,P+1,MaxInt) + ' (class ' + Copy(S,1,P-1) + ')';
      FInfoNodes.AddObject(S,TObject(Child.Handle));
    end;

    Child.ImageIndex := 2;
    Child.SelectedIndex := 2;
    Child.Data := E;
    for J := 0 to E.ItemCount - 1 do begin
      if E.Items[J] is TMember then begin
        ES := TMember(E.Items[J]);
        if ES.Declaration <> '' then begin
          SubChild := Node.Owner.AddChild(Child,ES.Name);
          SubChild.ImageIndex := 0;
          SubChild.SelectedIndex := 0;
          SubChild.Data := ES;
        end;
      end;
      if not (E.Items[J] is TMembers) then
        Continue;
      G := TMembers(E.Items[J]);
      if G.ItemCount > 0 then begin
        Child.ImageIndex := 0;
        Child.SelectedIndex := 0;
        SubChild := Node.Owner.AddChild(Child,G.VarName);
        SubChild.Data := G;
        if CreateChildNodes(SubChild,Legacy) then begin
          SubChild.ImageIndex := 0;
          SubChild.SelectedIndex := 0;
          Result := True;
        end else begin
          SubChild.ImageIndex := 3;
          SubChild.SelectedIndex := 3;
        end;
      end;
    end;
    Result := Result or not UnDoc;
    if E.IsBeta or E.IsDeprecated or not Result then begin
      Child.ImageIndex := 3;
      Child.SelectedIndex := 3;
    end else if not UnDoc then begin

      FDocInfoNodes.AddObject(CName,TObject(Child.Handle));

      S := CName;
      P := Pos('.',S);
      if P > 0 then begin
        S := Copy(S,P+1,MaxInt) + ' (class ' + Copy(S,1,P-1) + ')';
        FDocInfoNodes.AddObject(S,TObject(Child.Handle));
      end;

    end;
  end;
end;

function TfrmStrSecIIDoc.CreateChildNodesManual(Node: TTreeNode): Boolean;
var
  ADS: TASNClientData1Records;
  Classes: TClassesRecords;
  Members: TMembers;
  UnitNode, ClassNode, MbrNode: TTreeNode;
  I, J, K: Integer;

begin
  Result := True;
  ADS := ADSManual;
  for I := 0 to ADS.Count - 1 do begin
    UnitNode := Node.Owner.AddChildObject(Node,ADS[I].Name,ADS[I]);

    FInfoNodes.AddObject(ADS[I].Name,TObject(UnitNode.Handle));
    FDocInfoNodes.AddObject(ADS[I].Name,TObject(UnitNode.Handle));

    Classes := ADS[I].Classes;
    if Classes.Count > 0 then begin
      UnitNode.ImageIndex := 0;
      UnitNode.SelectedIndex := 0;
      for J := 0 to Classes.Count - 1 do begin
        ClassNode := Node.Owner.AddChildObject(UnitNode,Classes[J].Name,Classes[J]);
        FInfoNodes.AddObject(Classes[J].Name,TObject(ClassNode.Handle));
        FDocInfoNodes.AddObject(Classes[J].Name,TObject(ClassNode.Handle));
        Members := Classes[J].Methods;
        if Members.Count > 0 then begin
          ClassNode.ImageIndex := 0;
          ClassNode.SelectedIndex := 0;
          for K := 0 to Members.Count - 1 do begin
            MbrNode := Node.Owner.AddChildObject(ClassNode,TMember(Members.Items[K]).Name,Members.Items[K]);
            FInfoNodes.AddObject(TMember(Members.Items[K]).Name,TObject(MbrNode.Handle));
            FDocInfoNodes.AddObject(TMember(Members.Items[K]).Name,TObject(MbrNode.Handle));
            MbrNode.ImageIndex := 2;
            MbrNode.SelectedIndex := 2;
          end;
        end else begin
          ClassNode.ImageIndex := 2;
          ClassNode.SelectedIndex := 2;
        end;
      end;
    end else begin
      UnitNode.ImageIndex := 2;
      UnitNode.SelectedIndex := 2;
    end;
  end;
end;

procedure TfrmStrSecIIDoc.CreateNodes;
var
  Node: TTreeNode;
begin
  TreeView1.Items.Clear;
  TreeView1.Items.BeginUpdate;
  try
    Node := TreeView1.Items.Add(nil,'Manual');
    Node.Data := ADSManual;
    Node.ImageIndex := 0;
    Node.SelectedIndex := 1;
    CreateChildNodesManual(Node);
    Node := TreeView1.Items.Add(nil,'Units');
    Node.Data := ADS;
    Node.ImageIndex := 0;
    Node.SelectedIndex := 1;
    CreateChildNodes(Node,False);
    Node := TreeView1.Items.Add(nil,'Legacy Units');
    Node.Data := ADS;
    Node.ImageIndex := 0;
    Node.SelectedIndex := 1;
    CreateChildNodes(Node,True);
  finally
    TreeView1.Items.EndUpdate;
  end;
end;

procedure TfrmStrSecIIDoc.FormCreate(Sender: TObject);
begin
  FHistory := TList.Create;
  if Assigned(ADS) then
    Exit;
  ADS := TASNClientData1Records.Create(nil,nil,nil);
  DERData.DataStream.Position := 0;
  try
    ADS.LoadFromStream(DERData.DataStream);
  except
    ADS.Free;
    ADS := nil;
    raise Exception.Create('Could not load DER data');
  end;
  ADS.HTMLPath := '/tmp';
  ADSManual := TASNClientData1Records.Create(nil,nil,nil);
  DERDataManual.DataStream.Position := 0;
  try
    ADSManual.LoadFromStream(DERDataManual.DataStream);
  except
    ADSManual.Free;
    ADSManual := nil;
    raise Exception.Create('Could not load DER data');
  end;                   
  ADSManual.HTMLPath := '/tmp';
  FInfoNodes := TStringList.Create;
  FDocInfoNodes := TStringList.Create;
  FFoundItems := TStringList.Create;
  TStringList(FInfoNodes).Duplicates := dupIgnore;
  TStringList(FInfoNodes).Sorted := True;
  TStringList(FDocInfoNodes).Duplicates := dupIgnore;
  TStringList(FDocInfoNodes).Sorted := True;
  TStringList(FFoundItems).Duplicates := dupIgnore;
  TStringList(FFoundItems).Sorted := True;
  CreateNodes;
  FillIndex;
  lvIndex.Repaint;
  PageControl1.Align := alLeft;
  Splitter1.Align := alLeft;
  Panel1.Align := alClient;
  Label1.Align := alTop;
  Panel2.Align := alBottom;
  TextBrowser1.Align := alClient;
  CheckBox1.Top := TabSheet1.Height - 24;
  CheckBox1.Left := 8;
  CheckBox1.Anchors := [akLeft, akBottom];
  lvIndex.Height := TabSheet1.Height - 87;
  lvIndex.Width := TabSheet1.Width - 16;
  lvIndex.Anchors := [akLeft, akTop, akRight, akBottom];
  edtIndexSearch.Width := lvIndex.Width;
  edtIndexSearch.Anchors := [akLeft, akTop, akRight];
  lvSearch.Height := TabSheet1.Height - 63;
  lvSearch.Width := TabSheet1.Width - 16;
  lvSearch.Anchors := [akLeft, akTop, akRight, akBottom];
  edtSearch.Width := lvSearch.Width;
  edtSearch.Anchors := [akLeft, akTop, akRight];
  btnClose.Left := Panel2.Width - 85;
  btnClose.Anchors := [akTop, akRight];
//  SendMessage(RichEdit1.Handle, WM_USER + 91, Longint(True), 0);
end;

procedure TfrmStrSecIIDoc.TreeView1Change(Sender: TObject; Node: TTreeNode);
var
  F: TMember;
  FN: string;
begin
  if Node.Selected then begin
    if TObject(Node.Data) is TMember then begin
      Node.MakeVisible;
      F := Node.Data;
      FN := F.FileName;
      F.RecordToFile(FN);
      TextBrowser1.LoadFromFile(FN);
      Label1.Caption := F.Name;
      FHistory.Insert(0,Node.Handle);
    end;
  end;
end;

procedure TfrmStrSecIIDoc.TreeView1Collapsed(Sender: TObject; Node: TTreeNode);
begin
  if Node.ImageIndex = 1 then begin
    Node.ImageIndex := 0;
    Node.SelectedIndex := 0;
  end else if Node.ImageIndex = 4 then begin
    Node.ImageIndex := 3;
    Node.SelectedIndex := 3;
  end;
end;

procedure TfrmStrSecIIDoc.TreeView1Expanded(Sender: TObject; Node: TTreeNode);
begin
  if Node.ImageIndex = 0 then begin
    Node.ImageIndex := 1;
    Node.SelectedIndex := 1;
  end else if Node.ImageIndex = 3 then begin
    Node.ImageIndex := 4;
    Node.SelectedIndex := 4;
  end;
end;

procedure TfrmStrSecIIDoc.FormDestroy(Sender: TObject);
begin
  FInfoNodes.Free;
  FDocInfoNodes.Free;
  if Assigned(FFullTextIndex) then
    while FFullTextIndex.Count > 0 do begin
      TList(FFullTextIndex.Objects[0]).Free;
      FFullTextIndex.Delete(0);
    end;
  FFullTextIndex.Free;
  FFoundItems.Free;
  if Self = frmStrSecIIDoc then
    frmStrSecIIDoc := nil;
  FHistory.Free;
end;

procedure TfrmStrSecIIDoc.edtIndexSearchChange(Sender: TObject);
var
  I: Integer;
  LI: TListItem;
begin
  if CheckBox1.Checked then
    TStringList(FDocInfoNodes).Find(edtIndexSearch.Text,I)
  else
    TStringList(FInfoNodes).Find(edtIndexSearch.Text,I);
  if I > -1 then begin
    LI := lvIndex.Items[I];
    LI.Selected := True;
    LI.MakeVisible;
  end else
    Beep;
end;

procedure TfrmStrSecIIDoc.lvIndexClick(Sender: TObject);
var
  TNHandle: QListViewItemH;
  Node: TTreeNode;
begin
  if lvIndex.Selected = nil then
    Exit;
  if CheckBox1.Checked then
    TNHandle := QListViewItemH(FDocInfoNodes.Objects[lvIndex.Selected.Index])
  else
    TNHandle := QListViewItemH(FInfoNodes.Objects[lvIndex.Selected.Index]);
  Node := TreeView1.Items.GetNode(TNHandle);
  if Node <> nil then
    Node.Selected := True;
end;

procedure TfrmStrSecIIDoc.lvIndexData(Sender: TObject; Item: TListItem);
begin
  if CheckBox1.Checked then
    Item.Caption := FDocInfoNodes[Item.Index]
  else
    Item.Caption := FInfoNodes[Item.Index];
end;

procedure TfrmStrSecIIDoc.RichEdit1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  S: string;
  Link: Boolean;
  I: Integer;
  TNHandle: QListViewItemH;
  Node: TTreeNode;

  function IsLink: Boolean;
  begin
    Result := FHighlightLink <> '';
  end;

begin
  Link := IsLink;
  if Link then
    S := FHighlightLink
  else begin
    S := Trim(TextBrowser1.SelectedText);
    while (S <> '') and (S[Length(S)] = '.') do
      Delete(S,Length(S),1);
  end;
  if S <> '' then begin
    if Link then begin
      I := TStringList(FInfoNodes).IndexOf(S);
      if I < 0 then
        I := TStringList(FInfoNodes).IndexOf(StringReplace(S,'_',' ',[rfReplaceAll]));
      if I >= 0 then begin
        TNHandle := QListViewItemH(FInfoNodes.Objects[I]);
        Node := TreeView1.Items.GetNode(TNHandle);
        if Node <> nil then
          Node.Selected := True
        else
          Beep;
      end else
        Beep;
    end;
    edtIndexSearch.Text := S;
  end;
end;

procedure TfrmStrSecIIDoc.CheckBox1Click(Sender: TObject);
begin
  FillIndex;
end;

{ TStrSecIIDocServices }

{$IFNDEF D5UP}
procedure TStrSecIIDocServices.AfterSave;
begin

end;

procedure TStrSecIIDocServices.BeforeSave;
begin

end;

procedure TStrSecIIDocServices.Destroyed;
begin

end;
{$ENDIF}

procedure TStrSecIIDocServices.Execute;
var
  Curs: TCursor;
begin
  Curs := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if frmStrSecIIDoc = nil then
      frmStrSecIIDoc := TfrmStrSecIIDoc.Create(nil);

    frmStrSecIIDoc.Show;
  finally
    Screen.Cursor := Curs;
  end;
end;

function TStrSecIIDocServices.GetIDString: string;
begin
  Result := 'StreamSec.StrSecIIDocServices.Docs.1'; { do not localize }
end;

function TStrSecIIDocServices.GetMenuText: string;
resourcestring
  sMenuText = 'StrSecII Help';

begin
  Result := sMenuText;
end;

function TStrSecIIDocServices.GetName: string;
begin
  Result := 'StreamSec.StrSecIIDocServices.Docs'; { do not localize }
end;

function TStrSecIIDocServices.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

{$IFNDEF D5UP}
procedure TStrSecIIDocServices.Modified;
begin

end;
{$ENDIF}

procedure TStrSecIIDocServices.SetIndex(const Value: Integer);
begin
  FIndex := Value;
end;

resourcestring
  sStrSecIIDocError = 'Error creating StrSecIIDocServices wizard';

procedure InitStrSecIIDocServices;
begin
  if (BorlandIDEServices <> nil) then begin
    StrSecIIDocServices := TStrSecIIDocServices.Create;
    StrSecIIDocServices.Index := (BorlandIDEServices as IOTAWizardServices).AddWizard(StrSecIIDocServices as IOTAWizard);
    if StrSecIIDocServices.Index < 0 then
      raise EStrSecIIDocServices.Create(sStrSecIIDocError);
  end;
end;

procedure DoneStrSecIIDocServices;
begin
  if (BorlandIDEServices <> nil) then
  begin
    if frmStrSecIIDoc <> nil then
      frmStrSecIIDoc.Free;

    (BorlandIDEServices as IOTAWizardServices).RemoveWizard(StrSecIIDocServices.Index);
  end;
end;

procedure TfrmStrSecIIDoc.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmStrSecIIDoc.actBackExecute(Sender: TObject);
var
  TNHandle: QListViewItemH;
  Node: TTreeNode;
begin
  if FHistory.Count = 0 then Exit;
  FHistory.Delete(0);
  TNHandle := QListViewItemH(FHistory.First);
  if FHistory.Count = 0 then Exit;
  Node := TreeView1.Items.GetNode(TNHandle);
  if Node <> nil then
    Node.Selected := True
  else
    Beep;
end;

procedure TfrmStrSecIIDoc.actBackUpdate(Sender: TObject);
begin
  if Assigned(FHistory) then
    actBack.Enabled := FHistory.Count > 1;
end;

procedure TfrmStrSecIIDoc.edtSearchChange(Sender: TObject);
var
  SL: TStringList;
  I, J, K: Integer;
  List: TList;
  Node: TTreeNode;
  E: TMember;
begin
  if FFullTextIndex = nil then
    CreateIndex;

  SL := TStringList.Create;
  try
    SL.Text := StringReplace(edtSearch.Text,' ',#13#10,[rfReplaceAll]);
    for K := 0 to SL.Count - 1 do begin
      J := FFullTextIndex.IndexOf(LowerCase(SL[K]));
      if J >= 0 then begin
        List := TList(FFullTextIndex.Objects[J]);
        if K = 0 then begin
          FFoundItems.Clear;
          for I := 0 to List.Count - 1 do begin
            Node := TreeView1.Items.GetNode(List[I]);
            if Node = nil then Continue;
            E := Node.Data;
            FFoundItems.AddObject(E.Name,TObject(Node.Handle));
          end;
        end else
          for I := FFoundItems.Count - 1 downto 0 do
            if List.IndexOf(FFoundItems.Objects[I]) < 0 then
              FFoundItems.Delete(I);
        FillSearchResult;
      end else if K = 0 then begin
        FFoundItems.Clear;
        FillSearchResult;
        Break;
      end;
    end;
  finally
    SL.Free;
  end;
end;

procedure TfrmStrSecIIDoc.lvSearchData(Sender: TObject; Item: TListItem);
begin
  Item.Caption := FFoundItems[Item.Index];
end;

procedure TfrmStrSecIIDoc.lvSearchClick(Sender: TObject);
var
  TNHandle: QListViewItemH;
  Node: TTreeNode;
begin
  if lvSearch.Selected = nil then
    Exit;
  TNHandle := QListViewItemH(FFoundItems.Objects[lvSearch.Selected.Index]);
  Node := TreeView1.Items.GetNode(TNHandle);
  if Node <> nil then
    Node.Selected := True;
end;

procedure TfrmStrSecIIDoc.CreateIndex;
var
  CurItem: TTreeNode;  

  procedure AddToIndex(const Text: string; Node: TTreeNode);
  var
    SL: TStringList;
    P: PChar;
    S: string;
    I, J: Integer;
  begin
    SL := TStringList.Create;
    try
      SL.Sorted := True;
      SL.Duplicates := dupIgnore;
      SL.Capacity := Length(Text) shr 2;
      S := Text;
      UniqueString(S);
      J := Length(S)-1;
      while J > 0 do begin
        if not (S[J] in ['a'..'z','A'..'Z','0'..'9','.','-','_','#']) then begin
          P := @S[J+1];
          if P <> '' then
            SL.Add(P);
          S[J] := #0;
        end else if S[J] = '&' then
          S[J] := #0;
        Dec(J);
      end;
      P := PChar(S);
      if P <> '' then
        SL.Add(P);
      for I := 0 to SL.Count - 1 do begin
        S := SL[I];
        S := LowerCase(S);
        J := FFullTextIndex.IndexOf(S);
        if J < 0 then
          J := FFullTextIndex.AddObject(S,TList.Create);
        TList(FFullTextIndex.Objects[J]).Add(Node.Handle);
      end;
    finally
      SL.Free;
    end;
  end;

begin
  Screen.Cursor := crAppStart;
  try
    FFullTextIndex := TStringList.Create;
    TStringList(FFullTextIndex).Duplicates := dupIgnore;
    TStringList(FFullTextIndex).Sorted := True;
    CurItem := TreeView1.Items.GetFirstNode;
    while CurItem <> nil do begin
      if (CurItem.Data <> nil) and
         (TObject(CurItem.Data) is TMember) then begin
        AddToIndex(TMember(CurItem.Data).Description,CurItem);
        AddToIndex(TMember(CurItem.Data).Declaration,CurItem);
      end;
      CurItem := CurItem.GetNext;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmStrSecIIDoc.TextBrowser1HighlightText(Sender: TObject;
  const HighlightedText: WideString);
var
  S: string;
  I: Integer;
  TNHandle: QListViewItemH;
  Node: TTreeNode;
begin
  S := ExtractFileName(HighlightedText);
  S := ChangeFileExt(S,'');
  S := StringReplace(S,'_','.',[]);
  while (S <> '') and (S[Length(S)] = '.') do
    Delete(S,Length(S),1);
  if S <> '' then begin
    I := TStringList(FInfoNodes).IndexOf(S);
    if I < 0 then
      I := TStringList(FInfoNodes).IndexOf(StringReplace(S,'_',' ',[rfReplaceAll]));
    if I >= 0 then begin
      TNHandle := QListViewItemH(FInfoNodes.Objects[I]);
      Node := TreeView1.Items.GetNode(TNHandle);
      if Node <> nil then
        TMember(Node.Data).RecordToFile(TMember(Node.Data).FileName);
    end;
  end;
  FHighlightLink := S;
end;

procedure TfrmStrSecIIDoc.FillIndex;
var
  I: Integer;
  ListItem: TListItem;
begin
  lvIndex.Items.Clear;
  for I := 0 to FDocInfoNodes.Count - 1 do begin
    ListItem := lvIndex.Items.Add;
    lvIndexData(Self,ListItem);
  end;
end;

procedure TfrmStrSecIIDoc.FillSearchResult;
var
  I: Integer;
  ListItem: TListItem;
begin
  lvSearch.Items.Clear;
  for I := 0 to FFoundItems.Count - 1 do begin
    ListItem := lvSearch.Items.Add;
    lvSearchData(Self,ListItem);
  end;
end;

initialization
  InitStrSecIIDocServices;

finalization
  DoneStrSecIIDocServices;

end.
