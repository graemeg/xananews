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
unit QAsnEditorMain;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, Messages,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF}
  SysUtils, Classes, QGraphics, QControls, QForms,
  QDialogs, QComCtrls, Asn1, QStdCtrls, QExtCtrls, QMask, Asn1Data,
  AsnToPascal;

type
  TfrmASN1Editor = class(TForm)
    gbItems: TGroupBox;
    TreeView1: TTreeView;
    Panel1: TPanel;
    btnCancel: TButton;
    btnApply: TButton;
    btnOK: TButton;
    Splitter1: TSplitter;
    gbItemProps: TGroupBox;
    btnNewItem: TButton;
    btnDelete: TButton;
    btnLoad: TButton;
    gbType: TGroupBox;
    cbDefTypeName: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    cbCls: TComboBox;
    Label3: TLabel;
    meTag: TMaskEdit;
    gbDecl: TGroupBox;
    Label4: TLabel;
    edtVarName: TEdit;
    Label5: TLabel;
    chbOptional: TCheckBox;
    chbHasDefaultValue: TCheckBox;
    chbImplicit: TCheckBox;
    Label6: TLabel;
    edtPath: TEdit;
    lvChoices: TListView;
    Label8: TLabel;
    btnAddChoice: TButton;
    btnDeleteChoice: TButton;
    Label9: TLabel;
    edtSelectedChoice: TEdit;
    btnEditChoice: TButton;
    Label11: TLabel;
    lvTypeIdentifiers: TListView;
    Label13: TLabel;
    btnEditIdentifier: TButton;
    btnAddIdentifier: TButton;
    btnDeleteIdentifier: TButton;
    Label7: TLabel;
    cbBooleanValue: TComboBox;
    Label10: TLabel;
    chbViewAsHex: TCheckBox;
    Label14: TLabel;
    memHex: TMemo;
    btnEditAsStruct: TButton;
    btnApplyHex: TButton;
    btnApplyText: TButton;
    Label15: TLabel;
    memInteger: TMemo;
    rgShowInteger: TRadioGroup;
    btnApplyInteger: TButton;
    Label16: TLabel;
    edtYear: TEdit;
    Label17: TLabel;
    edtMonth: TEdit;
    Label18: TLabel;
    edtDay: TEdit;
    Label19: TLabel;
    edtHour: TEdit;
    Label20: TLabel;
    edtMinute: TEdit;
    Label21: TLabel;
    edtSeconds: TEdit;
    lblMilliseconds: TLabel;
    edtMilliseconds: TEdit;
    btnApplyDate: TButton;
    Label23: TLabel;
    edtRealContent: TEdit;
    cbIDField: TComboBox;
    btnApplyReal: TButton;
    OpenDialog1: TOpenDialog;
    btnSave: TButton;
    SaveDialog1: TSaveDialog;
    btnNewSubItem: TButton;
    cbTypeName: TComboBox;
    btnEditTemplate: TButton;
    btnApplyType: TButton;
    btnTypeChoice: TButton;
    btnTypeIdentifier: TButton;
    Label12: TLabel;
    cbObject: TComboBox;
    btnView: TButton;
    chbConstructed: TCheckBox;
    btnConvert: TButton;
    lbObjectIdentifies: TListBox;
    Label22: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    TabSheet10: TTabSheet;
    TabSheet11: TTabSheet;
    memText: TMemo;
    procedure lvTypeIdentifiersData(Sender: TObject; Item: TListItem);
    procedure rgShowIntegerClick(Sender: TObject);
    procedure btnApplyIntegerClick(Sender: TObject);
    procedure btnApplyTextClick(Sender: TObject);
    procedure cbBooleanValueChange(Sender: TObject);
    procedure btnApplyDateClick(Sender: TObject);
    procedure btnApplyHexClick(Sender: TObject);
    procedure btnApplyRealClick(Sender: TObject);
    procedure lvChoicesData(Sender: TObject; Item: TListItem);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnNewItemClick(Sender: TObject);
    procedure btnNewSubItemClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure FormCreate(Sender: TObject);
    procedure edtVarNameChange(Sender: TObject);
    procedure meTagChange(Sender: TObject);
    procedure cbClsChange(Sender: TObject);
    procedure chbOptionalClick(Sender: TObject);
    procedure chbHasDefaultValueClick(Sender: TObject);
    procedure chbImplicitClick(Sender: TObject);
    procedure cbDefTypeNameClick(Sender: TObject);
    procedure btnAddChoiceClick(Sender: TObject);
    procedure cbTypeNameDropDown(Sender: TObject);
    procedure btnApplyTypeClick(Sender: TObject);
    procedure btnEditTemplateClick(Sender: TObject);
    procedure lvChoicesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnDeleteChoiceClick(Sender: TObject);
    procedure btnEditChoiceClick(Sender: TObject);
    procedure edtSelectedChoiceChange(Sender: TObject);
    procedure btnTypeChoiceClick(Sender: TObject);
    procedure btnEditAsStructClick(Sender: TObject);
    procedure btnDeleteIdentifierClick(Sender: TObject);
    procedure lvTypeIdentifiersSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnTypeIdentifierClick(Sender: TObject);
    procedure btnAddIdentifierClick(Sender: TObject);
    procedure btnEditIdentifierClick(Sender: TObject);
    procedure cbObjectChange(Sender: TObject);
    procedure btnViewClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure chbConstructedClick(Sender: TObject);
    procedure cbIDFieldChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnConvertClick(Sender: TObject);
  private
    FStruct: TASN1Struct;
    FCurrent: PASN1Struct;
    FASNObject: TASNObject;
    FModified: Boolean;
    FApplied: Boolean;
    procedure CreateChildNodes(Node: TTreeNode);
    procedure CreateNodes;
    procedure Display;
    procedure DisplayBoolean;
    procedure DisplayChoice;
    procedure DisplayCurrent;
    procedure DisplayDateTime;
    procedure DisplayHex;
    procedure DisplayInteger;
    procedure DisplayObject;
    procedure DisplayReal;
    procedure DisplayText;
    procedure DisplayTypeIdentified;
    procedure ListDefaultTypeNames;
    procedure ListTypeNames(List: TStrings);
    procedure SelectNil;
    procedure SetStruct(const Value: TASN1Struct);
    procedure SetASNObject(const Value: TASNObject);
    procedure SetModified(const Value: Boolean);
  public
    property ASNObject: TASNObject read FASNObject write SetASNObject;
    property Struct: TASN1Struct read FStruct write SetStruct;
    property Modified: Boolean read FModified write SetModified;
  end;

var
  frmASN1Editor: TfrmASN1Editor;

implementation

uses
  Pkix, MpArithTypes, MpArith,
  AsnChoiceEditor, AsnTypeIdentifierEditor, AsnModuleView;

{$R *.xfm}

{ TfrmASN1Editor }

procedure TfrmASN1Editor.DisplayChoice;
var
  I: Integer;
begin
  lvChoices.Items.Clear;
  for I := 0 to FCurrent^.ChoiceCount - 1 do
    lvChoicesData(Self,lvChoices.Items.Add);
  edtSelectedChoice.Text := '-1';
  if FCurrent^.ChoiceTypeName <> '' then begin
    for I := 0 to FCurrent^.ChoiceCount - 1 do
      if FCurrent^.Choices[I]^.TypeName = FCurrent^.ChoiceTypeName then begin
        edtSelectedChoice.Text := IntToStr(I);
        Break;
      end;
  end;
  btnDeleteChoice.Enabled := lvChoices.Selected <> nil;
  btnEditChoice.Enabled := lvChoices.Selected <> nil;
  btnTypeChoice.Enabled := lvChoices.Selected <> nil;
  PageControl1.ActivePageIndex := 1;
end;

function IsChoiceOrTemplate(F: TASN1Struct): Boolean;
begin
  Result := F.ChoiceCount > 0;
  if not Result then
    if F.Owner <> nil then begin
      Result := (F.Owner.Template <> nil) or IsChoiceOrTemplate(F.Owner);
    end;
end;

procedure TfrmASN1Editor.DisplayCurrent;
var
  vTag: Integer;
begin
  btnDelete.Enabled := True;
  btnNewSubItem.Enabled := FCurrent^.Constructed and
                           not IsChoiceOrTemplate(FCurrent^);
  btnNewItem.Enabled := (FCurrent^.Owner <> nil) and
                        not IsChoiceOrTemplate(FCurrent^.Owner);

  cbDefTypeName.Enabled := (FCurrent^.Owner = nil) or
                           not IsChoiceOrTemplate(FCurrent^.Owner);
  cbCls.Enabled := (FCurrent^.Owner = nil) or
                   not IsChoiceOrTemplate(FCurrent^.Owner);
  meTag.Enabled := (FCurrent^.Owner = nil) or
                   not IsChoiceOrTemplate(FCurrent^.Owner);
  chbImplicit.Enabled := (FCurrent^.Owner = nil) or
                         not IsChoiceOrTemplate(FCurrent^.Owner);
  chbConstructed.Enabled := ((FCurrent^.Owner = nil) or
                             not IsChoiceOrTemplate(FCurrent^.Owner)) and
                            (FCurrent^.Implicit or
                             (FCurrent^.Tag = Cardinal(V_ASN1_UNDEF)));
  edtVarName.Enabled := (FCurrent^.Owner = nil) or
                        not IsChoiceOrTemplate(FCurrent^.Owner);
  cbTypeName.Enabled := (FCurrent^.Owner = nil) or
                        not IsChoiceOrTemplate(FCurrent^.Owner);
  chbOptional.Enabled := (FCurrent^.Owner = nil) or
                         not IsChoiceOrTemplate(FCurrent^.Owner);
  chbHasDefaultValue.Enabled := (FCurrent^.Owner = nil) or
                                not IsChoiceOrTemplate(FCurrent^.Owner);
  btnApplyType.Enabled := cbTypeName.Enabled;

  edtPath.Text := FCurrent^.GetNamePath;

  cbDefTypeName.Text := '(Select)';
  meTag.Text := IntToStr(FCurrent^.Tag);
  cbCls.ItemIndex := FCurrent^.Cls shr 6;
  chbImplicit.Checked := FCurrent^.Implicit;
  chbConstructed.Checked := FCurrent^.Constructed;

  edtVarName.Text := FCurrent^.VarName;
  cbTypeName.Text := FCurrent^.TypeName;

  chbOptional.Checked := FCurrent^.Optional;
  chbHasDefaultValue.OnClick := nil;
  chbHasDefaultValue.Checked := FCurrent^.HasDefaultValue;
  chbHasDefaultValue.OnClick := chbHasDefaultValueClick;

  if FCurrent^.ChoiceCount > 0 then begin
    if FCurrent^.TypeIdentified then
      DisplayTypeIdentified
    else
      DisplayChoice;
  end else begin
    lvChoices.Items.Clear;
    lvTypeIdentifiers.Items.Clear;

    if FCurrent^.Implicit then begin
      vTag := FCurrent^.ImplicitTag;
      if (vTag = 0) or (vTag = V_ASN1_UNDEF) then begin
        vTag := V_ASN1_SEQUENCE;
        FCurrent^.ImplicitTag := vTag;
      end;
    end else if FCurrent^.Cls = V_ASN1_UNIVERSAL then
      vTag := FCurrent^.Tag
    else
      vTag := V_ASN1_UNDEF;
    case vTag of
      V_ASN1_BOOLEAN:   DisplayBoolean;
      V_ASN1_GENERALIZEDTIME,
      V_ASN1_UTCTIME:   DisplayDateTime;
      V_ASN1_INTEGER:   DisplayInteger;
      V_ASN1_OBJECT:    DisplayObject;
      V_ASN1_PRINTABLESTRING,
      V_ASN1_VISIBLESTRING,
      V_ASN1_NUMERICSTRING,
      V_ASN1_IA5STRING,
      V_ASN1_TELETEXSTRING,
      V_ASN1_VIDEOTEXSTRING,
      V_ASN1_GENERALSTRING,
      V_ASN1_UNIVERSALSTRING,
      V_ASN1_UTF8STRING,
      V_ASN1_BMPSTRING: DisplayText;
      V_ASN1_REAL:      DisplayReal;
      V_ASN1_UNDEF,
      V_ASN1_SEQUENCE,
      V_ASN1_SET:
        if (FCurrent^.Template = nil) or not FCurrent^.Constructed then
          PageControl1.ActivePageIndex := 0
        else begin
          PageControl1.ActivePageIndex := 9;
          btnEditTemplate.Enabled := FCurrent^.ItemCount <= 0;
        end;
    else
      if FCurrent^.Constructed then begin
        if (FCurrent^.Template = nil) or not FCurrent^.Constructed then
          PageControl1.ActivePageIndex := 0
        else begin
          PageControl1.ActivePageIndex := 9;
          btnEditTemplate.Enabled := FCurrent^.ItemCount <= 0;
        end;
      end else
        DisplayHex;
    end;
  end;
end;

procedure TfrmASN1Editor.DisplayTypeIdentified;
var
  I, MyIdx: Integer;
  Owner: TASN1Struct;
begin
  lvTypeIdentifiers.Items.Clear;
  for I := 0 to FCurrent^.ChoiceCount - 1 do
    lvTypeIdentifiersData(Self,lvTypeIdentifiers.Items.Add);
  Owner := FCurrent^.Owner;
  if Owner = nil then
    MyIdx := 0
  else if (Owner.Cls <> V_ASN1_UNIVERSAL) and not Owner.Implicit then begin
    Owner := Owner.Owner;
    MyIdx := Owner.IndexOf(FCurrent^.Owner);
  end else
    MyIdx := Owner.IndexOf(FCurrent^);
  cbIDField.Items.Clear;
  for I := 0 to MyIdx - 1 do
    if ((Owner.Items[I]^.Cls = V_ASN1_UNIVERSAL) and
        (Owner.Items[I]^.Tag = V_ASN1_OBJECT)) or
       (Owner.Items[I]^.Implicit and
        (Owner.Items[I]^.ImplicitTag = V_ASN1_OBJECT)) then
      cbIDField.Items.Add(Owner.Items[I]^.VarName);
  cbIDField.Text := FCurrent^.IDField;
  btnDeleteIdentifier.Enabled := lvTypeIdentifiers.Selected <> nil;
  btnEditIdentifier.Enabled := lvTypeIdentifiers.Selected <> nil;
  btnTypeIdentifier.Enabled := lvTypeIdentifiers.Selected <> nil;
  PageControl1.ActivePageIndex := 2;
end;

procedure TfrmASN1Editor.ListDefaultTypeNames;
var
  SL: TStrings;
begin
  SL := cbDefTypeName.Items;
  SL.Add('BOOLEAN');            // 0
  SL.Add('INTEGER');            // 1
  SL.Add('BIT STRING');         // 2
  SL.Add('OCTET STRING');       // 3
  SL.Add('NULL');               // 4
  SL.Add('OBJECT');             // 5
  SL.Add('REAL');               // 6
  SL.Add('ENUMERATED');         // 7
  SL.Add('UTF8STRING');         // 8
  SL.Add('SEQUENCE');           // 9
  SL.Add('SEQUENCE OF');        // 10
  SL.Add('SET');                // 11
  SL.Add('SET OF');             // 12
  SL.Add('PrintableString');    // 13
  SL.Add('UTCTime');            // 14
  SL.Add('GeneralizedTime');    // 15
  SL.Add('VisibleString');      // 16
  SL.Add('BMPString');          // 17
  SL.Add('CHOICE');             // 18
  SL.Add('TYPE-IDENTIFIER');    // 19
end;

type
  THack = class(TASN1Struct);

procedure TfrmASN1Editor.SetStruct(const Value: TASN1Struct);
begin
  if Value = FStruct then Exit;
  if Assigned(FStruct) then
    THack(FStruct)._Release;
  FStruct := Value;
  if Assigned(Value) then begin
    THack(Value)._AddRef;
    Display;
  end;
end;

procedure TfrmASN1Editor.lvTypeIdentifiersData(Sender: TObject;
  Item: TListItem);
var
  Idx: Integer;
begin
  if not FCurrent^.TypeIdentified then Exit;

  Idx := Item.Index;
  Item.Caption := IntToStr(Idx);
  Item.SubItems.Add(FCurrent^.Choices[Idx]^.TypeName);
  Item.SubItems.Add(GetObjectName(FCurrent^.Choices[Idx]^.IdentifiedBy));
end;

procedure TfrmASN1Editor.DisplayBoolean;
begin
  if FCurrent^.IsEmpty then
    cbBooleanValue.ItemIndex := 0
  else if FCurrent^.ContentAsBoolean then
    cbBooleanValue.ItemIndex := 1
  else
    cbBooleanValue.ItemIndex := 2;
  PageControl1.ActivePageIndex := 4;
end;

procedure TfrmASN1Editor.DisplayDateTime;
var
  S: TSystemTime;
begin
  if FCurrent^.IsEmpty then begin
    edtYear.Text := '';
    edtMonth.Text := '';
    edtDay.Text := '';
    edtHour.Text := '';
    edtMinute.Text := '';
    edtSeconds.Text := '';
    edtMilliSeconds.Text := '';
  end else begin
    DateTimeToSystemTime(FCurrent^.ContentAsDateTime,S);
    edtYear.Text := IntToStr(S.wYear);
    edtMonth.Text := IntToStr(S.wMonth);
    edtDay.Text := IntToStr(S.wDay);
    edtHour.Text := IntToStr(S.wHour);
    edtMinute.Text := IntToStr(S.wMinute);
    edtMilliseconds.Text := IntToStr(S.wMilliseconds);
  end;

  if ((FCurrent^.Cls = V_ASN1_UNIVERSAL) and
      (FCurrent^.Tag = V_ASN1_UTCTIME)) or
     (FCurrent^.Implicit and
      (FCurrent^.ImplicitTag = V_ASN1_UTCTIME)) then begin
    edtMilliseconds.Enabled := False;
    lblMilliseconds.Enabled := False;
  end else begin
    edtMilliseconds.Enabled := True;
    lblMilliseconds.Enabled := True;
  end;
  PageControl1.ActivePageIndex := 7;
end;

procedure TfrmASN1Editor.DisplayHex;
begin
  memHex.Text := OSToHex(FCurrent^.ContentAsOctetString);
  PageControl1.ActivePageIndex := 5;
end;

procedure TfrmASN1Editor.DisplayInteger;
var
  X: PMPInteger;
begin
  if rgShowInteger.ItemIndex = 0 then
    memInteger.Text := OSToHex(FCurrent^.ContentAsOctetString)
  else begin
    X := nil;
    try
      case rgShowInteger.ItemIndex of
        1:
          begin
            FCurrent^.ContentAsMPInt(X);
            memInteger.Text := MPIntToBase10(X);
          end;
        2:
          begin
            FCurrent^.ContentAsUMPInt(X);
            memInteger.Text := MPIntToBase10(X);
          end;
      end;
    finally
      MPDealloc(X);
    end;
  end;
  PageControl1.ActivePageIndex := 6;
end;

procedure TfrmASN1Editor.DisplayReal;
begin
  edtRealContent.Text := FloatToStr(FCurrent^.ContentAsReal);
  PageControl1.ActivePageIndex := 8;
end;

procedure TfrmASN1Editor.DisplayText;
begin
  if chbViewAsHex.Checked then
    memText.Text := OSToHex(FCurrent^.ContentAsOctetString)
  else
    memText.Text := FCurrent^.DisplayContent;
  PageControl1.ActivePageIndex := 3;
end;

procedure TfrmASN1Editor.rgShowIntegerClick(Sender: TObject);
begin
  DisplayInteger;
end;

procedure TfrmASN1Editor.btnApplyIntegerClick(Sender: TObject);
var
  S: string;
  X: PMPInteger;
begin
  X := nil;
  try
    case rgShowInteger.ItemIndex of
      0:
        begin
          S := HexToOS(memInteger.Text);
          FCurrent^.SetContent(Pointer(S)^,Length(S));
        end;
      1:
        begin
          if not Base10ToMPInt(X,memInteger.Text) then
            raise Exception.Create('Illegal format');
          FCurrent^.EditContent(X,False);
        end;
      2:
        begin
          if not Base10ToMPInt(X,memInteger.Text) then
            raise Exception.Create('Illegal format');
          if X^.Sign < 0 then
            raise Exception.Create('Cannot store negative values as unsigned');
          FCurrent^.EditContent(X,True);
        end;
    end;
  finally
    MPDealloc(X);
  end;
end;

procedure TfrmASN1Editor.btnApplyTextClick(Sender: TObject);
var
  S: string;
begin
  if chbViewAsHex.Checked then begin
    S := HexToOS(memText.Text);
    FCurrent^.SetContent(Pointer(S)^,Length(S));
  end else
    FCurrent^.EditContent(memText.Text);
end;

procedure TfrmASN1Editor.cbBooleanValueChange(Sender: TObject);
begin
  case cbBooleanValue.ItemIndex of
    0: FCurrent^.SetContent(nil^,0);
    1: FCurrent^.EditContent(True);
    2: FCurrent^.EditContent(False);
  end;
end;

procedure TfrmASN1Editor.btnApplyDateClick(Sender: TObject);
var
  S: TSystemTime;
begin
  if edtYear.Text = '' then
    FCurrent^.SetContent(nil^,0)
  else begin
    FillChar(S,SizeOf(S),0);
    S.wYear := StrToInt(edtYear.Text);
    S.wMonth := StrToInt(edtMonth.Text);
    S.wDay := StrToInt(edtDay.Text);
    S.wHour := StrToInt(edtHour.Text);
    S.wMinute := StrToInt(edtMinute.Text);
    S.wSecond := StrToInt(edtSeconds.Text);
    S.wMilliseconds := StrToInt(edtMilliseconds.Text);
    FCurrent^.EditContent(SystemTimeToDateTime(S));
  end;
end;

procedure TfrmASN1Editor.btnApplyHexClick(Sender: TObject);
begin
  FCurrent^.EditContent(memHex.Text);
end;

procedure TfrmASN1Editor.btnApplyRealClick(Sender: TObject);
begin
  FCurrent^.EditContent(edtRealContent.Text);
end;

procedure TfrmASN1Editor.lvChoicesData(Sender: TObject; Item: TListItem);
var
  Idx: Integer;
begin
  if FCurrent^.ChoiceCount = 0 then Exit;

  Idx := Item.Index;
  Item.Caption := IntToStr(Idx);
  Item.SubItems.Add(FCurrent^.Choices[Idx]^.VarName);
  Item.SubItems.Add(FCurrent^.Choices[Idx]^.TypeName);
end;

procedure TfrmASN1Editor.btnLoadClick(Sender: TObject);
var
  FS: TFileStream;
begin
  if OpenDialog1.Execute then begin
    FS := TFileStream.Create(OpenDialog1.FileName,fmOpenRead);
    try
      if FStruct = nil then
        Struct := TASN1Struct.Create;
      case OpenDialog1.FilterIndex of
        1: FStruct.LoadFromStream(FS,fmtASN1);
        2,
        4: FStruct.LoadFromStream(FS,fmtDER);
        3: FStruct.LoadFromStream(FS,fmtDOM);
      end;
    finally
      FS.Free;
    end;
    Display;
  end;
end;

procedure TfrmASN1Editor.btnSaveClick(Sender: TObject);
var
  FS: TFileStream;
begin
  if SaveDialog1.Execute then begin
    FS := TFileStream.Create(SaveDialog1.FileName,fmCreate);
    try
      FStruct.ResetStreaming;
      case SaveDialog1.FilterIndex of
        1: FStruct.SaveToStream(FS,fmtASN1);
        2,
        4: FStruct.SaveToStream(FS,fmtDER);
        3: FStruct.SaveToStream(FS,fmtDOM);
      end;
    finally
      FS.Free;
    end;
  end;
end;

procedure TfrmASN1Editor.Display;
begin
  TreeView1.Items.Clear;
  CreateNodes;
  FCurrent := @FStruct;
  btnSave.Enabled := True;
  DisplayCurrent;
end;

procedure TfrmASN1Editor.btnNewItemClick(Sender: TObject);
begin
  if FStruct = nil then begin
    Struct := TASN1Struct.Create;
  end else begin
    FCurrent := FCurrent^.Owner.AddField;
    TreeView1.Items.Clear;
    CreateNodes;
  end;
  DisplayCurrent;
end;

procedure TfrmASN1Editor.btnNewSubItemClick(Sender: TObject);
begin
  FCurrent := FCurrent^.AddField;
  TreeView1.Items.Clear;
  CreateNodes;
  DisplayCurrent;
end;

procedure TfrmASN1Editor.btnDeleteClick(Sender: TObject);
var
  NextNode: TTreeNode;
  Idx: Integer;
begin
  if FCurrent^.Constructed and (FCurrent^.ItemCount > 0) then
    raise Exception.Create('Cannot delete items with sub items');
  NextNode := TreeView1.Selected.getNextSibling;
  if NextNode = nil then begin
    NextNode := TreeView1.Selected.getPrevSibling;
    if NextNode = nil then
      NextNode := TreeView1.Selected.Parent;
  end;
  TreeView1.Items.BeginUpdate;
  try
    TreeView1.Selected.Delete;
    if FCurrent^.Owner = nil then begin
      FCurrent := nil;
      Struct := nil;
    end else begin
      Idx := FCurrent^.Owner.IndexOf(FCurrent^);
      FCurrent^.Owner.DeleteItem(Idx);
      FCurrent := nil;
      NextNode.Selected := True;
    end;
  finally
    TreeView1.Items.EndUpdate;
  end;
  if FStruct = nil then
    btnSave.Enabled := False
  else
    Display;
end;

procedure TfrmASN1Editor.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  if Node = nil then
    SelectNil
  else begin
    FCurrent := Node.Data;
    DisplayCurrent;
  end;
end;

procedure TfrmASN1Editor.SelectNil;
begin
  FCurrent := nil;
  btnDelete.Enabled := False;
  btnNewSubItem.Enabled := False;

  cbDefTypeName.Enabled := False;
  cbCls.Enabled := False;
  meTag.Enabled := False;
  chbImplicit.Enabled := False;
  edtVarName.Enabled := False;
  cbTypeName.Enabled := False;
  chbOptional.Enabled := False;
  chbHasDefaultValue.Enabled := False;
  btnApplyType.Enabled := False;

  edtPath.Text := '';

  PageControl1.ActivePageIndex := 0;
end;

procedure TfrmASN1Editor.FormCreate(Sender: TObject);
begin
  ListDefaultTypeNames;
  SelectNil;
end;

procedure TfrmASN1Editor.edtVarNameChange(Sender: TObject);
begin
  if (FCurrent^ <> FStruct) or (edtVarName.Text <> '') then
    TreeView1.Selected.Text := edtVarName.Text;
  FCurrent^.VarName := edtVarName.Text;
  edtPath.Text := FCurrent^.GetNamePath;
end;

procedure TfrmASN1Editor.CreateNodes;
var
  Node: TTreeNode;
begin
  TreeView1.Items.BeginUpdate;
  try
    if FStruct.VarName = '' then
      Node := TreeView1.Items.AddObject(nil,'(root)',@FStruct)
    else
      Node := TreeView1.Items.AddObject(nil,FStruct.VarName,@FStruct);
    if @FStruct = FCurrent then
      Node.Selected := True;
    CreateChildNodes(Node);
    Node.Selected := True;
  finally
    TreeView1.Items.EndUpdate;
  end;
end;

procedure TfrmASN1Editor.CreateChildNodes(Node: TTreeNode);
var
  F, E: PASN1Struct;
  I: Integer;
  SubNode: TTreeNode;
begin
  F := Node.Data;
  if F^.Constructed then begin
    for I := 0 to F^.ItemCount - 1 do begin
      E := F^.Items[I];
      SubNode := Node.Owner.AddChildObject(Node,E^.VarName,E);
      if E = FCurrent then
        SubNode.Selected := True;
      SubNode.MakeVisible;
      CreateChildNodes(SubNode);
    end;
  end;
end;

procedure TfrmASN1Editor.meTagChange(Sender: TObject);
var
  Str: string;
begin
  Str := Trim(meTag.Text);
  if Str <> '' then begin
    FCurrent^.Tag := StrToInt(Str);
    DisplayCurrent;
  end;
end;

procedure TfrmASN1Editor.cbClsChange(Sender: TObject);
begin
  FCurrent^.Cls := 64 * cbCls.ItemIndex;
  DisplayCurrent;
end;

procedure TfrmASN1Editor.chbOptionalClick(Sender: TObject);
begin
  FCurrent^.Optional := chbOptional.Checked;
  DisplayCurrent;
end;

procedure TfrmASN1Editor.chbHasDefaultValueClick(Sender: TObject);
begin
  if chbHasDefaultValue.Checked then
    FCurrent^.SetAsDefault
  else begin
    FCurrent^.HasDefaultValue := False;
    FCurrent^.Default := '';
  end;
  if FCurrent^.Constructed then
    Display
  else
    DisplayCurrent;
end;

procedure TfrmASN1Editor.chbImplicitClick(Sender: TObject);
begin
  FCurrent^.Implicit := chbImplicit.Checked;
  DisplayCurrent;
end;

procedure TfrmASN1Editor.cbDefTypeNameClick(Sender: TObject);
begin
  FCurrent^.Persistent := False;
  FCurrent^.Cls := V_ASN1_UNIVERSAL;
  case cbDefTypeName.ItemIndex of
    0: FCurrent^.Tag := V_ASN1_BOOLEAN;
    1: FCurrent^.Tag := V_ASN1_INTEGER;
    2: FCurrent^.Tag := V_ASN1_BIT_STRING;
    3: FCurrent^.Tag := V_ASN1_OCTET_STRING;
    4: FCurrent^.Tag := V_ASN1_NULL;
    5: FCurrent^.Tag := V_ASN1_OBJECT;
    6: FCurrent^.Tag := V_ASN1_REAL;
    7: FCurrent^.Tag := V_ASN1_ENUMERATED;
    8: FCurrent^.Tag := V_ASN1_UTF8STRING;
    9,
    10: FCurrent^.Tag := V_ASN1_SEQUENCE;
    11,
    12: FCurrent^.Tag := V_ASN1_SET;
    13: FCurrent^.Tag := V_ASN1_PRINTABLESTRING;
    14: FCurrent^.Tag := V_ASN1_UTCTIME;
    15: FCurrent^.Tag := V_ASN1_GENERALIZEDTIME;
    16: FCurrent^.Tag := V_ASN1_VISIBLESTRING;
    17: FCurrent^.Tag := V_ASN1_BMPSTRING;
    18:
      begin
        PageControl1.ActivePageIndex := 1;
        Exit;
      end;
    19:
      begin
        PageControl1.ActivePageIndex := 2;
        FCurrent^.TypeIdentified := True;
        Exit;
      end;
  end;
  FCurrent^.Constructed := cbDefTypeName.ItemIndex in [9,10,11,12];
  FCurrent^.Persistent := False;
  if (cbDefTypeName.ItemIndex in [10,12]) and (FCurrent^.Template = nil) then
    FCurrent^.CreateOFTemplate;
  DisplayCurrent;
end;

procedure TfrmASN1Editor.btnAddChoiceClick(Sender: TObject);
var
  Dlg: TfrmChoiceEditor;
  CVarName, CTypeName: string;
  I: Integer;
  List: TStringList;
  S: string;
  F: PASN1Struct;
begin
  CVarName := '';
  CTypeName := '';
  Dlg := TfrmChoiceEditor.Create(nil);
  try
    ListTypeNames(Dlg.cbTypeName.Items);
    if Dlg.ShowModal = mrOK then begin
      CVarName := Dlg.edtVarName.Text;
      CTypeName := Dlg.cbTypeName.Text;
      FCurrent^.AddChoice(CVarName,CTypeName);
      Dlg.Choice := FCurrent^.Choices[FCurrent^.ChoiceCount - 1];
      List := TStringList.Create;
      try
        List.Sorted := True;
        for I := 0 to FCurrent^.ChoiceCount - 2 do begin
          S := IntToStr(FCurrent^.Choices[I]^.Cls) +
               IntToStr(FCurrent^.Choices[I]^.Tag);
          List.Add(S);
        end;
        I := FCurrent^.ChoiceCount - 1;
        S := IntToStr(FCurrent^.Choices[I]^.Cls) +
             IntToStr(FCurrent^.Choices[I]^.Tag);
        if List.IndexOf(S) >= 0 then begin
          FCurrent^.ChoiceCount := FCurrent^.ChoiceCount - 1;
          raise Exception.Create('The Cls and Tag you selected are already in the Choice list');
        end;
      finally
        List.Free;
      end;
      if Dlg.cbTypeName.ItemIndex >= 0 then begin
        F := Pointer(Dlg.cbTypeName.Items.Objects[Dlg.cbTypeName.ItemIndex]);
        Dlg.Choice^.CopyTypeInfo(F^);
        Dlg.Choice^.VarName := CVarName;
      end;
    end;
  finally
    Dlg.Free;
  end;
  DisplayCurrent;
end;

procedure ListSubItemTypeNames(F: PASN1Struct; List: TStrings);
var
  I: Integer;
  Str: string;
begin
  Str := F^.TypeName;
  if List.IndexOf(Str) < 0 then
    List.AddObject(Str,TObject(F));
  if F^.Constructed then
    for I := 0 to F^.ItemCount - 1 do
      ListSubItemTypeNames(F^.Items[I],List);
end;

procedure TfrmASN1Editor.ListTypeNames(List: TStrings);
begin
  List.Clear;
  ListSubItemTypeNames(@FStruct,List);
end;

procedure TfrmASN1Editor.cbTypeNameDropDown(Sender: TObject);
begin
  ListTypeNames(cbTypeName.Items);
end;

procedure TfrmASN1Editor.btnApplyTypeClick(Sender: TObject);
var
  S: string;    
  F: PASN1Struct;
  MS: TMemoryStream;
begin                 
  if cbTypeName.ItemIndex >= 0 then begin
    F := Pointer(cbTypeName.Items.Objects[cbTypeName.ItemIndex]);
    if (F <> FCurrent) and (F^ <> FStruct) and (FCurrent^ <> FStruct) and
       (Pos(FCurrent^.GetNamePath,F^.GetNamePath) = 0) and
       (Pos(F^.GetNamePath,FCurrent^.GetNamePath) = 0) then begin
      FCurrent^.CopyTypeInfo(F^);
      FCurrent^.TypeName := cbTypeName.Text;
    end else if (F <> FCurrent) then begin
      MS := TMemoryStream.Create;
      try
        F^.SaveToStream(MS,fmtASN1);
        MS.Position := 0;
        FCurrent^.LoadFromStream(MS,fmtASN1);
      finally
        MS.Free;
      end;
    end;
  end else begin
    S := cbTypeName.Text;
    FCurrent^.Persistent := True;
    FCurrent^.TypeName := S;
  end;
  DisplayCurrent;
end;

procedure TfrmASN1Editor.btnEditTemplateClick(Sender: TObject);
var
  F: TASN1Struct;
  Frm: TfrmASN1Editor;
begin
  F := TASN1Struct.Create;
  try
    F.Assign(FCurrent^.Template);
    Frm := TfrmASN1Editor.Create(nil);
    try
      Frm.Visible := False;
      Frm.Struct := F;
      if Frm.ShowModal = mrOK then
        FCurrent^.Template.Assign(Frm.Struct);
    finally
      Frm.Free;
    end;
  finally
    F.Free;
  end;
end;

procedure TfrmASN1Editor.lvChoicesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  btnEditChoice.Enabled := Selected;
  btnDeleteChoice.Enabled := Selected;
  btnTypeChoice.Enabled := Selected;
end;

procedure TfrmASN1Editor.btnDeleteChoiceClick(Sender: TObject);
begin
  FCurrent^.DeleteChoice(lvChoices.Selected.Index);
  DisplayCurrent;
end;

procedure TfrmASN1Editor.btnEditChoiceClick(Sender: TObject);
var
  Idx: Integer;
  Dlg: TfrmChoiceEditor;
  OldVarName, OldTypeName, CVarName, CTypeName: string;
  I: Integer;
  List: TStringList;
  S: string;
  F: PASN1Struct;
begin
  Idx := lvChoices.Selected.Index;
  CVarName := '';
  CTypeName := '';
  Dlg := TfrmChoiceEditor.Create(nil);
  try
    ListTypeNames(Dlg.cbTypeName.Items);
    Dlg.Choice := FCurrent^.Choices[Idx];
    if Dlg.ShowModal = mrOK then begin
      CVarName := Dlg.edtVarName.Text;
      CTypeName := Dlg.cbTypeName.Text;
      OldVarName := Dlg.Choice^.VarName;
      OldTypeName := Dlg.Choice^.VarName;
      Dlg.Choice^.VarName := CVarName;
      Dlg.Choice^.TypeName := CTypeName;
      List := TStringList.Create;
      try
        List.Sorted := True;
        for I := 0 to FCurrent^.ChoiceCount - 1 do
          if I <> Idx then begin
            S := IntToStr(FCurrent^.Choices[I]^.Cls) +
                 IntToStr(FCurrent^.Choices[I]^.Tag);
            List.Add(S);
          end;
        S := IntToStr(Dlg.Choice^.Cls) +
             IntToStr(Dlg.Choice^.Tag);
        if List.IndexOf(S) >= 0 then begin
          Dlg.Choice^.VarName := OldVarName;
          Dlg.Choice^.TypeName := OldTypeName;
          raise Exception.Create('The Cls and Tag you selected are already in the Choice list');
        end;
      finally
        List.Free;
      end;
      if Dlg.cbTypeName.ItemIndex >= 0 then begin
        F := Pointer(Dlg.cbTypeName.Items.Objects[Dlg.cbTypeName.ItemIndex]);
        Dlg.Choice^.CopyTypeInfo(F^);
        Dlg.Choice^.VarName := CVarName;
      end;
    end;
  finally
    Dlg.Free;
  end;
  DisplayCurrent;
end;

procedure TfrmASN1Editor.edtSelectedChoiceChange(Sender: TObject);
begin
  if edtSelectedChoice.Text <> '' then
    FCurrent^.SelectChoice(StrToInt(edtSelectedChoice.Text));
end;

procedure TfrmASN1Editor.btnTypeChoiceClick(Sender: TObject);
var
  Idx: Integer;
  F: TASN1Struct;
  Frm: TfrmASN1Editor;
begin
  Idx := lvChoices.Selected.Index;
  F := TASN1Struct.Create;
  try
    F.Assign(FCurrent^.Choices[Idx]^);
    Frm := TfrmASN1Editor.Create(nil);
    try
      Frm.Visible := False;
      Frm.Struct := F;
      if Frm.ShowModal = mrOK then
        FCurrent^.Choices[Idx]^.Assign(Frm.Struct);
    finally
      Frm.Free;
    end;
  finally
    F.Free;
  end;
  DisplayCurrent;
end;

procedure TfrmASN1Editor.btnEditAsStructClick(Sender: TObject);
var
  F: TASN1Struct;
  Frm: TfrmASN1Editor;
begin
  F := nil;
  try
    FCurrent^.ContentAsASN1Struct(F);
    Frm := TfrmASN1Editor.Create(nil);
    try
      Frm.Visible := False;
      Frm.Struct := F;
      if Frm.ShowModal = mrOK then begin
        if FCurrent.Encapsulated = nil then
          FCurrent.CreateEncapsulated;
        FCurrent^.Encapsulated.Assign(F);
        FCurrent^.EditContent(F);
        DisplayHex;
      end;
    finally
      Frm.Free;
    end;
  finally
    F.Free;
  end;
end;

procedure TfrmASN1Editor.btnDeleteIdentifierClick(Sender: TObject);
begin
  FCurrent^.DeleteChoice(lvTypeIdentifiers.Selected.Index);
  DisplayCurrent;
end;

procedure TfrmASN1Editor.lvTypeIdentifiersSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  btnEditIdentifier.Enabled := Selected;
  btnDeleteIdentifier.Enabled := Selected;
  btnTypeIdentifier.Enabled := Selected;
end;

procedure TfrmASN1Editor.btnTypeIdentifierClick(Sender: TObject);
var
  Idx: Integer;
  F: TASN1Struct;
  Frm: TfrmASN1Editor;
begin
  Idx := lvTypeIdentifiers.Selected.Index;
  F := TASN1Struct.Create;
  try
    F.Assign(FCurrent^.Choices[Idx]^);
    Frm := TfrmASN1Editor.Create(nil);
    try
      Frm.Visible := False;
      Frm.Struct := F;
      if Frm.ShowModal = mrOK then
        FCurrent^.Choices[Idx]^.Assign(Frm.Struct);
    finally
      Frm.Free;
    end;
  finally
    F.Free;
  end;
  DisplayCurrent;
end;

procedure TfrmASN1Editor.btnAddIdentifierClick(Sender: TObject);
var
  Dlg: TfrmTypeIdentifierEditor;
  Decl, OName, CObject, CTypeName: string;
  I: Integer;
  List: TStringList;
  S: string;
  C, F: PASN1Struct;
begin
  CObject := '';
  CTypeName := '';
  Dlg := TfrmTypeIdentifierEditor.Create(nil);
  try
    ListTypeNames(Dlg.cbTypeName.Items);
    GetOIDDeclarations(Dlg.cbObjectIdentifier.Items);
    if Dlg.ShowModal = mrOK then begin
      CTypeName := Dlg.cbTypeName.Text;
      Decl := Dlg.cbObjectIdentifier.Text;
      OName := '';
      CObject := '';
      RegisterOID(CObject,OName,Decl);

//      ShowMessage('Debug 1');

      C := FCurrent^.AddChoice('',CTypeName);
      C^.IdentifiedBy := CObject;
//      ShowMessage('Debug 2');
      if FCurrent^.ChoiceCount > 1 then begin
        List := TStringList.Create;
        try
          List.Sorted := True;
          for I := 0 to FCurrent^.ChoiceCount - 2 do begin
            S := FCurrent^.Choices[I]^.IdentifiedBy;
            List.Add(S);
          end;
          if List.IndexOf(CObject) >= 0 then begin
            FCurrent^.ChoiceCount := FCurrent^.ChoiceCount - 1;
            raise Exception.Create('The object identifier you selected is already in the list');
          end;
        finally
          List.Free;
        end;
      end;
      I := Dlg.cbTypeName.ItemIndex;
      if (I >= 0) and (Dlg.cbTypeName.Text = Dlg.cbTypeName.Items[I]) then begin
        F := Pointer(Dlg.cbTypeName.Items.Objects[I]);
        C^.CopyTypeInfo(F^);
        C^.IdentifiedBy := CObject;
      end;
    end;
  finally
    Dlg.Free;
  end;
//  ShowMessage('Debug 3');
  DisplayCurrent;
end;

procedure TfrmASN1Editor.btnEditIdentifierClick(Sender: TObject);
var
  Idx: Integer;
  Dlg: TfrmTypeIdentifierEditor;
  Decl, OName, OldObject, OldTypeName, CObject, CTypeName: string;
  I: Integer;
  List: TStringList;
  S: string;
  F: PASN1Struct;
begin
  Idx := lvTypeIdentifiers.Selected.Index;
  CObject := '';
  CTypeName := '';
  Dlg := TfrmTypeIdentifierEditor.Create(nil);
  try
    ListTypeNames(Dlg.cbTypeName.Items);
    GetOIDDeclarations(Dlg.cbObjectIdentifier.Items);
    Dlg.Choice := FCurrent^.Choices[Idx];
    if Dlg.ShowModal = mrOK then begin
      CTypeName := Dlg.cbTypeName.Text;
      Decl := Dlg.cbObjectIdentifier.Text;
      CObject := '';
      OName := '';
      RegisterOID(CObject,OName,Decl);
                              
      OldObject := Dlg.Choice^.IdentifiedBy;
      OldTypeName := Dlg.Choice^.TypeName;
      if (CObject <> OldObject) or (CTypeName <> OldTypeName) then begin
        Dlg.Choice^.IdentifiedBy := CObject;
        List := TStringList.Create;
        try
          List.Sorted := True;
          for I := 0 to FCurrent^.ChoiceCount - 1 do
            if I <> Idx then begin
              S := FCurrent^.Choices[I]^.IdentifiedBy;
              List.Add(S);
            end;
          S := Dlg.Choice^.IdentifiedBy;
          if List.IndexOf(S) >= 0 then begin
            Dlg.Choice^.IdentifiedBy := OldObject;
            raise Exception.Create('The object identifier you selected are already in the list');
          end;
        finally
          List.Free;
        end;
        if Dlg.cbTypeName.ItemIndex >= 0 then begin
          F := Pointer(Dlg.cbTypeName.Items.Objects[Dlg.cbTypeName.ItemIndex]);
          Dlg.Choice^.CopyTypeInfo(F^);
          Dlg.Choice^.IdentifiedBy := CObject;
        end else
          Dlg.Choice^.TypeName := CTypeName;
      end;
    end;
  finally
    Dlg.Free;
  end;
  DisplayCurrent;
end;

procedure TfrmASN1Editor.DisplayObject;
var
  F: PASN1Struct;
  I, J: Integer;
  Decl, OID: string;
begin
  lbObjectIdentifies.Clear;
  cbObject.Items.Clear;
  if THack(FCurrent^).Identifies <> nil then
    for J := 0 to THack(FCurrent^).Identifies.Count - 1 do begin
      F := FCurrent^.Owner.FindField(THack(FCurrent^).Identifies[J]);
      if Assigned(F) then begin
        lbObjectIdentifies.Items.Add(THack(FCurrent^).Identifies[J]);
        for I := 0 to F^.ChoiceCount - 1 do begin
          OID := F^.Choices[I]^.IdentifiedBy;
          Decl := GetOIDDeclaration(OID);
          cbObject.Items.Add(Decl);
        end;
      end;
    end
  else
    GetOIDDeclarations(cbObject.Items);

  if FCurrent^.IsEmpty then
    cbObject.Text := ''
  else
    cbObject.Text := GetOIDDeclaration(FCurrent^.ContentAsOID);
  PageControl1.ActivePageIndex := 10;
end;

procedure TfrmASN1Editor.cbObjectChange(Sender: TObject);
var
  OID, OName, Decl: string;
begin
  Decl := cbObject.Text;
  if Decl = '' then
    FCurrent^.SetContent(nil^,0)
  else begin
    OID := '';
    OName := '';
    RegisterOID(OID,OName,Decl);
    if OID <> '' then
      FCurrent^.EditContent(OID);
  end;
end;

procedure TfrmASN1Editor.btnViewClick(Sender: TObject);
var
  SS: TStringStream;
  Frm: TfrmASNModule;
begin
  if Assigned(FStruct) then begin
    SS := TStringStream.Create('');
    try
      FStruct.ResetStreaming;
      FStruct.SaveToStream(SS,fmtASN1);
      Frm := TfrmASNModule.Create(nil);
      try
        Frm.memASNModule.Text := SS.DataString;
        Frm.memASNModule.Modified := False;
        if Frm.ShowModal = mrOK then
          if Frm.memASNModule.Modified then begin
            SS.Size := 0;
            Frm.memASNModule.Lines.SaveToStream(SS);
            SS.Position := 0;
            FStruct.LoadFromStream(SS,fmtASN1);
            FModified := True;
            Display;
          end;
      finally
        Frm.Free;
      end;
    finally
      SS.Free;
    end;
  end;
end;

procedure TfrmASN1Editor.SetASNObject(const Value: TASNObject);
var
  F: TASN1Struct;
begin
  FASNObject := Value;
  if Assigned(Value) then begin
    if Value.Data = nil then
      SetStruct(nil)
    else begin
      F := TASN1Struct.Create;
      F.Assign(Value.Data);
      SetStruct(F);
    end;
  end;
end;

procedure TfrmASN1Editor.btnOKClick(Sender: TObject);
begin
  if Assigned(FASNObject) then begin
    if FStruct = nil then
      FASNObject.Data := nil
    else begin
      if FASNObject.Data = nil then
        FASNObject.Data := TASN1Struct.Create;
      FASNObject.Data.Assign(FStruct);
    end;
    FApplied := True;
  end;
end;

procedure TfrmASN1Editor.chbConstructedClick(Sender: TObject);
begin
  FCurrent^.Constructed := chbConstructed.Checked;
  if FCurrent^.Implicit then
    FCurrent^.ImplicitTag := V_ASN1_SEQUENCE;
  DisplayCurrent;
end;

procedure TfrmASN1Editor.cbIDFieldChange(Sender: TObject);
begin
  FCurrent^.IDField := cbIDField.Text;
end;

procedure TfrmASN1Editor.SetModified(const Value: Boolean);
begin
  FModified := Value;
end;

procedure TfrmASN1Editor.btnCancelClick(Sender: TObject);
begin
  if not FApplied then
    Modified := False;
end;

procedure TfrmASN1Editor.btnConvertClick(Sender: TObject);
begin
  if Assigned(FStruct) then
    GenerateUnit(FStruct);
end;

end.
