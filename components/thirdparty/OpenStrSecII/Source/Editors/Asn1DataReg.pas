{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     ASN1DataReg Unit                                  }
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
unit Asn1DataReg;

interface

uses
  Classes, Asn1Data,
  {$IFDEF MSWINDOWS}
  AsnEditorMain,
  {$ENDIF}
  {$IFDEF LINUX}
  QAsnEditorMain,
  {$ENDIF}
  {$IFDEF D6UP}
  DesignIntf, DesignEditors;
  {$ELSE}
  DsgnIntf;
  {$ENDIF}

type
  TASN1ObjectProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TASNFieldPathProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TASNDataEditor = class(TDefaultEditor)
  protected
    {$IFDEF D6UP}
    procedure EditProperty(const PropertyEditor: IProperty;
      var Continue: Boolean); override;
    {$ELSE}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); override;
    {$ENDIF}
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  {$IFDEF BETA}
  TASNVarNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;
  {$ENDIF}

  TASNChoiceTypeNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  SysUtils, Forms, Asn1,
  {$IFDEF BETA}
  ASNDB,
  {$ENDIF}
{$IFDEF MSWINDOWS}
  Controls
{$ENDIF}
{$IFDEF LINUX}
  Types, QControls
{$ENDIF};

procedure Register;
begin
  RegisterComponents('StreamSec',[TASNData]);
  {$IFDEF BETA}
  RegisterComponents('StreamSec',[TASNDBConverter,TASNClientData]);
  {$ENDIF}
  RegisterComponentEditor(TASNData, TASNDataEditor);
  RegisterPropertyEditor(TypeInfo(TASNObject), nil, '', TASN1ObjectProperty);
  RegisterPropertyEditor(TypeInfo(TASNFieldPath), nil, '', TASNFieldPathProperty);
  {$IFDEF BETA}
  RegisterPropertyEditor(TypeInfo(TASNVarName), TASNNestingChildRuleItem, '', TASNVarNameProperty);
  {$ENDIF}
  RegisterPropertyEditor(TypeInfo(TChoiceTypeName), TCustomChoice, '', TASNChoiceTypeNameProperty);
end;

{ TASN1ObjectProperty }

procedure TASN1ObjectProperty.Edit;
var
  ASNObject: TASNObject;
  ASNEditor: TfrmASN1Editor;
begin
  ASNObject := TASNObject(GetOrdValue);
  ASNEditor := TfrmASN1Editor.Create(Application);
  try
    ASNEditor.ASNObject := ASNObject;
    ASNEditor.ShowModal;
    if ASNEditor.Modified then
      Modified;
  finally
    ASNEditor.Free;
  end;
end;

function TASN1ObjectProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{ TASNDataEditor }

{$IFDEF D6UP}
procedure TASNDataEditor.EditProperty(const PropertyEditor: IProperty;
      var Continue: Boolean);
var
  TypeName: string;
begin
  TypeName := PropertyEditor.GetPropType.Name;
  if (CompareText(TypeName, 'TASNOBJECT') = 0) then begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;
{$ELSE}
procedure TASNDataEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
var
  TypeName: string;
begin
  TypeName := PropertyEditor.GetPropType.Name;
  if (CompareText(TypeName, 'TASNOBJECT') = 0) then begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;
{$ENDIF}

procedure TASNDataEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then Edit;
end;

function TASNDataEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'ASN Structure'
  else Result := '';
end;

function TASNDataEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TASNFieldPathProperty }

function TASNFieldPathProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect,paValueList,paRevertable];
end;

procedure TASNFieldPathProperty.GetValues(Proc: TGetStrProc);
var
  Owner: TPersistent;
  DataObj: TASNObject;
  I: Integer;
  SL: TStringList;
  P: string;
begin
  Owner := GetComponent(0);
  if Owner is TASNDataControlItem then
    DataObj := (TASNDataControlItem(Owner).Collection as TASNDataControls).Data
  else if Owner is TASNData then begin
    if TASNData(Owner).ASNData <> nil then
      DataObj := TASNData(Owner).ASNData.Data
    else
      DataObj := nil;
  end else
    DataObj := nil;
  if Assigned(DataObj) and Assigned(DataObj.Data) then begin
    SL := TStringList.Create;
    try
      DataObj.Data.ListFields(SL,True,True,True);
      for I := 0 to SL.Count - 1 do begin
        P := SL[I] + #0;
        Proc(P);
      end;
    finally
      SL.Free;
    end;
  end;
end;
              
{$IFDEF BETA}
{ TASNVarNameProperty }

function TASNVarNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect,paValueList,paRevertable];
end;

procedure TASNVarNameProperty.GetValues(Proc: TGetStrProc);
var
  Owner: TPersistent;
  Data: TASN1Struct;
  I: Integer;
  P: string;
begin
  Owner := GetComponent(0);
  Data := TASNNestingChildRuleItem(Owner).Data;
  if Assigned(Data) then begin
    Data := Data.Template;
    if Assigned(Data) then begin
      for I := 0 to Data.ItemCount - 1 do begin
        if Data.Items[I]^.Cls = V_ASN1_CONTEXT_SPECIFIC then
          Break;
        P := Data.Items[I]^.VarName + #0;
        Proc(P);
      end;
    end;
  end;
end;
{$ENDIF}

{ TASNChoiceTypeNameProperty }

function TASNChoiceTypeNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect,paValueList,paRevertable];
end;

type
  THack = class(TCustomChoice);

procedure TASNChoiceTypeNameProperty.GetValues(Proc: TGetStrProc);
var
  Owner: TPersistent;
  DataObj: THack;
  Idx: Integer;
begin
  Owner := GetComponent(0);
  DataObj := THack(Owner as TCustomChoice);
  for Idx := 0 to DataObj.GetChoiceCount - 1 do
    Proc(DataObj.GetChoiceTypeNames(Idx))
end;

end.
