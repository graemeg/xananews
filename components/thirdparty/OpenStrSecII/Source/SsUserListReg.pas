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
{$I ver.inc}
unit SsUserListReg;

interface

uses
  SysUtils, Classes,
{$IFDEF D6UP}
  DesignIntf, DesignEditors,
{$ELSE}
  DsgnIntf,
{$ENDIF}
  SsUserList, SsUserListDB;

type
  TUserListFieldProperty = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TUserListProperty = class(TComponentProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  TypInfo, DB, MpX509, StreamSecII, TlsInternalServer;

procedure Register;
begin
  RegisterComponents('StreamSec',[TSsEventUserList,TSsDBUserList]);
  RegisterPropertyEditor(TypeInfo(TFieldName),TSsDBUserList,'',TUserListFieldProperty);
  RegisterPropertyEditor(TypeInfo(TComponent),TCustomSimpleTLSInternalServer,'UserList',TUserListProperty);
end;

{ TUserListFieldProperty }

function TUserListFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

function TUserListFieldProperty.GetEditLimit: Integer;
begin
  Result := 127;
end;

function TUserListFieldProperty.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TUserListFieldProperty.GetValues(Proc: TGetStrProc);
var
  UserList: TSsDBUserList;
  DataSet: TDataset;
  I: Integer;
begin
  UserList := (GetComponent(0) as TSsDBUserList);
  if Assigned(UserList.DataSource) then begin
    DataSet := UserList.DataSource.DataSet;
    for I := 0 to DataSet.FieldCount - 1 do
      Proc(DataSet.Fields[I].FieldName);
  end;
end;

procedure TUserListFieldProperty.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

{ TUserListProperty }

procedure TUserListProperty.GetValues(Proc: TGetStrProc);
var
  List: TStrings;
  I: Integer;
begin
  List := TStringList.Create;
  try
    Designer.GetComponentNames(GetTypeData(TypeInfo(TComponent)),List.Append);
    for I := 0 to List.Count - 1 do
      if Supports(Designer.GetComponent(List[I]),IUserList) then
        Proc(List[I]);
  finally
    List.Free;
  end;
end;

end.
