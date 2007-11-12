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
unit AsnToPascal;

interface

{$WARNINGS OFF}
uses
  Classes, Asn1, EditIntf, ToolIntf;
{$WARNINGS ON}

type
  TASNToPascal = class
  private
    FSource: string;
    FUnitName: string;
    FStruct: TASN1Struct;
    FTypeMap: TStrings;
    FUsesList: TStrings;
    FDecl: TStrings;
    FPendDecl: TStrings;
    FPfx: TStrings;
    FDeclTree: TStrings;
    FIntf: TStrings;
    FRegs: TStrings;
    FImpl: TStrings;
    FGenerateComponent: Boolean;
    FUniqueItemDecl: string;
    FGetUniqueItemDecl: string;
    function ChoicesToEnumStr(Struct: TASN1Struct): string;
    procedure GenerateClass(Struct: TASN1Struct;
                            AsTopClass: Boolean;
                            AccessString: string = '');
    function GetPasTypeName(Struct: TASN1Struct;
                            var WrapClass: Boolean;
                            Inher: Boolean = False): string;
    function GetTypeMap(OwnerTypeName, VarName: string): string;
    function GetTypeName(Struct: TASN1Struct): string;
    procedure SetStruct(const Value: TASN1Struct);
    procedure SetTypeMap(OwnerTypeName, VarName: string; const Value: string);
    procedure SetUnitName(const Value: string);
    procedure SetCustomUsesList(const Value: TStrings);
    procedure SetGenerateComponent(const Value: Boolean);
    function GetWrapTypeMap(OwnerTypeName,
      VarName: string): TASNWrapperClass;
    procedure SetWrapTypeMap(OwnerTypeName, VarName: string;
      const Value: TASNWrapperClass);
    function FormatTypeName(Struct: TASN1Struct): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Generate;
    property GenerateComponent: Boolean read FGenerateComponent write SetGenerateComponent;
    property CustomUsesList: TStrings read FUsesList write SetCustomUsesList;
    property Source: string read FSource;
    property Struct: TASN1Struct read FStruct write SetStruct;
    property TypeMap[OwnerTypeName, VarName: string]: string read GetTypeMap write SetTypeMap;
    property UnitName: string read FUnitName write SetUnitName;
    property WrapTypeMap[OwnerTypeName, VarName: string]: TASNWrapperClass read GetWrapTypeMap write SetWrapTypeMap;
  end;
    
  TASNToPascalModule = class(TIModuleCreatorEx)
  private
    FConverter: TASNToPascal;
  public
    constructor Create(Struct: TASN1Struct);
    destructor Destroy; override;
    function Existing: Boolean; override;
    function GetAncestorName: string; override;
    function GetFileName: string; override;
    function GetFileSystem: string; override;
    function GetFormName: string; override;
    function NewModuleSource(const UnitIdent, FormIdent, AncestorIdent: string): string; override;
    procedure FormCreated(Form: TIFormInterface); override;
    function GetIntfName: string; override;
    function NewIntfSource(const UnitIdent, FormIdent, AncestorIdent: string): string; override;
  end;

procedure GenerateUnit(Struct: TASN1Struct);
function FormatModuleName(Name: string): string;

implementation

{$WARNINGS OFF}
uses
  SysUtils, Dialogs, PKIX, ExptIntf;
{$WARNINGS ON}

function FormatModuleName(Name: string): string;
var
  P: Integer;
begin
  P := Pos('{',Name);
  if P = 0 then
    Result := Trim(Name)
  else
    Result := Trim(Copy(Name,1,P-1));
  Result := StringReplace(Result,'-','_',[rfReplaceAll]);
  Result := StringReplace(Result,' ','_',[rfReplaceAll]);
end;

function FormatVarName(Name: string): string;
var
  I: Integer;
  PrevBlank, CanBeUpperCase, HasLowerCase: Boolean;
  C: Char;
begin
  if Copy(LowerCase(Name),1,2) = 'id' then
    Delete(Name,1,2);
  if Copy(Name,1,1) = '-' then
    Delete(Name,1,1);
  HasLowerCase := False;
  CanBeUpperCase := True;
  Result := '';
  PrevBlank := True;
  for I := 1 to Length(Name) do begin
    C := Name[I];
    if C in ['-','_'] then begin
      PrevBlank := True;
      HasLowerCase := True;
      CanBeUpperCase := True;
      Result := Result + '_';
    end else if C in ['a'..'z','0'..'9'] then begin
      HasLowerCase := True;
      if PrevBlank then
        Result := Result + UpperCase(C)
      else
        Result := Result + C;
      PrevBlank := False;
      CanBeUpperCase := True;
    end else if C in ['A'..'Z'] then begin
      if CanBeUpperCase then
        Result := Result + C
      else
        Result := Result + LowerCase(C);
      PrevBlank := False;
      CanBeUpperCase := HasLowerCase;
    end else begin
      HasLowerCase := False;
      CanBeUpperCase := True;
    end;
  end;
  if Result = '' then
    Result := '{ VarName missing }'
  else
    Result[1] := UpperCase(Result[1])[1];
end;

procedure GenerateUnit(Struct: TASN1Struct);
var
  NewModule: TASNToPascalModule;
begin
  NewModule := TASNToPascalModule.Create(Struct);
  try
    ToolServices.ModuleCreateEx(NewModule, [cmAddToProject, cmShowSource,
      cmUnNamed, cmNewFile]);
  finally
    NewModule.Free;
  end;
end;

function ListToStr(P: string; List: TStrings; Indent: Integer): string;
var
  I: Integer;
  S, N, Ind: string;
begin
  Ind := StringOfChar(' ',Indent);
  for I := 0 to List.Count - 1 do begin
    N := List[I];
    S := P + ', ' + N;
    if Length(S) > 79 then begin
      Result := Result + P + ','#13#10;
      P := Ind + N;
    end else
      P := S;
  end;
  Result := Result + P;
end;

function UsesListToStr(UsesList: TStrings; AsComponent: Boolean): string;
var
  P: string;
  I: Integer;
begin
  Result := '';
  I := UsesList.IndexOf('Classes');
  if I >= 0 then
    UsesList.Delete(I);
  I := UsesList.IndexOf('Asn1');
  if I >= 0 then
    UsesList.Delete(I);
  if AsComponent then begin
    I := UsesList.IndexOf('Asn1Data');
    if I >= 0 then
      UsesList.Delete(I);
    P := '  Classes, Asn1, Asn1Data'
  end else
    P := '  Classes, Asn1';
  Result := ListToStr(P,UsesList,2) + ';'#13#10#13#10;
end;

function GenerateModuleConstant(Struct: TASN1Struct): string;
var
  SS: TStringStream;
  SL: TStringList;
  I: Integer;
  S, P: string;
begin
  Result := 'const'#13#10 +
            '  crlf = #13#10;'#13#10 +
            '  ASNModule = '#13#10;
  SS := TStringStream.Create('');
  try
    Struct.SaveToStream(SS,fmtASN1);
    SL := TStringList.Create;
    try
      SS.Position := 0;
      SL.LoadFromStream(SS);
      S := '';
      for I := 0 to SL.Count - 1 do begin
        P := SL[I];
        if P = '' then
          S := S + ' + crlf'
        else begin
          if S <> '' then begin
            S := S + ' + ';
            Result := Result + S + #13#10;
          end;
          S := StringReplace(P,#9,'''#9''',[rfReplaceAll]);
          S := '    ''' + S + ''' + crlf';
        end;
      end;
      S := S + ';'#13#10;
      Result := Result + S + #13#10;
    finally
      SL.Free;
    end;
  finally
    SS.Free;
  end;
end;

type
  THack = class(TASN1Struct);

{ TASNToPascal }

constructor TASNToPascal.Create;
begin
  FUsesList := TStringList.Create;
  FPendDecl := TStringList.Create;
  FDecl := TStringList.Create;
  FTypeMap := TStringList.Create;
  FImpl := TStringList.Create;
  FIntf := TStringList.Create;
  FRegs := TStringList.Create;
  FPfx := TStringList.Create;
  FDeclTree := TStringList.Create;
end;

destructor TASNToPascal.Destroy;
begin
  FUsesList.Free;
  FPendDecl.Free;
  FDecl.Free;
  FTypeMap.Free;
  FImpl.Free;
  FIntf.Free;
  FRegs.Free;
  FPfx.Free;
  FDeclTree.Free;
  SetStruct(nil);
  inherited;
end;

procedure TASNToPascal.Generate;
var
  I: Integer;
begin
  FDecl.Clear;
  FPendDecl.Clear;
  FImpl.Clear;
  FIntf.Clear;
  FPfx.Clear;
  FDeclTree.Clear;

  for I := 0 to 30 do
    if WrapperUnivTypes[I] <> '' then
      FDecl.Add(WrapperUnivTypes[I]);
  for I := 0 to 30 do
    if MappedUnivTypes[I] <> '' then
      FDecl.Add(MappedUnivTypes[I]);


  GenerateClass(FStruct,True);

  FSource := Format('unit %s;'#13#10#13#10 +
                    'interface'#13#10#13#10 +
                    'uses'#13#10 +
                    '%s' +
                    '%s' +
                    'type'#13#10 +
                    '%s' +
                    'var'#13#10 +
                    '  %sFactories: IASNClassFactories = nil;'#13#10#13#10 +
                    'implementation'#13#10#13#10 +
                    'procedure RegisterFactories;'#13#10 +
                    'var'#13#10 +
                    '  SS: TStringStream;'#13#10 +
                    '  GlobalObject, F: TASN1Struct;'#13#10 +
                    '  Intf: IASNClassFactories;'#13#10 +
                    'begin'#13#10 +
                    '  SS := TStringStream.Create(ASNModule);'#13#10 +
                    '  try'#13#10 +
                    '    GlobalObject := TASN1Struct.Create;'#13#10 +
                    '    GlobalObject.LoadFromStream(SS,fmtASN1);'#13#10 +
                    '  finally'#13#10 +
                    '    SS.Free;'#13#10 +
                    '  end;'#13#10 +
                    '  Intf := TASNClassFactories.Create;'#13#10 +
                    '  %sFactories := Intf;'#13#10 +
                    '%s' +
                    'end;'#13#10 +
                    #13#10 +
                    'function CheckFactories: IASNClassFactories;'#13#10 +
                    'begin'#13#10 +
                    '  Result := %sFactories;'#13#10 +
                    '  if Result = nil then'#13#10 +
                    '    RegisterFactories;'#13#10 +  
                    '  Result := %sFactories;'#13#10 +
                    'end;'#13#10 +
                    '%s' +
                    'initialization'#13#10 +
                    '  RegisterFactories;'#13#10 +
                    'end.',
                    [FUnitName,
                     UsesListToStr(FUsesList,FGenerateComponent),
                     GenerateModuleConstant(FStruct),
                     FIntf.Text,
                     FUnitName,
                     FUnitName,
                     FRegs.Text,                      
                     FUnitName,
                     FUnitName,
                     FImpl.Text]);
end;

procedure TASNToPascal.GenerateClass(Struct: TASN1Struct; AsTopClass: Boolean;
  AccessString: string);
var
  I: Integer;
  F, G: PASN1Struct;
  E, Str, T: TASN1Struct;
  Explicit: Boolean;
  Constr, Destr, ClassImpl, Priv, Publ, Prop, Regs, Cls: TStringList;
  TypeName, PasTypeName, VarName, UTypeName,
  UniqueItemDecl, GetUniqueItemDecl,
  SubAS: string;
  WrapClass, Dummy, TypeIden, Unique: Boolean;
  Code: LongInt;

  procedure DoGenerateClass(SubStruct: TASN1Struct;
                            SubAccessString: string);
  var
    UN: string;
  begin
    if SubStruct.ModuleName = '' then
      SubStruct.ModuleName := Struct.ModuleName;
    if SubStruct.ModuleName <> Struct.ModuleName then begin
      UN := FormatModuleName(SubStruct.ModuleName);
      if FUsesList.IndexOf(UN) < 0 then
        FUsesList.Add(UN);
    end;
    GenerateClass(SubStruct,False,SubAccessString);
  end;

  function CheckModule(AStruct: TASN1Struct = nil): Boolean;
  begin
    if AStruct = nil then
      Result := FUnitName =
                FormatModuleName(Struct.ModuleName)
    else                                           
      Result := FUnitName =
                FormatModuleName(AStruct.ModuleName)
  end;

  procedure CheckExplicit(var Struct: PASN1Struct; var Explicit: Boolean);
  begin
    Explicit := Struct^.Constructed and
                (Struct^.ItemCount = 1);
    if Explicit then
      Struct := Struct^.Items[0];
  end;

  procedure InsertComment;
  var
    I: Integer;
    S: string;
  begin
    if not CheckModule then Exit;
    S := '    ';
    FIntf.Add('{ Declaration tree: ');
    for I := 0 to FDeclTree.Count - 2 do begin
      FIntf.Add(S + FDeclTree[I]);
      S := S + ' ';
    end;
    FIntf.Add(S + FDeclTree[FDeclTree.Count - 1] + '}');
    FIntf.Add('');
  end;

begin
  TypeIden := False;
  Unique := False;
  UniqueItemDecl := FUniqueItemDecl;
  GetUniqueItemDecl := FGetUniqueItemDecl;

  WrapClass := True;
  TypeName := GetPasTypeName(Struct,WrapClass);
  if (Struct.ChoiceCount = 0) and not Struct.Constructed then begin
    Dummy := True;
    PasTypeName := GetPasTypeName(Struct,Dummy,True);
    if TypeName = PasTypeName then
      Exit;
  end;
  FDeclTree.Add(TypeName);

  if (FPendDecl.IndexOf(GetTypeName(Struct)) >= 0) and CheckModule then begin
    FIntf.Add('  ' + TypeName + ' = class;');
    FIntf.Add('');
    FDecl.Add(GetTypeName(Struct));
  end else begin
    FPendDecl.Add(GetTypeName(Struct));
    Constr := TStringList.Create;
    try
      Destr := TStringList.Create;
      try
        ClassImpl := TStringList.Create;
        try
          Priv := TStringList.Create;
          try
            Publ := TStringList.Create;
            Prop := TStringList.Create;
            Cls := TStringList.Create;
            Regs := TStringList.Create;
            try
              if AsTopClass then begin
                Publ.Add('    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;');
                Constr.Add('constructor ' + TypeName + '.Create;');
                Constr.Add('begin');
                Constr.Add('  inherited;');

                Publ.Add('    destructor Destroy; override;');
                Destr.Add('destructor ' + TypeName + '.Destroy;');
                Destr.Add('begin');

                Regs.Add('  F := GlobalObject;');
              end else begin
                Publ.Add('    constructor Create(AOwner: TPersistent; AData: TASN1Struct; ATemplate: TASN1Struct = nil); override;');
                Constr.Add('constructor ' + TypeName + '.Create;');
                Constr.Add('begin');
                Constr.Add('  inherited;');

                Publ.Add('    destructor Destroy; override;');
                Destr.Add('destructor ' + TypeName + '.Destroy;');
                Destr.Add('begin');

                if AccessString <> '' then begin
                  Regs.Add('  F := GlobalObject' + AccessString + ';');
                  if Struct.GetNamePath <> '' then
                    Regs.Add(Format('  F := F.FindField(''%s'')^;',
                                    [Struct.GetNamePath]));
                end else
                  Regs.Add(Format('  F := GlobalObject.FindField(''%s'')^;',
                                  [Struct.GetNamePath]));
              end;

              Publ.Add('    class function GetClassFactories: IASNClassFactories; override;');
              ClassImpl.Add(Format('class function %s.GetClassFactories: IASNClassFactories;',[TypeName]));
              ClassImpl.Add('begin');
              ClassImpl.Add('  Result := CheckFactories;');
              ClassImpl.Add('end;');
              ClassImpl.Add('');

              if (Struct.ChoiceCount = 0) and
                 ((Struct.Encapsulated = nil) or not Struct.Encapsulated.Constructed) and
                 CheckModule and
                 not Struct.Constructed then begin
                FIntf.Add('  ' + TypeName + ' = class(' + PasTypeName + ')');
                Priv.Add('    { Add private declarations here }');
                Publ.Add('    { Add public declarations here }');
                Prop.Add('    { Add properties here }');
                FUniqueItemDecl := '';
              end else if Struct.ChoiceCount > 0 then begin
                Regs.Add('  Intf.RegisterChoiceClass(' + TypeName + ',F,');

                if Struct.TypeIdentified then
                  G := THack(Struct).FindIDField
                else
                  G := nil;
                for I := 0 to Struct.ChoiceCount - 1 do begin
                  F := Struct.Choices[I];
                  if (I > 0) and not (Struct.TypeIdentified) and
                     (F^.VarName = Struct.Choices[I-1].VarName) then
                    Continue;
                  if Struct.GetNamePath <> '' then
                    SubAS := Format(AccessString + '.FindField(''%s'')^.Choices[%d]^',[Struct.GetNamePath,I])
                  else
                    SubAS := Format(AccessString + '.Choices[%d]^',[I]);
                  if not F^.Constructed then begin
                    E := F^.Encapsulated;
                    Dummy := True;
                    if GetPasTypeName(F^,Dummy,True) = '' then begin
                      Cls.Add('nil');
                      Continue;
                    end;
                  end else
                    E := nil;
                  CheckExplicit(F,Explicit);
                  if Assigned(E) then begin
                    if (FDecl.IndexOf(GetTypeName(E)) < 0) and
                       (GetTypeMap(E.TypeName,'') = '') then
                      DoGenerateClass(F^,SubAS)
                  end else if (FDecl.IndexOf(GetTypeName(F^)) < 0) and
                          (GetTypeMap(F^.TypeName,'') = '') then
                    DoGenerateClass(F^,SubAS);

                  WrapClass := True;
                  Cls.Add(GetPasTypeName(F^,WrapClass));

                  if Explicit then
                    T := F^.Owner
                  else
                    T := F^;
                  if Struct.TypeIdentified then begin
                    if G = nil then begin
                      VarName := GetObjectName(T.IdentifiedBy);
                      if VarName[1] = '{' then
                        VarName := T.TypeName;
                    end else if G^.ActualTag = V_ASN1_OBJECT then
                      VarName := GetObjectName(T.IdentifiedBy)
                    else
                      VarName := T.TypeName;
                  end else if T.VarName <> '' then
                    VarName := T.VarName
                  else
                    VarName := T.TypeName;
                  VarName := FormatVarName(VarName);
                  if Assigned(E) then
                    F := @E;
                  WrapClass := False;
                  PasTypeName := GetPasTypeName(F^,WrapClass);
                  if (F^.ActualTag = V_ASN1_ENUMERATED) and
                     (PasTypeName <> 'Integer') and
                     CheckModule(F^) then begin
                    FIntf.Add(Format('  %s = ( { Enter values here } );',[PasTypeName]));
                    FIntf.Add('');
                  end;
                  Publ.Add(Format('    property As%s: %s read GetAs%s write SetAs%s;',
                                  [VarName,PasTypeName,VarName,VarName]));
                  Priv.Add(Format('    function GetAs%s: %s;',[VarName,PasTypeName]));
                  ClassImpl.Add(Format('function %s.GetAs%s: %s;',
                                       [TypeName,VarName,PasTypeName]));
                  ClassImpl.Add('begin');
                  Dummy := True;
                  if (F^.Constructed or
                      (F^.ChoiceCount > 0) or
                      WrapClass) then
                    ClassImpl.Add(Format('  Result := (FSelected as %s);',[PasTypeName]))
                  else if F^.ActualTag = V_ASN1_ENUMERATED then
                    ClassImpl.Add(Format('  Result := %s((FSelected as %s).Value);',
                                         [PasTypeName,GetPasTypeName(F^,Dummy)]))
                  else
                    ClassImpl.Add(Format('  Result := (FSelected as %s).Value;',
                                         [GetPasTypeName(F^,Dummy)]));
                  ClassImpl.Add('end;');
                  ClassImpl.Add('');
                  Priv.Add(Format('    procedure SetAs%s(const Value: %s);',[VarName,PasTypeName]));
                  ClassImpl.Add(Format('procedure %s.SetAs%s(const Value: %s);',[TypeName,VarName,PasTypeName]));
                  ClassImpl.Add('begin');
                  ClassImpl.Add(Format('  InternalSelectChoice(%d);',[I]));
                  if (F^.Constructed or
                      (F^.ChoiceCount > 0) or
                      WrapClass) then
                    ClassImpl.Add(Format('  (FSelected as %s).Assign(Value);',
                                         [PasTypeName,I]))
                  else if F^.ActualTag = V_ASN1_ENUMERATED then
                    ClassImpl.Add(Format('  (FSelected as %s).Value := Ord(Value);',
                                         ['TEnumeratedWrapper']))
                  else
                    ClassImpl.Add(Format('  (FSelected as %s).Value := Value;',
                                         [GetPasTypeName(F^,Dummy)]));
                  ClassImpl.Add('end;');
                  ClassImpl.Add('');
                  FUniqueItemDecl := '';
                end;
                if Cls.Count = 0 then
                  Regs.Add('    []);')
                else if Cls.Count = 1 then
                  Regs.Add('    [' + Cls[0] + ']);')
                else begin
                  Regs.Add('    [' + Cls[0] + ',');
                  for I := 1 to Cls.Count - 2 do
                    Regs.Add('     ' + Cls[I] + ',');
                  Regs.Add('     ' + Cls[Cls.Count - 1] + ']);');
                end;
                Constr.Add('  if FData.ChoiceVarName <> '''' then');
                Constr.Add('    Update;');

                Prop.Add(Format('    property Choice: %sEnum read GetChoice write SetChoice;',
                                [TypeName]));
                Priv.Add(Format('    function GetChoice: %sEnum;',[TypeName]));
                ClassImpl.Add(Format('function %s.GetChoice: %sEnum;',
                                     [TypeName,TypeName]));
                ClassImpl.Add('begin');
                ClassImpl.Add(Format('  Result := %sEnum(InternalGetChoice);',
                                     [TypeName]));
                ClassImpl.Add('end;');
                ClassImpl.Add('');
                Priv.Add(Format('    procedure SetChoice(const Value: %sEnum);',
                                [TypeName]));
                ClassImpl.Add(Format('procedure %s.SetChoice(const Value: %sEnum);',
                                     [TypeName,TypeName]));
                ClassImpl.Add('begin');
                ClassImpl.Add('  InternalSelectChoice(Ord(Value)-1);');
                ClassImpl.Add('end;');
                ClassImpl.Add('');

                InsertComment;

                if CheckModule then begin
                  FIntf.Add(ChoicesToEnumStr(Struct));
                  FIntf.Add('');
                  FIntf.Add('  ' + TypeName + ' = class(TASNChoiceWrapper)');
                end;
              end else if (Struct.Template <> nil) or
                          ((Struct.Encapsulated <> nil) and
                           (Struct.Encapsulated.Template <> nil)) then begin
                Regs.Add('  Intf.RegisterConstructedClass(' + TypeName + ',F,');

                if Struct.GetNamePath = '' then
                  SubAS := AccessString
                else
                  SubAS := AccessString + Format('.FindField(''%s'')^',[Struct.GetNamePath]);
                if Struct.Template = nil then begin
                  Str := Struct.Encapsulated;
                  SubAS := SubAS + '.Encapsulated';
                end else
                  Str := Struct;
                SubAS := SubAS + '.Template';

                WrapClass := True;
                PasTypeName := GetPasTypeName(Str.Template,WrapClass);

                G := nil;
                if Str.Template.Constructed then begin
                  if Str.Template.Encapsulated <> nil then begin
                    if (FDecl.IndexOf(GetTypeName(Str.Template)) < 0) and
                       (GetTypeMap(Str.Template.TypeName,'') = '') then
                      DoGenerateClass(Str.Template,SubAS);
                  end else if (FDecl.IndexOf(GetTypeName(Str.Template)) < 0) and
                              (GetTypeMap(Str.Template.TypeName,'') = '') then
                    DoGenerateClass(Str.Template,SubAS);
                  if Str.Template.ItemCount > 1 then begin
                    Dummy := True;
                    I := FDecl.IndexOf(GetTypeName(Str.Template));
                    if I < 0 then
                      Code := 0
                    else
                      Code := LongInt(FDecl.Objects[I]);
                    if Code = 1 then begin
                      if Str.Template.Items[0]^.Constructed then begin
                        if THack(Str.Template.Items[0]^.Items[0]^).Identifies <> nil then
                          G := Str.Template.FindField(THack(Str.Template.Items[0]^.Items[0]^).Identifies[0])
                      end else if THack(Str.Template.Items[0]^).Identifies <> nil then
                        G := Str.Template.FindField(THack(Str.Template.Items[0]^).Identifies[0]);
                      if Assigned(G) then begin
                        Unique := True;
                        Dummy := True;
                        UTypeName := GetPasTypeName(G^,Dummy);
                        UniqueItemDecl := Format('    property UniqueItem[Id: %sEnum]: %s read GetUniqueItem; default;',[UTypeName,PasTypeName]);
                        Prop.Add(UniqueItemDecl);
                        GetUniqueItemDecl := Format('UniqueItem(Id: %sEnum): %s;',
                                                    [UTypeName,PasTypeName]);
                        Priv.Add('    function Get' + GetUniqueItemDecl);
                        ClassImpl.Add(Format('function %s.GetUniqueItem(Id: %sEnum): %s;',
                                             [TypeName,UTypeName,PasTypeName]));
                        ClassImpl.Add('var');
                        ClassImpl.Add('  I: Integer;');
                        ClassImpl.Add('begin');
                        ClassImpl.Add('  for I := 0 to ItemCount - 1 do begin');
                        ClassImpl.Add('    Result := Items[I];');
                        ClassImpl.Add('    if Result.IsType(Id) then');
                        ClassImpl.Add('      Exit;');
                        ClassImpl.Add('  end;');
                        ClassImpl.Add('  Result := nil;');
                        ClassImpl.Add('end;');
                        ClassImpl.Add('');
                        Publ.Add(Format('    function AddUniqueItem(Id: %sEnum): %s;',
                                        [UTypeName,PasTypeName]));
                        ClassImpl.Add(Format('function %s.AddUniqueItem(Id: %sEnum): %s;',
                                             [TypeName,UTypeName,PasTypeName]));
                        ClassImpl.Add('begin');
                        ClassImpl.Add('  Result := GetUniqueItem(Id);');
                        ClassImpl.Add('  if Result = nil then begin');
                        ClassImpl.Add('    Result := Add;');
                        ClassImpl.Add('    Result.SetType(Id);');
                        ClassImpl.Add('  end;');
                        ClassImpl.Add('end;');
                        ClassImpl.Add('');
                      end;
                    end;
                  end;
                  if (G = nil) and (FUniqueItemDecl <> '') then begin
                    Prop.Add(FUniqueItemDecl);
                    Priv.Add('    function Get' + FGetUniqueItemDecl);
                    ClassImpl.Add(Format('function %s.Get%s',[TypeName,FGetUniqueItemDecl]));
                    ClassImpl.Add('var');
                    ClassImpl.Add('  I: Integer;');
                    ClassImpl.Add('begin');
                    ClassImpl.Add('  for I := 0 to ItemCount - 1 do begin');
                    ClassImpl.Add('    Result := Items[I].UniqueItem[Id];');
                    ClassImpl.Add('    if Assigned(Result) then');
                    ClassImpl.Add('      Exit;');
                    ClassImpl.Add('  end;');
                    ClassImpl.Add('  Result := nil;');
                    ClassImpl.Add('end;');
                    ClassImpl.Add('');
                    Publ.Add('    function Add' + FGetUniqueItemDecl);
                    ClassImpl.Add(Format('function %s.Add%s',[TypeName,FGetUniqueItemDecl]));
                    ClassImpl.Add('begin');
                    ClassImpl.Add('  Result := GetUniqueItem(Id);');
                    ClassImpl.Add('  if Result = nil then');
                    ClassImpl.Add('    Result := Add.AddUniqueItem(Id);');
                    ClassImpl.Add('end;');
                    ClassImpl.Add('');
                  end;
                end;

                Regs.Add('    [' + PasTypeName + ']);');
                Constr.Insert(Constr.IndexOf('begin')+1,'  FFieldType := GetItemClass(0);');

                WrapClass := False;
                PasTypeName := GetPasTypeName(Str.Template,WrapClass);
                if not (Str.Template.Constructed or
                        (Str.Template.ChoiceCount > 0) or
                        WrapClass) then begin
                  WrapClass := True;
                  PasTypeName := GetPasTypeName(Str.Template,WrapClass);
                  Publ.Add(Format('    function Add: %s;',[PasTypeName]));
                  ClassImpl.Add(Format('function %s.Add: %s;',[TypeName,PasTypeName]));
                  ClassImpl.Add('begin');
                  ClassImpl.Add(Format('  Result := %s(InternalAdd);',[PasTypeName]));
                  ClassImpl.Add('end;');
                  ClassImpl.Add('');

                  WrapClass := False;
                  PasTypeName := GetPasTypeName(Str.Template,WrapClass);
                  Publ.Add(Format('    procedure AddValue(const Value: %s);',[PasTypeName,PasTypeName]));
                  ClassImpl.Add(Format('procedure %s.AddValue(const Value: %s);',[TypeName,PasTypeName,PasTypeName]));
                  ClassImpl.Add('begin');
                  Dummy := True;
                  if Str.Template.ActualTag = V_ASN1_ENUMERATED then
                    ClassImpl.Add(Format('  %s(InternalAdd).Value := Ord(Value);',
                                         ['TEnumeratedWrapper']))
                  else
                    ClassImpl.Add(Format('  %s(InternalAdd).Value := Value;',
                                         [GetPasTypeName(Str.Template,Dummy)]));
                  ClassImpl.Add('end;');
                  ClassImpl.Add('');

                  WrapClass := True;
                  PasTypeName := GetPasTypeName(Str.Template,WrapClass);
                  Prop.Add(Format('    property Items[Index: Integer]: %s read GetItems;',[PasTypeName]));
                  Priv.Add(Format('    function GetItems(Index: Integer): %s;',[PasTypeName]));
                  ClassImpl.Add(Format('function %s.GetItems(Index: Integer): %s;',[TypeName,PasTypeName]));
                  ClassImpl.Add('begin');
                  ClassImpl.Add(Format('  Result := %s(InternalGetItems(Index));',
                                       [PasTypeName]));
                  ClassImpl.Add('end;');
                  ClassImpl.Add('');

                  WrapClass := False;
                  PasTypeName := GetPasTypeName(Str.Template,WrapClass);
                  if G = nil then
                    Publ.Add(Format('    property Values[Index: Integer]: %s read GetValues write SetValues; default;',[PasTypeName]))
                  else
                    Publ.Add(Format('    property Values[Index: Integer]: %s read GetValues write SetValues;',[PasTypeName]));
                  Priv.Add(Format('    function GetValues(Index: Integer): %s;',[PasTypeName]));
                  ClassImpl.Add(Format('function %s.GetValues(Index: Integer): %s;',[TypeName,PasTypeName]));
                  ClassImpl.Add('begin');
                  Dummy := True;
                  ClassImpl.Add(Format('  Result := %s(%s(InternalGetItems(Index)).Value);',
                                       [PasTypeName,
                                        GetPasTypeName(Str.Template,Dummy)]));
                  ClassImpl.Add('end;');
                  ClassImpl.Add('');
                  Priv.Add(Format('    procedure SetValues(Index: Integer; const Value: %s);',[PasTypeName]));
                  ClassImpl.Add(Format('procedure %s.SetValues(Index: Integer; const Value: %s);',[TypeName,PasTypeName]));
                  ClassImpl.Add('begin');
                  Dummy := True;
                  if Str.Template.ActualTag = V_ASN1_ENUMERATED then
                    ClassImpl.Add(Format('  %s(FItems[Index]).Value := Ord(Value);',
                                         ['TEnumeratedWrapper']))
                  else
                    ClassImpl.Add(Format('  %s(FItems[Index]).Value := Value;',
                                         [GetPasTypeName(Str.Template,Dummy)]));
                  ClassImpl.Add('end;');
                  ClassImpl.Add('');
                end else begin
                  Publ.Add(Format('    function Add: %s;',[PasTypeName]));
                  ClassImpl.Add(Format('function %s.Add: %s;',[TypeName,PasTypeName]));
                  ClassImpl.Add('begin');
                  ClassImpl.Add(Format('  Result := %s(InternalAdd);',[PasTypeName]));
                  ClassImpl.Add('end;');
                  ClassImpl.Add('');
                  if (G = nil) and (FUniqueItemDecl = '') then
                    Publ.Add(Format('    property Items[Index: Integer]: %s read GetItems; default;',[PasTypeName]))
                  else
                    Publ.Add(Format('    property Items[Index: Integer]: %s read GetItems;',[PasTypeName]));
                  Priv.Add(Format('    function GetItems(Index: Integer): %s;',[PasTypeName]));
                  ClassImpl.Add(Format('function %s.GetItems(Index: Integer): %s;',[TypeName,PasTypeName]));
                  ClassImpl.Add('begin');
                  ClassImpl.Add(Format('  Result := %s(InternalGetItems(Index));',
                                       [PasTypeName]));
                  ClassImpl.Add('end;');
                  ClassImpl.Add('');
                end;

                InsertComment;

                if CheckModule then
                  FIntf.Add('  ' + TypeName + ' = class(TASNCustomOFFieldWrapper)');
              end else begin
                Regs.Add('  Intf.RegisterConstructedClass(' + TypeName + ',F,');

                SubAS := AccessString;
                if Struct.Encapsulated <> nil then begin
                  Str := Struct.Encapsulated;
                  if Struct.GetNamePath <> '' then
                    SubAS := AccessString +
                             Format('.FindField(%s).Encapsulated',[Struct.GetNamePath])
                  else
                    SubAS := AccessString +
                             Format('.Encapsulated',[Struct.GetNamePath]);
                end else
                  Str := Struct;
                for I := 0 to Str.ItemCount - 1 do begin
                  F := Str.Items[I];
                  E := F^.Encapsulated;
                  if Assigned(E) and (F^.ChoiceCount = 0) then begin
                    if F^.GetNamePath <> '' then
                      SubAS := SubAS + Format('.FindField(''%s'').Encapsulated',[F.GetNamePath])
                    else
                      SubAS := SubAS + '.Encapsulated';
                    F := @E;
                  end;
                  CheckExplicit(F,Explicit);
                  if not F^.Constructed then begin
                    Dummy := True;
                    if GetPasTypeName(F^,Dummy,True) = '' then
                      Continue;
                  end;

                  if Assigned(E) then begin
                    if (E.Constructed or (E.ChoiceCount > 0)) and
                       (FDecl.IndexOf(GetTypeName(E)) < 0) and
                       (GetTypeMap(Struct.TypeName,E.VarName) = '') and
                       (GetTypeMap(E.TypeName,'') = '') then
                      DoGenerateClass(F^,SubAS)
                  end else if (F^.Constructed or (F.ChoiceCount > 0)) and
                     (FDecl.IndexOf(GetTypeName(F^)) < 0) and
                     (GetTypeMap(Struct.TypeName,F^.VarName) = '') and
                     (GetTypeMap(F^.TypeName,'') = '') then
                    DoGenerateClass(F^,SubAS);

                  WrapClass := True;
                  Cls.Add(GetPasTypeName(F^,WrapClass));
                  if Str = Struct then begin
                    if Explicit then
                      Constr.Add(Format('  FieldFactory(GetItemClass(%d),' +
                                                       'nil,' +
                                                       'FData.Items[%d]^.Items[0]^,' +
                                                       'nil^);',
                                        [I,I]))
                    else
                      Constr.Add(Format('  FieldFactory(GetItemClass(%d),' +
                                                       'nil,' +
                                                       'FData.Items[%d]^,' +
                                                       'nil^);',
                                        [I,I]))
                  end else
                    if Explicit then
                      Constr.Add(Format('  FieldFactory(GetItemClass(%d),' +
                                                       'nil,' +
                                                       'FData.Encapsulated.Items[%d]^.Items[0]^,' +
                                                       'nil^);',
                                        [I,I]))
                    else
                      Constr.Add(Format('  FieldFactory(GetItemClass(%d),' +
                                                       'nil,' +
                                                       'FData.Encapsulated.Items[%d]^,' +
                                                       'nil^);',
                                        [I,I]));
                  WrapClass := False;
                  PasTypeName := GetPasTypeName(F^,WrapClass);
                  if Explicit then
                    VarName := F^.Owner.VarName
                  else
                    VarName := F^.VarName;
                  VarName := FormatVarName(VarName);
                  if (F^.ActualTag = V_ASN1_ENUMERATED) and
                     (PasTypeName <> 'Integer') and
                     CheckModule(F^) then begin
                    FIntf.Add(Format('  %s = ( { TODO: Enter values here } );',[PasTypeName]));
                    FIntf.Add('');
                  end;
                  Prop.Add(Format('    property %s: %s read Get%s write Set%s;',
                                  [VarName,PasTypeName,VarName,VarName]));
                  Priv.Add(Format('    function Get%s: %s;',[VarName,PasTypeName]));
                  ClassImpl.Add(Format('function %s.Get%s: %s;',
                                       [TypeName,VarName,PasTypeName]));
                  ClassImpl.Add('begin');
                  Dummy := True;
                  if (F^.Constructed or
                      (F^.ChoiceCount > 0) or
                      WrapClass) then
                    ClassImpl.Add(Format('  Result := FItems[%d];',[I]))
                  else
                    ClassImpl.Add(Format('  Result := %s(%s(FItems[%d]).Value);',
                                         [PasTypeName,GetPasTypeName(F^,Dummy),I]));
                  ClassImpl.Add('end;');
                  ClassImpl.Add('');
                  Priv.Add(Format('    procedure Set%s(const Value: %s);',[VarName,PasTypeName]));
                  ClassImpl.Add(Format('procedure %s.Set%s(const Value: %s);',[TypeName,VarName,PasTypeName]));
                  ClassImpl.Add('begin');
                  if (F^.Constructed or
                      (F^.ChoiceCount > 0) or
                      WrapClass) then
                    ClassImpl.Add(Format('  %s(FItems[%d]).Assign(Value);',
                                         [GetPasTypeName(F^,WrapClass),I]))
                  else if F^.ActualTag = V_ASN1_ENUMERATED then
                    ClassImpl.Add(Format('  %s(FItems[%d]).Value := Ord(Value);',
                                         ['TEnumeratedWrapper',I]))
                  else
                    ClassImpl.Add(Format('  %s(FItems[%d]).Value := Value;',
                                         [GetPasTypeName(F^,Dummy),I]));
                  ClassImpl.Add('end;');
                  ClassImpl.Add('');

                  if F^.TypeIdentified then begin
                    Dummy := True;
                    if F^.Template <> nil then begin
                      Publ.Add(Format('    function IsType(Id: %sEnum): Boolean;',[GetPasTypeName(F^.Template,Dummy)]));
                      ClassImpl.Add(Format('function %s.IsType(Id: %sEnum): Boolean;',[TypeName,GetPasTypeName(F^.Template,Dummy)]));
                      ClassImpl.Add('begin');
                      ClassImpl.Add(Format('  if TASNCustomOFFieldWrapper(FItems[%d]).ItemCount > 0 then',[I]));
                      ClassImpl.Add(Format('    Result := %s(FItems[%d]).Choice = Id',[GetPasTypeName(F^.Template,Dummy),I]));
                      ClassImpl.Add('  else');
                      ClassImpl.Add('    Result := False;');
                      ClassImpl.Add('end;');
                      ClassImpl.Add('');
                    end else begin
                      Publ.Add(Format('    function IsType(Id: %sEnum): Boolean;',[GetPasTypeName(F^,Dummy)]));
                      ClassImpl.Add(Format('function %s.IsType(Id: %sEnum): Boolean;',[TypeName,GetPasTypeName(F^,Dummy)]));
                      ClassImpl.Add('begin');
                      ClassImpl.Add(Format('  Result := %s(FItems[%d]).Choice = Id;',[GetPasTypeName(F^,Dummy),I]));
                      ClassImpl.Add('end;');
                      ClassImpl.Add('');
                      Publ.Add(Format('    procedure SetType(Id: %sEnum);',[GetPasTypeName(F^,Dummy)]));
                      ClassImpl.Add(Format('procedure %s.SetType(Id: %sEnum);',[TypeName,GetPasTypeName(F^,Dummy)]));
                      ClassImpl.Add('begin');
                      ClassImpl.Add(Format('  %s(FItems[%d]).Choice := Id;',[GetPasTypeName(F^,Dummy),I]));
                      ClassImpl.Add('end;');
                      ClassImpl.Add('');
                    end;
                    TypeIden := True;
                  end;
                  FUniqueItemDecl := '';
                end;

                if Cls.Count = 0 then
                  Regs.Add('    []);')
                else if Cls.Count = 1 then
                  Regs.Add('    [' + Cls[0] + ']);')
                else begin
                  Regs.Add('    [' + Cls[0] + ',');
                  for I := 1 to Cls.Count - 2 do
                    Regs.Add('     ' + Cls[I] + ',');
                  Regs.Add('     ' + Cls[Cls.Count - 1] + ']);');
                end;

                InsertComment;

                if CheckModule then
                  FIntf.Add('  ' + TypeName + ' = class(TASNConstructedWrapper)');
              end;
              Constr.Add('end;');
              Constr.Add('');

              Destr.Add('  inherited Destroy;');
              Destr.Add('end;');
              Destr.Add('');

              if CheckModule then begin
                FImpl.Add('{ ' + TypeName + ' }');
                FImpl.Add('');
                FImpl.AddStrings(Constr);
                FImpl.AddStrings(Destr);
                FImpl.AddStrings(ClassImpl);

                FIntf.Add('  private');
                FIntf.AddStrings(Priv);
                FIntf.Add('  public');
                FIntf.AddStrings(Publ);
                if (Struct.Template = nil) and
                   ((Struct.Encapsulated = nil) or
                    (Struct.Encapsulated.Template = nil)) and
                   (Prop.Count > 0)  then
                  FIntf.Add('  published');
                FIntf.AddStrings(Prop);
                FIntf.Add('  end;');
                FIntf.Add('');

                FRegs.AddStrings(Regs);
              end;
            finally
              Publ.Free;
              Prop.Free;
              Cls.Free;
              Regs.Free;
            end;
          finally
            Priv.Free;
          end;
        finally
          ClassImpl.Free;
        end;
      finally
        Destr.Free;
      end;
    finally
      Constr.Free;
    end;
    if TypeIden then
      Code := 1
    else if Unique then
      Code := 2
    else
      Code := 0;
    if (Struct.Encapsulated <> nil) and
       (FDecl.IndexOf(GetTypeName(Struct.Encapsulated)) < 0) then
      FDecl.AddObject(GetTypeName(Struct.Encapsulated),TObject(Code))
    else if FDecl.IndexOf(GetTypeName(Struct)) < 0 then
      FDecl.AddObject(GetTypeName(Struct),TObject(Code));
    I := FPendDecl.IndexOf(GetTypeName(Struct));
    FPendDecl.Delete(I);
  end;
  FDeclTree.Delete(FDeclTree.Count - 1);
  FUniqueItemDecl := UniqueItemDecl;
  FGetUniqueItemDecl := GetUniqueItemDecl;
end;

function TASNToPascal.FormatTypeName(Struct: TASN1Struct): string;
var
  TN: string;
  I: Integer;
  PrevBlank, CanBeUpperCase, HasLowerCase: Boolean;
  C: Char;
begin
  TN := GetTypeName(Struct);
  if Struct.Template = nil then
    if (TN = 'SEQUENCE') or (TN = 'SET') then begin
      Result := '{TODO: Undefined type name}';
      Exit;
    end;
  HasLowerCase := False;
  CanBeUpperCase := True;
  Result := 'T';
  PrevBlank := True;
  for I := 1 to Length(TN) do begin
    C := TN[I];
    if C in ['-','_'] then begin
      PrevBlank := True;
      HasLowerCase := True;
      CanBeUpperCase := True;
      Result := Result + '_';
    end else if C in ['a'..'z','0'..'9'] then begin
      HasLowerCase := True;
      if PrevBlank then
        Result := Result + UpperCase(C)
      else
        Result := Result + C;
      PrevBlank := False;
      CanBeUpperCase := True;
    end else if C in ['A'..'Z'] then begin
      if CanBeUpperCase then
        Result := Result + C
      else
        Result := Result + LowerCase(C);
      PrevBlank := False;
      CanBeUpperCase := HasLowerCase;
    end else begin
      HasLowerCase := False;
      CanBeUpperCase := True;
    end;
  end;
end;

function TASNToPascal.GetPasTypeName(Struct: TASN1Struct;
  var WrapClass: Boolean; Inher: Boolean): string;
var
  vTag: Cardinal;
begin
  if (Struct.Encapsulated <> nil) and (Struct.ChoiceCount = 0) then
    Struct := Struct.Encapsulated;
  if Inher then
    Result := ''
  else
    Result := GetTypeMap(Struct.TypeName,'');
  if Result <> '' then begin
    if WrapClass or
       (GetWrapTypeMap(Struct.TypeName,'') = nil) then
      WrapClass := True;
  end else begin
    if Assigned(Struct.Owner) and not Inher then
      Result := GetTypeMap(Struct.Owner.TypeName,Struct.VarName);
    if Result <> '' then begin
      if WrapClass or
         (GetWrapTypeMap(Struct.Owner.TypeName,Struct.VarName) = nil) then
        WrapClass := True;
    end else begin
      if Struct.Constructed or (Struct.ChoiceCount > 0) then begin
        if Inher then begin
          if Struct.ChoiceCount > 0 then
            Result := 'TASNChoiceWrapper'
          else if Struct.Template <> nil then
            Result := 'TASNCustomOFFieldWrapper'
          else
            Result := 'TASNConstructedWrapper';
        end else
          Result := FormatTypeName(Struct);
      end else begin
        if Struct.Implicit then
          vTag := Struct.ImplicitTag
        else
          vTag := Struct.Tag;
        Result := WrapperUnivTypes[vTag];
        if not WrapClass then begin
          if (vTag = V_ASN1_ENUMERATED) and
             (GetTypeName(Struct) <> 'ENUMERATED') then begin
            WrapClass := False;
            Result := FormatTypeName(Struct);
          end else begin
            WrapClass := CompareStr(Result, MappedUnivTypes[vTag]) = 0;
            Result := MappedUnivTypes[vTag];
          end;
        end;
      end;
    end;
  end;
end;


function TASNToPascal.GetTypeMap(OwnerTypeName, VarName: string): string;
begin
  Result := FTypeMap.Values[Format('%s.%s',[OwnerTypeName, VarName])];
end;

function TASNToPascal.GetTypeName(Struct: TASN1Struct): string;
begin
  if THack(Struct).AliasTypeName <> '' then
    Result := THack(Struct).AliasTypeName
  else if Struct.Implicit then
    Result := Struct.ImplicitTypeName
  else
    Result := Struct.TypeName;
end;

function TASNToPascal.GetWrapTypeMap(OwnerTypeName,
  VarName: string): TASNWrapperClass;
var
  I: Integer;
begin
  I := FTypeMap.IndexOfName(Format('%s.%s',[OwnerTypeName, VarName]));
  Result := TASNWrapperClass(FTypeMap.Objects[I]);
end;

procedure TASNToPascal.SetCustomUsesList(const Value: TStrings);
begin
  FUsesList.Assign(Value);
end;

procedure TASNToPascal.SetGenerateComponent(const Value: Boolean);
begin
  FGenerateComponent := Value;
end;

procedure TASNToPascal.SetStruct(const Value: TASN1Struct);
begin
  if Assigned(FStruct) then
    THack(FStruct)._Release;
  FStruct := Value;         
  if Assigned(FStruct) then
    THack(FStruct)._AddRef;
end;

procedure TASNToPascal.SetTypeMap(OwnerTypeName, VarName: string; const Value: string);
begin
  FTypeMap.Values[Format('%s.%s',[OwnerTypeName, VarName])] := Value;
end;

procedure TASNToPascal.SetUnitName(const Value: string);
begin
  FUnitName := Value;
end;

procedure TASNToPascal.SetWrapTypeMap(OwnerTypeName, VarName: string;
  const Value: TASNWrapperClass);
var
  S: string;
  I: Integer;
begin
  S := Format('%s.%s',[OwnerTypeName, VarName]);
  FTypeMap.Values[S] := Value.ClassName;
  I := FTypeMap.IndexOfName(S);
  FTypeMap.Objects[I] := TObject(Value);
end;

function TASNToPascal.ChoicesToEnumStr(Struct: TASN1Struct): string;
var
  T, Pfx: string;
  SL: TStringList;
  F: PASN1Struct;
  I, J: Integer;
  WrapClass: Boolean;
begin
  WrapClass := True;
  T := GetPasTypeName(Struct,WrapClass);
  Pfx := '';
  for I := 2 to Length(T) do
    if T[I] in ['A'..'Z'] then
      Pfx := Pfx + LowerCase(T[I]);
  Pfx := Pfx + 'e';
  if FPfx.IndexOf(Pfx) > 0 then begin
    if Pfx = 'e' then
      Pfx := Copy(T,2,3) + 'e'
    else begin
      Pfx := '';
      for I := 2 to Length(T) do
        if T[I] in ['A'..'Z'] then begin
          Pfx := Pfx + LowerCase(T[I]);
          if (Length(Pfx) = 1) and (I < Length(T)) then
            Pfx := Pfx + LowerCase(T[I+1]);
        end;
      Pfx := Pfx + 'e';
      if FPfx.IndexOf(Pfx) > 0 then
        Pfx := LowerCase(Copy(T,2,MaxInt)) + 'e';
    end;
  end;
  FPfx.Add(Pfx);
  Result := '  ' + T + 'Enum = ('#13#10;
  SL := TStringList.Create;
  try
    if Struct.TypeIdentified then
      F := THack(Struct).FindIDField
    else
      F := nil;
    for I := 0 to Struct.ChoiceCount - 1 do begin
      if Struct.TypeIdentified then begin
        if (F = nil) or (F^.ActualTag = V_ASN1_OBJECT) then
          T := GetObjectName(Struct.Choices[I]^.IdentifiedBy)
        else
          T := Struct.Choices[I]^.TypeName;
      end else
        T := Struct.Choices[I]^.VarName;
      J := Length(T);
      while J > 0 do begin
        if T[J] in ['A'..'Z','a'..'z','0'..'9'] then
          Dec(J)
        else begin
          Delete(T,J,1);
          T[J] := UpperCase(T[J])[1];
        end;
      end;

      if T = '' then
        T := '{ VarName missing }'
      else
        T[1] := UpperCase(T[1])[1];
      SL.Add(Pfx + T);
    end;
    Result := Result + ListToStr('    ' + Pfx + 'Undefined',SL,4) + ');';
  finally
    SL.Free;
  end;
end;

{ TASNToPascalModule }

constructor TASNToPascalModule.Create(Struct: TASN1Struct);
begin
  FConverter := TASNToPascal.Create;
  FConverter.Struct := Struct;
end;

destructor TASNToPascalModule.Destroy;
begin
  inherited;
  FConverter.Free;
end;

function TASNToPascalModule.Existing: Boolean;
begin
  Result := False;
end;

procedure TASNToPascalModule.FormCreated(Form: TIFormInterface);
begin
end;

function TASNToPascalModule.GetAncestorName: string;
begin
  Result := '';
end;

function TASNToPascalModule.GetFileName: string;
begin
  Result := FConverter.FStruct.ModuleName;
  if Result <> '' then
    Result := FormatModuleName(Result) + '.pas';
end;

function TASNToPascalModule.GetFileSystem: string;
begin
  Result := '';
end;

function TASNToPascalModule.GetFormName: string;
begin
  Result := '';
end;

function TASNToPascalModule.GetIntfName: string;
begin
  Result := '';
end;

function TASNToPascalModule.NewIntfSource(const UnitIdent, FormIdent,
  AncestorIdent: string): string;
begin
  Result := '';
end;

function TASNToPascalModule.NewModuleSource(const UnitIdent, FormIdent,
  AncestorIdent: string): string;
begin
  FConverter.UnitName := UnitIdent;
  FConverter.Generate;
  Result := FConverter.Source;
end;

end.
