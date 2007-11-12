{*******************************************************}
{                                                       }
{     StreamSec Security Library for Borland Delphi     }
{     SignCodeWizard Unit                               }
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
{-------------------------------------------------------}
{     This file contains portions of code from          }
{     Project JEDI Code Library (JCL)                   }
{     JCL is licensed under MPL 1.1. This file must     }
{     not be included in any run time executable, but   }
{     only used as part of this design time package.    }
{                                                       }
{*******************************************************}
{$I ver.inc}
unit SignCodeWizard;

interface

uses
{$IFDEF D6UP}
  Variants,
{$ENDIF D6UP}
  Windows, Classes, Registry, Menus, ActnList, ToolsAPI, SysUtils, Graphics,
  Dialogs, Forms,
  SecUtils, SsCertMgr, MpSignCode;

type
  EStrSecIISignCodeWizard = class(Exception);

  TStrSecIISignCodeWizard = class({$IFNDEF D5UP}TInterfacedObject,{$ELSE}
                                  TNotifierObject,{$ENDIF} IOTAWizard)
  private                         
    FServices: IOTAServices;  
    FEnvVariables: TStringList;
    FBaseRegistryKey: string;
    FBuildMenuItem: TMenuItem;
    FBuildAction: TAction;
    {$IFNDEF D5UP}
    FSaveAllAction: TAction;
    {$ENDIF D5UP}
    FIndex: Integer;
    FPassword: ISecretKey;
    procedure ActionExecute(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
    function GetActiveProject: IOTAProject;
    function GetProjectGroup: IOTAProjectGroup;
    function GetDescription: string;
    procedure ReadEnvVariables;
    procedure SetIndex(const Value: Integer);
    procedure PrivateKeyRingPassword(Sender: TObject; Password: ISecretKey);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterCommand;
    procedure UnregisterCommand;
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
    function FindExecutableName(const PrelFileName, OutputDirectory: string; var ExecutableFileName: string): Boolean;
    function GetPrelFileName(const Project: IOTAProject): string;
    function GetOutputDirectory(const Project: IOTAProject): string;
    function IsPackage(const Project: IOTAProject): Boolean;
    function SubstitutePath(const Path: string): string;
    property ActiveProject: IOTAProject read GetActiveProject;
    property Description: string read GetDescription;
    property ProjectGroup: IOTAProjectGroup read GetProjectGroup;   
    property Index: Integer read FIndex write SetIndex;
  end;

implementation

uses
  Controls, PasswordMain, SignCodeWizardDlg, ImageHlp;

var
  StrSecIISignCodeWizard: TStrSecIISignCodeWizard;

const
  {$IFDEF LINUX}
  PathSeparator    = '/';
  {$ENDIF LINUX}
  {$IFDEF MSWINDOWS}
  PathSeparator    = '\';
  {$ENDIF MSWINDOWS}

function PathAddSeparator(const Path: string): string;
begin
  Result := Path;
  if (Length(Path) = 0) or (AnsiLastChar(Path) <> PathSeparator) then
    Result := Path + PathSeparator;
end;

function PathRemoveExtension(const Path: string): string;
var
  I: Integer;
begin
  I := LastDelimiter(':.' + PathSeparator, Path);
  if (I > 0) and (Path[I] = '.') then
    Result := Copy(Path, 1, I - 1)
  else
    Result := Path;
end;

function PathExtractFileNameNoExt(const Path: string): string;
begin
  Result := PathRemoveExtension(ExtractFileName(Path));
end;

function GetRegValue(Section, Ident, Default: string): string;
var
  Reg: TRegistry;
begin
  Result := Default;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly('Software\StreamSec\StrSecii\' + Section) then
      if Reg.ValueExists(Ident) then
        Result := Reg.ReadString(Ident);
  finally
    Reg.Free;
  end;
end;

procedure SetRegValue(Section, Ident, Value: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('Software',True) then
      if Reg.OpenKey('StreamSec',True) then
        if Reg.OpenKey('StrSecii',True) then
          if Reg.OpenKey(Section,True) then
            Reg.WriteString(Ident,Value);
  finally
    Reg.Free;
  end;
end;

function RelativeKey(const Key: string): PChar;
begin
  Result := PChar(Key);
  if (Key <> '') and (Key[1] = '\') then
    Inc(Result);
end;

function RegKeyExists(const RootKey: HKEY; const Key: string): Boolean;
var
  RegKey: HKEY;
begin
  Result := (RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS);
  if Result then RegCloseKey(RegKey);
end;
     
function RegGetValueNames(const RootKey: HKEY; const Key: string; const List: TStrings): Boolean;
var
  RegKey: HKEY;
  I: DWORD;
  Size: DWORD;
  NumSubKeys: DWORD;
  NumSubValues: DWORD;
  MaxSubValueLen: DWORD;
  ValueName: string;
begin
  Result := False;
  List.Clear;
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    if RegQueryInfoKey(RegKey, nil, nil, nil, @NumSubKeys, nil, nil, @NumSubValues, @MaxSubValueLen, nil, nil, nil) = ERROR_SUCCESS then
    begin
      SetLength(ValueName, MaxSubValueLen + 1);
      if NumSubValues <> 0 then
        for I := 0 to NumSubValues - 1 do begin
          Size := MaxSubValueLen + 1;
          RegEnumValue(RegKey, I, PChar(ValueName), Size, nil, nil, nil, nil);
          List.Add(PChar(ValueName));
        end;
      Result := True;
    end;
    RegCloseKey(RegKey);
  end else
    raise Exception.Create('Unable to open key ' + Key);
end;
             
function RegReadStringDef(const RootKey: HKEY; const Key, Name, Def: string): string;
var
  RegKey: HKEY;
  Size: DWORD;
  StrVal: string;
  RegKind: DWORD;
begin
  Result := Def;
  if RegOpenKeyEx(RootKey,
                  RelativeKey(Key),
                  0,
                  KEY_READ,
                  RegKey) = ERROR_SUCCESS then begin
    RegKind := 0;
    Size := 0;
    if RegQueryValueEx(RegKey,
                       PChar(Name),
                       nil,
                       @RegKind,
                       nil,
                       @Size) = ERROR_SUCCESS then
      if RegKind in [REG_SZ, REG_EXPAND_SZ] then begin
        SetLength(StrVal, Size);
        if RegQueryValueEx(RegKey,
                           PChar(Name),
                           nil,
                           @RegKind,
                           PByte(StrVal),
                           @Size) = ERROR_SUCCESS then begin
          SetLength(StrVal, StrLen(PChar(StrVal)));
          Result := StrVal;
        end;
      end;
    RegCloseKey(RegKey);
  end;
end;

resourcestring
  RsActionCaption = 'Build and Sign %s';
  RsProjectNone = '[none]';
  RsCantFindFiles = 'Can''t find executable file';

type
  TX509CertificateAuthorityHack = class(TX509CertificateAuthority);

{$R SignCodeWizard.res}

{ TStrSecIISignCodeWizard }

procedure TStrSecIISignCodeWizard.ActionExecute(Sender: TObject);
var
  Dlg: TfrmSignCodeWizard;
  Comp: TX509CertificateAuthorityHack;      
  SignCode: TMpSignCode;              
  CertMgr: TX509CertificateAuthority;
  vActiveProject: IOTAProject;
  BuildOK, Succ: Boolean;
  ProjOptions: IOTAProjectOptions;
  OutputDirectory, PrelFileName, ProjectFileName, ExecutableFileName: string;
  ProjectName: string;
{$IFDEF D5UP}
  OptionsModifiedState: Boolean;
{$ENDIF D5UP}
begin
  vActiveProject := ActiveProject;
  Assert(Assigned(vActiveProject));
  ProjectFileName := vActiveProject.FileName;
  ProjectName := ExtractFileName(ProjectFileName);

  ProjOptions := vActiveProject.ProjectOptions;
  OutputDirectory := GetOutputDirectory(vActiveProject);
  PrelFileName := GetPrelFileName(vActiveProject);

  Dlg := TfrmSignCodeWizard.Create(Application);
  try
    Dlg.Description := Description;
    Dlg.URL := GetRegValue('Info','URL','');
    Dlg.PrivateKeyRing := GetRegValue('Keys','PKR','');
    Dlg.MyCerts := GetRegValue('Keys','MyCerts','');
    Dlg.RootCerts := GetRegValue('Keys','RootCerts','');
    Succ := Dlg.ShowModal = mrOK;
    if Succ then begin
      SignCode := TMpSignCode.Create(nil);
      try
        CertMgr := TX509CertificateAuthority.Create(nil);
        try
          CertMgr.OnPassword := PrivateKeyRingPassword;   
          Comp := TX509CertificateAuthorityHack(CertMgr);
          if Dlg.Modified then begin
            SetRegValue('Info','URL',Dlg.URL);
            SetRegValue('Keys','PKR',Dlg.PrivateKeyRing);
            SetRegValue('Keys','MyCerts',Dlg.MyCerts);
            SetRegValue('Keys','RootCerts',Dlg.RootCerts);
          end;
          if Dlg.RootCerts <> '' then
            Comp.LoadRootCertsFromFile(Dlg.RootCerts);
          try
            Comp.LoadPrivateKeyRingFromFile(Dlg.PrivateKeyRing,FPassword);
            Comp.LoadMyCertsFromFile(Dlg.MyCerts);
            SignCode.PrivateKeyRing := Comp.StreamSecIIComp;
            SignCode.MyCertificates := Comp.MyCerts;
            SignCode.URL := Dlg.URL;
            SignCode.Description := Dlg.Description;

            {$IFDEF D5UP}
            OptionsModifiedState := ProjOptions.ModifiedState;
            BuildOK := vActiveProject.ProjectBuilder.BuildProject(cmOTABuild, False);
            ProjOptions.ModifiedState := OptionsModifiedState;
            {$ELSE  D5UP}
            BuildOK := vActiveProject.ProjectBuilder.BuildProject(cmOTABuild, False);
            {$ENDIF D5UP}

            if BuildOK then begin
              Succ := FindExecutableName(PrelFileName, OutputDirectory, ExecutableFileName);
              if not Succ then
                MessageDlg('Unable to find file "' + PrelFileName + '"', mtError, [mbOk], 0)
              else if not SignCode.SignCode(ExecutableFileName) then begin
                FPassword := nil;
                MessageDlg('Unable to sign file "' + ExecutableFileName + '"', mtError, [mbOk], 0);
              end;
            end;
          except
            FPassword := nil;
            raise;
          end;
        finally
          SignCode.MyCertificates := nil;
          SignCode.PrivateKeyRing := nil;
          CertMgr.Free;
        end;
      finally
        SignCode.Free;
      end;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TStrSecIISignCodeWizard.ActionUpdate(Sender: TObject);
var
  vActiveProject: IOTAProject;
  ProjectName: string;
begin
  vActiveProject := ActiveProject;
  if Assigned(vActiveProject) then
    ProjectName := ExtractFileName(vActiveProject.FileName)
  else
    ProjectName := '';
{$IFDEF D5UP}
  FBuildAction.Enabled := Assigned(vActiveProject);
{$ELSE}
  FBuildAction.Enabled := Assigned(vActiveProject) and (not FSaveAllAction.Enabled);
{$ENDIF D5UP}
  if not FBuildAction.Enabled then
    ProjectName := RsProjectNone;
  FBuildAction.Caption := Format(RsActionCaption, [ProjectName]);
end;

{$IFNDEF D5UP}
procedure TStrSecIISignCodeWizard.AfterSave;
begin

end;

procedure TStrSecIISignCodeWizard.BeforeSave;
begin

end;
{$ENDIF}

constructor TStrSecIISignCodeWizard.Create;
begin
  inherited Create;
  FEnvVariables := TStringList.Create;
  FServices := BorlandIDEServices as IOTAServices;
  FBaseRegistryKey := FServices.GetBaseRegistryKey;
  if (FBaseRegistryKey <> '') and (FBaseRegistryKey[Length(FBaseRegistryKey)] <> '\') then
    FBaseRegistryKey := FBaseRegistryKey + '\';
  RegisterCommand;
end;

destructor TStrSecIISignCodeWizard.Destroy;
begin
  UnregisterCommand;
  FEnvVariables.Free;
  inherited;
end;

{$IFNDEF D5UP}
procedure TStrSecIISignCodeWizard.Destroyed;
begin

end;
{$ENDIF}

procedure TStrSecIISignCodeWizard.Execute;
begin
  // Not handled here
end;

function TStrSecIISignCodeWizard.FindExecutableName(const PrelFileName,
  OutputDirectory: string; var ExecutableFileName: string): Boolean;
var
  Se: TSearchRec;
  Res: Integer;
  LatestTime: Integer;
  FileName: TFileName;
  LI: LoadedImage;
begin
  LatestTime := 0;
  ExecutableFileName := '';
  // the latest executable file is very likely our file
{$WARNINGS OFF} // This unit doesn't run on Linux anyway
  Res := FindFirst(ChangeFileExt(PrelFileName, '.*'), faArchive, Se);
{$WARNINGS ON}
  while Res = 0 do begin
    if (OutputDirectory <> '') and (OutputDirectory[Length(OutputDirectory)] <> '\') then
      FileName := OutputDirectory + '\' + Se.Name
    else
      FileName := OutputDirectory + Se.Name;
    if MapAndLoad(PChar(FileName), nil, @LI, False, True) then begin
      if (not LI.fDOSImage) and (Se.Time > LatestTime) then begin
        ExecutableFileName := FileName;
        LatestTime := Se.Time;
      end;
      UnMapAndLoad(@LI);
    end;
    Res := FindNext(Se);
  end;
  FindClose(Se);
  Result := (ExecutableFileName <> '');
end;

function TStrSecIISignCodeWizard.GetActiveProject: IOTAProject;
var
  vProjectGroup: IOTAProjectGroup;
begin
  vProjectGroup := ProjectGroup;
  if Assigned(vProjectGroup) then
    Result := vProjectGroup.ActiveProject
  else
    Result := nil;
end;

function TStrSecIISignCodeWizard.GetDescription: string;
var
  Options: IOTAOptions;
begin
  Options := ActiveProject.ProjectOptions;
  Result := Options.Values['ExeDescription'];
end;

function TStrSecIISignCodeWizard.GetIDString: string;
begin
  Result := 'StreamSec.' + ClassName;
end;

function TStrSecIISignCodeWizard.GetName: string;
begin
  Result := ClassName;
end;

function TStrSecIISignCodeWizard.GetOutputDirectory(
  const Project: IOTAProject): string;
begin
  if IsPackage(Project) then begin
    Result := VarToStr(Project.ProjectOptions.Values[PkgDllDirOptionName]);
    if Result = '' then
      Result := FServices.GetEnvironmentOptions.Values[BPLOutputDirOptionName];
  end else
    Result := VarToStr(Project.ProjectOptions.Values[OutputDirOptionName]);
  Result := SubstitutePath(Trim(Result));
  if Result = '' then
    Result := ExtractFilePath(Project.FileName);
end;

function TStrSecIISignCodeWizard.GetPrelFileName(
  const Project: IOTAProject): string;
var
  ProjectFileName, OutputDirectory, LibPrefix, LibSuffix: string;
begin
  ProjectFileName := Project.FileName;
  OutputDirectory := GetOutputDirectory(Project);
  {$IFDEF D6UP}
  if IsPackage(Project) then begin
    LibPrefix := Trim(VarToStr(Project.ProjectOptions.Values[LIBPREFIXOptionName]));
    LibSuffix := Trim(VarToStr(Project.ProjectOptions.Values[LIBSUFFIXOptionName]));
  end else begin
    LibPrefix := '';
    LibSuffix := '';
  end;
  {$ELSE D6UP}
  LibPrefix := '';
  LibSuffix := '';
  {$ENDIF D6UP}
  Result := PathAddSeparator(OutputDirectory) + LibPrefix + PathExtractFileNameNoExt(ProjectFileName) +
    LibSuffix + PrelExtension;
end;

function TStrSecIISignCodeWizard.GetProjectGroup: IOTAProjectGroup;
var
  ModuleServices: IOTAModuleServices;
  I: Integer;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  for I := 0 to ModuleServices.ModuleCount - 1 do
    if ModuleServices.Modules[I].QueryInterface(IOTAProjectGroup, Result) = S_OK then
      Exit;
  Result := nil;
end;

function TStrSecIISignCodeWizard.GetState: TWizardState;
begin
  Result := [];
end;

function TStrSecIISignCodeWizard.IsPackage(
  const Project: IOTAProject): Boolean;
begin                    
  {$IFDEF D5UP}
  Result := AnsiSameText(ExtractFileExt(Project.FileName), DPKExtension);
  {$ELSE  D5UP}
  Result := AnsiCompareText(ExtractFileExt(Project.FileName), DPKExtension) = 0;
  {$ENDIF D5UP}
end;

{$IFNDEF D5UP}
procedure TStrSecIISignCodeWizard.Modified;
begin

end;
{$ENDIF}

procedure TStrSecIISignCodeWizard.PrivateKeyRingPassword(Sender: TObject;
  Password: ISecretKey);
var
  Save: Boolean;
begin
  if Assigned(FPassword) then begin
    Password.SetLength(FPassword.KeyLen);
    Password.SetKeyAt(FPassword,0);
  end else begin
    Save := True;
    if PasswordDlg('Private Key Ring',Password,Save,False) then
      if Save then
        FPassword := Password;
  end;
end;

procedure TStrSecIISignCodeWizard.ReadEnvVariables;
{$IFDEF D6UP}
const
  EnvironmentVarsKey = 'Environment Variables';
var
  EnvNames: TStringList;
  I: Integer;
  EnvVarKeyName: string;
{$ENDIF D6UP}
begin
  FEnvVariables.Clear;
  {$IFDEF D6UP}
  EnvNames := TStringList.Create;
  try
    EnvVarKeyName := FBaseRegistryKey + EnvironmentVarsKey;
    if RegKeyExists(HKEY_CURRENT_USER, EnvVarKeyName) and
       RegGetValueNames(HKEY_CURRENT_USER, EnvVarKeyName, EnvNames) then
      for I := 0 to EnvNames.Count - 1 do
        FEnvVariables.Values[EnvNames[I]] := RegReadStringDef(HKEY_CURRENT_USER, EnvVarKeyName, EnvNames[I], '');
  finally
    EnvNames.Free;
  end;
  {$ENDIF D6UP}
end;

procedure TStrSecIISignCodeWizard.RegisterCommand;
var
  IDEMainMenu: TMainMenu;
  IDEProjectItem: TMenuItem;
  IDEActionList: TActionList;
  I: Integer;
  ImageBmp: TBitmap;
begin
  FBuildAction := TAction.Create(nil);
  FBuildAction.Caption := Format(RsActionCaption, [RsProjectNone]);
  FBuildAction.Visible := True;
  FBuildAction.OnExecute := ActionExecute;
  FBuildAction.OnUpdate := ActionUpdate;
  ImageBmp := TBitmap.Create;
  try
    ImageBmp.LoadFromResourceName(FindResourceHInstance(HInstance), 'BUILDSIGN');
    Assert((ImageBmp.Width > 0) and (ImageBmp.Height > 0),'Could not find image resource BUILDSIGN');
    FBuildAction.ImageIndex := (BorlandIDEServices as INTAServices).AddMasked(ImageBmp, clSilver);
    Assert(FBuildAction.ImageIndex > -1);
  finally
    ImageBmp.Free;
  end;
  IDEActionList := TActionList((BorlandIDEServices as INTAServices).ActionList);
  FBuildAction.ActionList := IDEActionList;
  FBuildMenuItem := TMenuItem.Create(nil);
  FBuildMenuItem.Action := FBuildAction;
  IDEMainMenu := (BorlandIDEServices as INTAServices).MainMenu;
  IDEProjectItem := nil;
  with IDEMainMenu do
    for I := 0 to Items.Count - 1 do
      if Items[I].Name = 'ProjectMenu' then
      begin
        IDEProjectItem := Items[I];
        Break;
      end;
  Assert(IDEProjectItem <> nil,'Unable to locate Project menu');
  with IDEProjectItem do
    for I := 0 to Count - 1 do
      if Items[I].Name = 'ProjectInformationItem' then
      begin
        IDEProjectItem.Insert(I + 1, FBuildMenuItem);
        System.Break;
      end;
  Assert(FBuildMenuItem.Parent <> nil,'Unable to insert Build and Sign menu item');
  FBuildMenuItem.Visible := True;
{$IFNDEF D5UP}
  FSaveAllAction := nil;
  with IDEActionList do
    for I := 0 to ActionCount - 1 do
      if Actions[I].Name = 'FileSaveAllCommand' then
      begin
        FSaveAllAction := TAction(Actions[I]);
        Break;
      end;
  Assert(FSaveAllAction <> nil);
{$ENDIF D5UP}
end;

procedure TStrSecIISignCodeWizard.SetIndex(const Value: Integer);
begin
  FIndex := Value;
end;

function TStrSecIISignCodeWizard.SubstitutePath(
  const Path: string): string;
var
  I: Integer;
  Name: string;
begin
  if FEnvVariables.Count = 0 then
    ReadEnvVariables;
  Result := Path;
  if Pos('$(', Result) > 0 then
    for I := 0 to FEnvVariables.Count - 1 do begin
      Name := FEnvVariables.Names[I];
      Result := StringReplace(Result, Format('$(%s)', [Name]), FEnvVariables.Values[Name], [rfReplaceAll, rfIgnoreCase]);
    end;
end;

procedure TStrSecIISignCodeWizard.UnregisterCommand;
begin
  FBuildMenuItem.Free;
  FBuildAction.Free;
end;

resourcestring
  sStrSecIISignCodeWizardError = 'Error creating StrSecIISignCodeWizard wizard';

procedure InitStrSecIISignCodeWizard;
begin
  if (BorlandIDEServices <> nil) then begin
    StrSecIISignCodeWizard := TStrSecIISignCodeWizard.Create;
    StrSecIISignCodeWizard.Index := (BorlandIDEServices as IOTAWizardServices).AddWizard(StrSecIISignCodeWizard as IOTAWizard);
    if StrSecIISignCodeWizard.Index < 0 then
      raise EStrSecIISignCodeWizard.Create(sStrSecIISignCodeWizardError);
  end;
end;

procedure DoneStrSecIISignCodeWizard;
begin
  if (BorlandIDEServices <> nil) then
    (BorlandIDEServices as IOTAWizardServices).RemoveWizard(StrSecIISignCodeWizard.Index);
end;

initialization
  InitStrSecIISignCodeWizard;

finalization
  DoneStrSecIISignCodeWizard;

end.
