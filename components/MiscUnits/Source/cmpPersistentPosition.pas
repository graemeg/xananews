(*======================================================================*
 | cmpPersistentPosition unit for MiscUnits package                     |
 |                                                                      |
 | Drop one on your main form, and it will run in the same position     |
 | as when it was previously closed.                                    |
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
 | Copyright © Colin Wilson 2005.  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      26/02/2002  CPWW  Original                                  |
 | 1.1      11/01/2005  CPWW  Fixed bug with saving position when       |
 |                            maximized.                                |
 | 1.2      12/05/2005  CPWW  Big changes to cope with RecreateHandle   |
 |                            happening on the owner form.              |
 |                                                                      |
 |                            Now restores 'minimized' state            |
 *======================================================================*)

unit cmpPersistentPosition;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, unitExSettings;

type
//------------------------------------------------------------------------
// TPersistentPosition class

  TOnGetSettingsClass = procedure (Owner : TObject; var SettingsClass : TExSettingsClass) of object;
  TOnGetSettingsFile = procedure (Owner : TObject; var fileName : string) of object;
  TPersistentPosition = class(TComponent)
  private
    fOldOwnerWindowMethod : TWndMethod;
    fObjectInstance : pointer;
    fManufacturer: string;
    fProduct: string;
    fVersion: string;
    fSubKey: string;
    fSaved : boolean;
    fMoved : boolean;
    fEnabled: boolean;
    fSubclassed : boolean;
    fOnGetSettingsClass: TOnGetSettingsClass;
    fOnGetSettingsFile: TOnGetSettingsFile;
    procedure OwnerWindowMethod (var msg : TMessage);
    function GetAppKey: string;
    procedure MoveToPosition;
    function GetPosition: TRect;
    procedure SavePosition;
    function CreateReg(canCreate: boolean; var reg : TExSettings): boolean;
    procedure Subclass;
    procedure UnSubClass;
  protected
    procedure Loaded; override;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    property ApplicationKey : string read GetAppKey;
    { Public declarations }

    function GetValue (const valueName : string) : Integer;
    function GetSzValue (const valueName : string) : string;
    procedure SetValue (const valueName : string; value : Integer);
    procedure SetSzValue (const valueName, value : string);

    property Position : TRect read GetPosition;

  published
    property Manufacturer : string read fManufacturer write fManufacturer;
    property Version : string read fVersion write fVersion;
    property Product : string read fProduct write fProduct;
    property SubKey : string read fSubKey write fSubKey;
    property Enabled : boolean read fEnabled write fEnabled default True;

    property OnGetSettingsClass : TOnGetSettingsClass read fOnGetSettingsClass write fOnGetSettingsClass;
    property OnGetSettingsFile : TOnGetSettingsFile read fOnGetSettingsFile write fOnGetSettingsFile;
    { Published declarations }
  end;

implementation

uses unitExFileSettings, unitExRegSettings;

resourcestring
  rstWoozle = 'Woozle';

{ TPersistentPosition }

{*----------------------------------------------------------------------*
 | constructor TPersistentPosition.Create                               |
 *----------------------------------------------------------------------*}
constructor TPersistentPosition.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  fEnabled := True;
  fManufacturer := rstWoozle;
end;

{*----------------------------------------------------------------------*
 | function TPersistentPosition.CreateReg                               |
 |                                                                      |
 | Create a TExSettings object for the application\position             |
 |                                                                      |
 | The function returns false if the registry key couldn't be opened    |
 | - maybe because canCreate was false and it didn't already exist      |
 *----------------------------------------------------------------------*}
function TPersistentPosition.CreateReg(canCreate: boolean; var reg : TExSettings): boolean;
var
  prod : string;
  settingsClass : TExSettingsClass;
  st : string;
begin
  if Product = '' then
    prod := Application.Title
  else
    prod := Product;

  settingsClass := TExRegSettings;
  if Assigned (OnGetSettingsClass) then
    OnGetSettingsClass (self, settingsClass);

  reg := settingsClass.Create (Manufacturer, prod, Version);
  try
    if (reg is TExFileSettings) and Assigned (fOnGetSettingsFile) then
    begin
      st := '';
      OnGetSettingsFile (self, st);
      TExFileSettings (reg).CustomPath := st
    end;

    if fSubkey = '' then
      reg.Section := 'Position'
    else
      reg.Section := fSubkey + '\Position';

    result := reg.Open(not canCreate);
  except
    FreeAndNil (reg);
    raise
  end
end;

(*----------------------------------------------------------------------*
 | TPersistentPosition.Destroy                                          |
 |                                                                      |
 | Destructor - Un-subclass main window.                                |
 *----------------------------------------------------------------------*)
destructor TPersistentPosition.Destroy;
begin
  UnSubclass;
  if Assigned (fObjectInstance) then
    Classes.FreeObjectInstance (fObjectInstance);
  inherited;
end;

{*----------------------------------------------------------------------*
 | functionTPersistentPosition.GetAppKey                                |
 |                                                                      |
 | 'Get' method for the ApplicationKey property                         |
 *----------------------------------------------------------------------*}
function TPersistentPosition.GetAppKey: string;
var
  prod : string;
begin
  if Product = '' then
    prod := Application.Title
  else
    prod := Product;

  Result := 'Software';
  if Manufacturer <> '' then
    Result := Result + '\' + Manufacturer;

  Result := Result + '\' + Prod;
  if Version <> '' then
    Result := Result + '\' + Version;

  if SubKey <> '' then
    Result := Result + '\' + SubKey
end;

{*----------------------------------------------------------------------*
 | function TPersistentPosition.GetSzValue                              |
 |                                                                      |
 | Get a string value from the Position area in the registry            |
 *----------------------------------------------------------------------*}
function TPersistentPosition.GetSzValue(const valueName: string): string;
var
  reg : TExSettings;
begin
  if CreateReg (false, reg) then
  try
    result := reg.StringValue [valueName];
  finally
    reg.Free
  end
end;

{*----------------------------------------------------------------------*
 | function TPersistentPosition.GetValue                                |
 |                                                                      |
 | Get an integer value from the Position area in the registry          |
 *----------------------------------------------------------------------*}
function TPersistentPosition.GetValue(const valueName: string): Integer;
var
  reg : TExSettings;
begin
  if CreateReg (false, reg) then
  try
    result := reg.GetIntegerValue(valueName, 0);
  finally
    reg.Free
  end
  else
    result := 0
end;

{*----------------------------------------------------------------------*
 | procedure TPersistentPosition.Loaded                                 |
 |                                                                      |
 | Override 'Loaded' to subclass the form's window handle so we can     |
 | intercept it's messages                                              |
 *----------------------------------------------------------------------*}
procedure TPersistentPosition.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    Subclass;
    if Application.MainForm = Nil then
      MoveToPosition
  end
end;

{*----------------------------------------------------------------------*
 | procedure TPersistentPosition.MoveToPosition                         |
 |                                                                      |
 | Move to the saved position                                           |
 *----------------------------------------------------------------------*}
procedure TPersistentPosition.MoveToPosition;
var
  fm : TForm;
  wp : TWindowPlacement;
  wasVisible : boolean;
  reg : TExSettings;
begin
  if fMoved or not fEnabled then Exit;
  fMoved := True;
  if Owner is TForm then
  begin
    fm := TForm (Owner);

    if CreateReg (false, reg) then
    try
      try
        FillChar (wp, sizeof (wp), 0);
        wp.length := sizeof (wp);

        wasVisible := fm.Visible;
        if wasVisible then
          case TWindowState (reg.GetIntegerValue ('State', Integer (wsNormal))) of
            wsNormal : wp.ShowCmd := SW_SHOW;
//            wsMinimized : wp.showCmd := SW_SHOWMINIMIZED; doesn't work so well when mainforontaskbar is set
            wsMaximized : wp.showCmd := SW_SHOWMAXIMIZED
          end
        else
          wp.ShowCmd := 0;

        // You'd be tempted to set ShowCmd to SW_SHOW,
        // SW_SHOWMAXIMIZED, etc.  But that causes a blank
        // window to be shown then filled in after a short
        // delay - which looks ghastly!

        wp.rcNormalPosition.Left := reg.GetIntegerValue ('Left', -1);
        wp.rcNormalPosition.Top := reg.GetIntegerValue ('Top', -1);
        wp.rcNormalPosition.Right := reg.GetIntegerValue ('Width', -1) + wp.rcNormalPosition.Left;
        wp.rcNormalPosition.Bottom := reg.GetIntegerValue ('Height', -1) + wp.rcNormalPosition.Top;

        if (wp.rcNormalPosition.Left   <> -1) and
           (wp.rcNormalPosition.Top    <> -1) and
           (wp.rcNormalPosition.Right  <> -1) and
           (wp.rcNormalPosition.Bottom <> -1) then
          SetWindowPlacement (fm.Handle, @wp);

        if not wasVisible then
          fm.WindowState := TWindowState (reg.GetIntegerValue ('State', Integer (wsNormal)));
      except
      end
    finally
      reg.Free
    end
  end
end;

procedure TPersistentPosition.SavePosition;
var
  fm : TForm;
  state : Integer;
  wp : TWindowPlacement;
  reg : TExSettings;
begin
  if fSaved then Exit;
  fSaved := True;

  if fEnabled and (Owner is TForm) then
  begin
    fm := TForm (Owner);
    FillChar (wp, sizeof (wp), 0);
    wp.length := sizeof (wp);
    GetWindowPlacement (fm.Handle, @wp);

//    if IsIconic (Application.Handle) then
//      state := Ord (wsMinimized)
//    else
      case wp.showCmd of
//        SW_SHOWMINIMIZED : state := Ord (wsMinimized);
        SW_SHOWMAXIMIZED : state := Ord (wsMaximized);
        else
          state := Ord (wsNormal)
      end;

    CreateReg (true, reg);
    with wp.rcNormalPosition do
    try
      reg.SetIntegerValue ('Left', Left, MaxInt);
      reg.SetIntegerValue ('Top', Top, MaxInt);
      reg.SetIntegerValue ('Width', Right - Left, MaxInt);
      reg.SetIntegerValue ('Height', Bottom - Top, MaxInt);
      reg.SetIntegerValue ('State', state, MaxInt);
    finally
      reg.Free
    end
  end
end;

{*----------------------------------------------------------------------*
 | procedure TPersistentPosition.SetValue                               |
 |                                                                      |
 | Set an integer value in the 'Position' area of the registry          |
 *----------------------------------------------------------------------*}
procedure TPersistentPosition.SetValue(const valueName: string;
  value: Integer);
var
  reg : TExSettings;
begin
  if CreateReg (true, reg) then
  try
    reg.IntegerValue  [valueName] := value
  finally
    reg.Free
  end
end;

{*----------------------------------------------------------------------*
 | procedure TPersistentPosition.SetValue                               |
 |                                                                      |
 | Set a string value in the 'Position' area of the registry            |
 *----------------------------------------------------------------------*}
procedure TPersistentPosition.SetSzValue(const valueName, value: string);
var
  reg : TExSettings;
begin
  if CreateReg (true, reg) then
  try
    reg.StringValue [valueName] := value
  finally
    reg.Free
  end
end;

function TPersistentPosition.GetPosition: TRect;
var
  reg : TExSettings;
begin
  if CreateReg (false, reg) then
  try
    try
      result := Rect (reg.IntegerValue ['Left'], reg.IntegerValue ['Top'], reg.IntegerValue ['Width'], reg.IntegerValue ['Height']);
      result.Right := result.Right + result.Left;
      result.Bottom := result.Bottom + result.Top;
    except
    end
  finally
    reg.Free
  end
  else
    result := Rect (0, 0, 0, 0);
end;

procedure TPersistentPosition.Subclass;
var
  ownerForm : TForm;
begin
  if not fSubclassed then
  begin
    if Owner is TForm then
    begin
      ownerForm := TForm (Owner);
      fOldOwnerWindowMethod := ownerForm.WindowProc;
      ownerForm.WindowProc := OwnerWindowMethod
    end;
    fSubclassed := True
  end
end;

procedure TPersistentPosition.UnSubClass;
var
  ownerForm : TForm;
begin
  if fSubclassed then
  begin
    if Owner is TForm then
    begin
      ownerForm := TForm (Owner);
      ownerForm.WindowProc := fOldOwnerWindowMethod
    end;

    fSubclassed := False
  end
end;

procedure TPersistentPosition.OwnerWindowMethod(var msg: TMessage);
begin
  case Msg.Msg of
    CM_SHOWINGCHANGED:
    begin
      fOldOwnerWindowMethod(msg);
      if not fMoved then
        MoveToPosition;
      Exit;
    end;
    WM_DESTROY : if csDestroying in ComponentState then
                   SavePosition;
  end;

  fOldOwnerWindowMethod (msg)
end;

end.
