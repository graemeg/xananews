unit PropertyPageSortingForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyPageForm, PropertyPageDefaultsForm, StdCtrls, ExtCtrls, unitSettings;

type
  TPropertyPageSortingData = class (TPropertyPageData)
  private
    fSettings : TDisplaySettings;
  protected
    procedure Initialize; override;
  public
    function Apply : boolean; override;
    destructor Destroy; override;
  end;

  TfmPropertyPageSorting = class(TfmPropertyPageDefaults)
    Label10: TLabel;
    cbSortOrder: TComboBox;
    Label9: TLabel;
    cbGroupSubjects: TCheckBox;
    Label1: TLabel;
    rbThreaded: TRadioButton;
    rbChronological: TRadioButton;
    Panel2: TPanel;
    rbDescending: TRadioButton;
    rbAscending: TRadioButton;
    procedure ControlClick(Sender: TObject);
  private
    fData : TPropertyPageSortingData;
    procedure UpdateData;
    procedure InitializeControls (settings : TDisplaySettings);
  protected
    function CanRestoreParentSettings : boolean; override;
    procedure RestoreParentSettings; override;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
    { Public declarations }
  end;

var
  fmPropertyPageSorting: TfmPropertyPageSorting;

implementation

uses NewsGlobals;

{$R *.dfm}

{ TPropertyPageSortingData }

function TPropertyPageSortingData.Apply : boolean;
var
  settings : TDisplaySettings;
begin
  result := True;
  settings := TDisplaySettings (Param);

  settings.ThreadOrder := fSettings.ThreadOrder;
  settings.ThreadSortOrder := fSettings.ThreadSortOrder;
  settings.ThreadSortDirection := fSettings.ThreadSortDirection;
  settings.GatherSubjects := fSettings.GatherSubjects;
end;

destructor TPropertyPageSortingData.Destroy;
begin
  fSettings.Free;

  inherited;
end;

procedure TPropertyPageSortingData.Initialize;
var
  settings : TDisplaySettings;
begin
  settings := TDisplaySettings (Param);
  fSettings := TDisplaySettings.Create(settings.Parent);
  fSettings.Assign(settings);
end;

{ TfmPropertyPageDefaults1 }

procedure TfmPropertyPageSorting.PopulateControls(
  AData: TPropertyPageData);
begin
  inherited;

  fData := AData as TPropertyPageSortingData;
  InitializeControls (fData.fSettings);
end;

procedure TfmPropertyPageSorting.UpdateData;
var
  idx : Integer;
begin
  if Populating then Exit;
  idx := cbSortOrder.ItemIndex;
  if idx = 5 then
    idx := 6;
  fData.fSettings.ThreadSortOrder := TThreadSortOrder (idx);
  if rbAscending.Checked then
    fData.fSettings.ThreadSortDirection := sdAscending
  else
    fData.fSettings.ThreadSortDirection := sdDescending;

  if rbThreaded.Checked then
    fData.fSettings.ThreadOrder := toThreaded
  else
    fData.fSettings.ThreadOrder := toChronological;

  fData.fSettings.GatherSubjects := cbGroupSubjects.Checked
end;

procedure TfmPropertyPageSorting.ControlClick(Sender: TObject);
begin
  UpdateData;
end;

class function TfmPropertyPageSorting.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageSortingData
end;

function TfmPropertyPageSorting.CanRestoreParentSettings: boolean;
var
  settings : TDisplaySettings;
begin
  settings := TDisplaySettings (fData.Param);
  result := Assigned (settings.Parent);
end;

procedure TfmPropertyPageSorting.RestoreParentSettings;
begin
  fData.fSettings.Assign(fData.fSettings.Parent);
  Populating := True;
  InitializeControls (fData.fSettings);
  Populating := False;
end;

procedure TfmPropertyPageSorting.InitializeControls(
  settings: TDisplaySettings);
var
  idx : Integer;
begin
  idx := Integer (settings.ThreadSortOrder);
  if idx = 6 then
    idx := 5;
  cbSortOrder.ItemIndex := idx;
  if settings.ThreadSortDirection = sdAscending then
    rbAscending.Checked := True
  else
    rbDescending.Checked := True;

  if settings.ThreadOrder = toThreaded then
    rbThreaded.Checked := True
  else
    rbChronological.Checked := True;

  cbGroupSubjects.Checked := settings.GatherSubjects;
end;

end.
