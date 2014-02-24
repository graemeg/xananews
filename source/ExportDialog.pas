unit ExportDialog;

interface

uses
  Windows, SysUtils, Messages, Classes, Dialogs, Controls, CommDlg, ExportSettingsFrame, ExtCtrls;

type
  TExportDialog = class(TSaveDialog)
  private
    FSettingsFrame : TfmeExportSettings;
    FSettingsPanel : TPanel;
    FGroups : TStrings;
    FExportSettings : boolean;

    procedure WmSize (var msg : TwmSize); message WM_SIZE;

    { Private declarations }
  protected
    procedure DoShow; override;
    procedure DoClose; override;
  public
    function Execute: Boolean; override;
    destructor Destroy; override;
    procedure FreeGroups;

    property ExportSettings : boolean read fExportSettings;
    property Groups : TStrings read fGroups;
  end;

implementation

{ TExportDialog }

destructor TExportDialog.Destroy;
begin
  FGroups.Free;

  inherited;
end;

procedure TExportDialog.DoClose;
begin
  inherited;
  fExportSettings := FSettingsFrame.ExportSettings;
  if not Assigned (FGroups) then
    FGroups := TStringList.Create;

  fSettingsFrame.GetGroups(FGroups);
  FreeAndNil (FSettingsPanel);
end;

procedure TExportDialog.DoShow;
var
  PreviewRect, StaticRect: TRect;
  h : Integer;
begin
  inherited;

  GetClientRect(Handle, PreviewRect);   // Get entire dialog
  StaticRect := GetStaticRect;          // Get standard dialog size

  h := PreviewRect.Bottom - StaticRect.Bottom;

  PreviewRect.Top := StaticRect.Bottom + 4;
  PreviewRect.Right := StaticRect.Right;
  PreviewRect.Bottom := PreviewRect.Top + h;

                                        // Frame seems to have to have a panel
                                        // to parent it.
  FSettingsPanel := TPanel.Create(self);
  FSettingsPanel.ParentWindow := Handle;
  FSettingsPanel.BoundsRect := PreviewRect;
  FSettingsPanel.BevelInner := bvNone;
  FSettingsPanel.BevelOuter := bvNone;
  FSettingsPanel.FullRepaint := False;

  FSettingsFrame := TfmeExportSettings.Create(Self);
  FSettingsFrame.Parent := FSettingsPanel;
  FSettingsFrame.Align := alClient;
  FSettingsFrame.Initialize;
end;

function TExportDialog.Execute: Boolean;
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
    Template := 'DLGTEMPLATE3' else
    Template := nil;
  Result := DoExecute(@GetSaveFileName);
end;

procedure TExportDialog.FreeGroups;
begin
  FGroups.Free;
end;

procedure TExportDialog.WmSize (var msg: TwmSize);
begin
  inherited;

  if Assigned (FSettingsPanel) then
    FSettingsPanel.Width := msg.Width - 8
end;

end.
