unit MoveMessagebaseDialog;

interface

{$WARN UNIT_PLATFORM OFF}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cmpFileCopier, StdCtrls, ComCtrls, ExtCtrls, FileCtrl,
  cmpPersistentPosition, unitExSettings;

type
  TdlgMoveMessagebase = class(TForm)
    FileCopier1: TFileCopier;
    Panel1: TPanel;
    lbWarning: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    GroupBox1: TGroupBox;
    edNewLocation: TEdit;
    btnSelectFolder: TButton;
    pnlStatus: TPanel;
    ProgressBar1: TProgressBar;
    lbCopying: TLabel;
    lbCurrentFile: TLabel;
    rbDefaultLocation: TRadioButton;
    rbCustomLocation: TRadioButton;
    PersistentPosition1: TPersistentPosition;
    procedure btnOKClick(Sender: TObject);
    procedure btnSelectFolderClick(Sender: TObject);
    procedure FileCopier1StartAnalysis(Sender: TObject);
    procedure FileCopier1EndCopy(Sender: TObject);
    procedure FileCopier1EndCopyFile(sender: TObject; const srcfileName, dstFileName: string; fileSize: Integer);
    procedure FileCopier1Exception(sender: TObject; e: Exception);
    procedure FileCopier1StartCopy(Sender: TObject);
    procedure FileCopier1StartCopyFile(sender: TObject; const srcfileName, dstFileName: string; fileSize: Integer);
    procedure FormShow(Sender: TObject);
    procedure PersistentPosition1GetSettingsClass(Owner: TObject; var SettingsClass: TExSettingsClass);
    procedure PersistentPosition1GetSettingsFile(Owner: TObject; var fileName: string);
    procedure rbDefaultLocationClick(Sender: TObject);
  private
    fSoFar: Int64;
    fWorking: Boolean;
  protected
    procedure UpdateActions; override;
  public
    { Public declarations }
  end;

var
  dlgMoveMessagebase: TdlgMoveMessagebase;

implementation

uses
  NewsGlobals, unitLog, unitNNTPServices;

{$R *.dfm}

procedure TdlgMoveMessagebase.btnOKClick(Sender: TObject);
var
  sr: TSearchRec;
  ext: string;
begin
  CloseLogFile;
  FileCopier1.SourceFiles.Clear;
  if FindFirst(gMessageBaseRoot + '\*.*', faAnyFile, sr) = 0 then
  try
    repeat
      if Copy(sr.Name, 1, 1) <> '.' then
      begin
        if (sr.Attr and faDirectory) <> 0 then
          FileCopier1.SourceFiles.Add(gMessageBaseRoot + '\' + sr.Name)
        else
        begin
          ext := ExtractFileExt(sr.Name);
          if SameText(ext, '.bmk') or
             SameText(ext, '.dat') or
             SameText(ext, '.sig') or
             SameText(ext, '.txt') then
            FileCopier1.SourceFiles.Add(gMessageBaseRoot + '\' + sr.Name);
        end;
      end;
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
  FileCopier1.DestFiles := edNewLocation.Text;
  fWorking := True;
  btnCancel.Enabled := False;
  FileCopier1.Start;
end;

procedure TdlgMoveMessagebase.FileCopier1StartAnalysis(Sender: TObject);
begin
  pnlStatus.Visible := True;
  lbCopying.Caption := 'Analysing files...';
end;

procedure TdlgMoveMessagebase.FileCopier1EndCopy(Sender: TObject);
var
  reg: TExSettings;
begin
  ProgressBar1.Position := 100;
  Application.ProcessMessages;
  reg := CreateExSettings;
  try
    reg.StringValue['Messagebase Directory'] := FileCopier1.DestFiles;
  finally
    reg.Free;
  end;
  gMessageBaseRoot := FileCopier1.DestFiles;

  lbCopying.Caption := 'Finished';
  lbCurrentFile.Caption := '';
  btnCancel.Caption := 'Close';
  btnCancel.Enabled := True;
  btnOK.Visible := False;
end;

procedure TdlgMoveMessagebase.FileCopier1EndCopyFile(sender: TObject;
  const srcfileName, dstFileName: string; fileSize: Integer);
begin
  Inc(fSoFar, filesize);
  ProgressBar1.Position := (fSoFar * 100) div FileCopier1.AnalyzedFileSize;
end;

procedure TdlgMoveMessagebase.FileCopier1StartCopy(Sender: TObject);
begin
  lbCopying.Caption := 'Copying files';
end;

procedure TdlgMoveMessagebase.FileCopier1StartCopyFile(sender: TObject;
  const srcfileName, dstFileName: string; fileSize: Integer);
var
  st: string;
begin
  st := ExtractFilePath(srcFileName);
  if Copy(st, Length(st), 1) = '\' then
    Delete(st, Length(st), 1);
  st := ExtractFileName(st);
  if st <> '' then
    st := st + '\';
  st := st + ExtractFilename(srcFileName);
  lbCurrentFile.Caption := st;
end;

procedure TdlgMoveMessagebase.btnSelectFolderClick(Sender: TObject);
const
  options: TSelectDirExtOpts = [sdNewFolder, sdShowShares, sdNewUI];
var
  dir: string;
begin
  dir := edNewLocation.Text;
  if SelectDirectory('Select folder to store the message base', '', dir, options, Self) then
    edNewLocation.Text := dir;
end;

procedure TdlgMoveMessagebase.FileCopier1Exception(sender: TObject; e: Exception);
begin
  lbCopying.Caption := 'Error';
  lbCurrentFile.Caption := e.Message;
  btnOK.Enabled := True;
  btnCancel.Enabled := True;
  fWorking := False;
end;

procedure TdlgMoveMessagebase.FormShow(Sender: TObject);
var
  st: string;
begin
  st := 'Your message base is currently stored in ';

  if gMessageBaseRoot = gBestMessageBaseLocation then
  begin
    rbDefaultLocation.Checked := True;
    st := st + 'the default location' + #13#10#13#10;
    st := st + 'This is usually the best place for it but you may want to move it if this disk drive is getting full';
  end
  else
  begin
    st := st + #13#10#13#10 + gMessageBaseRoot + #13#10#13#10;
    rbCustomLocation.Checked := True;
  end;

  edNewLocation.Text := gMessageBaseRoot;
  lbWarning.Caption := st;
end;

procedure TdlgMoveMessagebase.PersistentPosition1GetSettingsClass(
  Owner: TObject; var SettingsClass: TExSettingsClass);
begin
  SettingsClass := gExSettingsClass;
end;

procedure TdlgMoveMessagebase.PersistentPosition1GetSettingsFile(Owner: TObject;
  var fileName: string);
begin
  fileName := gExSettingsFile;
end;

procedure TdlgMoveMessagebase.rbDefaultLocationClick(Sender: TObject);
begin
  edNewLocation.Text := gBestMessageBaseLocation;
end;

procedure TdlgMoveMessagebase.UpdateActions;
begin
  edNewLocation.Enabled := not rbDefaultLocation.Checked;
  btnSelectFolder.Enabled := not rbDefaultLocation.Checked;

  btnOK.Enabled := not fWorking and (edNewLocation.Text <> '') and (edNewlocation.Text <> gMessageBaseRoot);
end;

end.
