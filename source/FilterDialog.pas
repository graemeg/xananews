(*======================================================================*
 | FilterDialog unit for NewsReader3                                    |
 |                                                                      |
 | Display news filter details dialog                                   |
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
 | Copyright © Colin Wilson 2002  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      29/01/2002  CPWW  Original                                  |
 *======================================================================*)

unit FilterDialog;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, unitNNTPFilters;

type
  TdlgDeleteMessages = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    rgColumn: TRadioGroup;
    rgOperation: TRadioGroup;
    Button1: TButton;
    edFilter: TEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure rgColumnClick(Sender: TObject);
    procedure rgOperationClick(Sender: TObject);
    procedure edFilterChange(Sender: TObject);
  private
    fFilter: TNNTPFilter;
    fChanges: Boolean;
    function GetFilter: TNNTPFilter;
  protected
    procedure UpdateActions; override;
  public
    destructor Destroy; override;
    property Filter: TNNTPFilter read GetFilter;
  end;

var
  dlgDeleteMessages: TdlgDeleteMessages;

implementation

{$R *.dfm}

uses
  MainForm, AddFilterDialog;

{ TdlgDeleteMessages }

procedure TdlgDeleteMessages.UpdateActions;
begin
  inherited;

  OKBtn.Enabled := edFilter.Text <> '';
  Button1.Enabled := OKBtn.Enabled and (rgColumn.ItemIndex < Ord(ftNumber));
end;

procedure TdlgDeleteMessages.Button1Click(Sender: TObject);
var
  dlg: TdlgAddFilter;
begin
  Application.CreateForm(TdlgAddFilter, dlg);
  try
    dlg.Filter := Filter;

    if dlg.ShowModal = mrOK then
    begin
      Filter.Name := dlg.edFilterName.Text;
      Filter.Save;
      AllFilters.AddFilter(TNNTPFilter.Clone(Filter));
    end;
  finally
    dlg.Free;
  end;
end;

function TdlgDeleteMessages.GetFilter: TNNTPFilter;
var
  column: TNNTPFilterColumn;
  operator: TNNTPFilterOperator;
  unread, interesting, caseSensitive: Boolean;
  i: Int64;
  dt: TDateTime;
begin
  if fChanges or not Assigned(fFilter) then
  begin
    fFilter.Free;
    fChanges := False;

    column := TNNTPFilterColumn(rgColumn.ItemIndex);
    operator := TNNTPFilterOperator(rgOperation.ItemIndex);

    unread := False;
    interesting := False;
    caseSensitive := False;

    case column of
      ftSubject,
      ftAuthor,
      ftMessageBody,
      ftMessageID,
      ftHeaderLines:
        fFilter := TNNTPFilter.Create('', column, operator, edFilter.Text, unread, interesting, caseSensitive);

      ftDate:
        if TryStrToDateTime(edFilter.Text, dt) then
          fFilter := TNNTPFilter.Create('', column, operator, dt, unread, interesting, caseSensitive);

      ftLines,
      ftCrossPosted,
      ftNumber:
        if TryStrToInt64(edFilter.Text, i) then
          fFilter := TNNTPFilter.Create('', column, operator, i, unread, interesting, caseSensitive);
    end;
  end;

  Result := fFilter;
end;

destructor TdlgDeleteMessages.Destroy;
begin
  fFilter.Free;
  inherited Destroy;
end;

procedure TdlgDeleteMessages.rgColumnClick(Sender: TObject);
begin
  fChanges := True;
end;

procedure TdlgDeleteMessages.rgOperationClick(Sender: TObject);
begin
  fChanges := True;
end;

procedure TdlgDeleteMessages.edFilterChange(Sender: TObject);
begin
  fChanges := True;
end;

end.
