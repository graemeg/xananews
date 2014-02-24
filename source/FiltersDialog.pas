(*======================================================================*
 | FiltersDialog unit for NewsReader3                                   |
 |                                                                      |
 | Display Filters dialog                                               |
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

unit FiltersDialog;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, unitNNTPServices, ImgList;

type
  TdlgFilters = class(TForm)
    btnClose: TButton;
    lvFilters: TListView;
    btnAdd: TButton;
    btnUpdate: TButton;
    btnDelete: TButton;
    procedure FormShow(Sender: TObject);
    procedure lvFiltersClick(Sender: TObject);
    procedure lvFiltersChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure UpdateActions; override;
  public
    obj : TFiltersCtnr;
  end;

var
  dlgFilters: TdlgFilters;

implementation

{$R *.dfm}

uses MainForm, unitNNTPFilters, FilterDetailsDialog;

(*----------------------------------------------------------------------*
 | TdlgFilters.FormShow                                                 |
 |                                                                      |
 | Initialize the form                                                  |
 |                                                                      |
 | Parameters:                                                          |
 |   Sender: TObject                                                    |
 *----------------------------------------------------------------------*)
procedure TdlgFilters.FormShow(Sender: TObject);
var
  i : Integer;
  filter : TNNTPFilter;
begin
  lvFilters.Items.BeginUpdate;          // Fill in the filters list view
  try
    lvFilters.Checkboxes := Assigned (obj);
    for i := 0 to AllFilters.Count - 1 do
    begin
      filter := TNNTPFilter (AllFilters.Items [i]);
      with lvFilters.Items.Add do
      begin
        Caption := filter.Name;
        SubItems.Add (Filter.Text);
        data := filter;
        if Assigned (obj) and Assigned (obj.Filters) then
          Checked := obj.Filters.IndexOf (filter) <> -1
        else
          Checked := False
      end
    end
  finally
    lvFilters.Items.EndUpdate;
  end;

  if lvFilters.Items.Count > 0 then     // Select the first filter
  begin
    lvFilters.Items [0].Selected := True;
    lvFiltersClick (Nil);
  end
end;

procedure TdlgFilters.lvFiltersClick(Sender: TObject);
begin
end;

(*----------------------------------------------------------------------*
 | TdlgFilters.UpdateActions                                            |
 |                                                                      |
 | Enable/Disable controls based on state                               |
 |                                                                      |
 | Parameters:                                                          |
 |   None                                                               |
 *----------------------------------------------------------------------*)
procedure TdlgFilters.UpdateActions;
begin
  btnDelete.Enabled := Assigned (lvFilters.Selected);
  btnUpdate.Enabled := Assigned (lvFilters.Selected);
end;

(*----------------------------------------------------------------------*
 | TdlgFilters.lvFiltersChange                                          |
 |                                                                      |
 | OnChange handler for the list box.  A change has been made to a list |
 | item.  This might have been checking / unchecking it's checkbox...   |
 |                                                                      |
 | Parameters:                                                          |
 |   Sender: TObject;                                                   |
 |   Item: TListItem;                                                   |
 |   Change: TItemChange                                                |
 *----------------------------------------------------------------------*)
procedure TdlgFilters.lvFiltersChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if Assigned (obj) then
    obj.EnableFilter (TNNTPFilter (item.Data), item.Checked)
end;

procedure TdlgFilters.btnAddClick(Sender: TObject);
var
  dlg : TdlgFilterDetails;
  f, filter : TNNTPFilter;
begin
  filter := Nil;
  dlg := TdlgFilterDetails.Create(nil);
  try
    filter := TNNTPFilter.Create;
    dlg.Filter := filter;

    if dlg.ShowModal = mrOK then
    begin
      filter.Save(fmMain.PersistentPosition.ApplicationKey);
      AllFilters.AddFilter(filter);
      f := Filter;
      filter := Nil;
      with lvFilters.Items.Add do
      begin
        Caption := f.Name;
        SubItems.Add(f.Text);
        data := f;
        if Assigned (obj) and Assigned (obj.Filters) then
          Checked := obj.Filters.IndexOf (f) <> -1
        else
          Checked := False
      end
    end
  finally
    filter.Free;
    dlg.Free
  end
end;

procedure TdlgFilters.btnDeleteClick(Sender: TObject);
var
  filter : TNNTPFilter;
begin
  if Assigned (lvFilters.Selected) then
  begin
    filter := lvFilters.Selected.Data;
    AllFilters.Remove(filter);
    AllFilters.Save(fmMain.PersistentPosition.ApplicationKey);
    lvFilters.Selected.Free
  end
end;

procedure TdlgFilters.btnUpdateClick(Sender: TObject);
var
  dlg : TdlgFilterDetails;
begin
  if Assigned (lvFilters.Selected) then
  begin
    dlg := TdlgFilterDetails.Create(nil);
    try
      dlg.Filter := lvFilters.Selected.Data;
      if dlg.ShowModal = mrOK then
      begin
        AllFilters.Save(fmMain.PersistentPosition.ApplicationKey);
        lvFilters.Selected.Caption := dlg.Filter.Name;
        lvFilters.Selected.SubItems [0] := dlg.Filter.Text
      end
    finally
      dlg.Free
    end
  end
end;

end.
