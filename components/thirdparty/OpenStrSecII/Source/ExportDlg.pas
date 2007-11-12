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
unit ExportDlg;

interface

uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
{$ENDIF}
{$IFDEF LINUX}
  Libc, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls,
{$ENDIF}
  ImportDlg;

type
  TfrmExportDlg = class(TfrmImportDlg)
    cbShortCut: TComboBox;
    lblShortCut: TLabel;
    procedure btnPasteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  SsCertMgrUtils;

{$IFDEF MSWINDOWS}
{$R *.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

procedure TfrmExportDlg.btnPasteClick(Sender: TObject);
begin
  memImport.SelectAll;
  memImport.CopyToClipboard;
end;

procedure TfrmExportDlg.FormCreate(Sender: TObject);
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    TSsCertMgrPaths.GetNamesAndPaths(SL);
    for I := 0 to SL.Count - 1 do
      cbShortCut.Items.Add(SL.Names[I]);
  finally
    SL.Free;
  end;
end;

procedure TfrmExportDlg.FormShow(Sender: TObject);
begin
  inherited;
  lblShortCut.Visible := cbShortCut.Visible;
end;

end.
