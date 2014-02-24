unit FindOnInternetDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TdlgFindMessageOnInternet = class(TForm)
    Label1: TLabel;
    edMessageID: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgFindMessageOnInternet: TdlgFindMessageOnInternet;

procedure GotoMessageOnInternet (handle : HWND; url, mid : string);

implementation

uses ShellApi, unitNewsReaderOptions, XnCoderQuotedPrintable, IdCoder;

{$R *.dfm}

procedure GotoMessageOnInternet (handle : HWND; url, mid : string);
var
  qid : string;
begin
  if Length (mid) < 2 then Exit;

  if mid [1] = '<' then Delete (mid, 1, 1);
  if mid [Length (mid)] = '>' then Delete (mid, Length (mid), 1);

  url := StringReplace (url, '%id%', mid, [rfReplaceAll]);

  if Pos ('%qid%', url) > 0 then
  begin
    qid := TXnEncoderQuotedPrintable.EncodeString(mid);
    url := StringReplace (url, '%qid%', qid, [rfReplaceAll]);
  end;

  ShellExecute (handle, 'open', PChar (url), nil, nil, SW_SHOW)
end;

procedure TdlgFindMessageOnInternet.btnOKClick(Sender: TObject);
begin
  GotoMessageOnInternet (Handle, XNOptions.SearchInternetURLStub, edMessageId.Text);
end;

end.
