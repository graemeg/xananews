unit AdvancedHeadersDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TdlgAdvancedHeaders = class(TForm)
    mmoAdvancedHeaders: TMemo;
    btnOK: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    procedure mmoAdvancedHeadersKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgAdvancedHeaders: TdlgAdvancedHeaders;

implementation

{$R *.dfm}

procedure TdlgAdvancedHeaders.mmoAdvancedHeadersKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    btnCancel.Click;
end;

end.
