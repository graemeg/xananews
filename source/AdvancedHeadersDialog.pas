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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgAdvancedHeaders: TdlgAdvancedHeaders;

implementation

{$R *.dfm}

end.
