unit CheckMessageDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TdlgCheckMessagebase = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    btnYes: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    Label2: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgCheckMessagebase: TdlgCheckMessagebase;

implementation

{$R *.dfm}

end.
