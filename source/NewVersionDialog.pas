unit NewVersionDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfmNewVersionNotification = class(TForm)
    Label1: TLabel;
    Image1: TImage;
    Label2: TLabel;
    btnYes: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmNewVersionNotification: TfmNewVersionNotification;

implementation

{$R *.dfm}

end.
