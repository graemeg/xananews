unit CheckCrosspostDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TdlgCheckCrosspost = class(TForm)
    Label1: TLabel;
    rbGoAhead: TRadioButton;
    rbFollowup: TRadioButton;
    rbFirstGroupOnly: TRadioButton;
    btnOK: TButton;
    btnCancel: TButton;
  end;

var
  dlgCheckCrosspost: TdlgCheckCrosspost;

implementation

uses unitNewsReaderOptions;

{$R *.dfm}

{ TdlgCheckCrosspost }

end.
