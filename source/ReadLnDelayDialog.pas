unit ReadLnDelayDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TdlgReadLnDelay = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    edReadLnDelay: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgReadLnDelay: TdlgReadLnDelay;

implementation

{$R *.dfm}

end.
