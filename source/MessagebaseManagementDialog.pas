unit MessagebaseManagementDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TdlgMessagebaseManagement = class(TForm)
    rgManagementAction: TRadioGroup;
    gbWhen: TGroupBox;
    rbMoreThanAWeek: TRadioButton;
    rbMoreThanAMonth: TRadioButton;
    rbOlderThan: TRadioButton;
    DatePicker: TDateTimePicker;
    btnOK: TButton;
    btnCancel: TButton;
    GroupBox1: TGroupBox;
    rbSelectedGroups: TRadioButton;
    rbAllGroupsInSelectedAccount: TRadioButton;
    rbAllGroups: TRadioButton;
    TimePicker: TDateTimePicker;
    procedure rbOlderThanClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgMessagebaseManagement: TdlgMessagebaseManagement;

implementation

{$R *.dfm}

procedure TdlgMessagebaseManagement.rbOlderThanClick(Sender: TObject);
var
  enable : boolean;
begin
  enable := rbOlderThan.Checked;

  if enable then
  begin
    DatePicker.Enabled := True;
    TimePicker.Enabled := True;
    DatePicker.SetFocus
  end
  else
  begin
    DatePicker.Enabled := False;
    TimePicker.Enabled := False
  end
end;

procedure TdlgMessagebaseManagement.FormShow(Sender: TObject);
begin
  DatePicker.DateTime := now;
  TimePicker.DateTime := now
end;

end.
