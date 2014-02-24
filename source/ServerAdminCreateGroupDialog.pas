unit ServerAdminCreateGroupDialog;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TdlgServerAdminCreateGroup = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    edGroupName: TEdit;
    cbModerated: TCheckBox;
    Label2: TLabel;
    edApproved: TEdit;
    Label3: TLabel;
    edDescription: TEdit;
    lbModeratorSubmissionAddress: TLabel;
    lbModeratorContactAddress: TLabel;
    edModeratorSubmissionAddress: TEdit;
    edModeratorContactAddress: TEdit;
    Label6: TLabel;
    mmoCharter: TMemo;
    procedure cbModeratedClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgServerAdminCreateGroup: TdlgServerAdminCreateGroup;

implementation

{$R *.dfm}

procedure TdlgServerAdminCreateGroup.cbModeratedClick(Sender: TObject);
var
  chk : boolean;
begin
  chk := cbModerated.Checked;
  lbModeratorContactAddress.Enabled := chk; edModeratorContactAddress.Enabled := chk;
  lbModeratorSubmissionAddress.Enabled := chk; edModeratorSubmissionAddress.Enabled := chk;
end;

end.
