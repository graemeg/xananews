unit ServerAdminRemoveGroupDialog;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TdlgServerAdminRemoveGroup = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    edGroupName: TEdit;
    Label2: TLabel;
    edApproved: TEdit;
    Label3: TLabel;
    mmoReason: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgServerAdminRemoveGroup: TdlgServerAdminRemoveGroup;

implementation

{$R *.dfm}

end.
