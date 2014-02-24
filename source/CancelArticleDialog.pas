unit CancelArticleDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, unitNNTPServices;

type
  TdlgCancelArticles = class(TForm)
    Label1: TLabel;
    Image1: TImage;
    stNB: TLabel;
    stReason: TLabel;
    mmoReason: TMemo;
    btnOK: TButton;
    btnNo: TButton;
    Label3: TLabel;
    procedure FormShow(Sender: TObject);
  private
    fMultiSelect: boolean;
    fNotFromMe: boolean;
    fGroup: TSubscribedGroup;
    { Private declarations }
  public
    property MultiSelect : boolean read fMultiSelect write fMultiSelect;
    property NotFromMe : boolean read fNotFromMe write fNotFromMe;
    property Group : TSubscribedGroup read fGroup write fGroup;
  end;

var
  dlgCancelArticles: TdlgCancelArticles;

implementation

{$R *.dfm}

procedure TdlgCancelArticles.FormShow(Sender: TObject);
var
  icon : TIcon;
  h : hIcon;
  st : string;
begin
  icon := TIcon.Create;
  try
    icon.Width := 48;
    icon.Height := 48;
    h := LoadImage (0, MakeIntResource (OIC_QUES), IMAGE_ICON, 48, 48, LR_SHARED);
    icon.Handle := h;
    Image1.Picture.Icon := icon
  finally
    icon.Free
  end;

  if MultiSelect then
  begin
    st := '  *  You have selected more than one article';
    if NotFromMe then
      st := st + #13#10'  *  Some of the articles you have selected are not from you';
    stReason.Caption := 'Reason for cancelling these articles:';
  end
  else
    if NotFromMe then
      st := '  *  The article is not from you'
    else
      st := '';

  if st <> '' then
    st := 'Please note:'#13#10#10 + st;

  stNB.Caption := st;
end;

end.
