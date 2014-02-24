unit AttachmentsDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, PostFrame, ExtDlgs;

type
  TdlgAttachments = class(TForm)
    ListView1: TListView;
    btnAdd: TButton;
    btnRemove: TButton;
    btnClose: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure btnAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fPostFrame: TfmePost;
  protected
    procedure UpdateActions; override;
  public
    property PostFrame: TfmePost read fPostFrame write fPostFrame;
    { Public declarations }
  end;

var
  dlgAttachments: TdlgAttachments;

implementation

uses unitNewsThread;

{$R *.dfm}

procedure TdlgAttachments.ListView1Data(Sender: TObject; Item: TListItem);
var
  attachment: TAttachment;
begin
  attachment := PostFrame.Attachment[item.Index];

  Item.Caption := attachment.FileName;
  Item.SubItems.Add(attachment.Directory);
  Item.SubItems.Add(DateToStr(attachment.Date));
  Item.SubItems.Add(IntToStr(attachment.Size));
end;

procedure TdlgAttachments.btnAddClick(Sender: TObject);
var
  i: Integer;
begin
  if OpenPictureDialog1.Execute(Handle) then
  begin
    for i := 0 to OpenPictureDialog1.Files.Count - 1 do
      PostFrame.AddAttachment(OpenPictureDialog1.Files[i]);
    ListView1.Items.Count := PostFrame.AttachmentCount;
    ListView1.Invalidate;
  end;
end;

procedure TdlgAttachments.FormCreate(Sender: TObject);
begin
  with ListView1 do
    Columns[1].Width := Width - Columns[0].Width - Columns[2].Width - Columns[3].Width - 4;
end;

procedure TdlgAttachments.btnRemoveClick(Sender: TObject);
begin
  if Assigned(ListView1.Selected) then
  begin
    PostFrame.RemoveAttachment(ListView1.Selected.Index);
    ListView1.Items.Count := PostFrame.AttachmentCount;
    ListView1.Invalidate;
  end;
end;

procedure TdlgAttachments.UpdateActions;
begin
  btnRemove.Enabled := Assigned(ListView1.Selected);
end;

procedure TdlgAttachments.FormShow(Sender: TObject);
begin
  ListView1.Items.Count := PostFrame.AttachmentCount;
end;

end.
