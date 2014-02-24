unit BozoDetailsDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TdlgBozoDetails = class(TForm)
    Label1: TLabel;
    edName: TEdit;
    Label2: TLabel;
    edEMail: TEdit;
    Label3: TLabel;
    dpDate: TDateTimePicker;
    btnOK: TButton;
    btnCancel: TButton;
    cbUseName: TCheckBox;
    cbUseEmail: TCheckBox;
    cbUseKeyphrase: TCheckBox;
    Label4: TLabel;
    edKeyphrase: TEdit;
    Bevel1: TBevel;
    Label5: TLabel;
    rbIgnore: TRadioButton;
    rbMarkAsRead: TRadioButton;
    rbIgnoreThread: TRadioButton;
    rbMarkAsReadThread: TRadioButton;
    rbDontDownload: TRadioButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgBozoDetails: TdlgBozoDetails;

implementation

{$R *.dfm}

end.
