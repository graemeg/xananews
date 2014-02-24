unit TestPerformanceDialog;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, unitNNTPServices, unitMessages;

type
  TdlgTestPerformance = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    edDecodeTimes: TEdit;
    Label2: TLabel;
    lbResult: TLabel;
    Label3: TLabel;
    lblAverage: TLabel;
    Label4: TLabel;
    SpeedButton1: TSpeedButton;
    procedure OKBtnClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    fArticle: TArticleBase;
    fTotal : Extended;
    fRuns : Integer;
    { Private declarations }
  public
    property Article : TArticleBase read fArticle write fArticle;
  end;

var
  dlgTestPerformance: TdlgTestPerformance;

implementation

{$R *.dfm}

procedure TdlgTestPerformance.OKBtnClick(Sender: TObject);
var
  i, count : Integer;
  starttime, endtime, result : extended;
  msg : TmvMessage;
  oldCursor : TCursor;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    lbResult.Caption := '';
    Application.ProcessMessages;
    count := StrToInt (edDecodeTimes.Text);
    msg := Article.Msg;

    starttime := GetTickCount;
    for i := 0 to count - 1 do
    begin
      msg.Dormant := True;
      msg.Dormant := False
    end;
    endtime := GetTickCount;

    result := (endtime - starttime) / 1000;
    lbResult.Caption := FloatToStr (result) + ' seconds';

    Inc (fRuns);
    fTotal := fTotal + result;
    lblAverage.Caption := FloatToStr (fTotal / fRuns);

  finally
    Screen.Cursor := oldCursor
  end
end;

procedure TdlgTestPerformance.SpeedButton1Click(Sender: TObject);
begin
  fTotal := 0;
  fRuns := 0;
  lblAverage.Caption := '0'
end;

end.
