unit PropertyPageForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TPropertyPageData = class
  private
    fCaption: string;
    fHelpText: string;
    fHelpKeyword: string;
    fParam: LPARAM;
    fMinX: Integer;
    fMinY: Integer;
    fInitialized: Boolean;
  protected
    property Param: LPARAM read fParam;
    procedure Initialize; virtual; abstract;
    function GetCaption: string; virtual;
    function GetHelpText: string; virtual;
  public
    constructor Create(const ACaption, AHelpText, AHelpKeyword: string; AMinCX, AMinCY: Integer; AParam: LPARAM = 0);
    function Apply: Boolean; virtual;
    procedure Cancel; virtual;
    procedure Error; virtual;

    property Caption: string read GetCaption;
    property Initialized: Boolean read fInitialized write fInitialized;
    property HelpText: string read GetHelpText;
    property HelpKeyword: string read fHelpKeyword;

    property MinX: Integer read fMinX;
    property MinY: Integer read fMinY;
  end;

  TPropertyPageDataClass = class of TPropertyPageData;

  TfmPropertyPage = class(TForm)
    Panel1: TPanel;
    Bevel1: TBevel;
    stSectionDetails: TLabel;
  private
    fAltKeyword: string;
  protected
    fPopulating: Boolean;
  public
    class function GetDataClass: TPropertyPageDataClass; virtual; abstract;
    procedure PopulateControls (AData: TPropertyPageData); virtual;
    property Populating: Boolean read fPopulating write fPopulating;
    property AltKeyword: string read fAltKeyword;
  end;

  TPropertyPageClass = class of TfmPropertyPage;

var
  fmPropertyPage: TfmPropertyPage;

implementation

{$R *.dfm}

{ TfmPropertyPage }

procedure TfmPropertyPage.PopulateControls(AData: TPropertyPageData);
begin
  if not AData.fInitialized then
  begin
    AData.Initialize;
    AData.fInitialized := True;
  end;
  stSectionDetails.Caption := AData.HelpText;
  fAltKeyword := AData.HelpKeyword;
end;

{ TPropertyPageData }

function TPropertyPageData.Apply: Boolean;
begin
  Result := True;  // Stub - return true to indicate success
end;

procedure TPropertyPageData.Cancel;
begin
// Stub
end;

constructor TPropertyPageData.Create(const ACaption, AHelpText, AHelpKeyword: string;
  AMinCX, AMinCY: Integer; AParam: LPARAM);
begin
  fCaption := ACaption;
  fHelpText := AHelpText;
  fHelpKeyword := AHelpKeyword;
  fParam := AParam;
  fMiNX := AMinCX;
  fMinY := AMinCY;
end;

procedure TPropertyPageData.Error;
begin
  // Stub
end;

function TPropertyPageData.GetCaption: string;
begin
  Result := fCaption
end;

function TPropertyPageData.GetHelpText: string;
begin
  Result := fHelpText
end;

end.
