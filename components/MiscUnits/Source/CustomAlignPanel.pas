unit CustomAlignPanel;

interface

uses
  Windows, SysUtils, Classes, Controls, ExtCtrls;

type
  TOnCustomAlignPosition = procedure (sender : TObject; var newLeft, newTop, newWidth, newHeight : Integer) of object;

  TCustomAlignPanel = class(TPanel)
  private
    fOnCustomAlignPosition: TOnCustomAlignPosition;
    { Private declarations }
  protected
    procedure CustomAlignPosition(Control: TControl; var NewLeft, NewTop, NewWidth,
      NewHeight: Integer; var AlignRect: TRect; AlignInfo: TAlignInfo); override;
  public
    { Public declarations }
  published
     property OnCustomAlignPosition : TOnCustomAlignPosition read fOnCustomAlignPosition write fOnCustomAlignPosition;
    { Published declarations }
  end;

implementation

{ TCustomAlignPanel }

procedure TCustomAlignPanel.CustomAlignPosition(Control: TControl;
  var NewLeft, NewTop, NewWidth, NewHeight: Integer; var AlignRect: TRect;
  AlignInfo: TAlignInfo);
begin
  if Assigned (OnCustomAlignPosition) and not (csDestroying in ComponentState) then
    OnCustomAlignPosition (self, NewLeft, NewTop, NewWidth, NewHeight);
end;

end.
