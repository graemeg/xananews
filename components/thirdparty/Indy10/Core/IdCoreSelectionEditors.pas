unit IdCoreSelectionEditors;

interface

{$I IdCompilerDefines.inc}

uses
  Classes
  {$IFDEF TSelectionEditor}
    {$IFDEF FPC}
    ,PropEdits
    ,ComponentEditors
    {$ELSE}
    ,DesignIntf
    ,DesignEditors
    {$ENDIF}
  {$ENDIF}
  ;

{$IFDEF TSelectionEditor}
type
  TIdContextSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;
{$ENDIF}

implementation

{$IFDEF TSelectionEditor}
procedure TIdContextSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);
  Proc('IdContext');
end;
{$ENDIF}

end.