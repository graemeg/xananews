unit unitBatches;

interface

uses
  Windows, Classes, SysUtils, StrUtils, unitExSettings, Contnrs;

type
  TBatchActionType = (batNone, batAllNew, batAll, batNextN, batLastN, batSince);
  TBatchManagementType = (bmtMarkAsRead, bmtDelete);
  TBatchManagementOption = (bmoNone, bmoAll, bmoDay, bmoWeek, bmoMonth);

  //-----------------------------------------------------------------------
  // TBatchAction class.

  TBatchAction = class(TObject)
  private
    fHeadersOnly: Boolean;
    fMessageCount: Integer;
    fGroupName: string;
    fAccountName: string;
    fActionType: TBatchActionType;
    fSince: TDateTime;
    fManagementType: TBatchManagementType;
    fManagementOption: TBatchManagementOption;
    fManagementCount: Integer;
    function GetActionName: string;
    procedure SetActionName(const Value: string);
    function GetActionText: string;
    procedure SetActionText(const Value: string);
  public
    procedure LoadFromRegistry(reg: TExSettings);
    procedure SaveToRegistry(reg: TExSettings);
    procedure Assign(action: TBatchAction);
    function Equals(Obj: TObject): Boolean; override;

    property AccountName: string read fAccountName write fAccountName;
    property GroupName: string read fGroupName write fGroupName;
    property ActionType: TBatchActionType read fActionType write fActionType;
    property MessageCount: Integer read fMessageCount write fMessageCount;
    property ManagementCount: Integer read fManagementCount write fManagementCount;
    property HeadersOnly: Boolean read fHeadersOnly write fHeadersOnly;
    property ActionName: string read GetActionName write SetActionName;
    property ActionText: string read GetActionText write SetActionText;
    property ManagementType: TBatchManagementType read fManagementType write fManagementType;
    property ManagementOption: TBatchManagementOption read fManagementOption write fManagementOption;
    property Since: TDateTime read fSince write fSince;
  end;

  //-----------------------------------------------------------------------
  // TNNTPBatch - contains a bunch of TBatchActions

  TNNTPBatch = class
  private
    fBatchName: string;
    fRunEvery: Integer;
    fLastRun: TDateTime;
    fLastBatchRef: Integer;
    fScheduled: boolean;
    fRunAtStart: boolean;
    fHasRunThisSession: boolean;
    function GetActionCount: Integer;
    function GetActions(idx: Integer): TBatchAction;
    function GetBatchTime: string;
  public
    fBatchActions: TObjectList;

    constructor Create(const ABatchName: string);
    destructor Destroy; override;
    function RemoveAction(const accountName, groupName: string): boolean;

    property BatchName: string read fBatchName write fBatchName;
    property ActionCount: Integer read GetActionCount;
    property Actions[idx: Integer]: TBatchAction read GetActions;
    function IndexOf(const ActionName: string): Integer;
    procedure DeleteAction(idx: Integer);
    function AddAction(Action: TBatchAction): Integer;
    property RunEvery: Integer read fRunEvery write fRunEvery;
    property LastRun: TDateTime read fLastRun write fLastRun;
    property LastBatchRef: Integer read fLastBatchRef write fLastBatchRef;
    property BatchTime: string read GetBatchTime;
    property Scheduled: boolean read fScheduled write fScheduled;
    property RunAtStart: boolean read fRunAtStart write fRunAtStart;
    property HasRunThisSession: boolean read fHasRunThisSession write fHasRunThisSession;
  end;

implementation

uses
  unitSearchString;

{ TBatchAction }

procedure TBatchAction.Assign(action: TBatchAction);
// Only assign the batch fields - not the Account Group or Name
begin
  fHeadersOnly      := action.fHeadersOnly;
  fMessageCount     := action.fMessageCount;
  fActionType       := action.fActionType;
  fManagementType   := action.fManagementType;
  fManagementOption := action.fManagementOption;
  fManagementCount  := action.fManagementCount;
end;

function TBatchAction.Equals(Obj: TObject): Boolean;
var
  action: TBatchAction;
begin
  if Obj is TBatchAction then
  begin
    // Only check the batch fields - not Account, Group or Name
    action := TBatchAction(Obj);
    Result :=
      (fHeadersOnly      = action.fHeadersOnly)      and
      (fMessageCount     = action.fMessageCount)     and
      (fActionType       = action.fActionType)       and
      (fManagementType   = action.fManagementType)   and
      (fManagementOption = action.fManagementOption) and
      (fManagementCount  = action.fManagementCount);
  end
  else
    Result := inherited Equals(Obj);
end;

function TBatchAction.GetActionName: string;
begin
  Result := AccountName + '_' + GroupName;
end;

function TBatchAction.GetActionText: string;
var
  st: string;
begin
  Result := '';
  if ManagementOption <> bmoNone then
  begin
    if ManagementType = bmtMarkAsRead then
      Result := 'Mark as read'
    else
      Result := 'Delete';

    Result := Result + ' ';

    if ManagementOption = bmoAll then
      Result := Result + 'all messages'
    else
    begin
      case ManagementOption of
        bmoDay: st := 'day';
        bmoMonth: st := 'month';
        bmoWeek: st := 'week';
      end;

      if ManagementCount <> 1 then
        st := st + 's';

      Result := Result + Format('messages more than %d %s old', [ManagementCount, st]);
    end;
    if ActionType <> batNone then
      Result := Result + ' then get ';
  end
  else
    if ActionType <> batNone then
      Result := 'Get ';

  if ActionType <> batNone then
  begin
    case ActionType of
      batAllNew: Result := Result + 'all new';
      batAll   : Result := Result + 'all';
      batNextN : Result := Result + Format('next %d', [MessageCount]);
      batLastN : Result := Result + Format('last %d', [MessageCount]);
    end;

    if HeadersOnly then
      Result := Result + ' headers'
    else
      Result := Result + ' messages';
  end
  else
    if Result = '' then
      Result := 'None';
end;

procedure TBatchAction.LoadFromRegistry(reg: TExSettings);
var
  i: Integer;
begin
  // Load the batch fields - not account, group, name

  if not reg.HasValue('New Action Type') then
  begin
// Translate old action type.
// TBatchActionType = (batAllNewMessages, batNextNMessages, batAllMessages, batLastNMessages, batOnlyNewMessages, batNoAction);

    i := reg.IntegerValue['Action Type'];

    if i in [2, 3, 4] then
    begin
      ManagementType := bmtDelete;
      ManagementOption := bmoAll;
    end
    else
    begin
      ManagementType := bmtMarkAsRead;
      if reg.BooleanValue['Mark First'] then
        ManagementOption := bmoAll
      else
        ManagementOption := bmoNone;
    end;

    case i of
      0: ActionType := batAllNew;
      1: ActionType := batNextN;
      2: ActionType := batAll;
      3: ActionType := batLastN;
      4: ActionType := batAllNew;
    end;
  end
  else
    ActionType := TBatchActionType(reg.GetIntegerValue('New Action Type', Ord(batNone)));

  MessageCount := reg.IntegerValue['Message Count'];
  ManagementCount := reg.IntegerValue['Management Count'];
  ManagementType := TBatchManagementType(reg.GetIntegerValue('Management Type', Ord(bmtMarkAsRead)));
  ManagementOption := TBatchManagementOption(reg.GetIntegerValue('Management Option', Ord(bmoNone)));
  HeadersOnly := reg.BooleanValue['Headers Only'];
end;

procedure TBatchAction.SaveToRegistry(reg: TExSettings);
begin
  reg.SetIntegerValue('New Action Type', Integer(ActionType), -1);
  reg.IntegerValue['Message Count'] := MessageCount;
  reg.IntegerValue['Management Count'] := ManagementCount;
  reg.BooleanValue['Headers Only'] := HeadersOnly;
  reg.IntegerValue['Management Type'] := Integer(ManagementType);
  reg.IntegerValue['Management Option'] := Integer(ManagementOption);
end;

procedure TBatchAction.SetActionName(const Value: string);
begin
  fGroupName := Value;
  fAccountName := SplitString('_', fGroupName);
end;

procedure TBatchAction.SetActionText(const Value: string);
var
  p: Integer;
  st: string;

  function GetWord: string;
  var
    p1: Integer;
  begin
    if p > Length(Value) then
      Result := ''
    else
    begin
      p1 := PosEx(' ', Value, p);
      if p1 = 0 then
      begin
        Result := Copy(Value, p, MaxInt);
        p := Length(Value) + 1;
      end
      else
      begin
        Result := Trim(Copy(Value, p, p1 - p));
        p := p1 + 1;
      end;
    end;
  end;

begin
  ActionType := batNone;
  ManagementOption := bmoNone;
  ManagementType := bmtMarkAsRead;
  ManagementCount := 0;
  MessageCount := 0;
  HeadersOnly := False;

  p := 1;

  st := GetWord;

  if (st = 'Delete') or (st = 'Mark') then
  begin
    if st = 'Mark' then
    begin
      ManagementType := bmtMarkAsRead;
      GetWord; GetWord // 'as' 'read'
    end
    else
      ManagementType := bmtDelete;
    st := GetWord;
    if st = 'all' then
    begin
      ManagementOption := bmoAll;
      GetWord; // 'messages'
    end
    else
    begin // st = 'messages'
      GetWord; GetWord; st := GetWord; // 'more' 'than' 'n';
      ManagementCount := StrToInt(st);
      st := Copy(GetWord, 1, 3);  // st = 'day', 'mon', or 'wee';

      if st = 'day' then
        ManagementOption := bmoDay
      else
        if st = 'wee' then
          ManagementOption := bmoWeek
        else
          if st = 'mon' then
            ManagementOption := bmoMonth;
      GetWord; // old
    end;
    GetWord; // then
    st := GetWord;
  end;

  if LowerCase(st) = 'get' then
  begin
    st := GetWord;
    if st = 'all' then
    begin
      st := GetWord;
      if st = 'new' then
      begin
        ActionType := batAllNew;
        st := GetWord;
      end
      else
        ActionType := batAll;
    end
    else
      if (st = 'next') or (st = 'last') then
      begin
        if st = 'next' then
          ActionType := batNextN
        else
          ActionType := batLastN;
        MessageCount := StrToInt(GetWord);
        st := GetWord;
      end;
    if st = 'headers' then
      HeadersOnly := True;
  end;
end;

{ TNNTPBatch }

function TNNTPBatch.AddAction(Action: TBatchAction): Integer;
begin
  Result := fBatchActions.Add(Action);
end;

constructor TNNTPBatch.Create(const ABatchName: string);
begin
  fBatchName := ABatchName;
  fBatchActions := TObjectList.Create;
end;

procedure TNNTPBatch.DeleteAction(idx: Integer);
begin
  fBatchActions.Delete(idx);
end;

destructor TNNTPBatch.Destroy;
begin
  fBatchActions.Free;
  inherited Destroy;
end;

function TNNTPBatch.GetActionCount: Integer;
begin
  Result := fBatchActions.Count;
end;

function TNNTPBatch.GetActions(idx: Integer): TBatchAction;
begin
  Result := TBatchAction(fBatchActions[idx]);
end;

function TNNTPBatch.GetBatchTime: string;
begin
  if Scheduled and (RunEvery > 0) then
    Result := Format('%d minutes', [RunEvery])
  else
    Result := '';
end;

function TNNTPBatch.IndexOf(const ActionName: string): Integer;
begin
  Result := 0;
  while Result < ActionCount do
    if CompareText(ActionName, Actions[Result].ActionName) = 0 then
      Exit
    else
      Inc(Result);

  Result := -1;
end;

function TNNTPBatch.RemoveAction(const accountName, groupName: string): boolean;
var
  i: Integer;
  action: TBatchAction;
begin
  i := 0;
  Result := False;
  while i < ActionCount do
  begin
    action := Actions[i];
    if SameText(action.GroupName, groupName) and SameText(action.AccountName, AccountName) then
    begin
      DeleteAction(i);
      Result := True;
    end
    else
      Inc(i);
  end;
end;

end.
