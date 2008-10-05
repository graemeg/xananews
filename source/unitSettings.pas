unit unitSettings;

interface

uses Windows, Classes, SysUtils, NewsGlobals, unitBatches, unitIdentities, TypInfo, unitCharsetMap, unitExSettings;

type

TSettings = class (TPersistent)
private
  fParent: TSettings;

//  procedure LoadSettings (rootKey : HKEY);
//  procedure SaveSettings (rootKey : HKEY);
protected
  function GetProp (var st : string; const name : string) : string; overload;
  function GetProp (var i : Integer; const name : string; deflt : Integer = 0) : Integer; overload;

  procedure SetProp (var st : string; const name, value : string); overload;
  procedure SetProp (var i : Integer; const name : string; value : Integer); overload;

  procedure WritePropToRegistry (reg : TExSettings; const valueName, st, name : string); overload;
  procedure WritePropToRegistry (reg : TExSettings; const valueName : string; i : Integer; const name : string; deft : Integer = 0); overload;

public
  constructor Create (AParent : TSettings); virtual;

  procedure ReadSettings (reg : TExSettings); virtual; abstract;
  procedure WriteSettings (reg : TExSettings); virtual; abstract;

  property Parent : TSettings read fParent;
end;

TPostingSettings = class (TSettings)
private
  fQuoteFooter: string;
  fQuoteHeader: string;
  fQuoteLineMarker: string;
  fQuoteSalutation : string;
  fMaxPostLines : Integer;
  fMaxPostLineLength : Integer;
  fTextPartStyle : TTextPartStyle;
  fPostingStyle : TPostingStyle;
  fDefaultCodePage : Integer;
  fDefaultSpellLanguage : Integer;
  fDelayPosting : Integer;
  fArchivePostedMessages: Integer;
  fIsGroup: boolean;
  function GetQuoteFooter: string;
  function GetQuoteHeader: string;
  function GetQuoteLineMarker: string;
  procedure SetQuoteFooter(const Value: string);
  procedure SetQuoteHeader(const Value: string);
  procedure SetQuoteLineMarker(const Value: string);
  function GetMaxPostLineLength: Integer;
  function GetMaxPostLines: Integer;
  function GetTextPartStyle: TTextPartStyle;
  procedure SetMaxPostLineLength(const Value: Integer);
  procedure SetMaxPostLines(const Value: Integer);
  procedure SetTextPartStyle(const Value: TTextPartStyle);
  function GetDefaultCodePage: Integer;
  procedure SetDefaultCodePage(const Value: Integer);
  function GetDefaultSpellLanguage: Integer;
  procedure SetDefaultSpellLanguage(const Value: Integer);
  function GetDelayPosting: Boolean;
  procedure SetDelayPosting(const Value: Boolean);
  function GetArchivePostedMessages: Boolean;
  procedure SetArchivePostedMessages(const Value: Boolean);
  function GetPostingStyle: TPostingStyle;
  procedure SetPostingStyle(const Value: TPostingStyle);
    function GetQuoteSalutation: string;
    procedure SetQuoteSalutation(const Value: string);
public
  procedure Assign (source : TPersistent); override;
  constructor Create (AParent : TSettings); override;
  property IsGroup : boolean read fIsGroup write fIsGroup;
  procedure ReadSettings (reg : TExSettings); override;
  procedure WriteSettings (reg : TExSettings); override;
published
  property QuoteHeader : string read GetQuoteHeader write SetQuoteHeader;
  property QuoteLineMarker : string read GetQuoteLineMarker write SetQuoteLineMarker;
  property QuoteFooter : string read GetQuoteFooter write SetQuoteFooter;
  property QuoteSalutation : string read GetQuoteSalutation write SetQuoteSalutation;

  property MaxPostLines : Integer read GetMaxPostLines write SetMaxPostLines;
  property MaxPostLineLength : Integer read GetMaxPostLineLength write SetMaxPostLineLength;
  property TextPartStyle : TTextPartStyle read GetTextPartStyle write SetTextPartStyle;
  property PostingStyle : TPostingStyle read GetPostingStyle write SetPostingStyle;
  property DefaultCodePage : Integer read GetDefaultCodePage write SetDefaultCodePage;
  property DefaultSpellLanguage : Integer read GetDefaultSpellLanguage write SetDefaultSpellLanguage;
  property DelayPosting : Boolean read GetDelayPosting write SetDelayPosting;
  property ArchivePostedMessages : Boolean read GetArchivePostedMessages write SetArchivePostedMessages;
end;

TDisplaySettings = class (TSettings)
private
  fDefaultCodePage : Integer;
  fPurgeFolder : string;
  fTruncateFrom : string;
  fSoundFile : string;
  fMessagebaseManagementDefault : Integer;
  fThreadOrder : Integer;
  fThreadSortOrder : Integer;
  fThreadSortDirection : Integer;
  fGatherSubjects : Integer;
  fExpanded: Integer;

  function GetDefaultCodePage: Integer;
  procedure SetDefaultCodePage(const Value: Integer);
  function GetPurgeFolder: string;
  procedure SetPurgeFolder(const Value: string);
  function GetTruncateFrom: string;
  procedure SetTruncateFrom(const Value: string);
  function GetSoundFile: string;
  procedure SetSoundFile(const Value: string);
  procedure SetMessagebaseManagementDefault(const Value: boolean);
  function GetMessagebaseManagementDefault: boolean;
  function GetThreadOrder: TThreadOrder;
  function GetThreadSortDirection: TThreadSortDirection;
  function GetThreadSortOrder: TThreadSortOrder;
  procedure SetThreadOrder(const Value: TThreadOrder);
  procedure SetThreadSortDirection(const Value: TThreadSortDirection);
  procedure SetThreadSortOrder(const Value: TThreadSortOrder);
  function GetGatherSubjects: boolean;
  procedure SetGatherSubjects(const Value: boolean);
  function GetExpanded: boolean;
  procedure SetExpanded(const Value: boolean);
public
  constructor Create (AParent : TSettings); override;
  procedure Assign (source : TPersistent); override;
  procedure ReadSettings (reg : TExSettings); override;
  procedure WriteSettings (reg : TExSettings); override;
published
  property DefaultCodepage : Integer read GetDefaultCodePage write SetDefaultCodePage;
  property PurgeFolder : string read GetPurgeFolder write SetPurgeFolder;
  property TruncateFrom : string read GetTruncateFrom write SetTruncateFrom;
  property SoundFile : string read GetSoundFile write SetSoundFile;
  property MessagebaseManagementDefault : boolean read GetMessagebaseManagementDefault write SetMessagebaseManagementDefault;
  property ThreadOrder : TThreadOrder read GetThreadOrder write SetThreadOrder;
  property ThreadSortOrder : TThreadSortOrder read GetThreadSortOrder write SetThreadSortOrder;
  property ThreadSortDirection : TThreadSortDirection read GetThreadSortDirection write SetThreadSortDirection;
  property GatherSubjects : boolean read GetGatherSubjects write SetGatherSubjects;
  property Expanded : boolean read GetExpanded write SetExpanded;
end;


TPerformDefaultAction = (paNever, paSession, paAlways);
TNNTPSettings = class (TSettings)
private
  fAdvancedHeaders : string;
  fDefaultAction : TBatchAction;
  fIdentity : TIdentity;
  fNoArchive : Integer;
  fMessageIDStub : string;
  fMessageIDDomain : string;
  fGenerateMessageIDs : Integer;
  fGenerateDateHeaders : Integer;
  fGenerateApprovedHeaders : Integer;
  fPerformDefaultAction : Integer;
  fSignatureOverride : string;

  function GetAdvancedHeaders: string;
  function GetDefaultAction: TBatchAction;
  function GetIdentity: TIdentity;
  function GetNoArchive: boolean;
  procedure SetAdvancedHeaders(const Value: string);
  procedure SetDefaultAction(const Value: TBatchAction);
  procedure SetNoArchive(const Value: boolean);
  function GetGenerateDateHeaders: boolean;
  function GetGenerateMessageIDs: boolean;
  function GetMessageIDStub: string;
  procedure SetGenerateDateHeaders(const Value: boolean);
  procedure SetMessageIDStub(const Value: string);
  procedure SetGenerateMessageIDs(const Value: boolean);
  function GetMessageIDDomain: string;
  procedure SetMessageIDDomain(const Value: string);
  function GetGenerateApprovedHeaders: boolean;
  procedure SetGenerateApprovedHeaders(const Value: boolean);
  function GetPerformDefaultAction: TPerformDefaultAction;
  procedure SetPerformDefaultAction(const Value: TPerformDefaultAction);
  procedure SetSignatureOverride(const Value: string);
  function GetSignatureOverride: string;

public
  procedure Assign (source : TPersistent); override;
  destructor Destroy; override;
  procedure SetIdentityName(const Value: string);
  property DefaultAction : TBatchAction read GetDefaultAction write SetDefaultAction;
  property Identity : TIdentity read GetIdentity;
  procedure ReadSettings (reg : TExSettings); override;
  procedure WriteSettings (reg : TExSettings); override;

published
  property NoArchive : boolean read GetNoArchive write SetNoArchive;
  property AdvancedHeaders : string read GetAdvancedHeaders write SetAdvancedHeaders;
  property GenerateDateHeaders : boolean read GetGenerateDateHeaders write SetGenerateDateHeaders;
  property GenerateApprovedHeaders : boolean read GetGenerateApprovedHeaders write SetGenerateApprovedHeaders;
  property GenerateMessageIDs : boolean read GetGenerateMessageIDs write SetGenerateMessageIDs;
  property MessageIDStub : string read GetMessageIDStub write SetMessageIDStub;
  property MessageIDDomain : string read GetMessageIDDomain write SetMessageIDDomain;
  property PerformDefaultAction : TPerformDefaultAction read GetPerformDefaultAction write SetPerformDefaultAction;
  property SignatureOverride : string read GetSignatureOverride write SetSignatureOverride;
end;

TServerSettings = class (TSettings)
private
  fConnectTimeout: Integer;
  fReadTimeout: Integer;
  fRASConnection : string;
  fServerAccountName : string;
  fServerLogonRequired : Integer;
  fServerName : string;
  fServerPassword : string;
  fServerPort : Integer;
  fSSLPort : Integer;
  fSSLRequired : Integer;
  fServerTimeout: Integer;
  fId: string;

  function GetConnectTimeout: Integer;
  function GetRASConnection: string;
  function GetReadTimeout: Integer;
  function GetServerAccountName: string;
  function GetServerLogonRequired: boolean;
  function GetServerName: string;
  function GetServerPassword: string;
  function GetServerPort: Integer;
  function GetServerTimeout: Integer;
  function GetSSLPort: Integer;
  function GetSSLRequired: boolean;
  procedure SetConnectTimeout(const Value: Integer);
  procedure SetRASConnection(const Value: string);
  procedure SetReadTimeout(const Value: Integer);
  procedure SetServerAccountName(const Value: string);
  procedure SetServerLogonRequired(const Value: boolean);
  procedure SetServerName(const Value: string);
  procedure SetServerPassword(const Value: string);
  procedure SetServerPort(const Value: Integer);
  procedure SetServerTimeout(const Value: Integer);
  procedure SetSSLPort(const Value: Integer);
  procedure SetSSLRequired(const Value: boolean);
protected
  function GetDefaultSSLPort : Integer; virtual;
public
  constructor Create (AParent : TSettings); override;
  function Equals(Obj: TObject): Boolean; override;
  property Id : string read fId write fId;
  procedure Assign (source : TPersistent); override;
  procedure ReadSettings (reg : TExSettings); override;
  procedure WriteSettings (reg : TExSettings); override;
published
  property ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout;
  property RASConnection : string read GetRASConnection write SetRASConnection;
  property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
  property ServerName : string read GetServerName write SetServerName;
  property ServerLogonRequired : boolean read GetServerLogonRequired write SetServerLogonRequired;
  property ServerAccountName : string read GetServerAccountName write SetServerAccountName;
  property ServerPassword : string read GetServerPassword write SetServerPassword;
  property ServerPort : Integer read GetServerPort write SetServerPort;
  property ServerTimeout : Integer read GetServerTimeout write SetServerTimeout;
  property SSLRequired : boolean read GetSSLRequired write SetSSLRequired;
  property SSLPort : Integer read GetSSLPort write SetSSLPort;
end;

TNNTPServerSettings = class (TServerSettings)
private
  fAlwaysAuthenticate: Integer;
  fPipelineSize : Integer;
  fMaxConnections : Integer;
  function GetAlwaysAuthenticate: boolean;
  procedure SetAlwaysAuthenticate(const Value: boolean);
  function GetPipelineSize: Integer;
  procedure SetPipelineSize(const Value: Integer);
  function GetMaxConnections: Integer;
  procedure SetMaxConnections(const Value: Integer);
public
  constructor Create (AParent : TSettings); override;
  procedure Assign (source : TPersistent); override;
  procedure ReadSettings (reg : TExSettings); override;
  procedure WriteSettings (reg : TExSettings); override;
published
  property AlwaysAuthenticate : boolean read GetAlwaysAuthenticate write SetAlwaysAuthenticate;
  property MaxConnections : Integer read GetMaxConnections write SetMaxConnections;
  property PipelineSize : Integer read GetPipelineSize write SetPipelineSize;
end;

TSMTPServerSettings = class (TServerSettings)
protected
  function GetDefaultSSLPort : Integer; override;
public
  constructor Create (AParent : TSettings); override;
  procedure ReadSettings (reg : TExSettings); override;
  procedure WriteSettings (reg : TExSettings); override;
end;

implementation

uses unitNNTPServices, cmpSpellChecker, unitSavedArticles;

{ TSettings }

constructor TSettings.Create(AParent: TSettings);
begin
  fParent := AParent;
end;

function TSettings.GetProp(var st : string; const name: string): string;
begin
  if st = #1 then
    if Assigned (Parent) then
    begin
      result := GetStrProp (parent, name);
      exit
    end
    else
      st := '';

  result := st
end;

function TSettings.GetProp(var i: Integer; const name: string; deflt : Integer = 0): Integer;
begin
  if i = -1 then
    if Assigned (parent) then
    begin
      result := GetOrdProp (parent, name);
      exit
    end
    else
      i := deflt;

  result := i
end;

(*
procedure TSettings.LoadSettings(rootKey: HKEY);
var
  reg : TExSettings;
begin
  reg := TExSettings.Create (KEY_READ);
  try
    reg.RootKey := rootKey;

    if reg.OpenKey('', False) then
      ReadSettings (reg)
  finally
    reg.Free
  end
end;
*)

(*
procedure TSettings.SaveSettings(rootKey: HKEY);
var
  reg : TExSettings;
begin
  reg := TExSettings.Create (KEY_READ or KEY_WRITE);
  try
    reg.RootKey := rootKey;

    if reg.OpenKey ('', False) then
      WriteSettings (reg)

  finally
    reg.Free;
  end
end;
*)

procedure TSettings.SetProp(var st : string; const name, value: string);
begin
  if Assigned (parent) and (GetStrProp (parent, name) = Value) then
    st := #1
  else
    st := value
end;

procedure TSettings.SetProp(var i: Integer; const name: string; value: Integer);
begin
  if Assigned (parent) and (GetOrdProp (parent, name) = Value) then
    i := -1
  else
    i := Value
end;

procedure TSettings.WritePropToRegistry(reg: TExSettings; const valueName, st, name: string);
var
  deflt : string;
begin
  if st = #1 then
    deflt := #1
  else
    if Assigned (Parent) then
      deflt := GetStrProp (parent, name)
    else
      deflt := '';

  reg.SetStringValue(valueName, st, deflt);
end;

procedure TSettings.WritePropToRegistry(reg: TExSettings; const valueName: string; i: Integer; const name: string; deft : Integer = 0);
var
  deflt : Integer;
begin
  if i = -1 then
    deflt := -1
  else
    if Assigned (Parent) then
      deflt := GetOrdProp (parent, name)
    else
      deflt := deft;

  reg.SetIntegerValue (valueName, i, deflt)
end;

{ TPostingSettings }

procedure TPostingSettings.Assign(source: TPersistent);
var
  src : TPostingSettings;
begin
  if source is TPostingSettings then
  begin
    src := TPostingSettings (source);
    QuoteHeader := src.QuoteHeader;
    QuoteFooter := src.QuoteFooter;
    QuoteLineMarker := src.QuoteLineMarker;
    QuoteSalutation := src.QuoteSalutation;

    MaxPostLines := src.MaxPostLines;
    MaxPostLineLength := src.MaxPostLineLength;
    TextPartStyle := src.TextPartStyle;
    PostingStyle := src.PostingStyle;

    DefaultCodePage := src.DefaultCodePage;
    DefaultSpellLanguage := src.DefaultSpellLanguage;

    DelayPosting := src.DelayPosting;
    ArchivePostedMessages := src.ArchivePostedMessages;
    IsGroup := src.IsGroup;
  end
  else
    Inherited
end;

constructor TPostingSettings.Create(AParent: TSettings);
begin
  inherited;

  if Assigned (parent) then
  begin
    fQuoteHeader := #1;
    fQuoteFooter := #1;
    fQuoteLineMarker := #1;
    fQuoteSalutation := #1;

    fMaxPostLines := -1;
    fMaxPostLineLength := -1;
    fTextPartStyle := TTextPartStyle (-1);
    fPostingStyle := TPostingStyle (-1);
    fDefaultCodePage := -1;
    fDefaultSpellLanguage := -1;
    fDelayPosting := -1;
    fArchivePostedMessages := -1;
  end
end;

function TPostingSettings.GetArchivePostedMessages: Boolean;
begin
  result := Boolean (GetProp (fArchivePostedMessages, 'ArchivePostedMessages'));
end;

function TPostingSettings.GetDefaultCodePage: Integer;
begin
  result := GetProp (fDefaultCodePage, 'DefaultCodePage');
end;

function TPostingSettings.GetDefaultSpellLanguage: Integer;
begin
  result := GetProp (fDefaultSpellLanguage, 'DefaultSpellLanguage');
end;

function TPostingSettings.GetDelayPosting: Boolean;
begin
  result := Boolean (GetProp (fDelayPosting, 'DelayPosting'));
end;

function TPostingSettings.GetMaxPostLineLength: Integer;
begin
  result := GetProp (fMaxPostLineLength, 'MaxPostLineLength');
end;

function TPostingSettings.GetMaxPostLines: Integer;
begin
  result := GetProp (fMaxPostLines, 'MaxPostLines');
end;

function TPostingSettings.GetPostingStyle: TPostingStyle;
var
  i : Integer;
begin
  i := ShortInt (fPostingStyle);
  result := TPostingStyle (GetProp (i, 'PostingStyle'));
  fPostingStyle := TPostingStyle (i)
end;

function TPostingSettings.GetQuoteFooter: string;
begin
  result := GetProp (fQuoteFooter, 'QuoteFooter');
end;

function TPostingSettings.GetQuoteHeader: string;
begin
  result := GetProp (fQuoteHeader, 'QuoteHeader');
end;

function TPostingSettings.GetQuoteLineMarker: string;
begin
  result := GetProp (fQuoteLineMarker, 'QuoteLineMarker');
end;

function TPostingSettings.GetQuoteSalutation: string;
begin
  result := GetProp (fQuoteSalutation, 'QuoteSalutation');
end;

function TPostingSettings.GetTextPartStyle: TTextPartStyle;
var
  i : Integer;
begin
  i := ShortInt (fTextPartStyle);
  result := TTextPartStyle (GetProp (i, 'TextPartStyle'));
  fTextPartStyle := TTextPartStyle (i)
end;

procedure TPostingSettings.ReadSettings(reg: TExSettings);
begin
  fQuoteHeader     := reg.GetStringValue ('Quote header', #1);
  fQuoteLineMarker := reg.GetStringValue ('Quote line marker', #1);
  fQuoteFooter     := reg.GetStringValue ('Quote footer', #1);
  fQuoteSalutation := reg.GetStringValue ('Quote Salutation', #1);

  if not Assigned (parent) then
  begin
    fMaxPostLines := reg.GetIntegerValue ('Max Post Lines', 5000);
    fMaxPostLineLength := reg.GetIntegerValue ('Max Post Line Length', DefaultMaxLineLength);
    fTextPartStyle := TTextPartStyle (reg.GetIntegerValue ('Text Part Style', Integer (tpNNTP)));
    fPostingStyle := TPostingStyle (reg.GetIntegerValue ('Posting Style', Integer (psBottom)));
    fDefaultCodePage := reg.GetIntegerValue('Default Codepage', unitCharsetMap.DefaultCodePage);
    fDefaultSpellLanguage := reg.GetIntegerValue('Default Spell Language', gDefaultISpellLanguage);
    fDelayPosting := reg.GetIntegerValue('Delay Posting', 0);
    fArchivePostedMessages := reg.GetIntegerValue('Archive Posted Messages', 0);

  end
  else
  begin
    fMaxPostLines := reg.GetIntegerValue ('Max Post Lines', -1);
    fMaxPostLineLength := reg.GetIntegerValue ('Max Post Line Length', -1);
    fTextPartStyle := TTextPartStyle (reg.GetIntegerValue ('Text Part Style', -1));
    fPostingStyle := TPostingStyle (reg.GetIntegerValue ('Posting Style', -1));
    fDefaultCodePage := reg.GetIntegerValue('Default Codepage', -1);
    fDefaultSpellLanguage := reg.GetIntegerValue('Default Spell Language', -1);
    fDelayPosting := reg.GetIntegerValue('Delay Posting', -1);
    fArchivePostedMessages := reg.GetIntegerValue('Archive Posted Messages', -1);
  end
end;

procedure TPostingSettings.SetArchivePostedMessages(const Value: Boolean);
begin
  SetProp (fArchivePostedMessages, 'ArchivePostedMessages', Ord (Value));
end;

procedure TPostingSettings.SetDefaultCodePage(const Value: Integer);
begin
  SetProp (fDefaultCodePage, 'DefaultCodepage', Value)
end;

procedure TPostingSettings.SetDefaultSpellLanguage(const Value: Integer);
begin
  SetProp (fDefaultSpellLanguage, 'DefaultSpellLanguage', Value)
end;

procedure TPostingSettings.SetDelayPosting(const Value: Boolean);
begin
  SetProp (fDelayPosting, 'DelayPosting', Ord (Value));
end;

procedure TPostingSettings.SetMaxPostLineLength(const Value: Integer);
begin
  SetProp (fMaxPostLineLength, 'MaxPostLineLength', Value);
end;

procedure TPostingSettings.SetMaxPostLines(const Value: Integer);
begin
  SetProp (fMaxPostLines, 'MaxPostLines', Value);
end;

procedure TPostingSettings.SetPostingStyle(const Value: TPostingStyle);
var
  i : Integer;
begin
  i := ShortInt (fPostingStyle);
  SetProp (i, 'PostingStyle', Integer (Value));
  fPostingStyle := TPostingStyle (i)
end;

procedure TPostingSettings.SetQuoteFooter(const Value: string);
begin
  SetProp (fQuoteFooter, 'QuoteFooter', Value)
end;

procedure TPostingSettings.SetQuoteHeader(const Value: string);
begin
  SetProp (fQuoteHeader, 'QuoteHeader', Value)
end;

procedure TPostingSettings.SetQuoteLineMarker(const Value: string);
begin
  SetProp (fQuoteLineMarker, 'QuoteLineMarker', Value)
end;

procedure TPostingSettings.SetQuoteSalutation(const Value: string);
begin
  SetProp (fQuoteSalutation, 'QuoteSalutation', Value)
end;

procedure TPostingSettings.SetTextPartStyle(const Value: TTextPartStyle);
var
  i : Integer;
begin
  i := ShortInt (fTextPartStyle);
  SetProp (i, 'TextPartStyle', Integer (Value));
  fTextPartStyle := TTextPartStyle (i)
end;

procedure TPostingSettings.WriteSettings(reg: TExSettings);
begin
  WritePropToRegistry (reg, 'Quote Header', fQuoteHeader, 'QuoteHeader');
  WritePropToRegistry (reg, 'Quote Footer', fQuoteFooter, 'QuoteFooter');
  WritePropToRegistry (reg, 'Quote Line Marker', fQuoteLineMarker, 'QuoteLineMarker');
  WritePropToRegistry (reg, 'Quote Salutation', fQuoteSalutation, 'QuoteSalutation');

  if not Assigned (Parent) then
  begin
    reg.SetIntegerValue('Max Post Lines', fMaxPostLines, 5000);
    reg.SetIntegerValue('Max Post Line Length', fMaxPostLineLength, DefaultMaxLineLength);
    reg.SetIntegerValue('Text Part Style', ShortInt (fTextPartStyle), Integer (tpNNTP));
    reg.SetIntegerValue('Posting Style', ShortInt (fPostingStyle), Integer (psBottom));
    reg.SetIntegerValue('Default Codepage', fDefaultCodepage, unitCharsetMap.DefaultCodePage);
    reg.SetIntegerValue('Default Spell Language', fDefaultSpellLanguage, gDefaultISpellLanguage);
    reg.SetIntegerValue('Delay Posting', fDelayPosting, 0);
    reg.SetIntegerValue('Archive Posted Messages', fArchivePostedMessages, 0);
  end
  else
  begin
    WritePropToRegistry (reg, 'Max Post Lines', fMaxPostLines, 'MaxPostLines');
    WritePropToRegistry (reg, 'Max Post Line Length', fMaxPostLineLength, 'MaxPostLineLength');
    WritePropToRegistry (reg, 'Text Part Style', ShortInt (fTextPartStyle), 'TextPartStyle');
    WritePropToRegistry (reg, 'Posting Style', ShortInt (fPostingStyle), 'PostingStyle');
    WritePropToRegistry (reg, 'Default Codepage', fDefaultCodepage, 'DefaultCodepage');
    WritePropToRegistry (reg, 'Default Spell Language', fDefaultSpellLanguage, 'DefaultSpellLanguage');
    WritePropToRegistry (reg, 'Delay Posting', fDelayPosting, 'DelayPosting');
    WritePropToRegistry (reg, 'Archive Posted Messages', fArchivePostedMessages, 'ArchivePostedMessages');
  end
end;

{ TDisplaySettings }

procedure TDisplaySettings.Assign(source: TPersistent);
var
  src : TDisplaySettings;
begin
  if source is TDisplaySettings then
  begin
    src := TDisplaySettings (source);

    DefaultCodepage := src.DefaultCodepage;
    PurgeFolder := src.PurgeFolder;
    TruncateFrom := src.TruncateFrom;
    SoundFile := src.SoundFile;
    MessagebaseManagementDefault := src.MessagebaseManagementDefault;
    ThreadOrder := src.ThreadOrder;
    ThreadSortOrder := src.ThreadSortOrder;
    ThreadSortDirection := src.ThreadSortDirection;
    GatherSubjects := src.GatherSubjects;
  end
  else
    Inherited
end;

constructor TDisplaySettings.Create(AParent: TSettings);
var
  reg : TExSettings;
  purgeToBin : boolean;
  binFolder : string;
begin
  inherited;

  if Assigned (parent) then
  begin
    fDefaultCodePage := -1;
    fMessagebaseManagementDefault := -1;
    fGatherSubjects := -1;
    fThreadOrder := -1;
    fThreadSortOrder := -1;
    fThreadSortDirection := -1;
    fPurgeFolder := #1;
    fSoundFile := #1;
    fTruncateFrom := #1;
    fExpanded := 0;
  end
  else
  begin
    reg := CreateExSettings;
    try
      purgeToBin := True;
      reg.Section := 'General';
      if reg.HasValue('Purge To Bin') then  // Support legacy 'purge to bin' entry
      begin
        purgeToBin := reg.GetBooleanValue('Purge To Bin', True);
        reg.DeleteValue('Purge To Bin')       // Delete legacy setting
      end;

      binFolder := gArticleFolders.FindFolder ('').Name;
      if purgeToBin then
//        fPurgeFolder := binFolder  1.17.5.1.  Don't default any more
        fPurgeFolder := ''
      else
      begin  // Only gets here if legacy setting existed, and
             // Purge to Bin was turned off.

        fPurgeFolder := '';
(*  1.17.5.1
        if reg.OpenKey (gKeyName + '\Globals', True) then
          reg.SetValue ('Purge Folder', fPurgeFolder, binFolder);
*)
        reg.Section := 'Globals';
        reg.DeleteValue('Purge Folder')
      end
    finally
      reg.Free
    end
  end
end;

function TDisplaySettings.GetDefaultCodePage: Integer;
begin
  result := GetProp (fDefaultCodePage, 'DefaultCodePage');
end;

function TDisplaySettings.GetExpanded: boolean;
begin
  result := Boolean(fExpanded);
end;

function TDisplaySettings.GetGatherSubjects: boolean;
begin
  result := Boolean (GetProp (fGatherSubjects, 'GatherSubjects'))
end;

function TDisplaySettings.GetMessagebaseManagementDefault: boolean;
begin
  result := Boolean (GetProp (fMessagebaseManagementDefault, 'MessagebaseManagementDefault'))
end;

function TDisplaySettings.GetPurgeFolder: string;
begin
  result := GetProp (fPurgeFolder, 'PurgeFolder');
end;

function TDisplaySettings.GetSoundFile: string;
begin
  result := GetProp (fSoundFile, 'SoundFile');
end;

function TDisplaySettings.GetThreadOrder: TThreadOrder;
begin
  result := TThreadOrder (GetProp (fThreadOrder, 'ThreadOrder'));
end;

function TDisplaySettings.GetThreadSortDirection: TThreadSortDirection;
begin
  result := TThreadSortDirection (GetProp (fThreadSortDirection , 'ThreadSortDirection'));
end;

function TDisplaySettings.GetThreadSortOrder: TThreadSortOrder;
begin
  result := TThreadSortOrder (GetProp (fThreadSortOrder, 'ThreadSortOrder'));
end;

function TDisplaySettings.GetTruncateFrom: string;
begin
  result := GetProp (fTruncateFrom, 'TruncateFrom');
end;

procedure TDisplaySettings.ReadSettings(reg: TExSettings);
var
  oldSoundFile : string;
begin
  if not Assigned (parent) then
  begin
    oldSoundFile := gXanaNewsDir + '\' + 'NewNews.wav';
    fDefaultCodePage := reg.GetIntegerValue ('Default Message CodePage', CP_USASCII);
    fPurgeFolder := reg.GetStringValue ('Purge Folder', fPurgeFolder);
    fTruncateFrom := reg.GetStringValue ('Truncate Messages From', fTruncateFrom);
    fMessagebaseManagementDefault := reg.GetIntegerValue ('Messagebase Management Default', 1);
    fGatherSubjects := reg.GetIntegerValue('Gather Subjects', 0);
    fExpanded := reg.GetIntegerValue('Expanded', 0);

    fThreadOrder := reg.GetIntegerValue('Thread Order', Integer (toThreaded));
    fThreadSortDirection := reg.GetIntegerValue('Thread Sort Direction', Integer (sdAscending));
    fThreadSortOrder := reg.GetIntegerValue ('Thread Sort Order', Integer (soDate));

    if reg.HasValue ('NewSoundFile') or not FileExists (oldSoundFile) then
      fSoundFile := reg.GetStringValue ('Sound File', fSoundFile)
    else
      fSoundFile := reg.GetStringValue ('Sound File', oldSoundFile)
  end
  else
  begin
    fDefaultCodePage     := reg.GetIntegerValue ('Default Message CodePage', -1);
    fPurgeFolder         := reg.GetStringValue ('Purge Folder', #1);
    fTruncateFrom        := reg.GetStringValue ('Truncate Messages From', #1);
    fSoundFile           := reg.GetStringValue ('Sound File', #1);
    fMessagebaseManagementDefault := reg.GetIntegerValue ('Messagebase Management Default', -1);
    fGatherSubjects      := reg.GetIntegerValue('Gather Subjects', -1);
    fThreadOrder         := reg.GetIntegerValue ('Thread Order', -1);
    fThreadSortOrder     := reg.GetIntegerValue ('Thread Sort Order', -1);
    fThreadSortDirection := reg.GetIntegerValue ('Thread Sort Direction', -1);
    fExpanded            := reg.GetIntegerValue ('Expanded', 0);
  end
end;

procedure TDisplaySettings.SetDefaultCodePage(const Value: Integer);
begin
  SetProp (fDefaultCodePage, 'DefaultCodePage', Value);
end;

procedure TDisplaySettings.SetExpanded(const Value: boolean);
begin
  fExpanded := Ord (Value);
end;

procedure TDisplaySettings.SetGatherSubjects(const Value: boolean);
begin
  SetProp (fGatherSubjects, 'GatherSubjects', Ord (Value));
end;

procedure TDisplaySettings.SetMessagebaseManagementDefault(
  const Value: boolean);
begin
  SetProp (fMessagebaseManagementDefault, 'MessagebaseManagementDefault', Integer (Value));
end;

procedure TDisplaySettings.SetPurgeFolder(const Value: string);
begin
  SetProp (fPurgeFolder, 'PurgeFolder', Value)
end;

procedure TDisplaySettings.SetSoundFile(const Value: string);
begin
  SetProp (fSoundFile, 'SoundFile', Value);
end;

procedure TDisplaySettings.SetThreadOrder(const Value: TThreadOrder);
begin
  SetProp (fThreadOrder, 'ThreadOrder', Integer (Value));
end;

procedure TDisplaySettings.SetThreadSortDirection(
  const Value: TThreadSortDirection);
begin
  SetProp (fThreadSortDirection, 'ThreadSortDirection', Integer (Value));
end;

procedure TDisplaySettings.SetThreadSortOrder(
  const Value: TThreadSortOrder);
begin
  SetProp (fThreadSortOrder, 'ThreadSortOrder', Integer (Value));
end;

procedure TDisplaySettings.SetTruncateFrom(const Value: string);
begin
  SetProp (fTruncateFrom, 'TruncateFrom', Value);
end;

procedure TDisplaySettings.WriteSettings(reg: TExSettings);
begin
  if not Assigned (Parent) then
  begin
    reg.SetIntegerValue('Default Message CodePage', fDefaultCodePage, CP_USASCII);
    reg.SetStringValue ('Purge Folder', fPurgeFolder, ''); // 1.17.5.1  gArticleFolders.FindFolder ('').Name);
    reg.SetStringValue ('Truncate Messages From', fTruncateFrom, '');
    reg.SetIntegerValue('NewSoundFile', 1, 0);
    reg.SetStringValue('Sound File', fSoundFile, '');
    reg.SetIntegerValue ('Messagebase Management Default', fMessagebasemanagementDefault, 1);
    reg.SetIntegerValue ('Gather Subjects', fGatherSubjects, 0);
    reg.SetIntegerValue ('Thread Order', fThreadOrder, Integer (toThreaded));
    reg.SetIntegerValue('Thread Sort Direction', fThreadSortDirection, Integer (sdAscending));
    reg.SetIntegerValue ('Thread Sort Order', fThreadSortOrder, Integer (soDate));
    reg.SetIntegerValue ('Expanded', fExpanded, 0);
  end
  else
  begin
    WritePropToRegistry (reg, 'Default Message Codepage', fDefaultCodePage, 'DefaultCodepage');
    WritePropToRegistry (reg, 'Purge Folder', fPurgeFolder, 'PurgeFolder');
    WritePropToRegistry (reg, 'Truncate Messages From', fTruncateFrom, 'TruncateFrom');
    WritePropToRegistry (reg, 'Sound File', fSoundFile, 'SoundFile');
    WritePropToRegistry (reg, 'Messagebase Management Default', fMessagebaseManagementDefault, 'MessagebaseManagementDefault');
    WritePropToRegistry (reg, 'Gather Subjects', fGatherSubjects, 'GatherSubjects');
    WritePropToRegistry (reg, 'Expanded', fExpanded, 'Expanded');

    WritePropToRegistry (reg, 'Thread Order', fThreadOrder, 'ThreadOrder');
    WritePropToRegistry (reg, 'Thread Sort Direction', fThreadSortDirection, 'ThreadSortDirection');
    WritePropToRegistry (reg, 'Thread Sort Order', fThreadSortOrder, 'ThreadSortOrder');
  end
end;

{ TNNTPSettings }

procedure TNNTPSettings.Assign(source: TPersistent);
var
  src : TNNTPSettings;
begin
  if source is TNNTPSettings then
  begin
    src := TNNTPSettings (source);
    AdvancedHeaders := src.AdvancedHeaders;
    DefaultAction := src.DefaultAction;
    SetIdentityName (src.Identity.Name);
    NoArchive := src.NoArchive;
    MessageIDStub := src.MessageIDStub;
    MessageIDDomain := src.MessageIDDomain;
    GenerateMessageIDs := src.GenerateMessageIDs;
    GenerateDateHeaders := src.GenerateDateHeaders;
    GenerateApprovedHeaders:= src.GenerateApprovedHeaders;
    PerformDefaultAction := src.PerformDefaultAction;
    SignatureOverride := src.SignatureOverride;
  end
  else
    Inherited
end;

destructor TNNTPSettings.Destroy;
begin
  fDefaultAction.Free;

  inherited;
end;

function TNNTPSettings.GetAdvancedHeaders: string;
begin
  result := GetProp (fAdvancedHeaders, 'AdvancedHeaders');
end;

function TNNTPSettings.GetDefaultAction: TBatchAction;
begin
  if Assigned (fDefaultAction) or not Assigned (parent) then
    result := fDefaultAction
  else
    result := TNNTPSettings (Parent).DefaultAction;
end;

function TNNTPSettings.GetGenerateApprovedHeaders: boolean;
begin
  result := Boolean (GetProp (fGenerateApprovedHeaders, 'GenerateApprovedHeaders', 1));
end;

function TNNTPSettings.GetGenerateDateHeaders: boolean;
begin
  result := Boolean (GetProp (fGenerateDateHeaders, 'GenerateDateHeaders'));
end;

function TNNTPSettings.GetGenerateMessageIDs: boolean;
begin
  result := Boolean (GetProp (fGenerateMessageIDs, 'GenerateMessageIDs'));
end;

function TNNTPSettings.GetIdentity: TIdentity;
begin
  if Assigned (fIdentity) or (Parent = Nil) then
    result := fIdentity
  else
    result := TNNTPSettings (Parent).Identity;

  if not Assigned (result) then
    result := NNTPAccounts.Identities.DefaultIdentity
end;

function TNNTPSettings.GetMessageIDDomain: string;
begin
  result := GetProp (fMessageIDDomain, 'MessageIDDomain');
end;

function TNNTPSettings.GetMessageIDStub: string;
begin
  result := GetProp (fMessageIDStub, 'MessageIDStub');
end;

function TNNTPSettings.GetNoArchive: boolean;
begin
  result := Boolean (GetProp (fNoArchive, 'NoArchive'))
end;

function TNNTPSettings.GetPerformDefaultAction: TPerformDefaultAction;
begin
  result := TPerformDefaultAction (GetProp (fPerformDefaultAction, 'PerformDefaultAction'));
end;

function TNNTPSettings.GetSignatureOverride: string;
begin
  result := GetProp (fSignatureOverride, 'SignatureOverride');
end;

procedure TNNTPSettings.ReadSettings(reg: TExSettings);
var
  reg1 : TExSettings;
begin
  fNoArchive := reg.GetIntegerValue('NoArchive', -1);
  fAdvancedHeaders := reg.GetStringValue('Advanced Headers', #1);
  fMessageIDStub := reg.GetStringValue('Message ID Stub', #1);
  fSignatureOverride := reg.GetStringValue('Signature Override', #1);
  fMessageIDDomain := reg.GetStringValue('Message ID Domain', #1);
  fGenerateMessageIDs := reg.GetIntegerValue('Generate Message IDs', -1);
  fGenerateDateHeaders := reg.GetIntegerValue('Generate Date Headers', -1);
  fGenerateApprovedHeaders := reg.GetIntegerValue ('Generate Approved Headers', -1);
  fPerformDefaultAction := reg.GetIntegerValue ('Perform Default Action', -1);

  SetIdentityName(reg.GetStringValue ('Identity', ''));

  if reg.HasSection ('Default Action') then
  begin
    reg1 := CreateChildSettings (reg, 'Default Action');
    try
      fDefaultAction := TBatchAction.Create;
      fDefaultAction.LoadFromRegistry(reg1);
    finally
      reg1.Free
    end
  end
end;

procedure TNNTPSettings.SetAdvancedHeaders(const Value: string);
begin
  SetProp (fAdvancedHeaders, 'AdvancedHeaders', Value);
end;

procedure TNNTPSettings.SetDefaultAction(const Value: TBatchAction);
var
  ownerAction : TBatchAction;
  ownerActionCreated : boolean;
begin
  ownerActionCreated := False;
  ownerAction := Nil;
  try
    if Assigned (Parent) then
      ownerAction := TNNTPSettings (Parent).DefaultAction;

    if not Assigned (ownerAction) then
    begin
      ownerActionCreated := True;
      ownerAction := TBatchAction.Create;
      ownerAction.ActionType := batAllNew;
    end;

    if Assigned (Value) and not value.Equals(ownerAction) then
    begin
      if not Assigned (fDefaultAction) then
        fDefaultAction := TBatchAction.Create;

      fDefaultAction.Assign(Value)
    end
    else
      FreeAndNil (fDefaultAction)
  finally
    if ownerActionCreated then
      ownerAction.Free
  end
end;

procedure TNNTPSettings.SetGenerateApprovedHeaders(const Value: boolean);
begin
  SetProp (fGenerateApprovedHeaders, 'GenerateApprovedHeaders', Integer (value));
end;

procedure TNNTPSettings.SetGenerateDateHeaders(const Value: boolean);
begin
  SetProp (fGenerateDateHeaders, 'GenerateDateHeaders', Integer (value));
end;


procedure TNNTPSettings.SetGenerateMessageIDs(const Value: boolean);
begin
  SetProp (fGenerateMessageIDs, 'GenerateMessageIDs', Integer (value));
end;

procedure TNNTPSettings.SetIdentityName(const Value: string);
begin
  fIdentity := NNTPAccounts.Identities.Find(Value);
  if Assigned (Parent) and (fIdentity = TNNTPSettings (Parent).Identity) then
    fIdentity := Nil
end;

procedure TNNTPSettings.SetMessageIDDomain(const Value: string);
begin
  SetProp (fMessageIDDomain, 'MessageIDDomain', value);
end;

procedure TNNTPSettings.SetMessageIDStub(const Value: string);
begin
  SetProp (fMessageIDStub, 'MessageIDStub', value);
end;

procedure TNNTPSettings.SetNoArchive(const Value: boolean);
begin
  SetProp (fNoArchive, 'NoArchive', Integer (value));
end;

procedure TNNTPSettings.SetPerformDefaultAction(
  const Value: TPerformDefaultAction);
begin
  SetProp (fPerformDefaultAction, 'PerformDefaultAction', Integer (value));
end;

procedure TNNTPSettings.SetSignatureOverride(const Value: string);
begin
  SetProp (fSignatureOverride, 'SignatureOverride', value);
end;

procedure TNNTPSettings.WriteSettings(reg: TExSettings);
var
  reg1 : TExSettings;
begin
  WritePropToRegistry (reg, 'Advanced Headers', fAdvancedHeaders, 'AdvancedHeaders');
  WritePropToRegistry (reg, 'NoArchive', fNoArchive, 'NoArchive');
  WritePropToRegistry (reg, 'Generate Message IDs', fGenerateMessageIDs, 'GenerateMessageIDs');
  WritePropToRegistry (reg, 'Generate Date Headers', fGenerateDateHeaders, 'GenerateDateHeaders');
  WritePropToRegistry (reg, 'Generate Approved Headers', fGenerateApprovedHeaders, 'GenerateApprovedHeaders', 1);
  WritePropToRegistry (reg, 'Message ID Stub', fMessageIDStub, 'MessageIDStub');
  WritePropToRegistry (reg, 'Signature Override', fSignatureOverride, 'SignatureOverride');
  WritePropToRegistry (reg, 'Message ID Domain', fMessageIDDomain, 'MessageIDDomain');
  WritePropToRegistry (reg, 'Perform Default Action', fPerformDefaultAction, 'PerformDefaultAction');

  if Parent <> Nil then
    if Identity <> TNNTPSettings (Parent).Identity then
      reg.SetStringValue('Identity', Identity.Name, '')
    else
      reg.DeleteValue('Identity')
  else
    reg.DeleteValue ('Identity');

  if Assigned (fDefaultAction) then
  begin
    reg1 := CreateChildSettings (reg, 'Default Action');
    try
      fDefaultAction.SaveToRegistry(reg1);
    finally
      reg1.Free
    end;
  end
  else
    reg.DeleteSection ('Default Action');
end;

{ TServerSettings }

procedure TServerSettings.Assign(source: TPersistent);
var
  src : TServerSettings;
begin
  if source is TServerSettings then
  begin
    src := TServerSettings (source);

    fConnectTimeout:= src.fConnectTimeout;
    fRASConnection := src.fRASConnection;
    fReadTimeout:= src.fReadTimeout;
    fServerAccountName := src.fServerAccountName;
    fServerLogonRequired := src.fServerLogonRequired;
    fServerName := src.fServerName;
    fServerPassword := src.fServerPassword;
    fServerPort := src.fServerPort;
    fServerTimeout:= src.fServerTimeout;
    fSSLPort := src.fSSLPort;
    fSSLRequired := src.fSSLRequired;
    fId:= src.fId
  end
  else
    Inherited;
end;

constructor TServerSettings.Create(AParent: TSettings);
begin
  inherited Create (AParent);
  fConnectTimeout := 60;
  fReadTimeout := 60;
  fServerTimeout := 60;
end;

function TServerSettings.Equals(Obj: TObject): Boolean;
var
  settings: TServerSettings;
begin
  if Obj is TServerSettings then
  begin
    settings := TServerSettings(Obj);
    Result := (RASConnection = settings.RASConnection) and
              (ServerName = settings.ServerName) and
              (ServerAccountName = settings.ServerAccountName) and
              (ServerPort = settings.ServerPort) and
              (SSLPort = settings.SSLPort) and
              (SSLRequired = settings.SSLRequired) and
              (fID = settings.fId);
  end
  else
    Result := inherited Equals(Obj);
end;

function TServerSettings.GetConnectTimeout: Integer;
begin
  Result := GetProp(fConnectTimeout, 'ConnectTimeout');
end;

function TServerSettings.GetDefaultSSLPort: Integer;
begin
  result := 563;
end;

function TServerSettings.GetRASConnection: string;
begin
  result := GetProp (fRASConnection, 'RASConnection');
end;

function TServerSettings.GetReadTimeout: Integer;
begin
  Result := GetProp(fReadTimeout, 'ReadTimeout');
end;

function TServerSettings.GetServerAccountName: string;
begin
  result := GetProp (fServerAccountName, 'ServerAccountName');
end;

function TServerSettings.GetServerLogonRequired: boolean;
begin
  result := boolean (GetProp (fServerLogonRequired, 'ServerLogonRequired'));
end;

function TServerSettings.GetServerName: string;
begin
  result := GetProp (fServerName, 'ServerName');
end;

function TServerSettings.GetServerPassword: string;
begin
  result := GetProp (fServerPassword, 'ServerPassword');
end;

function TServerSettings.GetServerPort: Integer;
begin
  result := GetProp (fServerPort, 'ServerPort');
end;

function TServerSettings.GetServerTimeout: Integer;
begin
  Result := GetProp (fServerTimeout, 'ServerTimeout');
end;

function TServerSettings.GetSSLPort: Integer;
begin
  result := GetProp (fSSLPort, 'SSLPort');
end;

function TServerSettings.GetSSLRequired: boolean;
begin
  result := boolean (GetProp (fSSLRequired, 'SSLRequired'));
end;

procedure TServerSettings.ReadSettings(reg: TExSettings);
begin
  fConnectTimeout := reg.GetIntegerValue('Connect Timeout', 60);
  fRASConnection := reg.GetStringValue ('RAS Connection', '');
  fReadTimeout := reg.GetIntegerValue('Read Timeout', 60);
  fServerName := reg.GetStringValue('Server Name', '');
  fServerTimeout := reg.GetIntegerValue('Server Timeout', 60);
  fSSLPort := reg.GetIntegerValue ('SSL Port', GetDefaultSSLPort);
  fSSLRequired := reg.GetIntegerValue('SSL Required', -1);

  if reg.HasValue ('Server Account Name') then
  begin
    fServerAccountName := reg.GetStringValue ('Server Account Name', '');
    fServerLogonRequired := 1;
    fServerPassword := reg.GetStringValue('Server Password', '')
  end
  else
   fServerLogonRequired := -1;
end;

procedure TServerSettings.SetConnectTimeout(const Value: Integer);
begin
  SetProp(fConnectTimeout, 'ConnectTimeout', Value)
end;

procedure TServerSettings.SetRASConnection(const Value: string);
begin
  SetProp (fRASConnection, 'RASConnection', Value);
end;

procedure TServerSettings.SetReadTimeout(const Value: Integer);
begin
  SetProp(fReadTimeout, 'ReadTimeout', Value)
end;

procedure TServerSettings.SetServerAccountName(const Value: string);
begin
  SetProp (fServerAccountName, 'ServerAccountName', Value);
end;

procedure TServerSettings.SetServerLogonRequired(const Value: boolean);
begin
  SetProp (fServerLogonRequired, 'ServerLogonRequired', Integer (Value));
end;

procedure TServerSettings.SetServerName(const Value: string);
begin
  SetProp (fServerName, 'ServerName', Value);
end;

procedure TServerSettings.SetServerPassword(const Value: string);
begin
  SetProp (fServerPassword, 'ServerPassword', Value);
end;

procedure TServerSettings.SetServerPort(const Value: Integer);
begin
  SetProp (fServerPort, 'ServerPort', Value);
end;

procedure TServerSettings.SetServerTimeout(const Value: Integer);
begin
  SetProp (fServerTimeout, 'ServerTimeout', Value)
end;

procedure TServerSettings.SetSSLPort(const Value: Integer);
begin
  SetProp (fSSLPort, 'SSLPort', Value);
end;

procedure TServerSettings.SetSSLRequired(const Value: boolean);
begin
  SetProp (fSSLRequired, 'SSLRequired', Integer (Value));
end;

procedure TServerSettings.WriteSettings(reg: TExSettings);
begin
  reg.SetIntegerValue('Connect Timeout', fConnectTimeout, 60);
  reg.SetStringValue('RAS Connection', fRASConnection, '');
  reg.SetIntegerValue('Read Timeout', fReadTimeout, 60);
  reg.SetStringValue('Server Name', fServerName, '');
  reg.SetIntegerValue('Server Timeout', fServerTimeout, 60);
  reg.SetIntegerValue('SSL Port', fSSLPort, 563);
  reg.SetIntegerValue('SSL Required', fSSLRequired, -1);

  if fServerLogonRequired = 1 then
  begin
    reg.SetStringValue('Server Account Name', fServerAccountName, '');
    reg.SetStringValue('Server Password', fServerPassword, '');
  end
  else
  begin
    reg.DeleteValue ('Server Account Name');
    reg.DeleteValue ('Server Password');
  end
end;

{ TNNTPServerSettings }

procedure TNNTPServerSettings.Assign(source: TPersistent);
var
  src : TNNTPServerSettings;
begin
  inherited;
  if source is TNNTPServerSettings then
  begin
    src := TNNTPServerSettings (source);

    self.fAlwaysAuthenticate := src.fAlwaysAuthenticate;
    self.fPipelineSize := src.fPipelineSize;
    self.fMaxConnections := src.fMaxConnections;
  end
end;

constructor TNNTPServerSettings.Create(AParent: TSettings);
begin
  inherited Create (AParent);

  fPipelineSize := 1024;
  fServerPort := 119;
  fSSLPort := 563;
  fMaxConnections := 4;
end;

function TNNTPServerSettings.GetAlwaysAuthenticate: boolean;
begin
  result := Boolean (GetProp (fAlwaysAuthenticate, 'AlwaysAuthenticate'));
end;

function TNNTPServerSettings.GetMaxConnections: Integer;
begin
  result := GetProp (fMaxConnections, 'MaxConnections');
end;

function TNNTPServerSettings.GetPipelineSize: Integer;
begin
  result := GetProp (fPipelineSize, 'PipelineSize');
end;

procedure TNNTPServerSettings.ReadSettings(reg: TExSettings);
begin
  inherited;

  fServerPort := reg.GetIntegerValue('NNTP Port', 119);
  fPipeLineSize := reg.GetIntegerValue('Pipeline Size', 1024);
  fMaxConnections := reg.GetIntegerValue('Max Connections', 4);
  if reg.HasValue ('Server Account Name') then
    fAlwaysAuthenticate := Integer (reg.GetBooleanValue('Always Authenticate', False))
  else
    fAlwaysAuthenticate := 0
end;

procedure TNNTPServerSettings.SetAlwaysAuthenticate(const Value: boolean);
begin
  SetProp (fAlwaysAuthenticate, 'AlwaysAuthenticate', Integer (value))
end;

procedure TNNTPServerSettings.SetMaxConnections(const Value: Integer);
begin
  SetProp (fMaxConnections, 'MaxConnections', value);

end;

procedure TNNTPServerSettings.SetPipelineSize(const Value: Integer);
begin
  SetProp (fPipelineSize, 'PipelineSize', value)
end;

procedure TNNTPServerSettings.WriteSettings(reg: TExSettings);
begin
  inherited;
  reg.SetIntegerValue('NNTP Port', fServerPort, 119);
  reg.SetIntegerValue('Pipeline Size', fPipelineSize, 1024);
  reg.SetIntegerValue('Max Connections', fMaxConnections, 4);
  if fServerAccountName <> '' then
    reg.SetIntegerValue('Always Authenticate', fAlwaysAuthenticate, -1)
  else
    reg.DeleteValue ('Always Authenticate')
end;

{ TSMTPServerSettings }

constructor TSMTPServerSettings.Create(AParent: TSettings);
begin
  inherited Create (AParent);

  fServerPort := 25;
  fSSLPort := 465;
end;

function TSMTPServerSettings.GetDefaultSSLPort: Integer;
begin
  result := 465
end;

procedure TSMTPServerSettings.ReadSettings(reg: TExSettings);
var
  reg1 : TExSettings;
begin
  inherited ReadSettings (reg);

  if reg.HasSection ('SMTP Server') then
  begin
    reg1 := CreateChildSettings (reg, 'SMTP Server');
    try
      inherited ReadSettings (reg1);
      fServerPort := reg1.GetIntegerValue('SMTP Port', 25);

      if fServerPort = 0 then
        fServerPort := 25;     // Work round bug.
    finally
      reg1.Free
    end
  end
  else
    fServerPort := 25
end;

procedure TSMTPServerSettings.WriteSettings(reg: TExSettings);
var
  reg1 : TExSettings;
begin
  reg1 := CreateChildSettings(reg, 'SMTP Server');
  try
    fServerLogonRequired := Integer ((fServerAccountName <> '') or (fServerPassword <> ''));
    inherited WriteSettings (reg1);
    reg1.SetIntegerValue('SMTP Port', fServerPort, 25);
  finally
    reg1.Free
  end
end;

end.
