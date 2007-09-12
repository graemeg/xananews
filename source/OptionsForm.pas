unit OptionsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyBaseForm, VirtualTrees, ExtCtrls, StdCtrls,
  cmpPersistentPosition, Menus, unitNNTPServices;

type
  TfmOptions = class(TfmPropertyBase)
    Button1: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  public
    constructor Create (AOwner : TComponent); override;
  end;

var
  fmOptions: TfmOptions;

implementation

uses Registry, PropertyPageForm, unitNewsReaderOptions, NewsGlobals, MainForm,

     PropertyPageGeneralForm,
     PropertyPageEnterKeyForm,
     PropertyPageConnectionForm,
     PropertyPageGroupsTreeForm,
     PropertyPageFontForm,
     PropertyPageColorFontForm,
     PropertyPageMessageTreeActionsForm,
     PropertyPageMessageTreeDisplayForm,
     PropertyPageMessagePaneForm,
     PropertyPageCustomHeadersForm,
     PropertyPageWin98Form,
     PropertyPageKeywordsForm,
     PropertyPagePosting,
     PropertyPageExtraPostingForm,
     PropertyPageQuotingForm,
     PropertyPageFiltersForm,
     PropertyPagePreferencesForm,
     PropertyPageSortingForm,
     PropertyPageBozoForm,
     PropertyPageShortcutsForm;

{$R *.dfm}
{ TfmOptions }

constructor TfmOptions.Create(AOwner: TComponent);
var
  page, page1, pg1 : TPropertyPageDetails;


begin
  inherited Create (AOwner);

  page := AddPropertyPageDetails (TfmPropertyPageGeneral, Nil);
  AddPropertyPageDetails (TfmPropertyPageEnterKey, page);
  AddPropertyPageDetails (TfmPropertyPageConnection, page);

  page := AddPropertyPageDetails (TfmPropertyPageDummy, Nil, rstColorsFonts, rstColorsFontsHelp);

  page1 := AddPropertyPageDetails (TfmPropertyPageFont, page, rstMessageTree, rstColorFontMessageTreeHelp, '', Integer (apMessageHeaders));
  AddPropertyPageDetails (TfmPropertyPageColorFont, page1, '', '', '', Integer (apMessageHeaders));
  AddPropertyPageDetails (TfmPropertyPageColorFont, page1, '', '', '', Integer (apMessagesToMe));
  AddPropertyPageDetails (TfmPropertyPageColorFont, page1, '', '', '', Integer (apMessagesFromMe));
  AddPropertyPageDetails (TfmPropertyPageColorFont, page1, '', '', '', Integer (apXanaNewsMessages));
  AddPropertyPageDetails (TfmPropertyPageColorFont, page1, '', '', '', Integer (apDormantMessages));
  AddPropertyPageDetails (TfmPropertyPageColorFont, page1, '', '', '', Integer (apReplies));
  AddPropertyPageDetails (TfmPropertyPageColorFont, page1, '', '', '', Integer (apIgnoredMessages));
  AddPropertyPageDetails (TfmPropertyPageColorFont, page1, '', '', '', Integer (apChildlessMessages));
  AddPropertyPageDetails (TfmPropertyPageColorFont, page1, '', '', '', Integer (apInterestingMessages));

  page1 := AddPropertyPageDetails (TfmPropertyPageFont, page, rstMessagePane, rstColorFontMessagePaneHelp, '', Integer (apMessagePane));
  AddPropertyPageDetails (TfmPropertyPageColorFont, page1, '', '', '', Integer (apMessagePane));
  AddPropertyPageDetails (TfmPropertyPageColorFont, page1, '', '', '', Integer (apHeadersInMessagePane));
  AddPropertyPageDetails (TfmPropertyPageColorFont, page1, '', '', '', Integer (apSignaturesInMessagePane));
  AddPropertyPageDetails (TfmPropertyPageColorFont, page1, '', '', '', Integer (apLevel1Quotes));
  AddPropertyPageDetails (TfmPropertyPageColorFont, page1, '', '', '', Integer (apLevel2Quotes));
  AddPropertyPageDetails (TfmPropertyPageColorFont, page1, '', '', '', Integer (apLevel3Quotes));

  AddPropertyPageDetails (TfmPropertyPageColorFont, page, '', '', '', Integer (apMessageEditor));
  AddPropertyPageDetails (TfmPropertyPageColorFont, page, '', '', '', Integer (apSubscribedGroups));

  AddPropertyPageDetails (TfmPropertyPageGroupsTree, Nil);

  page := AddPropertyPageDetails (TfmPropertyPageMessageTreeDisplay, nil);
  AddPropertyPageDetails (TfmPropertyPageMessageTreeActions, page);

  page := AddPropertyPageDetails (TfmPropertyPageMessagePane, nil);
  AddPropertyPageDetails (TfmPropertyPageCustomHeaders, page);

  if Win32Platform <> VER_PLATFORM_WIN32_NT then
    AddPropertyPageDetails (TfmPropertyPageWin98, page);

  AddPropertyPageDetails (TfmPropertyPageKeywords, Nil);
  AddPropertyPageDetails (TfmPropertyPageBozos, Nil);

  if fmMain.fDisableShortcutCount = 0 then
    AddPropertyPageDetails (TfmPropertyPageShortcuts, Nil);

  page := AddPropertyPageDetails (TfmPropertyPageDummy, Nil, rstDefaultSettings, rstDefaultSettingsHelp, 'Options');

  pg1 := AddPropertyPageDetails (TfmPropertyPagePreferences, page, '', rstDefaultSettingsHelp, '', Integer (NNTPAccounts.DisplaySettings));
  TPropertyPagePreferencesData (pg1.Data).InitObject(NNTPAccounts);

  AddPropertyPageDetails (TfmPropertyPageSorting, page, '', rstDefaultSettingsHelp, '', Integer (NNTPAccounts.DisplaySettings));
  AddPropertyPageDetails (TfmPropertyPagePosting, page, '', rstDefaultSettingsHelp, '', Integer (NNTPAccounts.PostingSettings));
  AddPropertyPageDetails (TfmPropertyPageExtraPosting, page, '', rstDefaultSettingsHelp, '', Integer (NNTPAccounts.NNTPSettings));
  AddPropertyPageDetails (TfmPropertyPageQuoting, page, '', rstDefaultSettingsHelp, '', Integer (NNTPAccounts.PostingSettings));
  AddPropertyPageDetails (TfmPropertyPageFilters, page, '', '', '', Integer (NNTPAccounts));
end;

procedure TfmOptions.Button1Click(Sender: TObject);
var
  reg, reg1 : TRegistry; // Checked

  procedure SetProtocol (rootKey : HKEY; const proto : string);
  var
    reg : TRegistry;
  begin
    reg := TRegistry.Create (KEY_READ or KEY_WRITE);
    try
      reg.RootKey := rootKey;
      if reg.OpenKey('\Protocols\' + proto + '\DefaultIcon', True) then
        reg.WriteString('', '"' + ParamStr (0) + ',1"');

      if reg.OpenKey ('\Protocols\' + proto + '\shell\open\command', True) then
        reg.WriteString('', '"' + ParamStr (0) + '" %1')
    finally
      reg.Free
    end
  end;

  procedure SetClasses (const proto : string);
  begin
    if reg.OpenKey('\' + proto + '\DefaultIcon', True) then
      reg.WriteString('', '"' + ParamStr (0) + ',1"');

    if reg.OpenKey ('\' + proto + '\shell\open\command', True) then
      reg.WriteString ('', '"' + ParamStr (0) + '" %1')
  end;

begin
  reg1 := Nil;
  reg := TRegistry.Create (KEY_READ or KEY_WRITE);
  try
    if MessageBox (Handle, PChar (rstConfirmDefaultNewsreader), PChar (Application.Title), MB_YESNO or MB_ICONQUESTION or MB_DEFBUTTON2) <> IDYES then
      exit;
    reg.RootKey := HKEY_LOCAL_MACHINE;

    try
      if reg.OpenKey ('\Software\Clients\News', True) then
      begin
        reg1 := TRegistry.Create (KEY_READ or KEY_WRITE);
        reg1.RootKey := reg.CurrentKey;
        if reg1.OpenKey ('XanaNews', True) then
        begin
          reg1.WriteString('', 'XanaNews');
          SetProtocol (reg1.CurrentKey, 'news');
          SetProtocol (reg1.CurrentKey, 'snews');
          SetProtocol (reg1.CurrentKey, 'nntp');

          if reg1.OpenKey('shell\open\command', True) then
            reg1.WriteString('', '"' + ParamStr (0) + '"');
        end;

        reg.WriteString('', 'XanaNews');

        FreeAndNil (reg);
        FreeAndNil (reg1);

        reg := TRegistry.Create (KEY_READ or KEY_WRITE);
        reg.RootKey := HKEY_CLASSES_ROOT;
        SetClasses ('nntp');
        SetClasses ('news');
        SetClasses ('snews');
      end
    except
      ShowMessage (rstAdminOnly)
    end
  finally
    reg1.Free;
    reg.Free
  end
end;

procedure TfmOptions.FormShow(Sender: TObject);
var
  p : PVirtualNode;
begin
  inherited;

  p := vstSections.GetFirst;
  while p <> Nil do
    if vstSections.Text [p, 0] = rstColorsFonts then
    begin
      vstSections.FullCollapse(p);
      break
    end
    else
      p := vstSections.GetNextSibling(p)
end;

end.
