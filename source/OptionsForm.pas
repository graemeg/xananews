unit OptionsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PropertyBaseForm, VirtualTrees, ExtCtrls, StdCtrls,
  cmpPersistentPosition, Menus, unitNNTPServices;

type
  TfmOptions = class(TfmPropertyBase)
    btnDefaultNewsreader: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnDefaultNewsreaderClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  fmOptions: TfmOptions;

implementation

uses
  ShellAPI, Registry, PropertyPageForm, unitNewsReaderOptions, NewsGlobals, MainForm,
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

function IsVistaOrLater: Boolean;
begin
  Result := (Win32MajorVersion >= 6);
end;

function StartElevated(const commandline: string): Boolean;
var
  filename: string;
  filepath: string;
begin
  filename := Application.ExeName;
  filepath := ExtractFilePath(filename);

  Result := ShellExecute(0, 'runas', PChar(filename), PChar(commandline),
    PChar(filepath), SW_SHOW) > 32;
end;

{ TfmOptions }

constructor TfmOptions.Create(AOwner: TComponent);
var
  page, page1, pg1: TPropertyPageDetails;
begin
  inherited Create(AOwner);

  page := AddPropertyPageDetails(TfmPropertyPageGeneral, nil);
  AddPropertyPageDetails(TfmPropertyPageEnterKey, page);
  AddPropertyPageDetails(TfmPropertyPageConnection, page);

  page := AddPropertyPageDetails(TfmPropertyPageDummy, nil, rstColorsFonts, rstColorsFontsHelp);

  page1 := AddPropertyPageDetails(TfmPropertyPageFont, page, rstMessageTree, rstColorFontMessageTreeHelp, '', LPARAM(apMessageHeaders));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page1, '', '', '', LPARAM(apMessageHeaders));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page1, '', '', '', LPARAM(apMessagesToMe));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page1, '', '', '', LPARAM(apMessagesFromMe));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page1, '', '', '', LPARAM(apXanaNewsMessages));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page1, '', '', '', LPARAM(apDormantMessages));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page1, '', '', '', LPARAM(apReplies));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page1, '', '', '', LPARAM(apIgnoredMessages));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page1, '', '', '', LPARAM(apChildlessMessages));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page1, '', '', '', LPARAM(apInterestingMessages));

  page1 := AddPropertyPageDetails(TfmPropertyPageFont, page, rstMessagePane, rstColorFontMessagePaneHelp, '', LPARAM(apMessagePane));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page1, '', '', '', LPARAM(apMessagePane));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page1, '', '', '', LPARAM(apHeadersInMessagePane));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page1, '', '', '', LPARAM(apSignaturesInMessagePane));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page1, '', '', '', LPARAM(apLevel1Quotes));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page1, '', '', '', LPARAM(apLevel2Quotes));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page1, '', '', '', LPARAM(apLevel3Quotes));

  AddPropertyPageDetails(TfmPropertyPageColorFont, page, '', '', '', LPARAM(apMessageEditor));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page, '', '', '', LPARAM(apSubscribedGroups));

  page1 := AddPropertyPageDetails(TfmPropertyPageFont, page, rstMainForm, rstColorFontMainFormHelp, '', LPARAM(apMainForm));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page1, '', '', '', LPARAM(apMainForm));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page1, '', '', '', LPARAM(apMenu));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page1, '', '', '', LPARAM(apToolBar));
  AddPropertyPageDetails(TfmPropertyPageColorFont, page1, '', '', '', LPARAM(apMessageDetailsPanel));

  AddPropertyPageDetails(TfmPropertyPageGroupsTree, nil);

  page := AddPropertyPageDetails(TfmPropertyPageMessageTreeDisplay, nil);
  AddPropertyPageDetails(TfmPropertyPageMessageTreeActions, page);

  page := AddPropertyPageDetails(TfmPropertyPageMessagePane, nil);
  AddPropertyPageDetails(TfmPropertyPageCustomHeaders, page);

  if Win32Platform <> VER_PLATFORM_WIN32_NT then
    AddPropertyPageDetails(TfmPropertyPageWin98, page);

  AddPropertyPageDetails(TfmPropertyPageKeywords, nil);
  AddPropertyPageDetails(TfmPropertyPageBozos, nil);

  if fmMain.fDisableShortcutCount = 0 then
    AddPropertyPageDetails(TfmPropertyPageShortcuts, nil);

  page := AddPropertyPageDetails(TfmPropertyPageDummy, nil, rstDefaultSettings, rstDefaultSettingsHelp, 'Options');

  pg1 := AddPropertyPageDetails(TfmPropertyPagePreferences, page, '', rstDefaultSettingsHelp, '', LPARAM(NNTPAccounts.DisplaySettings));
  TPropertyPagePreferencesData(pg1.Data).InitObject(NNTPAccounts);

  AddPropertyPageDetails(TfmPropertyPageSorting, page, '', rstDefaultSettingsHelp, '', LPARAM(NNTPAccounts.DisplaySettings));
  AddPropertyPageDetails(TfmPropertyPagePosting, page, '', rstDefaultSettingsHelp, '', LPARAM(NNTPAccounts.PostingSettings));
  AddPropertyPageDetails(TfmPropertyPageExtraPosting, page, '', rstDefaultSettingsHelp, '', LPARAM(NNTPAccounts.NNTPSettings));
  AddPropertyPageDetails(TfmPropertyPageQuoting, page, '', rstDefaultSettingsHelp, '', LPARAM(NNTPAccounts.PostingSettings));
  AddPropertyPageDetails(TfmPropertyPageFilters, page, '', '', '', LPARAM(NNTPAccounts));

  btnDefaultNewsreader.ElevationRequired := IsVistaOrLater;
end;

procedure TfmOptions.btnDefaultNewsreaderClick(Sender: TObject);
var
  reg, reg1: TRegistry;

  procedure SetProtocol(rootKey: HKEY; const proto: string);
  var
    reg: TRegistry;
  begin
    reg := TRegistry.Create(KEY_READ or KEY_WRITE);
    try
      reg.RootKey := rootKey;
      if reg.OpenKey('\Protocols\' + proto + '\DefaultIcon', True) then
        reg.WriteString('', '"' + ParamStr(0) + ',1"');

      if reg.OpenKey('\Protocols\' + proto + '\shell\open\command', True) then
        reg.WriteString('', '"' + ParamStr(0) + '" %1');
    finally
      reg.Free;
    end;
  end;

  procedure SetClasses(const proto: string);
  begin
    if reg.OpenKey('\' + proto + '\DefaultIcon', True) then
      reg.WriteString('', '"' + ParamStr(0) + ',1"');

    if reg.OpenKey('\' + proto + '\shell\open\command', True) then
      reg.WriteString('', '"' + ParamStr(0) + '" %1');
  end;

begin
  if IsVistaOrLater then
  begin
    // A second elevated instance will create/write the necessary registry entries.
    StartElevated('-sadnr');
  end
  else
  begin
    if MessageBox(Handle, PChar(rstConfirmDefaultNewsreader), PChar(Application.Title), MB_YESNO or MB_ICONQUESTION or MB_DEFBUTTON2) <> IDYES then
      Exit;

    reg1 := nil;
    reg := TRegistry.Create(KEY_READ or KEY_WRITE);
    try
      reg.RootKey := HKEY_LOCAL_MACHINE;

      try
        if reg.OpenKey('\Software\Clients\News', True) then
        begin
          reg1 := TRegistry.Create(KEY_READ or KEY_WRITE);
          reg1.RootKey := reg.CurrentKey;
          if reg1.OpenKey('XanaNews', True) then
          begin
            reg1.WriteString('', 'XanaNews');
            SetProtocol(reg1.CurrentKey, 'news');
            SetProtocol(reg1.CurrentKey, 'snews');
            SetProtocol(reg1.CurrentKey, 'nntp');

            if reg1.OpenKey('shell\open\command', True) then
              reg1.WriteString('', '"' + ParamStr(0) + '"');
          end;

          reg.WriteString('', 'XanaNews');

          FreeAndNil(reg);
          FreeAndNil(reg1);

          reg := TRegistry.Create(KEY_READ or KEY_WRITE);
          reg.RootKey := HKEY_CLASSES_ROOT;
          SetClasses('nntp');
          SetClasses('news');
          SetClasses('snews');
        end;
      except
        ShowMessage(rstAdminOnly);
      end;
    finally
      reg1.Free;
      reg.Free;
    end;
  end;
end;

procedure TfmOptions.FormShow(Sender: TObject);
var
  p: PVirtualNode;
begin
  inherited;

  p := vstSections.GetFirst;
  while p <> nil do
    if vstSections.Text[p, 0] = rstColorsFonts then
    begin
      vstSections.FullCollapse(p);
      Break;
    end
    else
      p := vstSections.GetNextSibling(p);
end;

end.
