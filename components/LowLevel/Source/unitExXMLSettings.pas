(*======================================================================*
 | unitExXMLSettings                                                    |
 |                                                                      |
 | XML application settings classes.                                    |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2006  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      02/03/2006  CPWW  Original                                  |
 *======================================================================*)
unit unitExXMLSettings;

interface

uses
  Classes, SysUtils, unitExSettings, unitExFileSettings, MSXML;

type

  //-----------------------------------------------------------------------
  // TExXMLSettings.
  //
  // Class to store application and other settings to XML files
  TExXMLSettings = class(TExFileSettings)
  private
    fDoc: IXMLDOMDocument;
    fAutoReadOnly: Boolean;
    fSectionElement: IXMLDOMElement;
    fDirty: Boolean;

    function EncodeNodeName(nodeName: string): string;
    function DecodeNodeName(const nodeName: string): string;
    procedure SetupSectionElement;

    function FindChild(elem: IXMLDOMElement; const name: string; section: Boolean): IXMLDOMElement;
    function AppendChild(elem: IXMLDOMElement; const name: string): IXMLDOMElement;
    function AppendChildSection(elem: IXMLDOMElement; const name: string): IXMLDOMElement;
    function AppendChildValue(elem: IXMLDOMElement; const name, value: string): IXMLDOMElement;
    function IsSection(node: IXMLDOMElement): Boolean;
    procedure EnumChildNames(elem: IXMLDOMElement; childNames: TStrings; section: Boolean);

  protected
    function IsOpen: Boolean; override;
    function  CheckIsOpen(readOnly, autoReadOnly: Boolean): TIsOpen; override;
    procedure InternalSetStringValue(const valueName, value: string); override;
    procedure SetSection(const SectionPath: string); override;
  public
    procedure Close; override;
    constructor CreateChild(AParent: TExSettings; const ASection: string); override;
    function Open(readOnly: Boolean = False): Boolean; override;
    procedure Flush; override;

    procedure DeleteValue(const valueName: string); override;
    procedure DeleteSection(const sectionName: string); override;
    function HasSection(const ASection: string): Boolean; override;
    function HasValue(const AValue: string): Boolean; override;
    procedure GetValueNames(names: TStrings); override;
    procedure GetSectionNames(names: TStrings); override;
    procedure RenameSection(const oldValue, newValue: string); override;
    procedure RenameValue(const oldValue, newValue: string); override;

    function GetStringValue(const valueName: string; const deflt: string = ''): string; override;
    function GetExportValue(const valueName: string): string; override;
  end;

implementation

uses
  ComObj, unitSearchString;

{ TExXMLSettings }

function TExXMLSettings.CheckIsOpen(readOnly, autoReadOnly: Boolean): TIsOpen;
var
  fn: string;
begin
  // Ensure that the file is open.
  Result := inherited CheckIsOpen(readOnly, fAutoReadOnly);

  case Result of
    woClosed:
      begin
        if Open(readOnly) then
          Result := woOpen;
        fReadOnly := False;
      end;
    woReopen:
      begin
        fAutoReadOnly := readOnly;  // Actually, readOnly will always be False
                                    // because we wouldn't be reopening it
                                    // otherwise!

        fn := GetFileName('.xml');
        if not readOnly then
          ForceDirectories(ExtractFilePath(fn));
        Result := woOpen;
      end;
  end;
end;

procedure TExXMLSettings.Close;
begin
  if Parent = nil then
    Flush
  else
    TExXMLSettings(Parent).fDirty := TExXMLSettings(Parent).fDirty or fDirty;

  fSectionElement := nil;
  fDoc := nil;  // Release the DOC object
  fDirty := False;
end;

constructor TExXMLSettings.CreateChild(AParent: TExSettings; const ASection: string);
begin
  inherited;

  fDoc := TExXMLSettings(AParent).fDoc;
  SetupSectionElement;
end;


const
  escape = '-';

function TExXMLSettings.DecodeNodeName(const nodeName: string): string;
var
  L, ip, op: Integer;
  ch: Char;
  st: string;
begin
  L := Length(nodeName);
  SetLength(Result, L);

  ip := 1;
  op := 1;

  while ip <= L do
  begin
    ch := nodeName[ip];
    Inc(ip);

    if (ip = 2) and (ch = '_') then
    begin
      ch := nodeName[ip];
      Inc(ip);
    end;

    if ch = escape then
    begin
      if ip <= L - 1 then
      begin
        st := Copy(nodeName, ip, 2);
        Inc(ip, 2);
        ch := Char(StrToInt('$' + st));
      end;
    end
    else
      if ch = '_' then
        ch := ' ';

    Result[op] := ch;
    Inc(op);
  end;
  SetLength(Result, op - 1);
end;

procedure TExXMLSettings.DeleteSection(const sectionName: string);
var
  n: IXMLDOMElement;
begin
  CheckIsOpen(False, fAutoReadOnly);

  if fSectionElement <> nil then
  begin
    n := FindChild(fSectionElement, sectionName, True);
    if n <> nil then
    begin
      fSectionElement.removeChild(n);
      fDirty := True;
    end;
  end;
end;

procedure TExXMLSettings.DeleteValue(const valueName: string);
var
  n: IXMLDOMElement;
begin
  CheckIsOpen(False, fAutoReadOnly);

  if fSectionElement <> nil then
  begin
    n := FindChild(fSectionElement, valueName, False);
    if n <> nil then
    begin
      fSectionElement.removeChild(n);
      fDirty := True;
    end;
  end;
end;

const
  specialChars: set of char = ['$', '_', '%', '+', '!', '"', '£', '^', '&'];
  specialFirstChars = ['0'..'9', '.'];

(*----------------------------------------------------------------------*
 | procedure TExXMLSettings.EncodeNodeNames                             |
 |                                                                      |
 | Node names can't contain certian characters, and in addition they    |
 | can't start with numbers and dots.                                   |         |
 |                                                                      |
 | If a special character occurs, we escape it by replacing it with     |
 | '-', followed by the character's ordinal value in 2 hex digits.      |
 |                                                                      |
 | Spaces aren't valid either, so they get replaced with '_'.  If an    |
 | actual '_' occurs it is escaped.                                     |
 |                                                                      |
 | Finally, node names can't start with numbers or dots.  If they occur |
 | they are preceeded with a '_'.                                       |
 *----------------------------------------------------------------------*)
function TExXMLSettings.EncodeNodeName(nodeName: string): string;
var
  L, ip, op: Integer;
  ch: Char;
  st: string;
  dontEscape: Boolean;
begin
  nodeName := Trim(nodeName);

  L := Length(nodeName);
  SetLength(Result, L * 3);

  ip := 1;
  op := 1;
  while ip <= L do
  begin
    ch := nodeName[ip];
    Inc(ip);

    if ch = ' ' then
      Result[op] := '_'
    else
    begin
      if (ch = escape) or (ch in specialChars) or ((ip = 2) and (ch in specialFirstChars)) then
      begin
        dontEscape := False;
        if ip = 2 then
        begin
          Result[op] := '_';
          Inc(op);
          if (ch <> escape) and not (ch in specialChars)  then
            dontEscape := True;
        end;

        if not dontEscape then
        begin
          Result[op] := escape;
          Inc(op);

          st := IntToHex(Ord(ch), 2);
          Result[op] := st[1];
          Inc(op);
          ch := st[2];
        end
      end;
      Result[op] := ch;
    end;

    Inc(op);
  end;
  SetLength(Result, op - 1);
end;

procedure TExXMLSettings.EnumChildNames(elem: IXMLDOMElement;
  childNames: TStrings; section: Boolean);
var
  node: IXMLDOMNode;
begin
  childNames.Clear;
  if elem = nil then Exit;

  node := elem.firstChild;
  while (node <> nil) do
  begin
    if (node.nodeType = NODE_ELEMENT) and (section = IsSection(node as IXMLDOMElement)) then
      childNames.Add(DecodeNodeName(node.nodeName));
    node := node.nextSibling;
  end;
end;

function TExXMLSettings.FindChild(elem: IXMLDOMElement;
  const name: string; section: Boolean): IXMLDOMElement;
var
  node: IXMLDOMNode;
  nm: string;
begin
  // Find the XMLDOMNode for a child value or section
  node := elem.firstChild;
  nm := EncodeNodeName(name);

  while node <> nil do
  begin
    if node.nodeType = NODE_ELEMENT then
    begin
      Result := node as IXMLDOMElement;
      if (section = IsSection(Result)) and SameText(Result.nodeName, nm) then
        break;
      node := node.nextSibling;
    end;
  end;

  if node = nil then
    Result := nil;
end;

procedure TExXMLSettings.Flush;
begin
  if (fDoc <> nil) and fDirty then
  begin
    fDoc.Save(GetFileName('.xml'));
    fDirty := False;
  end;
end;

function TExXMLSettings.GetExportValue(const valueName: string): string;
begin
  Result := '"' + valueName + '"="' + MakeCStringConst(StringValue[valueName]) + '"';
end;

procedure TExXMLSettings.GetSectionNames(names: TStrings);
begin
  CheckIsOpen(True, fAutoReadOnly);
  EnumChildNames(fSectionElement, names, True);
end;

function TExXMLSettings.GetStringValue(const valueName, deflt: string): string;
var
  n: IXMLDOMElement;
begin
  CheckIsOpen(True, fAutoReadOnly);

  if fSectionElement <> nil then
  begin
    n := FindChild(fSectionElement, valueName, False);

    if n <> nil then
      Result := n.text
    else
      Result := deflt;
  end;
end;

procedure TExXMLSettings.GetValueNames(names: TStrings);
begin
  CheckIsOpen(True, fAutoReadOnly);
  EnumChildNames(fSectionElement, names, False);
end;

function TExXMLSettings.HasSection(const ASection: string): Boolean;
var
  e: IXMLDOMElement;
  s, st: string;
begin
  CheckIsOpen(True, fAutoReadOnly);
  Result := False;

  if fSectionElement <> nil then
  begin
    if ASection = '' then
      Result := True
    else
    begin
      s := ASection;
      e := fSectionElement;
      while s <> '' do
      begin
        st := SplitString('\', s);
        e := FindChild(e, st, True);
        if e = nil then
          Break;
      end;
      Result := e <> nil;
    end;
  end;
end;

function TExXMLSettings.HasValue(const AValue: string): Boolean;
var
  n: IXMLDOMElement;
begin
  CheckIsOpen(True, fAutoReadOnly);
  Result := False;

  if fSectionElement <> nil then
  begin
    n := FindChild(fSectionElement, AValue, False);

    if n <> nil then
      Result := True;
  end;
end;

function TExXMLSettings.AppendChild(elem: IXMLDOMElement; const name: string): IXMLDOMElement;
begin
  Result := fDoc.createElement(EncodeNodeName(name));
  elem.appendChild(Result);
  fDirty := True;
end;

function TExXMLSettings.AppendChildSection(elem: IXMLDOMElement;
  const name: string): IXMLDOMElement;
begin
  Result := AppendChild(elem, name);
  Result.setAttribute('Section', True);
end;

function TExXMLSettings.AppendChildValue(elem: IXMLDOMElement; const name, value: string): IXMLDOMElement;
begin
  Result := AppendChild(elem, name);
  Result.text := value;
end;

procedure TExXMLSettings.InternalSetStringValue(const valueName, value: string);
var
  n: IXMLDOMElement;
begin
  CheckIsOpen(False, fAutoReadOnly);

  if fSectionElement <> nil then
  begin
    n := FindChild(fSectionElement, valueName, False);

    if n <> nil then
      n.text := value
    else
      AppendChildValue(fSectionElement, valueName, value);

    fDirty := True;
  end;
end;

function TExXMLSettings.IsOpen: Boolean;
begin
  Result := fDoc <> nil;
end;

function TExXMLSettings.IsSection(node: IXMLDOMElement): Boolean;
var
  n: IXMLDOMNode;
begin
  n := node.getAttributeNode('Section');
  if n <> nil then
    Result := n.nodeValue
  else
    Result := False;
end;

function TExXMLSettings.Open(readOnly: Boolean): Boolean;
var
  fn, xml: string;
begin
  // Open the XML file.  Create it if it doesn't exist
  inherited Open(ReadOnly);
  Result := True;
  if fDoc <> nil then
    Close;
  fAutoReadOnly := readOnly;

  fn := GetFileName('.xml');
  if not readOnly then
    ForceDirectories(ExtractFilePath(fn))
  else
    if not FileExists(fn) then
    begin
      Result := False;
      Exit;
    end;

  fDoc := CoDOMDocument.Create;

  if not fDoc.load(fn) then
  begin
    xml := '<?xml version="1.0" encoding="UTF-8"?><' + EncodeNodeName(Application) + '></' + EncodeNodeName(Application) + '>';
    if not fDoc.loadXML(xml) then
      raise EExSettings.Create('Unable to create the XML document');
  end;

  SetupSectionElement;
end;

procedure TExXMLSettings.RenameSection(const oldValue, newValue: string);
var
  newN, n: IXMLDOMElement;
  c: IXMLDOMNode;
begin
  CheckIsOpen(False, fAutoReadOnly);

  if fSectionElement <> nil then
  begin
    n := FindChild(fSectionElement, newValue, True);
    if n <> nil then
      raise EExSettings.Create('Section ' + newValue + ' already exists');

    n := FindChild(fSectionElement, oldValue, True);
    if n = nil then
      raise EExSettings.Create('Section ' + oldValue + ' does not exist');

    newN := fDoc.createElement((EncodeNodeName(newValue)));

    // We know that the only attribute is 'Section' - so rather than
    // cloning the attributes, simply create a new Section attribute
    newN.setAttribute('Section', True);

    c := n.firstChild;
    while c <> nil do
    begin
      newN.appendChild(c.cloneNode(True));
      c := c.nextSibling;
    end;

    fSectionElement.replaceChild(newN, n);
    fDirty := True;
  end;
end;

procedure TExXMLSettings.RenameValue(const oldValue, newValue: string);
var
  newN, n: IXMLDOMElement;
begin
  CheckIsOpen(False, fAutoReadOnly);

  if fSectionElement <> nil then
  begin
    n := FindChild(fSectionElement, oldValue, False);
    if n = nil then
      raise EExSettings.Create('Value ' + oldValue + ' does not exist');

    newN := fDoc.createElement(EncodeNodeName(newValue));
    newN.text := n.text;
    fSectionElement.replaceChild(newN, n);

    fDirty := True;
  end;
end;

procedure TExXMLSettings.SetSection(const SectionPath: string);
begin
  inherited;
  SetupSectionElement;
end;

procedure TExXMLSettings.SetupSectionElement;
var
  s, n: string;
  node: IXMLDOMElement;
begin
  // Find or create the node for the current section
  if fDoc <> nil then
  begin
    fSectionElement := fDoc.documentElement;

    s := Section;

    repeat
      n := SplitString('\', s);
      if n = '' then break;

      node := FindChild(fSectionElement, n, True);

      if node = nil then
        fSectionElement := AppendChildSection(fSectionElement, n)
      else
        fSectionElement := node;
    until False;
  end
  else
    fSectionElement := nil;
end;

end.
