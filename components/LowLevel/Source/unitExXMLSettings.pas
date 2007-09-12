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

uses Classes, SysUtils, unitExSettings, unitExFileSettings, MSXML;

type

//-----------------------------------------------------------------------
// TExXMLSettings.
//
// Class to store application and other settings to XML files
TExXMLSettings = class (TExFileSettings)
private
  fDoc : IXMLDOMDocument;
  fAutoReadOnly : boolean;
  fSectionElement : IXMLDOMElement;
  fDirty : boolean;

  function EncodeNodeName (nodeName : string) : string;
  function DecodeNodeName (const nodeName : string) : string;
  procedure SetupSectionElement;

  function FindChild (elem : IXMLDOMElement; const name : string; section : boolean) : IXMLDOMElement;
  function AppendChild(elem : IXMLDOMElement; const name: string) : IXMLDOMElement;
  function AppendChildSection (elem : IXMLDOMElement; const name : string) : IXMLDOMElement;
  function AppendChildValue(elem : IXMLDOMElement; const name, value : string) : IXMLDOMElement;
  function IsSection (node : IXMLDOMElement) : boolean;
  procedure EnumChildNames(elem : IXMLDOMElement; childNames : TStrings; section : boolean);

protected
  function IsOpen : boolean; override;
  function  CheckIsOpen (readOnly, autoReadOnly : boolean) : TIsOpen; override;
  procedure InternalSetStringValue (const valueName, value : string); override;
  procedure SetSection(const SectionPath : string); override;
public
  procedure Close; override;
  constructor CreateChild (AParent : TExSettings; const ASection : string); override;
  function Open (readOnly : boolean = false) : boolean; override;
  procedure Flush; override;

  procedure DeleteValue (const valueName : string); override;
  procedure DeleteSection (const sectionName : string); override;
  function HasSection (const ASection : string) : boolean; override;
  function HasValue (const AValue : string) : boolean; override;
  procedure GetValueNames (names : TStrings); override;
  procedure GetSectionNames (names : TStrings); override;
  procedure RenameSection (const oldValue, newValue : string); override;
  procedure RenameValue (const oldValue, newValue : string); override;

  function GetStringValue  (const valueName : string; const deflt : string = '') : string; override;
end;

implementation

uses ComObj, unitSearchString;

{ TExXMLSettings }

(*----------------------------------------------------------------------*
 | function TExXMLSettings.CheckIsOpen                                  |
 |                                                                      |
 | Ensure that the file is open.                                        |
 *----------------------------------------------------------------------*)
function TExXMLSettings.CheckIsOpen (readOnly, autoReadOnly : boolean) : TIsOpen;
var
  fn : string;
begin
  result := inherited CheckIsOpen (readOnly, fAutoReadOnly);

  case result of
    woClosed :
      begin
        if Open (readOnly) then
          result := woOpen;
        fReadOnly := False;
      end;
    woReopen :
      begin
        fAutoReadOnly := readOnly;  // Actually, readOnly will always be false
                                    // because we wouldn't be reopening it
                                    // otherwise!

        fn := GetFileName ('.xml');
        if not readOnly then
          ForceDirectories (ExtractFilePath (fn));
        result := woOpen
      end;
  end;
end;

(*----------------------------------------------------------------------*
 | procedure TExXMLSettings.Close                                       |
 |                                                                      |
 | Close the XML file                                                   |
 *----------------------------------------------------------------------*)
procedure TExXMLSettings.Close;
begin
  if Parent = Nil then
    Flush
  else
    TExXMLSettings (Parent).fDirty := TExXMLSettings (Parent).fDirty or fDirty;

  fSectionElement := Nil;
  fDoc := Nil;  // Release the DOC object
  fDirty := False;
end;

constructor TExXMLSettings.CreateChild(AParent: TExSettings;
  const ASection: string);
begin
  inherited;

  fDoc := TExXMLSettings (AParent).fDoc;
  SetupSectionElement;
end;


const
  escape = '-';
  
function TExXMLSettings.DecodeNodeName(const nodeName: string): string;
var
  l, ip, op : Integer;
  ch : char;
  st : string;
begin
  l := Length (nodeName);
  SetLength (result, l);

  ip := 1;
  op := 1;

  while ip <= l do
  begin
    ch := nodeName [ip];
    Inc (ip);

    if (ip = 2) and (ch = '_') then
    begin
      ch := nodeName [ip];
      Inc (ip)
    end;

    if ch = escape then
    begin
      if ip <= l - 2 then
      begin
        st := Copy (nodeName, ip, 2);
        Inc (ip, 2);
        ch := char (StrToInt ('$' + st));
      end;
    end
    else
      if ch = '_' then
        ch := ' ';

    result [op] := ch;
    Inc (op)
  end;
  SetLength (result, op-1);
end;

(*----------------------------------------------------------------------*
 | procedure TExXMLSettings.DeleteSection                               |
 |                                                                      |
 | Delete a section                                                     |
 *----------------------------------------------------------------------*)
procedure TExXMLSettings.DeleteSection(const sectionName: string);
var
  n : IXMLDOMElement;
begin
  CheckIsOpen (false, fAutoReadOnly);

  if fSectionElement <> Nil then
  begin
    n := FindChild (fSectionElement, sectionName, true);
    if n <> Nil then
    begin
      fSectionElement.removeChild(n);
      fDirty := True
    end
  end
end;

(*----------------------------------------------------------------------*
 | procedure TExXMLSettings.DeleteValue                                 |
 |                                                                      |
 | Delete a value                                                       |
 *----------------------------------------------------------------------*)
procedure TExXMLSettings.DeleteValue(const valueName: string);
var
  n : IXMLDOMElement;
begin
  CheckIsOpen (false, fAutoReadOnly);

  if fSectionElement <> Nil then
  begin
    n := FindChild (fSectionElement, valueName, false);
    if n <> Nil then
    begin
      fSectionElement.removeChild(n);
      fDirty := True
    end
  end
end;

const
  specialChars : set of char = ['$', '_', '%', '+', '!', '"', '£', '^', '&'];
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
  l, ip, op : Integer;
  ch : char;
  st : string;
  dontEscape : boolean;
begin
  nodeName := Trim (nodeName);

  l := Length (nodeName);
  SetLength (result, l * 3);

  ip := 1;
  op := 1;
  while ip <= l do
  begin
    ch := nodeName [ip];
    Inc (ip);

    if ch = ' ' then
      result [op] := '_'
    else
    begin
      if (ch = escape) or (ch in specialChars) or ((ip = 2) and (ch in specialFirstChars)) then
      begin
        dontEscape := False;
        if ip = 2 then
        begin
          result [op] := '_';
          Inc (op);
          if (ch <> escape) and not (ch in specialChars)  then
            dontEscape := True
        end;

        if not dontEscape then
        begin
          result [op] := escape;
          Inc (op);

          st := IntToHex (Ord (ch), 2);
          result [op] := st [1];
          Inc (op);
          ch := st [2];
        end
      end;
      result [op] := ch
    end;

    Inc (op)
  end;
  SetLength (result, op - 1);
end;

procedure TExXMLSettings.EnumChildNames(elem: IXMLDOMElement;
  childNames: TStrings; section: boolean);
var
  node : IXMLDOMNode;
begin
  childNames.Clear;
  if elem = Nil then Exit;

  node := elem.firstChild;
  while (node <> Nil) do
  begin
    if (node.nodeType = NODE_ELEMENT) and (section = IsSection (node as IXMLDOMElement)) then
      childNames.Add(DecodeNodeName (node.nodeName));
    node := node.nextSibling;
  end
end;

(*----------------------------------------------------------------------*
 | function TExXMLSettings.FindChild                                    |
 |                                                                      |
 | Find the XMLDOMNode for a child value or section                     |
 *----------------------------------------------------------------------*)
function TExXMLSettings.FindChild(elem: IXMLDOMElement;
  const name: string; section : boolean): IXMLDOMElement;
var
  node : IXMLDOMNode;
  nm : string;

begin
  node := elem.firstChild;
  nm := EncodeNodeName (name);

  while node <> Nil do
  begin
    if node.nodeType = NODE_ELEMENT then
    begin
      result := node as IXMLDOMElement;
      if (section = IsSection (result)) and SameText (result.nodeName, nm) then
        break;
      node := node.nextSibling
    end
  end;

  if node = nil then
    result := nil
end;

(*----------------------------------------------------------------------*
 | procedure TExXMLSettings.Flush                                       |
 |                                                                      |
 | Save the settings to the XML file                                    |
 *----------------------------------------------------------------------*)
procedure TExXMLSettings.Flush;
begin
  if (fDoc <> Nil) and fDirty then
  begin
    fDoc.Save (GetFileName ('.xml'));
    fDirty := False
  end
end;

(*----------------------------------------------------------------------*
 | function TExXMLSettings.GetStringValue                               |
 |                                                                      |
 | Return a sting value, or the default if the value doesn't exist      |
 *----------------------------------------------------------------------*)
procedure TExXMLSettings.GetSectionNames(names: TStrings);
begin
  CheckIsOpen (true, fAutoReadOnly);
  EnumChildNames (fSectionElement, names, true);
end;

function TExXMLSettings.GetStringValue(const valueName, deflt: string): string;
var
  n : IXMLDOMElement;
begin
  CheckIsOpen (true, fAutoReadOnly);

  if fSectionElement <> Nil then
  begin
    n := FindChild (fSectionElement, valueName, false);

    if n <> Nil then
      result := n.text
    else
      result := deflt
  end
end;

procedure TExXMLSettings.GetValueNames(names: TStrings);
begin
  CheckIsOpen (true, fAutoReadOnly);
  EnumChildNames (fSectionElement, names, false)
end;

function TExXMLSettings.HasSection(const ASection: string): boolean;
var
  e : IXMLDOMElement;
  s, st : string;
begin
  CheckIsOpen (true, fAutoReadOnly);
  result := False;

  if fSectionElement <> Nil then
  begin
    if ASection = '' then
      result := True
    else
    begin
      s := ASection;
      e := fSectionElement;
      while s <> '' do
      begin
        st := SplitString ('\', s);
        e := FindChild (e, st, true);
        if e = Nil then break
        
      end;
      result := e <> Nil
    end
  end
end;

function TExXMLSettings.HasValue(const AValue: string): boolean;
var
  n : IXMLDOMElement;
begin
  CheckIsOpen (true, fAutoReadOnly);
  result := False;

  if fSectionElement <> Nil then
  begin
    n := FindChild (fSectionElement, AValue, false);

    if n <> Nil then
      result := True
  end
end;

(*----------------------------------------------------------------------*
 | function TExXMLSettings.AppendChild                                  |
 |                                                                      |
 | Append a child value or section to the specified section node        |
 *----------------------------------------------------------------------*)
function TExXMLSettings.AppendChild(elem : IXMLDOMElement; const name: string) : IXMLDOMElement;
begin
  result := fDoc.createElement(EncodeNodeName (name));
  elem.appendChild(result);
  fDirty := True;
end;

function TExXMLSettings.AppendChildSection(elem: IXMLDOMElement;
  const name: string): IXMLDOMElement;
begin
  result := AppendChild (elem, name);
  result.setAttribute('Section', true);
end;

function TExXMLSettings.AppendChildValue(elem : IXMLDOMElement; const name, value : string) : IXMLDOMElement;
begin
  result := AppendChild (elem, name);
  result.text := value;
end;

(*----------------------------------------------------------------------*
 | procedure TExXMLSettings.InternalSetStringValue                      |
 |                                                                      |
 | Set a string value.                                                  |
 *----------------------------------------------------------------------*)
procedure TExXMLSettings.InternalSetStringValue(const valueName, value: string);
var
  n : IXMLDOMElement;
begin
  CheckIsOpen (false, fAutoReadOnly);

  if fSectionElement <> Nil then
  begin
    n := FindChild (fSectionElement, valueName, false);

    if n <> Nil then
      n.text := value
    else
      AppendChildValue(fSectionElement, valueName, value);

    fDirty := True
  end
end;

(*----------------------------------------------------------------------*
 | function TExXMLSettings.IsOpen                                       |
 |                                                                      |
 | Return true if the object is Open                                    |
 *----------------------------------------------------------------------*)
function TExXMLSettings.IsOpen: boolean;
begin
  result := fDoc <> Nil
end;

function TExXMLSettings.IsSection(node: IXMLDOMElement): boolean;
var
  n : IXMLDOMNode;
begin
  n := node.getAttributeNode('Section');
  if n <> Nil then
    result := n.nodeValue
  else
    result := False
end;

(*----------------------------------------------------------------------*
 | procedure TExXMLSettings.Open                                        |
 |                                                                      |
 | Open the XML file.  Create it if it doesn't exist                    |
 *----------------------------------------------------------------------*)
function TExXMLSettings.Open(readOnly: boolean) : boolean;
var
  fn, xml : string;
begin
  inherited Open (ReadOnly);
  result := True;
  if fDoc <> Nil then
    Close;
  fAutoReadOnly := readOnly;

  fn := GetFileName ('.xml');
  if not readOnly then
    ForceDirectories (ExtractFilePath (fn))
  else
    if not FileExists (fn) then
    begin
      result := False;
      Exit;
    end;

  fDoc := CoDOMDocument.Create;

  if not fDoc.load(fn) then
  begin
    xml := '<?xml version="1.0" encoding="UTF-8"?><' + EncodeNodeName (Application) + '></' + EncodeNodeName (Application) + '>';
    if not fDoc.loadXML(xml) then
      raise EExSettings.Create ('Unable to create the XML document');
  end;

  SetupSectionElement
end;

procedure TExXMLSettings.RenameSection(const oldValue, newValue: string);
var
  newN, n : IXMLDOMElement;
  c : IXMLDOMNode;
begin
  CheckIsOpen (false, fAutoReadOnly);

  if fSectionElement <> Nil then
  begin
    n := FindChild (fSectionElement, newValue, true);
    if n <> Nil then
      raise EExSettings.Create('Section ' + newValue + ' already exists');

    n := FindChild (fSectionElement, oldValue, true);
    if n = Nil then
      raise EExSettings.Create('Section ' + oldValue + ' does not exist');

    newN := fDoc.createElement ((EncodeNodeName (newValue)));

    // We know that the only attribute is 'Section' - so rather than
    // cloning the attributes, simply create a new Section attribute
    newN.setAttribute('Section', true);

    c := n.firstChild;
    while c <> Nil do
    begin
      newN.appendChild(c.cloneNode(true));
      c := c.nextSibling;
    end;

    fSectionElement.replaceChild(newN, n);
    fDirty := True
  end
end;

procedure TExXMLSettings.RenameValue(const oldValue, newValue: string);
var
  newN, n : IXMLDOMElement;
begin
  CheckIsOpen (false, fAutoReadOnly);

  if fSectionElement <> Nil then
  begin
    n := FindChild (fSectionElement, oldValue, false);
    if n = Nil then
      raise EExSettings.Create('Value ' + oldValue + ' does not exist');

    newN := fDoc.createElement(EncodeNodeName (newValue));
    newN.text := n.text;
    fSectionElement.replaceChild(newN, n);

    fDirty := True
  end
end;

(*----------------------------------------------------------------------*
 | procedure TExXMLSettings.SetSection                                  |
 |                                                                      |
 | Override the 'Set' method for the Section property                   |
 *----------------------------------------------------------------------*)
procedure TExXMLSettings.SetSection(const SectionPath: string);
begin
  inherited;

  SetupSectionElement;
end;

(*----------------------------------------------------------------------*
 | procedure TExXMLSettings.SetupSectionElement                         |
 |                                                                      |
 | Find or create the node for the current section                      |
 *----------------------------------------------------------------------*)
procedure TExXMLSettings.SetupSectionElement;
var
  s, n : string;
  node : IXMLDOMElement;
begin
  if fDoc <> Nil then
  begin
    fSectionElement := fDoc.documentElement;

    s := Section;

    repeat
      n := SplitString ('\', s);
      if n = '' then break;

      node := FindChild (fSectionElement, n, true);

      if node = Nil then
        fSectionElement := AppendChildSection (fSectionElement, n)
      else
        fSectionElement := node;
    until false;
  end
  else
    fSectionElement := Nil;
end;

end.
