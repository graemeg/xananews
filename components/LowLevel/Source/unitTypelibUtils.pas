unit unitTypelibUtils;

interface

uses Windows, ActiveX;

function CreateTypeLibrary (const fileName, name, docstring : string) : TGuid;

implementation

uses ComObj;

function CreateTypeLibrary (const fileName, name, docstring : string) : TGuid;
var
  wfn, wn, wds : WideString;
  ctl : ICreateTypeLib;
begin
  wfn := fileName;
  wn := name;
  wds := docstring;

  OleCheck (CreateTypeLib (SYS_WIN32, PWideChar (wfn), ctl));
  OleCheck (CoCreateGuid (result));
  OleCheck (ctl.SetGuid(result));
  OleCheck (ctl.SetLcid($409));
  OleCheck (ctl.SetVersion(1, 0));

  OleCheck (ctl.SetName(PWideChar (wn)));
  OleCheck (ctl.SetDocString(PWidechar (wds)));

  OleCheck (ctl.SaveAllChanges);
end;

end.
