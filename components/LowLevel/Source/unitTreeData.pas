{*======================================================================*
 | unitTreeData                                                         |
 |                                                                      |
 | Base classes for tree-shaped data                                    |
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
 | Copyright © Colin Wilson 2005  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      23/03/2005  CPWW  Original                                  |
 *======================================================================*}


unit unitTreeData;

interface

uses SysUtils, Classes;

type

TTreeDataNodeFlag = (tdnfInitialized, tdnfGotChildCount);
TTreeDataNodeFlags = set of TTreeDataNodeFlag;

TTreeDataNodeState = (tdnsHasChildren, tdnsExpanded);
TTreeDataNodeStates = set of TTreeDataNodeState;

TTreeDataNodeInitialState = tdnsHasChildren..tdnsHasChildren;
TTreeDataNodeInitialStates = set of TTreeDataNodeInitialState;

TTreeData = class;

TTreeDataNode = class
private
  fOwner : TTreeData;
  fFlags : TTreeDataNodeFlags;
  fChild: TTreeDataNode;
  fSibling: TTreeDataNode;
  fParent: TTreeDataNode;
  fStates: TTreeDataNodeStates;
  fChildCount : Integer;
  fIndex : Integer;
  function GetHasChildren: boolean;
  function GetExpanded: boolean;
  function GetChild: TTreeDataNode;
  function GetStates: TTreeDataNodeStates;
  function GetLevel: Integer;
protected
  property ChildCount : Integer read fChildCount;
public
  constructor Create (AOwner : TTreeData; AParent : TTreeDataNode);
  destructor Destroy; override;

  property Child : TTreeDataNode read GetChild;
  property Expanded : boolean read GetExpanded;
  property Flags : TTreeDataNodeFlags read fFlags;
  property HasChildren : boolean read GetHasChildren;
  property States : TTreeDataNodeStates read GetStates;
  property Parent : TTreeDataNode read fParent;
  property Sibling : TTreeDataNode read fSibling;
  property Index : Integer read fIndex;
  property Level : Integer read GetLevel;
end;

TTreeDataNodeClass = class of TTreeDataNode;

TTreeData = class
private
  fOwner : TObject;
  fRoot : TTreeDataNode;
    function GetRoot: TTreeDataNode;
  function GetRootNodeCount: Integer;
  procedure SetRootNodeCount(Value: Integer);
  function GetChildCount (node : TTreeDataNode) : Integer;
  procedure SetChildCount (Node : TTreeDataNode; count : Integer);

  function AllocBranch (parent : TTreeDataNode; from, span : Integer) : TTreeDataNode;
  procedure FreeBranch (var branch : TTreeDataNode);
  procedure InitializeChildren (Node : TTreeDataNode);
  procedure InitializeNode (Node : TTreeDataNode);
  function GetFirstNode: TTreeDataNode;
  function GetExpanded(node: TTreeDataNode): boolean;
protected
  procedure DoInitChildren (Node : TTreeDataNode; var ChildCount : Integer); virtual; abstract;
  procedure DoInitNode (parent, node : TTreedataNode; var initialStates : TTreeDataNodeInitialStates); virtual; abstract;
  function InternalGetChild (Node : TTreeDataNode; idx : Integer) : TTreeDataNode;

  property Owner : TObject read fOwner;
public
  constructor Create (AOwner : TObject); virtual;
  destructor Destroy; override;
  class function GetNodeClass : TTreeDataNodeClass; virtual;

  procedure CollapseNode (Node : TTreeDataNode);
  procedure ExpandNode (Node : TTreeDataNode);
  function GetChild (Node : TTreeDataNode; idx : Integer) : TTreeDataNode;
  function GetNextSibling (Node : TTreeDataNode) : TTreeDataNode;

  property ChildCount [Node : TTreeDataNode] : Integer read GetChildCount;
  property Expanded [node : TTreeDataNode] : boolean read GetExpanded;
  property FirstNode : TTreeDataNode read GetFirstNode;
  property Root : TTreeDataNode read GetRoot;  // Invisible master root

published
  property RootNodeCount : Integer read GetRootNodeCount write SetRootNodeCount;
end;

TTreeDataClass = class of TTreeData;

implementation

{ TTreeData }

{*----------------------------------------------------------------------*
 | function TTreeData.AllocBranch                                       |
 |                                                                      |
 | Return a linked list of new, uninitialized siblings containg 'span'  |
 | nodes.                                                               |
 |                                                                      |
 | The first node will have an Index of 'from+1', etc.                  |
 *----------------------------------------------------------------------*}
function TTreeData.AllocBranch(parent: TTreeDataNode;
  from, span: Integer): TTreeDataNode;
var
  p, e : TTreeDataNode;
begin
  p := Nil;
  result := Nil;
  while span > 0 do
  begin
    e := GetNodeClass.Create(self, parent);
    Inc (from);
    e.fIndex := from;
    if p = Nil then
      result := e
    else
      p.fSibling := e;
    p := e;
    Dec (span)
  end
end;

procedure TTreeData.CollapseNode(Node: TTreeDataNode);
begin
  if node <> Nil then
  begin
    if not (tdnsExpanded in Node.States) then Exit;

    Exclude (Node.fStates, tdnsExpanded);
    Exclude (Node.fFlags, tdnfGotChildCount);
    Node.fChildCount := 0;
    FreeBranch (Node.fChild);
  end
end;

constructor TTreeData.Create(AOwner: TObject);
begin
  fOwner := AOwner;
  fRoot := GetNodeClass.Create (self, Nil);
    // nb.  (The invisible) fRoot is uninitialized at this time.  It may get
    //      initialized later in a call to SetRootNodeCount.  If not it will be
    //      passed to InitializeNode in the usual way.  In this case, the
    //      overridden DoInitNode should recognize it (by its Nil parent) and
    //      treat it as a special case.
end;

destructor TTreeData.Destroy;
begin
  fRoot.Free;

  inherited;
end;

procedure TTreeData.ExpandNode(Node: TTreeDataNode);
begin
  if node <> Nil then
  begin
    if tdnsExpanded in Node.States then Exit;

    if ChildCount [Node] > 0 then
      Include (Node.fStates, tdnsExpanded);
  end
end;

procedure TTreeData.FreeBranch(var branch: TTreeDataNode);
var
  p : TTreeDataNode;
begin
  p := branch;
  branch := Nil;
  p.Free
end;

function TTreeData.GetChild(Node: TTreeDataNode; idx: Integer): TTreeDataNode;
begin
  result := InternalGetChild (Node, idx);
  if Assigned (result) then
    InitializeNode (result)
end;

function TTreeData.GetChildCount (node : TTreeDataNode): Integer;
begin
  InitializeNode (Node);
  InitializeChildren (Node);
  result := Node.fChildCount;
end;

function TTreeData.GetExpanded(node: TTreeDataNode): boolean;
begin
  InitializeNode (Node);
  result := node.Expanded
end;

function TTreeData.GetFirstNode: TTreeDataNode;
begin

  InitializeNode (fRoot);
  InitializeChildren (fRoot);
  if fRoot.fChild <> Nil then
    fRoot.fStates := fRoot.fStates + [tdnsHasChildren, tdnsExpanded];

  result := fRoot.fChild;
  InitializeNode (result);
end;

function TTreeData.GetNextSibling(Node: TTreeDataNode): TTreeDataNode;
begin
  if Assigned (node) then
  begin
    result := Node.fSibling;
    InitializeNode (result)
  end
  else
    result := Nil
end;

class function TTreeData.GetNodeClass: TTreeDataNodeClass;
begin
  result := TTreeDataNode
end;

function TTreeData.GetRoot: TTreeDataNode;
begin
  result := fRoot;
  InitializeNode (result)
end;

function TTreeData.GetRootNodeCount: Integer;
begin
  result := ChildCount [fRoot];
end;

procedure TTreeData.InitializeChildren(Node: TTreeDataNode);
var
  childCount : Integer;
begin
  if not (tdnfInitialized in Node.Flags) then
    InitializeNode (Node);
  if tdnfGotChildCount in Node.Flags then Exit;
  Include (Node.fFlags, tdnfGotChildCount);

  DoInitChildren (Node, childCount);
  SetChildCount (Node, childCount);
end;

procedure TTreeData.InitializeNode(Node: TTreeDataNode);
var
  parent : TTreeDataNode;
  initialStates : TTreeDataNodeInitialStates;
  i : TTreeDataNodeInitialState;
begin
  if not Assigned (node) then Exit;
  
  if tdnfInitialized in Node.Flags then Exit;
  Include (Node.fFlags, tdnfInitialized);

  parent := Node.Parent;
  if parent = fRoot then
    parent := Nil;
  initialStates := [];
  DoInitNode (parent, node, initialStates);

  for i := Low (TTreeDataNodeInitialState) to High (TTreeDataNodeInitialState) do
    if i in initialStates then
      Include (node.fStates, i);
end;

function TTreeData.InternalGetChild(Node: TTreeDataNode;
  idx: Integer): TTreeDataNode;
begin
  result := node.Child;
  while idx > 0 do
  begin
    result := result.Sibling;
    Dec (idx)
  end
end;

procedure TTreeData.SetChildCount(Node: TTreeDataNode; count: Integer);
var
  existingCount : Integer;
  n : TTreeDataNode;
begin
  existingCount := ChildCount [Node];

  if count < existingCount then
    if count = 0 then
      FreeBranch (node.fChild)
    else
    begin
      n := InternalGetChild (Node, count - 1);
      FreeBranch (n.fSibling)
    end
  else
    if count > existingCount then
      if existingCount = 0 then
        Node.fChild := AllocBranch (Node, -1, count)
      else
        InternalGetChild (Node, existingCount - 1).fSibling := AllocBranch (Node, existingCount, count);
  Node.fChildCount := count
end;

procedure TTreeData.SetRootNodeCount(Value: Integer);
begin
  fRoot.fFlags := [tdnfInitialized, tdnfGotChildCount];
  if Value > 0 then
    fRoot.fStates := [tdnsHasChildren, tdnsExpanded];
  SetChildCount (fRoot, Value)
end;

{ TTreeDataNode }

constructor TTreeDataNode.Create(AOwner : TTreeData; AParent: TTreeDataNode);
begin
  fOwner := AOwner;
  fParent := AParent;
end;

destructor TTreeDataNode.Destroy;
begin
  fChild.Free;
  fSibling.Free;

  inherited;
end;

function TTreeDataNode.GetChild: TTreeDataNode;
begin
  fOwner.InitializeChildren(self);
  result := fChild
end;

function TTreeDataNode.GetExpanded: boolean;
begin
  result := tdnsExpanded in States;
end;

function TTreeDataNode.GetHasChildren: boolean;
begin
  result := tdnsHasChildren in States;
end;

function TTreeDataNode.GetLevel: Integer;
var
  n : TTreeDataNode;
begin
  result := 0;
  n := Parent;
  while Assigned (n) and Assigned (n.Parent) do
  begin
    Inc (result);
    n := n.Parent
  end
end;

function TTreeDataNode.GetStates: TTreeDataNodeStates;
begin
  fOwner.InitializeNode(self);
  result := fStates;
end;

end.
