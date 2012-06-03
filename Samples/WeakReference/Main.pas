unit Main;

interface

uses
  DSharp.Core.Weak;

type
  IChild = interface;
  IParent = interface
    ['{62DC70E1-8D82-4012-BF01-452EB0F7F45A}']
    procedure AddChild(const AChild: IChild);
  end;

  IChild = interface
    ['{E1DB1DA0-55D6-408E-8143-072CA433412D}']
    function GetParent: IParent;
  end;

  TParent = class(TInterfacedObject, IParent)
  private
    FChild: IChild;
    procedure AddChild(const AChild: IChild);
  public
    destructor Destroy; override;
  end;

  TChild = class(TInterfacedObject, IChild)
  private
    FParent: Weak<IParent>;
  public
    constructor Create(AParent: IParent);
    destructor Destroy; override;

    function GetParent: IParent;
  end;

  IListItem = interface
    ['{7AEDD191-432C-454B-A855-8BF1137444AA}']
    function Append: IListItem;
    function Next: IListItem;
    function Prev: IListItem;
  end;

  TListItem = class(TInterfacedObject, IListItem)
  private
    FPrev: Weak<IListItem>;
    FNext: IListItem;
  public
    constructor Create(Prev, Next: IListItem);
    destructor Destroy; override;
    function Append: IListItem;
    function Next: IListItem;
    function Prev: IListItem;
  end;

procedure ChildParent;
procedure DoubleLinkedList;

implementation

{ TParent }

procedure TParent.AddChild(const AChild: IChild);
begin
  FChild := AChild;
end;

destructor TParent.Destroy;
begin
  if Assigned(FChild) then
    FChild := nil;
  inherited;
end;

{ TChild }

constructor TChild.Create(AParent: IParent);
begin
  inherited Create;
  FParent := AParent;
  AParent.AddChild(Self);
end;

destructor TChild.Destroy;
begin
  FParent := nil;
  inherited;
end;

function TChild.GetParent: IParent;
begin
  Result := FParent;
end;

{ TListItem }

constructor TListItem.Create(Prev, Next: IListItem);
begin
  FPrev := Prev;
  FNext := Next;
end;

destructor TListItem.Destroy;
begin
  if FPrev.IsAlive then
    Writeln('prev is alive!');
  inherited;
end;

function TListItem.Next: IListItem;
begin
  Result := FNext;
end;

function TListItem.Prev: IListItem;
begin
  Result := FPrev;
end;

function TListItem.Append: IListItem;
begin
  FNext := TListItem.Create(Self, nil);
  Result := FNext;
end;


procedure ChildParent;
var
  parent: IParent;
  child: IChild;
begin
  parent := TParent.Create;
  child := TChild.Create(parent);
//  child := nil;
//  parent := nil;

  parent := child.GetParent;
  parent.AddChild(child);
end;

procedure DoubleLinkedList;
var
  i: Integer;
  root, item: IListItem;
begin
  root := TListItem.Create(nil, nil);
  item := root;

  for i := 1 to 1000 do
  begin
    item := item.Append;
  end;

  item := root;
  i := 0;
  repeat
    if Assigned(item.Prev) then
      Inc(i);
    item := item.Next;
  until not Assigned(item);
  Writeln(i);
  item := nil;

  // drop the first 500 items
  item := root;
  for i := 1 to 500 do
    item := item.Next;
  root := item;

  // normally the cross references would keep all items alive
  // but with the weak reference to prev this is not the case
  root := nil;
end;

end.
