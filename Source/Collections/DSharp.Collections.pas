(*
  Copyright (c) 2011, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

unit DSharp.Collections;

interface

uses
  Classes,
  DSharp.Core.Events,
  Generics.Defaults,
  Rtti;

type
  TCollectionChangedAction = (caAdd, caRemove, caReplace, caMove);
  TCollectionChangedEvent<T> = procedure(Sender: TObject; Item: T;
    Action: TCollectionChangedAction) of object;

  IEnumerator = interface
    function GetCurrent: TValue;
    function MoveNext: Boolean;
    property Current: TValue read GetCurrent;
  end;

  IEnumerator<T> = interface(IEnumerator)
    function GetCurrent: T;
    property Current: T read GetCurrent;
  end;

  IEnumerable = interface
    function GetEnumerator: IEnumerator;
  end;

  IEnumerable<T> = interface(IEnumerable)
    function GetEnumerator: IEnumerator<T>;
  end;

  IList<T> = interface(IEnumerable<T>)
    function GetCount: NativeInt;

    function Add(Value: T): NativeInt;
    procedure AddRange(Values: array of T); overload;
    procedure AddRange(Values: IEnumerable<T>); overload;
    procedure Clear;
    function Contains(Value: T): Boolean;
    procedure Delete(Index: NativeInt);
    procedure DeleteRange(Index, Count: NativeInt);
    function First: T;
    function GetItem(Index: NativeInt): T;
    function IndexOf(Value: T): NativeInt;
    procedure Insert(Index: NativeInt; Value: T);
    procedure InsertRange(Index: NativeInt; Values: array of T); overload;
    procedure InsertRange(Index: NativeInt; Values: IEnumerable<T>); overload;
    function Last: T;
    function LastIndexOf(Value: T): NativeInt;
    procedure Move(OldIndex, NewIndex: NativeInt);
    function Remove(Value: T): NativeInt;
    procedure SetItem(Index: NativeInt; const Value: T);

    function ToArray: TArray<T>;

    property Count: NativeInt read GetCount;
    property Items[Index: NativeInt]: T read GetItem write SetItem; default;
  end;

  TEnumerator = class(TInterfacedObject, IEnumerator)
  private
    function GetCurrentBase: TValue; virtual;
    function IEnumerator.GetCurrent = GetCurrentBase;
  public
    function MoveNext: Boolean; virtual;
    property Current: TValue read GetCurrentBase;
  end;

  TEnumerator<T> = class(TEnumerator, IEnumerator<T>)
  private
    function GetCurrentBase: TValue; override;
  protected
    function GetCurrent: T; virtual;
  public
    property Current: T read GetCurrent;
  end;

  TEnumerable = class(TInterfacedObject, IEnumerable)
  private
    function GetEnumeratorBase: IEnumerator; virtual;
    function IEnumerable.GetEnumerator = GetEnumeratorBase;
  end;

  TEnumerable<T> = class(TEnumerable, IEnumerable<T>, IEnumerable)
  private
    function GetEnumeratorBase: IEnumerator; override;
  public
    function GetEnumerator: IEnumerator<T>; reintroduce; virtual;
  end;

  TList<T> = class(TEnumerable<T>, IList<T>)
  private
    FItems: TArray<T>;
    FCount: NativeInt;
    FComparer: IComparer<T>;
    FOnCollectionChanged: TEvent<TCollectionChangedEvent<T>>;
    function GetCount: NativeInt;
    function GetItem(Index: NativeInt): T;
    function GetOnCollectionChanged: TEvent<TCollectionChangedEvent<T>>;
    procedure Grow;
    procedure SetItem(Index: NativeInt; const Value: T);
  protected
    procedure Notify(Value: T; Action: TCollectionChangedAction); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Value: T): NativeInt;
    procedure AddRange(Values: array of T); overload;
    procedure AddRange(Values: IEnumerable<T>); overload;
    procedure Clear;
    function Contains(Value: T): Boolean;
    procedure Delete(Index: NativeInt);
    procedure DeleteRange(Index, Count: NativeInt);
    function First: T;
    function IndexOf(Value: T): NativeInt;
    procedure Insert(Index: NativeInt; Value: T);
    procedure InsertRange(Index: NativeInt; Values: array of T); overload;
    procedure InsertRange(Index: NativeInt; Values: IEnumerable<T>); overload;
    function Last: T;
    function LastIndexOf(Value: T): NativeInt;
    procedure Move(OldIndex, NewIndex: NativeInt);
    function Remove(Value: T): NativeInt;

    function GetEnumerator: IEnumerator<T>; override;
    function ToArray: TArray<T>;

    type
      TEnumerator = class(TEnumerator<T>)
      private
        FList: TList<T>;
        FIndex: Integer;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(AList: TList<T>);
        function MoveNext: Boolean; override;
        property Current: T read GetCurrent;
      end;

    property Count: NativeInt read GetCount;
    property Items[Index: NativeInt]: T read GetItem write SetItem; default;
    property OnCollectionChanged: TEvent<TCollectionChangedEvent<T>>
      read GetOnCollectionChanged;
  end;

  TObjectList<T: class> = class(TList<T>)
  private
    FOwnsObjects: Boolean;
  protected
    procedure Notify(Value: T; Action: TCollectionChangedAction); override;
  public
    constructor Create(AOwnsObjects: Boolean = True);

    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

implementation

uses
  RTLConsts,
  SysUtils;

{ TEnumerator }

function TEnumerator.GetCurrentBase: TValue;
begin
  Result := TValue.Empty;
end;

function TEnumerator.MoveNext: Boolean;
begin
  Result := False;
end;

{ TEnumerator<T> }

function TEnumerator<T>.GetCurrent: T;
begin
  Result := Default(T);
end;

function TEnumerator<T>.GetCurrentBase: TValue;
begin
  Result := TValue.From<T>(GetCurrent());
end;

{ TEnumerable }

function TEnumerable.GetEnumeratorBase: IEnumerator;
begin
  Result := TEnumerator.Create();
end;

{ TEnumerable<T> }

function TEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator<T>.Create();
end;

function TEnumerable<T>.GetEnumeratorBase: IEnumerator;
begin
  Result := GetEnumerator();
end;

{ TList<T> }

function TList<T>.Add(Value: T): NativeInt;
begin
  Insert(FCount, Value);
end;

procedure TList<T>.AddRange(Values: array of T);
begin
  InsertRange(FCount, Values);
end;

procedure TList<T>.AddRange(Values: IEnumerable<T>);
begin
  InsertRange(FCount, Values);
end;

procedure TList<T>.Clear;
var
  i: Integer;
begin
  for i := FCount - 1 downto 0 do
    Delete(i);
  SetLength(FItems, 0);
end;

function TList<T>.Contains(Value: T): Boolean;
begin
  Result := IndexOf(Value) >= 0;
end;

constructor TList<T>.Create;
begin
  FComparer := TComparer<T>.Default;
end;

procedure TList<T>.Delete(Index: NativeInt);
var
  LItem: T;
begin
  if (Index < 0) or (Index >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  LItem := FItems[Index];
  FItems[Index] := Default(T);
  Dec(FCount);
  if Index < FCount then
  begin
    System.Move(FItems[Index + 1], FItems[Index], (FCount - Index) * SizeOf(T));
    FillChar(FItems[FCount], SizeOf(T), 0);
  end;

  Notify(LItem, caRemove);
end;

procedure TList<T>.DeleteRange(Index, Count: NativeInt);
var
  i: NativeInt;
begin
  if (Index < 0) or (Count < 0) or (Index + Count < 0)
    or (Index + Count > FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  for i := 0 to Count - 1 do
    Delete(Index);
end;

destructor TList<T>.Destroy;
begin
  Clear();
  inherited;
end;

function TList<T>.First: T;
begin
  Result := Items[0];
end;

function TList<T>.GetCount: NativeInt;
begin
  Result := FCount;
end;

function TList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TList<T>.GetItem(Index: NativeInt): T;
begin
  if (Index < 0) or (Index >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  Result := FItems[Index];
end;

function TList<T>.GetOnCollectionChanged: TEvent<TCollectionChangedEvent<T>>;
begin
  Result := FOnCollectionChanged.EventHandler;
end;

procedure TList<T>.Grow;
begin
  if FCount = 0 then
    SetLength(FItems, 1)
  else
    SetLength(FItems, FCount * 2);
end;

function TList<T>.IndexOf(Value: T): NativeInt;
var
  i: NativeInt;
begin
  for i := 0 to FCount - 1 do
    if FComparer.Compare(FItems[i], Value) = 0 then
      Exit(i);
  Result := -1;
end;

procedure TList<T>.Insert(Index: NativeInt; Value: T);
begin
  if (Index < 0) or (Index > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  if FCount = Length(FItems) then
    Grow();

  if Index < FCount then
  begin
    System.Move(FItems[Index], FItems[Index + 1], (FCount - Index) * SizeOf(T));
    FillChar(FItems[Index], SizeOf(FItems[Index]), 0);
  end;

  FItems[Index] := Value;
  Inc(FCount);
  Notify(Value, caAdd);
end;

procedure TList<T>.InsertRange(Index: NativeInt; Values: array of T);
var
  LItem: T;
begin
  for LItem in Values do
  begin
    Insert(Index, LItem);
    Inc(Index);
  end;
end;

procedure TList<T>.InsertRange(Index: NativeInt; Values: IEnumerable<T>);
var
  LItem: T;
begin
  for LItem in Values do
  begin
    Insert(Index, LItem);
    Inc(Index);
  end;
end;

function TList<T>.Last: T;
begin
  Result := Items[Count - 1];
end;

function TList<T>.LastIndexOf(Value: T): NativeInt;
var
  i: Integer;
begin
  for i := FCount - 1 downto 0 do
    if FComparer.Compare(FItems[i], Value) = 0 then
      Exit(i);
  Result := -1;
end;

procedure TList<T>.Move(OldIndex, NewIndex: NativeInt);
var
  LItem: T;
begin
  if (NewIndex < 0) or (NewIndex >= FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  LItem := FItems[OldIndex];
  FItems[OldIndex] := Default(T);
  if OldIndex < NewIndex then
    System.Move(FItems[OldIndex + 1], FItems[OldIndex], (NewIndex - OldIndex) * SizeOf(T))
  else
    System.Move(FItems[NewIndex], FItems[NewIndex + 1], (OldIndex - NewIndex) * SizeOf(T));

  FillChar(FItems[NewIndex], SizeOf(T), 0);
  FItems[NewIndex] := LItem;

  Notify(LItem, caMove);
end;

procedure TList<T>.Notify(Value: T; Action: TCollectionChangedAction);
begin
  FOnCollectionChanged.Invoke(Self, Value, Action);
end;

function TList<T>.Remove(Value: T): NativeInt;
begin
  Result := IndexOf(Value);
  if Result >= 0 then
    Delete(Result);
end;

procedure TList<T>.SetItem(Index: NativeInt; const Value: T);
var
  LItem: T;
begin
  if (Index < 0) or (Index >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  LItem := FItems[Index];
  FItems[Index] := Value;

  Notify(LItem, caRemove);
  Notify(Value, caAdd);
end;

function TList<T>.ToArray: TArray<T>;
begin
  Result := Copy(FItems, 0, FCount);
end;

{ TList<T>.TEnumerator }

constructor TList<T>.TEnumerator.Create(AList: TList<T>);
begin
  inherited Create();
  FList := AList;
  FIndex := -1;
end;

function TList<T>.TEnumerator.GetCurrent: T;
begin
  Result := FList[FIndex];
end;

function TList<T>.TEnumerator.MoveNext: Boolean;
begin
  if FIndex < FList.Count then
  begin
    Inc(FIndex);
    Result := FIndex < FList.Count;
  end
  else
    Result := False;
end;

{ TObjectList<T> }

constructor TObjectList<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create();
  FOwnsObjects := AOwnsObjects;
end;

procedure TObjectList<T>.Notify(Value: T; Action: TCollectionChangedAction);
begin
  inherited;
  if FOwnsObjects and (Action = caRemove) then
    Value.Free;
end;

end.
