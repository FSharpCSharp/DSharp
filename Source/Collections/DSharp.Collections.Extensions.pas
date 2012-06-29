(*
  Copyright (c) 2011-2012, Stefan Glienke
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

unit DSharp.Collections.Extensions;

interface

uses
  DSharp.Collections,
  Generics.Collections,
  Generics.Defaults,
  SysUtils;

type
  Enumerable<T> = record
  private
    Enumerable: IEnumerable<T>;

    type
      TEqualityComparer = class(TDelegatedEqualityComparer<T>)
      public
        constructor Create(const AEquals: TEqualityComparison<T>);
        function GetHashCode(const Value: T): Integer; overload; override;
      end;

      THashSet = class(TDictionary<T, Integer>)
      public
        function Add(const Value: T): Boolean;
        function Contains(const Value: T): Boolean;
      end;
  public
    function All(predicate: TPredicate<T>): Boolean;

    function Any: Boolean; overload;
    function Any(predicate: TPredicate<T>): Boolean; overload;

    function Cast<TResult>: Enumerable<TResult>;

    function Concat(const second: IEnumerable<T>): Enumerable<T>;

    function Contains(const value: T): Boolean; overload;
    function Contains(const value: T; comparer: IEqualityComparer<T>): Boolean; overload;
    function Contains(const value: T; comparison: TEqualityComparison<T>): Boolean; overload;

    function Count: NativeInt; overload;
    function Count(predicate: TPredicate<T>): NativeInt; overload;

    function DefaultIfEmpty: Enumerable<T>; overload;
    function DefaultIfEmpty(const defaultValue: T): Enumerable<T>; overload;

    function Distinct: Enumerable<T>; overload;
    function Distinct(comparer: IEqualityComparer<T>): Enumerable<T>; overload;
    function Distinct(comparison: TEqualityComparison<T>): Enumerable<T>; overload;

    function ElementAt(index: NativeInt): T;
    function ElementAtOrDefault(index: NativeInt): T;

    function Except_(second: IEnumerable<T>): Enumerable<T>; overload;
    function Except_(second: IEnumerable<T>; comparer: IEqualityComparer<T>): Enumerable<T>; overload;
    function Except_(second: IEnumerable<T>; comparison: TEqualityComparison<T>): Enumerable<T>; overload;

    function First: T; overload;
    function First(predicate: TPredicate<T>): T; overload;
    function FirstOrDefault: T; overload;
    function FirstOrDefault(predicate: TPredicate<T>): T; overload;

    function Intersect(second: IEnumerable<T>): Enumerable<T>; overload;
    function Intersect(second: IEnumerable<T>; comparer: IEqualityComparer<T>): Enumerable<T>; overload;
    function Intersect(second: IEnumerable<T>; comparison: TEqualityComparison<T>): Enumerable<T>; overload;

    function Last: T; overload;
    function Last(predicate: TPredicate<T>): T; overload;
    function LastOrDefault: T; overload;
    function LastOrDefault(predicate: TPredicate<T>): T; overload;

    function Max: T; overload;
    function Max(comparer: IComparer<T>): T; overload;
    function Max(comparison: TComparison<T>): T; overload;
    function Min: T; overload;
    function Min(comparer: IComparer<T>): T; overload;
    function Min(comparison: TComparison<T>): T; overload;

    function Reverse: Enumerable<T>;

    function Select<TResult>(selector: TFunc<T, TResult>): Enumerable<TResult>;

    function Single: T; overload;
    function Single(predicate: TPredicate<T>): T; overload;
    function SingleOrDefault: T; overload;
    function SingleOrDefault(predicate: TPredicate<T>): T; overload;

    function Skip(count: NativeInt): Enumerable<T>;
    function SkipWhile(predicate: TPredicate<T>): Enumerable<T>; overload;
    function SkipWhile(predicate: TFunc<T, NativeInt, Boolean>): Enumerable<T>; overload;

    function Take(count: NativeInt): Enumerable<T>;
    function TakeWhile(predicate: TPredicate<T>): Enumerable<T>; overload;
    function TakeWhile(predicate: TFunc<T, NativeInt, Boolean>): Enumerable<T>; overload;

    function Union(second: IEnumerable<T>): Enumerable<T>; overload;
    function Union(second: IEnumerable<T>; comparer: IEqualityComparer<T>): Enumerable<T>; overload;
    function Union(second: IEnumerable<T>; comparison: TEqualityComparison<T>): Enumerable<T>; overload;

    function Where(predicate: TPredicate<T>): Enumerable<T>; overload;
    function Where(predicate: TFunc<T, NativeInt, Boolean>): Enumerable<T>; overload;

    function GetEnumerator: IEnumerator<T>;

    class operator Implicit(const Value: Enumerable<T>): IEnumerable<T>;
    class operator Implicit(const Value: IEnumerable<T>): Enumerable<T>;
  end;

  EInvalidOperation = class(Exception);
  EArgumentOutOfRange = class(Exception);

implementation

uses
  DSharp.Collections.Yield,
  Rtti;

{ Enumerable<T> }

function Enumerable<T>.All(predicate: TPredicate<T>): Boolean;
var
  item: T;
begin
  for item in Enumerable do
    if not predicate(item) then
      Exit(False);
  Result := True;
end;

function Enumerable<T>.Any: Boolean;
var
  item: T;
begin
  for item in Enumerable do
    Exit(True);
  Result := False;
end;

function Enumerable<T>.Any(predicate: TPredicate<T>): Boolean;
var
  item: T;
begin
  for item in Enumerable do
    if predicate(item) then
      Exit(True);
  Result := False;
end;

function Enumerable<T>.Cast<TResult>: Enumerable<TResult>;
var
  this: IEnumerable<T>;
begin
  this := Self;
  Result := TYieldEnumerable<TResult>.Create(
    procedure
    var
      item: T;
      value: TValue;
      return: Yield<TResult>;
    begin
      for item in this do
      begin
        value := TValue.From<T>(item);
        return := value.AsType<TResult>;
      end;
    end);
end;

function Enumerable<T>.Concat(const second: IEnumerable<T>): Enumerable<T>;
var
  first: IEnumerable<T>;
begin
  first := Self;
  Result := TYieldEnumerable<T>.Create(
    procedure
    var
      return: Yield<T>;
    begin
      for return in first do;
      for return in second do;
    end);
end;

function Enumerable<T>.Contains(const value: T): Boolean;
begin
  Result := Contains(value, TEqualityComparer.Default());
end;

function Enumerable<T>.Contains(const value: T; comparer: IEqualityComparer<T>): Boolean;
var
  item: T;
begin
  for item in Enumerable do
    if comparer.Equals(item, value) then
      Exit(True);
  Result := False;
end;

function Enumerable<T>.Contains(const value: T;
  comparison: TEqualityComparison<T>): Boolean;
begin
  Result := Contains(value, TEqualityComparer.Create(comparison));
end;

function Enumerable<T>.Count: NativeInt;
var
  item: T;
begin
  Result := 0;
  for item in Enumerable do
    Inc(Result);
end;

function Enumerable<T>.Count(predicate: TPredicate<T>): NativeInt;
var
  item: T;
begin
  Result := 0;
  for item in Enumerable do
    if predicate(item) then
      Inc(Result);
end;

function Enumerable<T>.DefaultIfEmpty: Enumerable<T>;
begin
  Result := DefaultIfEmpty(Default(T));
end;

function Enumerable<T>.DefaultIfEmpty(const defaultValue: T): Enumerable<T>;
var
  this: IEnumerable<T>;
begin
  this := Self;
  Result := TYieldEnumerable<T>.Create(
    procedure
    var
      isEmpty: Boolean;
      return: Yield<T>;
    begin
      isEmpty := True;
      for return in this do
        if isEmpty then
          isEmpty := False;
      if isEmpty then
        return := defaultValue;
    end);
end;

function Enumerable<T>.Distinct: Enumerable<T>;
begin
  Result := Distinct(TEqualityComparer.Default());
end;

function Enumerable<T>.Distinct(comparer: IEqualityComparer<T>): Enumerable<T>;
var
  this: IEnumerable<T>;
begin
  this := Self;
  Result := TYieldEnumerable<T>.Create(
    procedure
    var
      hashSet: THashSet;
      item: T;
      return: Yield<T>;
    begin
      hashSet := THashSet.Create(comparer);
      for item in this do
        if hashSet.Add(item) then
          return := item;
    end);
end;

function Enumerable<T>.Distinct(
  comparison: TEqualityComparison<T>): Enumerable<T>;
begin
  Result := Distinct(TEqualityComparer.Create(comparison));
end;

function Enumerable<T>.ElementAt(index: NativeInt): T;
begin
  for Result in Enumerable do
    if index = 0 then
      Exit
    else
      Dec(index);
  raise EArgumentOutOfRange.Create('');
end;

function Enumerable<T>.ElementAtOrDefault(index: NativeInt): T;
begin
  for Result in Enumerable do
    if index = 0 then
      Exit
    else
      Dec(index);
  Result := Default(T);
end;

function Enumerable<T>.Except_(second: IEnumerable<T>): Enumerable<T>;
begin
  Result := Except_(second, TEqualityComparer.Default());
end;

function Enumerable<T>.Except_(second: IEnumerable<T>;
  comparer: IEqualityComparer<T>): Enumerable<T>;
var
  first: IEnumerable<T>;
begin
  first := Self;
  Result := TYieldEnumerable<T>.Create(
    procedure
    var
      hashSet: THashSet;
      item: T;
      return: Yield<T>;
    begin
      hashSet := THashSet.Create(comparer);
      for item in second do
        hashSet.Add(item);
      for item in first do
        if not hashSet.Contains(item) then
          return := item;
    end);
end;

function Enumerable<T>.Except_(second: IEnumerable<T>;
  comparison: TEqualityComparison<T>): Enumerable<T>;
begin
  Result := Except_(second, TEqualityComparer.Create(comparison));
end;

function Enumerable<T>.First: T;
begin
  for Result in Enumerable do
    Exit;
  raise EInvalidOperation.Create('');
end;

function Enumerable<T>.First(predicate: TPredicate<T>): T;
begin
  for Result in Enumerable do
    if predicate(Result) then
      Exit;
  raise EInvalidOperation.Create('');
end;

function Enumerable<T>.FirstOrDefault: T;
begin
  for Result in Enumerable do
    Exit;
  Result := Default(T);
end;

function Enumerable<T>.FirstOrDefault(predicate: TPredicate<T>): T;
begin
  for Result in Enumerable do
    if predicate(Result) then
      Exit;
  Result := Default(T);
end;

function Enumerable<T>.Intersect(second: IEnumerable<T>): Enumerable<T>;
begin
  Result := Intersect(second, TEqualityComparer.Default());
end;

function Enumerable<T>.Intersect(second: IEnumerable<T>;
  comparer: IEqualityComparer<T>): Enumerable<T>;
var
  first: IEnumerable<T>;
begin
  first := Self;
  Result := TYieldEnumerable<T>.Create(
    procedure
    var
      hashSet: THashSet;
      item: T;
      return: Yield<T>;
    begin
      hashSet := THashSet.Create(comparer);
      for item in first do
        hashSet.Add(item);
      for item in second do
        if hashSet.Contains(item) then
          return := item;
    end);
end;

function Enumerable<T>.Intersect(second: IEnumerable<T>;
  comparison: TEqualityComparison<T>): Enumerable<T>;
begin
  Result := Intersect(second, TEqualityComparer.Create(comparison));
end;

function Enumerable<T>.Last: T;
var
  isEmpty: Boolean;
begin
  isEmpty := True;
  for Result in Enumerable do
    isEmpty := False;
  if isEmpty then
    raise EInvalidOperation.Create('');
end;

function Enumerable<T>.Last(predicate: TPredicate<T>): T;
var
  isEmpty: Boolean;
  item: T;
begin
  isEmpty := True;
  for item in Enumerable do
    if predicate(item) then
    begin
      Result := item;
      isEmpty := False;
    end;
  if isEmpty then
    raise EInvalidOperation.Create('');
end;

function Enumerable<T>.LastOrDefault: T;
var
  isEmpty: Boolean;
begin
  isEmpty := True;
  for Result in Enumerable do
    isEmpty := False;
  if isEmpty then
    Result := Default(T);
end;

function Enumerable<T>.LastOrDefault(predicate: TPredicate<T>): T;
var
  isEmpty: Boolean;
  item: T;
begin
  isEmpty := True;
  for item in Enumerable do
    if predicate(item) then
    begin
      Result := item;
      isEmpty := False;
    end;
  if isEmpty then
    Result := Default(T);
end;

function Enumerable<T>.Max: T;
begin
  Result := Max(TComparer<T>.Default());
end;

function Enumerable<T>.Max(comparer: IComparer<T>): T;
var
  item: T;
begin
  Result := Default(T);
  for item in Self do
    if comparer.Compare(item, Result) > 0 then
      Result := item;
end;

function Enumerable<T>.Max(comparison: TComparison<T>): T;
begin
  Result := Max(TComparer<T>.Construct(comparison));
end;

function Enumerable<T>.Min: T;
begin
  Result := Min(TComparer<T>.Default());
end;

function Enumerable<T>.Min(comparer: IComparer<T>): T;
var
  item: T;
begin
  Result := Default(T);
  for item in Self do
    if comparer.Compare(item, Result) < 0 then
      Result := item;
end;

function Enumerable<T>.Min(comparison: TComparison<T>): T;
begin
  Result := Min(TComparer<T>.Construct(comparison));
end;

function Enumerable<T>.Reverse: Enumerable<T>;
var
  this: IEnumerable<T>;
begin
  this := Self;
  Result := TYieldEnumerable<T>.Create(
    procedure
    var
      items: TArray<T>;
      i: Integer;
      return: Yield<T>;
    begin
      items := this.ToArray;
      for i := High(items) downto Low(items) do
        return := items[i];
    end);
end;

function Enumerable<T>.Single: T;
var
  isSingle: Boolean;
begin
  isSingle := False;
  for Result in Enumerable do
  begin
    isSingle := not isSingle;
    if not isSingle then
      Break;
  end;
  if not isSingle then
    raise EInvalidOperation.Create('');
end;

function Enumerable<T>.Select<TResult>(
  selector: TFunc<T, TResult>): Enumerable<TResult>;
var
  this: IEnumerable<T>;
begin
  this := Self;
  Result := TYieldEnumerable<TResult>.Create(
    procedure
    var
      item: T;
      return: Yield<TResult>;
    begin
      for item in this do
      begin
        return := selector(item);
      end;
    end);
end;

function Enumerable<T>.Single(predicate: TPredicate<T>): T;
var
  isSingle: Boolean;
begin
  isSingle := False;
  for Result in Enumerable do
  begin
    if predicate(Result) then
    begin
      isSingle := not isSingle;
      if not isSingle then
        Break;
    end;
  end;
  if not isSingle then
    raise EInvalidOperation.Create('');
end;

function Enumerable<T>.SingleOrDefault: T;
var
  isSingle: Boolean;
begin
  isSingle := False;
  for Result in Enumerable do
  begin
    isSingle := not isSingle;
    if not isSingle then
      Break;
  end;
  if not isSingle then
    Result := Default(T);
end;

function Enumerable<T>.SingleOrDefault(predicate: TPredicate<T>): T;
var
  isSingle: Boolean;
begin
  isSingle := False;
  for Result in Enumerable do
  begin
    if predicate(Result) then
    begin
      isSingle := not isSingle;
      if not isSingle then
        Break;
    end;
  end;
  if not isSingle then
    Result := Default(T);
end;

function Enumerable<T>.Skip(count: NativeInt): Enumerable<T>;
var
  this: IEnumerable<T>;
begin
  this := Self;
  Result := TYieldEnumerable<T>.Create(
    procedure
    var
      item: T;
      return: Yield<T>;
    begin
      for item in this do
        if count > 0 then
          Dec(count)
        else
          return := item;
    end);
end;

function Enumerable<T>.SkipWhile(predicate: TPredicate<T>): Enumerable<T>;
var
  this: IEnumerable<T>;
begin
  this := Self;
  Result := TYieldEnumerable<T>.Create(
    procedure
    var
      skipped: Boolean;
      item: T;
      return: Yield<T>;
    begin
      skipped := False;
      for item in this do
      begin
        if not skipped then
        begin
          if not predicate(item) then
            skipped := True
          else
            Continue;
        end;
        return := item;
      end;
    end);
end;

function Enumerable<T>.SkipWhile(
  predicate: TFunc<T, NativeInt, Boolean>): Enumerable<T>;
var
  this: IEnumerable<T>;
begin
  this := Self;
  Result := TYieldEnumerable<T>.Create(
    procedure
    var
      index: NativeInt;
      skipped: Boolean;
      item: T;
      return: Yield<T>;
    begin
      index := 0;
      skipped := False;
      for item in this do
      begin
        if not skipped then
        begin
          if predicate(item, index) then
          begin
            Inc(index);
            Continue;
          end
          else
            skipped := True;
        end;
        return := item;
      end;
    end);
end;

function Enumerable<T>.Take(count: NativeInt): Enumerable<T>;
var
  this: IEnumerable<T>;
begin
  this := Self;
  Result := TYieldEnumerable<T>.Create(
    procedure
    var
      item: T;
      return: Yield<T>;
    begin
      for item in this do
      begin
        if count <= 0 then
          Break;
        return := item;
        Dec(count);
      end;
    end);
end;

function Enumerable<T>.TakeWhile(predicate: TPredicate<T>): Enumerable<T>;
var
  this: IEnumerable<T>;
begin
  this := Self;
  Result := TYieldEnumerable<T>.Create(
    procedure
    var
      item: T;
      return: Yield<T>;
    begin
      for item in this do
      begin
        if not predicate(item) then
          Break;
        return := item;
      end;
    end);
end;

function Enumerable<T>.TakeWhile(
  predicate: TFunc<T, NativeInt, Boolean>): Enumerable<T>;
var
  this: IEnumerable<T>;
begin
  this := Self;
  Result := TYieldEnumerable<T>.Create(
    procedure
    var
      index: NativeInt;
      item: T;
      return: Yield<T>;
    begin
      index := 0;
      for item in this do
      begin
        if not predicate(item, index) then
          Break;
        return := item;
        Inc(index);
      end;
    end);
end;

function Enumerable<T>.Union(second: IEnumerable<T>): Enumerable<T>;
begin
  Result := Union(second, TEqualityComparer.Default);
end;

function Enumerable<T>.Union(second: IEnumerable<T>;
  comparer: IEqualityComparer<T>): Enumerable<T>;
var
  first: IEnumerable<T>;
begin
  first := Self;
  Result := TYieldEnumerable<T>.Create(
    procedure
    var
      hashSet: THashSet;
      item: T;
      return: Yield<T>;
    begin
      hashSet := THashSet.Create(comparer);
      for item in first do
        if hashSet.Add(item) then
          return := item;
      for item in second do
        if hashSet.Add(item) then
          return := item;
    end);
end;

function Enumerable<T>.Union(second: IEnumerable<T>;
  comparison: TEqualityComparison<T>): Enumerable<T>;
begin
  Result := Union(second, TEqualityComparer.Create(comparison));
end;

function Enumerable<T>.Where(predicate: TPredicate<T>): Enumerable<T>;
var
  this: IEnumerable<T>;
begin
  this := Self;
  Result := TYieldEnumerable<T>.Create(
    procedure
    var
      item: T;
      return: Yield<T>;
    begin
      for item in this do
        if predicate(item) then
          return := item;
    end);
end;

function Enumerable<T>.Where(
  predicate: TFunc<T, NativeInt, Boolean>): Enumerable<T>;
var
  this: IEnumerable<T>;
begin
  this := Self;
  Result := TYieldEnumerable<T>.Create(
    procedure
    var
      index: NativeInt;
      item: T;
      return: Yield<T>;
    begin
      index := 0;
      for item in this do
      begin
        if predicate(item, index) then
          return := item;
        Inc(index);
      end;
    end);
end;

function Enumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := Enumerable.GetEnumerator();
end;

class operator Enumerable<T>.Implicit(const Value: Enumerable<T>): IEnumerable<T>;
begin
  Result := Value.Enumerable;
end;

class operator Enumerable<T>.Implicit(const Value: IEnumerable<T>): Enumerable<T>;
begin
  Result.Enumerable := Value;
end;

{ Enumerable<T>.TEqualityComparer }

constructor Enumerable<T>.TEqualityComparer.Create(
  const AEquals: TEqualityComparison<T>);
begin
  inherited Create(AEquals, nil);
end;

function Enumerable<T>.TEqualityComparer.GetHashCode(const Value: T): Integer;
begin
  Result := 0;
end;

{ Enumerable<T>.THashSet }

function Enumerable<T>.THashSet.Add(const Value: T): Boolean;
begin
  Result := not ContainsKey(Value);
  if Result then
    inherited Add(Value, 0);
end;

function Enumerable<T>.THashSet.Contains(const Value: T): Boolean;
begin
  Result := ContainsKey(Value);
end;

end.
