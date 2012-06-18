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
//  DSharp.Collections.Yield,
  Generics.Defaults,
  SysUtils;

type
  Enumerable<T> = record
  private
    Enumerable: IEnumerable<T>;
  public
    function All(predicate: TPredicate<T>): Boolean;

    function Any: Boolean; overload;
    function Any(predicate: TPredicate<T>): Boolean; overload;

    //    function Cast<TResult>: IEnumerable<TResult>;

    function Contains(const value: T): Boolean; overload;
    function Contains(const value: T; comparer: IEqualityComparer<T>): Boolean; overload;

    function Count: NativeInt; overload;
    function Count(predicate: TPredicate<T>): NativeInt; overload;

    function ElementAt(index: NativeInt): T;
    function ElementAtOrDefault(index: NativeInt): T;

    function First: T; overload;
    function First(predicate: TPredicate<T>): T; overload;
    function FirstOrDefault: T; overload;
    function FirstOrDefault(predicate: TPredicate<T>): T; overload;

    function Last: T; overload;
    function Last(predicate: TPredicate<T>): T; overload;
    function LastOrDefault: T; overload;
    function LastOrDefault(predicate: TPredicate<T>): T; overload;

//    function Max: T;
//    function Min: T;

    function Single: T; overload;
    function Single(predicate: TPredicate<T>): T; overload;
    function SingleOrDefault: T; overload;
    function SingleOrDefault(predicate: TPredicate<T>): T; overload;

    function GetEnumerator: IEnumerator<T>;

    class operator Implicit(const Value: Enumerable<T>): IEnumerable<T>;
    class operator Implicit(const Value: IEnumerable<T>): Enumerable<T>;
  end;

  EInvalidOperation = class(Exception);
  EArgumentOutOfRange = class(Exception);

implementation

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

function Enumerable<T>.Contains(const value: T): Boolean;
begin
  Result := Contains(value, TEqualityComparer<T>.Default);
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

end.
