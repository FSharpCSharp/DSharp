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

unit Collections.Generics;

interface

uses
  Generics.Collections,
  Rtti;

type
  TEnumerableEx<T> = class(TEnumerable<T>, IEnumerable<T>, IEnumerable)
  private
    FRefCount: Integer;
    FEnumerable: TEnumerable<T>;
    FList: TList<T>;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetEnumerator: IEnumerator;
    function GetEnumeratorGeneric: IEnumerator<T>; virtual;
    function IEnumerable<T>.GetEnumerator = GetEnumeratorGeneric;
  protected
    function DoGetEnumerator: TEnumerator<T>; override;
  public
    constructor Create(AArray: array of T); overload;
    constructor Create(AEnumerable: TEnumerable<T>); overload;
    destructor Destroy; override;
  end;

  TEnumeratorEx<T> = class(TEnumerator<T>, IEnumerator<T>, IEnumerator)
  private
    FRefCount: Integer;
    FItems: TObjectList<TObject>;
    FEnumerator: TEnumerator<T>;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetCurrent: TObject;
    function GetCurrentGeneric: T;
    function IEnumerator<T>.GetCurrent = GetCurrentGeneric;
  protected
    function DoGetCurrent: T; override;
    function DoMoveNext: Boolean; override;
  public
    constructor Create(AEnumerator: TEnumerator<T>);
    destructor Destroy; override;
    function MoveNext: Boolean;
    procedure Reset; virtual;
  end;

implementation

uses
  System.Generics,
  Windows;

{ TEnumerableEx<T> }

constructor TEnumerableEx<T>.Create(AArray: array of T);
begin
  FList := TList<T>.Create();
  FList.AddRange(AArray);
  FEnumerable := FList;
end;

constructor TEnumerableEx<T>.Create(AEnumerable: TEnumerable<T>);
begin
  FEnumerable := AEnumerable;
end;

destructor TEnumerableEx<T>.Destroy;
begin
  FList.Free();
  inherited;
end;

function TEnumerableEx<T>.DoGetEnumerator: TEnumerator<T>;
begin
  inherited;
end;

function TEnumerableEx<T>.GetEnumerator: IEnumerator;
begin
  if Assigned(FEnumerable) then
  begin
    Result := TEnumeratorEx<T>.Create(FEnumerable.GetEnumerator());
  end
  else
  begin
    Result := TEnumeratorEx<TObject>(DoGetEnumerator());
  end;
end;

function TEnumerableEx<T>.GetEnumeratorGeneric: IEnumerator<T>;
begin
  Result := TEnumeratorEx<T>(DoGetEnumerator());
end;

function TEnumerableEx<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
  begin
    Result := 0;
  end
  else
  begin
    Result := E_NOINTERFACE;
  end;
end;

function TEnumerableEx<T>._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TEnumerableEx<T>._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
  begin
    Destroy();
  end;
end;

{ TEnumeratorEx<T> }

function TEnumeratorEx<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
  begin
    Result := 0;
  end
  else
  begin
    Result := E_NOINTERFACE;
  end;
end;

function TEnumeratorEx<T>._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TEnumeratorEx<T>._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
  begin
    Destroy();
  end;
end;

constructor TEnumeratorEx<T>.Create(AEnumerator: TEnumerator<T>);
begin
  FItems := TObjectList<TObject>.Create(True);
  FEnumerator := AEnumerator;
end;

destructor TEnumeratorEx<T>.Destroy;
begin
  FItems.Free();
  FEnumerator.Free();
  inherited;
end;

function TEnumeratorEx<T>.DoGetCurrent: T;
begin
  if Assigned(FEnumerator) then
  begin
    Result := FEnumerator.Current;
  end
  else
  begin
    Result := Default(T);
  end;
end;

function TEnumeratorEx<T>.DoMoveNext: Boolean;
begin
  if Assigned(FEnumerator) then
  begin
    Result := FEnumerator.MoveNext;
  end
  else
  begin
    Result := False;
  end;
end;

function TEnumeratorEx<T>.GetCurrent: TObject;
begin
  if TRttiContext.Create.GetType(TypeInfo(T)).IsInstance then
  begin
    Result := TObject(DoGetCurrent());
  end
  else
  begin
    Result := TBox<T>.Create(DoGetCurrent());
    FItems.Add(Result);
  end;
end;

function TEnumeratorEx<T>.GetCurrentGeneric: T;
begin
  Result := DoGetCurrent();
end;

function TEnumeratorEx<T>.MoveNext: Boolean;
begin
  Result := DoMoveNext();
end;

procedure TEnumeratorEx<T>.Reset;
begin

end;

end.
