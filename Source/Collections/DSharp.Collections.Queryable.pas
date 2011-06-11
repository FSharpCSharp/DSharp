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

unit DSharp.Collections.Queryable;

interface

uses
  DSharp.Collections.Generics,
  DSharp.Core.Expressions,
  DSharp.Linq.QueryProvider;

type
  IQueryable<T> = interface(IEnumerable<T>)
    function GetExpression: IExpression;
    function GetProvider: IQueryProvider;
    function Where(predicate: Variant): IQueryable<T>;
    property Expression: IExpression read GetExpression;
    property Provider: IQueryProvider read GetProvider;
  end;

  TQueryable<T> = class(TEnumerableEx<T>, IQueryable<T>)
  private
    FExpression: IExpression;
    FProvider: IQueryProvider;

    function GetExpression: IExpression;
    function GetProvider: IQueryProvider;
  public
    constructor Create(Provider: IQueryProvider);

    function GetEnumerator: IEnumerator<T>;

    function Where(predicate: Variant): IQueryable<T>;

    property Expression: IExpression read GetExpression;
    property Provider: IQueryProvider read GetProvider;
  end;

implementation

uses
  System.Lambda;

{ TQueryable<T> }

constructor TQueryable<T>.Create(Provider: IQueryProvider);
begin
  FProvider := Provider;
end;

function TQueryable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := IEnumerable<T>(Provider.Execute(Expression) as TEnumerableEx<T>).GetEnumerator();
end;

function TQueryable<T>.GetExpression: IExpression;
begin
  Result := FExpression;
end;

function TQueryable<T>.GetProvider: IQueryProvider;
begin
  Result := FProvider;
end;

function TQueryable<T>.Where(predicate: Variant): IQueryable<T>;
begin
  // Keep it simple for now and only support one where call
  FExpression := TLambda.InitExpression(predicate);

  Result := Self;
end;

end.
