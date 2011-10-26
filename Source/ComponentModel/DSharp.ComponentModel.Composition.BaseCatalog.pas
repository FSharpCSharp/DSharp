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

unit DSharp.ComponentModel.Composition.BaseCatalog;

interface

uses
  DSharp.ComponentModel.Composition.Primitives,
  Generics.Collections;

type
  TRegistration = record
  private
    FInfo: TExportInfo;
  public
    constructor Create(Info: TExportInfo);
    function Implements<T: IInterface>: TRegistration;
  end;

  TBaseCatalog = class(TCustomCatalog)
  private
    FCatalog: TObjectDictionary<string, TExportInfoList>;
    function GetExportList(const Name: string): TExportInfoList;
  public
    constructor Create;
    destructor Destroy; override;
    function CanResolve(const Name: string): Boolean; override;
    function GetExports(const Name: string): TExportInfoList; override;

    function RegisterComponent<T: class>: TRegistration;
  end;


implementation

uses
  DSharp.Core.Reflection,
  Rtti;

{ TRegistration }

constructor TRegistration.Create(Info: TExportInfo);
begin
  FInfo := Info;
end;

function TRegistration.Implements<T>: TRegistration;
var
  LType: TRttiType;
begin
  LType := GetRttiType(TypeInfo(T));
  FInfo.Name := LType.QualifiedName;
end;

{ TBaseCatalog }

constructor TBaseCatalog.Create;
begin
  FCatalog := TObjectDictionary<string, TExportInfoList>.Create([doOwnsValues]);
end;

destructor TBaseCatalog.Destroy;
begin
  FCatalog.Free;
  inherited;
end;

function TBaseCatalog.CanResolve(const Name: string): Boolean;
begin
  Result := FCatalog.ContainsKey(Name);
end;

function TBaseCatalog.GetExportList(const Name: string): TExportInfoList;
begin
  if not FCatalog.TryGetValue(Name, Result) then
  begin
    Result := TExportInfoList.Create;
    FCatalog.Add(Name, Result);
  end;
end;

function TBaseCatalog.GetExports(const Name: string): TExportInfoList;
begin
  if not FCatalog.TryGetValue(Name, Result) then
  begin
    raise ECompositionException.Create('No export is defined for ' + Name);
  end;
end;

function TBaseCatalog.RegisterComponent<T>: TRegistration;
var
  LType: TRttiType;
  LInfo: TExportInfo;
begin
  LType := GetRttiType(TypeInfo(T));
  LInfo := TExportInfo.Create(LType.QualifiedName, TClass(T));
  GetExportList(LInfo.Name).Add(LInfo);
  Result := TRegistration.Create(LInfo);
end;

end.