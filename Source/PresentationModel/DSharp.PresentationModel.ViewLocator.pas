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

unit DSharp.PresentationModel.ViewLocator;

interface

uses
  DSharp.PresentationModel.NameTransformer;

type
  ViewLocator = record
  private
    class var FNameTransformer: INameTransformer;
  public
    class constructor Create;
    class function FindViewType(ModelType: TClass): TClass; static;
    class function GetOrCreateViewType(ModelType: TClass): TObject; static;
  end;

implementation

uses
  DSharp.Core.Reflection,
  DSharp.PresentationModel.Composition,
  Rtti,
  StrUtils,
  SysUtils;

{ ViewLocator }

class constructor ViewLocator.Create;
begin
  FNameTransformer := TNameTransformer.Create();

  FNameTransformer.AddRule('Model$', '');
  FNameTransformer.AddRule('ViewModel$', 'View');
  FNameTransformer.AddRule('ViewModel$', 'ViewForm');
  FNameTransformer.AddRule('ViewModel$', 'ViewFrame');
  FNameTransformer.AddRule('ViewModel$', 'Form');
  FNameTransformer.AddRule('ViewModel$', 'Frame');
end;

class function ViewLocator.FindViewType(ModelType: TClass): TClass;
var
  LViewTypeName: string;
  LType: TRttiType;
begin
  Result := nil;

  for LViewTypeName in FNameTransformer.Transform(ModelType.ClassName) do
  begin
    if FindType(LViewTypeName, LType) then
    begin
      Result := LType.AsInstance.MetaclassType;
    end;
  end;
end;

class function ViewLocator.GetOrCreateViewType(ModelType: TClass): TObject;
var
  LViewClass: TClass;
begin
  LViewClass := FindViewType(ModelType);
  Result := Composition.GetInstance(LViewClass.ClassInfo, '').AsObject;

  if Result = nil then
  begin
    raise Exception.CreateFmt('Cannot find view for %0:s.', [ModelType.ClassName]);
  end;
end;

end.
