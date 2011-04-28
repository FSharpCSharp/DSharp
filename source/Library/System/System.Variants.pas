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

unit System.Variants;

interface

function AsVariant(AObject: TObject): Variant;

implementation

uses
  Rtti,
  System.Expressions,
  Variants;

type
  TBoxedObjectVarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    FObject: TObject;
    Reserved4: LongWord;
  end;

  TBoxedObjectVariantType = class(TInvokeableVariantType)
  private
    class var FContext: TRttiContext;
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
  end;

var
  BoxedObject: TBoxedObjectVariantType;

function AsVariant(AObject: TObject): Variant;
begin
  VarClear(Result);

  with TBoxedObjectVarData(Result) do
  begin
    VType := BoxedObject.VarType;
    FObject := AObject;
  end;
end;

{ TBoxedObjectVariantType }

procedure TBoxedObjectVariantType.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  TBoxedObjectVarData(V).FObject := nil;
end;

procedure TBoxedObjectVariantType.Copy(var Dest: TVarData;
  const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
  begin
    VarDataCopyNoInd(Dest, Source);
  end
  else
  begin
    with TBoxedObjectVarData(Dest) do
    begin
      VType := VarType;
      FObject := TBoxedObjectVarData(Source).FObject;
    end;
  end;
end;

function TBoxedObjectVariantType.GetProperty(var Dest: TVarData;
  const V: TVarData; const Name: string): Boolean;
var
  LProperty: TRttiProperty;
  LExpression: IExpression;
begin
  with TBoxedObjectVarData(V) do
  begin
    LProperty := FContext.GetType(FObject.ClassInfo).GetProperty(Name);
    Result := Assigned(LProperty);

    if Result then
    begin
      Dest.VType := varByRef;
      LExpression := TEntityExpression.Create(LProperty.Name);
      LExpression._AddRef;
      Dest.VPointer := Pointer(LExpression);
    end
    else
    begin
      Clear(Dest);
    end;
  end;
end;

initialization
  BoxedObject := TBoxedObjectVariantType.Create();

finalization
  BoxedObject.Free();;

end.
