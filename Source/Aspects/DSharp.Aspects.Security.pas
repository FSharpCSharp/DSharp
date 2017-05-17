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

unit DSharp.Aspects.Security;

interface

uses
  DSharp.Aspects,
  Rtti;

type
  TSecurityAspect = class(TAspect)
  public
//    class procedure DoAfter(Instance: TObject; Method: TRttiMethod;
//      const Args: TArray<TValue>; var Result: TValue); override;
    class procedure DoBefore(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out DoInvoke: Boolean;
      out Result: TValue); override;
//    class procedure DoException(Instance: TObject; Method: TRttiMethod;
//      const Args: TArray<TValue>; out RaiseException: Boolean;
//      Exception: Exception; out Result: TValue); override;
  end;

  SecurityAttribute = class(AspectAttribute)
  private
    FPassword: string;
  public
    constructor Create(const Password: string);
    property Password: string read FPassword;
  end;

implementation

uses
  DSharp.Core.Reflection,
  DSharp.PresentationModel.Composition,
  DSharp.PresentationModel.WindowManager;

{ TSecurityAspect }

class procedure TSecurityAspect.DoBefore(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);
var
  LAttribute: SecurityAttribute;
begin
  if Method.TryGetCustomAttribute<SecurityAttribute>(LAttribute)
    or Method.Parent.TryGetCustomAttribute<SecurityAttribute>(LAttribute) then
  begin
    DoInvoke := Composition.Get<IWindowManager>.InputBox('Password', 'Enter password', '', True) =
      LAttribute.Password;
  end;

  if not DoInvoke then
    Result := TValue.Empty;
end;

{ SecurityAttribute }

constructor SecurityAttribute.Create(const Password: string);
begin
  inherited Create(TSecurityAspect);
  // we just passing in the password for demo purpose here
  // in a real world scenario we would delegate the security check
  FPassword := Password;
end;

end.
