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

unit DSharp.PresentationModel.Bootstrapper;

interface

uses
  DSharp.ComponentModel.Composition,
  DSharp.Core.Lazy,
  DSharp.PresentationModel.WindowManager,
  Rtti,
  TypInfo;

type
  TBootstrapper<T> = class
  private
    FServiceLocator: IServiceLocator;
    FViewModel: Lazy<T>;
    FWindowManager: IWindowManager;
    function GetInstance(TypeInfo: PTypeInfo; const Name: string): TValue;
  public
    constructor Create(ServiceLocator: IServiceLocator);
    procedure StartRuntime;
  end;

implementation

uses
  DSharp.PresentationModel.Composition,
  SysUtils;

{ TBootstrapper<T> }

constructor TBootstrapper<T>.Create(ServiceLocator: IServiceLocator);
begin
  if not (PTypeInfo(TypeInfo(T)).Kind in [tkClass, tkInterface]) then
  begin
    raise Exception.CreateFmt('"%s" is not a valid class or interface type', [PTypeInfo(TypeInfo(T)).Name]);
  end;

  FServiceLocator := ServiceLocator;

  Composition.GetInstance := GetInstance;
end;

function TBootstrapper<T>.GetInstance(TypeInfo: PTypeInfo; const Name: string): TValue;
begin
  Result := FServiceLocator.Resolve(TypeInfo, Name);
end;

procedure TBootstrapper<T>.StartRuntime;
begin
  try
    FWindowManager := Composition.Get<IWindowManager>;

    FViewModel := Lazy<T>(
      function: T
      begin
        Result := Composition.Get<T>;
      end);

    if PTypeInfo(TypeInfo(T)).Kind = tkClass then
    begin
      FWindowManager.ShowWindow(TValue.From<T>(FViewModel.Value).AsObject);
    end
    else
    begin
      FWindowManager.ShowWindow(TValue.From<T>(FViewModel.Value).AsInterface);
    end;
  except
  end;
end;

end.
