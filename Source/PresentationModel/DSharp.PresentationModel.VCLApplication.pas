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

unit DSharp.PresentationModel.VCLApplication;

interface

uses
{$IF COMPILERVERSION > 21}
  DSharp.Aspects.Weaver,
{$IFEND}
  DSharp.ComponentModel.Composition.SpringContainer,
  DSharp.PresentationModel.Bootstrapper,
  DSharp.PresentationModel.VCLWindowManager,
  Forms;

type
  TApplicationVCLHelper = class helper for TApplication
    procedure Start<T>;
  end;

implementation

uses
  Classes;

{ TApplicationVCLHelper }

procedure TApplicationVCLHelper.Start<T>;
var
  LContainer: TSpringContainer;
  LBootstrapper: TBootstrapper<T>;
begin
  LContainer := TSpringContainer.Create();
{$IF COMPILERVERSION > 21}
  LContainer.AspectWeaver := TAspectWeaver.Create();
{$IFEND}
{$IF COMPILERVERSION = 21}
  {$MESSAGE HINT 'Apply Spring_Delphi2010_Bugfix.patch to Spring\Source\Core\Container'}
  LContainer.RegisterType<TComponent>.Implements<TComponent>.DelegateTo(
    function: TComponent
    begin
      Result := nil;
    end);
{$IFEND}

  try
    LContainer.ImportRtti();
    LBootstrapper := TBootstrapper<T>.Create(LContainer);
    try
      LBootstrapper.StartRuntime();
    finally
      LBootstrapper.Free();
    end;
  finally
    LContainer.Free();
  end;
end;

end.
