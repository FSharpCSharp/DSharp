(*
  Copyright (c) 2012, Stefan Glienke
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

unit DSharp.Interception.SpringExtension;

interface

uses
  DSharp.Interception,
  Rtti,
  Spring.Container.Core;

type
  TInterceptionExtension = class(TInterfacedObject, IContainerExtension)
  private
    fContainerContext: IContainerContext;
    function GetContainerContext: IContainerContext;
    procedure SetContainerContext(const value: IContainerContext);

    procedure DoResolve(Sender: TObject; var instance: TValue);
  end;

implementation

{ TInterceptionExtension }

procedure TInterceptionExtension.DoResolve(Sender: TObject; var instance: TValue);
begin
  instance := TIntercept.ThroughProxyByAttributes(instance, instance.TypeInfo);
end;

function TInterceptionExtension.GetContainerContext: IContainerContext;
begin
  Result := fContainerContext;
end;

procedure TInterceptionExtension.SetContainerContext(
  const value: IContainerContext);
begin
  if Assigned(fContainerContext) then
  begin
    fContainerContext.DependencyResolver.OnResolve.Remove(DoResolve);
    fContainerContext.ServiceResolver.OnResolve.Remove(DoResolve);
  end;

  fContainerContext := value;

  if Assigned(fContainerContext) then
  begin
    fContainerContext.DependencyResolver.OnResolve.Add(DoResolve);
    fContainerContext.ServiceResolver.OnResolve.Add(DoResolve);
  end;
end;

end.
