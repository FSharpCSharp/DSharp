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

unit System.Windows.Forms;

interface

uses
  Forms,
  Controls;

type
  TForm = class(Forms.TForm)
  protected
    FRefCount: Integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create; reintroduce;
  end;

  TFrame = class(Forms.TFrame)
  public
    constructor Create; reintroduce;
  end;

implementation

uses
  Classes,
  Windows;

{ TForm }

constructor TForm.Create;
begin
  inherited Create(nil);
end;

function TForm._AddRef: Integer;
begin
  if VCLComObject = nil then
    Result := InterlockedIncrement(FRefCount)
  else
    Result := IVCLComObject(VCLComObject)._AddRef;
end;

function TForm._Release: Integer;
begin
  if VCLComObject = nil then
    Result := InterlockedDecrement(FRefCount)
  else
    Result := IVCLComObject(VCLComObject)._Release;

  if Result = 0 then
    Destroy;
end;

{ TFrame }

constructor TFrame.Create;
begin
  inherited Create(nil);
end;

end.
