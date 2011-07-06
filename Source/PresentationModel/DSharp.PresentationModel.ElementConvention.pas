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

unit DSharp.PresentationModel.ElementConvention;

interface

uses
  Classes,
  DSharp.Bindings;

type
  TBindingType = (btProperty, btEvent);

  TElementConvention = class
  private
    FEventName: string;
    FPropertyName: string;
  public
    constructor Create(APropertyName: string; AEventName: string);

    procedure ApplyBinding(AViewModel: TObject; APropertyName: string;
      AViewElement: TComponent; ABindingType: TBindingType = btProperty);

    property EventName: string read FEventName;
    property PropertyName: string read FPropertyName;
  end;

implementation

{ TElementConvention }

procedure TElementConvention.ApplyBinding(AViewModel: TObject;
  APropertyName: string; AViewElement: TComponent; ABindingType: TBindingType);
var
  LBindingGroup: TBindingGroup;
  LBinding: TBinding;
begin
  LBindingGroup := FindBindingGroup(AViewElement);
  if not Assigned(LBindingGroup) then
  begin
    LBindingGroup := TBindingGroup.Create(AViewElement.Owner);
  end;

  LBinding := LBindingGroup.Bindings.Add();
  LBinding.Source := AViewModel;
  LBinding.SourcePropertyName := APropertyName;
  LBinding.Target := AViewElement;

  case ABindingType of
    btProperty:
    begin
      LBinding.TargetPropertyName := FPropertyName;
    end;
    btEvent:
    begin
      LBinding.TargetPropertyName := FEventName;
    end;
  end;
end;

constructor TElementConvention.Create(APropertyName, AEventName: string);
begin
  FPropertyName := APropertyName;
  FEventName := AEventName;
end;

end.