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

unit DSharp.PresentationModel.ConventionManager;

interface

uses
  DSharp.PresentationModel.ElementConvention,
  Generics.Collections;

type
  ConventionManager = record
  private
    class var FConventions: TDictionary<TClass, TElementConvention>;
  public
    class constructor Create;
    class destructor Destroy;

    class function AddElementConvention<T: class>(APropertyName: string;
      AEventName: string): TElementConvention; static;
    class function GetElementConvention(
      AElementType: TClass): TElementConvention; static;
  end;

implementation

uses
  ActnList,
  ComCtrls,
  ExtCtrls,
  StdCtrls;

{ TConventionManager }

class function ConventionManager.AddElementConvention<T>(APropertyName,
  AEventName: string): TElementConvention;
begin
  Result := TElementConvention.Create(APropertyName, AEventName);
  FConventions.AddOrSetValue(T, Result);
end;

class constructor ConventionManager.Create;
begin
  FConventions := TObjectDictionary<TClass, TElementConvention>.Create([doOwnsValues]);

  AddElementConvention<TAction>('Caption', 'OnExecute');
  AddElementConvention<TButton>('Caption', 'OnClick');
  AddElementConvention<TCheckBox>('Checked', 'OnClick');
  AddElementConvention<TColorBox>('Selected', 'OnChange');
  AddElementConvention<TComboBox>('Text', 'OnChange');
  AddElementConvention<TEdit>('Text', 'OnChange');
  AddElementConvention<TLabel>('Caption', 'OnClick');
  AddElementConvention<TLabeledEdit>('Text', 'OnChange');
  AddElementConvention<TMemo>('Text', 'OnChange');
  AddElementConvention<TMonthCalendar>('Date', 'OnClick');
  AddElementConvention<TRadioButton>('Checked', 'OnClick');
  AddElementConvention<TRadioGroup>('ItemIndex', 'OnClick');
  AddElementConvention<TTrackBar>('Position', 'OnChange');
end;

class destructor ConventionManager.Destroy;
begin
  FConventions.Free();
end;

class function ConventionManager.GetElementConvention(
  AElementType: TClass): TElementConvention;
begin
  if not FConventions.TryGetValue(AElementType, Result)
    and (AElementType.ClassParent <> nil) then
  begin
    Result := GetElementConvention(AElementType.ClassParent);
  end;
end;

end.