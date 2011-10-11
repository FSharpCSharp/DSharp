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

unit DSharp.Collections.ObservableCollection;

interface

uses
  DSharp.Collections,
  DSharp.Bindings.Collections,
  DSharp.Bindings.Notifications,
  DSharp.Core.Events;

type
  TObservableCollection<T: class> = class(TObjectList<T>,
    INotifyCollectionChanged, INotifyPropertyChanged)
  private
    FOnCollectionChanged: TEvent<TCollectionChangedEvent>;
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    function GetOnCollectionChanged: TEvent<TCollectionChangedEvent>;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
  protected
    procedure DoCollectionChanged(Value: T; Action: TCollectionChangedAction);
    procedure DoItemPropertyChanged(ASender: TObject; APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
    procedure Notify(Value: T; Action: TCollectionChangedAction); override;
  public
    property OnCollectionChanged: TEvent<TCollectionChangedEvent> read GetOnCollectionChanged;
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  end;

implementation

uses
  DSharp.Core.Utils;

{ TObservableCollection<T> }

procedure TObservableCollection<T>.DoCollectionChanged(Value: T;
  Action: TCollectionChangedAction);
begin
  FOnCollectionChanged.Invoke(Self, Value, Action);
end;

procedure TObservableCollection<T>.DoItemPropertyChanged(ASender: TObject;
  APropertyName: string; AUpdateTrigger: TUpdateTrigger);
begin
  DoCollectionChanged(T(ASender), caReplace);
end;

procedure TObservableCollection<T>.DoPropertyChanged(
  const APropertyName: string; AUpdateTrigger: TUpdateTrigger);
begin
  FOnPropertyChanged.Invoke(Self, 'Count');
end;

function TObservableCollection<T>.GetOnCollectionChanged: TEvent<TCollectionChangedEvent>;
begin
  Result := FOnCollectionChanged.EventHandler;
end;

function TObservableCollection<T>.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

procedure TObservableCollection<T>.Notify(Value: T; Action: TCollectionChangedAction);
var
  LNotifyPropertyChanged: INotifyPropertyChanged;
  LPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  DoCollectionChanged(Value, TCollectionChangedAction(Action));

  if Supports(TObject(Value), INotifyPropertyChanged, LNotifyPropertyChanged) then
  begin
    LPropertyChanged := LNotifyPropertyChanged.OnPropertyChanged;
    case Action of
      caAdd: LPropertyChanged.Add(DoItemPropertyChanged);
      caRemove: LPropertyChanged.Remove(DoItemPropertyChanged);
    end;
  end;

  inherited;
  DoPropertyChanged('Count');
end;

end.
