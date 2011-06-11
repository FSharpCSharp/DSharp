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

unit DSharp.Bindings.Collections;

interface

uses
  Classes,
  DSharp.Core.DataTemplates,
  DSharp.Core.Events,
  Generics.Collections,
  SysUtils;

type
  TCollectionChangedEvent = procedure(Sender: TObject; Item: TObject;
    Action: TCollectionNotification) of object;

  INotifyCollectionChanged = interface
    ['{FE0D3160-6BCE-46B6-B01D-1B3C23EA76F3}']
    function GetOnCollectionChanged: TEvent<TCollectionChangedEvent>;
    property OnCollectionChanged: TEvent<TCollectionChangedEvent> read GetOnCollectionChanged;
  end;

  ICollectionView = interface(INotifyCollectionChanged)
    ['{A13215DC-49AC-46AF-A85A-EEC3CC0D709C}']
    function GetCurrentItem: TObject;
    procedure SetCurrentItem(const Value: TObject);
    function GetFilter: TPredicate<TObject>;
    procedure SetFilter(const Value: TPredicate<TObject>);
    function GetItemsSource: TList<TObject>;
    procedure SetItemsSource(const Value: TList<TObject>);
    function GetItemTemplate: IDataTemplate;
    procedure SetItemTemplate(const Value: IDataTemplate);

    property CurrentItem: TObject read GetCurrentItem write SetCurrentItem;
    property Filter: TPredicate<TObject> read GetFilter write SetFilter;
    property ItemsSource: TList<TObject> read GetItemsSource write SetItemsSource;
    property ItemTemplate: IDataTemplate read GetItemTemplate write SetItemTemplate;
  end;

implementation

end.
