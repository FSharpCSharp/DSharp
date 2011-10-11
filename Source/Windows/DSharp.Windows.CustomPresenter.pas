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

unit DSharp.Windows.CustomPresenter;

interface

uses
  Classes,
  Controls,
  DSharp.Bindings.Collections,
  DSharp.Bindings.Notifications,
  DSharp.Collections,
  DSharp.Core.DataTemplates,
  DSharp.Core.Events,
  DSharp.Windows.ColumnDefinitions,
  Menus,
  SysUtils;

type
  TCustomPresenter = class(TComponent, ICollectionView, INotifyPropertyChanged)
  private
    FColumnDefinitions: TColumnDefinitions;
    FItemsSource: IList<TObject>;
    FItemTemplate: IDataTemplate;
    FImageList: TImageList;
    FFilter: TPredicate<TObject>;
    FOnCollectionChanged: TEvent<TCollectionChangedEvent>;
    FOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    FPopupMenu: TPopupMenu;
    function GetFilter: TPredicate<TObject>;
    function GetItemsSource: IList<TObject>;
    function GetItemTemplate: IDataTemplate; overload;
    function GetOnCollectionChanged: TEvent<TCollectionChangedEvent>;
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    procedure SetColumnDefinitions(const Value: TColumnDefinitions);
    procedure SetFilter(const Value: TPredicate<TObject>);
    procedure SetItemsSource(const Value: IList<TObject>);
    procedure SetItemTemplate(const Value: IDataTemplate);
    procedure SetImageList(const Value: TImageList);
    procedure SetPopupMenu(const Value: TPopupMenu);
  protected
    procedure DoPropertyChanged(const APropertyName: string;
      AUpdateTrigger: TUpdateTrigger = utPropertyChanged);
    procedure DoSourceCollectionChanged(Sender: TObject; Item: TObject;
      Action: TCollectionChangedAction); virtual;
    function GetCurrentItem: TObject; virtual;
    procedure SetCurrentItem(const Value: TObject); virtual;
    procedure InitColumns; virtual;
    procedure InitControl; virtual;
    procedure InitEvents; virtual;
    procedure InitProperties; virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetItemTemplate(const Item: TObject): IDataTemplate; overload;
    procedure Refresh; virtual;

    property CurrentItem: TObject read GetCurrentItem write SetCurrentItem;
    property Filter: TPredicate<TObject> read GetFilter write SetFilter;
    property ItemsSource: IList<TObject> read GetItemsSource write SetItemsSource;
    property ItemTemplate: IDataTemplate read GetItemTemplate write SetItemTemplate;
    property OnCollectionChanged: TEvent<TCollectionChangedEvent> read GetOnCollectionChanged;
  published
    property ColumnDefinitions: TColumnDefinitions
      read FColumnDefinitions write SetColumnDefinitions;
    property ImageList: TImageList read FImageList write SetImageList;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
  end;

implementation

uses
  DSharp.Windows.ColumnDefinitions.DataTemplate;

{ TCustomPresenter }

constructor TCustomPresenter.Create(AOwner: TComponent);
begin
  inherited;
  FOnCollectionChanged.Add(DoSourceCollectionChanged);

  FColumnDefinitions := TColumnDefinitions.Create(Self);
  FItemTemplate := TColumnDefinitionsDataTemplate.Create(FColumnDefinitions);
end;

destructor TCustomPresenter.Destroy;
begin
  if Assigned(FColumnDefinitions) and (FColumnDefinitions.Owner = Self) then
  begin
    FColumnDefinitions.Free();
  end;
  inherited;
end;

procedure TCustomPresenter.DoPropertyChanged(const APropertyName: string;
  AUpdateTrigger: TUpdateTrigger);
begin
  FOnPropertyChanged.Invoke(Self, APropertyName, AUpdateTrigger);
end;

procedure TCustomPresenter.DoSourceCollectionChanged(Sender, Item: TObject;
  Action: TCollectionChangedAction);
begin
  DoPropertyChanged('ItemsSource');

  if Action = caReplace then
  begin
    if Item = CurrentItem then
    begin
      DoPropertyChanged('CurrentItem');
    end;
  end;
end;

function TCustomPresenter.GetCurrentItem: TObject;
begin
  Result := nil; // implemented by descendants
end;

function TCustomPresenter.GetFilter: TPredicate<TObject>;
begin
  Result := FFilter;
end;

function TCustomPresenter.GetItemsSource: IList<TObject>;
begin
  Result := FItemsSource;
end;

function TCustomPresenter.GetItemTemplate: IDataTemplate;
begin
  Result := FItemTemplate;
end;

function TCustomPresenter.GetItemTemplate(const Item: TObject): IDataTemplate;
begin
  Result := nil;

  if Assigned(FItemTemplate) then
  begin
    Result := FItemTemplate.GetItemTemplate(Item);
  end
end;

function TCustomPresenter.GetOnCollectionChanged: TEvent<TCollectionChangedEvent>;
begin
  Result := FOnCollectionChanged.EventHandler;
end;

function TCustomPresenter.GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
begin
  Result := FOnPropertyChanged.EventHandler;
end;

procedure TCustomPresenter.InitColumns;
begin
  // implemented by descendants
end;

procedure TCustomPresenter.InitControl;
begin
  InitColumns();
  InitEvents();
  InitProperties();
  Refresh();
end;

procedure TCustomPresenter.InitEvents;
begin
  // implemented by descendants
end;

procedure TCustomPresenter.InitProperties;
begin
  // implemented by descendants
end;

procedure TCustomPresenter.Loaded;
begin
  inherited;
  InitColumns();
end;

procedure TCustomPresenter.Refresh;
begin
  // implemented by descendants
end;

procedure TCustomPresenter.SetColumnDefinitions(
  const Value: TColumnDefinitions);
begin
  if Assigned(FColumnDefinitions) and (FColumnDefinitions.Owner = Self) then
  begin
    if (FItemTemplate is TColumnDefinitionsDataTemplate)
      and ((FItemTemplate as TColumnDefinitionsDataTemplate).ColumnDefinitions = FColumnDefinitions) then
    begin
      FItemTemplate := nil;
    end;

    FColumnDefinitions.Free();
  end;
  FColumnDefinitions := Value;
  InitColumns();
end;

procedure TCustomPresenter.SetCurrentItem(const Value: TObject);
begin
  DoPropertyChanged('CurrentItem');
end;

procedure TCustomPresenter.SetFilter(const Value: TPredicate<TObject>);
begin
  FFilter := Value;
  Refresh();
end;

procedure TCustomPresenter.SetImageList(const Value: TImageList);
begin
  FImageList := Value;
  InitControl();
end;

procedure TCustomPresenter.SetItemsSource(const Value: IList<TObject>);
var
  LNotifyCollectionChanged: INotifyCollectionChanged;
  LCollectionChanged: TEvent<TCollectionChangedEvent>;
begin
  if FItemsSource <> Value then
  begin
    if Supports(FItemsSource, INotifyCollectionChanged, LNotifyCollectionChanged) then
    begin
      LCollectionChanged := LNotifyCollectionChanged.OnCollectionChanged;
      LCollectionChanged.Remove(DoSourceCollectionChanged);
    end;

    FItemsSource := Value;

    if Assigned(FItemsSource)
      and Supports(FItemsSource, INotifyCollectionChanged, LNotifyCollectionChanged) then
    begin
      LCollectionChanged := LNotifyCollectionChanged.OnCollectionChanged;
      LCollectionChanged.Add(DoSourceCollectionChanged)
    end;
    Refresh();

    DoPropertyChanged('ItemsSource');
  end;
end;

procedure TCustomPresenter.SetItemTemplate(const Value: IDataTemplate);
begin
  FItemTemplate := Value;
  InitColumns();
  Refresh();
end;

procedure TCustomPresenter.SetPopupMenu(const Value: TPopupMenu);
begin
  FPopupMenu := Value;
  InitControl();
end;

end.
