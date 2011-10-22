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

unit DSharp.Windows.ColumnDefinitions;

interface

uses
  Classes,
  DSharp.Bindings,
  DSharp.Collections,
  DSharp.Core.Collections,
  DSharp.Core.DataTemplates,
  SysUtils;

const
  CDefaultWidth = 100;

type
  TColumnDefinition = class;

  TCustomDrawEvent = function(Sender: TObject; ColumnDefinition: TColumnDefinition;
    Item: TObject; TargetCanvas: TCanvas; CellRect: TRect;
    ImageList: TCustomImageList; DrawMode: TDrawMode): Boolean of object;
  TGetTextEvent = function(Sender: TObject; ColumnDefinition: TColumnDefinition;
    Item: TObject): string of object;
  TSetTextEvent = procedure(Sender: TObject; ColumnDefinition: TColumnDefinition;
    Item: TObject; const Value: string) of object;

  TColumnDefinition = class(TCollectionItem)
  private
    FBinding: TBinding;
    FCaption: string;
    FCustomFilter: string;
    FFilter: TPredicate<TObject>;
    FOnCustomDraw: TCustomDrawEvent;
    FOnGetText: TGetTextEvent;
    FOnSetText: TSetTextEvent;
    FWidth: Integer;
    procedure SetCaption(const Value: string);
    procedure SetCustomFilter(const Value: string);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Binding: TBinding read FBinding write FBinding;
    property Caption: string read FCaption write SetCaption;
    property CustomFilter: string read FCustomFilter write SetCustomFilter;
    property Filter: TPredicate<TObject> read FFilter;
    property OnCustomDraw: TCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnGetText: TGetTextEvent read FOnGetText write FOnGetText;
    property OnSetText: TSetTextEvent read FOnSetText write FOnSetText;
    property Width: Integer read FWidth write FWidth default CDefaultWidth;
  end;

  TColumnDefinitions = class(TOwnedCollection<TColumnDefinition>)
  protected
    procedure Initialize; virtual;
  public
    constructor Create(AOwner: TPersistent = nil); override;
    function Add(const Caption: string; const Width: Integer = CDefaultWidth): TColumnDefinition; overload;
  end;

implementation

uses
  DSharp.Bindings.Collections,
  DSharp.Bindings.Notifications,
  StrUtils;

{ TColumnDefinition }

constructor TColumnDefinition.Create(Collection: TCollection);
begin
  inherited;
  FBinding := TBinding.Create();
  FBinding.BindingMode := bmOneWay;
  FBinding.TargetUpdateTrigger := utExplicit;
  FWidth := CDefaultWidth;
end;

destructor TColumnDefinition.Destroy;
begin
  FBinding.Free();
  inherited;
end;

procedure TColumnDefinition.Assign(Source: TPersistent);
var
  LSource: TColumnDefinition;
begin
  if Source is TColumnDefinition then
  begin
    LSource := TColumnDefinition(Source);
    Binding.SourcePropertyName := LSource.Binding.SourcePropertyName;
    Caption := LSource.Caption;
    CustomFilter := LSource.CustomFilter;
    OnCustomDraw := LSource.OnCustomDraw;
    OnGetText := LSource.OnGetText;
    Width := LSource.Width;
  end
  else
  begin
    inherited;
  end;
end;

procedure TColumnDefinition.SetCaption(const Value: string);
begin
  if FCaption = FBinding.SourcePropertyName then
  begin
    FBinding.SourcePropertyName := Value;
  end;
  FCaption := Value;
end;

procedure TColumnDefinition.SetCustomFilter(const Value: string);
var
  LCollectionView: ICollectionView;
begin
  if FCustomFilter <> Value then
  begin
    FCustomFilter := Value;

    if FCustomFilter <> '' then
    begin
      FFilter :=
        function(Item: TObject): Boolean
        begin
          Binding.Source := Item;
          Result := ContainsText(Binding.SourceProperty.GetValue(Item).ToString, FCustomFilter);
          Binding.Source := nil;
        end;
    end
    else
    begin
      FFilter := nil;
    end;

    if Supports(Collection.Owner, ICollectionView, LCollectionView) then
    begin
      // trigger filtering
      LCollectionView.Filter := LCollectionView.Filter;
    end;
  end;
end;

{ TColumnDefinitions }

constructor TColumnDefinitions.Create(AOwner: TPersistent);
begin
  inherited;
  Initialize();
end;

procedure TColumnDefinitions.Initialize;
begin
  // implemented by descendants
end;

function TColumnDefinitions.Add(const Caption: string; const Width: Integer): TColumnDefinition;
begin
  Result := Add();
  Result.Caption := Caption;
  Result.Width := Width;
end;

end.
