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

unit System.Bindings;

interface

uses
  Classes,
  Generics.Collections,
  Rtti,
  System.Bindings.Collections,
  System.Collections,
  System.Data.Conversion,
  System.Events,
  System.NotificationHandler,
  System.PropertyPath,
  System.Validations,
  SysUtils;

type
  TBindingMode = (bmOneWay, bmTwoWay, bmOneWayToSource);
  TUpdateTrigger = (utPropertyChanged, utLostFocus, utExplicit);

const
  BindingModeDefault = bmTwoWay;
  UpdateTriggerDefault = utPropertyChanged;

type
  TPropertyChangedEvent = procedure(ASender: TObject;
    APropertyName: string; AUpdateTrigger: TUpdateTrigger = utPropertyChanged) of object;

  INotifyPropertyChanged = interface
    ['{6627279B-8112-4A92-BBD3-795185A41966}']
    function GetOnPropertyChanged: TEvent<TPropertyChangedEvent>;
    property OnPropertyChanged: TEvent<TPropertyChangedEvent> read GetOnPropertyChanged;
  end;

  TBindingGroup = class;

  TBindingBase = class abstract(TCollectionItem)
  strict protected
    FActive: Boolean;
    FBindingGroup: TBindingGroup;
    FBindingMode: TBindingMode;
    FConverter: IValueConverter;
    FNotificationHandler: TNotificationHandler<TBindingBase>;
    FOnValidation: TEvent<TValidationEvent>;
    FTarget: TObject;
    FTargetProperty: IPropertyPath;
    FTargetPropertyName: string;
    FTargetUpdateTrigger: TUpdateTrigger;
    FUpdating: Boolean;
    FValidationRules: TObjectList<TValidationRule>;
    procedure DoTargetPropertyChanged(ASender: TObject;
      APropertyName: string; AUpdateTrigger: TUpdateTrigger); virtual; abstract;
    function GetOnValidation: TEvent<TValidationEvent>;
    procedure InitConverter; virtual; abstract;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); virtual;
    procedure SetActive(const Value: Boolean);
    procedure SetBindingGroup(const Value: TBindingGroup);
    procedure SetConverter(const Value: IValueConverter);
    procedure SetTarget(const Value: TObject);
    procedure SetTargetPropertyName(const AValue: string);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure UpdateTarget; virtual; abstract;
    function Validate: Boolean; virtual; abstract;

    property Active: Boolean read FActive write SetActive;
    property BindingGroup: TBindingGroup read FBindingGroup write SetBindingGroup;
    property Converter: IValueConverter read FConverter write SetConverter;
    property OnValidation: TEvent<TValidationEvent> read GetOnValidation;
    property TargetProperty: IPropertyPath read FTargetProperty;
    property ValidationRules: TObjectList<TValidationRule> read FValidationRules;
  published
    property BindingMode: TBindingMode read FBindingMode write FBindingMode default BindingModeDefault;
    property Target: TObject read FTarget write SetTarget;
    property TargetPropertyName: string read FTargetPropertyName
      write SetTargetPropertyName;
    property TargetUpdateTrigger: TUpdateTrigger
      read FTargetUpdateTrigger write FTargetUpdateTrigger default UpdateTriggerDefault;
  end;

  TBinding = class(TBindingBase)
  strict protected
    FSource: TObject;
    FSourceCollectionChanged: INotifyCollectionChanged;
    FSourceProperty: IPropertyPath;
    FSourcePropertyName: string;
    FSourceUpdateTrigger: TUpdateTrigger;
    procedure DoSourceCollectionChanged(Sender: TObject; Item: TObject;
      Action: TCollectionNotification);
    procedure DoSourcePropertyChanged(ASender: TObject;
      APropertyName: string; AUpdateTrigger: TUpdateTrigger);
    procedure DoTargetPropertyChanged(ASender: TObject;
      APropertyName: string; AUpdateTrigger: TUpdateTrigger); override;
    procedure InitConverter; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure SetSource(const Value: TObject);
    procedure SetSourceProperty(AObject: TObject; APropertyName: string);
    procedure SetSourcePropertyName(const Value: string);
  public
    constructor Create(ASource: TObject = nil; ASourcePropertyName: string = '';
      ATarget: TObject = nil; ATargetPropertyName: string = '';
      ABindingMode: TBindingMode = BindingModeDefault;
      AConverter: IValueConverter = nil); reintroduce; overload;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure UpdateSource;
    procedure UpdateTarget; override;
    function Validate: Boolean; override;

    property SourceProperty: IPropertyPath read FSourceProperty;
  published
    property Source: TObject read FSource write SetSource;
    property SourcePropertyName: string read FSourcePropertyName
      write SetSourcePropertyName;
    property SourceUpdateTrigger: TUpdateTrigger
      read FSourceUpdateTrigger write FSourceUpdateTrigger default UpdateTriggerDefault;
  end;

  TBindingGroup = class(TComponent)
  private
    FBindings: TCollection<TBindingBase>;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadBindings(AReader: TReader);
    procedure WriteBindings(AWriter: TWriter);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetBindingForTarget(ATarget: TObject): TBinding;
    property Bindings: TCollection<TBindingBase> read FBindings;
  end;

function FindBindingGroup(AComponent: TPersistent): TBindingGroup;

implementation

uses
  System.Data.Conversion.Default;

function FindBindingGroup(AComponent: TPersistent): TBindingGroup;
var
  i: Integer;
  LOwner: TPersistent;
begin
  Result := nil;
  if AComponent is TCollectionItem then
  begin
    LOwner := GetUltimateOwner(TCollectionItem(AComponent));
  end
  else
  if AComponent is TCollection then
  begin
    LOwner := GetUltimateOwner(TCollection(AComponent));
  end
  else
  begin
    LOwner := GetUltimateOwner(AComponent);
  end;
  if Assigned(LOwner) and (LOwner is TComponent) then
  begin
    for i := 0 to Pred(TComponent(LOwner).ComponentCount) do
    begin
      if TComponent(LOwner).Components[i] is TBindingGroup then
      begin
        Result := TBindingGroup(TComponent(LOwner).Components[i]);
        Break;
      end;
    end;
  end
end;

{ TBindingBase }

procedure TBindingBase.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBindingBase) then
  begin
    Active := TBindingBase(Source).Active;
//    BindingGroup := TBindingBase(Source).BindingGroup;
    Converter := TBindingBase(Source).Converter;
    Target := TBindingBase(Source).Target;
    TargetPropertyName := TBindingBase(Source).TargetPropertyName;
    TargetUpdateTrigger := TBindingBase(Source).TargetUpdateTrigger;
  end;
end;

constructor TBindingBase.Create(Collection: TCollection);
begin
  inherited;

  FNotificationHandler := TNotificationHandler<TBindingBase>.Create(Self, Notification);
  FValidationRules := TObjectList<TValidationRule>.Create();

  FBindingMode := BindingModeDefault;

  FActive := True;
end;

destructor TBindingBase.Destroy;
begin
  SetBindingGroup(nil);
  SetTarget(nil);

  FNotificationHandler.Free();
  FValidationRules.Free();

  inherited;
end;

function TBindingBase.GetOnValidation: TEvent<TValidationEvent>;
begin
  Result := FOnValidation.EventHandler;
end;

procedure TBindingBase.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if AOperation = opRemove then
  begin
    if AComponent = FTarget then
    begin
      FTarget := nil;
      Free;
    end;
  end;
end;

procedure TBindingBase.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
  end;
end;

procedure TBindingBase.SetBindingGroup(const Value: TBindingGroup);
begin
  if FBindingGroup <> Value then
  begin
    FBindingGroup := Value;

    if Assigned(FBindingGroup) then
    begin
      Collection := FBindingGroup.Bindings;
    end;
  end;
end;

procedure TBindingBase.SetConverter(const Value: IValueConverter);
begin
  if FConverter <> Value then
  begin
    FConverter := Value;
  end;
  InitConverter();
end;

procedure TBindingBase.SetTarget(const Value: TObject);
var
  LNotifyPropertyChanged: INotifyPropertyChanged;
begin
  if FTarget <> Value then
  begin
    if Assigned(FTarget) then
    begin
      if FTarget is TComponent then
      begin
        TComponent(FTarget).RemoveFreeNotification(FNotificationHandler);
      end;

      if Supports(FTarget, INotifyPropertyChanged, LNotifyPropertyChanged) then
      begin
        LNotifyPropertyChanged.OnPropertyChanged.Remove(DoTargetPropertyChanged);
      end;
    end;

    FTarget := Value;
    FTargetProperty := nil;

    if Assigned(FTarget) then
    begin
      FTargetProperty := TPropertyPath.Create(FTarget, FTargetPropertyName);

      if FTarget is TComponent then
      begin
        TComponent(FTarget).FreeNotification(FNotificationHandler);
      end;

      if Supports(FTarget, INotifyPropertyChanged, LNotifyPropertyChanged) then
      begin
        LNotifyPropertyChanged.OnPropertyChanged.Add(DoTargetPropertyChanged);
      end;
    end;

    UpdateTarget();
  end;
end;

procedure TBindingBase.SetTargetPropertyName(const AValue: string);
begin
  if not SameText(FTargetPropertyName, AValue) then
  begin
    FTargetPropertyName := AValue;
    if Assigned(FTarget) then
    begin
      FTargetProperty := TPropertyPath.Create(FTarget, FTargetPropertyName);
      UpdateTarget();
    end;
  end;
end;

{ TBinding }

procedure TBinding.Assign(Source: TPersistent);
begin
  inherited;
  if Assigned(Source) and (Source is TBinding) then
  begin
    Self.Source := TBinding(Source).Source;
    Self.SourcePropertyName := TBinding(Source).SourcePropertyName;
    Self.SourceUpdateTrigger := TBinding(Source).SourceUpdateTrigger;
  end;
end;

constructor TBinding.Create(ASource: TObject; ASourcePropertyName: string;
  ATarget: TObject; ATargetPropertyName: string; ABindingMode: TBindingMode;
  AConverter: IValueConverter);
begin
  inherited Create(nil);
  FActive := False;

  FBindingMode := ABindingMode;

  FSourcePropertyName := ASourcePropertyName;
  SetSource(ASource);
  FTargetPropertyName := ATargetPropertyName;
  SetTarget(ATarget);

  SetConverter(AConverter);
  FActive := True;

  UpdateTarget();
end;

destructor TBinding.Destroy;
begin
  if IsValid(FSource) then  // workaround for already freed non TComponent source
  begin
    SetSource(nil);
  end;

  inherited;
end;

procedure TBinding.DoSourceCollectionChanged(Sender: TObject;
  Item: TObject; Action: TCollectionNotification);
var
  LCollectionView: ICollectionView;
  LEvent: TEvent<TCollectionChangedEvent>;
begin
  if Supports(FTarget, ICollectionView, LCollectionView) then
  begin
    LEvent := LCollectionView.OnCollectionChanged;
    LEvent.Invoke(Sender, Item, Action);
  end;
end;

procedure TBinding.DoSourcePropertyChanged(ASender: TObject;
  APropertyName: string; AUpdateTrigger: TUpdateTrigger);
begin
  if not FUpdating and (FBindingMode in [bmOneWay..bmTwoWay])
    and (AUpdateTrigger = FTargetUpdateTrigger)
    and (SameText(APropertyName, FSourceProperty.Root)) then
  begin
    FUpdating := True;
    try
      UpdateTarget();
    finally
      FUpdating := False;
    end;
  end;
end;

procedure TBinding.DoTargetPropertyChanged(ASender: TObject;
  APropertyName: string; AUpdateTrigger: TUpdateTrigger);
begin
  if not FUpdating and (FBindingMode in [bmTwoWay..bmOneWayToSource])
    and (AUpdateTrigger = FSourceUpdateTrigger)
    and SameText(APropertyName, FTargetProperty.Root) then
  begin
    FUpdating := True;
    try
      if Validate() then
      begin
        UpdateSource();
      end;
    finally
      FUpdating := False;
    end;
  end;
end;

procedure TBinding.InitConverter;
begin
  if (not Assigned(FConverter) or ((FConverter as TObject) is TDefaultConverter))
    and Assigned(FSourceProperty) and Assigned(FSourceProperty.PropertyType)
    and Assigned(FTargetProperty) and Assigned(FTargetProperty.PropertyType) then
  begin
    FConverter := TDefaultConverter.Create(
      FSourceProperty.PropertyType.Handle,
      FTargetProperty.PropertyType.Handle);
  end;
end;

procedure TBinding.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited;

  if AOperation = opRemove then
  begin
    if AComponent = FSource then
    begin
      FSource := nil;
      Free;
    end;
  end;
end;

procedure TBinding.SetSource(const Value: TObject);
var
  LNotifyPropertyChanged: INotifyPropertyChanged;
begin
  if FSource <> Value then
  begin
    if Assigned(FSource) then
    begin
      if FSource is TComponent then
      begin
        TComponent(FSource).RemoveFreeNotification(FNotificationHandler);
      end;

      if Supports(FSource, INotifyPropertyChanged, LNotifyPropertyChanged) then
      begin
        LNotifyPropertyChanged.OnPropertyChanged.Remove(DoSourcePropertyChanged);
      end;

      if Assigned(FSourceCollectionChanged) then
      begin
        FSourceCollectionChanged.OnCollectionChanged.Remove(DoSourceCollectionChanged);
      end;
    end;

    SetSourceProperty(Value, FSourcePropertyName);

    if Assigned(FSource) then
    begin
      SetSourceProperty(FSource, FSourcePropertyName);

      if FSource is TComponent then
      begin
        TComponent(FSource).FreeNotification(FNotificationHandler);
      end;

      if Supports(FSource, INotifyPropertyChanged, LNotifyPropertyChanged) then
      begin
        LNotifyPropertyChanged.OnPropertyChanged.Add(DoSourcePropertyChanged);
      end;

      // maybe the source itself is a collection?
      if not Assigned(FSourceCollectionChanged)
        and Supports(FSource, INotifyCollectionChanged, FSourceCollectionChanged) then
      begin
        FSourceCollectionChanged.OnCollectionChanged.Add(DoSourceCollectionChanged);
      end;
    end;

    UpdateTarget();
  end;
end;

procedure TBinding.SetSourceProperty(AObject: TObject; APropertyName: string);
begin
  if Assigned(FSourceCollectionChanged) then
  begin
    FSourceCollectionChanged.OnCollectionChanged.Remove(DoSourceCollectionChanged);
  end;

  FSource := AObject;

  if Assigned(FSource) then
  begin
    FSourceProperty := TPropertyPath.Create(FSource, FSourcePropertyName);
  end
  else
  begin
    FSourceProperty := nil;
  end;

  if Assigned(FSource) and Assigned(FSourceProperty)
    and FSourceProperty.PropertyType.IsInstance and FSourceProperty.IsReadable
    and Supports(FSourceProperty.GetValue(FSource).AsObject,
    INotifyCollectionChanged, FSourceCollectionChanged) then
  begin
    FSourceCollectionChanged.OnCollectionChanged.Add(DoSourceCollectionChanged);
  end;
end;

procedure TBinding.SetSourcePropertyName(const Value: string);
begin
  if not SameText(FSourcePropertyName, Value) then
  begin
    FSourcePropertyName := Value;
    if Assigned(FSource) then
    begin
      SetSourceProperty(FSource, FSourcePropertyName);

      UpdateTarget();
    end;
  end;
end;

procedure TBinding.UpdateSource;
var
  LSourceValue: TValue;
  LTargetValue: TValue;
begin
  if FActive
    and Assigned(FTarget) and Assigned(FTargetProperty)
    and Assigned(FSource) and Assigned(FSourceProperty)
    and FTargetProperty.IsReadable and FSourceProperty.IsWritable then
  begin
    LTargetValue := FTargetProperty.GetValue(FTarget);

    InitConverter();
    LSourceValue := FConverter.ConvertBack(LTargetValue);

    FSourceProperty.SetValue(FSource, LSourceValue);
  end;
end;

procedure TBinding.UpdateTarget;
var
  LSourceValue: TValue;
  LTargetValue: TValue;
begin
  if FActive
    and Assigned(FTarget) and Assigned(FTargetProperty)
    and Assigned(FSource) and Assigned(FSourceProperty)
    and FTargetProperty.IsWritable and FSourceProperty.IsReadable then
  begin
    LSourceValue := FSourceProperty.GetValue(FSource);

    if Assigned(FSourceCollectionChanged) then
    begin
      FSourceCollectionChanged.OnCollectionChanged.Remove(DoSourceCollectionChanged);
    end;

    FSourceCollectionChanged := nil;

    if LSourceValue.IsObject and Supports(LSourceValue.AsObject,
      INotifyCollectionChanged, FSourceCollectionChanged) then
    begin
      FSourceCollectionChanged.OnCollectionChanged.Add(DoSourceCollectionChanged);
    end;

    InitConverter();
    LTargetValue := FConverter.Convert(LSourceValue);

    FTargetProperty.SetValue(FTarget, LTargetValue);
  end;
end;

function TBinding.Validate: Boolean;
var
  LSourceValue: TValue;
  LTargetValue: TValue;
  LValidationResult: TValidationResult;
  LValidationRule: TValidationRule;
begin
  Result := True;
  LValidationRule := nil;
  try
    if Assigned(FTarget) and Assigned(FTargetProperty) and FTargetProperty.IsReadable then
    begin
      LTargetValue := FTargetProperty.GetValue(FTarget);

      for LValidationRule in FValidationRules do
      begin
        if LValidationRule.ValidationStep = vsRawProposedValue then
        begin
          LValidationResult := LValidationRule.Validate(LTargetValue);
          FOnValidation.Invoke(Self, LValidationRule, LValidationResult);
          if not LValidationResult.IsValid then
          begin
            Result := False;
            Break;
          end;
        end;
      end;

      if Result then
      begin
        InitConverter();
        LSourceValue := FConverter.ConvertBack(LTargetValue);

        for LValidationRule in FValidationRules do
        begin
          if LValidationRule.ValidationStep = vsConvertedProposedValue then
          begin
            LValidationResult := LValidationRule.Validate(LSourceValue);
            FOnValidation.Invoke(Self, LValidationRule, LValidationResult);
            if not LValidationResult.IsValid then
            begin
              Result := False;
              Break;
            end;
          end;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      FOnValidation.Invoke(Self, LValidationRule, TValidationResult.Create(False, E.Message));
      Result := False;
    end;
  end;
end;

{ TBindingGroup }

constructor TBindingGroup.Create(AOwner: TComponent);
var
  i: Integer;
begin
  if Assigned(AOwner) then
  begin
    for i := 0 to AOwner.ComponentCount - 1 do
    begin
      if AOwner.Components[i] is TBindingGroup then
      begin
        raise Exception.Create('Only one binding group allowed');
      end;
    end;
  end;
  inherited;
  FBindings := TOwnedCollection<TBindingBase>.Create(Self);
end;

destructor TBindingGroup.Destroy;
begin
  FBindings.Free();
  inherited;
end;

function TBindingGroup.GetBindingForTarget(ATarget: TObject): TBinding;
var
  LBinding: TBindingBase;
begin
  Result := nil;
  for LBinding in Bindings do
  begin
    if LBinding.Target = ATarget then
    begin
      Result := LBinding as TBinding;
      Break;
    end;
  end;
  if not Assigned(Result) then
  begin
    Result := TBinding.Create(nil);
    Result.Active := False;
    Result.BindingGroup := Self;
    Result.Target := ATarget;
  end;
end;

procedure TBindingGroup.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Bindings', ReadBindings, WriteBindings, True);
end;

procedure TBindingGroup.ReadBindings(AReader: TReader);
var
  LBindings: TCollection<TBinding>;
begin
  FBindings.Clear();
  LBindings := TCollection<TBinding>.Create();
  try
    AReader.ReadValue();
    AReader.ReadCollection(LBindings);
    while LBindings.Count > 0 do
    begin
      LBindings[0].Active := not (csDesigning in ComponentState);
      LBindings[0].BindingGroup := Self;
    end;
  finally
    LBindings.Free();
  end;
end;

procedure TBindingGroup.WriteBindings(AWriter: TWriter);
var
  LBinding: TBindingBase;
  LBindings: TCollection<TBinding>;
begin
  LBindings := TCollection<TBinding>.Create();
  try
    for LBinding in FBindings do
    begin
      if (LBinding is TBinding) and Assigned(TBinding(LBinding).Source)
        and (Trim(TBinding(LBinding).SourcePropertyName) <> '') then
      begin
        LBindings.Add().Assign(LBinding);
      end;
    end;
    AWriter.WriteCollection(LBindings);
  finally
    LBindings.Free();
  end;
end;

end.
