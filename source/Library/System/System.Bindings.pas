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
  System.Events,
  System.NotificationHandler,
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

  IValueConverter = interface
    ['{20006CE1-6C5A-41AF-9E2C-5D625C2BC07D}']
    function Convert(Value: TValue): TValue;
    function ConvertBack(Value: TValue): TValue;
  end;

  TBindingGroup = class;

  TBindingBase = class abstract(TPersistent)
  strict protected
    FActive: Boolean;
    FBindingGroup: TBindingGroup;
    FBindingMode: TBindingMode;
    FConverter: IValueConverter;
    FNotificationHandler: TNotificationHandler<TBindingBase>;
    FOnValidation: TEvent<TValidationEvent>;
    FTarget: TObject;
    FTargetProperty: TRttiProperty;
    FTargetPropertyName: string;
    FTargetUpdateTrigger: TUpdateTrigger;
    FUpdating: Boolean;
    FValidationRules: TObjectList<TValidationRule>;
    procedure DoTargetPropertyChanged(ASender: TObject;
      APropertyName: string; AUpdateTrigger: TUpdateTrigger); virtual; abstract;
    function GetOnValidation: TEvent<TValidationEvent>;
    procedure InitConverter; virtual; abstract;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); virtual;
    procedure SetActive(AValue: Boolean);
    procedure SetBindingGroup(const Value: TBindingGroup);
    procedure SetConverter(Value: IValueConverter);
    procedure SetTarget(AValue: TObject);
    procedure SetTargetPropertyName(AValue: string);

    class var FContext: TRttiContext;
  public
    constructor Create; overload;
    destructor Destroy; override;

    procedure UpdateTarget; virtual; abstract;
    function Validate: Boolean; virtual; abstract;

    property Active: Boolean read FActive write SetActive;
    property BindingGroup: TBindingGroup read FBindingGroup write SetBindingGroup;
    property Converter: IValueConverter read FConverter write SetConverter;
    property OnValidation: TEvent<TValidationEvent> read GetOnValidation;
    property TargetProperty: TRttiProperty read FTargetProperty;
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
    FSourceProperty: TRttiProperty;
    FSourcePropertyName: string;
    FSourceUpdateTrigger: TUpdateTrigger;
    procedure DoSourcePropertyChanged(ASender: TObject;
      APropertyName: string; AUpdateTrigger: TUpdateTrigger);
    procedure DoTargetPropertyChanged(ASender: TObject;
      APropertyName: string; AUpdateTrigger: TUpdateTrigger); override;
    procedure InitConverter; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure SetSource(AValue: TObject);
    procedure SetSourcePropertyName(AValue: string);
  public
    constructor Create(ASource: TObject; ASourcePropertyName: string;
      ATarget: TObject; ATargetPropertyName: string;
      ABindingMode: TBindingMode = BindingModeDefault;
      AConverter: IValueConverter = nil); overload;
    destructor Destroy; override;

    procedure UpdateSource;
    procedure UpdateTarget; override;
    function Validate: Boolean; override;

    property SourceProperty: TRttiProperty read FSourceProperty;
  published
    property Source: TObject read FSource write SetSource;
    property SourcePropertyName: string read FSourcePropertyName
      write SetSourcePropertyName;
    property SourceUpdateTrigger: TUpdateTrigger
      read FSourceUpdateTrigger write FSourceUpdateTrigger default UpdateTriggerDefault;
  end;

  TBindingGroup = class(TComponent)
  private
    FBindings: TList<TBindingBase>;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadBindings(AReader: TReader);
    procedure WriteBindings(AWriter: TWriter);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetBindingForTarget(ATarget: TObject): TBinding;
    property Bindings: TList<TBindingBase> read FBindings;
  end;

function FindBindingGroup(AOwner: TComponent): TBindingGroup;

implementation

uses
  System.Data.Converter.Default;

function FindBindingGroup(AOwner: TComponent): TBindingGroup;
var
  I: Integer;
begin
  Result := nil;
  for i := 0 to Pred(AOwner.ComponentCount) do
  begin
    if AOwner.Components[i] is TBindingGroup then
    begin
      Result := AOwner.Components[i] as TBindingGroup;
      Break;
    end;
  end;
end;

{ TBindingBase }

constructor TBindingBase.Create;
begin
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

procedure TBindingBase.SetActive(AValue: Boolean);
begin
  if FActive <> AValue then
  begin
    FActive := AValue;
  end;
end;

procedure TBindingBase.SetBindingGroup(const Value: TBindingGroup);
begin
  if FBindingGroup <> Value then
  begin
    if Assigned(FBindingGroup) and FBindingGroup.Bindings.Contains(Self) then
    begin
      FBindingGroup.Bindings.Remove(Self);
    end;

    FBindingGroup := Value;

    if Assigned(FBindingGroup) and not FBindingGroup.Bindings.Contains(Self) then
    begin
      FBindingGroup.Bindings.Add(Self);
    end;
  end;
end;

procedure TBindingBase.SetConverter(Value: IValueConverter);
begin
  if FConverter <> Value then
  begin
    FConverter := Value;
  end;
  InitConverter();
end;

procedure TBindingBase.SetTarget(AValue: TObject);
var
  LNotifyPropertyChanged: INotifyPropertyChanged;
begin
  if FTarget <> AValue then
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

    FTarget := AValue;
    FTargetProperty := nil;

    if Assigned(FTarget) then
    begin
      FTargetProperty := FContext.GetType(FTarget.ClassInfo).GetProperty(FTargetPropertyName);
      InitConverter();

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

procedure TBindingBase.SetTargetPropertyName(AValue: string);
begin
  if not SameText(FTargetPropertyName, AValue) then
  begin
    FTargetPropertyName := AValue;
    if Assigned(FTarget) then
    begin
      FTargetProperty := FContext.GetType(FTarget.ClassInfo).GetProperty(FTargetPropertyName);
      InitConverter();
      UpdateTarget();
    end;
  end;
end;

{ TBinding }

constructor TBinding.Create(ASource: TObject; ASourcePropertyName: string;
  ATarget: TObject; ATargetPropertyName: string; ABindingMode: TBindingMode;
  AConverter: IValueConverter);
begin
  inherited Create();
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
  SetSource(nil);

  inherited;
end;

procedure TBinding.DoSourcePropertyChanged(ASender: TObject;
  APropertyName: string; AUpdateTrigger: TUpdateTrigger);
begin
  if not FUpdating and (FBindingMode in [bmOneWay..bmTwoWay])
    and (AUpdateTrigger = FTargetUpdateTrigger)
    and SameText(APropertyName, FSourceProperty.Name) then
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
    and SameText(APropertyName, FTargetProperty.Name) then
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
  if not Assigned(FConverter) and Assigned(FSourceProperty) and Assigned(FTargetProperty)
    and (FSourceProperty.PropertyType.Handle <> FTargetProperty.PropertyType.Handle) then
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

procedure TBinding.SetSource(AValue: TObject);
var
  LNotifyPropertyChanged: INotifyPropertyChanged;
begin
  if FSource <> AValue then
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
    end;

    FSource := AValue;
    FSourceProperty := nil;

    if Assigned(FSource) then
    begin
      FSourceProperty := FContext.GetType(FSource.ClassInfo).GetProperty(FSourcePropertyName);
      InitConverter();

      if FSource is TComponent then
      begin
        TComponent(FSource).FreeNotification(FNotificationHandler);
      end;

      if Supports(FSource, INotifyPropertyChanged, LNotifyPropertyChanged) then
      begin
        LNotifyPropertyChanged.OnPropertyChanged.Add(DoSourcePropertyChanged);
      end;
    end;

    UpdateTarget();
  end;
end;

procedure TBinding.SetSourcePropertyName(AValue: string);
begin
  if not SameText(FSourcePropertyName, AValue) then
  begin
    FSourcePropertyName := AValue;
    if Assigned(FSource) then
    begin
      FSourceProperty := FContext.GetType(FSource.ClassInfo).GetProperty(FSourcePropertyName);
      InitConverter();
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
    and Assigned(FSource) and Assigned(FSourceProperty) and FSourceProperty.IsWritable
    and Assigned(FTarget) and Assigned(FTargetProperty) and FTargetProperty.IsReadable then
  begin
    LTargetValue := FTargetProperty.GetValue(FTarget);

    if Assigned(FConverter) then
    begin
      LSourceValue := FConverter.ConvertBack(LTargetValue);
    end
    else
    begin
      LSourceValue := LTargetValue;
    end;

    FSourceProperty.SetValue(FSource, LSourceValue);
  end;
end;

procedure TBinding.UpdateTarget;
var
  LSourceValue: TValue;
  LTargetValue: TValue;
begin
  if FActive
    and Assigned(FSource) and Assigned(FSourceProperty) and FSourceProperty.IsReadable
    and Assigned(FTarget) and Assigned(FTargetProperty) and FTargetProperty.IsWritable then
  begin
    LSourceValue := FSourceProperty.GetValue(FSource);

    if Assigned(FConverter) then
    begin
      LTargetValue := FConverter.Convert(LSourceValue);
    end
    else
    begin
      LTargetValue := LSourceValue;
    end;

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
        if Assigned(FConverter) then
        begin
          LSourceValue := FConverter.ConvertBack(LTargetValue);
        end
        else
        begin
          LSourceValue := LTargetValue;
        end;

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
  FBindings := TList<TBindingBase>.Create;
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
    Result := TBinding.Create(nil, '', nil, '');
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

type
  TReaderHack = class(TReader)
  end;

procedure TBindingGroup.ReadBindings(AReader: TReader);
var
  LBinding: TBinding;
begin
  AReader.ReadValue();
  try
    if not AReader.EndOfList() then
    begin
      FBindings.Clear();
    end;
    while not AReader.EndOfList() do
    begin
      if AReader.NextValue in [vaInt8, vaInt16, vaInt32] then
      begin
        AReader.ReadInteger;
      end;
      LBinding := TBinding.Create(nil, '', nil, '');
      LBinding.BindingGroup := Self;
      if csDesigning in ComponentState then
      begin
        LBinding.Active := False;
      end;
      AReader.ReadListBegin();
      while not AReader.EndOfList do
      begin
        TReaderHack(AReader).ReadProperty(LBinding);
      end;
      AReader.ReadListEnd();
    end;
    AReader.ReadListEnd();
  finally

  end;
end;

type
  TWriterHack = class(TWriter)
  end;

procedure TBindingGroup.WriteBindings(AWriter: TWriter);
var
  LAncestor: TPersistent;
  LBinding: TBindingBase;
begin
  LAncestor := AWriter.Ancestor;
  AWriter.Ancestor := nil;
  try
    TWriterHack(AWriter).WriteValue(vaCollection);
    for LBinding in FBindings do
    begin
      AWriter.WriteListBegin();
      AWriter.WriteProperties(LBinding);
      AWriter.WriteListEnd();
    end;
    AWriter.WriteListEnd();
  finally
    AWriter.Ancestor := LAncestor;
  end;
end;

end.
