unit DSharp.Bindings.Validations;

interface

uses
  DSharp.Core.Validations,
  Rtti;

type
  TDataErrorValidationRule = class(TValidationRule)
  public
    constructor Create; override;
    function Validate(const Value: TValue): IValidationResult; override;
  end;

implementation

uses
  DSharp.Bindings,
  DSharp.Collections,
  SysUtils;

{ TDataErrorValidationRule }

constructor TDataErrorValidationRule.Create;
begin
  ValidationStep := vsCommittedValue;
end;

function TDataErrorValidationRule.Validate(const Value: TValue): IValidationResult;
var
  LBinding: TBinding;
  LBindingGroup: TBindingGroup;
  LItems: TList<TObject>;
  LItem: TObject;
  i: Integer;
  LInfo: IDataErrorInfo;
  LError: string;
  LName: string;
begin
  Result := TValidationResult.ValidResult;

  if Value.IsType<TBindingGroup> then
  begin
    LBindingGroup := Value.AsType<TBindingGroup>;
    if Assigned(LBindingGroup) then
    begin
      LItems := LBindingGroup.Items;

      for i := 0 to Pred(LItems.Count) do
      begin
        if Supports(LItems[i], IDataErrorInfo, LInfo) then
        begin
          LError := LInfo.Error;
          if LError <> '' then
          begin
            Result := TValidationResult.Create(False, LError);
          end;
        end;
      end;
    end;
  end
  else if Value.IsType<TBinding> then
  begin
    LBinding := Value.AsType<TBinding>;
    if Assigned(LBinding) then
    begin
      if Supports(LBinding.Source, IDataErrorInfo, LInfo) then
      begin
        LName := LBinding.SourcePropertyName;
        if LName <> '' then
        begin
          LError := LInfo[LName];
          if LError <> '' then
          begin
            Result := TValidationResult.Create(False, LError);
          end;
        end;
      end;
    end;
  end
  else if Value.IsObject then
  begin
    LItem := Value.AsObject();
    if Assigned(LItem) then
    begin
      if Supports(LItem, IDataErrorInfo, LInfo) then
      begin
        LError := LInfo.Error;
        if LError <> '' then
        begin
          Result := TValidationResult.Create(False, LError);
        end;
      end;
    end;
  end;
end;

end.