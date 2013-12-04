unit DSharp.PresentationModel.VCLViewModelBinder;

interface

uses
  Classes,
  DSharp.PresentationModel.ViewModelBinder;

type
  ViewModelBinderHelper = class helper for ViewModelBinder
    class procedure Initialize; static;
  end;

implementation

uses
  Rtti,
  DSharp.Core.Reflection,
  TypInfo,
  ActnList;

{ ViewModelBinderHelper }

class procedure ViewModelBinderHelper.Initialize;
begin
  BindCustomAction :=
      function(Action: TComponent; ViewModel: TObject;
      Method: TRttiMethod): Boolean
    var
      LMethod: TMethod;
      LProperty: TRttiProperty;
    begin
      Result := False;
      if Action is TCustomAction then
      begin
        LProperty := GetRttiType(Action.ClassType).GetProperty('OnExecute');
        if LProperty.PropertyType.TypeKind = tkMethod then
        begin
          LMethod.Data := ViewModel;
          LMethod.Code := Method.CodeAddress;
          SetMethodProp(Action, 'OnExecute', LMethod);
          Result := True;
        end
      end;
    end;
end;

initialization

ViewModelBinder.Initialize;

end.
