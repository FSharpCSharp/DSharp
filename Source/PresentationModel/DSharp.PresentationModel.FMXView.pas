unit DSharp.PresentationModel.FMXView;

interface

uses
  Classes,
  SysUtils,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Forms,
  FMX.Layouts,
  FMX.Types,
  DSharp.Core.Reflection,
  DSharp.PresentationModel,
  DSharp.PresentationModel.View;

type
  ViewHelper = class helper for View
    class procedure Initialize; static;
    class procedure SetContentPropertyCore(TargetLocation: TObject;
      View: TObject); static;
  end;

implementation

uses
  Rtti;

resourcestring
  SViewMustHaveASingleRootElement =
    'The view %s must have a TLayout root element.';

  { ViewHelper }

class procedure ViewHelper.Initialize;
begin
  GetFirstNonGeneratedView := function(View: TObject): TObject
    var
      LComponent: TFmxObject;
    begin
      LComponent := View as TFmxObject;
      if LComponent = nil then
        Exit(View);
      if IsGeneratedProperty.GetValue(LComponent).AsBoolean then
      begin
        // Return first element
        if (LComponent.ChildrenCount = 1) and not IsGeneratedProperty.GetValue
          (LComponent.Children[0]).AsBoolean then
          Exit(LComponent.Children[0]);
        raise EInvalidOperation.CreateFmt(SViewMustHaveASingleRootElement,
          [View.ToString]);
      end;
      Result := LComponent;
    end;

  SetContentProperty := procedure(TargetLocation: TObject; View: TObject)
    var
      LElement: TFmxObject;
    begin
      LElement := View as TFmxObject;

      if Assigned(LElement) and Assigned(LElement.Parent) then
        SetContentPropertyCore(LElement.Parent, nil);

      SetContentPropertyCore(TargetLocation, View);
    end;
end;

class procedure ViewHelper.SetContentPropertyCore(TargetLocation,
  View: TObject);
var
  LType: TRttiType;
  LContentProperty: ContentPropertyAttribute;
  LAttributes: TArray<ContentPropertyAttribute>;
  LProperty: TRttiProperty;
  LTargetLocation: TControl;
  LForm: TForm;
  LView: TFmxObject;
begin
  try
    if (TargetLocation is TLayout) or (TargetLocation is TPanel) then
    begin
      LTargetLocation := TargetLocation as TControl;

      // Disable aligning of controls
      LTargetLocation.BeginUpdate;
      try
        // Remove all visible children from target location
        while LTargetLocation.ChildrenCount > 0 do
        begin
          LTargetLocation.Children[0].Parent := nil;
        end;

        // Insert view into target location
        if Assigned(View) then
        begin
          // TForm is the host for the actual view
          LForm := View as TForm;
          // Set TForm's parent to TargentLocation so it will be destroyed when TargetLocation gets destroyed
          LForm.Parent := LTargetLocation;

          // Check if view has single root element
          if (LForm.ComponentCount < 1) and not(LForm.Components[0] is TLayout)
          then
            raise EInvalidOperation.CreateFmt(SViewMustHaveASingleRootElement,
              [View.ToString]);

          // The actual view is the first and only child of TForm
          LView := LForm.Components[0] as TFmxObject;
          // This will show control inside target location
          LView.Parent := LTargetLocation;
        end;
      finally
        // Enable aligning of controls
        LTargetLocation.EndUpdate;
      end;
    end
    else
    begin
      // The following code has not been tested
      LType := GetRttiType(TargetLocation.ClassType);
      LAttributes := LType.GetCustomAttributes<ContentPropertyAttribute>(True);
      if Length(LAttributes) > 0 then
        LContentProperty := LAttributes[0]
      else
        LContentProperty := DefaultContentProperty;

      LProperty := LType.GetProperty(LContentProperty.Name);
      if Assigned(LProperty) then
        LProperty.SetValue(TargetLocation, View);
    end;
  except
    on e: Exception do
      Log.LogException(e);
  end;
end;

initialization

View.Initialize;

end.
