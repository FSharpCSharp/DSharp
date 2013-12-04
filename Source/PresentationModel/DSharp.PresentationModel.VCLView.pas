unit DSharp.PresentationModel.VCLView;

interface

uses
  Classes,
  SysUtils,
  Controls,
  ExtCtrls,
  Forms,
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
  Rtti,
  DSharp.PresentationModel.Extensions,
  DSharp.PresentationModel.ViewAwareIntf;

{ ViewHelper }

class procedure ViewHelper.Initialize;
begin
  GetFirstNonGeneratedView := function(View: TObject): TObject
    var
      LComponent: TComponent;
    begin
      LComponent := View as TComponent;
      if LComponent = nil then
        Exit(View);
      if IsGeneratedProperty.GetValue(LComponent).AsBoolean then
      begin
        // Return first element
        if (TWinControl(LComponent).ControlCount = 1) and
          not IsGeneratedProperty.GetValue(TWinControl(LComponent).Controls[0]).AsBoolean
        then
          Exit(TWinControl(LComponent).Controls[0]);
        raise ENotImplemented.Create
          ('TODO: Implement search for GetFirstNonGeneratedView');
      end;
      Result := LComponent;
    end;

  SetContentProperty := procedure(TargetLocation: TObject; View: TObject)
    var
      LElement: TControl;
    begin
      LElement := View as TControl;

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
  LTargetLocation: TPanel;
  LViewAware: IViewAware;
begin
  try
    if TargetLocation is TPanel then
    begin
      LTargetLocation := TargetLocation as TPanel;
      // Disable aligning of controls
      LTargetLocation.DisableAlign;
      try
        // Remove all children from target location
        while LTargetLocation.ControlCount > 0 do
        begin
          // Reparent view to its ViewModel (datacontext)
          if Supports(LTargetLocation.Controls[0].DataContext, IViewAware,
            LViewAware) then
            LViewAware.TakeOwnership(LTargetLocation.Controls[0]);

          // Disconnect from target view
          LTargetLocation.Controls[0].Parent := nil;
        end;

        // Insert view into target location
        if Assigned(View) then
        begin
          // This will show control inside target location
          (View as TControl).Parent := LTargetLocation;

          // Inject view into content presenter (for working TAction shortcuts)
          LTargetLocation.InsertComponent(View as TComponent);
        end;
      finally
        // Enable aligning of controls
        LTargetLocation.EnableAlign;
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
