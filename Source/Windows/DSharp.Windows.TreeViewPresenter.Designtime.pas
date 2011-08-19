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

unit DSharp.Windows.TreeViewPresenter.Designtime;

interface

procedure Register;

implementation

uses
  Classes,
  ColnEdit,
  DesignEditors,
  DesignIntf,
  DSharp.Bindings,
  DSharp.Windows.ColumnDefinitions,
  DSharp.Windows.TreeViewPresenter,
  SysUtils,
  TypInfo;

type
  TBindingProperty = class(TClassProperty)
  private
    function FilterFunc(const ATestEditor: IProperty): Boolean;
  public
    procedure GetProperties(Proc: TGetPropProc); override;
  end;

  TTreeViewPresenterComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TTreeViewPresenterSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure Register;
begin
  RegisterComponents('Virtual Controls', [TTreeViewPresenter]);
  RegisterComponentEditor(TTreeViewPresenter, TTreeViewPresenterComponentEditor);
  RegisterSelectionEditor(TTreeViewPresenter, TTreeViewPresenterSelectionEditor);
  RegisterPropertyEditor(TypeInfo(TBinding), TColumnDefinition, 'Binding', TBindingProperty);
end;

{ TBindingProperty }

function TBindingProperty.FilterFunc(const ATestEditor: IProperty): Boolean;
begin
  Result := SameText(ATestEditor.GetName(), 'SourcePropertyName');
end;

procedure TBindingProperty.GetProperties(Proc: TGetPropProc);
var
  Components: IDesignerSelections;
begin
  Components := TDesignerSelections.Create;
  Components.Add(TPersistent(GetOrdValue));
  GetComponentProperties(Components, tkProperties, Designer, Proc, FilterFunc);
end;

{ TTreeViewPresenterComponentEditor }

procedure TTreeViewPresenterComponentEditor.ExecuteVerb(Index: Integer);
begin
  ShowCollectionEditorClass(Designer, TCollectionEditor, Component,
    (Component as TTreeViewPresenter).ColumnDefinitions, 'ColumnDefinitions');
end;

function TTreeViewPresenterComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Column defintions...';
end;

function TTreeViewPresenterComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TTreeViewPresenterSelectionEditor }

procedure TTreeViewPresenterSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('DSharp.Windows.ColumnDefinitions');
end;

end.
