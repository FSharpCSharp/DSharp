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

unit DSharp.ComponentModel.Composition.Catalog;

interface

uses
  DSharp.ComponentModel.Composition,
  DSharp.ComponentModel.Composition.Primitives,
  Generics.Collections,
  Rtti,
  SysUtils;

type
  TRttiCatalog = class(TCustomCatalog)
  private
    FCatalog: TObjectDictionary<string, TExportInfoList>;
    FInheritedExportedInterfaces: TObjectDictionary<TGUID, TExportDeclarationInfo>;
    FContext: TRttiContext;
    function CheckAbstract(RttiType: TRttiType): Boolean;
    function GetExportList(const Name: string): TExportInfoList;
    function GetMeta(RttiType: TRttiType): TExportMetaData;
    function GetName(Attribute: TBaseExportAttribute; RttiType: TRttiType): string;
    function GetPolicy(RttiType: TRttiType; Default: TCreationPolicy): TCreationPolicy;
    procedure HandleExport(RttiType: TRttiType; InstanceClass: TClass; ExportList: TExportInfoList);
    procedure HandleInheritedExportClass(RttiType: TRttiType; InstanceClass: TClass; ExportList: TExportInfoList);
    procedure HandleInheritedExportedInterface(RttiType: TRttiType; InstanceClass: TClass; ExportList: TExportInfoList);
    procedure FindBaseExport(RttiType: TRttiType; InstanceClass: TClass; ExportList: TExportInfoList);
    procedure FindExport(RttiType: TRttiType; InstanceClass: TClass; ExportList: TExportInfoList);
  public
    constructor Create;
    destructor Destroy; override;
    function CanResolve(const Name: string): Boolean; override;
    function GetExports(const Name: string): TExportInfoList; override;
  end;

implementation

const
  EmptyGUID: TGUID = ();

{ TRttiCatalog }

constructor TRttiCatalog.Create;
var
  LType, LParentType: TRttiType;
  LAttribute: TCustomAttribute;
  LDeclarationInfo: TExportDeclarationInfo;
  LExports: TExportInfoList;
  LExport: TExportInfo;
  LMetaData: TExportMetaData;
  LPolicy: TCreationPolicy;
begin
  FContext := TRttiContext.Create;

  FCatalog := TObjectDictionary<string, TExportInfoList>.Create([doOwnsValues]);

  FInheritedExportedInterfaces := TObjectDictionary<TGUID, TExportDeclarationInfo>.Create([doOwnsValues]);
  try
    for LType in FContext.GetTypes do
    begin
      if LType is TRttiInterfaceType then
      begin
        for LAttribute in LType.GetAttributes do
        begin
          if LAttribute is InheritedExportAttribute then
          begin
            if IsEqualGUID(TRttiInterfaceType(LType).GUID, EmptyGUID) then
              raise ECompositionException.Create('Interface without IID not supported.');
            LDeclarationInfo := TExportDeclarationInfo.Create;
            FInheritedExportedInterfaces.Add(TRttiInterfaceType(LType).GUID, LDeclarationInfo);
            LDeclarationInfo.Name := InheritedExportAttribute(LAttribute).Name;
            if LDeclarationInfo.Name = '' then
              LDeclarationInfo.Name := LType.QualifiedName;
          end;
        end;
      end;
    end;

    for LType in FContext.GetTypes do
    begin
      if LType is TRttiInstanceType then
      begin
        // If class is marked [Abstract], we ignore any exports
        if not CheckAbstract(LType) then
        begin
          LExports := TExportInfoList.Create(False);
          try
            // try to find [Export] and interfaces implementing [InheritedExport]
            FindExport(LType, LType.AsInstance.MetaclassType, LExports);

            // Go down the hierarchy and try
            // to find [InheritedExport] in base classes or interfaces implementing
            // [InheritedExport]
            LParentType := LType.BaseType;
            while LParentType <> nil do
            begin
              FindBaseExport(LParentType, LType.AsInstance.MetaclassType, LExports);
              LParentType := LParentType.BaseType;
            end;

            LMetaData := GetMeta(LType);
            LPolicy := GetPolicy(LType, cpNonShared);

            for LExport in LExports do
            begin
              LExport.Policy := LPolicy;
              LExport.MetaData := LMetaData;
              GetExportList(LExport.Name).Add(LExport);
            end;
          finally
            LExports.Free;
          end;
        end;
      end;
    end;
  finally
    FInheritedExportedInterfaces.Free;
  end;
end;

destructor TRttiCatalog.Destroy;
begin
  FCatalog.Free;
  inherited;
end;

function TRttiCatalog.CanResolve(const Name: string): Boolean;
begin
  Result := FCatalog.ContainsKey(Name);
end;

// Check whether the class was marked abastract using the abstract attribute
function TRttiCatalog.CheckAbstract(RttiType: TRttiType): Boolean;
var
  LAttribute: TCustomAttribute;
begin
  Result := False;
  for LAttribute in RttiType.GetAttributes do
  begin
    if LAttribute is AbstractAttribute then
    begin
      Result := True;
      Break;
    end;
  end;
end;

// Create export information for a given type and incorporates the target class
// which will be the type used for instantiating this export.
// This looks for InheritedExport on the class or InheritedExport on interfaces.
procedure TRttiCatalog.FindBaseExport(RttiType: TRttiType; InstanceClass: TClass; ExportList: TExportInfoList);
begin
  HandleInheritedExportClass(RttiType, InstanceClass, ExportList);
  HandleInheritedExportedInterface(RttiType, InstanceClass, ExportList);
end;

// Create export information for a given type and incorporates the target class
// which will be the type used for instantiating this export.
// This looks for Export on the class or InheritedExport on interfaces.
procedure TRttiCatalog.FindExport(RttiType: TRttiType; InstanceClass: TClass; ExportList: TExportInfoList);
begin
  HandleExport(RttiType, InstanceClass, ExportList);
  HandleInheritedExportedInterface(RttiType, InstanceClass, ExportList);
end;

function TRttiCatalog.GetExportList(const Name: string): TExportInfoList;
begin
  // Either take the export list from the catalog or create a new one
  // if it doesn't exist in the catalog yet.
  if not FCatalog.TryGetValue(Name, Result) then
  begin
    Result := TExportInfoList.Create;
    FCatalog.Add(Name, Result);
  end;
end;

function TRttiCatalog.GetExports(const Name: string): TExportInfoList;
begin
  if not FCatalog.TryGetValue(Name, Result) then
  begin
    raise ECompositionException.Create('No export is defined for ' + Name);
  end;
end;

function TRttiCatalog.GetMeta(RttiType: TRttiType): TExportMetaData;
var
  LAttribute: TCustomAttribute;
  LType: TRttiType;
begin
  LType := RttiType;
  while Assigned(LType) do
  begin
    for LAttribute in LType.GetAttributes do
    begin
      if LAttribute is ExportMetaDataAttribute then
      begin
        Exit(ExportMetaDataAttribute(LAttribute));
      end;
    end;
    LType := LType.BaseType;
  end;
  Result := nil;
end;

function TRttiCatalog.GetName(Attribute: TBaseExportAttribute;
  RttiType: TRttiType): string;
begin
  Result := Attribute.Name;
  if Result = '' then
  begin
    Result := RttiType.QualifiedName;
  end;
end;

// Searches the class hierarchy for creation policies, or takes the default one
// it it cannot be found.
function TRttiCatalog.GetPolicy(RttiType: TRttiType; Default: TCreationPolicy): TCreationPolicy;
var
  LAttribute: TCustomAttribute;
  LType: TRttiType;
begin
  LType := RttiType;
  while Assigned(LType) do
  begin
    for LAttribute in LType.GetAttributes do
    begin
      if LAttribute is PartCreationPolicyAttribute then
      begin
        Exit(PartCreationPolicyAttribute(LAttribute).Policy);
      end;
    end;
    LType := LType.BaseType;
  end;
  Result := Default;
end;

// Looks for the Export attribute and creates an Export
procedure TRttiCatalog.HandleExport(RttiType: TRttiType; InstanceClass: TClass; ExportList: TExportInfoList);
var
  LAttribute: TCustomAttribute;
  LProperty: TRttiProperty;
  LExportInfo: TExportInfo;
begin
  for LAttribute in RttiType.GetAttributes do
  begin
    if LAttribute is ExportAttribute then
    begin
      ExportList.Add(TExportInfo.Create(
        GetName(TBaseExportAttribute(LAttribute), RttiType), InstanceClass));
    end;
  end;
  for LProperty in RttiType.GetProperties do
  begin
    for LAttribute in LProperty.GetAttributes do
    begin
      if LAttribute is ExportAttribute then
      begin
        LExportInfo := TExportInfo.Create(
          GetName(TBaseExportAttribute(LAttribute), LProperty.PropertyType), InstanceClass);
        LExportInfo.RttiProperty := LProperty;
        ExportList.Add(LExportInfo);
      end;
    end;
  end;
end;

// Looks for the InheritedExport attribute and creates an Export
procedure TRttiCatalog.HandleInheritedExportClass(RttiType: TRttiType; InstanceClass: TClass; ExportList: TExportInfoList);
var
  LAttribute: TCustomAttribute;
begin
  for LAttribute in RttiType.GetAttributes do
  begin
    if LAttribute is InheritedExportAttribute then
    begin
      ExportList.Add(TExportInfo.Create(
        GetName(TBaseExportAttribute(LAttribute), RttiType), InstanceClass));
    end;
  end;
end;

procedure TRttiCatalog.HandleInheritedExportedInterface(RttiType: TRttiType;
  InstanceClass: TClass; ExportList: TExportInfoList);
var
  i: Integer;
  LInterfaceInfo: TExportDeclarationInfo;
  LInterfaceTable: PInterfaceTable;
begin
  LInterfaceTable := RttiType.AsInstance.MetaclassType.GetInterfaceTable();
  if Assigned(LInterfaceTable) then
  begin
    for i := 0 to LInterfaceTable.EntryCount - 1 do
    begin
      if FInheritedExportedInterfaces.TryGetValue(
        LInterfaceTable.Entries[i].IID, LInterfaceInfo) then
      begin
        ExportList.Add(TExportInfo.Create(LInterfaceInfo.Name, InstanceClass));
      end;
    end;
  end;
end;

end.
