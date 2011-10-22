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

unit DSharp.ComponentModel.Composition.Primitives;

interface

uses
  DSharp.ComponentModel.Composition,
  Generics.Collections,
  Rtti,
  SysUtils;

type
  TExportMetaData = ExportMetaDataAttribute;

  TExportDeclarationInfo = class
  private
    FName: string;
    FPolicy: TCreationPolicy;
  public
    constructor Create; overload;
    constructor Create(const Name: string); overload;
    // The creation policy (singleton or instance per request)
    property Policy: TCreationPolicy read FPolicy write FPolicy;
    // The identifier
    property Name: string read FName write FName;
  end;

  // Helper class containing information on exported types
  TExportInfo = class(TExportDeclarationInfo)
  private
    FClass: TClass;
    FMeta: TExportMetaData;
    FRttiProperty: TRttiProperty;
  public
    constructor Create(const Name: string; AInstanceClass: TClass); overload;
    // The class to be instantiated
    property InstanceClass: TClass read FClass write FClass;
    // Meta data (if any)
    property MetaData: TExportMetaData read FMeta write FMeta;
    // exported property (if any)
    property RttiProperty: TRttiProperty read FRttiProperty write FRttiProperty;
  end;

  TExportInfoList = TObjectList<TExportInfo>;

  // Base class for catalog for resolving identifiers to the corresponding
  // types.
  TCustomCatalog = class abstract
  public
    function GetExports(const Name: string): TExportInfoList; virtual; abstract;
    function CanResolve(const Name: string): Boolean; virtual; abstract;
  end;

  ECompositionException = class(Exception);

implementation

{ TExportInfo }

constructor TExportInfo.Create(const Name: string; AInstanceClass: TClass);
begin
  inherited Create(Name);
  InstanceClass := AInstanceClass;
end;

{ TExportDeclarationInfo }

constructor TExportDeclarationInfo.Create;
begin
  inherited;
end;

constructor TExportDeclarationInfo.Create(const Name: string);
begin
  Create;
  FName := Name;
end;

end.
