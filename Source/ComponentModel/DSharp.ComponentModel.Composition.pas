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

unit DSharp.ComponentModel.Composition;

interface

uses
  TypInfo;

type
  // The creation policy for a class,
  // either shared (singleton)
  // or non-shared (a new instance per request)
  TCreationPolicy = (cpShared, cpNonShared);

  // Base class for all attributes
  TBaseAttribute = class abstract(TCustomAttribute)
  end;

  // Marks a class as abstract
  AbstractAttribute = class(TCustomAttribute)
  end;

  // Base class for export related attributes
  TBaseExportAttribute = class abstract(TBaseAttribute)
  private
    FName: string;
  public
    property Name: string read FName;
    constructor Create(const Name: string); overload;
    constructor Create(const AClass: TClass); overload;
    constructor Create(const ATypeInfo: PTypeInfo); overload;
    constructor Create; overload;
  end;

  // Attribute that marks all derived classes as exported
  InheritedExportAttribute = class(TBaseExportAttribute)
  end;

  // Attribute that marks the class as exported
  ExportAttribute = class(TBaseExportAttribute)
  end;

  ExportMetaDataAttribute = class(TCustomAttribute)
  end;

  ImportBase = class(TBaseAttribute)
  private
    FName: string;
  public
    property Name: string read FName;
    constructor Create(const Name: string); overload;
    constructor Create; overload;
  end;

  // Attribute marking a property for importing the single export
  ImportAttribute = class(ImportBase);

  // Attribute marking a property for importing all exports as a dynamic array
  ImportManyAttribute = class(ImportBase);

  // Part creation policy defining whether a singleton or an instance
  // per request should be created.
  PartCreationPolicyAttribute = class(TBaseExportAttribute)
  private
    FPolicy: TCreationPolicy;
  public
    constructor Create(Policy: TCreationPolicy);
    property Policy: TCreationPolicy read FPolicy;
  end;

  // Attribute for marking a constructor that the arguments should be injected
  ImportingConstructorAttribute = class(TBaseAttribute)
  end;

implementation

uses
  Rtti;

{ PartCreationPolicy }

constructor PartCreationPolicyAttribute.Create(Policy: TCreationPolicy);
begin
  FPolicy := Policy;
end;

{ ImportBase }

constructor ImportBase.Create(const Name: string);
begin
  FName := Name;
end;

constructor ImportBase.Create;
begin

end;

{ TBaseExportAttribute }

constructor TBaseExportAttribute.Create(const Name: string);
begin
  FName := Name;
  Create;
end;

constructor TBaseExportAttribute.Create;
begin

end;

constructor TBaseExportAttribute.Create(const AClass: TClass);
begin
  Create(TRttiContext.Create.GetType(AClass).QualifiedName);
end;

constructor TBaseExportAttribute.Create(const ATypeInfo: PTypeInfo);
begin
  Create(TRttiContext.Create.GetType(ATypeInfo).QualifiedName);
end;

end.
