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

unit DSharp.Core.Dynamics;

interface

uses
{$IF COMPILERVERSION < 23}
  DSharp.Core.VirtualInterface,
{$IFEND}
  Generics.Collections,
  Rtti,
  SysUtils,
  TypInfo;

function Supports(const ModuleName: string; IID: TGUID; out Intf): Boolean; overload;

type
  TVirtualObjectInterface = class(TVirtualInterface)
  private
    FInstance: TObject;
    FMethods: TDictionary<Integer, TRttiMethod>;
    procedure InternalInvoke(Method: TRttiMethod;
      const Args: TArray<TValue>; out Result: TValue);
  public
    constructor Create(TypeInfo: PTypeInfo; Instance: TObject);
    destructor Destroy; override;
  end;

  TInterfaceMethodInterceptor = class
  private
    FGuid: TGUID;
    FOnBefore: TInterceptBeforeNotify;
    FOnAfter: TInterceptAfterNotify;
    FOnException: TInterceptExceptionNotify;
    FTypeInfo: Pointer;
    procedure DoAfter(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; var Result: TValue);
    procedure DoBefore(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);
    procedure DoException(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out RaiseException: Boolean;
      TheException: Exception; out Result: TValue);
  public
    constructor Create(TypeInfo: PTypeInfo);
    function Proxify(Instance: IInterface): IInterface;
    property OnBefore: TInterceptBeforeNotify read FOnBefore write FOnBefore;
    property OnAfter: TInterceptAfterNotify read FOnAfter write FOnAfter;
    property OnException: TInterceptExceptionNotify read FOnException write FOnException;
  end;

implementation

uses
  DSharp.Core.Reflection,
  RTLConsts,
  Windows;

resourcestring
  CInterfaceMissingRTTI = 'interface %s does not contain RTTI';
  CMethodNotImplemented = 'method not implemented: %0:s.%1:s(%2:s)';

type
  TVirtualLibraryInterface = class(TVirtualInterface)
  private
    FLibrayHandle: THandle;
    FMethods: TDictionary<string, Pointer>;
    procedure InternalInvoke(Method: TRttiMethod;
      const Args: TArray<TValue>; out Result: TValue);
  public
    constructor Create(PIID: PTypeInfo; ALibraryHandle: THandle);
    destructor Destroy; override;
  end;

function Supports(const ModuleName: string; IID: TGUID; out Intf): Boolean; overload;
var
  LType: TRttiType;
  LLibraryHandle: THandle;
  LVirtualLibraryInterface: IInterface;
begin
  Result := False;
  LLibraryHandle := 0;
  try
    LLibraryHandle := LoadLibrary(PChar(ModuleName));
    if LLibraryHandle <> 0 then
    begin
      if FindType(IID, LType) then
      begin
        LVirtualLibraryInterface := TVirtualLibraryInterface.Create(
          LType.Handle, LLibraryHandle);
      end;
      Result := Supports(
        LVirtualLibraryInterface, TRttiInterfaceType(LType).GUID, Intf);
    end;
  except
    FreeLibrary(LLibraryHandle);
  end;
end;

{ TVirtualLibraryInterface }

constructor TVirtualLibraryInterface.Create(PIID: PTypeInfo; ALibraryHandle: THandle);
var
  LType: TRttiType;
  LMethods: TArray<TRttiMethod>;
  LMethod: TRttiMethod;
  LMethodPointer: Pointer;
begin
  FLibrayHandle := ALibraryHandle;
  FMethods := TDictionary<string, Pointer>.Create();
  LType := GetRttiType(PIID);
  LMethods := LType.GetMethods;
  for LMethod in LMethods do
  begin
    if LMethod.VirtualIndex > 2 then
    begin
      LMethodPointer := GetProcAddress(FLibrayHandle, PChar(LMethod.Name));
      if Assigned(LMethodPointer) then
        FMethods.Add(LMethod.Name, LMethodPointer)
      else
        raise ENotSupportedException.CreateFmt('Method not found: "%s"', [LMethod.Name]);
    end;
  end;

  inherited Create(PIID, InternalInvoke);
end;

destructor TVirtualLibraryInterface.Destroy;
begin
  FMethods.Free();
  FreeLibrary(FLibrayHandle);
  inherited;
end;

procedure TVirtualLibraryInterface.InternalInvoke(Method: TRttiMethod;
  const Args: TArray<TValue>; out Result: TValue);
var
  i: Integer;
  LArgs: TArray<TValue>;
  LParams: TArray<TRttiParameter>;
begin
  LParams := Method.GetParameters();

  if Pred(Length(Args)) <> Length(LParams) then
    raise EInvocationError.CreateRes(@SParameterCountMismatch);

  SetLength(LArgs, Length(LParams));

  for i := Low(LArgs) to High(LArgs) do
  begin
    if LParams[i].ParamType = nil then
    begin
      LArgs[i] := TValue.From<Pointer>(Args[i + 1].GetReferenceToRawData);
    end
    else
    begin
      if LParams[i].Flags * [pfVar, pfOut] <> [] then
      begin
        if LParams[i].ParamType.Handle <> Args[i + 1].TypeInfo then
          raise EInvalidCast.CreateRes(@SByRefArgMismatch);
        LArgs[i] := TValue.From<Pointer>(Args[i + 1].GetReferenceToRawData);
      end
      else
      begin
        LArgs[i] := Args[i + 1].Cast(LParams[i].ParamType.Handle);
      end;
    end;
  end;

  if Method.ReturnType = nil then
  begin
    Result := Invoke(FMethods[Method.Name], LArgs, Method.CallingConvention, nil);
  end
  else
  begin
    Result := Invoke(FMethods[Method.Name], LArgs, Method.CallingConvention, Method.ReturnType.Handle);
  end;
end;

{ TVirtualObjectInterface }

constructor TVirtualObjectInterface.Create(TypeInfo: PTypeInfo; Instance: TObject);
var
  LType: TRttiType;
  LMethods: TArray<TRttiMethod>;
  LMethod: TRttiMethod;
  LInstanceType: TRttiType;
  LInstanceMethod: TRttiMethod;
begin
  FInstance := Instance;
  FMethods := TObjectDictionary<Integer, TRttiMethod>.Create();
  LType := GetRttiType(TypeInfo);
  LMethods := LType.GetMethods;
  LInstanceType := GetRttiType(Instance.ClassInfo);
  if Length(LMethods) = 0 then
  begin
    if LType.IsPublicType then
      raise Exception.CreateResFmt(@CInterfaceMissingRTTI, [LType.QualifiedName])
    else
      raise Exception.CreateResFmt(@CInterfaceMissingRTTI, [LType.Name]);
  end;

  for LMethod in LMethods do
  begin
    if LMethod.VirtualIndex > 2 then
    begin
      for LInstanceMethod in LInstanceType.GetMethods() do
      begin
        if SameText(LMethod.Name, LInstanceMethod.Name) then
        begin
          if TRttiParameter.Equals(
            LMethod.GetParameters(), LInstanceMethod.GetParameters()) then
          begin
            FMethods.Add(LMethod.VirtualIndex, LInstanceMethod);
            Break;
          end;
        end;
      end;
    end;
  end;

  inherited Create(TypeInfo, InternalInvoke);
end;

destructor TVirtualObjectInterface.Destroy;
begin
  FMethods.Free();
  inherited;
end;

procedure TVirtualObjectInterface.InternalInvoke(Method: TRttiMethod;
  const Args: TArray<TValue>; out Result: TValue);
var
  i: Integer;
  LArgs: TArray<TValue>;
  LParams: TArray<TRttiParameter>;
begin
  if FMethods.ContainsKey(Method.VirtualIndex) then
  begin
    LParams := Method.GetParameters();

    if Pred(Length(Args)) <> Length(LParams) then
      raise EInvocationError.CreateRes(@SParameterCountMismatch);

    SetLength(LArgs, Length(LParams));

    for i := Low(LArgs) to High(LArgs) do
    begin
      if LParams[i].ParamType = nil then
      begin
        LArgs[i] := TValue.From<Pointer>(Args[i + 1].GetReferenceToRawData);
      end
      else
      begin
        if (pfConst in LParams[i].Flags) and (LParams[i].ParamType.TypeSize > SizeOf(Pointer))
          or (LParams[i].Flags * [pfVar, pfOut] <> []) then
        begin
          if LParams[i].ParamType.Handle <> Args[i + 1].TypeInfo then
            raise EInvalidCast.CreateRes(@SByRefArgMismatch);
          TValue.Make(Args[i + 1].GetReferenceToRawData, Args[i + 1].TypeInfo, LArgs[i]);
        end
        else
        begin
          LArgs[i] := Args[i + 1].Cast(LParams[i].ParamType.Handle);
        end;
      end;
    end;

    if FMethods[Method.VirtualIndex].IsClassMethod then
    begin
      Result := FMethods[Method.VirtualIndex].Invoke(FInstance.ClassType, LArgs);
    end
    else
    begin
      Result := FMethods[Method.VirtualIndex].Invoke(FInstance, LArgs);
    end;
  end
  else
  begin
    raise ENotImplemented.CreateResFmt(@CMethodNotImplemented,
      [Method.Parent.Name, Method.Name, TValue.ToString(@Args[1])]);
  end;
end;

{ TInterfaceMethodInterceptor }

constructor TInterfaceMethodInterceptor.Create(TypeInfo: PTypeInfo);
begin
//  inherited Create(TypeInfo, InternalInvoke);
  FGuid := (GetRttiType(TypeInfo) as TRttiInterfaceType).GUID;
  FTypeInfo := TypeInfo;
end;

procedure TInterfaceMethodInterceptor.DoAfter(Instance: TObject;
  Method: TRttiMethod; const Args: TArray<TValue>; var Result: TValue);
begin
  if Assigned(FOnAfter) then
    FOnAfter(Instance, Method, Args, Result);
end;

procedure TInterfaceMethodInterceptor.DoBefore(Instance: TObject;
  Method: TRttiMethod; const Args: TArray<TValue>; out DoInvoke: Boolean;
  out Result: TValue);
begin
  if Assigned(FOnBefore) then
    FOnBefore(Instance, Method, Args, DoInvoke, Result);
end;

procedure TInterfaceMethodInterceptor.DoException(Instance: TObject;
  Method: TRttiMethod; const Args: TArray<TValue>; out RaiseException: Boolean;
  TheException: Exception; out Result: TValue);
begin
  if Assigned(FOnException) then
    FOnException(Instance, Method, Args, RaiseException, TheException, Result);
end;

function TInterfaceMethodInterceptor.Proxify(Instance: IInterface): IInterface;
var
  LVirtualInterface: TVirtualInterface;
begin
  if Supports(Instance, FGuid) then
  begin
    LVirtualInterface := TVirtualInterface.Create(FTypeInfo,
      procedure(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue)
      var
        i: Integer;
        LArgs: TArray<TValue>;
        LParams: TArray<TRttiParameter>;
        LInstance: IInterface;
        LDoInvoke: Boolean;
      begin
        SetLength(LArgs, Length(Args) - 1);
        for i := 1 to Length(Args) - 1 do
          LArgs[i - 1] := Args[i];
        Supports(Instance, FGuid, LInstance);

        LDoInvoke := True;
        try
          DoBefore(LInstance as TObject, Method, LArgs, LDoInvoke, Result);
          if LDoInvoke then
          begin
            try
              LParams := Method.GetParameters();

              for i := 1 to Length(Args) - 1 do
              begin
                if (pfConst in LParams[i - 1].Flags) and (LParams[i - 1].ParamType.TypeSize > SizeOf(Pointer))
                  or (LParams[i - 1].Flags * [pfVar, pfOut] <> []) then
                begin
                  if LParams[i - 1].ParamType.Handle <> Args[i].TypeInfo then
                    raise EInvalidCast.CreateRes(@SByRefArgMismatch);
                  TValue.Make(Args[i].GetReferenceToRawData, Args[i].TypeInfo, LArgs[i - 1]);
                end
                else
                begin
                  LArgs[i - 1] := Args[i];
                end;
              end;

              Result := Method.Invoke(TValue.From<IInterface>(LInstance), LArgs);
            except
              on E: Exception do
              begin
                DoException(LInstance as TObject, Method, LArgs, LDoInvoke, E, Result);
                if LDoInvoke then
                  raise;
              end;
            end;
            DoAfter(LInstance as TObject, Method, LArgs, Result);
          end;
        finally
          for i := 1 to Length(Args) - 1 do
            Args[i] := LArgs[i - 1];
        end;
      end);

    Supports(LVirtualInterface, FGuid, Result);
  end
  else
    raise ENotSupportedException.Create('');
end;

end.
