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

unit DSharp.Testing.Mock.Interfaces;

interface

uses
  Rtti,
  SysUtils;

type
  TMockAction = reference to function(var Args: array of TValue): TValue;
  TMockMode = (Mock, Stub);

  IWhen<T> = interface
    function WhenCalling: T;
    function WhenCallingWithAnyArguments: T;
  end;

  ISequence<T> = interface(IWhen<T>)
    function InSequence(const Name: string = ''): IWhen<T>;
  end;

  IExpect<T> = interface
    function AtLeast(const Count: Cardinal): ISequence<T>;
    function AtLeastOnce: ISequence<T>;
    function AtMost(const Count: Cardinal): ISequence<T>;
    function AtMostOnce: ISequence<T>;
    function Between(const LowValue, HighValue: Cardinal): ISequence<T>;
    function Exactly(const Count: Cardinal): ISequence<T>;
    function Never: IWhen<T>;
    function Once: ISequence<T>;
  end;

  IMock<T> = interface
    function GetInstance: T;
    function GetMode: TMockMode;
    procedure SetMode(const Value: TMockMode);
    procedure Verify;
    function WillExecute(const Action: TMockAction): IExpect<T>;
    function WillRaise(const Exception: TFunc<Exception>): IExpect<T>;
    function WillReturn(const Value: TValue): IExpect<T>;
    property Instance: T read GetInstance;
    property Mode: TMockMode read GetMode write SetMode;
  end;

implementation

end.