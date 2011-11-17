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

unit DSharp.Core.RegularExpressions;

interface

uses
{$IF COMPILERVERSION < 22}
  PerlRegEx; // download from http://www.regular-expressions.info/download/TPerlRegEx.zip
{$ELSE}
  RegularExpressions;
{$IFEND}

type
{$IF COMPILERVERSION < 22}
  TRegEx = record
  public
    class function IsMatch(const Input, Pattern: string): Boolean; static;
    class function Replace(const Input, Pattern, Replacement: string): string; static;
  end;
{$ELSE}
  TRegEx = RegularExpressions.TRegEx;
{$IFEND}

implementation

{ TRegEx }

{$IF COMPILERVERSION < 22}
class function TRegEx.IsMatch(const Input, Pattern: string): Boolean;
var
  LRegEx: TPerlRegEx;
begin
  LRegEx := TPerlRegEx.Create();
  try
    LRegEx.RegEx := UTF8Encode(Pattern);
    LRegEx.Subject := UTF8Encode(Input);
    Result := LRegEx.Match();
  finally
    LRegEx.Free();
  end;
end;

class function TRegEx.Replace(const Input, Pattern, Replacement: string): string;
var
  LRegEx: TPerlRegEx;
begin
  LRegEx := TPerlRegEx.Create();
  try
    LRegEx.RegEx := UTF8Encode(Pattern);
    LRegEx.Subject := UTF8Encode(Input);
    LRegEx.Replacement := UTF8Encode(Replacement);
    LRegEx.ReplaceAll();
    Result := UTF8ToString(LRegEx.Subject);
  finally
    LRegEx.Free();
  end;
end;
{$IFEND}

end.