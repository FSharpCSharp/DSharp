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

unit DSharp.Logging.CodeSite;

interface

uses
  DSharp.Logging;

type
  TCodeSiteLog = class(TLogBase)
  protected
    procedure LogEntry(const ALogEntry: TLogEntry); override;
  end;

implementation

uses
  CodeSiteLogging,
  DSharp.Core.Reflection,
  DSharp.Logging.CodeSite.Helper,
  SysUtils;

{ TCodeSiteLog }

procedure TCodeSiteLog.LogEntry(const ALogEntry: TLogEntry);
begin
  if not Assigned(CodeSite) then
    Exit;

  CodeSite.Category := GetTypeInfoName();
  case ALogEntry.LogKind of
    lkEnterMethod:
    begin
      if not ALogEntry.Value.IsEmpty then
      begin
        if ALogEntry.Value.IsClass then
          CodeSite.EnterMethod(ALogEntry.Value.AsClass.ClassName + '.' + ALogEntry.FormatText)
        else if ALogEntry.Value.IsObject then
          CodeSite.EnterMethod(ALogEntry.Value.AsObject, ALogEntry.FormatText)
        else
          CodeSite.EnterMethod(UTF8ToString(ALogEntry.Value.TypeInfo.Name) + '.' + ALogEntry.FormatText);
      end
      else
        CodeSite.EnterMethod(ALogEntry.FormatText);
    end;
    lkLeaveMethod:
    begin
      if not ALogEntry.Value.IsEmpty then
      begin
        if ALogEntry.Value.IsClass then
          CodeSite.ExitMethod(ALogEntry.Value.AsClass.ClassName + '.' + ALogEntry.FormatText)
        else if ALogEntry.Value.IsObject then
          CodeSite.ExitMethod(ALogEntry.Value.AsObject, ALogEntry.FormatText)
        else
          CodeSite.ExitMethod(UTF8ToString(ALogEntry.Value.TypeInfo.Name) + '.' + ALogEntry.FormatText);
      end
      else
        CodeSite.ExitMethod(ALogEntry.FormatText);
    end;
    lkMessage:
    begin
      CodeSite.SendFmtMsg(ALogEntry.FormatText, TValue.ToVarRecs(ALogEntry.Values));
    end;
    lkWarning:
    begin
      CodeSite.SendWarning(Format(ALogEntry.FormatText, TValue.ToVarRecs(ALogEntry.Values)));
    end;
    lkError:
    begin
      CodeSite.SendError(Format(ALogEntry.FormatText, TValue.ToVarRecs(ALogEntry.Values)));
    end;
    lkException:
    begin
      CodeSite.SendException(ALogEntry.FormatText, ALogEntry.Value.AsType<Exception>);
    end;
    lkValue:
    begin
      CodeSite.Send(ALogEntry.FormatText, ALogEntry.Value);
    end;
  end;
end;

initialization
  RegisterLogging(TCodeSiteLog.Create);

end.