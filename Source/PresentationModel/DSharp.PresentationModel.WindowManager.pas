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

unit DSharp.PresentationModel.WindowManager;

interface

uses
{$IF COMPILERVERSION > 22}
  System.UITypes,
{$IFEND}
  DSharp.Aspects.Logging,
  DSharp.ComponentModel.Composition;

type
{$IF COMPILERVERSION > 22}
  TMsgDlgType = System.UITypes.TMsgDlgType;
  TMsgDlgButtons = System.UITypes.TMsgDlgButtons;
{$ELSE}
  TMsgDlgType = (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);
  TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
    mbAll, mbNoToAll, mbYesToAll, mbHelp, mbClose);
  TMsgDlgButtons = set of TMsgDlgBtn;
{$IFEND}

  [InheritedExport]
  [Logging]
  IWindowManager = interface(IInvokable)
    ['{61AB99D7-E09D-4D94-B1EC-EA154959809D}']
    function InputBox(const ACaption, APrompt, ADefault: string;
      AShowPasswordChar: Boolean): string;
    function MessageDlg(const Msg: string; DlgType: TMsgDlgType;
      Buttons: TMsgDlgButtons; HelpCtx: LongInt = 0): Integer;

    function ShowDialog(Model: IInterface): Integer; overload;
    function ShowDialog(Model: TObject): Integer; overload;
    procedure ShowWindow(Model: IInterface); overload;
    procedure ShowWindow(Model: TObject); overload;
  end;

const
{$IF COMPILERVERSION > 22}
  mtWarning = System.UITypes.TMsgDlgType.mtWarning;
  mtError = System.UITypes.TMsgDlgType.mtError;
  mtInformation = System.UITypes.TMsgDlgType.mtInformation;
  mtConfirmation = System.UITypes.TMsgDlgType.mtConfirmation;
  mtCustom = System.UITypes.TMsgDlgType.mtCustom;

  mbYes = System.UITypes.TMsgDlgBtn.mbYes;
  mbNo = System.UITypes.TMsgDlgBtn.mbNo;
  mbOK = System.UITypes.TMsgDlgBtn.mbOK;
  mbCancel = System.UITypes.TMsgDlgBtn.mbCancel;
  mbAbort = System.UITypes.TMsgDlgBtn.mbAbort;
  mbRetry = System.UITypes.TMsgDlgBtn.mbRetry;
  mbIgnore= System.UITypes.TMsgDlgBtn.mbIgnore;
  mbAll = System.UITypes.TMsgDlgBtn.mbAll;
  mbNoToAll = System.UITypes.TMsgDlgBtn.mbNoToAll;
  mbYesToAll = System.UITypes.TMsgDlgBtn.mbYesToAll;
  mbHelp = System.UITypes.TMsgDlgBtn.mbHelp;
  mbClose = System.UITypes.TMsgDlgBtn.mbClose;

  mrNone = System.UITypes.mrNone;
  mrOk = System.UITypes.mrOk;
  mrCancel = System.UITypes.mrCancel;
  mrAbort = System.UITypes.mrAbort;
  mrRetry = System.UITypes.mrRetry;
  mrIgnore = System.UITypes.mrIgnore;
  mrYes = System.UITypes.mrYes;
  mrNo = System.UITypes.mrNo;
  mrClose = System.UITypes.mrClose;
  mrHelp = System.UITypes.mrHelp;
  mrTryAgain = System.UITypes.mrTryAgain;
  mrContinue = System.UITypes.mrContinue;
  mrAll = System.UITypes.mrAll;
  mrNoToAll = System.UITypes.mrNoToAll;
  mrYesToAll = System.UITypes.mrYesToAll;
{$ELSE}
  mrNone = 0;
  mrOk = 1;
  mrCancel = 2;
  mrAbort = 3;
  mrRetry = 4;
  mrIgnore = 5;
  mrYes = 6;
  mrNo = 7;
  mrAll = 8;
  mrNoToAll = 9;
  mrYesToAll = 10;
  mrClose = 11;
{$IFEND}

  mbYesNo = [mbYes, mbNo];
  mbYesNoCancel = [mbYes, mbNo, mbCancel];
  mbYesAllNoAllCancel = [mbYes, mbYesToAll, mbNo, mbNoToAll, mbCancel];
  mbOKCancel = [mbOK, mbCancel];
  mbAbortRetryIgnore = [mbAbort, mbRetry, mbIgnore];
  mbAbortIgnore = [mbAbort, mbIgnore];

implementation

end.
