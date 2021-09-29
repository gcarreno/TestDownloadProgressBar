{ Implements Forms.Main

  MIT License

  Copyright (c) 2021 Gustavo Carreno <guscarreno@gmail.com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}
unit DPB.Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, ActnList, StdActns;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actDownloadThreadSequential: TAction;
    alMain: TActionList;
    actDownloadSequencial: TAction;
    btnThreadSequential: TButton;
    actFileExit: TFileExit;
    btnFileExit: TButton;
    panMain: TPanel;
    btnSequencial: TButton;
    memLog: TMemo;
    procedure actDownloadSequencialExecute(Sender: TObject);
    procedure actDownloadThreadSequentialExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    procedure InitShortcuts;
    procedure Log(const AMessage: String);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  LCLType
, DPB.Forms.Sequencial
, DPB.Forms.ThreadSequential
;

const
{$IFDEF MSWINDOWS}
  CPaddingToTestMinimizeName = 'C:\Users\User\Downloads\TestDownloadProgressBar\';
{$ELSE}
  CPaddingToTestMinimizeName = '/home/user/Downloads/TestDownloadProgressBar/';
{$ENDIF}

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.actDownloadSequencialExecute(Sender: TObject);
var
  frmSeq: TfrmSequencial;
begin
  actDownloadSequencial.Enabled:= False;
  actDownloadThreadSequential.Enabled:= False;
  actFileExit.Enabled:= False;
  Application.ProcessMessages;
  try
    Log('Performing Sequencial download.');
    Log('  Creating form.');
    frmSeq:= TfrmSequencial.Create(nil);

    Log('  Adding: time_series_covid19_confirmed_global.csv');
    frmSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv',
      CPaddingToTestMinimizeName + 'time_series_covid19_confirmed_global.csv');

    Log('  Adding: time_series_covid19_deaths_global.csv');
    frmSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv',
      CPaddingToTestMinimizeName + 'time_series_covid19_recovered_global.csv');

    Log('  Adding: time_series_covid19_deaths_global.csv');
    frmSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv',
      CPaddingToTestMinimizeName + 'time_series_covid19_recovered_global.csv');

    Log('  Adding: time_series_covid19_confirmed_US.csv');
    frmSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv',
      CPaddingToTestMinimizeName + 'time_series_covid19_confirmed_US.csv');

    Log('  Adding: time_series_covid19_deaths_US.csv');
    frmSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv',
      CPaddingToTestMinimizeName + 'time_series_covid19_deaths_US.csv');

    Log('  Calling Show Modal.');
    frmSeq.ShowModal;
    Log('Done.');
  finally
    Application.ProcessMessages;
    actDownloadSequencial.Enabled:= True;
    actDownloadThreadSequential.Enabled:= True;
    actFileExit.Enabled:= True;
  end;
end;

procedure TfrmMain.actDownloadThreadSequentialExecute(Sender: TObject);
var
  frmThreadSeq: TfrmThreadSequential;
begin
  actDownloadSequencial.Enabled:= False;
  actDownloadThreadSequential.Enabled:= False;
  actFileExit.Enabled:= False;
  Application.ProcessMessages;
  try
    Log('Performing Threaded Sequencial download.');
    Log('  Creating form.');
    frmThreadSeq:= TfrmThreadSequential.Create(nil);

    Log('  Adding: time_series_covid19_confirmed_global.csv');
    frmThreadSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv',
      CPaddingToTestMinimizeName + 'time_series_covid19_confirmed_global.csv');

    Log('  Adding: time_series_covid19_deaths_global.csv');
    frmThreadSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv',
      CPaddingToTestMinimizeName + 'time_series_covid19_recovered_global.csv');

    Log('  Adding: time_series_covid19_deaths_global.csv');
    frmThreadSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv',
      CPaddingToTestMinimizeName + 'time_series_covid19_recovered_global.csv');

    Log('  Adding: time_series_covid19_confirmed_US.csv');
    frmThreadSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv',
      CPaddingToTestMinimizeName + 'time_series_covid19_confirmed_US.csv');

    Log('  Adding: time_series_covid19_deaths_US.csv');
    frmThreadSeq.AddDownload(
      'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv',
      CPaddingToTestMinimizeName + 'time_series_covid19_deaths_US.csv');

    Log('  Calling Show Modal.');
    frmThreadSeq.ShowModal;
    Log('Done.');
  finally
    Application.ProcessMessages;
    actDownloadSequencial.Enabled:= True;
    actDownloadThreadSequential.Enabled:= True;
    actFileExit.Enabled:= True;
  end;
end;

procedure TfrmMain.InitShortcuts;
begin
{$IFDEF LINUX}
  actFileExit.ShortCut := KeyToShortCut(VK_Q, [ssCtrl]);
{$ENDIF}
{$IFDEF WINDOWS}
  actFileExit.ShortCut := KeyToShortCut(VK_X, [ssAlt]);
{$ENDIF}
end;

procedure TfrmMain.Log(const AMessage: String);
begin
  memLog.Append(AMessage);
  Application.ProcessMessages;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  InitShortcuts;
end;

end.

