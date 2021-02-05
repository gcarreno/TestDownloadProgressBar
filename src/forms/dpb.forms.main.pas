unit DPB.Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnDownload: TButton;
    edtURL: TEdit;
    lblURL: TLabel;
    memLog: TMemo;
    panMain: TPanel;
    pbMain: TProgressBar;
    procedure btnDownloadClick(Sender: TObject);
  private
    FSize: Int64;
    procedure Log(const AMessage: String);
    procedure DataReceived(Sender : TObject; Const ContentLength, CurrentPos : Int64);
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  fphttpclient
, opensslsockets
, DPB.Common.Utils
;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnDownloadClick(Sender: TObject);
var
  http: TFPHTTPClient;
  headers: TStringList;
  index: Integer;
begin
  btnDownload.Enabled:= False;
  memLog.Clear;
  http:= TFPHTTPClient.Create(nil);
  http.AllowRedirect:= True;
  pbMain.Position:= 0;
  Log('Getting "' + edtURL.Text + '"');
  try
    try
      headers:= TStringList.Create;
      headers.Delimiter:=':';
      TFPHTTPClient.Head(edtURL.Text, headers);
      FSize := 0;
      for index := 0 to Pred(headers.Count) do
      begin
        if LowerCase(headers.Names[index]) = 'content-length' then
        begin
          FSize:= StrToInt64(headers.ValueFromIndex[index]);
        end;
      end;
      Log(Format('Size of file: %s B (%s)', [FormatFloat('#,##0', FSize), FormatBytes(FSize)]));
      http.OnDataReceived:= @DataReceived;
      http.Get(edtURL.Text);
    except
      on E: Exception do
      begin
        if http.ResponseStatusCode > 399 then
        begin
          Log(Format('Status: %d', [http.ResponseStatusCode]));
        end;
        Log('Error: ' + E.Message);
      end;
    end;
  finally
    headers.Free;
    http.Free;
    btnDownload.Enabled:= True;
  end;
end;

procedure TfrmMain.Log(const AMessage: String);
begin
  memLog.Append(AMessage);
  Application.ProcessMessages;
end;

procedure TfrmMain.DataReceived(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
var
  currentPercent: Double;
begin
  currentPercent:= (CurrentPos*100)/FSize;
  pbMain.Position:= round(currentPercent);
  Log(Format('Getting %d of %d (%3.2n%%)', [CurrentPos, FSize, currentPercent]));
end;

end.

