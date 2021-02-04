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
;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnDownloadClick(Sender: TObject);
var
  http: TFPHTTPClient;
  response: TStringStream;
begin
  btnDownload.Enabled:= False;
  http:= TFPHTTPClient.Create(nil);
  memLog.Clear;
  pbMain.Position:= 0;
  Application.ProcessMessages;
  try
    http.AllowRedirect:= True;
    response:= TStringStream.Create('');
    http.HTTPMethod('HEAD', edtURL.Text, response, []);
    //http.OnDataReceived:= @DataReceived;
    //http.Get(edtURL.Text);
  finally
    response.Free;
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
begin
  if ContentLength > 0 then
  begin
    if pbMain.Max <> ContentLength then
    begin
      pbMain.Max:= ContentLength;
    end;
    pbMain.Position:= CurrentPos;
    Log(Format('Getting %d of %d', [CurrentPos, ContentLength]));
  end;
end;

end.

