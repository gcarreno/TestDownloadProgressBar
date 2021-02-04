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
    function FormatBytes(ABytes: Int64): String;
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
  headers: TStringList;
  index: Integer;
begin
  btnDownload.Enabled:= False;
  memLog.Clear;
  http:= TFPHTTPClient.Create(nil);
  pbMain.Position:= 0;
  Log('Getting "' + edtURL.Text + '"');
  try
    http.AllowRedirect:= True;
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
    Log('Size of file: ' + FormatBytes(FSize));
    http.OnDataReceived:= @DataReceived;
    http.Get(edtURL.Text);
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

function TfrmMain.FormatBytes(ABytes: Int64): String;
var
  dSize: Double;
begin
  Result := '';
  dSize := 0.0;
  if ABytes < 1024 then
  begin
    Result := IntToStr(ABytes) + ' B';
    exit;
  end;
  if ABytes < (1024*1024) then
  begin
    dSize := ABytes / 1024;
    Result := FormatFloat('0.##', dSize) + ' KB';
    exit;
  end;
  if ABytes < (1024*1024*1024) then
  begin
    dSize := ABytes / 1024 / 1024;
    Result := FormatFloat('0.##', dSize) + ' MB';
    exit;
  end;
  if ABytes < (1024*1024*1024*1024) then
  begin
    dSize := ABytes / 1024 / 1024 / 1024;
    Result := FormatFloat('0.##', dSize) + 'GB';
    exit;
  end;
  if ABytes < (1024*1024*1024*1024*1024) then
  begin
    dSize := ABytes / 1024 / 1024 / 1024 / 1024;
    Result := FormatFloat('0.##', dSize) + ' TB';
  end;
end;

procedure TfrmMain.DataReceived(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
begin
  pbMain.Position:= CurrentPos;
  Log(Format('Getting %d of %d', [CurrentPos, FSize]));
end;

end.

