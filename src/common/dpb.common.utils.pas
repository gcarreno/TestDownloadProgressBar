unit DPB.Common.Utils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils
;

function FormatBytes(ABytes: Int64): String;

implementation

function FormatBytes(ABytes: Int64): String;
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

end.

