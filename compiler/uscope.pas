unit uscope;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

var
  Scopes: array of record
    Name: string;
    WiderScope: integer;
  end;
  ScopeCount: integer;
  GlobalScope, RunTimeScope: integer;

function NewScope(AWiderScope: integer; AName: string): integer;
function GetWiderScope(AScope: integer): integer;
function GetFullscopeName(AScope: integer; AAppendDot: boolean = false): string;
procedure InitScopes;

implementation

function NewScope(AWiderScope: integer; AName: string): integer;
begin
  if ScopeCount >= length(Scopes) then
    setlength(Scopes, ScopeCount*2+4);
  result := ScopeCount;
  Scopes[result].WiderScope:= AWiderScope;
  Scopes[result].Name:= AName;
  inc(ScopeCount);
end;

function GetWiderScope(AScope: integer): integer;
begin
  if (AScope >= 0) and (AScope < ScopeCount) then
    result := Scopes[AScope].WiderScope
  else
    result := -1;
end;

function GetFullscopeName(AScope: integer; AAppendDot: boolean = false): string;
begin
  result := '';
  while AScope <> -1 do
  begin
    if Scopes[AScope].Name <> '' then
    begin
      if (result <> '') then result := '.' + result;
      result := Scopes[AScope].Name + result;
    end;
    AScope:= GetWiderScope(AScope);
  end;
  if AAppendDot and (result <> '') then result += '.';
end;

procedure InitScopes;
begin
  ScopeCount:= 0;
  GlobalScope := NewScope(-1, '');
  RunTimeScope:= NewScope(-1, 'Runtime');
end;

end.

