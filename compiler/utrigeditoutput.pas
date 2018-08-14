unit utrigeditoutput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure WriteTrigEditCode(AFilename: string);
procedure WriteTrigEditUnitProperties(AFilename: string);

implementation

uses uvariables, usctypes, utrigedittypes, utriggercode;

procedure WriteTrigEditCode(AFilename: string);
var
  t: TextFile;
  i, j: Integer;
begin
  AssignFile(t, AFilename);
  Rewrite(t);
  for i := 0 to IntArrayCount-1 do
    if not IntArrays[i].Predefined and not IntArrays[i].Constant then
    begin
      writeln(t, '// ', IntArrays[i].Name, '(1 to '+ intToStr(MaxTriggerPlayers)+') stored in "', StarcraftUnitTrigEditNames[IntArrays[i].UnitType], '" //');
      for j := 1 to IntArrays[i].Size do
        with IntVars[IntArrays[i].Vars[j-1]] do
          if UnitType <> IntArrays[i].UnitType then
            writeln(t, '// ', IntArrays[i].Name, '('+ intToStr(j)+') stored in "', StarcraftUnitTrigEditNames[UnitType], '" of "', PlayerToTrigEditStr(Player),'" //');
    end;
  for i := 0 to IntVarCount-1 do
    if not IntVars[i].Predefined and not IntVars[i].Constant then
      writeln(t, '// ', IntVars[i].Name, ' stored in "', IntVars[i].UnitType, '" of "', PlayerToTrigEditStr(IntVars[i].Player),'" //');
  for i := 0 to BoolArrayCount-1 do
    if not BoolArrays[i].Constant then
      writeln(t, '// ', BoolArrays[i].Name, '(1 to '+inttostr(BoolArrays[i].Size)+') stored in ' +
      '"Switch', BoolVars[BoolArrays[i].Vars[0]].Switch, '" to '+
      '"Switch', BoolVars[BoolArrays[i].Vars[BoolArrays[i].Size-1]].Switch,'" //');
  for i := 0 to BoolVarCount-1 do
    if not BoolVars[i].Constant and (BoolVars[i].BoolArray = -1) then
      writeln(t, '// ', BoolVars[i].Name, ' stored in "Switch', BoolVars[i].Switch, '" //');
  writeln(t);
  for i := 0 to Triggers.Count-1 do
  begin
    writeln(t,Triggers[i].ToTrigEdit);
    writeln(t);
  end;
  CloseFile(t);
end;

procedure WriteTrigEditUnitProperties(AFilename: string);
var
  t: TextFile;
  i: Integer;
begin
  AssignFile(t, AFilename);
  Rewrite(t);
  for i := 0 to UnitPropCount-1 do
  with UnitPropVars[i].Value do
  begin
    writeln(t,'Unit Property ' + inttostr(i) + ' // ' + UnitPropVars[i].Name);
    writeln(t);
    writeln(t,'HP: ' + inttostr(Life));
    writeln(t,'SP: ' + inttostr(Shield));
    writeln(t,'EP: ' + inttostr(Energy));
    writeln(t,'Res: ' + inttostr(Resource));
    writeln(t,'Hangar Units: ' + inttostr(HangarCount));
    write(t,'Flags: ');
    if Hallucinated then write(t,'HALLUCINATED');
    if Invincible then write(t,'INVINCIBLE');
    if Burrowed then write(t,'BURROWED');
    if Lifted then write(t,'INTRANSIT');
    if Cloaked then write(t,'CLOAKED');
    writeln(t);
    writeln(t);
  end;
  CloseFile(t);
end;

end.

