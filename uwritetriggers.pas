unit uwritetriggers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, usctypes;

procedure WriteTriggers(AFilename: string; AMainThread: TPlayer);
procedure WriteUnitProperties(AFilename: string);

implementation

uses utriggercode, uarithmetic, ureadprog, uinstructions, uvariables;

var
  HyperWaitVar: integer;

procedure ConfigureHyperWait;
begin
  if HyperTriggers then
  begin
    if HyperWaitVar = -1 then
    begin
      HyperWaitVar := IntArrayIndexOf('_hyperwait');
      if HyperWaitVar = -1 then
        HyperWaitVar := CreateIntArray('_hyperwait', MaxTriggerPlayers, []);
    end;
  end;
end;

procedure ExpandInstructions(AProg: TInstructionList; AInProc: integer; APlayers: TPlayers);
var
  i, expo, j, nextIP, procIdx: Integer;
  sdi: TSetIntegerInstruction;
  expanded: TInstructionList;
  switches: array of integer;
  call: TCallInstruction;
  ret: TReturnInstruction;
  nesting, elseIndex, endIfIndex: integer;
  startWhileIP, msgIdx, k: integer;
  ifInstr: TIfInstruction;
  waitInstr: TWaitConditionInstruction;
  splitInstr: TSplitInstruction;
  thenPart,elsePart: TInstructionList;
  notCond: TNotCondition;

begin
  expanded := TInstructionList.Create;
  i := -1;
  while i < AProg.Count-1 do
  begin
    inc(i);

    if AProg[i] is TTransferIntegerInstruction then
    begin
      ExpandIntegerTransfer(TTransferIntegerInstruction(AProg[i]), expanded);
      AProg[i].Free;
      Continue;
    end else
    if HyperTriggers and (AProg[i] is TWaitInstruction) then
    begin
      ConfigureHyperWait;

      expanded.Add(TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[HyperWaitVar].UnitType, simSetTo, TWaitInstruction(AProg[i]).DelayMs));
      AProg[i].Free;

      startWhileIP:= NewIP;
      expanded.Add(TChangeIPInstruction.Create(startWhileIP, 1));

      waitInstr := TWaitConditionInstruction.Create(TIntegerCondition.Create(plCurrentPlayer, IntArrays[HyperWaitVar].UnitType, icmAtLeast, 1), NewIP);
      expanded.Add(waitInstr);

      expanded.Add(TSetIntegerInstruction.Create(plCurrentPlayer, IntArrays[HyperWaitVar].UnitType, simSubtract, 140));

      splitInstr := TSplitInstruction.Create(waitInstr.IP, startWhileIP);

      expanded.Add(splitInstr);
      expanded.Add(TChangeIPInstruction.Create(NewIP, -1));

      continue;
    end else
    if AProg[i] is TWhileInstruction then
    begin
      nesting := 1;
      for j := i+1 to AProg.Count-1 do
      begin
        if AProg[j] is TWhileInstruction then inc(nesting) else
        if AProg[j] is TEndWhileInstruction then
        begin
          dec(nesting);
          if nesting = 0 then
          begin
            startWhileIP:= NewIP;
            expanded.Add(TChangeIPInstruction.Create(startWhileIP, 1));
            waitInstr := TWaitConditionInstruction.Create(TWhileInstruction(AProg[i]).Conditions, NewIP);
            TWhileInstruction(AProg[i]).Conditions := nil;
            expanded.Add(waitInstr);

            AProg[j].Free;
            splitInstr := TSplitInstruction.Create(waitInstr.IP, startWhileIP);
            AProg[j] := splitInstr;

            AProg.Insert(j+1, TChangeIPInstruction.Create(NewIP, -1) );
            break;
          end;
        end;
      end;
      if nesting <> 0 then raise exception.Create('The number of End While do not match the number of While');
      continue;
    end else
    if AProg[i] is TIfInstruction then
    begin
      nesting := 1;
      elseIndex := -1;
      for j := i+1 to AProg.Count-1 do
      begin
        if AProg[j] is TIfInstruction then inc(nesting) else
        if (AProg[j] is TElseInstruction) and (nesting = 1) then elseIndex:= j else
        if AProg[j] is TEndIfInstruction then
        begin
          dec(nesting);
          if nesting = 0 then
          begin
            if elseIndex = -1 then
            begin
              splitInstr := TSplitInstruction.Create(-1,-1);
              AProg.Insert(j, splitInstr);
              elseIndex := j;
              endIfIndex := j+1;
            end else
            begin
              AProg[elseIndex].Free;
              splitInstr := TSplitInstruction.Create(-1,-1);
              AProg[elseIndex] := splitInstr;
              endIfIndex := j;
            end;

            ifInstr := TIfInstruction(AProg[i]);
            //if condition is a Not, it can be done be swapping Then part and Else part
            if (ifInstr.Conditions.Count = 1) and (ifInstr.Conditions[0] is TNotCondition) then
            begin
              notCond := TNotCondition(ifInstr.Conditions[0]);
              ifInstr.Conditions := notCond.Conditions;
              notCond.Conditions := TConditionList.Create;
              notCond.Free;

              thenPart := TInstructionList.Create;
              for k := i+1 to elseIndex-1 do thenPart.Add(AProg[k]);
              elsePart := TInstructionList.Create;
              for k := elseIndex+1 to endIfIndex-1 do elsePart.Add(AProg[k]);
              elseIndex := i+1+elsePart.Count;
              for k := 0 to elsePart.Count-1 do AProg[i+1+k] := elsePart[k];
              AProg[elseIndex] := splitInstr;
              for k := 0 to thenPart.Count-1 do AProg[elseIndex+1+k] := thenPart[k];
              thenPart.Free;
              elsePart.Free;
            end;

            nextIP := NewIP;
            expanded.Add(TWaitConditionInstruction.Create(ifInstr.Conditions, nextIP));
            ifInstr.Conditions := nil;
            ifInstr.Free;

            TSplitInstruction(AProg[elseIndex]).ResumeIP:= nextIP;

            nextIP := NewIP;
            TSplitInstruction(AProg[elseIndex]).EndIP:= nextIP;
            AProg[endIfIndex].Free;
            AProg[endIfIndex] := TChangeIPInstruction.Create(nextIP, 0);
            break;
          end;
        end;
      end;
      if nesting <> 0 then raise exception.Create('The number of End If do not match the number of If');
      continue;
    end else
    if AProg[i] is TElseInstruction then
    begin
      raise exception.Create('Else instruction does not match an If instruction');
    end else
    if AProg[i] is TEndIfInstruction then
    begin
      raise exception.Create('End If instruction does not match an If instruction');
    end else
    if AProg[i] is TSetIntegerInstruction then
    begin
      sdi := TSetIntegerInstruction(AProg[i]);
      if sdi.Mode = simRandomize then
      begin
        expanded.Add(TSetIntegerInstruction.Create(sdi.Player, sdi.UnitType, simSetTo, 0));

        //insert randomize instruction
        expo := GetExponentOf2(sdi.Value);
        NeedTempBools(expo);
        setlength(switches, expo);
        for j := 0 to expo-1 do
        begin
          switches[j] := BoolVars[TempBools[j]].Switch;
          expanded.Add( TSetSwitchInstruction.Create(switches[j], svRandomize) );
        end;

        expanded.Add( TAddIntegerFromSwitchesInstruction.Create(sdi.Player, sdi.UnitType, switches) );

        sdi.Free;
        continue;
      end;
    end else
    if AProg[i] is TCallInstruction then
    begin
      call := TCallInstruction(AProg[i]);

      procIdx := ProcedureIndexOf(call.Name, length(call.Params));
      if procIdx = -1 then
        raise Exception.Create('Procedure not found "' + call.Name + '" with ' + Inttostr(length(call.Params)) + ' parameter(s)');

      if (call.ReturnType <> 'Void') and (Procedures[procIdx].ReturnType <> call.ReturnType) then
        raise exception.Create('Expecting ' + call.ReturnType + ' return type but ' + Procedures[procIdx].ReturnType + ' found');

      if Procedures[procIdx].StartIP = -1 then Procedures[procIdx].StartIP:= NewIP;

      If AInProc <> -1 then
      begin
        if Procedures[AInProc].Calls.IndexOf(procIdx)=-1 then
          Procedures[AInProc].Calls.Add(procIdx);
      end;

      nextIP := NewIP;
      AddSysCall(expanded, nextIP, Procedures[procIdx].StartIP);

      call.Free;
      continue;
    end else
    if AProg[i] is TReturnInstruction then
    begin
      ret := TReturnInstruction(AProg[i]);
      if AInProc<>-1 then AddSysReturn(expanded)
      else
        expanded.Add( TSplitInstruction.Create(NewIP, -1));
      ret.Free;
      continue;
    end;
    expanded.Add(AProg[i]);
  end;
  AProg.Clear;
  for i := 0 to expanded.Count-1 do
    AProg.Add(expanded[i]);
  expanded.Free;
end;

procedure WriteFile(AFilename: string; ALines: TStringList);
var
  t: TextFile;
  i, j: Integer;
begin
  AssignFile(t, AFilename);
  Rewrite(t);
  for i := 0 to IntArrayCount-1 do
    if not IntArrays[i].Predefined and not IntArrays[i].Constant then
    begin
      writeln(t, '// ', IntArrays[i].Name, '(1 to '+ intToStr(MaxTriggerPlayers)+') stored in "', IntArrays[i].UnitType, '" //');
      for j := 1 to IntArrays[i].Size do
        with IntVars[IntArrays[i].Vars[j-1]] do
          if UnitType <> IntArrays[i].UnitType then
            writeln(t, '// ', IntArrays[i].Name, '('+ intToStr(j)+') stored in "', UnitType, '" of "', PlayerToStr(Player),'" //');
    end;
  for i := 0 to IntVarCount-1 do
    if not IntVars[i].Predefined and not IntVars[i].Constant then
      writeln(t, '// ', IntVars[i].Name, ' stored in "', IntVars[i].UnitType, '" of "', PlayerToStr(IntVars[i].Player),'" //');
  for i := 0 to BoolArrayCount-1 do
    if not BoolArrays[i].Constant then
      writeln(t, '// ', BoolArrays[i].Name, '(1 to '+inttostr(BoolArrays[i].Size)+') stored in ' +
      '"Switch', BoolVars[BoolArrays[i].Vars[0]].Switch, '" to '+
      '"Switch', BoolVars[BoolArrays[i].Vars[BoolArrays[i].Size-1]].Switch,'" //');
  for i := 0 to BoolVarCount-1 do
    if not BoolVars[i].Constant and (BoolVars[i].BoolArray = -1) then
      writeln(t, '// ', BoolVars[i].Name, ' stored in "Switch', BoolVars[i].Switch, '" //');
  writeln(t);
  for i := 0 to ALines.Count-1 do
    writeln(t,ALines[i]);
  CloseFile(t);
end;

procedure WriteHyperTriggers(AOutput: TStringList);
var
  wait: TWaitInstruction;
  actions: array of string;
  i: Integer;
begin
  if not HyperTriggers then exit;

  AOutput.Add('// Hyper Triggers //');

  wait := TWaitInstruction.Create(15);

  setlength(actions,63);
  for i := 0 to high(actions) do
    actions[i] := wait.tostring;

  for i := 1 to 4 do
    WriteTrigger(AOutput, [plAllPlayers], [], actions, -1, true);

  wait.Free;
end;

procedure WriteTriggers(AFilename: string; AMainThread: TPlayer);
var
  i, j, EndIP: Integer;
  initSub, combineProg: TInstructionList;
  mainOutput: TStringList;
  allProcDone: Boolean;
  players: TPlayers;
  noSysIP: TCondition;

begin
  InitTriggerCode;
  InitArithmetic;

  HyperWaitVar := -1;

  initSub := TInstructionList.Create;
  for i := 0 to IntArrayCount-1 do
  with IntArrays[i] do
  if not Constant then
  begin
    for j := 1 to Size do
      if Values[j-1] <> 0 then
      with IntVars[Vars[j-1]] do
        initSub.Add(TSetIntegerInstruction.Create(Player, UnitType, simSetTo, Values[j-1]))
  end;

  for i := 0 to IntVarCount-1 do
    if (IntVars[i].Value <> 0) and not IntVars[i].Constant and not IntVars[i].Predefined then
    begin
      if IntVars[i].Randomize then
        initSub.Add(TSetIntegerInstruction.Create(IntVars[i].Player, IntVars[i].UnitType, simRandomize, IntVars[i].Value))
      else
        initSub.Add(TSetIntegerInstruction.Create(IntVars[i].Player, IntVars[i].UnitType, simSetTo, IntVars[i].Value));
    end;

  for i := 0 to BoolVarCount-1 do
    if (BoolVars[i].Value in [svSet,svRandomize]) and not BoolVars[i].Constant then
      initSub.Add(TSetSwitchInstruction.Create(BoolVars[i].Switch, BoolVars[i].Value));

  mainOutput := TStringList.Create;

  EndIP := 0;

  ExpandInstructions(initSub, -1, [AMainThread]);
  ExpandInstructions(MainProg, -1, [AMainThread]);

  for i := 0 to EventCount-1 do
    ExpandInstructions(Events[i].Instructions, -1, [AMainThread]);

  repeat
    allProcDone := true;
    for i := 0 to ProcedureCount-1 do
      if (Procedures[i].StartIP <> -1) and not Procedures[i].Done then
      begin
        allProcDone:= false;

        ExpandInstructions(Procedures[i].Instructions, i, [AMainThread]);
        Procedures[i].Done := true;
      end;
  until allProcDone;

  mainOutput.Add('// Program //');

  if initSub.Count > 0 then
  begin
    combineProg := TInstructionList.Create;
    for i := 0 to initSub.Count-1 do
      combineProg.Add(initSub[i]);
    for i := 0 to MainProg.Count-1 do
      combineProg.Add(MainProg[i]);

    WriteProg(mainOutput, [AMainThread], [], combineProg, -1, EndIP, False);

    combineProg.Free;

    for i := 0 to initSub.Count-1 do
      initSub[i].Free;
  end else
    WriteProg(mainOutput, [AMainThread], [], MainProg, -1, EndIP, False);

  initSub.Free;

  noSysIP := CheckSysIP(0);
  for i := 0 to ProcedureCount-1 do
    if Procedures[i].StartIP <> -1 then
    begin
      mainOutput.Add('// Sub ' + Procedures[i].Name + ' //');
      WriteProg(mainOutput, [plAllPlayers], [noSysIP], Procedures[i].Instructions, Procedures[i].StartIP, EndIP, true);
    end;
  noSysIP.Free;

  for i := 0 to EventCount-1 do
  begin
    mainOutput.Add('// When //');
    players := Events[i].Players;
    if players = [] then players := [AMainThread];
    WriteProg(mainOutput, players, Events[i].Conditions, Events[i].Instructions, EndIP, EndIP, Events[i].Preserve);
  end;

  //write generated code at the end of the file
  WriteStackTriggers(mainOutput);
  WriteArithmeticTriggers(mainOutput);

  //it is recommended to put hyper triggers at the end
  WriteHyperTriggers(mainOutput);

  WriteFile(AFilename, mainOutput);
  mainOutput.Free;
end;

procedure WriteUnitProperties(AFilename: string);
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

