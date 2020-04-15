unit uwritetriggers;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl, usctypes, uscope;

procedure CreateTriggers(AMainThread: TPlayer; ASourceCode: string = '');

implementation

uses utriggercode, uarithmetic, ureadprog, uinstructions, uvariables, uparsevb,
  uexpressions, utriggerinstructions, utriggerconditions, utrigedittypes,
  uprocedures;

var
  HyperWaitVar: integer;

procedure ConfigureHyperWait;
begin
  if HyperTriggersOption then
  begin
    if HyperWaitVar = -1 then
    begin
      HyperWaitVar := IntArrayIndexOf(RunTimeScope, 'Hyperwait');
      if HyperWaitVar = -1 then
        HyperWaitVar := CreateIntArray(RunTimeScope,'Hyperwait', MaxTriggerPlayers, [], 24);
    end;
  end;
end;

var
  MessageSysIP: integer;
  GlobalExprTempVarInt: integer;

function GetGlobalExprTempVarInt(ABitCount: integer): integer;
begin
  if GlobalExprTempVarInt = -1 then
    GlobalExprTempVarInt:= AllocateTempInt(ABitCount)
  else
  begin
    if IntVars[GlobalExprTempVarInt].BitCount < ABitCount then
      IntVars[GlobalExprTempVarInt].BitCount := ABitCount;
  end;
  result := GlobalExprTempVarInt;
end;

procedure WriteMessageTriggers(AMainThread: TPlayer);
var
  i: Integer;
  proc: TInstructionList;
  condSysIP, condSysParam: TCondition;

begin
  If MessageSysIP = -1 then exit;
  condSysIP := CheckSysIP( MessageSysIP, AMainThread );
  for i := 0 to MessageCount-1 do
  begin
    proc := TInstructionList.Create;
    proc.Add( TCommentInstruction.Create('Print(' + StrToBasic(Messages[i].Text) + ')') );
    proc.Add( TDisplayTextMessageInstruction.Create(True, Messages[i].Text) );
    condSysParam := CheckSysParam( i, AMainThread );
    WriteProg(Messages[i].Players, [condSysIP, condSysParam], proc, -1, -1, True);
    condSysParam.Free;
    proc.FreeAll;
  end;

  proc := TInstructionList.Create;
  proc.Add( SetNextSysIP(0) );
  WriteProg([AMainThread], [condSysIP], proc, -1,-1, true);
  condSysIP.Free;
  proc.FreeAll;
end;

type

  { TExpandInstructionsContext }

  TExpandInstructionsContext = record
    InProc: integer;
    IsMain: boolean;
    Players: TPlayers;
    EndIP: integer;
    ContinueWhileIP, ExitWhileIP: integer;
    function ChangeEndIP(AEndIP: integer): TExpandInstructionsContext;
  end;


{ TExpandInstructionsContext }

function TExpandInstructionsContext.ChangeEndIP(AEndIP: integer): TExpandInstructionsContext;
begin
  result := self;
  result.EndIP:= AEndIP;
end;

function ExpandInstructionsContext(AInProc: integer; AIsMain: boolean;
  APlayers: TPlayers; AEndIP: integer): TExpandInstructionsContext;
begin
  with result do
  begin
    InProc:= AInProc;
    IsMain:= AIsMain;
    Players:= APlayers;
    EndIP:= AEndIP;
    ContinueWhileIP := -1;
    ExitWhileIP:= -1;
  end;
end;

procedure ExpandInstructions(AProg: TInstructionList;
  AExpanded: TInstructionList; AContext: TExpandInstructionsContext);
var
  currentThreads: TPlayers;

  function AddWaitCondition(AConditions: TConditionList; AWaitIP: integer): TWaitConditionInstruction;
  var
    arithm: TArithmeticCondition;
    tempInt: Integer;
    tempExpand: TInstructionList;
  begin
    if (AConditions.Count = 1) and (AConditions[0] is TArithmeticCondition) then
    begin
      arithm := TArithmeticCondition(AConditions[0]);
      if arithm.Expression.CanPutInAccumulator then
      begin
        tempExpand := TInstructionList.Create;
        arithm.Expression.AddToProgramInAccumulator(tempExpand);
        ExpandInstructions(tempExpand, AExpanded, AContext.ChangeEndIP(-1));
        tempExpand.FreeAll;

        result := TWaitConditionInstruction.Create(CompareAccumulator(arithm.CompareMode,arithm.CompareValue), AWaitIP);
        AExpanded.Add(result);
      end
      else
      begin
        if AContext.InProc = -1 then
          tempInt := GetGlobalExprTempVarInt(arithm.GetBitCount)
        else
          tempInt := GetProcedureExprTempVarInt(AContext.InProc, arithm.GetBitCount);
        tempExpand := TInstructionList.Create;
        arithm.Expression.AddToProgram(tempExpand, IntVars[tempInt].Player,IntVars[tempInt].UnitType, simSetTo);
        ExpandInstructions(tempExpand, AExpanded, AContext.ChangeEndIP(-1));
        tempExpand.FreeAll;
        result := TWaitConditionInstruction.Create(CreateIntegerCondition(IntVars[tempInt].Player,IntVars[tempInt].UnitType, arithm.CompareMode,arithm.CompareValue), AWaitIP);
        AExpanded.Add(result);
      end;
    end else
    if (AConditions.Count = 1) and (AConditions[0] is TCallFunctionCondition) then
    begin
      tempExpand := TInstructionList.Create;
      with TCallFunctionCondition(AConditions[0]) do
        tempExpand.Add(TCallInstruction.Create(Scope, Name, DuplicateParameterValues(ParamValues), 'Boolean'));
      ExpandInstructions(tempExpand, AExpanded, AContext.ChangeEndIP(-1));
      tempExpand.FreeAll;
      result := TWaitConditionInstruction.Create(CompareAccumulator(icmAtLeast,1), AWaitIP);
      AExpanded.Add(result);
    end else
    if (AConditions.Count = 1) and (AConditions[0] is TAndCondition) then
    begin
      result := AddWaitCondition(TAndCondition(AConditions[0]).Conditions, AWaitIP);
    end else
    begin
      if AConditions.IsComputed then
      begin
        if AContext.InProc = -1 then
          tempInt := GetGlobalExprTempVarInt(8)
        else
          tempInt := GetProcedureExprTempVarInt(AContext.InProc, 8);
        tempExpand := TInstructionList.Create;
        AConditions.Compute(tempExpand, IntVars[tempInt].Player,IntVars[tempInt].UnitType);
        ExpandInstructions(tempExpand, AExpanded, AContext.ChangeEndIP(-1));
        tempExpand.FreeAll;
        result := TWaitConditionInstruction.Create(CreateIntegerCondition(IntVars[tempInt].Player,IntVars[tempInt].UnitType, icmAtLeast, 1), AWaitIP);
        AExpanded.Add(result);
      end else
      begin
        result := TWaitConditionInstruction.Create(AConditions.Duplicate, AWaitIP);
        AExpanded.Add(result);
      end;
    end;
  end;

var
  i, expo, j, nextIP, procIdx: Integer;
  rndInstr: TRandomizeIntegerInstruction;
  switches: array of integer;
  call: TCallInstruction;
  nesting, elseIndex: integer;
  startWhileIP, k, varIdx: integer;
  ifInstr: TIfInstruction;
  waitInstr: TWaitConditionInstruction;
  splitInstr: TSplitInstruction;
  thenPart,elsePart: TInstructionList;
  notCond: TNotCondition;
  tempExpand, tempPart: TInstructionList;
  doAs: TDoAsInstruction;
  pl: TPlayer;
  endDoAs: TEndDoAsInstruction;
  endDoIP: integer;
  err: string;
  paramCond: TCondition;
  paramExpr: TExpression;
  accBitInstr: TAccumulatorBitInstruction;
  transf: TTransferIntegerInstruction;
  fastIf: TFastIfInstruction;
  newContext: TExpandInstructionsContext;

begin
  currentThreads := AContext.Players;
  err := '';
  i := -1;
  endDoIP := -1;
  while i < AProg.Count-1 do
  begin
    inc(i);

    if AProg[i] is TFastIfInstruction then
    begin
      fastIf := TFastIfInstruction(AProg[i]);
      tempExpand := TInstructionList.Create;
      ExpandInstructions(fastIf.Instructions, tempExpand, AContext.ChangeEndIP(-1));
      fastIf.Instructions.FreeAll;
      fastIf.Instructions := tempExpand;
      tempExpand := nil;
    end else
    if AProg[i] is TTransferIntegerInstruction then
    begin
      tempExpand := TInstructionList.Create;
      ExpandIntegerTransfer(TTransferIntegerInstruction(AProg[i]), tempExpand);
      ExpandInstructions(tempExpand, AExpanded, AContext.ChangeEndIP(-1));
      tempExpand.FreeAll;
      Continue;
    end else
    if AProg[i] is TPrintForAnyPlayerInstruction then
    begin
       if MessageSysIP = -1 then MessageSysIP := NewSysIP;
       AExpanded.Add( SetNextSysIP( MessageSysIP ) );
       AExpanded.Add( SetSysParam( TPrintForAnyPlayerInstruction(AProg[i]).Msg ) );
       AExpanded.Add( TWaitConditionInstruction.Create( CheckSysIP(0), NewIP ) );
       Continue;
    end else
    if AProg[i] is TWaitForPresenceDefinedInstruction then
    begin
       varIdx := GetPlayerPresenceDefinedVar;
       AExpanded.Add(TWaitConditionInstruction.Create(TSwitchCondition.Create(BoolVars[varIdx].Switch, true), NewIP));
       Continue;
    end else
    if AProg[i] is TDoAsInstruction then
    begin
      doAs := TDoAsInstruction(AProg[i]);
      if doAs.Players - AContext.Players <> [] then
      begin
        varIdx := GetRunEventBoolVar;
        if not AContext.IsMain then
          AExpanded.Add( TSetSwitchInstruction.Create(BoolVars[varIdx].Switch, svClear) );
        AExpanded.Add( TWaitForPlayersInstruction.Create( doAs.Players - AContext.Players ) );
      end;
      nextIP := NewIP;
      for pl := low(TPlayer) to high(TPlayer) do
        if pl in (doAs.Players - AContext.Players) then
          AExpanded.Add( SetNextIP(nextIP, pl));

      endDoIP := NewIP;
      if AContext.Players <= doAs.Players then
        AExpanded.Add( TSplitInstruction.Create(nextIP, nextIP, doAs.Players) )
      else
        AExpanded.Add( TSplitInstruction.Create(nextIP, EndDoIP, doAs.Players) );
      currentThreads := doAs.Players;
      continue;
    end else
    if AProg[i] is TEndDoAsInstruction then
    begin
      endDoAs := TEndDoAsInstruction(AProg[i]);
      if endDoAs.Players - AContext.Players <> [] then
      begin
        AExpanded.Add( TDropThreadInstruction.Create(InitialIP, EndDoIP, endDoAs.Players - AContext.Players, AContext.Players) );
        AExpanded.Add( TWaitForPlayersInstruction.Create( endDoAs.Players - AContext.Players ) );
        if not AContext.IsMain then
          AExpanded.Add( TSetSwitchInstruction.Create(BoolVars[GetRunEventBoolVar].Switch, svSet) );
      end;
      currentThreads := AContext.Players;
      continue;
    end else
    if HyperTriggersOption and (AProg[i] is TWaitInstruction) then
    begin
      ConfigureHyperWait;

      AExpanded.Add(CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[HyperWaitVar].UnitType, simSetTo, TWaitInstruction(AProg[i]).DelayMs));

      startWhileIP:= NewIP;
      AExpanded.Add(TChangeIPInstruction.Create(startWhileIP, 1));

      waitInstr := TWaitConditionInstruction.Create(CreateIntegerCondition(plCurrentPlayer, IntArrays[HyperWaitVar].UnitType, icmAtLeast, 1), NewIP);
      AExpanded.Add(waitInstr);

      AExpanded.Add(CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[HyperWaitVar].UnitType, simSubtract, HYPER_TIME_GRAIN_MS));

      splitInstr := TSplitInstruction.Create(waitInstr.IP, startWhileIP);

      AExpanded.Add(splitInstr);
      AExpanded.Add(TChangeIPInstruction.Create(NewIP, -1));

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
            AExpanded.Add(TChangeIPInstruction.Create(startWhileIP, 1));
            waitInstr := AddWaitCondition(TWhileInstruction(AProg[i]).Conditions, NewIP);

            tempExpand := TInstructionList.Create;
            for k := i+1 to j-1 do
              tempExpand.Add(AProg[k].Duplicate);
            try
              newContext := AContext;
              newContext.ContinueWhileIP:= startWhileIP;
              newContext.ExitWhileIP:= waitInstr.IP;
              newContext.EndIP:= -1;
              ExpandInstructions(tempExpand, AExpanded, newContext);
            finally
              tempExpand.FreeAll;
            end;

            splitInstr := TSplitInstruction.Create(waitInstr.IP, startWhileIP);
            AExpanded.Add(splitInstr);
            i := j;
            break;
          end;
        end;
      end;
      if nesting <> 0 then
      begin
        err := 'The number of End While do not match the number of While';
        break;
      end;
      continue;
    end else
    if AProg[i] is TContinueWhileInstruction then
    begin
      if AContext.ContinueWhileIP = -1 then
      begin
        err := 'Continue While unexpected outside of While loop';
        break;
      end;
      AExpanded.Add( TSplitInstruction.Create(-1, AContext.ContinueWhileIP) );
      continue;
    end else
    if AProg[i] is TExitWhileInstruction then
    begin
      if AContext.ExitWhileIP = -1 then
      begin
        err := 'Exit While unexpected outside of While loop';
        break;
      end;
      AExpanded.Add( TSplitInstruction.Create(-1, AContext.ExitWhileIP) );
      continue;
    end else
    if AProg[i] is TIfInstruction then
    begin
      nesting := 1;
      elseIndex := -1;
      for j := i+1 to AProg.Count-1 do
      begin
        if AProg[j] is TIfInstruction then inc(nesting) else
        if (AProg[j] is TElseInstruction) and (nesting = 1) then
        begin
          if elseIndex = -1 then elseIndex:= j
          else if AProg[elseIndex] is TElseInstruction then
            begin
              err := 'Only one Else is allowed for one If statement';
              break;
            end;
        end else
        if (AProg[j] is TElseIfInstruction) and (nesting = 1) then
        begin
          if elseIndex = -1 then elseIndex := j
          else if AProg[elseIndex] is TElseInstruction then
          begin
            err := 'ElseIf is not allowed after an Else instruction';
            break;
          end;
        end else
        if AProg[j] is TEndIfInstruction then
        begin
          dec(nesting);
          if nesting = 0 then
          begin
            thenPart := TInstructionList.Create;
            elsePart := TInstructionList.Create;

            try
              if elseIndex = -1 then
              begin
                for k := i+1 to j-1 do
                  thenPart.Add(AProg[k].Duplicate);
              end else
              begin
                for k := i+1 to elseIndex-1 do
                  thenPart.Add(AProg[k].Duplicate);
                for k := elseIndex+1 to j-1 do
                  elsePart.Add(AProg[k].Duplicate);

                if AProg[elseIndex] is TElseIfInstruction then
                begin
                  elsePart.Insert(0, TIfInstruction.Create(TElseIfInstruction(AProg[elseIndex]).Conditions.Duplicate));
                  elsePart.Add(TEndIfInstruction.Create);
                end;
              end;

              ifInstr := TIfInstruction(AProg[i]);
              nextIP := NewIP;
              //if condition is a Not, it can be done be swapping Then part and Else part
              if (ifInstr.Conditions.Count = 1) and (ifInstr.Conditions[0] is TNotCondition) then
              begin
                tempPart := thenPart;
                thenPart := elsePart;
                elsePart := tempPart;

                notCond := TNotCondition(ifInstr.Conditions[0]);
                AddWaitCondition(notCond.Conditions, nextIP);
              end
              else
                AddWaitCondition(ifInstr.Conditions, nextIP);

              ExpandInstructions(thenPart, AExpanded, AContext.ChangeEndIP(-1));
              thenPart.FreeAll;
              thenPart := nil;

              splitInstr := TSplitInstruction.Create(nextIP,-1);
              AExpanded.Add(splitInstr);

              if (j = AProg.Count-1) and (AContext.EndIP <> -1) then
              begin
                splitInstr.EndIP:= AContext.EndIP;
                ExpandInstructions(elsePart, AExpanded, AContext);
                elsePart.FreeAll;
                elsePart := nil;
              end
              else
              begin
                nextIP := NewIP;
                splitInstr.EndIP:= nextIP;
                ExpandInstructions(elsePart, AExpanded, AContext.ChangeEndIP(nextIP));
                elsePart.FreeAll;
                elsePart := nil;
                AExpanded.Add(TChangeIPInstruction.Create(nextIP, 0));
              end;

              i := j;

            finally
              if Assigned(thenPart) then thenPart.FreeAll;
              if Assigned(elsePart) then elsePart.FreeAll;
            end;
            break;
          end;
        end;
      end;
      if err<>'' then break;
      if nesting <> 0 then
      begin
        err := 'The number of End If do not match the number of If';
        break;
      end;
      continue;
    end else
    if AProg[i] is TElseInstruction then
    begin
      err := 'Else instruction does not match an If instruction';
      break;
    end else
    if AProg[i] is TEndIfInstruction then
    begin
      err := 'End If instruction does not match an If instruction';
      break;
    end else
    if AProg[i] is TRandomizeIntegerInstruction then
    begin
      rndInstr := TRandomizeIntegerInstruction(AProg[i]);
      AExpanded.Add(CreateSetIntegerInstruction(rndInstr.Player, rndInstr.UnitType, simSetTo, 0));

      //randomize bools
      expo := GetExponentOf2(rndInstr.Range);
      setlength(switches, expo);
      for j := 0 to expo-1 do
      begin
        switches[j] := AllocateTempBool;
        AExpanded.Add( TSetSwitchInstruction.Create(switches[j], svRandomize) );
      end;

      //add powers of 2
      for j := 0 to high(switches) do
        AExpanded.Add( TFastIfInstruction.Create([TSwitchCondition.Create(switches[j], true)],
                      [CreateSetIntegerInstruction(rndInstr.Player, rndInstr.UnitType, simAdd, 1 shl j)]) );

      for j := 0 to expo-1 do
        ReleaseTempBool(switches[j]);

      continue;
    end else
    if AProg[i] is TCallInstruction then
    begin
      call := TCallInstruction(AProg[i]);

      procIdx := ProcedureIndexOf(call.Scope, call.Name, length(call.ParamValues));
      if procIdx = -1 then
      begin
        if CompareText(call.Name,'Main') = 0 then
          err := 'Main cannot be called explicitely'
          else err := 'Procedure not found "' + call.Name + '" with ' + Inttostr(length(call.ParamValues)) + ' parameter(s)';
        break;
      end;

      if not AreThreadsIncluded(currentThreads, Procedures[procIdx].Players) then
        raise exception.Create('The procedure "' + Procedures[procIdx].Name+'" is not available in this thread');

      if (call.ReturnType <> 'Void') and not
      ((IsIntegerType(Procedures[procIdx].ReturnType) and IsIntegerType(call.ReturnType))
      or (Procedures[procIdx].ReturnType = call.ReturnType)) then
      begin
        err := 'Expecting ' + call.ReturnType + ' return type but ' + Procedures[procIdx].ReturnType + ' found';
        break;
      end;

      if Procedures[procIdx].StartIP = -1 then Procedures[procIdx].StartIP:= NewIP;

      If AContext.InProc <> -1 then
      begin
        if Procedures[AContext.InProc].Calls.IndexOf(procIdx)=-1 then
          Procedures[AContext.InProc].Calls.Add(procIdx);
      end;

      tempPart := TInstructionList.Create;
      for j := 0 to high(call.ParamValues) do
      begin
        if Assigned(call.ParamValues[j].Condition) then
        begin
          paramCond := call.ParamValues[j].Condition as TCondition;
          varIdx := BoolVarIndexOf(Procedures[procIdx].InnerScope, Procedures[procIdx].Params[j].Name, false);
          if varIdx = -1 then raise exception.Create('Cannot find parameter variable "'+Procedures[procIdx].Params[j].Name+'"');
          if paramCond is TAlwaysCondition then
            tempPart.Add(TSetSwitchInstruction.Create(BoolVars[varIdx].Switch, svSet))
          else if paramCond is TNeverCondition then
            tempPart.Add(TSetSwitchInstruction.Create(BoolVars[varIdx].Switch, svClear))
          else
          begin
            tempPart.Add(TIfInstruction.Create(paramCond.Duplicate));
            tempPart.Add(TSetSwitchInstruction.Create(BoolVars[varIdx].Switch, svSet));
            tempPart.Add(TElseInstruction.Create);
            tempPart.Add(TSetSwitchInstruction.Create(BoolVars[varIdx].Switch, svClear));
            tempPart.Add(TEndIfInstruction.Create);
          end;
        end else
        if Assigned(call.ParamValues[j].Expression) then
        begin
          paramExpr := call.ParamValues[j].Expression as TExpression;
          varIdx := IntVarIndexOf(Procedures[procIdx].InnerScope, Procedures[procIdx].Params[j].Name, false);
          if varIdx = -1 then raise exception.Create('Cannot find parameter variable "'+Procedures[procIdx].Params[j].Name+'"');
          if paramExpr.IsConstant then
          begin
            tempPart.Add(CreateSetIntegerInstruction(IntVars[varIdx].Player, IntVars[varIdx].UnitType, simSetTo, paramExpr.ConstElement));
          end else
            paramExpr.AddToProgram(tempPart, IntVars[varIdx].Player, IntVars[varIdx].UnitType, simSetTo);
        end;
      end;
      ExpandInstructions(tempPart, AExpanded, AContext.ChangeEndIP(-1));
      tempPart.FreeAll;

      nextIP := NewIP;
      AddSysCall(AExpanded, nextIP, Procedures[procIdx].StartIP);

      continue;
    end else
    if AProg[i] is TReturnInstruction then
    begin
      if AContext.InProc<>-1 then AddSysReturn(AExpanded)
      else
        AExpanded.Add( TSplitInstruction.Create(-1, -1));
      continue;
    end else
    if AProg[i] is TAccumulatorBitInstruction then
    begin
      accBitInstr := TAccumulatorBitInstruction(AProg[i]);
      tempPart := TInstructionList.Create;
      for j := accBitInstr.Instructions.Count-1 downto 0 do
      begin
        tempExpand := TInstructionList.Create;
        tempExpand.Add( accBitInstr.Instructions[j].Duplicate );
        transf := TTransferIntegerInstruction.Create(1 shl j, itSubtractIntoAccumulator);
        ExpandIntegerTransfer(transf, tempExpand);
        transf.Free;
        tempPart.Add( TFastIfInstruction.Create( [CompareAccumulator( icmAtLeast, 1 shl j)],
                                                  tempExpand) );
      end;
      ExpandInstructions(tempPart, AExpanded, AContext.ChangeEndIP(-1));
      tempPart.FreeAll;
      continue;
    end;
    AExpanded.Add(AProg[i].Duplicate);
  end;

  if err <> '' then
    raise exception.Create(err);
end;

procedure WriteHyperTriggers;
var
  actions: array of TInstruction;
  i: Integer;
begin
  if not HyperTriggersOption then exit;

  setlength(actions,63);
  actions[0] := TCommentInstruction.Create('Hyper triggers');
  for i := 1 to high(actions) do
    actions[i] := TWaitInstruction.Create(15);

  for i := 1 to 4 do
    WriteTrigger([plAllPlayers], [], actions, -1, true);
  for i := 0 to high(actions) do actions[i].Free;
end;

procedure WritePlayerPresenceTopTrigger({%H-}AMainThread: TPlayer; AMainExpanded: TInstructionList; AProceduresExpanded: array of TInstructionList);

  procedure CheckProcedure(AProc: TInstructionList);
  var
    j: Integer;
    pl: TPlayer;
  begin
    if AProc = nil then exit;
    for j := 0 to AProc.Count-1 do
      if AProc[j] is TWaitForPlayersInstruction then
      begin
        for pl := plPlayer1 to plPlayer8 do
          if pl in TWaitForPlayersInstruction(AProc[j]).Players then
            GetPlayerPresenceBoolVar(pl);
      end;
  end;

var
  proc: TInstructionList;
  pl: TPlayer;
  i: Integer;

begin
  //at the begining of the cycle, set presence
  CheckProcedure(AMainExpanded);
  for i := 0 to high(AProceduresExpanded) do
    CheckProcedure(AProceduresExpanded[i]);
  for pl := plPlayer1 to plPlayer8 do
    if IsPlayerPresenceUsed(pl) then
    begin
      proc := TInstructionList.Create;
      proc.Add(TCommentInstruction.Create(PlayerIdentifiers[pl]+'.Present = True'));
      proc.Add(TSetSwitchInstruction.Create(BoolVars[GetPlayerPresenceBoolVar(pl)].Switch, svSet));
      WriteProg([pl], [], proc, -1, -1, true);
      proc.FreeAll;
    end;
end;

procedure WritePlayerPresenceBottomTrigger(AMainThread: TPlayer);
var
  proc: TInstructionList;
  pl: TPlayer;
  switch: Integer;
  cond: TSwitchCondition;
  setIP: TTriggerInstruction;
  comment: TCommentInstruction;
begin
  //at the end of main thread cycle, clear presence flags (they will be set until next main thread cycle)
  proc := TInstructionList.Create;
  for pl := plPlayer1 to plPlayer8 do
    if IsPlayerPresenceUsed(pl) then
    begin
      if not IPUnused then
      begin
        switch := BoolVars[GetPlayerPresenceBoolVar(pl)].Switch;
        cond := TSwitchCondition.Create(switch, False);
        setIP := SetNextIP(InitialIP, pl);
        comment := TCommentInstruction.Create('Reset IP');
        WriteTrigger([AMainThread], [cond], [comment, setIP], -1, true);
        setIP.Free;
        cond.Free;
        comment.Free;
      end;
      proc.Add(TSetSwitchInstruction.Create(switch, svClear));
    end;
  if proc.Count > 0 then
  begin
    proc.Insert(0, TCommentInstruction.Create('Reset presence'));
    proc.Add(TSetSwitchInstruction.Create(BoolVars[GetPlayerPresenceDefinedVar].Switch, svSet)); //will be until next cycle
    WriteProg([AMainThread], [], proc, -1, -1, true);
  end;
  proc.FreeAll;
end;

procedure DecreaseTimers(AMainThread: TPlayer);
var
  i, timeGrain, arrIdx, j: Integer;
  proc: TInstructionList;
  pl: TPlayer;
begin
  if HyperTriggersOption then
    timeGrain := HYPER_TIME_GRAIN_MS
    else timeGrain := NORMAL_TIME_GRAIN_MS;
  proc := TInstructionList.Create;
  for i := 0 to IntVarCount-1 do
  if IntVars[i].IsTimer then
  begin
    if IntVars[i].Player = plCurrentPlayer then
    begin
      arrIdx := IntVars[i].IntArray;
      if (arrIdx <> -1) and IntArrays[arrIdx].Multithread then
      begin
        for j := 1 to IntArrays[arrIdx].Size do
        begin
          pl := TPlayer(ord(plPlayer1)+j-1);
          proc.Add( CreateSetIntegerInstruction(pl, IntVars[i].UnitType,
                                          simSubtract, timeGrain) )
        end;
      end;
    end else
      proc.Add( CreateSetIntegerInstruction(IntVars[i].Player,
                    IntVars[i].UnitType, simSubtract, timeGrain) )
  end;
  WriteProg([AMainThread], [], proc, -1, -1, true);
  proc.FreeAll;
end;

procedure WriteTriggerForSourceCode(ASourceCode: string);
var
  never: TNeverCondition;
begin
  never := TNeverCondition.Create;
  WriteTrigger([plNeutralPlayers], [never], [TCommentInstruction.Create('BroodBasic source code'), TCommentInstruction.Create(ASourceCode)], -1, false);
  never.Free;
end;

procedure CreateTriggers(AMainThread: TPlayer; ASourceCode: string = '');
var
  i, EndIP, tempInt, j: Integer;
  allProcDone, mainEmpty: Boolean;
  players, allEventPlayers: TPlayers;
  noSysIP: TCondition;
  computeConditionProc, computeConditionProcExpanded : TInstructionList;
  computedConds, nonComputedConds: TConditionList;
  tempCond: TCondition;
  MainProgExpanded: TInstructionList;
  EventsExpanded, ProceduresExpanded: array of TInstructionList;
  ctx: TExpandInstructionsContext;

begin
  InitTriggerCode;
  InitArithmetic;

  HyperWaitVar := -1;
  MessageSysIP := -1;
  GlobalExprTempVarInt:= -1;

  mainEmpty := true;
  for i := 0 to MainProg.Count-1 do
    if not (MainProg[i] is TCommentInstruction) then
      mainEmpty := false;
  EndIP := InitialIP;

  MainProgExpanded := nil;
  setlength(EventsExpanded, EventCount);
  setlength(ProceduresExpanded, ProcedureCount);
  allEventPlayers:= [];

  try
    MainProgExpanded := TInstructionList.Create;
    if not mainEmpty then
    begin
      ctx := ExpandInstructionsContext(-1, true, [AMainThread], EndIP);
      ExpandInstructions(MainProg, MainProgExpanded, ctx);
    end;

    for i := 0 to EventCount-1 do
    begin
      ctx := ExpandInstructionsContext(-1, false, Events[i].Players, EndIP);
      EventsExpanded[i] := TInstructionList.Create;
      ExpandInstructions(Events[i].Instructions, EventsExpanded[i], ctx);
    end;

    repeat
      allProcDone := true;
      for i := 0 to ProcedureCount-1 do
        if (Procedures[i].StartIP <> -1) and not Procedures[i].Expanded then
        begin
          allProcDone:= false;
          ctx := ExpandInstructionsContext(i, false, Procedures[i].Players, -1);
          ProceduresExpanded[i] := TInstructionList.Create;
          ExpandInstructions(Procedures[i].Instructions, ProceduresExpanded[i], ctx);
          Procedures[i].Expanded := true;
        end;
    until allProcDone;

    if ASourceCode <> '' then
      WriteTriggerForSourceCode(ASourceCode);

    WritePlayerPresenceTopTrigger(AMainThread, MainProgExpanded, ProceduresExpanded);
    WriteMessageTriggers(AMainThread);

    if not mainEmpty or RunEventBoolVarUsed then
    begin
      MainProgExpanded.Insert(0, TCommentInstruction.Create('Sub Main'));
      if RunEventBoolVarUsed then
        MainProgExpanded.Add(TSetSwitchInstruction.Create(BoolVars[GetRunEventBoolVar].Switch, svSet));
      WriteProg([AMainThread], [], MainProgExpanded, -1, EndIP, False);
    end;

    if ProcedureCount > 0 then
    begin
      noSysIP := CheckSysIP(0);
      for i := 0 to ProcedureCount-1 do
        if Procedures[i].StartIP <> -1 then
        begin
          if Procedures[i].ReturnType <> 'Void' then
            ProceduresExpanded[i].Insert(0, TCommentInstruction.Create('Function '+Procedures[i].Name+' As '+Procedures[i].ReturnType))
            else ProceduresExpanded[i].Insert(0, TCommentInstruction.Create('Sub '+Procedures[i].Name));
          WriteProg(Procedures[i].Players, [noSysIP], ProceduresExpanded[i], Procedures[i].StartIP, EndIP, true);
        end;
      noSysIP.Free;
    end;

    for i := 0 to EventCount-1 do
    begin
      players := Events[i].Players;
      if players = [] then players := [AMainThread];
      if (players <> [AMainThread]) and RunEventBoolVarUsed then
        Events[i].Conditions.Add(TSwitchCondition.Create(BoolVars[GetRunEventBoolVar].Switch,True));
      allEventPlayers += players;

      if Events[i].Conditions.IsComputed then
      begin
        tempInt := GetGlobalExprTempVarInt(8);
        nonComputedConds := TConditionList.Create;
        try
          computeConditionProc := TInstructionList.Create;
          computeConditionProcExpanded := TInstructionList.Create;
          try
            computedConds := TConditionList.Create;
            try
              for j := 0 to Events[i].Conditions.Count-1 do
                if Events[i].Conditions[j].IsComputed then
                  computedConds.Add(Events[i].Conditions[j])
                else
                  nonComputedConds.Add(Events[i].Conditions[j]);
              computedConds.Compute(computeConditionProc, IntVars[tempInt].Player, IntVars[tempInt].UnitType);
              ctx := ExpandInstructionsContext(-1, false, players, EndIP);
              ExpandInstructions(computeConditionProc, computeConditionProcExpanded, ctx);
              computeConditionProcExpanded.Insert(0, TCommentInstruction.Create('On '+ Events[i].Conditions.ToBasic(True)));
              if (players <> [AMainThread]) and RunEventBoolVarUsed then
              begin
                tempCond := TSwitchCondition.Create(BoolVars[GetRunEventBoolVar].Switch,True);
                WriteProg(players, [tempCond], computeConditionProcExpanded, EndIP, EndIP, Events[i].Preserve);
                tempCond.Free;
              end else
              begin
                WriteProg(players, [], computeConditionProcExpanded, EndIP, EndIP, Events[i].Preserve);
              end;
            finally
              computedConds.Free;
            end;
          finally
            computeConditionProcExpanded.FreeAll;
            computeConditionProc.FreeAll;
          end;
          tempCond := CreateIntegerCondition(IntVars[tempInt].Player, IntVars[tempInt].UnitType, icmAtLeast, 1);
          nonComputedConds.Add(tempCond);
          WriteProg(players, nonComputedConds, EventsExpanded[i], EndIP, EndIP, Events[i].Preserve);
          tempCond.Free;
        finally
          nonComputedConds.Free;
        end;
      end else
      begin
        EventsExpanded[i].Insert(0, TCommentInstruction.Create('On '+ Events[i].Conditions.ToBasic(True)));
        WriteProg(players, Events[i].Conditions, EventsExpanded[i], EndIP, EndIP, Events[i].Preserve);
      end;
    end;

    //write generated code at the end of the file
    WriteStackTriggers;
    WriteArithmeticTriggers;
    RemoveIPIfUnused;
    WritePlayerPresenceBottomTrigger(AMainThread);
    DecreaseTimers(AMainThread);

    //it is recommended to put hyper CompiledTriggers at the end
    WriteHyperTriggers;

  finally
    MainProgExpanded.FreeAll;
    for i := 0 to high(EventsExpanded) do
      EventsExpanded[i].FreeAll;
    for i := 0 to high(ProceduresExpanded) do
      ProceduresExpanded[i].FreeAll;
  end;

  if CompiledTriggersMultiCount > MAX_TRIGGER_COUNT then
    raise exception.Create('Too many triggers');
end;

end.

