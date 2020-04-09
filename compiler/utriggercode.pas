unit utriggercode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, uinstructions, utriggerinstructions, utriggerconditions, usctypes,
  utriggerchunk, uscope;

const
  MaxStackSize = 6;
  MAX_TRIGGER_COUNT = 65534;

type
  TTriggerConditionList = specialize TFPGObjectList<TTriggerCondition>;
  TTriggerInstructionList = specialize TFPGObjectList<TTriggerInstruction>;

  { TTrigger }

  TTrigger = class
  private
    function GetAction(AIndex: integer): TTriggerInstruction;
    function GetActionCount: integer;
    function GetCondition(AIndex: integer): TTriggerCondition;
    function GetConditionCount: integer;
    function GetIsMultithread: boolean;
    function GetThreadCount: integer;
    procedure SetPreserve(AValue: boolean);
  protected
    FPlayers: TPlayers;
    FConditions: TTriggerConditionList;
    FActions: TTriggerInstructionList;
    FPreserve: boolean;
  public
    constructor Create;
    constructor Create(APlayers: TPlayers);
    destructor Destroy; override;
    procedure Clear;
    procedure AddCondition(ACondition: TCondition);
    procedure DeleteCondition(AIndex: integer);
    procedure AddAction(AInstruction: TInstruction);
    procedure DeleteAction(AIndex: integer);
    function ToTrigEdit: string;
    function ToTrigEditFor(APlayer: TPlayer): string;
    procedure WriteTriggerData(var AData: TTriggerData);
    procedure WriteTriggerDataFor(APlayer: TPlayer; var AData: TTriggerData);
    procedure ReadTriggerData(const AData: TTriggerData);
    function ToBasic: string;

    property ConditionCount: integer read GetConditionCount;
    property ActionCount: integer read GetActionCount;
    property Preserve: boolean read FPreserve write SetPreserve;
    property Players: TPlayers read FPlayers write FPlayers;
    property Condition[AIndex: integer]: TTriggerCondition read GetCondition;
    property Action[AIndex: integer]: TTriggerInstruction read GetAction;
    property IsMultithread: boolean read GetIsMultithread;
    property ThreadCount: integer read GetThreadCount;
  end;
  TTriggerList = specialize TFPGObjectList<TTrigger>;

var
  CompiledTriggers: TTriggerList;
  CompiledTriggersMultiCount: integer;

procedure InitTriggerCode;

//basic trigger output function
procedure WriteTrigger(APlayers: TPlayers;
                       AConditions: array of TTriggerCondition;
                       AActions: array of TInstruction;
                       ANextIP: integer;
                       APreserve: boolean);

procedure WriteProg(APlayers: TPlayers; AConditions: array of TCondition; AProg: TInstructionList;
                    AIPStart: integer; AReturnIP: integer; APreserve: boolean; ATempPreserve: integer = 0);

procedure WriteProg(APlayers: TPlayers; AConditions: TConditionList; AProg: TInstructionList;
                    AIPStart: integer; AReturnIP: integer; APreserve: boolean; ATempPreserve: integer = 0);

function NewIP: integer;
function SetNextIP(AValue: integer; APlayer: TPlayer = plCurrentPlayer): TTriggerInstruction;
function CheckIP(AValue: integer; APlayer: TPlayer = plCurrentPlayer): TTriggerCondition;
function IPUnused: boolean;
function RemoveIPIfUnused: boolean;

function NewSysIP: integer;
function SetNextSysIP(AValue: integer; AMode: TSetIntegerMode = simSetTo): TInstruction;
function CheckSysIP(AValue: integer; APlayer: TPlayer = plCurrentPlayer): TCondition;
procedure CheckSysIPRange(AMinValue, AMaxValue: integer; out ACond1, ACond2: TCondition);

function SetSysParam(AValue: integer): TInstruction;
function CheckSysParam(AValue: integer; APlayer: TPlayer = plCurrentPlayer): TCondition;

procedure AddSysCall(AInstructions: TInstructionList; AReturnIP, ACalledIP: integer);
procedure AddSysReturn(AInstructions: TInstructionList);
procedure WriteStackTriggers;

const InitialIP = 0;

implementation

uses ureadprog, uvariables, utrigedittypes, uprocedures;

// instruction pointer for regular blocks
var
  IPVar, CurIPValue: integer;
  BusyIPValue : integer;

function IPUnused: boolean;
begin
  result := CurIPValue = InitialIP;
end;

function NewIP: integer;
begin
  Inc(CurIPValue);
  result := CurIPValue;
end;

function BusyIP: integer;
begin
  if BusyIPValue = -1 then
    BusyIPValue:= NewIP;
  result := BusyIPValue;
end;

function GetIPVar: integer;
begin
  if IPVar = -1 then
  begin
    IPVar := IntArrayIndexOf(RunTimeScope, 'IP');
    if IPVar = -1 then
      IPVar := CreateIntArray(RunTimeScope, 'IP', MaxTriggerPlayers, [], 16);
  end;
  result := IPVar;
end;

procedure DeleteIPVar;
begin
  if IPVar <> -1 then
  begin
    IntArrays[IPVar].Deleted:= true;
    IPVar := -1;
  end;
end;

function SetNextIP(AValue: integer; APlayer: TPlayer = plCurrentPlayer): TTriggerInstruction;
begin
  result := CreateSetIntegerInstruction(APlayer, IntArrays[GetIPVar].UnitType, simSetTo, AValue) as TTriggerInstruction;
end;

function CheckIP(AValue: integer; APlayer: TPlayer = plCurrentPlayer): TTriggerCondition;
begin
  result := CreateIntegerCondition(APlayer, IntArrays[GetIPVar].UnitType, icmExactly, AValue) as TTriggerCondition;
end;

function RemoveIPIfUnused: boolean;
var
  i, j: Integer;
  t: TTrigger;
  dc: TDeathCountCondition;
  sd: TSetDeathInstruction;
begin
  if (IPVar <> -1) and IPUnused then
  begin
    result := true;
    for i := 0 to CompiledTriggers.Count-1 do
    begin
      t := CompiledTriggers[i];
      for j := 0 to t.ConditionCount-1 do
        if t.Condition[j] is TDeathCountCondition then
        begin
          dc := TDeathCountCondition(t.Condition[j]);
          if (dc.Player = plCurrentPlayer) and (dc.Mode = icmExactly) and
            (dc.UnitType = IntArrays[IPVar].UnitType) then
          begin
            t.DeleteCondition(j);
            if t.ConditionCount = 0 then
              t.AddCondition(TAlwaysCondition.Create);
            break;
          end;
        end;
      for j := t.ActionCount-1 downto 0 do
        if t.Action[j] is TSetDeathInstruction then
        begin
          sd := TSetDeathInstruction(t.Action[j]);
          if (sd.Player = plCurrentPlayer) and (sd.Mode = simSetTo) and
            (sd.UnitType = IntArrays[IPVar].UnitType) then
          begin
            t.DeleteAction(j);
            break;
          end;
        end;
    end;
    DeleteIPVar;
  end else
    result := false;
end;

procedure WriteTrigger(APlayers: TPlayers;
                       AConditions: array of TTriggerCondition;
                       AActions: array of TInstruction;
                       ANextIP: integer;
                       APreserve: boolean);
var
  t: TTrigger;
  i, multiCount: Integer;
  setNext, preserve: TTriggerInstruction;
  always: TAlwaysCondition;
begin
  t := TTrigger.Create(APlayers);
  setNext := nil;
  preserve := nil;
  always := nil;
  try
    for i := 0 to high(AConditions) do t.AddCondition(AConditions[i]);
    for i := 0 to high(AActions) do t.AddAction(AActions[i]);
    if t.ConditionCount = 0 then
    begin
      always := TAlwaysCondition.Create;
      t.AddCondition(always);
    end;
    if ANextIP <> -1 then
    begin
      setNext := SetNextIP(ANextIP);
      t.AddAction(setNext);
    end;
    if APreserve then
    begin
      preserve := TPreserveTriggerInstruction.Create;
      t.AddAction(preserve);
    end;
    FreeAndNil(setNext);
    FreeAndNil(preserve);
    FreeAndNil(always);
  except on ex: exception do
   begin
     t.Free;
     setNext.Free;
     preserve.Free;
     always.Free;
     raise exception.Create('Unable to create trigger. '+ex.Message);
   end;
  end;

  if t.IsMultithread then
  begin
    multiCount := t.ThreadCount;
    if CompiledTriggersMultiCount+multiCount > MAX_TRIGGER_COUNT then
      raise exception.Create('Too many triggers');
    CompiledTriggers.Add(t);
    inc(CompiledTriggersMultiCount, multiCount);
  end
  else
  begin
    if CompiledTriggersMultiCount >= MAX_TRIGGER_COUNT then
      raise exception.Create('Too many triggers');
    CompiledTriggers.Add(t);
    inc(CompiledTriggersMultiCount);
  end;
end;

// instruction pointer for system calls

var
  SysIPVar, CurSysIPValue: integer;
  SysParamArray: integer; //parameter for system functions

function GetSysIPVar: integer;
begin
  if SysIPVar = -1 then
  begin
    SysIPVar := IntArrayIndexOf(RunTimeScope, 'SysIP');
    if SysIPVar = -1 then
      SysIPVar := CreateIntArray(RunTimeScope, 'SysIP', MaxTriggerPlayers, [], 16);
  end;
  result := SysIPVar;
end;

function SetNextSysIP(AValue: integer; AMode: TSetIntegerMode): TInstruction;
begin
  result := CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[GetSysIPVar].UnitType, AMode, AValue);
end;

function CheckSysIP(AValue: integer; APlayer: TPlayer = plCurrentPlayer): TCondition;
begin
  result := CreateIntegerCondition(APlayer, IntArrays[GetSysIPVar].UnitType, icmExactly, AValue);
end;

procedure CheckSysIPRange(AMinValue, AMaxValue: integer; out ACond1,
  ACond2: TCondition);
begin
  ACond1 := CreateIntegerCondition(plCurrentPlayer, IntArrays[GetSysIPVar].UnitType, icmAtLeast, AMinValue);
  ACond2 := CreateIntegerCondition(plCurrentPlayer, IntArrays[GetSysIPVar].UnitType, icmAtMost, AMaxValue);
end;

procedure NeedSysParam;
begin
  if SysParamArray = -1 then
  begin
    SysParamArray:= IntArrayIndexOf(RunTimeScope, 'SysParam');
    if SysParamArray = -1 then
      SysParamArray := CreateIntArray(RunTimeScope, 'SysParam', MaxTriggerPlayers, [], 16);
  end;
end;

function SetSysParam(AValue: integer): TInstruction;
begin
  NeedSysParam;
  result := CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[SysParamArray].UnitType, simSetTo, AValue)
end;

function CheckSysParam(AValue: integer; APlayer: TPlayer = plCurrentPlayer): TCondition;
begin
  NeedSysParam;
  result := CreateIntegerCondition(APlayer, IntArrays[SysParamArray].UnitType, icmExactly, AValue)
end;

function NewSysIP: integer;
begin
  Inc(CurSysIPValue);
  result := CurSysIPValue;

  NeedSysParam;
end;

//stack

var
  SPArrayVar: integer;
  StackArrays: array[1..MaxStackSize] of integer;
  StackSize: integer;     //number of values that can be on the stack (max is MaxArraySize)
  MaxStackBits: integer;  //number of bits for one value on the stack (max is 32)
  ReturnSysIP, PushSysIP, WaitReturnIP: integer; //return and push functions

procedure NeedStack;
begin
  if SPArrayVar = -1 then
  begin
    SPArrayVar := IntArrayIndexOf(RunTimeScope, 'SP');
    if SPArrayVar = -1 then
      SPArrayVar := CreateIntArray(RunTimeScope, 'SP', MaxTriggerPlayers, [], 16);

    ReturnSysIP := NewSysIP;
    PushSysIP:= NewSysIP;
    WaitReturnIP := NewIP;
  end;
end;

function SetReturnSysIP: TInstruction;
begin
  NeedStack;
  result := SetNextSysIP(ReturnSysIP);
end;

function SetPushSysIP: TInstruction;
begin
  NeedStack;
  result := SetNextSysIP(PushSysIP);
end;

procedure DetermineStackValueSize;
var
  i,depth: Integer;

  function GetDepthRec(AProc: integer; ADepth: integer): integer;
  var j,sub: integer;
  begin
    result := ADepth;
    if ADepth = MaxStackSize+1 then exit;

    with Procedures[AProc] do
      for j := 0 to Calls.Count-1 do
      begin
        if Procedures[Calls[j]].StackChecked then raise exception.Create('Recursive calls not allowed (' + Procedures[AProc].Name + ' => ' + Procedures[Calls[j]].Name + ')');
        Procedures[Calls[j]].StackChecked := true;

        sub:= GetDepthRec(Calls[j], ADepth+1);
        if sub > result then result := sub;
      end;
  end;

begin
  MaxStackBits:= 1;
  while (1 shl MaxStackBits) - 1 < CurIPValue do inc(MaxStackBits);

  for i := 0 to ProcedureCount-1 do
    Procedures[i].StackChecked := false;
  StackSize := 1;
  for i := 0 to ProcedureCount-1 do
  if Procedures[i].StartIP <> -1 then
  begin
    depth := GetDepthRec(i, 1);
    if depth > StackSize then StackSize:= depth;
  end;

  if StackSize > MaxStackSize then
  begin
    Writeln('Warning: calls will exceed stack size');
    StackSize := MaxStackSize;
  end;

  for i := 1 to StackSize do
  begin
    StackArrays[i]:= IntArrayIndexOf(RunTimeScope, 'StackValue'+inttostr(i));
    if StackArrays[i] = -1 then
      StackArrays[i] := CreateIntArray(RunTimeScope, 'StackValue'+inttostr(i), MaxTriggerPlayers, [], 16);
  end;
end;

// write instruction block

procedure WriteProg(APlayers: TPlayers; AConditions: TConditionList; AProg: TInstructionList;
                    AIPStart: integer; AReturnIP: integer; APreserve: boolean; ATempPreserve: integer;
                    AFirstInstr, ALastInstr: integer); forward;

procedure WriteProg(APlayers: TPlayers; AConditions: array of TCondition; AProg: TInstructionList;
                    AIPStart: integer; AReturnIP: integer; APreserve: boolean; ATempPreserve: integer;
                    AFirstInstr, ALastInstr: integer);
var
  i,j: Integer;
  finalConditions : array of TTriggerCondition;
  consecutiveInstructions: array of TInstruction;
  instrCount: integer;

  NextIP, whileIP, beforeIP: integer;
  dropThread: TDropThreadInstruction;
  waitCond: TWaitConditionInstruction;
  fastIf: TFastIfInstruction;
  changeIP: TChangeIPInstruction;
  proc: TInstructionList;
  waitFor: TWaitForPlayersInstruction;
  pl: TPlayer;
  ipCheck: TCondition;
  splitInstr: TSplitInstruction;
  waitFound: Boolean;

  checkIPStart: TTriggerCondition;
  setIPStart: TTriggerInstruction;
  done: boolean;

begin
  setlength(finalConditions, length(AConditions) );
  for i := 0 to high(AConditions) do
  begin
    if AConditions[i].IsComputed then
      raise exception.Create('Computed conditions cannot be used as trigger conditions');
    if not (AConditions[i] is TTriggerCondition) then
      raise exception.Create('Supplied condition is not translatable to a trigger condition');
    finalConditions[i] := TTriggerCondition(AConditions[i]);
  end;

  checkIPStart := nil;
  setIPStart := nil;
  done := false;
  try
    if AIPStart <> -1 then
    begin
      setlength(finalConditions, length(finalConditions) + 1);
      checkIPStart := CheckIP(AIPStart);
      finalConditions[High(finalConditions)] := checkIPStart;
    end;

    setlength(consecutiveInstructions, ALastInstr - AFirstInstr + 1 + 1);
    instrCount := 0;
    if (AIPStart <> -1) and (AReturnIP <> -1) then
    begin
      i := AFirstInstr;
      waitFound := false;
      while (i <= ALastInstr) and not (AProg[i] is TDropThreadInstruction) and not
          (AProg[i] is TChangeIPInstruction) and not (AProg[i] is TWaitConditionInstruction)
          and not (AProg[i] is TWaitForPlayersInstruction)
          and not (AProg[i] is TSplitInstruction) do
      begin
        if AProg[i] is TWaitInstruction then waitFound := true;
        inc(i);
      end;
      if waitFound then
      begin
        setIPStart := SetNextIP(BusyIP);
        consecutiveInstructions[0] := setIPStart;
        inc(instrCount);
      end;
    end;

    for i := AFirstInstr to ALastInstr do
    begin
      if AProg[i] is TDropThreadInstruction then
      begin
        dropThread := TDropThreadInstruction(AProg[i]);

        if dropThread.PlayersToResume <> [] then
        begin
          if APlayers <= dropThread.PlayersToDrop then
            WriteTrigger(APlayers, finalConditions, slice(consecutiveInstructions, instrCount), dropThread.DropIP, APreserve or (ATempPreserve > 0))
          else
          begin
            NextIP := NewIP;
            WriteTrigger(APlayers, finalConditions, slice(consecutiveInstructions, instrCount), nextIP, APreserve or (ATempPreserve > 0));

            proc := TInstructionList.Create;
            if APlayers * (dropThread.PlayersToResume - dropThread.PlayersToDrop) <> [] then
              WriteProg(APlayers * (dropThread.PlayersToResume - dropThread.PlayersToDrop), [], proc, nextIP, dropThread.ResumeIP, APreserve or (ATempPreserve > 0));

            if APlayers * dropThread.PlayersToDrop <> [] then
              WriteProg(APlayers * dropThread.PlayersToDrop, [], proc, nextIP, dropThread.DropIP, APreserve or (ATempPreserve > 0));
            proc.Free;
          end;

          //carry on with the rest of the prog
          WriteProg(dropThread.PlayersToResume, [], AProg, dropThread.ResumeIP, AReturnIP, APreserve, ATempPreserve, i+1, ALastInstr);
        end else
          WriteTrigger(APlayers, finalConditions, slice(consecutiveInstructions, instrCount), dropThread.DropIP, APreserve or (ATempPreserve > 0));

        done := true;
        break;
      end else
      if AProg[i] is TWaitForPlayersInstruction then
      begin
        waitFor := TWaitForPlayersInstruction(AProg[i]);

        if (length(AConditions) = 0) and (instrCount = 0) and (AIPStart <> -1) then
          beforeIP := AIPStart
        else
        begin
          beforeIP := NewIP;
          WriteTrigger(APlayers, finalConditions, slice(consecutiveInstructions, instrCount), beforeIP, APreserve or (ATempPreserve > 0));
        end;

        proc := TInstructionList.Create;
        whileIP := NewIP;
        WriteProg(APlayers, [], proc, beforeIP,whileIP, true);

        for pl := low(TPlayer) to high(TPlayer) do
          if pl in waitFor.Players then
          begin
            ipCheck := CreateIntegerCondition(pl, IntArrays[GetIPVar].UnitType, icmAtLeast, 1);
            WriteProg(APlayers, [ipCheck], proc, whileIP, beforeIP, true);
            ipCheck.Free;
          end;

        proc.FreeAll;

        //carry on with the rest of the prog
        WriteProg(APlayers, [], AProg, whileIP, AReturnIP, APreserve, ATempPreserve, i+1, ALastInstr);
        done := true;
        break;
      end else
      if AProg[i] is TWaitConditionInstruction then
      begin
        waitCond := TWaitConditionInstruction(AProg[i]);
        WriteTrigger(APlayers, finalConditions, slice(consecutiveInstructions, instrCount), waitCond.IP, APreserve or (ATempPreserve > 0));

        //carry on with the rest of the prog
        WriteProg(APlayers, waitCond.Conditions, AProg, waitCond.IP, AReturnIP, APreserve, ATempPreserve, i+1, ALastInstr);
        done := true;
        break;
      end else
      if AProg[i] is TSplitInstruction then
      begin
        splitInstr := TSplitInstruction(AProg[i]);
        if splitInstr.EndIP = -1 then splitInstr.EndIP := AReturnIP;

        WriteTrigger(APlayers, finalConditions, slice(consecutiveInstructions, instrCount), splitInstr.EndIP, APreserve or (ATempPreserve > 0));

        //carry on with the rest of the prog
        if splitInstr.ChangePlayers <> [] then APlayers := splitInstr.ChangePlayers;
        WriteProg(APlayers, [], AProg, splitInstr.ResumeIP, AReturnIP, APreserve, ATempPreserve, i+1, ALastInstr);
        done := true;
        break;
      end else
      if AProg[i] is TChangeIPInstruction then
      begin
        changeIP := TChangeIPInstruction(AProg[i]);
        WriteTrigger(APlayers, finalConditions, slice(consecutiveInstructions, instrCount), changeIP.IP, APreserve or (ATempPreserve > 0));

        //carry on with the rest of the prog
        WriteProg(APlayers, [], AProg, changeIP.IP, AReturnIP,
                  APreserve, ATempPreserve + changeIP.Preserve,  i+1, ALastInstr);
        done := true;
        break;
      end else
      if AProg[i] is TFastIfInstruction then
      begin
        NextIP:= NewIP;
        WriteTrigger(APlayers, finalConditions, slice(consecutiveInstructions, instrCount), NextIP, APreserve or (ATempPreserve > 0));

        j := i;
        while (j <= ALastInstr) and (AProg[j] is TFastIfInstruction) do
        begin
          fastIf := TFastIfInstruction(AProg[j]);
          WriteProg(APlayers, fastIf.Conditions, fastIf.Instructions, NextIP, NextIP, APreserve, ATempPreserve);
          inc(j);
        end;

        //carry on with the rest of the prog
        WriteProg(APlayers, [], AProg, NextIP, AReturnIP, APreserve, ATempPreserve, j, ALastInstr);
        done := true;
        break;
      end;
      if not (AProg[i] is TTriggerInstruction) then
        raise exception.Create('Expecting trigger instruction but ' + AProg[i].Classname + ' found')
      else
        consecutiveInstructions[instrCount] := TTriggerInstruction(AProg[i]);

      inc(instrCount);
      if instrCount >= MAX_ACTIONS-1 then
      begin
        nextIP := NewIP;
        WriteTrigger(APlayers, finalConditions, slice(consecutiveInstructions, instrCount), nextIP, APreserve or (ATempPreserve > 0));
        WriteProg(APlayers, [], AProg, NextIP, AReturnIP, APreserve, ATempPreserve, i+1, ALastInstr);
        done := true;
        break;
      end;
    end;

    if not done then
      WriteTrigger(APlayers, finalConditions, slice(consecutiveInstructions, instrCount), AReturnIP, APreserve or (ATempPreserve > 0));

  finally
    checkIPStart.Free;
    setIPStart.Free;
  end;
end;

procedure WriteProg(APlayers: TPlayers; AConditions: array of TCondition; AProg: TInstructionList;
                    AIPStart: integer; AReturnIP: integer; APreserve: boolean; ATempPreserve: integer = 0);
begin
  WriteProg(APlayers, AConditions, AProg, AIPStart, AReturnIP, APreserve, ATempPreserve, 0, AProg.Count-1);
end;

procedure WriteProg(APlayers: TPlayers; AConditions: TConditionList; AProg: TInstructionList;
                    AIPStart: integer; AReturnIP: integer; APreserve: boolean; ATempPreserve: integer;
                    AFirstInstr, ALastInstr: integer);
var cond: array of TCondition;
  i: Integer;
begin
  setlength(cond,AConditions.Count);
  for i := 0 to AConditions.Count-1 do
    cond[i] := AConditions[i];
  WriteProg(APlayers, cond, AProg, AIPStart, AReturnIP, APreserve, ATempPreserve, AFirstInstr, ALastInstr);
end;

procedure WriteProg(APlayers: TPlayers; AConditions: TConditionList; AProg: TInstructionList;
                    AIPStart: integer; AReturnIP: integer; APreserve: boolean; ATempPreserve: integer = 0);
begin
  WriteProg(APlayers, AConditions, AProg, AIPStart, AReturnIP, APreserve, ATempPreserve, 0, AProg.Count-1);
end;

procedure AddSysCall(AInstructions: TInstructionList; AReturnIP, ACalledIP: integer);
begin
  AInstructions.Add(SetSysParam(AReturnIP));
  AInstructions.Add(SetPushSysIP);
  AInstructions.Add(TSplitInstruction.Create(AReturnIP, ACalledIP));
end;

procedure AddSysReturn(AInstructions: TInstructionList);
begin
  AInstructions.Add( SetReturnSysIP );
  AInstructions.Add( TDropThreadInstruction.Create(WaitReturnIP, -1, [plAllPlayers], []));
end;

procedure WriteStackTriggers;
var proc: TInstructionList;
  cond: TConditionList;
  spCond, valCond: TCondition;
  returnCond, pushCond: TCondition;
  subStackVal,addIP,addStackVal, subVal: TInstruction;
  sp, bit: integer;

  procedure EmptyProc;
  var
    i: Integer;
  begin
    for i := 0 to proc.Count-1 do
      proc[i].Free;
    proc.Clear;
  end;

  procedure EmptyCond;
  var
    i: Integer;
  begin
    for i := 0 to cond.Count-1 do
      cond[i].Free;
    cond.Clear;
  end;

begin
  if SPArrayVar = -1 then exit;

  DetermineStackValueSize;

  proc := TInstructionList.Create;
  cond := TConditionList.Create;

  //return handler

  //SP -= 1, IP := 0
  returnCond := CheckSysIP(ReturnSysIP);
  cond.Add(returnCond);
  proc.Add(TCommentInstruction.Create('Return'));
  proc.Add(CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[SPArrayVar].UnitType, simSubtract, 1));
  proc.Add(SetNextIP(0));
  WriteProg([plAllPlayers], cond, proc, -1, -1, True);
  cond.Clear;
  EmptyProc;

  //copy stack value to IP
  for sp := 0 to StackSize-1 do
  begin
    spCond := CreateIntegerCondition(plCurrentPlayer, IntArrays[SPArrayVar].UnitType, icmExactly, sp);


    for bit := MaxStackBits-1 downto 0 do
    begin
      valCond := CreateIntegerCondition(plCurrentPlayer, IntArrays[StackArrays[sp+1]].UnitType, icmAtLeast, 1 shl bit);

      subStackVal := CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[StackArrays[sp+1]].UnitType, simSubtract, 1 shl bit);
      addIP := CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[GetIPVar].UnitType, simAdd, 1 shl bit);
      proc.Add(subStackVal);
      proc.Add(addIP);

      WriteProg([plAllPlayers], [spCond,valCond], proc, -1, -1, True);
      EmptyProc;

      valCond.Free;
    end;

    proc.Add(SetNextSysIP(0));
    WriteProg([plAllPlayers], [returnCond,spCond], proc, -1, -1, True);
    EmptyProc;

    spCond.Free;
  end;
  returnCond.Free;

  //stack overflow handler
  cond.Add(CheckSysIP(PushSysIP));
  cond.Add(CreateIntegerCondition(plCurrentPlayer, IntArrays[SPArrayVar].UnitType, icmAtLeast, StackSize));
  proc.Add(TCommentInstruction.Create('Push'));
  proc.Add(TDisplayTextMessageInstruction.Create(True, 'Stack overflow'));
  proc.Add(TWaitInstruction.Create(4000));
  WriteProg([plAllPlayers], cond, proc, -1, -1, True);
  EmptyCond;
  EmptyProc;

  //push handler
  pushCond := CheckSysIP(PushSysIP);
  for sp := 0 to StackSize-1 do
  begin
    spCond := CreateIntegerCondition(plCurrentPlayer, IntArrays[SPArrayVar].UnitType, icmExactly, sp);
    for bit := MaxStackBits-1 downto 0 do
    begin
      valCond := CreateIntegerCondition(plCurrentPlayer, IntArrays[SysParamArray].UnitType, icmAtLeast, 1 shl bit);

      addStackVal := CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[StackArrays[sp+1]].UnitType, simAdd, 1 shl bit);
      subVal := CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[SysParamArray].UnitType, simSubtract, 1 shl bit);
      proc.Add(addStackVal);
      proc.Add(subVal);

      WriteProg([plAllPlayers], [pushCond,spCond,valCond], proc, -1, -1, True);
      EmptyProc;
      valCond.Free;
    end;

    proc.add(CreateSetIntegerInstruction(plCurrentPlayer, IntArrays[SPArrayVar].UnitType, simAdd, 1));
    proc.Add(SetNextSysIP(0));
    WriteProg([plAllPlayers], [pushCond, spCond], proc, -1, -1, True);
    EmptyProc;

    spCond.Free;
  end;
  pushCond.Free;

  cond.Free;
  proc.Free;
end;

procedure InitTriggerCode;
begin
  IPVar := -1;
  CurIPValue := InitialIP;

  //IntArrays[IPVar].UnitType:= 'Gas'; //debug

  CurSysIPValue:= 0;
  SysIPVar := -1;

  SPArrayVar := -1;
  ReturnSysIP := -1;
  PushSysIP:= -1;
  SysParamArray := -1;

  BusyIPValue := -1;
  CompiledTriggers.Clear;
  CompiledTriggersMultiCount := 0;
end;

{ TTrigger }

function TTrigger.GetAction(AIndex: integer): TTriggerInstruction;
begin
  result := FActions[AIndex];
end;

function TTrigger.GetActionCount: integer;
begin
  result := FActions.Count;
end;

function TTrigger.GetCondition(AIndex: integer): TTriggerCondition;
begin
  result := FConditions[AIndex];
end;

function TTrigger.GetConditionCount: integer;
begin
  result := FConditions.Count;
end;

function TTrigger.GetIsMultithread: boolean;
var
  i: Integer;
begin
  for i := 0 to ActionCount-1 do
    if Action[i].IsMultithread then exit(true);
  result := false;
end;

function TTrigger.GetThreadCount: integer;
var
  pl: TPlayer;
begin
  result := 0;
  for pl := succ(plNone) to high(TPlayer) do
    if pl in Players then inc(result);
end;

procedure TTrigger.SetPreserve(AValue: boolean);
begin
  if FPreserve=AValue then Exit;
  FPreserve:=AValue;
end;

constructor TTrigger.Create(APlayers: TPlayers);
begin
  FPlayers := APlayers;
  FConditions := TTriggerConditionList.Create;
  FActions := TTriggerInstructionList.Create;
end;

constructor TTrigger.Create;
begin
  FPlayers := [];
  FConditions := TTriggerConditionList.Create;
  FActions := TTriggerInstructionList.Create;
end;

destructor TTrigger.Destroy;
begin
  FConditions.Free;
  FActions.Free;
  inherited Destroy;
end;

procedure TTrigger.Clear;
begin
  FPlayers := [];
  FConditions.Clear;
  FActions.Clear;
  FPreserve:= false;
end;

procedure TTrigger.AddCondition(ACondition: TCondition);
begin
  if not (ACondition is TTriggerCondition) then
    raise exception.Create('Expecting trigger condition');
  if ConditionCount >= MAX_CONDITIONS then
    raise exception.Create('Too many conditions');
  FConditions.Add(TTriggerCondition(ACondition.Duplicate));
end;

procedure TTrigger.DeleteCondition(AIndex: integer);
begin
  FConditions.Delete(AIndex);
end;

procedure TTrigger.AddAction(AInstruction: TInstruction);
var
  i: Integer;
begin
  if AInstruction is TEmptyInstruction then exit;
  if not (AInstruction is TTriggerInstruction) then
    raise exception.Create('Expecting trigger instruction but ' +AInstruction.Classname + ' found');
  if ActionCount >= MAX_ACTIONS then
  begin
    //if too many actions, try to store the preserve trigger as a trigger flag
    if AInstruction is TPreserveTriggerInstruction then
    begin
      Preserve := true;
      exit;
    end else
    begin
      for i := 0 to ActionCount-1 do
        if FActions[i] is TPreserveTriggerInstruction then
        begin
          FActions.Delete(i);
          Preserve:= true;
          break;
        end;
    end;

    if ActionCount >= MAX_ACTIONS then
      raise exception.Create('Too many actions');
  end;
  FActions.Add(TTriggerInstruction(AInstruction).Duplicate as TTriggerInstruction);
end;

procedure TTrigger.DeleteAction(AIndex: integer);
begin
  FActions.Delete(AIndex);
end;

function TTrigger.ToTrigEdit: string;
begin
  result := ToTrigEditFor(plNone);
end;

function TTrigger.ToTrigEditFor(APlayer: TPlayer): string;
var
  i: Integer;
  pl: TPlayer;
  firstPl: boolean;
begin
  result := 'Trigger(';
  if APlayer <> plNone then
    result += '"' + PlayerToTrigEditStr(APlayer) + '"'
  else
  begin
    firstPl:= true;
    for pl := low(TPlayer) to high(TPlayer) do
      if pl in FPlayers then
      begin
        if not firstPl then result += ',';
        result += '"' + PlayerToTrigEditStr(pl) + '"';
        firstPl:= false;
      end;
  end;
  result += '){'+LineEnding+'Conditions:'+LineEnding;
  for i := 0 to FConditions.Count-1 do
    result+= #9 + FConditions[i].ToTrigEdit + ';'+LineEnding;
  result += 'Actions:'+LineEnding;
  for i := 0 to FActions.Count-1 do
    if (APlayer <> plNone) and FActions[i].IsMultithread then
      result += #9 + (FActions[i] as TTriggerMultiInstruction).ToTrigEditFor(APlayer) + ';'+LineEnding
      else result += #9 + FActions[i].ToTrigEdit + ';'+LineEnding;
  result += '}';
end;

procedure TTrigger.WriteTriggerData(var AData: TTriggerData);
begin
  WriteTriggerDataFor(plNone, AData);
end;

procedure TTrigger.WriteTriggerDataFor(APlayer: TPlayer; var AData: TTriggerData);
var
  i: Integer;
begin
  for i := 0 to FConditions.Count-1 do
    FConditions[i].WriteTriggerData(AData.Conditions[i]);
  for i := 0 to FActions.Count-1 do
    if (APlayer <> plNone) and FActions[i].IsMultithread then
      (FActions[i] as TTriggerMultiInstruction).WriteTriggerDataFor(APlayer, AData.Actions[i])
      else FActions[i].WriteTriggerData(AData.Actions[i]);
  if APlayer <> plNone then
    AData.Players := [APlayer]
    else AData.Players := FPlayers;
  AData.PreserveTrigger := Preserve;
end;

procedure TTrigger.ReadTriggerData(const AData: TTriggerData);
var
  i: Integer;
  cond: TTriggerCondition;
  act: TTriggerInstruction;
begin
  Clear;
  FPlayers := AData.Players;
  for i := 0 to MAX_CONDITIONS-1 do
  begin
    cond := TTriggerCondition.LoadFromData(AData.Conditions[i]);
    if cond <> nil then AddCondition(cond);
  end;
  for i := 0 to MAX_ACTIONS-1 do
  begin
    act := TTriggerInstruction.LoadFromData(AData.Actions[i]);
    if act <> nil then AddAction(act);
  end;
end;

function TTrigger.ToBasic: string;
var
  nb, i: integer;
  pl: TPlayer;
  preserveAction: Boolean;
begin
  result := '';
  nb := 0;
  for pl := low(TPlayer) to high(TPlayer) do
    if (pl in Players) and (PlayerIdentifiers[pl]<>'') then
    begin
      if nb > 0 then result += ', ';
      result += PlayerIdentifiers[pl];
      inc(nb);
    end;
  if nb = 1 then result := 'As ' + result + LineEnding
  else if nb > 1 then result := 'As {' + result + '}' + LineEnding;

  result += 'On ';
  for i := 0 to ConditionCount-1 do
  begin
    if i > 0 then result += ' And ';
    result += Condition[i].ToBasic(False);
  end;
  if ConditionCount = 0 then
    result += 'True';
  result += LineEnding;

  preserveAction := false;
  for i := 0 to ActionCount-1 do
    if Action[i] is TPreserveTriggerInstruction then
      preserveAction := true
    else
      result += '    '+Action[i].ToBasic+LineEnding;

  if not (preserveAction or Preserve) then result += '    Stop'+LineEnding;
  result += 'End On';
end;

initialization

  CompiledTriggers:= TTriggerList.Create;

finalization

  CompiledTriggers.Free;

end.

