unit ureadprog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uinstructions, fgl, usctypes, uprocedures;

function ReadProg(AFilename: string; out AMainThread: TPlayer; out ALastScope: integer; ADefaultMainThread: TPlayer): boolean;
function ReadProg(ALines: TStrings; out AMainThread: TPlayer; out ALastScope: integer; ADefaultMainThread: TPlayer): boolean;

var
  ReadProgErrors, ReadProgWarnings: TStringList;

procedure AddError(ALine: integer; AText: string);
procedure AddWarning(ALine: integer; AText: string);

implementation

uses uparsevb, uvariables, utriggerinstructions,
  utriggerconditions, umapinfo, uparsescalar, uparsecomplex, ureadinstruction;

const MAX_ERRORS = 3;
const MaxInstructionsPerSub = 65534*63 div 4;

procedure AddError(ALine: integer; AText: string);
begin
  ReadProgErrors.Add('Line '+Inttostr(ALine)+': [Error] ' + AText);
end;

procedure AddWarning(ALine: integer; AText: string);
begin
  ReadProgWarnings.Add('Line '+Inttostr(ALine)+': [Warning] ' + AText);
end;

function ParseOption(ALine: TStringList): boolean;
var
  index: Integer;

  procedure CheckEndOfLine;
  begin
    if index <> ALine.Count then
      raise exception.Create('Expecting end of line');
  end;

begin
  index := 0;
  if TryToken(ALine,index,'Option') then
  begin
    if TryToken(ALine,index,'Hyper') then
    begin
      if TryToken(ALine,index,'On') then HyperTriggersOption:= true
      else If TryToken(ALine,index,'Off') then HyperTriggersOption:= false
      else raise exception.Create('Expecting On or Off');
    end else
      raise exception.Create('Unknown option');
    CheckEndOfLine;
    exit(true);
  end;
  exit(false);
end;

function RemoveTrailingCommentAndTabs(AText: string): string;
var inStr: boolean;
  i: Integer;
begin
  inStr := false;
  for i := 1 to length(AText) do
  begin
    if AText[i] = '"' then inStr := not inStr;
    if (AText[i] = '''') and not inStr then
      exit(copy(AText,1,i-1));
    if (AText[i] in[#0..#31]) and not inStr then AText[i] := ' ';
  end;
  exit(AText);
end;

function ReadProg(AFilename: string; out AMainThread: TPlayer; out ALastScope: integer; ADefaultMainThread: TPlayer): boolean;
var
  lines: TStringList;
begin
  lines := TStringList.Create;
  lines.LoadFromFile(AFilename);
  try
    result := ReadProg(lines, AMainThread, ALastScope, ADefaultMainThread);
  finally
    lines.free;
  end;
end;

procedure ParseCode(AThreads: TPlayers; AMainThread: TPlayer; inSubMain: boolean;
  inSub, inEvent: integer; ACustomCode: TCodeLineList = nil; ANestedLoopCount: Integer = 0);
var
  code: TCodeLineList;
  i, lineNumber: Integer;
  line: TStringList;
  index: integer;
  doPlayers: TPlayers;
  inDoAs: Boolean;
  scope: integer;
  warning: string;

  procedure CheckEndOfLine;
  begin
    if index <> line.Count then
      raise exception.Create('Expecting end of line');
  end;

  procedure DoParseInstruction;
  begin
    if inSub <> -1 then
    begin
      ParseInstruction(scope, line, Procedures[inSub].Instructions, AThreads, AMainThread, inSub,false);
      if Procedures[inSub].Instructions.Count > MaxInstructionsPerSub then
        raise exception.Create('Too many instructions');
    end
    else if inEvent <> -1 then
    begin
      if inDoAs then
        ParseInstruction(scope, line, Events[inEvent].Instructions, doPlayers, AMainThread, -1, false)
      else
        ParseInstruction(scope, line, Events[inEvent].Instructions, AThreads, AMainThread, -1, false);
      if Events[inEvent].Instructions.Count > MaxInstructionsPerSub then
        raise exception.Create('Too many instructions');
    end else
    begin
      if inDoAs then
        ParseInstruction(scope, line, MainProg, doPlayers, AMainThread, -1, true)
      else
        ParseInstruction(scope, line, MainProg, AThreads, AMainThread, -1, true);
      if MainProg.Count > MaxInstructionsPerSub then
        raise exception.Create('Too many instructions');
    end
  end;

  procedure DoForLoop(var ALineIndex: integer);
  const MAX_LOOP = 128;
    MaxNestedForLoop = 2;

    procedure CheckLoopCount(ACount: integer);
    begin
      if ACount > MAX_LOOP then raise exception.Create('Too many iterations (maximum is '+inttostr(MAX_LOOP)+' but '+inttostr(ACount)+' found)');
    end;

    function ParseLoopCode(AFromLine,AToLine: integer; ALoopVar: string; AValues: array of string): boolean;
    var
      loopVarPositions: array of record
        LineIndex, TokenIndex: integer;
      end;
      loopVarPositionCount: integer;

      procedure AddLoopVarPosition(ALineIndex,ATokenIndex: integer);
      begin
        if loopVarPositionCount >= length(loopVarPositions) then
          setlength(loopVarPositions, loopVarPositionCount*2+4);
        loopVarPositions[loopVarPositionCount].LineIndex:= ALineIndex;
        loopVarPositions[loopVarPositionCount].TokenIndex:= ATokenIndex;
        inc(loopVarPositionCount);
      end;

    var
      loopCode: TCodeLineList;
      loopLine: TCodeLine;
      i, j, k: integer;
      prevErrCount: integer;
    begin
      result := true;
      prevErrCount := ReadProgErrors.Count;
      loopCode := TCodeLineList.Create;
      try
        loopVarPositions := nil;
        loopVarPositionCount := 0;
        for i := AFromLine to AToLine do
        begin
          loopLine := TCodeLine.Create(code[i].Tokens,code[i].LineNumber);
          for j := 0 to loopLine.Tokens.Count-1 do
            if (CompareText(loopLine.Tokens[j], ALoopVar)=0) and
              ((j = 0) or (loopLine.Tokens[j-1] <> '.')) then
              AddLoopVarPosition(loopCode.Count, j);
          loopCode.Add(loopLine);
        end;

        for k := 0 to high(AValues) do
        begin
          for i := 0 to loopVarPositionCount-1 do
            with loopVarPositions[i] do
              loopCode.Items[LineIndex].Tokens[TokenIndex] := AValues[k];

          if inDoAs then
            ParseCode(doPlayers, AMainThread, inSubMain, inSub, inEvent, loopCode, ANestedLoopCount + 1)
          else
            ParseCode(AThreads, AMainThread, inSubMain, inSub, inEvent, loopCode, ANestedLoopCount + 1);

          if ReadProgErrors.Count <> prevErrCount then
          begin
            result := false;
            break;
          end;
        end;
      finally
        loopCode.FreeAll;
      end;
    end;

  var j: integer;
    loopValues: array of string;
    fromValue, stepValue, nesting, toValue, bitCount: integer;
    isString, isBoolean: boolean;
    intValues: ArrayOfInteger;
    loopVar, varType: string;
    boolValues: ArrayOfSwitchValue;

    procedure ParseLoopVarType(AAcceptBool, AAcceptStr: boolean);
    begin
      bitCount := 0;
      isString := false;
      isBoolean:= false;
      if TryToken(line,index,'As') then
      begin
        if TryUnsignedIntegerType(line,index) then
        begin
          bitCount := GetBitCountOfType(line[index-1]);
          if bitCount = 0 then bitCount := 24;
        end else
        if not AAcceptBool and PeekToken(line,index,'Boolean') then
          raise exception.Create('Boolean type not accepted here') else
        if TryToken(line,index,'Boolean') then isBoolean := true else
        if not AAcceptStr and PeekToken(line,index,'String') then
          raise exception.Create('String type not accepted here') else
        if PeekToken(line,index,'Integer') or PeekToken(line,index,'UInteger') then
          raise exception.Create('Please specify the bit count of the integer by using Byte, UInt16 or UInt24') else
        if TryToken(line,index,'String') then isString := true else
        begin
          TryIdentifier(line,index,varType,true);
          if varType = '' then raise exception.Create('Expecting loop variable type')
          else raise exception.Create('Expecting loop variable type but "'+vartype+'" found');
        end;
      end else
      begin
        varType := 'UInt24';
        bitCount := 24;
      end;
    end;

    procedure CheckIntBounds(AValue: integer);
    begin
      if (AValue < 0) or (AValue >= 1 shl bitCount) then
          raise exception.Create('Integer value out of bounds')
    end;

  begin
    if ANestedLoopCount >= MaxNestedForLoop then
      raise exception.Create('Too many nested for loops');
    if TryIdentifier(line,index,loopVar, false) then
    begin
      if IsVarNameUsed(scope, loopVar, 0) then
        raise exception.Create('The name "'+loopVar+'" is already in use');
      ParseLoopVarType(False,False);
      ExpectToken(line,index,'=');
      fromValue := ExpectIntegerConstant(AThreads, scope, line, index, true);
      CheckIntBounds(fromValue);
      ExpectToken(line,index,'To');
      toValue := ExpectIntegerConstant(AThreads, scope, line, index, true);
      CheckIntBounds(toValue);
      if TryToken(line,index,'Step') then
      begin
        stepValue := ExpectIntegerConstant(AThreads, scope, line, index, true);
        if stepValue = 0 then raise exception.Create('Zero is not allowed as a step value');
      end
      else
        stepValue := 1;

      if ((stepValue > 0) and (toValue < fromValue)) or
         ((stepValue < 0) and (toValue > fromValue)) then
        loopValues := nil
      else
      begin
        j := abs(toValue-fromValue) div abs(stepValue) + 1;
        CheckLoopCount(j);
        setlength(loopValues, j);
        for j := 0 to high(loopValues) do
          loopValues[j] := inttostr(fromValue + j*stepValue);
      end;
    end else
    if TryToken(line,index,'Each') then
    begin
      if TryIdentifier(line,index,loopVar, false) then
      begin
        if IsVarNameUsed(scope, loopVar, 0) then
          raise exception.Create('The name "'+loopVar+'" is already in use');
        ParseLoopVarType(true,true);
        ExpectToken(line,index,'In');
        if bitCount > 0 then
        begin
          intValues := ParseIntArray(AThreads, scope, line, index);
          CheckLoopCount(length(intValues));
          if bitCount <> 0 then
            for j := 0 to high(intValues) do
              CheckIntBounds(intValues[j]);

          setlength(loopValues, length(intValues));
          for j := 0 to high(loopValues) do
            loopValues[j] := inttostr(intValues[j]);
        end else
        if isString then
        begin
          loopValues := ParseStringArray(AThreads, scope, line, index);
          CheckLoopCount(length(loopValues));
          for j := 0 to high(loopValues) do
            loopValues[j] := '"'+stringreplace(loopValues[j],'"','""',[rfReplaceAll])+'"';
        end else
        if isBoolean then
        begin
          boolValues := ParseBoolArray(AThreads, scope, line, index);
          CheckLoopCount(length(boolValues));
          setlength(loopValues, length(boolValues));
          for j := 0 to high(loopValues) do
            case boolValues[j] of
            svSet: loopValues[j] := 'True';
            svClear: loopValues[j] := 'False';
            svRandomize: loopValues[j] := 'Rnd';
            else raise exception.Create('Unexpected boolean value');
            end;
        end;
      end else
        raise exception.Create('Loop variable expected');
    end else
      raise exception.Create('Loop variable expected');
    CheckEndOfLine;

    if length(loopValues) = 0 then
      AddWarning(lineNumber,'Loop will never be executed');

    nesting := 1;
    for j := ALineIndex+1 to code.Count-1 do
    if code[j].Tokens.Count > 0 then
    begin
      if CompareText(code[j].Tokens[0], 'For')=0 then inc(nesting)
      else if CompareText(code[j].Tokens[0], 'Next')=0 then
      begin
        if code[j].Tokens.Count > 1 then
        begin
          if CompareText(code[j].Tokens[1], loopVar)<>0 then
            raise exception.Create('Expecting end of line or matching loop variable');

          if code[j].Tokens.Count > 2 then
            raise exception.Create('Expecting end of line');
        end;
        dec(nesting);
        if nesting = 0 then
        begin
          ParseLoopCode(ALineIndex+1, j-1, loopVar, loopValues);
          ALineIndex := j;
          break;
        end;
      end else if (CompareText(code[j].Tokens[0],'Dim')=0) or
        (CompareText(code[j].Tokens[0],'Const')=0) then
        begin
          lineNumber := code[j].LineNumber;
          raise exception.Create('You cannot declare variables or constants within a For loop');
        end;
    end;

    if nesting <> 0 then raise exception.Create('Matching "Next" not found');
  end;

  procedure DoStop;
  var j: integer;
  begin
    if inEvent <> -1 then
    begin
      ExpectToken(line, index, 'Stop');
      if Events[inEvent].Preserve then
      begin
        Events[inEvent].Preserve := false;
        for j := 0 to Events[inEvent].Instructions.Count-1 do
          if Events[inEvent].Instructions[j] is TReturnInstruction then
            raise exception.Create('If you use Stop in an event, you cannot use Return in the same event');
      end;
      CheckEndOfLine;
      Events[inEvent].Instructions.Add(TReturnInstruction.Create);
    end else
      raise exception.Create('Stop instruction only allowed in event');
  end;

begin
  if inSubMain then
  begin
    code := MainCode;
    scope := SubMainScope;
  end
  else if inSub <> -1 then
  begin
    code := Procedures[inSub].Code;
    scope := Procedures[inSub].InnerScope;
  end
  else if inEvent <> -1 then
  begin
    code := Events[inEvent].Code;
    scope := Events[inEvent].InnerScope;
  end
  else raise exception.Create('Not in a code block');

  if ACustomCode <> nil then code := ACustomCode;

  inDoAs := false;
  doPlayers := [];

  i := -1;
  while i < code.Count-1 do
  begin
    i += 1;
    lineNumber := code[i].LineNumber;
    line := code[i].tokens;
    index := 0;
    if FillParseCompletionList then
      ClearParseCompletionList;

    try
      if TryToken(line,index,'For') then DoForLoop(i) else
      if TryToken(line,index,'Dim') or TryToken(line,index,'Const') then
      begin
        if inEvent <> -1 then
          ProcessDim(AThreads, scope, line, Events[inEvent].Instructions, true, warning)
        else if inSub <> -1 then
          ProcessDim(AThreads, scope, line, Procedures[inSub].Instructions, true, warning)
        else
          ProcessDim(AThreads, scope, line, MainProg, false, warning);

        if warning<>'' then AddWarning(lineNumber, warning);
      end
      else if (inEvent <> -1) and TryToken(line,index, 'Return') then
      begin
        if not Events[inEvent].Preserve then
          raise exception.Create('If you use Stop in an event, you cannot use Return in the same event');
        CheckEndOfLine;
        Events[inEvent].Instructions.Add(TReturnInstruction.Create);
      end
      else if PeekToken(line,index, 'Stop') then DoStop
      else if TryToken(line,index,'Do') then
      begin
        if inDoAs and PeekToken(line,index,'As') then
          raise exception.Create('Nested multi-thread instruction not allowed');
        if not inDoAs and TryToken(line,index,'As') then
        begin
          doPlayers := ExpectPlayers(AThreads, scope, line, index);
          if (plAllPlayers in doPlayers) or ([plForce1,plForce2,plForce3,plForce4] <= doPlayers) then
            doPlayers := [plPlayer1,plPlayer2,plPlayer3,plPlayer4,
                      plPlayer5,plPlayer6,plPlayer7,plPlayer8];

          if (AThreads<> []) and (AThreads <> [AMainThread]) then
            raise exception.Create('Multi-thread instruction only allowed in main thread');

          if inEvent<>-1 then
            Events[inEvent].Instructions.Add(TDoAsInstruction.Create(doPlayers)) else
          if inSubMain then
            MainProg.Add(TDoAsInstruction.Create(doPlayers))
          else
            raise exception.Create('Unhandled case');

          CheckEndOfLine;

          inDoAs := true;
        end else
          raise exception.Create('Do loop not implemented. Use While instead.');
      end else
      if TryToken(line,index,'End') then
      begin
        if TryToken(line,index,'Do') then
        begin
          if inDoAs then
          begin
            inDoAs := false;
            if inSubMain then
              MainProg.Add(TEndDoAsInstruction.Create(doPlayers) ) else
            if (inEvent<>-1) then
              Events[inEvent].Instructions.Add(TEndDoAsInstruction.Create(doPlayers) )
            else
              raise exception.Create('Unhandled case');
          end
          else raise exception.Create('Unexpected end of block');
        end else
          DoParseInstruction;
      end else
        DoParseInstruction;

    except
      on ex:Exception do
        if ReadProgErrors.Count < MAX_ERRORS then
          AddError(lineNumber, ex.Message);
    end;
  end;

  if inDoAs then raise exception.Create('Multi-thread instruction not finished');
end;

function ReadProg(ALines: TStrings; out AMainThread: TPlayer;
  out ALastScope: integer; ADefaultMainThread: TPlayer): boolean;
var
  line: TStringList;
  index: integer;
  fileLineNumber, lineNumber: integer;

  function ReadOneLine: string;
  begin
    result := ALines[fileLineNumber];
    inc(fileLineNumber);
  end;

  function EOF: boolean;
  begin
    result := fileLineNumber >= ALines.Count;
  end;

  procedure ReadNextLine;
  var s, lastToken: string;
    extraLine: TStringList;
  begin
    s := ReadOneLine;
    lineNumber := fileLineNumber;
    if Assigned(line) then FreeAndNil(line);
    line := ParseLine(s);
    index := 0;
    while not EOF and (line.Count > 0) do
    begin
      lastToken := line[line.Count-1];
      if IsTokenOverEndOfLine(lastToken) then
      begin
        s := ReadOneLine;
        extraLine := ParseLine(s);
        if (extraLine.Count > 0) and (CompareText(extraLine[0],'End') <> 0) then
        begin
          line.AddStrings(extraLine);
          extraLine.Free;
        end
        else
        begin
          extraline.Free;
          dec(fileLineNumber);
          break;
        end;
      end else break;
    end;
  end;

  procedure CheckEndOfLine;
  begin
    if index <> line.Count then
      raise exception.Create('Expecting end of line');
  end;

  procedure SetMainThread(APlayers: TPlayers);
  var
    pl: TPlayer;
  begin
    if not IsUniquePlayer(APlayers) or (APlayers = [plCurrentPlayer]) then
      raise exception.Create('Main thread player must be one specific player');
    pl := GetUniquePlayer(APlayers);
    if not (pl in[plPlayer1..plPlayer8]) then raise exception.Create('The player '+ PlayerIdentifiers[pl]+' can''t be used as a main thread');
    if AMainThread = plNone then AMainThread := pl else
    if pl <> AMainThread then raise exception.Create('Main thread already defined to be ' + PlayerIdentifiers[AMainThread]);
  end;

var
  inSub, inEvent, inClass, i: integer;
  inSubMain, subMainDeclared, done: boolean;
  curClassPlayers: TPlayers;
  str: string;
  players: TPlayers;
  pl: TPlayer;
  warning: string;
begin
  if not (ADefaultMainThread in[plPlayer1..plPlayer8]) then
    raise exception.Create('This player can''t be used as a main thread');

  ReadProgErrors.Clear;
  ReadProgWarnings.Clear;
  ClearParseCompletionList;
  FillParseCompletionList := true;

  InitVariables;
  ClearProceduresAndEvents;

  AMainThread := plNone;
  ALastScope := GlobalScope;

  PredefineIntArray(GlobalScope,'Ore',suResourceOre,24);
  PredefineIntArray(GlobalScope,'Minerals',suResourceOre,24);
  PredefineIntArray(GlobalScope,'Gas',suResourceGas,24);
  PredefineIntArray(GlobalScope,'OreAndGas',suResourceOreAndGas,24);
  PredefineIntArray(GlobalScope,'MineralsAndGas',suResourceOreAndGas,24);
  PredefineIntArray(GlobalScope,'UnitScore',suScoreUnits,24);
  PredefineIntArray(GlobalScope,'BuildingScore',suScoreBuildings,24);
  PredefineIntArray(GlobalScope,'UnitAndBuildingScore',suScoreUnitsAndBuildings,24);
  PredefineIntArray(GlobalScope,'KillScore',suScoreKills,24);
  PredefineIntArray(GlobalScope,'RazingScore',suScoreRazings,24);
  PredefineIntArray(GlobalScope,'KillAndRazingScore',suScoreKillsAndRazings,24);
  PredefineIntArray(GlobalScope,'CustomScore',suScoreCustom,24);
  PredefineIntArray(GlobalScope,'TotalScore',suScoreTotal,24);
  PredefineIntVar(GlobalScope,'Countdown', plNone, suCountdown,16);
  CreateString(GlobalScope, 'vbCr', #13, true);
  CreateString(GlobalScope, 'vbLf', #10, true);
  CreateString(GlobalScope, 'vbCrLf', #13#10, true);
  CreateString(GlobalScope, 'vbTab', #9, true);
  CreateString(GlobalScope, 'Anywhere', MapInfo.AnywhereLocationName, true);

  fileLineNumber:= 0;
  lineNumber := 0;
  line := nil;

  inSub := -1;
  inEvent := -1;
  inSubMain := false;
  subMainDeclared := false;
  inClass := -1;
  curClassPlayers := [];

  try
    while not Eof and (ReadProgErrors.Count < MAX_ERRORS) do
    begin
      try
        ClearParseCompletionList;
        ReadNextLine;

        if TryToken(line,index,'Class') then
        begin
          if inClass <> -1 then
            raise exception.Create('Nested classes not allowed');
          if (inSub <> -1) or inSubMain or (inEvent <> -1) then
            raise exception.Create('Classes not allowed within functions or events');
          pl := TryParsePlayer([], GlobalScope,line, index);
          if pl = plNone then
          begin
            if not TryIdentifier(line, index, str, false) then
              raise exception.Create('Class identifier expected');
            inClass := ClassIndexOf(str);
            if inClass = -1 then
            begin
              ExpectToken(line,index,'=');
              curClassPlayers := ExpectPlayers([], GlobalScope, line, index);
              inClass := CreateClass(curClassPlayers, str);
            end else
              curClassPlayers:= ClassDefinitions[inClass].Threads;
          end else
          begin
            str:= PlayerIdentifiers[pl];
            curClassPlayers:= [pl];
            inClass := ClassIndexOf(str);
            if inClass = -1 then
              inClass := CreateClass(curClassPlayers, str);
          end;
          CheckEndOfLine;
        end else
        if TryToken(line,index,'Dim') or TryToken(line,index,'Const') then
        begin
          if inEvent <> -1 then Events[inEvent].Code.Add(TCodeLine.Create(line, lineNumber))
          else if inSub <> -1 then Procedures[inSub].Code.Add(TCodeLine.Create(line, lineNumber))
          else if inSubMain then MainCode.Add(TCodeLine.Create(line, lineNumber))
          else
          begin
            ProcessDim([], GlobalScope, line, MainProg, false, warning);
            if warning <> '' then AddWarning(lineNumber, warning);
          end;
        end
        else if (inSub = -1) and (inEvent = -1) and not inSubMain and
         (TryToken(line,index, 'Sub') or TryToken(line,index,'Function')) then
        begin
          inSub := ProcessSubStatement(line, curClassPlayers);
          if inSub = -1 then
          begin
            if subMainDeclared then raise exception.Create('Sub Main already declared');
            inSubMain:= true;
            subMainDeclared := true;
            if inClass <> -1 then SetMainThread(curClassPlayers) else
            if AMainThread = plNone then
            begin
              AMainThread:= ADefaultMainThread;
              AddWarning(lineNumber, 'Main thread implicitely defined to '+PlayerIdentifiers[ADefaultMainThread]);
            end;
          end;
        end
        else if (inSub = -1) and (inEvent = -1) and not inSubMain and (inClass = -1) and
            TryToken(line,index,'As') then
        begin
          players := ExpectPlayers([], GlobalScope, line, index);
          if index < line.Count then
          begin
            for i := index-1 downto 0 do
              line.Delete(i);
            index := 0;
          end else
          begin
            if eof then raise exception.Create('End of file not expected');
            ReadNextLine;
          end;

          if TryToken(line,index,'Sub') or TryToken(line,index,'Function') then
          begin
            inSub := ProcessSubStatement(line, players);
            if inSub = -1 then
            begin
              if subMainDeclared then raise exception.Create('Sub Main already declared');
              inSubMain := true;
              subMainDeclared:= true;
              SetMainThread(players);
            end;
          end else
            inEvent := ProcessEventStatement(line, players);
        end
        else if (inSub = -1) and (inEvent = -1) and not inSubMain and
          TryToken(line,index,'On') then
        begin
          if inClass <> -1 then
            inEvent := ProcessEventStatement(line, curClassPlayers)
          else
          begin
            if AMainThread = plNone then
            begin
              AMainThread:= ADefaultMainThread;
              AddWarning(lineNumber, 'Main thread implicitely defined to '+PlayerIdentifiers[ADefaultMainThread]);
            end;
            inEvent := ProcessEventStatement(line, [AMainThread])
          end;
        end else
        begin
          done := false;
          if not inSubMain and (inSub= -1) and (inEvent = -1) and (inClass = -1) then
          begin
            if PeekToken(line,index,'End') then
              raise exception.Create('Not in any code structure');
          end else
          if TryToken(line,index,'End') then
          begin
            if TryToken(line,index,'Sub') then
            begin
              if inSubMain then
              begin
                try
                  FillParseCompletionList := false;
                  if AMainThread <> plNone then
                    ParseCode([AMainThread], AMainThread, inSubMain, inSub, inEvent)
                  else
                    ParseCode([], AMainThread, inSubMain, inSub, inEvent);
                finally
                  FillParseCompletionList := true;
                  inSubMain := false;
                end;
              end
              else
              begin
                if inSub= -1 then
                  raise Exception.create('Not in a procedure');
                if Procedures[inSub].ReturnType <> 'Void' then raise exception.Create('Expecting "End Function"');
                try
                  FillParseCompletionList := false;
                  ParseCode(Procedures[inSub].Players, AMainThread, inSubMain, inSub, inEvent);
                finally
                  FillParseCompletionList := true;
                  with Procedures[inSub].Instructions do
                    if (Count = 0) or not (Items[Count-1] is TReturnInstruction) then
                      Add(TReturnInstruction.Create);
                  inSub := -1;
                end;
              end;
              CheckEndOfLine;
              done := true;
            end
            else if TryToken(line,index,'Function') then
            begin
              if inSub= -1 then
                raise Exception.create('Not in a function');
              if Procedures[inSub].ReturnType = 'Void' then raise exception.Create('Expecting "End Sub" instead');
              try
                FillParseCompletionList := false;
                ParseCode(Procedures[inSub].Players, AMainThread, inSubMain, inSub, inEvent);
              finally
                FillParseCompletionList := true;
                with Procedures[inSub].Instructions do
                  if (Count = 0) or not (Items[Count-1] is TReturnInstruction) then
                    Add(TReturnInstruction.Create);
                inSub := -1;
              end;
              CheckEndOfLine;
              done := true;
            end
            else if TryToken(line,index,'On') then
            begin
              if inEvent= -1 then
                raise Exception.create('Not in an event');
              try
                FillParseCompletionList := false;
                ParseCode(Events[inEvent].Players, AMainThread, inSubMain, inSub, inEvent);
                with Events[inEvent] do
                  while (Instructions.Count > 0) and (Instructions[Instructions.Count-1] is TReturnInstruction) do
                  begin
                    Instructions[Instructions.Count-1].Free;
                    Instructions.Delete(Instructions.Count-1);
                  end;
              finally
                FillParseCompletionList := true;
                inEvent := -1;
              end;
              CheckEndOfLine;
              done := true;
            end else if TryToken(line, index, 'Class') then
            begin
              if inClass = -1 then
                raise exception.Create('Not in a class');
              if inEvent <> -1 then raise exception.Create('Event not finished');
              if inSubMain or (inSub <> -1) then raise exception.Create('Sub not finished');
              curClassPlayers:= [];
              inClass := -1;
              CheckEndOfLine;
              done := true;
            end;
          end;
          if not done then
          begin
            if inSub<>-1 then Procedures[inSub].Code.Add(TCodeLine.Create(line,lineNumber))
            else if inEvent<>-1 then Events[inEvent].Code.Add(TCodeLine.Create(line,lineNumber))
            else if inSubMain then MainCode.Add(TCodeLine.Create(line,lineNumber))
            else
            begin
              if PeekToken(line,index,'As') then
                raise exception.Create('"As" keyword not allowed within class');

              if not ParseOption(line) then
              begin
                if line.Count > 0 then
                  raise exception.Create('Unexpected instruction. Please put initialization code into Sub Main.');
              end;
            end;
          end;
        end;
      except
        on ex:Exception do
          AddError(lineNumber, ex.Message);
      end;
    end;

    line.Free;

    if ReadProgErrors.Count < MAX_ERRORS then
    begin
      if inSub<>-1 then
      begin
        AddError(lineNumber, 'Sub or Function not finished');
        ParseCode(Procedures[inSub].Players, AMainThread, inSubMain, inSub, inEvent);
        ALastScope:= Procedures[inSub].InnerScope;
      end;

      if inEvent<>-1 then
      begin
        AddError(lineNumber, 'Event not finished');
        ParseCode(Events[inEvent].Players, AMainThread, inSubMain, inSub, inEvent);
        ALastScope:= Events[inEvent].InnerScope;
      end;

      if inSubMain then
      begin
        AddError(lineNumber, 'Sub Main not finished');
        if AMainThread <> plNone then
          ParseCode([AMainThread], AMainThread, inSubMain, inSub, inEvent)
        else
          ParseCode([], AMainThread, inSubMain, inSub, inEvent);
        ALastScope:= SubMainScope;
      end;

      if curClassPlayers <> [] then
        AddError(lineNumber, 'Class ' + str+' not finished');
    end;
  except
    on ex: Exception do
      AddError(lineNumber, ex.message);
  end;

  result := ReadProgErrors.Count = 0;
end;

initialization

  ReadProgErrors := TStringList.Create;
  ReadProgWarnings := TStringList.Create;

finalization

  ReadProgErrors.Free;
  ReadProgWarnings.Free;

end.

