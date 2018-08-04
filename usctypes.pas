unit usctypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSwitchValue = (svClear, svSet, svRandomize, svToggle);

const
  BoolToSwitch : array[Boolean] of TSwitchValue = (svClear, svSet);

type
  TPlayer = (plNone, plPlayer1, plPlayer2, plPlayer3, plPlayer4,
             plPlayer5, plPlayer6, plPlayer7, plPlayer8,
             plPlayer9, plPlayer10, plPlayer11, plPlayer12,
             plUnknown13, plCurrentPlayer, plFoes, plAllies,
             plNeutralPlayers, plAllPlayers,
             plForce1, plForce2, plForce3, plForce4,
             plUnknown23, plUnknown24, plUnknown25, plUnknown26,
             plNonAlliedVictoryPlayers);
  TPlayers = set of TPlayer;

function IntToPlayer(APlayer: integer): TPlayer;

const
  MaxTriggerPlayers = 8;
  PlayerIdentifiers : array[TPlayer] of string =
    ('', 'Player1', 'Player2', 'Player3', 'Player4',
    'Player5', 'Player6', 'Player7', 'Player8',
    'Player9', 'Player10', 'Player11', 'Player12',
    '', 'Me', 'Foes', 'Allies',
    'NeutralPlayers', 'AllPlayers',
    'Force1', 'Force2', 'Force3', 'Force4',
    '', '', '', '', 'NonAlliedVictoryPlayers');

const
  NonKillableUnits: array[1..51] of string =
    ('Data Disc',
    'Goliath Turret',
    'Tank Turret(Tank Mode)',
    'Nuclear Missile',
    'Alan Schezar Turret',
    'Edmund Duke Turret',
    'Tank Turret (Siege Mode)',
    'Scanner Sweep',
    'Unused - Cargo Ship',
    'Unused - Mercenary Gunship',
    'Map Revealer',
    'Disruption Web',
    'Unused Zerg Building1',
    'Unused Zerg Building2',
    'Unused Protoss Building1',
    'Unused Protoss Building2',
    'Khaydarin Crystal Formation',
    'Mineral Field (Type 1)',
    'Mineral Field (Type 2)',
    'Mineral Field (Type 3)',
    'Cave-in',
    'Cantina',
    'Mining Platform',
    'Independant Command Center',
    'Independant Starport',
    'Independant Jump Gate',
    'Ruins',
    'Kyadarin Crystal Formation',
    'Vespene Geyser',
    'Zerg Marker',
    'Terran Marker',
    'Protoss Marker',
    'Zerg Beacon',
    'Terran Beacon',
    'Protoss Beacon',
    'Zerg Flag Beacon',
    'Terran Flag Beacon',
    'Protoss Flag Beacon',
    'Dark Swarm',
    'Floor Hatch',
    'Left Upper Level Door',
    'Right Upper Level Door',
    'Left Pit Door',
    'Right Pit Door',
    'Start Location',
    'Flag',
    'Uraj Crystal',
    'Khalis Crystal',
    'Psi Emitter',
    'Khaydarin Crystal',
    'Cave');

type
  TUnitProperties = record
    Life, Shield, Energy: byte;
    Resource, HangarCount: integer;
    Invincible, Burrowed, Lifted, Hallucinated, Cloaked: boolean;
  end;

function IsUniquePlayer(APlayers: TPlayers): boolean;

implementation

function IntToPlayer(APlayer: integer): TPlayer;
begin
  result := TPlayer(ord(plPlayer1)+APlayer-1);
end;

function IsUniquePlayer(APlayers: TPlayers): boolean;
var count: integer;
  pl: TPlayer;
begin
  if APlayers = [plCurrentPlayer] then exit(true);
  count := 0;
  for pl := succ(plNone) to high(TPlayer) do
  begin
    if pl in APlayers then
    begin
      if pl > plPlayer12 then exit(false);
      inc(count);
    end;
  end;
  exit(count=1);
end;

end.

