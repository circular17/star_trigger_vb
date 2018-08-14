unit utrigedittypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usctypes, umapinfo;

const
  IntegerConditionModeToTrigEditStr: array[TIntegerConditionMode] of string =
    ('At least', 'At most','Exactly');

  StarcraftUnitTrigEditNames : array[low(TStarcraftUnit)..high(TStarcraftUnit)] of string =
   ('Terran Marine',
    'Terran Ghost',
    'Terran Vulture',
    'Terran Goliath',
    'Goliath Turret',
    'Terran Siege Tank (Tank Mode)',
    'Tank Turret type   1',
    'Terran SCV',
    'Terran Wraith',
    'Terran Science Vessel',
    'Gui Montag (Firebat)',
    'Terran Dropship',
    'Terran Battlecruiser',
    'Vulture Spider Mine',
    'Nuclear Missile',
    'Terran Civilian',
    'Sarah Kerrigan (Ghost)',
    'Alan Schezar (Goliath)',
    'Alan Turret',
    'Jim Raynor (Vulture)',
    'Jim Raynor (Marine)',
    'Tom Kazansky (Wraith)',
    'Magellan (Science Vessel)',
    'Edmund Duke (Siege Tank)',
    'Duke Turret type   1',
    'Edmund Duke (Siege Mode)',
    'Duke Turret type   2',
    'Arcturus Mengsk (Battlecruiser)',
    'Hyperion (Battlecruiser)',
    'Norad II (Battlecruiser)',
    'Terran Siege Tank (Siege Mode)',
    'Tank Turret type   2',
    'Terran Firebat',
    'Scanner Sweep',
    'Terran Medic',
    'Zerg Larva',
    'Zerg Egg',
    'Zerg Zergling',
    'Zerg Hydralisk',
    'Zerg Ultralisk',
    'Zerg Broodling',
    'Zerg Drone',
    'Zerg Overlord',
    'Zerg Mutalisk',
    'Zerg Guardian',
    'Zerg Queen',
    'Zerg Defiler',
    'Zerg Scourge',
    'Torrasque (Ultralisk)',
    'Matriarch (Queen)',
    'Infested Terran',
    'Infested Kerrigan (Infested Terran)',
    'Unclean One (Defiler)',
    'Hunter Killer (Hydralisk)',
    'Devouring One (Zergling)',
    'Kukulza (Mutalisk)',
    'Kukulza (Guardian)',
    'Yggdrasill (Overlord)',
    'Terran Valkyrie',
    'Cocoon',
    'Protoss Corsair',
    'Protoss Dark Templar',
    'Zerg Devourer',
    'Protoss Dark Archon',
    'Protoss Probe',
    'Protoss Zealot',
    'Protoss Dragoon',
    'Protoss High Templar',
    'Protoss Archon',
    'Protoss Shuttle',
    'Protoss Scout',
    'Protoss Arbiter',
    'Protoss Carrier',
    'Protoss Interceptor',
    'Dark Templar (Hero)',
    'Zeratul (Dark Templar)',
    'Tassadar/Zeratul (Archon)',
    'Fenix (Zealot)',
    'Fenix (Dragoon)',
    'Tassadar (Templar)',
    'Mojo (Scout)',
    'Warbringer (Reaver)',
    'Gantrithor (Carrier)',
    'Protoss Reaver',
    'Protoss Observer',
    'Protoss Scarab',
    'Danimoth (Arbiter)',
    'Aldaris (Templar)',
    'Artanis (Scout)',
    'Rhynadon (Badlands)',
    'Bengalaas (Jungle)',
    'Unused type   1',
    'Unused type   2',
    'Scantid (Desert)',
    'Kakaru (Twilight)',
    'Ragnasaur (Ash World)',
    'Ursadon (Ice World)',
    'Zerg Lurker Egg',
    'Raszagal (Dark Templar)',
    'Samir Duran (Ghost)',
    'Alexei Stukov (Ghost)',
    'Map Revealer',
    'Gerard DuGalle (Ghost)',
    'Zerg Lurker',
    'Infested Duran',
    'Disruption Field',
    'Terran Command Center',
    'Terran Comsat Station',
    'Terran Nuclear Silo',
    'Terran Supply Depot',
    'Terran Refinery',
    'Terran Barracks',
    'Terran Academy',
    'Terran Factory',
    'Terran Starport',
    'Terran Control Tower',
    'Terran Science Facility',
    'Terran Covert Ops',
    'Terran Physics Lab',
    'Unused Terran Bldg type   1',
    'Terran Machine Shop',
    'Unused Terran Bldg type   2',
    'Terran Engineering Bay',
    'Terran Armory',
    'Terran Missile Turret',
    'Terran Bunker',
    'Norad II (Crashed Battlecruiser)',
    'Ion Cannon',
    'Uraj Crystal',
    'Khalis Crystal',
    'Infested Command Center',
    'Zerg Hatchery',
    'Zerg Lair',
    'Zerg Hive',
    'Zerg Nydus Canal',
    'Zerg Hydralisk Den',
    'Zerg Defiler Mound',
    'Zerg Greater Spire',
    'Zerg Queen''s Nest',
    'Zerg Evolution Chamber',
    'Zerg Ultralisk Cavern',
    'Zerg Spire',
    'Zerg Spawning Pool',
    'Zerg Creep Colony',
    'Zerg Spore Colony',
    'Unused Zerg Bldg',
    'Zerg Sunken Colony',
    'Zerg Overmind (With Shell)',
    'Zerg Overmind',
    'Zerg Extractor',
    'Mature Crysalis',
    'Zerg Cerebrate',
    'Zerg Cerebrate Daggoth',
    'Unused Zerg Bldg 5',
    'Protoss Nexus',
    'Protoss Robotics Facility',
    'Protoss Pylon',
    'Protoss Assimilator',
    'Protoss Unused type   1',
    'Protoss Observatory',
    'Protoss Gateway',
    'Protoss Unused type   2',
    'Protoss Photon Cannon',
    'Protoss Citadel of Adun',
    'Protoss Cybernetics Core',
    'Protoss Templar Archives',
    'Protoss Forge',
    'Protoss Stargate',
    'Stasis Cell/Prison',
    'Protoss Fleet Beacon',
    'Protoss Arbiter Tribunal',
    'Protoss Robotics Support Bay',
    'Protoss Shield Battery',
    'Khaydarin Crystal Formation',
    'Protoss Temple',
    'Xel''Naga Temple',
    'Mineral Field (Type 1)',
    'Mineral Field (Type 2)',
    'Mineral Field (Type 3)',
    'Cave',
    'Cave-in',
    'Cantina',
    'Mining Platform',
    'Independent Command Center',
    'Independent Starport',
    'Jump Gate',
    'Ruins',
    'Kyadarin Crystal Formation',
    'Vespene Geyser',
    'Warp Gate',
    'Psi Disrupter',
    'Zerg Marker',
    'Terran Marker',
    'Protoss Marker',
    'Zerg Beacon',
    'Terran Beacon',
    'Protoss Beacon',
    'Zerg Flag Beacon',
    'Terran Flag Beacon',
    'Protoss Flag Beacon',
    'Power Generator',
    'Overmind Cocoon',
    'Dark Swarm',
    'Floor Missile Trap',
    'Floor Hatch (UNUSED)',
    'Left Upper Level Door',
    'Right Upper Level Door',
    'Left Pit Door',
    'Right Pit Door',
    'Floor Gun Trap',
    'Left Wall Missile Trap',
    'Left Wall Flame Trap',
    'Right Wall Missile Trap',
    'Right Wall Flame Trap',
    'Start Location',
    'Flag',
    'Young Chrysalis',
    'Psi Emitter',
    'Data Disc',
    'Khaydarin Crystal',
    'Mineral Chunk (Type 1)',
    'Mineral Chunk (Type 2)',
    'Vespene Orb (Protoss Type 1)',
    'Vespene Orb (Protoss Type 2)',
    'Vespene Sac (Zerg Type 1)',
    'Vespene Sac (Zerg Type 2)',
    'Vespene Tank (Terran Type 1)',
    'Vespene Tank (Terran Type 2)',
    'None',
    'Any unit',
    'Men',
    'Buildings',
    'Factories',
    'ore',
    'gas',
    'ore and gas',
    'Units',
    'Buildings',
    'Units a nd buildings',
    'Kills',
    'Razings',
    'Kills and razings',
    'Total',
    'Custom',
    'Countdown',
    'Const',
    'Switch');

  SwitchValueToStr : array[TSwitchValue] of string = ('clear','set','randomize','toggle');

function SetIntegerModeToStr(AMode: TSetIntegerMode): string;
function StarcraftResourceToStr(AResource: TStarcraftResource): string;
function StarcraftScoreToStr(AScore: TStarcraftScore): string;
function PlayerToTrigEditStr(APlayer: TPlayer): string;
function AddTrigEditQuotes(AText: string): string;
function SwitchToTrigEditCode(ASwitch: integer): string;

implementation

function SetIntegerModeToStr(AMode: TSetIntegerMode): string;
begin
  case AMode of
  simSetTo: result := 'Set To';
  simSubtract: result := 'Subtract';
  else
    {simAdd: }result := 'Add';
  end;
end;

function StarcraftResourceToStr(AResource: TStarcraftResource): string;
begin
  case AResource of
  srGas: result := 'gas';
  srOreAndGas: result := 'ore and gas';
  else {srOre:} result := 'ore';
  end;
end;

function StarcraftScoreToStr(AScore: TStarcraftScore): string;
begin
  case AScore of
  ssUnitScore: result := 'Units';
  ssBuildingScore: result := 'Buildings';
  ssUnitAndBuildingScore: result := 'Units and buidlings';
  ssKillScore: result := 'Kills';
  ssRazingScore: result := 'Razings';
  ssKillAndRazingScore: result := 'Kills and razings';
  ssTotalScore: result := 'Total';
  ssCustomScore: result := 'Custom';
  end;
end;

function PlayerToTrigEditStr(APlayer: TPlayer): string;
begin
  case APlayer of
  plNone: result := 'None';
  plPlayer1..plPlayer12: result := 'Player ' + IntToStr(ord(APlayer) - ord(plPlayer1)+1);
  plCurrentPlayer: result := 'Current Player';
  plFoes: result := 'Foes';
  plAllies: result := 'Allies';
  plNeutralPlayers: result := 'Neutral players';
  plAllPlayers: result := 'All players';
  plForce1..plForce4: result := MapInfo.ForceName[ord(APlayer)-ord(plForce1)+1];
  plNonAlliedVictoryPlayers: result := 'Non Allied Victory Players';
  else result := 'Unknown';
  end;
end;

function AddTrigEditQuotes(AText: string): string;
var
  i: Integer;
begin
  result := '"' + StringReplace(StringReplace(AText, '\', '\\', [rfReplaceAll]), '"', '\"', [rfReplaceAll]) + '"';
  for i := length(result) downto 1 do
    if result[i] in [#0..#31] then
    begin
      if result[i] = #13 then insert('\r',result,i+1)
      else if result[i] = #10 then insert('\n',result,i+1)
      else insert('\x0' +IntToHex(ord(result[i]),2), result,i+1);
      delete(result,i,1);
    end;
end;

function SwitchToTrigEditCode(ASwitch: integer): string;
begin
  result := AddTrigEditQuotes(MapInfo.SwitchName[ASwitch]);
end;

end.

