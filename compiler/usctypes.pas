unit usctypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  NORMAL_TIME_GRAIN_MS = 1890;
  HYPER_TIME_GRAIN_MS = 125;

type
  TSwitchValue = (svClear, svSet, svRandomize, svToggle);
  ArrayOfSwitchValue = array of TSwitchValue;
  TSetIntegerMode = (simSetTo, simAdd, simSubtract);
  TStarcraftScore = (ssTotalScore, ssUnitScore, ssBuildingScore, ssUnitAndBuildingScore,
                   ssKillScore, ssRazingScore, ssKillAndRazingScore,
                   ssCustomScore);
  TStarcraftResource = (srOre, srGas, srOreAndGas);
  TIntegerConditionMode = (icmAtLeast,icmAtMost,icmExactly);
  TUnitOrder = (uoMove, uoPatrol, uoAttack);
  TAllianceStatus = (asEnnemy, asAlly, asAlliedVictory);

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
function IsUniquePlayer(APlayers: TPlayers): boolean;
function GetUniquePlayer(APlayers: TPlayers): TPlayer;
function AreThreadsIncluded(APlayers, AInPlayers: TPlayers): boolean;
function AreThreadsEqual(APlayers1, APlayers2: TPlayers): boolean;
function GetMaxPlayerNumber(APlayers: TPlayers): integer;

const
  MaxTriggerPlayers = 8;
  MaxSwitches = 256;
  PlayerIdentifiers : array[TPlayer] of string =
    ('', 'Player1', 'Player2', 'Player3', 'Player4',
    'Player5', 'Player6', 'Player7', 'Player8',
    'Player9', 'Player10', 'Player11', 'Player12',
    '', 'Me', 'Foes', 'Allies',
    'NeutralPlayers', 'AllPlayers',
    'Force1', 'Force2', 'Force3', 'Force4',
    '', '', '', '', 'NonAlliedVictoryPlayers');

type
  TUnitProperties = packed record
    Resource, HangarCount: integer;
    Life, Shield, Energy: byte;
    Invincible, Burrowed, Lifted, Hallucinated, Cloaked: ByteBool;
  end;

  TAIScript = record
    Code: string;
    Identifier: string;
  end;

const
  AIScripts: array[1..293] of TAIScript = (
    (Code: 'DWHe'; Identifier: 'CastDisruptionWeb'),
    (Code: 'ReHe'; Identifier: 'CastRecall'),
    (Code: 'ClrC'; Identifier: 'ClearPreviousCombatData'),
    (Code: 'EnBk'; Identifier: 'EnterClosestBunker'),
    (Code: 'EnTr'; Identifier: 'EnterTransport'),
    (Code: 'ExTr'; Identifier: 'ExitTransport'),
    (Code: 'HaHe'; Identifier: 'HarassHere'),
    (Code: 'JYDg'; Identifier: 'JunkYardDog'),
    (Code: 'StPt'; Identifier: 'MakeTheseUnitsPatrol'),
    (Code: 'MvTe'; Identifier: 'MoveDarkTemplarsToRegion'),
    (Code: 'NuHe'; Identifier: 'NukeHere'),
    (Code: 'SuiR'; Identifier: 'SendAllUnitsOnRandomSuicideMissions'),
    (Code: 'Suic'; Identifier: 'SendAllUnitsOnStrategicSuicideMissions'),
    (Code: 'StTg'; Identifier: 'SetGenericCommandTarget'),
    (Code: 'Ally'; Identifier: 'SetPlayerToAllyHere'),
    (Code: 'Enmy'; Identifier: 'SetPlayerToEnemyHere'),
    (Code: 'Rscu'; Identifier: 'SwitchComputerPlayerToRescuePassive'),
    (Code: 'VluA'; Identifier: 'ValueThisAreaHigher'),
    (Code: '-Vi0'; Identifier: 'SharedVision.TurnOffPlayer1'),
    (Code: '-Vi1'; Identifier: 'SharedVision.TurnOffPlayer2'),
    (Code: '-Vi2'; Identifier: 'SharedVision.TurnOffPlayer3'),
    (Code: '-Vi3'; Identifier: 'SharedVision.TurnOffPlayer4'),
    (Code: '-Vi4'; Identifier: 'SharedVision.TurnOffPlayer5'),
    (Code: '-Vi5'; Identifier: 'SharedVision.TurnOffPlayer6'),
    (Code: '-Vi6'; Identifier: 'SharedVision.TurnOffPlayer7'),
    (Code: '-Vi7'; Identifier: 'SharedVision.TurnOffPlayer8'),
    (Code: '+Vi0'; Identifier: 'SharedVision.TurnOnPlayer1'),
    (Code: '+Vi1'; Identifier: 'SharedVision.TurnOnPlayer2'),
    (Code: '+Vi2'; Identifier: 'SharedVision.TurnOnPlayer3'),
    (Code: '+Vi3'; Identifier: 'SharedVision.TurnOnPlayer4'),
    (Code: '+Vi4'; Identifier: 'SharedVision.TurnOnPlayer5'),
    (Code: '+Vi5'; Identifier: 'SharedVision.TurnOnPlayer6'),
    (Code: '+Vi6'; Identifier: 'SharedVision.TurnOnPlayer7'),
    (Code: '+Vi7'; Identifier: 'SharedVision.TurnOnPlayer8'),
    (Code: 'PB1A'; Identifier: 'BroodWar.Protoss1.TownA'),
    (Code: 'PB1B'; Identifier: 'BroodWar.Protoss1.TownB'),
    (Code: 'PB1C'; Identifier: 'BroodWar.Protoss1.TownC'),
    (Code: 'PB1D'; Identifier: 'BroodWar.Protoss1.TownD'),
    (Code: 'PB1E'; Identifier: 'BroodWar.Protoss1.TownE'),
    (Code: 'PB1F'; Identifier: 'BroodWar.Protoss1.TownF'),
    (Code: 'PB2A'; Identifier: 'BroodWar.Protoss2.TownA'),
    (Code: 'PB2B'; Identifier: 'BroodWar.Protoss2.TownB'),
    (Code: 'PB2C'; Identifier: 'BroodWar.Protoss2.TownC'),
    (Code: 'PB2D'; Identifier: 'BroodWar.Protoss2.TownD'),
    (Code: 'PB2E'; Identifier: 'BroodWar.Protoss2.TownE'),
    (Code: 'PB2F'; Identifier: 'BroodWar.Protoss2.TownF'),
    (Code: 'PB3A'; Identifier: 'BroodWar.Protoss3.TownA'),
    (Code: 'PB3B'; Identifier: 'BroodWar.Protoss3.TownB'),
    (Code: 'PB3C'; Identifier: 'BroodWar.Protoss3.TownC'),
    (Code: 'PB3D'; Identifier: 'BroodWar.Protoss3.TownD'),
    (Code: 'PB3E'; Identifier: 'BroodWar.Protoss3.TownE'),
    (Code: 'PB3F'; Identifier: 'BroodWar.Protoss3.TownF'),
    (Code: 'PB4A'; Identifier: 'BroodWar.Protoss4.TownA'),
    (Code: 'PB4B'; Identifier: 'BroodWar.Protoss4.TownB'),
    (Code: 'PB4C'; Identifier: 'BroodWar.Protoss4.TownC'),
    (Code: 'PB4D'; Identifier: 'BroodWar.Protoss4.TownD'),
    (Code: 'PB4E'; Identifier: 'BroodWar.Protoss4.TownE'),
    (Code: 'PB4F'; Identifier: 'BroodWar.Protoss4.TownF'),
    (Code: 'PB5A'; Identifier: 'BroodWar.Protoss5.TownA'),
    (Code: 'PB5B'; Identifier: 'BroodWar.Protoss5.TownB'),
    (Code: 'PB5C'; Identifier: 'BroodWar.Protoss5.TownC'),
    (Code: 'PB5D'; Identifier: 'BroodWar.Protoss5.TownD'),
    (Code: 'PB5E'; Identifier: 'BroodWar.Protoss5.TownE'),
    (Code: 'PB5F'; Identifier: 'BroodWar.Protoss5.TownF'),
    (Code: 'PB6A'; Identifier: 'BroodWar.Protoss6.TownA'),
    (Code: 'PB6B'; Identifier: 'BroodWar.Protoss6.TownB'),
    (Code: 'PB6C'; Identifier: 'BroodWar.Protoss6.TownC'),
    (Code: 'PB6D'; Identifier: 'BroodWar.Protoss6.TownD'),
    (Code: 'PB6E'; Identifier: 'BroodWar.Protoss6.TownE'),
    (Code: 'PB6F'; Identifier: 'BroodWar.Protoss6.TownF'),
    (Code: 'PB7A'; Identifier: 'BroodWar.Protoss7.TownA'),
    (Code: 'PB7B'; Identifier: 'BroodWar.Protoss7.TownB'),
    (Code: 'PB7C'; Identifier: 'BroodWar.Protoss7.TownC'),
    (Code: 'PB7D'; Identifier: 'BroodWar.Protoss7.TownD'),
    (Code: 'PB7E'; Identifier: 'BroodWar.Protoss7.TownE'),
    (Code: 'PB7F'; Identifier: 'BroodWar.Protoss7.TownF'),
    (Code: 'PB8A'; Identifier: 'BroodWar.Protoss8.TownA'),
    (Code: 'PB8B'; Identifier: 'BroodWar.Protoss8.TownB'),
    (Code: 'PB8C'; Identifier: 'BroodWar.Protoss8.TownC'),
    (Code: 'PB8D'; Identifier: 'BroodWar.Protoss8.TownD'),
    (Code: 'PB8E'; Identifier: 'BroodWar.Protoss8.TownE'),
    (Code: 'PB8F'; Identifier: 'BroodWar.Protoss8.TownF'),
    (Code: 'TB1A'; Identifier: 'BroodWar.Terran1.TownA'),
    (Code: 'TB1B'; Identifier: 'BroodWar.Terran1.TownB'),
    (Code: 'TB1C'; Identifier: 'BroodWar.Terran1.TownC'),
    (Code: 'TB1D'; Identifier: 'BroodWar.Terran1.TownD'),
    (Code: 'TB1E'; Identifier: 'BroodWar.Terran1.TownE'),
    (Code: 'TB1F'; Identifier: 'BroodWar.Terran1.TownF'),
    (Code: 'TB2A'; Identifier: 'BroodWar.Terran2.TownA'),
    (Code: 'TB2B'; Identifier: 'BroodWar.Terran2.TownB'),
    (Code: 'TB2C'; Identifier: 'BroodWar.Terran2.TownC'),
    (Code: 'TB2D'; Identifier: 'BroodWar.Terran2.TownD'),
    (Code: 'TB2E'; Identifier: 'BroodWar.Terran2.TownE'),
    (Code: 'TB2F'; Identifier: 'BroodWar.Terran2.TownF'),
    (Code: 'TB3A'; Identifier: 'BroodWar.Terran3.TownA'),
    (Code: 'TB3B'; Identifier: 'BroodWar.Terran3.TownB'),
    (Code: 'TB3C'; Identifier: 'BroodWar.Terran3.TownC'),
    (Code: 'TB3D'; Identifier: 'BroodWar.Terran3.TownD'),
    (Code: 'TB3E'; Identifier: 'BroodWar.Terran3.TownE'),
    (Code: 'TB3F'; Identifier: 'BroodWar.Terran3.TownF'),
    (Code: 'TB4A'; Identifier: 'BroodWar.Terran4.TownA'),
    (Code: 'TB4B'; Identifier: 'BroodWar.Terran4.TownB'),
    (Code: 'TB4C'; Identifier: 'BroodWar.Terran4.TownC'),
    (Code: 'TB4D'; Identifier: 'BroodWar.Terran4.TownD'),
    (Code: 'TB4E'; Identifier: 'BroodWar.Terran4.TownE'),
    (Code: 'TB4F'; Identifier: 'BroodWar.Terran4.TownF'),
    (Code: 'TB5A'; Identifier: 'BroodWar.Terran5.TownA'),
    (Code: 'TB5B'; Identifier: 'BroodWar.Terran5.TownB'),
    (Code: 'TB5C'; Identifier: 'BroodWar.Terran5.TownC'),
    (Code: 'TB5D'; Identifier: 'BroodWar.Terran5.TownD'),
    (Code: 'TB5E'; Identifier: 'BroodWar.Terran5.TownE'),
    (Code: 'TB5F'; Identifier: 'BroodWar.Terran5.TownF'),
    (Code: 'TB6A'; Identifier: 'BroodWar.Terran6.TownA'),
    (Code: 'TB6B'; Identifier: 'BroodWar.Terran6.TownB'),
    (Code: 'TB6C'; Identifier: 'BroodWar.Terran6.TownC'),
    (Code: 'TB6D'; Identifier: 'BroodWar.Terran6.TownD'),
    (Code: 'TB6E'; Identifier: 'BroodWar.Terran6.TownE'),
    (Code: 'TB6F'; Identifier: 'BroodWar.Terran6.TownF'),
    (Code: 'TB7A'; Identifier: 'BroodWar.Terran7.TownA'),
    (Code: 'TB7B'; Identifier: 'BroodWar.Terran7.TownB'),
    (Code: 'TB7C'; Identifier: 'BroodWar.Terran7.TownC'),
    (Code: 'TB7D'; Identifier: 'BroodWar.Terran7.TownD'),
    (Code: 'TB7E'; Identifier: 'BroodWar.Terran7.TownE'),
    (Code: 'TB7F'; Identifier: 'BroodWar.Terran7.TownF'),
    (Code: 'TB8A'; Identifier: 'BroodWar.Terran8.TownA'),
    (Code: 'TB8B'; Identifier: 'BroodWar.Terran8.TownB'),
    (Code: 'TB8C'; Identifier: 'BroodWar.Terran8.TownC'),
    (Code: 'TB8D'; Identifier: 'BroodWar.Terran8.TownD'),
    (Code: 'TB8E'; Identifier: 'BroodWar.Terran8.TownE'),
    (Code: 'TB8F'; Identifier: 'BroodWar.Terran8.TownF'),
    (Code: 'ZB0A'; Identifier: 'BroodWar.Zerg1.TownA'),
    (Code: 'ZB0B'; Identifier: 'BroodWar.Zerg1.TownB'),
    (Code: 'ZB0C'; Identifier: 'BroodWar.Zerg1.TownC'),
    (Code: 'ZB0D'; Identifier: 'BroodWar.Zerg1.TownD'),
    (Code: 'ZB0E'; Identifier: 'BroodWar.Zerg1.TownE'),
    (Code: 'ZB0F'; Identifier: 'BroodWar.Zerg1.TownF'),
    (Code: 'ZB2A'; Identifier: 'BroodWar.Zerg2.TownA'),
    (Code: 'ZB2B'; Identifier: 'BroodWar.Zerg2.TownB'),
    (Code: 'ZB2C'; Identifier: 'BroodWar.Zerg2.TownC'),
    (Code: 'ZB2D'; Identifier: 'BroodWar.Zerg2.TownD'),
    (Code: 'ZB2E'; Identifier: 'BroodWar.Zerg2.TownE'),
    (Code: 'ZB2F'; Identifier: 'BroodWar.Zerg2.TownF'),
    (Code: 'ZB3A'; Identifier: 'BroodWar.Zerg3.TownA'),
    (Code: 'ZB3B'; Identifier: 'BroodWar.Zerg3.TownB'),
    (Code: 'ZB3C'; Identifier: 'BroodWar.Zerg3.TownC'),
    (Code: 'ZB3D'; Identifier: 'BroodWar.Zerg3.TownD'),
    (Code: 'ZB3E'; Identifier: 'BroodWar.Zerg3.TownE'),
    (Code: 'ZB3F'; Identifier: 'BroodWar.Zerg3.TownF'),
    (Code: 'ZB4A'; Identifier: 'BroodWar.Zerg4.TownA'),
    (Code: 'ZB4B'; Identifier: 'BroodWar.Zerg4.TownB'),
    (Code: 'ZB4C'; Identifier: 'BroodWar.Zerg4.TownC'),
    (Code: 'ZB4D'; Identifier: 'BroodWar.Zerg4.TownD'),
    (Code: 'ZB4E'; Identifier: 'BroodWar.Zerg4.TownE'),
    (Code: 'ZB4F'; Identifier: 'BroodWar.Zerg4.TownF'),
    (Code: 'ZB5A'; Identifier: 'BroodWar.Zerg5.TownA'),
    (Code: 'ZB5B'; Identifier: 'BroodWar.Zerg5.TownB'),
    (Code: 'ZB5C'; Identifier: 'BroodWar.Zerg5.TownC'),
    (Code: 'ZB5D'; Identifier: 'BroodWar.Zerg5.TownD'),
    (Code: 'ZB5E'; Identifier: 'BroodWar.Zerg5.TownE'),
    (Code: 'ZB5F'; Identifier: 'BroodWar.Zerg5.TownF'),
    (Code: 'ZB6A'; Identifier: 'BroodWar.Zerg6.TownA'),
    (Code: 'ZB6B'; Identifier: 'BroodWar.Zerg6.TownB'),
    (Code: 'ZB6C'; Identifier: 'BroodWar.Zerg6.TownC'),
    (Code: 'ZB6D'; Identifier: 'BroodWar.Zerg6.TownD'),
    (Code: 'ZB6E'; Identifier: 'BroodWar.Zerg6.TownE'),
    (Code: 'ZB6F'; Identifier: 'BroodWar.Zerg6.TownF'),
    (Code: 'ZB7A'; Identifier: 'BroodWar.Zerg7.TownA'),
    (Code: 'ZB7B'; Identifier: 'BroodWar.Zerg7.TownB'),
    (Code: 'ZB7C'; Identifier: 'BroodWar.Zerg7.TownC'),
    (Code: 'ZB7D'; Identifier: 'BroodWar.Zerg7.TownD'),
    (Code: 'ZB7E'; Identifier: 'BroodWar.Zerg7.TownE'),
    (Code: 'ZB7F'; Identifier: 'BroodWar.Zerg7.TownF'),
    (Code: 'ZB8A'; Identifier: 'BroodWar.Zerg8.TownA'),
    (Code: 'ZB8B'; Identifier: 'BroodWar.Zerg8.TownB'),
    (Code: 'ZB8C'; Identifier: 'BroodWar.Zerg8.TownC'),
    (Code: 'ZB8D'; Identifier: 'BroodWar.Zerg8.TownD'),
    (Code: 'ZB8E'; Identifier: 'BroodWar.Zerg8.TownE'),
    (Code: 'ZB8F'; Identifier: 'BroodWar.Zerg8.TownF'),
    (Code: 'ZB9A'; Identifier: 'BroodWar.Zerg9.TownA'),
    (Code: 'ZB9B'; Identifier: 'BroodWar.Zerg9.TownB'),
    (Code: 'ZB9C'; Identifier: 'BroodWar.Zerg9.TownC'),
    (Code: 'ZB9D'; Identifier: 'BroodWar.Zerg9.TownD'),
    (Code: 'ZB9E'; Identifier: 'BroodWar.Zerg9.TownE'),
    (Code: 'ZB9F'; Identifier: 'BroodWar.Zerg9.TownF'),
    (Code: 'ZB1A'; Identifier: 'BroodWar.Zerg10.TownA'),
    (Code: 'ZB1B'; Identifier: 'BroodWar.Zerg10.TownB'),
    (Code: 'ZB1C'; Identifier: 'BroodWar.Zerg10.TownC'),
    (Code: 'ZB1D'; Identifier: 'BroodWar.Zerg10.TownD'),
    (Code: 'ZB1E'; Identifier: 'BroodWar.Zerg10.TownE'),
    (Code: 'ZB1F'; Identifier: 'BroodWar.Zerg10.TownF'),
    (Code: 'PARx'; Identifier: 'Expansion.ProtossCampaign.AreaTown'),
    (Code: 'PHIx'; Identifier: 'Expansion.ProtossCampaign.Difficult'),
    (Code: 'PLOx'; Identifier: 'Expansion.ProtossCampaign.Easy'),
    (Code: 'PSUx'; Identifier: 'Expansion.ProtossCampaign.Insane'),
    (Code: 'PMEx'; Identifier: 'Expansion.ProtossCampaign.Medium'),
    (Code: 'PMCx'; Identifier: 'Expansion.ProtossCustomLevel'),
    (Code: 'TARx'; Identifier: 'Expansion.TerranCampaign.AreaTown'),
    (Code: 'THIx'; Identifier: 'Expansion.TerranCampaign.Difficult'),
    (Code: 'TLOx'; Identifier: 'Expansion.TerranCampaign.Easy'),
    (Code: 'TSUx'; Identifier: 'Expansion.TerranCampaign.Insane'),
    (Code: 'TMEx'; Identifier: 'Expansion.TerranCampaign.Medium'),
    (Code: 'TMCx'; Identifier: 'Expansion.TerranCustomLevel'),
    (Code: 'ZARx'; Identifier: 'Expansion.ZergCampaign.AreaTown'),
    (Code: 'ZHIx'; Identifier: 'Expansion.ZergCampaign.Difficult'),
    (Code: 'ZLOx'; Identifier: 'Expansion.ZergCampaign.Easy'),
    (Code: 'ZSUx'; Identifier: 'Expansion.ZergCampaign.Insane'),
    (Code: 'ZMEx'; Identifier: 'Expansion.ZergCampaign.Medium'),
    (Code: 'ZMCx'; Identifier: 'Expansion.ZergCustomLevel'),
    (Code: 'P10o'; Identifier: 'Original.Protoss1.ZergTown'),
    (Code: 'Pro2'; Identifier: 'Original.Protoss2.ZergTown'),
    (Code: 'Pr3R'; Identifier: 'Original.Protoss3.AirZergTown'),
    (Code: 'Pr3G'; Identifier: 'Original.Protoss3.GroundZergTown'),
    (Code: 'Pro4'; Identifier: 'Original.Protoss4.ZergTown'),
    (Code: 'Pr5B'; Identifier: 'Original.Protoss5.ZergTownBase'),
    (Code: 'Pr5I'; Identifier: 'Original.Protoss5.ZergTownIsland'),
    (Code: 'Pro7'; Identifier: 'Original.Protoss7.LeftProtossTown'),
    (Code: 'Pr7B'; Identifier: 'Original.Protoss7.RightProtossTown'),
    (Code: 'Pr7S'; Identifier: 'Original.Protoss7.ShrineProtoss'),
    (Code: 'Pro8'; Identifier: 'Original.Protoss8.LeftProtossTown'),
    (Code: 'Pr8D'; Identifier: 'Original.Protoss8.ProtossDefenders'),
    (Code: 'Pr8B'; Identifier: 'Original.Protoss8.RightProtossTown'),
    (Code: 'Pr9W'; Identifier: 'Original.Protoss9.AirZerg'),
    (Code: 'Pro9'; Identifier: 'Original.Protoss9.GroundZerg'),
    (Code: 'Pr9Y'; Identifier: 'Original.Protoss9.SpellZerg'),
    (Code: 'Pro1'; Identifier: 'Original.Protoss10.MiniTownMaster'),
    (Code: 'P10C'; Identifier: 'Original.Protoss10.MiniTowns'),
    (Code: 'Pr10'; Identifier: 'Original.Protoss10.OvermindDefenders'),
    (Code: 'PARE'; Identifier: 'ProtossCampaign.AreaTown'),
    (Code: 'PHIf'; Identifier: 'ProtossCampaign.Difficult'),
    (Code: 'PLOf'; Identifier: 'ProtossCampaign.Easy'),
    (Code: 'PSUP'; Identifier: 'ProtossCampaign.Insane'),
    (Code: 'PMED'; Identifier: 'ProtossCampaign.Medium'),
    (Code: 'PMCu'; Identifier: 'ProtossCustomLevel'),
    (Code: 'T12P'; Identifier: 'Original.Terran1.ElectronicDistribution'),
    (Code: 'T12T'; Identifier: 'Original.Terran1.Shareware'),
    (Code: 'TED2'; Identifier: 'Original.Terran2.ElectronicDistribution'),
    (Code: 'TSW2'; Identifier: 'Original.Terran2.Shareware'),
    (Code: 'TED3'; Identifier: 'Original.Terran3.ElectronicDistribution'),
    (Code: 'TSW3'; Identifier: 'Original.Terran3.Shareware'),
    (Code: 'Ter3'; Identifier: 'Original.Terran3.ZergTown'),
    (Code: 'TSW4'; Identifier: 'Original.Terran4.Shareware'),
    (Code: 'TSW5'; Identifier: 'Original.Terran5.Shareware'),
    (Code: 'Te5H'; Identifier: 'Original.Terran5.TerranHarvestTown'),
    (Code: 'Ter5'; Identifier: 'Original.Terran5.TerranMainTown'),
    (Code: 'Ter6'; Identifier: 'Original.Terran6.AirAttackZerg'),
    (Code: 'Te6b'; Identifier: 'Original.Terran6.GroundAttackZerg'),
    (Code: 'Te6c'; Identifier: 'Original.Terran6.ZergSupportTown'),
    (Code: 'Ter7'; Identifier: 'Original.Terran7.BottomZergTown'),
    (Code: 'Te7m'; Identifier: 'Original.Terran7.MiddleZergTown'),
    (Code: 'Te7s'; Identifier: 'Original.Terran7.RightZergTown'),
    (Code: 'Ter8'; Identifier: 'Original.Terran8.CondeferateTown'),
    (Code: 'Tr9H'; Identifier: 'Original.Terran9.HeavyAttack'),
    (Code: 'Tr9L'; Identifier: 'Original.Terran9.LightAttack'),
    (Code: 'TED1'; Identifier: 'Original.Terran10.CondeferateTowns'),
    (Code: 'TSW1'; Identifier: 'Original.Terran11.LowerProtossTown'),
    (Code: 'Te10'; Identifier: 'Original.Terran11.UpperProtossTown'),
    (Code: 'T11a'; Identifier: 'Original.Terran11.ZergTown'),
    (Code: 'T11b'; Identifier: 'Original.Terran12.NukeTown'),
    (Code: 'T11z'; Identifier: 'Original.Terran12.PhoenixTown'),
    (Code: 'T12N'; Identifier: 'Original.Terran12.TankTown'),
    (Code: 'TARE'; Identifier: 'TerranCampaign.AreaTown'),
    (Code: 'THIf'; Identifier: 'TerranCampaign.Difficult'),
    (Code: 'TLOf'; Identifier: 'TerranCampaign.Easy'),
    (Code: 'TSUP'; Identifier: 'TerranCampaign.Insane'),
    (Code: 'TMED'; Identifier: 'TerranCampaign.Medium'),
    (Code: 'TMCu'; Identifier: 'TerranCustomLevel'),
    (Code: 'Zer1'; Identifier: 'Original.Zerg1.TerranTown'),
    (Code: 'Zer2'; Identifier: 'Original.Zerg2.ProtossTown'),
    (Code: 'Zer3'; Identifier: 'Original.Zerg3.TerranTown'),
    (Code: 'Ze4S'; Identifier: 'Original.Zerg4.LowerTerranTown'),
    (Code: 'Zer4'; Identifier: 'Original.Zerg4.RightTerranTown'),
    (Code: 'Zer6'; Identifier: 'Original.Zerg6.ProtossTown'),
    (Code: 'Zr7a'; Identifier: 'Original.Zerg7.AirTown'),
    (Code: 'Zr7g'; Identifier: 'Original.Zerg7.GroundTown'),
    (Code: 'Zr7s'; Identifier: 'Original.Zerg7.SupportTown'),
    (Code: 'Zer8'; Identifier: 'Original.Zerg8.ScoutTown'),
    (Code: 'Ze8T'; Identifier: 'Original.Zerg8.TemplarTown'),
    (Code: 'Z9lo'; Identifier: 'Original.Zerg9.LeftOrangeProtoss'),
    (Code: 'Z9ly'; Identifier: 'Original.Zerg9.LeftYellowProtoss'),
    (Code: 'Z9ro'; Identifier: 'Original.Zerg9.RightOrangeProtoss'),
    (Code: 'Z9ry'; Identifier: 'Original.Zerg9.RightYellowProtoss'),
    (Code: 'Zer9'; Identifier: 'Original.Zerg9.TealProtoss'),
    (Code: 'Z10a'; Identifier: 'Original.Zerg10.LeftTealAttack'),
    (Code: 'Z10c'; Identifier: 'Original.Zerg10.LeftYellowSupport'),
    (Code: 'Z10e'; Identifier: 'Original.Zerg10.RedProtoss'),
    (Code: 'Z10b'; Identifier: 'Original.Zerg10.RightTealSupport'),
    (Code: 'Z10d'; Identifier: 'Original.Zerg10.RightYellowAttack'),
    (Code: 'ZARE'; Identifier: 'ZergCampaign.AreaTown'),
    (Code: 'ZHIf'; Identifier: 'ZergCampaign.Difficult'),
    (Code: 'ZLOf'; Identifier: 'ZergCampaign.Easy'),
    (Code: 'ZSUP'; Identifier: 'ZergCampaign.Insane'),
    (Code: 'ZMED'; Identifier: 'ZergCampaign.Medium'),
    (Code: 'ZMCu'; Identifier: 'ZergCustomLevel') );

type TStarcraftUnit =
   (suTerranMarine,
    suTerranGhost,
    suTerranVulture,
    suTerranGoliath,
    suTerranGoliathTurret,
    suTerranSiegeTankTankMode,
    suTerranSiegeTankTankModeTurret,
    suTerranSCV,
    suTerranWraith,
    suTerranScienceVessel,
    suHeroGuiMontag,
    suTerranDropship,
    suTerranBattlecruiser,
    suTerranVultureSpiderMine,
    suTerranNuclearMissile,
    suTerranCivilian,
    suHeroSarahKerrigan,
    suHeroAlanSchezar,
    suHeroAlanSchezarTurret,
    suHeroJimRaynorVulture,
    suHeroJimRaynorMarine,
    suHeroTomKazansky,
    suHeroMagellan,
    suHeroEdmundDukeTankMode,
    suHeroEdmundDukeTankModeTurret,
    suHeroEdmundDukeSiegeMode,
    suHeroEdmundDukeSiegeModeTurret,
    suHeroArcturusMengsk,
    suHeroHyperion,
    suHeroNoradII,
    suTerranSiegeTankSiegeMode,
    suTerranSiegeTankSiegeModeTurret,
    suTerranFirebat,
    suSpellScannerSweep,
    suTerranMedic,
    suZergLarva,
    suZergEgg,
    suZergZergling,
    suZergHydralisk,
    suZergUltralisk,
    suZergBroodling,
    suZergDrone,
    suZergOverlord,
    suZergMutalisk,
    suZergGuardian,
    suZergQueen,
    suZergDefiler,
    suZergScourge,
    suHeroTorrasque,
    suHeroMatriarch,
    suZergInfestedTerran,
    suHeroInfestedKerrigan,
    suHeroUncleanOne,
    suHeroHunterKiller,
    suHeroDevouringOne,
    suHeroKukulzaMutalisk,
    suHeroKukulzaGuardian,
    suHeroYggdrasill,
    suTerranValkyrie,
    suZergCocoon,
    suProtossCorsair,
    suProtossDarkTemplar,
    suZergDevourer,
    suProtossDarkArchon,
    suProtossProbe,
    suProtossZealot,
    suProtossDragoon,
    suProtossHighTemplar,
    suProtossArchon,
    suProtossShuttle,
    suProtossScout,
    suProtossArbiter,
    suProtossCarrier,
    suProtossInterceptor,
    suHeroDarkTemplar,
    suHeroZeratul,
    suHeroTassadarZeratulArchon,
    suHeroFenixZealot,
    suHeroFenixDragoon,
    suHeroTassadar,
    suHeroMojo,
    suHeroWarbringer,
    suHeroGantrithor,
    suProtossReaver,
    suProtossObserver,
    suProtossScarab,
    suHeroDanimoth,
    suHeroAldaris,
    suHeroArtanis,
    suCritterRhynadon,
    suCritterBengalaas,
    suSpecialCargoShip,
    suSpecialMercenaryGunship,
    suCritterScantid,
    suCritterKakaru,
    suCritterRagnasaur,
    suCritterUrsadon,
    suZergLurkerEgg,
    suHeroRaszagal,
    suHeroSamirDuran,
    suHeroAlexeiStukov,
    suSpecialMapRevealer,
    suHeroGerardDuGalle,
    suZergLurker,
    suHeroInfestedDuran,
    suSpellDisruptionWeb,
    suTerranCommandCenter,
    suTerranComsatStation,
    suTerranNuclearSilo,
    suTerranSupplyDepot,
    suTerranRefinery,
    suTerranBarracks,
    suTerranAcademy,
    suTerranFactory,
    suTerranStarport,
    suTerranControlTower,
    suTerranScienceFacility,
    suTerranCovertOps,
    suTerranPhysicsLab,
    suUnusedTerran1,
    suTerranMachineShop,
    suUnusedTerran2,
    suTerranEngineeringBay,
    suTerranArmory,
    suTerranMissileTurret,
    suTerranBunker,
    suSpecialCrashedNoradII,
    suSpecialIonCannon,
    suPowerupUrajCrystal,
    suPowerupKhalisCrystal,
    suZergInfestedCommandCenter,
    suZergHatchery,
    suZergLair,
    suZergHive,
    suZergNydusCanal,
    suZergHydraliskDen,
    suZergDefilerMound,
    suZergGreaterSpire,
    suZergQueensNest,
    suZergEvolutionChamber,
    suZergUltraliskCavern,
    suZergSpire,
    suZergSpawningPool,
    suZergCreepColony,
    suZergSporeColony,
    suUnusedZerg1,
    suZergSunkenColony,
    suSpecialOvermindWithShell,
    suSpecialOvermind,
    suZergExtractor,
    suSpecialMatureChrysalis,
    suSpecialCerebrate,
    suSpecialCerebrateDaggoth,
    suUnusedZerg2,
    suProtossNexus,
    suProtossRoboticsFacility,
    suProtossPylon,
    suProtossAssimilator,
    suUnusedProtoss1,
    suProtossObservatory,
    suProtossGateway,
    suUnusedProtoss2,
    suProtossPhotonCannon,
    suProtossCitadelofAdun,
    suProtossCyberneticsCore,
    suProtossTemplarArchives,
    suProtossForge,
    suProtossStargate,
    suSpecialStasisCellPrison,
    suProtossFleetBeacon,
    suProtossArbiterTribunal,
    suProtossRoboticsSupportBay,
    suProtossShieldBattery,
    suSpecialKhaydarinCrystalForm,
    suSpecialProtossTemple,
    suSpecialXelNagaTemple,
    suResourceMineralField1,
    suResourceMineralField2,
    suResourceMineralField3,
    suUnusedCave,
    suUnusedCaveIn,
    suUnusedCantina,
    suUnusedMiningPlatform,
    suUnusedIndependantCommandCenter,
    suSpecialIndependantStarport,
    suUnusedIndependantJumpGate,
    suUnusedRuins,
    suUnusedKhaydarinCrystalFormation,
    suResourceVespeneGeyser,
    suSpecialWarpGate,
    suSpecialPsiDisrupter,
    suUnusedZergMarker,
    suUnusedTerranMarker,
    suUnusedProtossMarker,
    suSpecialZergBeacon,
    suSpecialTerranBeacon,
    suSpecialProtossBeacon,
    suSpecialZergFlagBeacon,
    suSpecialTerranFlagBeacon,
    suSpecialProtossFlagBeacon,
    suSpecialPowerGenerator,
    suSpecialOvermindCocoon,
    suSpellDarkSwarm,
    suSpecialFloorMissileTrap,
    suSpecialFloorHatch,
    suSpecialLeftUpperLevelDoor,
    suSpecialRightUpperLevelDoor,
    suSpecialLeftPitDoor,
    suSpecialRightPitDoor,
    suSpecialFloorGunTrap,
    suSpecialWallMissileTrap,
    suSpecialWallFlameTrap,
    suSpecialRightWallMissileTrap,
    suSpecialRightWallFlameTrap,
    suSpecialStartLocation,
    suPowerupFlag,
    suPowerupYoungChrysalis,
    suPowerupPsiEmitter,
    suPowerupDataDisk,
    suPowerupKhaydarinCrystal,
    suPowerupMineralClusterType1,
    suPowerupMineralClusterType2,
    suPowerupProtossGasOrbType1,
    suPowerupProtossGasOrbType2,
    suPowerupZergGasSacType1,
    suPowerupZergGasSacType2,
    suPowerupTerranGasTankType1,
    suPowerupTerranGasTankType2,
    suNone,
    suAnyUnit,
    suMen,
    suBuildings,
    suFactories,
    suResourceOre,
    suResourceGas,
    suResourceOreAndGas,
    suScoreUnits,
    suScoreBuildings,
    suScoreUnitsAndBuildings,
    suScoreKills,
    suScoreRazings,
    suScoreKillsAndRazings,
    suScoreTotal,
    suScoreCustom,
    suCountdown,
    suConst,
    suSwitch);

const
  StarcraftUnitIdentifier: array[low(TStarcraftUnit)..high(TStarcraftUnit)] of string =
   ('Terran.Marine',
    'Terran.Ghost',
    'Terran.Vulture',
    'Terran.Goliath',
    'Terran.GoliathTurret',
    'Terran.SiegeTankTankMode',
    'Terran.SiegeTankTankModeTurret',
    'Terran.SCV',
    'Terran.Wraith',
    'Terran.ScienceVessel',
    'Hero.GuiMontag',
    'Terran.Dropship',
    'Terran.Battlecruiser',
    'Terran.VultureSpiderMine',
    'Terran.NuclearMissile',
    'Terran.Civilian',
    'Hero.SarahKerrigan',
    'Hero.AlanSchezar',
    'Hero.AlanSchezarTurret',
    'Hero.JimRaynorVulture',
    'Hero.JimRaynorMarine',
    'Hero.TomKazansky',
    'Hero.Magellan',
    'Hero.EdmundDukeTankMode',
    'Hero.EdmundDukeTankModeTurret',
    'Hero.EdmundDukeSiegeMode',
    'Hero.EdmundDukeSiegeModeTurret',
    'Hero.ArcturusMengsk',
    'Hero.Hyperion',
    'Hero.NoradII',
    'Terran.SiegeTankSiegeMode',
    'Terran.SiegeTankSiegeModeTurret',
    'Terran.Firebat',
    'Spell.ScannerSweep',
    'Terran.Medic',
    'Zerg.Larva',
    'Zerg.Egg',
    'Zerg.Zergling',
    'Zerg.Hydralisk',
    'Zerg.Ultralisk',
    'Zerg.Broodling',
    'Zerg.Drone',
    'Zerg.Overlord',
    'Zerg.Mutalisk',
    'Zerg.Guardian',
    'Zerg.Queen',
    'Zerg.Defiler',
    'Zerg.Scourge',
    'Hero.Torrasque',
    'Hero.Matriarch',
    'Zerg.InfestedTerran',
    'Hero.InfestedKerrigan',
    'Hero.UncleanOne',
    'Hero.HunterKiller',
    'Hero.DevouringOne',
    'Hero.KukulzaMutalisk',
    'Hero.KukulzaGuardian',
    'Hero.Yggdrasill',
    'Terran.Valkyrie',
    'Zerg.Cocoon',
    'Protoss.Corsair',
    'Protoss.DarkTemplar',
    'Zerg.Devourer',
    'Protoss.DarkArchon',
    'Protoss.Probe',
    'Protoss.Zealot',
    'Protoss.Dragoon',
    'Protoss.HighTemplar',
    'Protoss.Archon',
    'Protoss.Shuttle',
    'Protoss.Scout',
    'Protoss.Arbiter',
    'Protoss.Carrier',
    'Protoss.Interceptor',
    'Hero.DarkTemplar',
    'Hero.Zeratul',
    'Hero.TassadarZeratulArchon',
    'Hero.FenixZealot',
    'Hero.FenixDragoon',
    'Hero.Tassadar',
    'Hero.Mojo',
    'Hero.Warbringer',
    'Hero.Gantrithor',
    'Protoss.Reaver',
    'Protoss.Observer',
    'Protoss.Scarab',
    'Hero.Danimoth',
    'Hero.Aldaris',
    'Hero.Artanis',
    'Critter.Rhynadon',
    'Critter.Bengalaas',
    'Special.CargoShip',
    'Special.MercenaryGunship',
    'Critter.Scantid',
    'Critter.Kakaru',
    'Critter.Ragnasaur',
    'Critter.Ursadon',
    'Zerg.LurkerEgg',
    'Hero.Raszagal',
    'Hero.SamirDuran',
    'Hero.AlexeiStukov',
    'Special.MapRevealer',
    'Hero.GerardDuGalle',
    'Zerg.Lurker',
    'Hero.InfestedDuran',
    'Spell.DisruptionWeb',
    'Terran.CommandCenter',
    'Terran.ComsatStation',
    'Terran.NuclearSilo',
    'Terran.SupplyDepot',
    'Terran.Refinery',
    'Terran.Barracks',
    'Terran.Academy',
    'Terran.Factory',
    'Terran.Starport',
    'Terran.ControlTower',
    'Terran.ScienceFacility',
    'Terran.CovertOps',
    'Terran.PhysicsLab',
    'Unused.Terran1',
    'Terran.MachineShop',
    'Unused.Terran2',
    'Terran.EngineeringBay',
    'Terran.Armory',
    'Terran.MissileTurret',
    'Terran.Bunker',
    'Special.CrashedNoradII',
    'Special.IonCannon',
    'Powerup.UrajCrystal',
    'Powerup.KhalisCrystal',
    'Zerg.InfestedCommandCenter',
    'Zerg.Hatchery',
    'Zerg.Lair',
    'Zerg.Hive',
    'Zerg.NydusCanal',
    'Zerg.HydraliskDen',
    'Zerg.DefilerMound',
    'Zerg.GreaterSpire',
    'Zerg.QueensNest',
    'Zerg.EvolutionChamber',
    'Zerg.UltraliskCavern',
    'Zerg.Spire',
    'Zerg.SpawningPool',
    'Zerg.CreepColony',
    'Zerg.SporeColony',
    'Unused.Zerg1',
    'Zerg.SunkenColony',
    'Special.OvermindWithShell',
    'Special.Overmind',
    'Zerg.Extractor',
    'Special.MatureChrysalis',
    'Special.Cerebrate',
    'Special.CerebrateDaggoth',
    'Unused.Zerg2',
    'Protoss.Nexus',
    'Protoss.RoboticsFacility',
    'Protoss.Pylon',
    'Protoss.Assimilator',
    'Unused.Protoss1',
    'Protoss.Observatory',
    'Protoss.Gateway',
    'Unused.Protoss2',
    'Protoss.PhotonCannon',
    'Protoss.CitadelofAdun',
    'Protoss.CyberneticsCore',
    'Protoss.TemplarArchives',
    'Protoss.Forge',
    'Protoss.Stargate',
    'Special.StasisCellPrison',
    'Protoss.FleetBeacon',
    'Protoss.ArbiterTribunal',
    'Protoss.RoboticsSupportBay',
    'Protoss.ShieldBattery',
    'Special.KhaydarinCrystalForm',
    'Special.ProtossTemple',
    'Special.XelNagaTemple',
    'Resource.MineralField1',
    'Resource.MineralField2',
    'Resource.MineralField3',
    'Unused.Cave',
    'Unused.CaveIn',
    'Unused.Cantina',
    'Unused.MiningPlatform',
    'Unused.IndependantCommandCenter',
    'Special.IndependantStarport',
    'Unused.IndependantJumpGate',
    'Unused.Ruins',
    'Unused.KhaydarinCrystalFormation',
    'Resource.VespeneGeyser',
    'Special.WarpGate',
    'Special.PsiDisrupter',
    'Unused.ZergMarker',
    'Unused.TerranMarker',
    'Unused.ProtossMarker',
    'Special.ZergBeacon',
    'Special.TerranBeacon',
    'Special.ProtossBeacon',
    'Special.ZergFlagBeacon',
    'Special.TerranFlagBeacon',
    'Special.ProtossFlagBeacon',
    'Special.PowerGenerator',
    'Special.OvermindCocoon',
    'Spell.DarkSwarm',
    'Special.FloorMissileTrap',
    'Special.FloorHatch',
    'Special.LeftUpperLevelDoor',
    'Special.RightUpperLevelDoor',
    'Special.LeftPitDoor',
    'Special.RightPitDoor',
    'Special.FloorGunTrap',
    'Special.WallMissileTrap',
    'Special.WallFlameTrap',
    'Special.RightWallMissileTrap',
    'Special.RightWallFlameTrap',
    'Special.StartLocation',
    'Powerup.Flag',
    'Powerup.YoungChrysalis',
    'Powerup.PsiEmitter',
    'Powerup.DataDisk',
    'Powerup.KhaydarinCrystal',
    'Powerup.MineralClusterType1',
    'Powerup.MineralClusterType2',
    'Powerup.ProtossGasOrbType1',
    'Powerup.ProtossGasOrbType2',
    'Powerup.ZergGasSacType1',
    'Powerup.ZergGasSacType2',
    'Powerup.TerranGasTankType1',
    'Powerup.TerranGasTankType2',
    'NoUnit',
    'AnyUnit',
    'Men',
    'Buildings',
    'Factories',
    'ResourceOre',
    'ResourceGas',
    'ResourceOreAndGas',
    'ScoreUnits',
    'ScoreBuildings',
    'ScoreUnitsAndBuildings',
    'ScoreKills',
    'ScoreRazings',
    'ScoreKillsAndRazings',
    'ScoreTotal',
    'ScoreCustom',
    'Countdown',
    'Const',
    'Switch');

var
  StarcraftUnitPrefixes: TStringList;
  StarcraftUnitSplitIdentifier: array[TStarcraftUnit] of record
    Prefix: string;
    PartialName: string;
  end;

const
  NonKillableUnits: array[1..52] of TStarcraftUnit =
   (suPowerupDataDisk,
    suPowerupFlag,
    suPowerupUrajCrystal,
    suPowerupKhalisCrystal,
    suPowerupKhaydarinCrystal,
    suPowerupPsiEmitter,
    suResourceMineralField1,
    suResourceMineralField2,
    suResourceMineralField3,
    suResourceVespeneGeyser,
    suSpecialZergBeacon,
    suSpecialTerranBeacon,
    suSpecialProtossBeacon,
    suSpecialZergFlagBeacon,
    suSpecialTerranFlagBeacon,
    suSpecialProtossFlagBeacon,
    suTerranGoliathTurret,
    suTerranSiegeTankTankModeTurret,
    suTerranSiegeTankSiegeModeTurret,
    suTerranNuclearMissile,
    suHeroAlanSchezarTurret,
    suHeroEdmundDukeSiegeModeTurret,
    suHeroEdmundDukeTankModeTurret,
    suSpecialCargoShip,
    suSpecialMercenaryGunship,
    suSpecialMapRevealer,
    suUnusedZerg1,
    suUnusedZerg2,
    suUnusedProtoss1,
    suUnusedProtoss2,
    suSpecialKhaydarinCrystalForm,
    suUnusedCantina,
    suUnusedMiningPlatform,
    suUnusedIndependantCommandCenter,
    suUnusedIndependantJumpGate,
    suSpecialIndependantStarport,
    suUnusedRuins,
    suUnusedKhaydarinCrystalFormation,
    suUnusedZergMarker,
    suUnusedTerranMarker,
    suUnusedProtossMarker,
    suSpecialFloorHatch,
    suSpecialLeftUpperLevelDoor,
    suSpecialRightUpperLevelDoor,
    suSpecialLeftPitDoor,
    suSpecialRightPitDoor,
    suSpecialStartLocation,
    suUnusedCaveIn,
    suUnusedCave,
    suSpellScannerSweep,
    suSpellDisruptionWeb,
    suSpellDarkSwarm);

function IsUnitType(AName: string): boolean;

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

function GetUniquePlayer(APlayers: TPlayers): TPlayer;
var
  pl: TPlayer;
begin
  if IsUniquePlayer(APlayers) then
  begin
    for pl := succ(plNone) to high(TPlayer) do
      if pl in APlayers then exit(pl);
  end else
    exit(plNone);
end;

function AreThreadsIncluded(APlayers, AInPlayers: TPlayers): boolean;
var
  pl: TPlayer;
begin
  if plAllPlayers in AInPlayers then exit(true);
  for pl := succ(plNone) to high(TPlayer) do
    if pl in APlayers then
    begin
      if not (pl in AInPlayers) then
      begin
        if (pl = plAllPlayers) then
        begin
          if not ([plPlayer1,plPlayer2,plPlayer3,plPlayer4,
                 plPlayer5,plPlayer6,plPlayer7,plPlayer8] <= AInPlayers)
           then exit(false);
        end
          else exit(false);
      end;
    end;
  result := true;
end;

function AreThreadsEqual(APlayers1, APlayers2: TPlayers): boolean;
begin
  result := AreThreadsIncluded(APlayers1, APlayers2) and
    AreThreadsIncluded(APlayers2, APlayers1);
end;

function GetMaxPlayerNumber(APlayers: TPlayers): integer;
var
  pl: TPlayer;
begin
  result := 0;
  for pl := succ(plNone) to high(TPlayer) do
    if (pl in APlayers) then
    begin
      if pl in[plPlayer1..plPlayer12] then
        result := ord(pl)-ord(plPlayer1)+1
      else
        result := 12;
    end;
end;

procedure InitStarcraftUnitPrefixes;
var
  posDot, idx: integer;
  prefix: String;
  u: TStarcraftUnit;
begin
  StarcraftUnitPrefixes := TStringList.Create;
  for u := low(StarcraftUnitIdentifier) to high(StarcraftUnitIdentifier) do
  begin
    posDot := pos('.', StarcraftUnitIdentifier[u]);
    if posDot > 0 then
    begin
      prefix := copy(StarcraftUnitIdentifier[u], 1, posDot-1);
      if StarcraftUnitPrefixes.IndexOf(prefix) = -1 then
        idx := StarcraftUnitPrefixes.Add(prefix)
      else
        idx := StarcraftUnitPrefixes.IndexOf(prefix);
      StarcraftUnitSplitIdentifier[u].Prefix:= StarcraftUnitPrefixes[idx];
      StarcraftUnitSplitIdentifier[u].PartialName := StarcraftUnitIdentifier[u].Substring(posDot);
    end else
    begin
      StarcraftUnitSplitIdentifier[u].Prefix:= '';
      StarcraftUnitSplitIdentifier[u].PartialName := StarcraftUnitIdentifier[u];
    end;

  end;
end;

function IsUnitType(AName: string): boolean;
var
  u: TStarcraftUnit;
  posDot, i: integer;
  partialName: String;
begin
  for i := 0 to StarcraftUnitPrefixes.Count-1 do
    if CompareText(StarcraftUnitPrefixes[i],AName)=0 then exit(true);

  for u := low(TStarcraftUnit) to suFactories do
  begin
    posDot := pos('.',StarcraftUnitIdentifier[u]);
    if posDot <> 0 then
    begin
      partialName := copy(StarcraftUnitIdentifier[u], posDot+1,
                      length(StarcraftUnitIdentifier[u]) - posDot);
      if CompareText(partialName,AName)=0 then exit(true);
    end;
  end;
  exit(false);
end;

initialization

  InitStarcraftUnitPrefixes;

finalization

  StarcraftUnitPrefixes.Free;

end.

