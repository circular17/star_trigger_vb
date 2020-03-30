unit uunitpropchunk;  //UPRP

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  MIN_UNIT_PROPERTIES = 1;
  MAX_UNIT_PROPERTIES = 64;
  FLAG_CLOAK = 1;
  FLAG_BURROW = 2;
  FLAG_IN_TRANSIT = 4;
  FLAG_HALLUCINATE = 8;
  FLAG_INVINCIBLE = 16;

  FLAG_HITPOINTS = 2;
  FLAG_SHIELD = 4;
  FLAG_ENERGY = 8;
  FLAG_RESOURCE_AMOUNT = 16;
  FLAG_HANGAR_COUNT = 32;

type
  PUnitPropertiesData = ^TUnitPropertiesData;

  { TUnitPropertiesData }

  TUnitPropertiesData = object
  private
    function GetBurrowValid: boolean;
    function GetCloakValid: boolean;
    function GetEnergyValid: boolean;
    function GetHallucinateValid: boolean;
    function GetHangarCountValid: boolean;
    function GetHitPointsValid: boolean;
    function GetInTransitValid: boolean;
    function GetInvincibleValid: boolean;
    function GetIsBurrowed: boolean;
    function GetIsCloaked: boolean;
    function GetIsHallucinated: boolean;
    function GetIsInTransit: boolean;
    function GetIsInvincible: boolean;
    function GetResourceAmountValid: boolean;
    function GetShieldValid: boolean;
  public
    ValidFlags: word;
    ValidData: word;
    Player: byte;
    HitPoints, Shield, Energy: Byte;
    ResourceAmount: dword;
    HangarCount: word;
    Flags: word;
    Dummy: dword;
    property CloakValid: boolean read GetCloakValid;
    property IsCloaked: boolean read GetIsCloaked;
    property BurrowValid: boolean read GetBurrowValid;
    property IsBurrowed: boolean read GetIsBurrowed;
    property InTransitValid: boolean read GetInTransitValid;
    property IsInTransit: boolean read GetIsInTransit;
    property HallucinateValid: boolean read GetHallucinateValid;
    property IsHallucinated: boolean read GetIsHallucinated;
    property InvincibleValid: boolean read GetInvincibleValid;
    property IsInvincible: boolean read GetIsInvincible;
    property HitPointsValid: boolean read GetHitPointsValid;
    property ShieldValid: boolean read GetShieldValid;
    property EnergyValid: boolean read GetEnergyValid;
    property ResourceAmountValid: boolean read GetResourceAmountValid;
    property HangarCountValid: boolean read GetHangarCountValid;
  end;

implementation

{ TUnitPropertiesData }

function TUnitPropertiesData.GetBurrowValid: boolean;
begin
  result := (ValidFlags and FLAG_BURROW) <> 0;
end;

function TUnitPropertiesData.GetCloakValid: boolean;
begin
  result := (ValidFlags and FLAG_CLOAK) <> 0;
end;

function TUnitPropertiesData.GetHallucinateValid: boolean;
begin
  result := (ValidFlags and FLAG_HALLUCINATE) <> 0;
end;

function TUnitPropertiesData.GetInTransitValid: boolean;
begin
  result := (ValidFlags and FLAG_IN_TRANSIT) <> 0;
end;

function TUnitPropertiesData.GetInvincibleValid: boolean;
begin
  result := (ValidFlags and FLAG_INVINCIBLE) <> 0;
end;

function TUnitPropertiesData.GetIsBurrowed: boolean;
begin
  result := (Flags and FLAG_BURROW) <> 0;
end;

function TUnitPropertiesData.GetIsCloaked: boolean;
begin
  result := (Flags and FLAG_CLOAK) <> 0;
end;

function TUnitPropertiesData.GetIsHallucinated: boolean;
begin
  result := (Flags and FLAG_HALLUCINATE) <> 0;
end;

function TUnitPropertiesData.GetIsInTransit: boolean;
begin
  result := (Flags and FLAG_IN_TRANSIT) <> 0;
end;

function TUnitPropertiesData.GetIsInvincible: boolean;
begin
  result := (Flags and FLAG_INVINCIBLE) <> 0;
end;

function TUnitPropertiesData.GetEnergyValid: boolean;
begin
  result := (ValidData and FLAG_ENERGY) <> 0;
end;

function TUnitPropertiesData.GetHangarCountValid: boolean;
begin
  result := (ValidData and FLAG_HANGAR_COUNT) <> 0;
end;

function TUnitPropertiesData.GetHitPointsValid: boolean;
begin
  result := (ValidData and FLAG_HITPOINTS) <> 0;
end;

function TUnitPropertiesData.GetResourceAmountValid: boolean;
begin
  result := (ValidData and FLAG_RESOURCE_AMOUNT) <> 0;
end;

function TUnitPropertiesData.GetShieldValid: boolean;
begin
  result := (ValidData and FLAG_SHIELD) <> 0;
end;

end.

