'The following arrays can be indexed by a player, for example Ore(Me)
Dim Ore(8) As UInt24
Property Minerals(8) As UInt24   'Alias for Ore
Dim Gas(8) As UInt24
Property OreAndGas(8) As UInt24
Property MineralsAndGas(8) As UInt24  'Alias for OreAndGas
Dim UnitScore(8) As UInt24
Dim BuildingScore(8) As UInt24
Property UnitAndBuildingScore(8) As UInt24
Dim KillScore(8) As UInt24
Dim RazingScore(8) As UInt24
Property KillAndRazingScore(8) As UInt24
Dim CustomScore(8) As UInt24
Property TotalScore(8) As UInt24
Dim Present(8) As Boolean   'accessible only from main thread

Dim Countdown As UInt16
Dim CountdownPaused As Boolean
Property ElapsedTime As UInteger
Dim GamePaused As Boolean
Dim NextScenario As String
Dim Switch(256) As Boolean  'Avoid accessing them directly, use boolean variables instead
Sub Wait(DurationMs As UInteger)

Readonly Property Location(Name As String) As Location
Const Anywhere As String = "Anywhere"
Class Location
	Sub Attract(OtherLocation As String)
	Sub CenterOn(OtherLocation As String)
End Class

Property Player(Index As Byte) As Player
Class Player
	Function CreateUnit(Count As Byte, UnitType As Unit, 
	    Optional Location As String = Anywhere, 
		Optional Properties As UnitProperties = Nothing) As Units
	Sub CenterView()
	Sub MinimapPing()
	Sub Print(Text As String)
	Sub TalkingPortrait(UnitType As Unit, DurationMs As UInteger)
	Sub RunAIScript(ScriptCode As String, Optional Location As String = Anywhere)
	Sub Defeat()
	Sub Draw()
	Sub Victory()
		
	Property UnitCount(UnitType As Unit, Optional Location As String = Anywhere) As UInt24
	Readonly Property KillCount(UnitType As Unit, Optional Location As String = Anywhere) As UInt24
	Property DeathCount(UnitType As Unit, Optional Location As String = Anywhere) As UInt24
	Property OpponentCount As Byte
	Readonly Property Units(UnitType As Unit, Optional Location As String = Anywhere) As Units	
	Property MissionObjectives As String
	Readonly Property Leaderboard As Leaderboard
	Readonly Property Alliance As Alliance
	Property UnitSpeech As Boolean
End Class

Class Units
	Sub AttractLocation(OtherLocation As String)
	Sub Teleport(OtherLocation As String)
	Sub Kill(Optional Quantity As Byte = 255)
	Sub Remove(Optional Quantity As Byte = 255)
	Sub Give(OtherPlayer As Player, Optional Quantity As Byte = 255)
	
	Sub MoveOrder(OtherLocation As String)
	Sub PatrolOrder(OtherLocation As String)
	Sub AttackOrder(OtherLocation As String)
	Sub EnterTransport()
	Sub ExitTransport()
	Sub EnterClosestBunker()
	
	Sub ToggleInvincibility()
	Sub ToggleDoodadState()

	Property Properties As UnitProperties
	Property Invincible As Boolean
	Property DoodadState As Boolean
	Property Life As Byte
	Property Shield As Byte
	Property Energy As Byte
	Property Resource As UInt16
	Property HangarCount As UInt16
	Property Location As String		
End Class

Class Leaderboard
	Sub ToggleComputers()
	Sub Show(Values As UInt24(), Optional Text As String)
	Sub ShowResources(Optional MaxValue As UInteger)
	Property Computers As Boolean	
End Class

Class Alliance 
	Sub Ally(Players As Player())
	Sub Ennemy(Players As Player())
	Sub AlliedVictory(Players As Player())
	Property SharedVision(OtherPlayer As Player) As Boolean	
End Class

Class Sound
    Public Filename As String
    Public Duration As UInteger
	Sub Play()
End Class

Class UnitProperties
    Public Life As Byte    '0..100
    Public Shield As Byte  '0..100
    Public Energy As Byte  '0..100
    Public Invincible As Boolean
    Public Burrowed As Boolean
    Public Lifted As Boolean
    Public Hallucinated As Boolean
    Public Cloaked As Boolean
    Public Resource As UInt16
    Public HangarCount As UInt16
End Class

Function CountIf(Values As UInteger(), CompareTo As UInteger) As Byte
Function CountIf(Values As Boolean(), CompareTo As Boolean) As Byte
Function LBound(Values As Array) As Byte
Function UBound(Values As Array) As Byte

Function Rnd() As Single  '0 included..1 excluded
Function Rnd() As Boolean
Function Abs(Value As Integer) As UInteger

Function CustomUnit(CustomName As String) As Unit
Enum Unit
    Terran.Marine
    Terran.Ghost
    Terran.Vulture
    Terran.Goliath
    Terran.GoliathTurret
    Terran.SiegeTankTankMode
    Terran.SiegeTankTankModeTurret
    Terran.SCV
    Terran.Wraith
    Terran.ScienceVessel
    Hero.GuiMontag
    Terran.Dropship
    Terran.Battlecruiser
    Terran.VultureSpiderMine
    Terran.NuclearMissile
    Terran.Civilian
    Hero.SarahKerrigan
    Hero.AlanSchezar
    Hero.AlanSchezarTurret
    Hero.JimRaynorVulture
    Hero.JimRaynorMarine
    Hero.TomKazansky
    Hero.Magellan
    Hero.EdmundDukeTankMode
    Hero.EdmundDukeTankModeTurret
    Hero.EdmundDukeSiegeMode
    Hero.EdmundDukeSiegeModeTurret
    Hero.ArcturusMengsk
    Hero.Hyperion
    Hero.NoradII
    Terran.SiegeTankSiegeMode
    Terran.SiegeTankSiegeModeTurret
    Terran.Firebat
    Spell.ScannerSweep
    Terran.Medic
    Zerg.Larva
    Zerg.Egg
    Zerg.Zergling
    Zerg.Hydralisk
    Zerg.Ultralisk
    Zerg.Broodling
    Zerg.Drone
    Zerg.Overlord
    Zerg.Mutalisk
    Zerg.Guardian
    Zerg.Queen
    Zerg.Defiler
    Zerg.Scourge
    Hero.Torrasque
    Hero.Matriarch
    Zerg.InfestedTerran
    Hero.InfestedKerrigan
    Hero.UncleanOne
    Hero.HunterKiller
    Hero.DevouringOne
    Hero.KukulzaMutalisk
    Hero.KukulzaGuardian
    Hero.Yggdrasill
    Terran.Valkyrie
    Zerg.Cocoon
    Protoss.Corsair
    Protoss.DarkTemplar
    Zerg.Devourer
    Protoss.DarkArchon
    Protoss.Probe
    Protoss.Zealot
    Protoss.Dragoon
    Protoss.HighTemplar
    Protoss.Archon
    Protoss.Shuttle
    Protoss.Scout
    Protoss.Arbiter
    Protoss.Carrier
    Protoss.Interceptor
    Hero.DarkTemplar
    Hero.Zeratul
    Hero.TassadarZeratulArchon
    Hero.FenixZealot
    Hero.FenixDragoon
    Hero.Tassadar
    Hero.Mojo
    Hero.Warbringer
    Hero.Gantrithor
    Protoss.Reaver
    Protoss.Observer
    Protoss.Scarab
    Hero.Danimoth
    Hero.Aldaris
    Hero.Artanis
    Critter.Rhynadon
    Critter.Bengalaas
    Special.CargoShip
    Special.MercenaryGunship
    Critter.Scantid
    Critter.Kakaru
    Critter.Ragnasaur
    Critter.Ursadon
    Zerg.LurkerEgg
    Hero.Raszagal
    Hero.SamirDuran
    Hero.AlexeiStukov
    Special.MapRevealer
    Hero.GerardDuGalle
    Zerg.Lurker
    Hero.InfestedDuran
    Spell.DisruptionWeb
    Terran.CommandCenter
    Terran.ComsatStation
    Terran.NuclearSilo
    Terran.SupplyDepot
    Terran.Refinery
    Terran.Barracks
    Terran.Academy
    Terran.Factory
    Terran.Starport
    Terran.ControlTower
    Terran.ScienceFacility
    Terran.CovertOps
    Terran.PhysicsLab
    Unused.Terran1
    Terran.MachineShop
    Unused.Terran2
    Terran.EngineeringBay
    Terran.Armory
    Terran.MissileTurret
    Terran.Bunker
    Special.CrashedNoradII
    Special.IonCannon
    Powerup.UrajCrystal
    Powerup.KhalisCrystal
    Zerg.InfestedCommandCenter
    Zerg.Hatchery
    Zerg.Lair
    Zerg.Hive
    Zerg.NydusCanal
    Zerg.HydraliskDen
    Zerg.DefilerMound
    Zerg.GreaterSpire
    Zerg.QueensNest
    Zerg.EvolutionChamber
    Zerg.UltraliskCavern
    Zerg.Spire
    Zerg.SpawningPool
    Zerg.CreepColony
    Zerg.SporeColony
    Unused.Zerg1
    Zerg.SunkenColony
    Special.OvermindWithShell
    Special.Overmind
    Zerg.Extractor
    Special.MatureChrysalis
    Special.Cerebrate
    Special.CerebrateDaggoth
    Unused.Zerg2
    Protoss.Nexus
    Protoss.RoboticsFacility
    Protoss.Pylon
    Protoss.Assimilator
    Unused.Protoss1
    Protoss.Observatory
    Protoss.Gateway
    Unused.Protoss2
    Protoss.PhotonCannon
    Protoss.CitadelofAdun
    Protoss.CyberneticsCore
    Protoss.TemplarArchives
    Protoss.Forge
    Protoss.Stargate
    Special.StasisCellPrison
    Protoss.FleetBeacon
    Protoss.ArbiterTribunal
    Protoss.RoboticsSupportBay
    Protoss.ShieldBattery
    Special.KhaydarinCrystalForm
    Special.ProtossTemple
    Special.XelNagaTemple
    Resource.MineralField1
    Resource.MineralField2
    Resource.MineralField3
    Unused.Cave
    Unused.CaveIn
    Unused.Cantina
    Unused.MiningPlatform
    Unused.IndependantCommandCenter
    Special.IndependantStarport
    Unused.IndependantJumpGate
    Unused.Ruins
    Unused.KhaydarinCrystalFormation
    Resource.VespeneGeyser
    Special.WarpGate
    Special.PsiDisrupter
    Unused.ZergMarker
    Unused.TerranMarker
    Unused.ProtossMarker
    Special.ZergBeacon
    Special.TerranBeacon
    Special.ProtossBeacon
    Special.ZergFlagBeacon
    Special.TerranFlagBeacon
    Special.ProtossFlagBeacon
    Special.PowerGenerator
    Special.OvermindCocoon
    Spell.DarkSwarm
    Special.FloorMissileTrap
    Special.FloorHatch
    Special.LeftUpperLevelDoor
    Special.RightUpperLevelDoor
    Special.LeftPitDoor
    Special.RightPitDoor
    Special.FloorGunTrap
    Special.WallMissileTrap
    Special.WallFlameTrap
    Special.RightWallMissileTrap
    Special.RightWallFlameTrap
    Special.StartLocation
    Powerup.Flag
    Powerup.YoungChrysalis
    Powerup.PsiEmitter
    Powerup.DataDisk
    Powerup.KhaydarinCrystal
    Powerup.MineralClusterType1
    Powerup.MineralClusterType2
    Powerup.ProtossGasOrbType1
    Powerup.ProtossGasOrbType2
    Powerup.ZergGasSacType1
    Powerup.ZergGasSacType2
    Powerup.TerranGasTankType1
    Powerup.TerranGasTankType2
    NoUnit
    AnyUnit
    Men
    Buildings
    Factories
End Enum

Function Chr(Value As Byte) As Char
Function Len(Text As String) As Byte
Const vbCr = Chr(13)
Const vbLf = Chr(10)
Const vbCrLf = Chr(13)+Chr(10)
Const vbTab = Chr(9)

Class Align
    Public Const Left As String = ""
    Public Const Right As String = Chr(&H12)
    Public Const Center As String = Chr(&H13)
    Public Const Clear As String = Chr(&H0C)
End Class 

Class Color
	Public PaleBlue As String = Chr(2)
	Public Yellow As String = Chr(3)
	Public White As String = Chr(4)
	Public Gray As String = Chr(5)
	Public Red As String = Chr(6)
	Public Green As String = Chr(7)
	Public RedPlayer As String = Chr(8)
	Public Invisible As String = Chr(&H0B)
	Public BluePlayer As String = Chr(&H0E)
	Public TealPlayer As String = Chr(&H0F)
	Public PurplePlayer As String = Chr(&H10)
	Public OrangePlayer As String = Chr(&H11)
	Public BrownPlayer As String = Chr(&H15)
	Public WhitePlayer As String = Chr(&H16)
	Public YellowPlayer As String = Chr(&H17)
	Public GreenPlayer As String = Chr(&H18)
	Public BrightYellowPlayer As String = Chr(&H19)
	Public Cyan As String = Chr(&H1A)
	Public PinkPlayer As String = Chr(&H1B)
	Public DarkCyanPlayer As String = Chr(&H1C)
	Public GrayGreen As String = Chr(&H1D)
	Public BlueGray As String = Chr(&H1E)
	Public Turquoise As String = Chr(&H1F)	
End Class   