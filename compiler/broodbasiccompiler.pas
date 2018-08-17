{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit broodbasiccompiler;

{$warn 5023 off : no warning about unused units}
interface

uses
  uarithmetic, uexpressions, uinstructions, uparseconditions, uparsevb, 
  ureadprog, usctypes, utriggercode, uvariables, uwritetriggers, 
  utrigeditoutput, utriggerchunk, umapinfo, utriggerinstructions, 
  utriggerconditions, uparsescalar, uparsecomplex, uprocedures, 
  ureadinstruction, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('broodbasiccompiler', @Register);
end.
