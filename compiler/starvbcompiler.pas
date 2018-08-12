{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit starvbcompiler;

{$warn 5023 off : no warning about unused units}
interface

uses
  uarithmetic, uexpressions, uinstructions, uparseconditions, uparsevb, 
  ureadprog, usctypes, utriggercode, uvariables, uwritetriggers, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('starvbcompiler', @Register);
end.
