program network;

{$mode objfpc}{$H+}

uses
  fpCGI, cgimain, common, database, CheckRuns, floatstr,
  fattree, torus;

{$R *.res}

begin
  Application.Initialize;
  Application.Run;
end.

