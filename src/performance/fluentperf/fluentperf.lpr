program fluentperf;

{$mode objfpc}{$H+}

uses
  fpCGI, main, perf_common, AnsysFluent_v13, CheckRuns, FloatStr;

{$R *.res}

begin
  Application.Initialize;
  Application.Run;
end.

