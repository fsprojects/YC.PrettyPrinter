@echo off
cls

.paket\paket.bootstrapper.exe prerelease
if errorlevel 1 (
  exit /b %errorlevel%
)

.paket\paket.exe restore -v
if errorlevel 1 (
  exit /b %errorlevel%
)

packages\FAKE\tools\FAKE.exe build.fsx "target=Release" "NugetKey=d9b0fe46-e878-47c9-aed6-2a540779a6fc" "github-user=gsvgit"  "github-pw=gsv2git"
