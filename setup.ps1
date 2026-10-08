$root = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $root

Write-Host "== 1/4 Verification des fichiers de l'application ==" -ForegroundColor Cyan
$required = @(
  "shiny_achat\app.R","shiny_achat\global.R","shiny_achat\ui.R","shiny_achat\server.R","shiny_achat\www\style.css",
  "shiny_stock\app.R","shiny_stock\global.R","shiny_stock\ui.R","shiny_stock\server.R","shiny_stock\www\style.css",
  "main.js","preload.js","shell.html","package.json","install_packages.R"
)
foreach ($f in $required) {
  if (-not (Test-Path "$root\$f")) { Write-Host "MANQUANT: $f" -ForegroundColor Red; exit 1 }
}

Write-Host "== 2/4 Copie de R dans R-Portable ==" -ForegroundColor Cyan
if (-not (Test-Path "$root\R-Portable\bin\Rscript.exe")) {
  $rDir = Get-ChildItem "C:\Program Files\R" -Directory -Filter "R-*" | Sort-Object Name -Descending | Select-Object -First 1
  if (-not $rDir) { Write-Host "R n'est pas installe" -ForegroundColor Red; exit 1 }
  Copy-Item $rDir.FullName -Destination "$root\R-Portable" -Recurse
}
& "$root\R-Portable\bin\Rscript.exe" --version

Write-Host "== 3/4 Installation des packages dans R-Portable (5 a 15 min) ==" -ForegroundColor Cyan
$env:R_LIBS_USER = "$root\R-Portable\library"
$env:R_LIBS = "$root\R-Portable\library"
& "$root\R-Portable\bin\Rscript.exe" "$root\install_packages.R"
if ($LASTEXITCODE -ne 0) { Write-Host "Echec installation des packages" -ForegroundColor Red; exit 1 }

Write-Host "== 4/4 npm install ==" -ForegroundColor Cyan
npm install

Write-Host "TERMINE. Test: npm start   |   Installateur: npm run build" -ForegroundColor Green
