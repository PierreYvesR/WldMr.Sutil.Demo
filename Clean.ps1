#!/usr/bin/env pwsh
remove-item -rec node_modules -erroraction silentlycontinue
remove-item -rec dist -erroraction silentlycontinue
remove-item -rec .paket -erroraction silentlycontinue
remove-item -rec paket-files -erroraction silentlycontinue
remove-item -rec src/App/build -erroraction silentlycontinue
gci -recurse . -directory | where name -match "^(bin|obj)$" | select fullname | foreach { remove-item -rec -force $_.FullName}
