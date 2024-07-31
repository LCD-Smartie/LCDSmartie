dotnet tool restore
dotnet tool run wix -- extension add WixToolset.UI.wixext
dotnet tool run wix -- build -ext WixToolset.UI.wixext -arch x86 .\Installer.wxs -o installer_x86.msi
dotnet tool run wix -- build -ext WixToolset.UI.wixext -arch x64 .\Installer.wxs -o installer_x64.msi
