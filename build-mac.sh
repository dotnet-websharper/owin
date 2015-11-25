#!/bin/bash
export EnableNuGetPackageRestore=true
: ${MonoHome=/Library/Frameworks/Mono.framework/Versions/Current/lib/mono}
: ${FSharpHome=$MonoHome/4.5}
: ${NuGetHome=tools/NuGet}
export FSharpHome
export MonoHome
export NuGetHome
mono $NuGetHome/NuGet.exe install IntelliFactory.Build -pre -ExcludeVersion -o tools/packages
mono $FSharpHome/fsi.exe --exec build.fsx %*
