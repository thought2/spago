let AppTarget = (./AppTarget.dhall).Type
let LibraryTarget = (./LibraryTarget.dhall).Type

let TargetType = < AppTarget : AppTarget | LibraryTarget : LibraryTarget >

in TargetType