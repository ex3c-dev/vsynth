$al_dir = $PSScriptRoot + "\libraries\openal-soft-1.20.1-bin"
$include_dir = $al_dir + "\include"
$lib_dir = $al_dir + "\libs\Win64"

stack build --extra-include-dirs=$include_dir --extra-lib-dirs=$lib_dir  