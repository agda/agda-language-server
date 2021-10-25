#!/bin/sh
# Bundle icu4c DLLs

# see if icu4c has been installed 
if [ "$(brew list | grep icu4c)" = "" ]
    then
        echo "installing icu4c" 
        brew install icu4c
fi

# get the directory of the DDLs we want (icuuc, icui18n, icudata)
dylib_dir=$(dirname "$(brew list icu4c | grep icuuc.dylib)")

# find the path of "als"
executable=$(find "$(stack path --local-install-root)"/bin -name "als")

# remove the old dylib, and make a new one 
rm -rf dylib
mkdir dylib 

################################################################################
# icuuc 
################################################################################

icuuc_id=$(otool -L "$executable" | grep icuuc | awk '{print $1}')
icuuc_id_basename=$(basename "$icuuc_id")

icuuc_path=$dylib_dir/$icuuc_id_basename
icuuc_path_new=dylib/$icuuc_id_basename
icuuc_id_new=@loader_path/dylib/$icuuc_id_basename

# copy icuuc to the new directory
cp "$icuuc_path" "$icuuc_path_new"

# change icuuc's ID referenced by ALS
install_name_tool -change "$icuuc_id" "$icuuc_id_new" "$executable" 

echo "icuuc referenced by ALS"
echo "    old ID  : $icuuc_id"
echo "    new ID  : $icuuc_id_new"
echo "    old path: $icuuc_path"
echo "    new path: $icuuc_path_new"

################################################################################
# icui18n 
################################################################################

icui18n_id=$(otool -L "$executable" | grep icui18n | awk '{print $1}')
icui18n_id_basename=$(basename "$icui18n_id")

icui18n_path=$dylib_dir/$icui18n_id_basename
icui18n_path_new=dylib/$icui18n_id_basename
icui18n_id_new=@loader_path/dylib/$icui18n_id_basename

# copy icui18n to the new directory
cp "$icui18n_path" "$icui18n_path_new"

# change icui18n's ID referenced by ALS
install_name_tool -change "$icui18n_id" "$icui18n_id_new" "$executable" 

echo "icui18n referenced by ALS"
echo "    old ID  : $icui18n_id"
echo "    new ID  : $icui18n_id_new"
echo "    old path: $icui18n_path"
echo "    new path: $icui18n_path_new"

################################################################################
# icudata 
################################################################################

# otool -L "$icui18n_id" | grep icudata | awk '{print $1}'
icudata_id=$(otool -L "$icuuc_path" | grep icudata | awk '{print $1}')
icudata_id_basename=$(basename "$icudata_id")

icudata_path=$dylib_dir/$icudata_id_basename
icudata_path_new=dylib/$icudata_id_basename

# copy icudata to the new directory
cp "$icudata_path" "$icudata_path_new"

# no need of changing the ID because supposely it's already of "@loader_path"

echo "icudata referenced by icuuc"
echo "    old ID    : $icudata_id"
echo "    old path  : $icudata_path"
echo "    new path  : $icudata_path_new"
