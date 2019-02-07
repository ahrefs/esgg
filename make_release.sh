base=$(date +%Y%m%d)
suffix=""
nr=0

while ! git tag -a -m "release $base$suffix" "$base$suffix" ; do
  nr=$((nr+1))
  suffix="-$nr"
done

version="$base$suffix"

echo ">>> created tag $version - check and push if ok"

if [ -z "$AHREFS_DEV_ROOT" ] ; then
  echo no AHREFS_DEV_ROOT set, skipping creation of opam package
else
  path=$AHREFS_DEV_ROOT/opam/packages/esgg.$version
  mkdir "$path"
  cp esgg.opam "$path/opam"
  printf "url {\n  src: \"git+ssh://git@git.ahrefs.com/ahrefs/esgg.git#%s\"\n}\n" "$version" >> "$path/opam"
  git -C "$path" add opam
  git -C "$path" commit -m "esgg: publish $version" opam
  echo ">>> created opam package at $path - check and push if ok"
fi
