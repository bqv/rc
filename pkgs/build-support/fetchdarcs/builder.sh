source $stdenv/setup

tagtext=""
tagflags=""
if test -n "$rev"; then
    tagtext="(tag $rev) "
    tagflags="--tag=$rev"
elif test -n "$patch"; then
    tagtext="(patch $patch) "
    tagflags="--to-hash=$patch"
elif test -n "$context"; then
    tagtext="(context) "
    tagflags="--context=$context"
fi

header "getting $url $partial ${tagtext} into $out"

darcs get --lazy $tagflags "$url" "$out"
(cd "$out" && darcs changes --last=1)
# remove metadata, because it can change
rm -rf "$out/_darcs"

stopNest
