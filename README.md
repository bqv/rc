apparmor-profiles
--------------------

AppArmor profiles I use for binary or potentially complex/dangerous/exposed apps
like browsers, binary IM clients, random electron and wine stuff.

Even confined to specific uid they get unwanted access to a lot of things in
$HOME and can read a lot of poorly-secured files on the system
(like /etc/passwd or some non-chmodded config), which is obviously undesirable,
and what AppArmor can help to fix.

Some profiles and abstractions are reused from upstreams like ubuntu, suse and
various misc other repos, but often found them too lax or bloated for specific
system (currently Arch Linux), allowing stuff like `@{HOME}/** r`, so prefer to
use them just for reference, copying only obvious and safe access lines from there,
getting (or confirming) the rest from audit logs.

Main doc on rule syntax:
https://gitlab.com/apparmor/apparmor/wikis/AppArmor_Core_Policy_Reference

### Important note

This is more of a "my configuration" repository, and profiles here are mostly
written in an ad-hoc fashion for my system, not to be generic fit for any linux
(or even app usage scenario) out there.

Plus I'm no security expert, so can - and do - miss some things, just making
sure that the most obvious bad things can't happen (or will trigger a warning),
not trying to build super-secure system or anything, thinking of it more like
basic hygeine than hardening against a dedicated attacker.

Therefore it might be wise to only use these profiles for reference
(e.g. to get the general idea where app needs access), and not as a drop-in things.

Some paths in profiles like @{HOME\_GIT} and @{SYS\_GIT} are specific to my
systems (configuration git repos), and can/should be removed or updated to local paths.
