apparmor-profiles
--------------------

Apparmor profiles I use for binary or potentially dangerous apps.

I consider running stuff like skype, adobe-flash, firefox or wine apps quite
dangerous - even confined to specific uid they can mess up or gain unsanctioned
access to a lot of stuff in $HOME, plus just read a lot of poorly-secured stuff
on system (like /etc/passwd or some non-chmodded config), which I don't want
them to.

Flash is quite famous for it's security issues, skype is closed-source
potentially-malicious blob of spyware, firefox (with all the addons and plugins)
is just too complex to be secure enough, so it's better to err on the side of
caution here, plus apparmor itself is insanely easy to use and non-intrusive.

I tried to re-use profiles from upstreams like ubuntu, suse and various misc
repos and googleable blogs, but often found them too lax, allowing stuff like
"@{HOME}/** r", so I prefer to use them just reference, copying only the obvious
and safe access lines from there, getting (or confirming) the rest from audit
logs.

### Important note

This is more of a "my configuration" repository, and profiles here are mostly
written from scratch in an ad-hoc fashion for my system, not to be generic fit
for any linux (or even app usage scenario) out there.

Plus I'm no security expert, so can - and do - miss some things, making sure
just that the most obvious bad things can't happen (and will trigger a warning),
not trying to build super-secure system or anything, thinking of it more like
basic hygeine than hardening against a dedicated attacker.

Therefore it might be wise to only use these for reference (e.g. to get the
general idea where app needs access), and not as a drop-in things.

Some paths in these profiles (like ~/.cFG/* and /etc/core) are specific to my
systems (configuration git repos), and can/should be removed or updated to local
paths.

### Note on abstractions with multiarch

In the distro I've used in the past, multiarch was structured like this:

	/usr/*-linux-gnu*/bin/...
	/usr/*-linux-gnu*/sbin -> bin
	/usr/*-linux-gnu*/lib/...
	/usr/*-linux-gnu*/include/...
	/usr/host -> *-linux-gnu*
	/usr/share/...
	/usr/{bin,sbin,lib} -> host/{bin,sbin,lib}
	/{bin,sbin,lib} -> host/{bin,sbin,lib}

And stuff under "profiles/abstractions" is copied from those shipped from
apparmor (in "/etc/apparmor.d/abstractions"), but with paths adjustable for that
layout via "multiarch" var (see "profiles/tunables/multiarch").

For instance, with default `@{multiarch}=""`,
`/usr/@{multiarch}{lib/firefox,bin}/firefox` will be
`/usr/{lib/firefox,bin}/firefox`, but with `@{multiarch}=*-linux-gnu*/`, it will
be `/usr/x86_64-pc-linux-gnu/{lib/firefox,bin}/firefox`.
Note that final slash in "multiarch" var is required if it isn't empty.

Default "/etc/apparmor.d/abstractions" can probably be used with these profiles,
as they should have same stuff, but with more "classic" layout.

**BUT** as I'm not using multiarch-distro these days, all this stuff might be
  bitrotten, and all the profile attachment specs are wrong for multiarch
  (e.g. "/usr/bin/firefox"), as I've never figured-out how to attach same
  profile to multiple binaries easily.
