apparmor-profiles: generic apparmor profiles for binary or dangerous apps
--------------------

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

Some paths here (like ~/.cFG/*) are specific to my systems, and can/should be
removed or updated to local paths.
