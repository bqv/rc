# hsroots, the Haskell bindings to wlroots

If you haven't seen it, [wlroots](https://github.com/swaywm/wlroots) is the compositor library created by the same people who built [sway](https://github.com/swaywm/sway) to implement a few things that weren't possible with [wlc](https://github.com/Cloudef/wlc).

### What is this:

* Basic (low! level) bindings to wlroots functionality
* (Re)implementation of basic examples. This is mostly to test
* cabal project to track updated dependencies (I had to expose a bit of functionality, this isn't upstreamed yet).

### What this is (mostly) not [help wanted]:

* Complete
* well documented
* Abstracting

This one is semi-intentional. This library is intended to expose pointers as they are, so it can be used with other middlewares etc.
* In a good functional style

## Why does this exist?

I mainly created this to support [my own endavours](https://github.com/Ongy/waymonad).
This implies that I will somewhat selectivly add to this as I need it in any project based on this.

Should you be interested in using this and feel like there's a feature missing, I will always appreciate PRs, and will aim to implemented feature requests in a timely manner.

### Build instructions

* Install `wlroots` with the instructions provided in their Readme
* `git clone --recursive https://github.com/swaywm/hsroots`
* `cd hsroots`
* `cabal new-build`

This should download all dependencies needed for hsroots and build it together
with the examples provided in this repository.
