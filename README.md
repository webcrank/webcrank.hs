Webcrank
========

Webcrank is an ambitious project to build a fast, reliable and
__correct__, HTTP toolkit.

This project forms the core of the haskell implementation.

See <https://github.com/webcrank/webcrank> for more details.

Build
-----

[Travis](https://travis-ci.org/webcrank/webcrank.hs) [![Build Status](https://travis-ci.org/webcrank/webcrank.hs.png)](https://travis-ci.org/webcrank/webcrank.hs)

Open questions
-----

Should we write our own dispatcher, use an existing one, or leave it open for users to pick?
---

Currently leaning towards building on top of [reroute](https://github.com/agrafix/reroute). Provides a nice level of type-safety without the need for TH or lots of boilerplace..


Should we try and target multiple web frameworks, or just pick one - like WAI/Warp?
---

Currently leaning towards just building on top of WAI, but abstracting out as much of the details as possible.
