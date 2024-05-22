# Tracmacs

I like the Trac ticket tracker. Like ServiceNow, it has several features that
make it beneficial for teams that want direct communication with users, but there
is one drawback: if you want to host your code on GitHub or another public platform,
then you lose Trac's own Git integration.

You could set git up on the machine that hosts Trac, and you could set up git mirroring
so that Trac would see your repo and start looking through new commits for closed
tickets, but perhaps you don't have administrator rights on the machine that
hosts Trac and/or you don't want to bother the sysadmin to set this up for you.

Currently, this package just lets me close tickets programmatically, which has
marginally less friction than using my browser. If there's a feature you want
let me know with an issue or open a PR.
