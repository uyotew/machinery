some notes on android devices i use, since they cannot??? be configured with guix

download f-droid, and termux..
and the wireguard app, for wg access

in termux, zig, helix and git can be installed.
pkg install zig helix git
it is possible to build keypit from termux, just run
git clone https://github.com/uyotew/keypit
cd keypit
zig build

zig will display errors, but the binary is in zig-out/bin/keypit, and works fine
