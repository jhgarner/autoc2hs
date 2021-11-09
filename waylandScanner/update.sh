#!/usr/bin/env bash

scanner=`pkg-config --variable=wayland_scanner wayland-scanner`
protocol=`pkg-config --variable=pkgdatadir wayland-protocols`

$scanner server-header $protocol/stable/xdg-shell/xdg-shell.xml xdg-shell-protocol.h
