Mirage Xen Test VM interface, library and CLI tool
==================================================

The Mirage Xen Test VM is a small kernel which can be controlled remotely
over Vchan from domain 0. The kernel can be asked to perform real I/O (to
verify the datapath actually works), to reboot, suspend, resume, migrate,
crash etc.

This repository contains the interface which should be implemented by the
test kernel, a shared library which should be linked into a test harness
and an example command-line tool.
