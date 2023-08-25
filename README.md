# My emacs configuration

See [init.el](./init.el) for the configuration entry point. I also make use of an [Early Init](https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html) file, located at [early-init.el](./early-init.el).

The configuration is split up into several files, which group different aspects of my Emacs configuration. This makes it easier to find different parts of the configuration, and enable or disable them in case needed. This comes at the cost of a bit of overhead, since one has to think were to put new configuration options and potentially create new files.

Packages are installed and configured using [straight](https://github.com/radian-software/straight.el) package manager.
