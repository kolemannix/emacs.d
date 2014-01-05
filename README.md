# Your Old Emacs Config Files

This directory contains any Emacs configuration files that had existed prior
to installing Emacs Live.

To see which files have been preserved:

    ls -allh /Users/knix/emacs-live-old-config

To revert back to your old Emacs configs simply:

    rm -rf ~/.emacs.d
    mv /Users/knix/emacs-live-old-config/.emacs* ~/
    rm -rf /Users/knix/emacs-live-old-config
