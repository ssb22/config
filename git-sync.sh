#!/bin/bash
git pull --no-edit
wget -N http://people.ds.cam.ac.uk/ssb22/setup/alpine.txt http://people.ds.cam.ac.uk/ssb22/setup/antialias.txt http://people.ds.cam.ac.uk/ssb22/setup/console.txt http://people.ds.cam.ac.uk/ssb22/setup/dark-background.reg http://people.ds.cam.ac.uk/ssb22/setup/dark-background.txt http://people.ds.cam.ac.uk/ssb22/setup/emacs-patch.txt http://people.ds.cam.ac.uk/ssb22/setup/emacs.tgz http://people.ds.cam.ac.uk/ssb22/setup/fbtermrc.txt http://people.ds.cam.ac.uk/ssb22/setup/_fontsconf.txt http://people.ds.cam.ac.uk/ssb22/setup/_forms.txt http://people.ds.cam.ac.uk/ssb22/setup/_gtkrc.txt http://people.ds.cam.ac.uk/ssb22/setup/high-contrast.zip http://people.ds.cam.ac.uk/ssb22/setup/invert-vnc.diff http://people.ds.cam.ac.uk/ssb22/setup/jedrc.txt http://people.ds.cam.ac.uk/ssb22/setup/_lynxcfg.txt http://people.ds.cam.ac.uk/ssb22/setup/_lynxrc.txt http://people.ds.cam.ac.uk/ssb22/setup/_lyxprefs.txt http://people.ds.cam.ac.uk/ssb22/setup/_lyxrc.txt http://people.ds.cam.ac.uk/ssb22/setup/_message-formatter.txt http://people.ds.cam.ac.uk/ssb22/setup/modeline.py http://people.ds.cam.ac.uk/ssb22/setup/_muttrc.txt http://people.ds.cam.ac.uk/ssb22/setup/_nanorc.txt http://people.ds.cam.ac.uk/ssb22/setup/tmuxconf.txt http://people.ds.cam.ac.uk/ssb22/setup/_twmrc.txt http://people.ds.cam.ac.uk/ssb22/setup/x11vnc.diff http://people.ds.cam.ac.uk/ssb22/setup/_xres.txt http://people.ds.cam.ac.uk/ssb22/setup/_vimrc.txt
cp -a _fontsconf.txt .fonts.conf
cp -a _forms.txt forms.css
cp -a _gtkrc.txt .gtkrc
cp -a _lynxcfg.txt .lynx.cfg
cp -a _lynxrc.txt .lynxrc
cp -a _lyxrc.txt .lyx/lyxrc
cp -a _lyxprefs.txt .lyx/preferences
cp -a _muttrc.txt .muttrc
cp -a _message-formatter.txt .message-formatter
cp -a _twmrc.txt .twmrc
cp -a _xres.txt .Xresources
cp -a _vimrc.txt .vimrc
cp -a console.txt .console-setup
cp -a fbtermrc.txt .fbtermrc
cp -a jedrc.txt .jedrc
cp -a tmuxconf.txt .tmux.conf
cd riscos-high-contrast ; unzip -o ../high-contrast.zip ; git add * ; cd ..
tar -zxvf emacs.tgz ; git add .xemacs/*
git commit -am "Update $(echo $(git diff|grep '^--- a/'|sed -e 's,^--- a/,,')|sed -e 's/ /, /g')" && git push
