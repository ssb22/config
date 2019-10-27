#!/bin/bash
git pull --no-edit
wget -N http://ssb22.user.srcf.net/setup/alpine.txt http://ssb22.user.srcf.net/setup/antialias.txt http://ssb22.user.srcf.net/setup/console.txt http://ssb22.user.srcf.net/setup/dark-background.reg http://ssb22.user.srcf.net/setup/dark-background.txt http://ssb22.user.srcf.net/setup/emacs-patch.txt http://ssb22.user.srcf.net/setup/emacs.tgz http://ssb22.user.srcf.net/setup/fbtermrc.txt http://ssb22.user.srcf.net/setup/_fontsconf.txt http://ssb22.user.srcf.net/setup/_forms.txt http://ssb22.user.srcf.net/setup/_gtkrc.txt http://ssb22.user.srcf.net/setup/high-contrast.zip http://ssb22.user.srcf.net/setup/invert-vnc.diff http://ssb22.user.srcf.net/setup/jedrc.txt http://ssb22.user.srcf.net/setup/_lynxcfg.txt http://ssb22.user.srcf.net/setup/_lynxrc.txt http://ssb22.user.srcf.net/setup/_lyxprefs.txt http://ssb22.user.srcf.net/setup/_lyxrc.txt http://ssb22.user.srcf.net/setup/_message-formatter.txt http://ssb22.user.srcf.net/setup/modeline.py http://ssb22.user.srcf.net/setup/_muttrc.txt http://ssb22.user.srcf.net/setup/_nanorc.txt http://ssb22.user.srcf.net/setup/tmuxconf.txt http://ssb22.user.srcf.net/setup/_twmrc.txt http://ssb22.user.srcf.net/setup/x11vnc.diff http://ssb22.user.srcf.net/setup/_xres.txt http://ssb22.user.srcf.net/setup/_vimrc.txt
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
