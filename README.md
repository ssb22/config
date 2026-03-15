# Low-vision configuration for GNU/Linux, Unix and RISC OS
from https://ssb22.user.srcf.net/setup and related pages

(also [mirrored on GitLab Pages](https://ssb22.gitlab.io/setup) just in case)

## Low-vision X11 configuration
I use one of two approaches, depending on the circumstances: low-resolution scrollable desktop, or high-resolution non-scrollable desktop with larger fonts.

## 1. Low-resolution scrollable desktop
* Works well if you have older software that uses bitmap fonts, or if you have a Mac
* You can set the window you’re working in to fit the “viewport”, and quickly pan the viewport to see other windows
* May not work so well with desktop environments like GNOME; simple window managers are best
* Can be more trouble to set up on modern graphics hardware and/or laptops

**Configuration files** are usually files and directories in your home directory whose names start with a dot (.).  You have to use `ls -a` to list them.  I do not recommend replacing your existing files with these without first checking that your existing files do not contain anything important.  Look at the files and see if you want to merge anything.

* [.Xresources](.Xresources)—used for setting fonts and colours (etc) in various X applications; a good starting point.  See the comments at the beginning of the file about setup.
* [.gtkrc](.gtkrc)—needed in addition to the above for some applications.  **You should save this file as both `.gtkrc` and `.gtkrc-2.0`** (or just save it as `.gtkrc` and do `ln -s .gtkrc .gtkrc-2.0`).  Note: Some badly-behaved programs will need this `.wgtkrc` to be temporarily renamed before they are started.
* [.lyx/lyxrc](.lyx/lyxrc) and [.lyx/preferences](.lyx/preferences)—used to configure LyX (a LaTeX front-end).  Note: If using a version of LyX prior to 1.5.3, use the XForms version rather than the Qt version, due to LyX bug 4293.

**Window managers:** `flwm`’s colours can be configured at the command line from within `.xsession`, e.g. `flwm -fg white -bg darkblue`; if you have an old version that doesn’t obey the `-fg` flag, you may be able to upgrade, or apply the patch I submitted to Debian bug #267983.  On some systems you may have to fall back to TWM in which case you could try this [.twmrc](.twmrc). 

Some distributions also have a package `big-cursor` which provides large fonts for the mouse cursor (sometimes you can just install it, but very old versions might need a little setting up).

Don’t forget to also adjust the monitor’s ‘brightness’ and ‘contrast’ settings to comfortable levels.  (I usually find a high ‘contrast’ and a low ‘brightness’ is better.)

### FreeType at low resolution

The FreeType font system is fine in high resolutions because you can set the screen DPI to a large value and have all the fonts enlarged instantly.  However, in low resolutions, some versions of FreeType do not render fonts so well.  If you find yourself in that situation, it can be better to use the largest fixed-size fonts instead and adjust the display resolution rather than the font size.  If you often work in low resolutions (perhaps because some of your applications don’t use FreeType, or they have graphical controls that won’t enlarge, or you need the scrollable desktop estate) then you might want to get FreeType to use the older fonts too.

1. Make the fixed size fonts available in FreeType.  In most cases you can do this by creating a symbolic link to the top of the X fontpath, like this:

    cd ~
    mkdir .fonts
    ln -s $(xset q |
            grep /fonts/ |
            head -1 |
            sed -e "s|fonts/.*|fonts|") .fonts/fontpath

2. Use my [.fonts.conf](.fonts.conf) and customise as necessary (this one asks for 20-pixel fonts)
3. Edit your .Xresources and check the setting of `Xft.dpi`.  Add a line if it isn’t there already; it should look like this:

    Xft.dpi: 125

The ideal DPI setting is probably 7.2 times the pixel size of your bitmap fonts (so 144 for 20-pixel) but it seems that some installations of FreeType work better if you reduce it a little; you may need to experiment to get the best size.

Some versions of FreeType contain a bug that prevents it from loading the bitmap fonts.  You can check by running `fc-list : file | grep dpi` after making the above changes; if there is no output then your system has the bug.  If you find yourself on such a system then undo the first two steps (`rm -rf ~/.fonts*`) and try to find some fonts that FreeType doesn’t render too badly.  You can try installing the Debian package `msttcorefonts` but the results are still likely to be less readable than the bitmaps.  It may help to add the following to your Xresources (it’s already there in the ones on this site) :

    Xft.hinting:    true
    Xft.hintstyle:  hintslight
    Xft.antialias:  0

### Magnification in old X11

Most newer GNU/Linux boxes can’t do this anymore, and will need a high-DPI setup instead, unless you want to try the “last-resort” approach mentioned below.

(The last computer I had that used X11 hardware magnification was disposed of in 2015 due to space constraints after marriage; our newer equipment was smaller, quieter and less power-hungry, so the old “rattle-bang box” was by then just a standby, except when I needed to use its parallel port for the old laser printer which was degrading beyond modern repair; both pieces of equipment had each given 15+ years’ service to multiple owners but it no longer made sense to keep them going.)

Control-Alt-keypad + should zoom in (multiple zooms may be available) and you can zoom out again with - instead of +.

If it doesn’t work, or if the screen is uncomfortable to look at, the picture is not centred on the screen, or you cannot zoom far enough, then you might need to re-configure your X server (more on this below).  You may be able to manage without zoom by setting even larger fonts, but I recommend configuring the X server if you’re setting up a machine for long-term use, otherwise you could have problems when things don’t fit on the screen or cannot be enlarged.

Warning: Fully-sighted observers may get dizzy if you scroll around quickly, since they often use visual information for balance and they are not in control of the mouse.  Try to avoid or warn about rapid movement when others are watching.

**Backdrop:** The default checkered X backdrop can be annoying especially when you scroll it.  There are various ways of changing it, one of which is the command `xsetroot -solid darkblue` (type it at a prompt or put it in a startup file).  On some systems you can set the backdrop to any image using `chameleon` or a similar program.  Most “desktop environments” do this anyway.

**Reconfiguring the X server:**
For maximum benefit, the X server needs to be configured not only to allow low resolutions (so that Control-Alt-keypad + works) but also to display them in the best way that is allowed by your monitor, video card and signal cable.  This will reduce eye strain.  Configuring X requires root access to the machine and is difficult; the exact settings will depend on your particular brand of monitor and video card and getting it wrong could cause damage.

The following advice should work with modern, reasonably large monitors.  If you have a very old monitor then don’t try it unless you really know what you’re doing.  At any rate it’s **at your own risk**; usual disclaimers apply.  Check that you understand all of these instructions before you begin.

Note: Some motherboards with onboard video do not support the following at all, and their built-in lower resolutions can cause eyestrain on modern displays (unless you can find an LCD monitor whose actual resolution is an exact multiple of the supported resolution).  These setups may need software magnification.

1. Use xtiming.sourceforge.net [July 2025 update: this site is currently experiencing problems] to work out some modelines:
  * For resolutions, try 1152x864, 1024x768, 800x600, 640x480 and 320x240 (calculate a separate modeline for each).  The highest resolution gives good desktop area; you can then zoom in to the lower resolutions.  You probably don’t want to make the highest resolution’s first number any larger than 1.6 or 1.8 times that of the resolution you usually work in, otherwise you may “get lost” since even a full-screen window can disappear too easily.
  * For LCD and other flat-screen monitors:
    * You should check the manufacturers’ preferred refresh rate (probably 60 Hz) and preferred resolution.
    * Sadly, if you set a lower resolution, some flat screens will anti-alias the picture in a bad way that causes eyestrain.
    * **However,** taking the preferred resolution and halving both numbers gives some magnification and usually results in no compromise on crispness.
    * **And** many flat screens can be brought closer to the eye than is practical with CRTs (although a few models do cause eyestrain at close range so choose carefully).
    * **But** some of the newer “16x9” **widescreen** LCDs won’t work well with xtimings’ calculations and you’ll need to tweak the sync pulses manually.  For example, when connecting an old video card to an energy-saving Philips 192EL2, the **only** low-res modeline I found that worked anywhere near OK is `Modeline "680x384" 42.75 680 720 792 896 384 392 396 404 doublescan` (here is the [calculation script](modeline.py))
    * **And** some newer graphics hardware, such as the Broadcom VideoCore IV in the Raspberry Pi, performs its own anti-aliasing on the HDMI/DVI output (especially in a vertical direction) whenever the resolution is less than the monitor’s *even by an integer factor* (and irrespective of the doublescan setting); I don’t yet have a way around this.
  * For CRT monitors (and for TFTs that are more flexible, e.g. on some laptops):
    * You might like to work out and include your “optimal” resolution(s).  To do this, you need to know the best size in points (36, 48 or whatever) for text you read **at the distance of the screen.**
      1. First you need to know your normal distance to the screen.  You can move the screen nearer, but the limits will vary with the type of mounting and the size of your keyboard, desk, chair, etc; you need to be aware of this variation if you use different computers.  Also, if you have variable sight, remember to allow yourself room to get even nearer when your sight is worse (and without hurting your posture too much), which is usually easier than temporary size changes.
      2. Divide the screen’s distance by your normal reading distance for printed text (both measured from your eyes to the text itself, ignoring any magnifying device in between), and multiply by the size in points of the printed text you prefer to read.
      3. If you normally use a magnifier for printed text but cannot use it for the screen, multiply your answer by the scale factor of this magnifier.
      4. If you have a full-screen magnifier (fresnel lens) permanently mounted to the screen, then divide your answer by the scale factor of this magnifier.
      5. Do **not** try to work out your size by setting the “point size” on the screen of a wordprocessor or similar, because that might not be calibrated correctly.
      * Let F be the pixel size of the bitmap fonts you have (the configuration files on this site use 20-pixel bitmap fonts, so that number is 20), D be your monitor size in inches (measure across the diagonal if you don’t know), P be your on-screen pont size, and H and V be the horizontal and vertical parts of your monitor’s aspect ratio (this is normally 4 and 3, but it can be 8 and 5 on some wide-screen laptops).  Take the square of (F*D*72/P), divide it by (H*H + V*V), take the square root, take the nearest whole number, and use a resolution of H times that number by V times that number.
    * For CRT monitors, refresh rates should be between 85 Hz and 100 Hz; higher rates are more risky and lower rates can tire the eyes.  Start with 85 Hz and change it if the monitor starts making an unbearable high-pitched noise (you won’t know this until you’ve finished setting up, so you might have to come back later and adjust things).  For 320x240 you might have to drop the refresh rate to around 75 Hz.
    * For CRTs, enable doublescan for all resolutions up to 800x600 (disable it again if xtimings warns about too high a frequency).  High-resolution CRT monitors can show gaps between scan lines at lower resolutions, and this can tire your eyes even if you don’t notice them, but doublescan can reduce the problem.  At 320x240 it might not solve the problem completely but you probably still need it to get the timings within range.
2. Edit your `XF86Config` or `xorg.conf` file (you’ll almost certainly need root privileges for this).  It’s probably in `/etc/X11` or a similar directory (try `locate XF86Config` and `locate xorg.conf`).  If you have a file called `XF86Config-4`, edit that instead.  Make sure you take a backup of the old file in case something goes wrong.
   * Some newer Linux distributions don’t use `xorg.conf` at all.  You might be able to generate one by doing `Xorg :1 -configure` (the :1 is necessary if you’re already running an X server on :0) and save it to `/etc/X11/xorg.conf`; if that doesn’t work then you could try starting with an empty file and creating just the `Monitor`, `Screen` and `ServerLayout` sections as specified below (each needs an `Identifier`, and you might want to include things like `Option "DPMS" "true"` and `Option "Backingstore" "true"` in the `Screen` section). 
3. Create a section like this, changing it to suit the settings for your monitor:

    Section "Monitor"
       Identifier "whatever you want to call it"
       HorizSync 27-110
       VertRefresh 50-160

       # insert modelines here

    EndSection

  * With some newer X servers you also need to add `Option "NoDDC"` to this section for the modelines to take effect.
4. Find `Section "Screen"` and change the setting of `Monitor` to match the `"whatever you want to call it"` identifier you used above.  If there is more than one `Section "Screen"` in the file then you need to see which one is referred to in the `Section "ServerLayout"` (hopefully there’ll only be one of those), or if in doubt do this for **each** `Section "Screen"` that you find in the file (as long as your X server will only drive one monitor).
5. You might also need to ensure that the names of your modes (e.g. `"1024x768"`) are listed on all `Modes` lines within the section.  If there aren’t any, create one like this:

    SubSection "Display"
    Modes "your mode 1" "your mode 2" ...
    EndSubSection

 * I find it’s useful to delete any frequency annotations (i.e. the `@85Hz` etc) from the `Monitor` section to reduce confusion.
6. You may also need to add a `Virtual` directive to the above subsection(s) and specify the width and height of the highest resolution you want, e.g. `Virtual 960 600`, to stop the driver from adding higher ones.
7. Save the configuration file and re-start your X server.  Check that you can zoom through all the different zoom levels with Control-Alt-keypad +.  Check especially that you are happy with the zoom level that you’ll use most often.  If your monitor has a menu system that tells you the refresh rate, check that this is what you wanted (85 Hz or 99 Hz or whatever) and check that the monitor is behaving normally.
8. If the mouse pointer in doublescan modes looks squashed and/or in the wrong place, you may have to upgrade your X server to a later version, or change the graphics card, or do without doublescan.
9. If you can switch between modes OK but the lower-resolution modes do not let you scroll around the larger desktop with the mouse (i.e. you are “locked in” to the smaller desktop size), then this might be caused by the “configurator” on the display manager GDM; try disabling it by setting `Configurator=/bin/true` in `/etc/gdm/gdm.conf` or `/etc/gdm/custom.conf`, or by using `startx` instead of GDM.
  * On some setups (e.g. Raspberry Pi) neither mode-switching nor scrolling works; you might have to fall back to a high-resolution non-scrollable desktop with larger fonts, or if you are on reasonably-capable hardware you could try setting one low-resolution mode and obtaining a larger virtual desktop by following the instructions on last-resort X11 magnification (the x11vnc method) but omit the scaling options because your hardware is doing the scaling.  (As you are omitting these, you can cut out x11vnc altogether and just connect the vncviewer directly to the vncserver on `:2`.  If nobody else can access your system then you might want to use TightVNC Server and do `echo | tightvncpasswd -f > .vnc/passwd` for an empty VNC password, although the prompt may still be displayed.  Alternatively some clients can be set to read the password file directly by putting `-passwd .vnc/passwd` on the command line.)

**More things to check (especially for old CRTs):** If you will be using your monitor for long periods then it’s best to get the image as stable as possible.  You can check the following things after you have done the above setup.

* For each zoom level, you may need to re-adjust the monitor’s horizontal and vertical position and horizontal and vertical size.  Try to get the display’s image as large as possible (don’t waste any of the screen to a black border).  With good monitors you should only have to do this once after you have configured the X server, but you may need to do it again if you change the configuration of the X server.
* You may also want to turn Contrast to maximum and set Brightness fairly low but not too low, depending on the lighting in the room.
* Put lots of text on a black background in the centre of the screen, and examine it with a powerful magnifier to make sure none of the phosphor dots at the edges of letters are flickering.  If they are then perhaps the horizontal frequency is too high for your signal cable; it may help to reduce the refresh rate, but don’t set it too low or you’ll get another kind of flicker.  It can also help to ensure the monitor signal cable is straight, firmly plugged in, and held away from other cables such as power cables, and don’t set the brightness too high.
* If you see the mouse cursor flicker when the window under it is changing (particularly noticeable in xaos), it’s probably because your (older?) graphics card doesn’t properly support the mouse in doublescan modes, so X is having to emulate it in software, which is not so good.  Normally you can just move the mouse out of the way of anything that’s moving or changing, but if that’s sometimes unavoidable (e.g. you use xaos) then you might want to temporarily switch into a non-doublescan mode for that particular task.  You could make both doublescan and non-doublescan versions of your most frequently used mode and include both of them in the Monitor and Screen sections of XF86Config (see instructions above), so you can switch between them using the Control-Alt-keypad +/- keys.  In this case you need to give the modes different names even though they have the same resolutions, so instead of saying `"640x480"` you need to say `"640x480d"` or `"640x480s"` for example, and make sure both are listed in the `Modes` lists under `Screen`.

### Last-resort software magnification in old X11

I have not tested this on recently-released distributions.  ​This approach takes more CPU and RAM than the hardware approach, and has some other disadvantages, but it can be better than nothing.

**x11vnc:** The idea is to run your desktop on a VNC server, and get a VNC client to display a magnified image of this.  Few of the free VNC clients have unlimited magnification functionality, so we use x11vnc (a VNC server for real X servers) to magnify another X-based VNC server.  This method is usually quite fast and allows you to scroll around a large virtual desktop.  It assumes that you are using a Unix-based terminal.  If you are using X11 remotely from a non-Unix terminal then see below.

1. Using your package manager or otherwise, install the VNC server program (vncserver or vnc4server or tightvncserver), the TightVNC viewer program (xvncviewer or xvnc4viewer or xtightvncviewer), and x11vnc.
  * If x11vnc is not in your package manager, you may be able to copy the binary from another distribution.  (If you have problems compiling yourself, try `./configure --without-zlib --without-libz --without-jpeg` as you won’t need those features for this hack.)
  * **If you are able to compile x11vnc yourself** you may be interested in my [x11vnc patch for large mouse cursors](x11vnc.diff)—it applies to x11vnc version 0.7.1 and you should do `patch x11vnc/x11vnc.c x11vnc.diff` and `CPPFLAGS=-DBIG_CURSOR make -e` instead of `make`.  **Newer versions have this functionality integrated** and you can use (e.g.) `-scale_cursor 4:nb` on the x11vnc command line.
  * If you can’t get the TightVNC version of vncviewer then you may not be able to do bump scrolling, in this case you may want to limit your virtual desktop size to half your actual screen resolution.
  * You may be able to copy across the `xvncviewer` binary and its `libvncauth.so.0` library from another distribution, in which case don’t forget to add the library’s directory to `LD_LIBRARY_PATH`.
2. Give yourself a VNC password by using the vncpasswd command.
3. Start the VNC server by using a command such as `vncserver :2 -geometry 1052x864 -depth 16`
4. Quit the VNC server (e.g. use `killall Xvnc Xrealvnc Xtightvnc`); this first run was only to create the initial `.vnc/xstartup` file and does not need to be done every time.
5. Edit the `.vnc/xstartup` file and add the line `x11vnc -display :2 -rfbport 5903 -forever -scale 2:nb &` where the 2 after the `-scale` is the scale factor you want (the `:nb` means avoid blurring, which gives a clearer image when using integer scale factors).
6. If you have x11vnc version 0.7.2 or newer, add `-scale_cursor 4:nb` to the above command line (the 4 is how much you want the mouse cursor to scale in relation to the *unmagnified* screen).
7. **Note:** If the .vnc/xstartup file was not present, then you’re probably using Xrealvnc instead of Xvnc, in which case it will look at your Xsession on startup; if you’re using a real X server as well then you’ll have to script some way of distinguishing between the two (perhaps by checking the DISPLAY environment variable).
8. Start the VNC server again using the above command.
9. Connect to the magnified display using the command `xvncviewer -fullscreen :3`
10. You can disconnect and reconnect the viewer as you wish.  To shut down the desktop, do `killall Xvnc Xrealvnc Xtightvnc`.
11. If the desktop was completely blank, or not to your satisfaction, then you may need to edit the `.vnc/xstartup` file before starting the server again.
12. You might want to put the above into your `.xsession` and avoid running a window manager outside the VNC client, so that all your keypresses reach the window manager on the VNC server (but remember to add a `sleep 2` to let the server start before running the viewer).
13. If possible, enable backing store in your X server by having the line `Option: "backingstore"` added to the `Device` section of `XF86Config` and restart the X server.  This will improve the responsiveness of scrolling around the desktop.  (If it takes time to repaint the screen when you scroll then that means backingstore has not been enabled properly.)

Note: Any `setxkbmap` command (to change the keyboard e.g. for Dvorak) may not work unless it is sent to the *real* display.  `xmodmap` should work however.

**Windows VNC Viewer version 3:** One of the few free VNC clients that can magnify is [version 3 of vncviewer.exe](https://ssb22.user.srcf.net/setup/vncviewer.zip).  Note that this is an old version; newer versions introduced a charge for the magnification functionality (it’s still free in the Windows version of TightVNC but the range is more limited).  This old vncviewer.exe works in Wine, but is slower and won’t let you drag the screen around a large virtual desktop.  It can sometimes blur the magnification too, even with integer magnification factors.  It may however be useful if for some reason you can’t get x11vnc to work.
1. Give yourself a VNC password by using the `vncpasswd` command.
2. Start the VNC server by using a command such as `vncserver :2 -geometry 640x480 -depth 16` (the geometry, in this example 640x480, should be at most half of your screen’s resolution, so if your resolution is 1280x1024 then your maximum geometry is 640x512; if you want to magnify three times, choose one third instead of half, and so on: any larger and you will have to scroll around the desktop using scrollbars, which can be awkward)
3. Run `wine vncviewer.exe` (or run it under Windows) and a small dialogue box should appear.  In Wine it is possible that the text in the dialogue box will not display; don’t worry about this.
4. Double click on the text box and type the server you want to connect to; this is `localhost:2` for the above example.  Now press the button on the bottom right to bring up the Settings dialogue.  Find the check box that has two small text boxes immediately to the right of it, and check it.  Double click on the first text box to the right of it and type 2 (the magnification factor).  Press Enter, then click the top button.
5. If the connection is successful, you will now be prompted for your VNC password.  Type it and press Enter.  The magnified desktop should now appear.
6. If you know what you’re doing you can use a remote server, tunneled over SSH if necessary; in this case it’s also better if you have the old version 3 of the server, otherwise the version 3 client will blur its magnification more often, or you can try TightVNC instead (and you get better compression if you have the TightVNC versions of both viewer and server).
7. You can disconnect and reconnect the viewer as you wish.  To shut down the desktop, do `killall Xvnc Xrealvnc`.
8. If using X11 locally (rather than Windows), you might want to put the above into your `.xsession` and avoid running a window manager outside the VNC client, so that all your keypresses reach the window manager on the VNC server.  This is especially the case because you will be doing a lot of keyboard navigation due to the lack of being able to drag around the desktop.  (Choose a window manager with many shortcuts, e.g. FVWM2, to run on the VNC server.)

**NX Client and coLinux:** When working on a remote desktop over a limited-bandwidth connection, you may want to use nomachine.com’s NX Client instead of VNC.  However it seems that NX Client can’t very easily magnify the desktop (and magnifying it at the remote end using the above method will slow down NX considerably), so you may want to run the NX client on a local VNC server and then use one of the above methods to enlarge that local VNC server.

If your local machine is Windows then setting up an off-display VNC server can be difficult.  But if you have administrative access to the system then (if the system won’t run Linux) you might be able to install CoLinux:

1. Download the stable release from colinux.org and go through the installation wizard (I chose the Debian image)
2. After installation, look in C:\Program Files\coLinux, decompress the .1gb.bz2 disk image, and rename it to “root_fs”.
3. Look in control panel’s Network Connections for “Local Area Connection” (or whatever your default outgoing Internet connection is), right-click on it and choose Properties, Advanced, check the “Allow other users...” box and in the drop-down box choose the new “Local Area Connection 9” (which is a virtual connection between the host Windows OS and the guest Linux OS).  This will allow coLinux to use your outgoing Internet connection.  Now run coLinux like this: `colinux-daemon.exe kernel=vmlinux cobd0=root_fs root=/dev/cobd0 hda1=:cobd0 eth0=tuntap,"Local Area Connection 9" mem=512 -t nt` (you may want to put that in a batch file.)  The initial root password is “root”, and you’ll have to type in qwerty regardless of your keyboard map; however you can do “loadkeys dvorak” once logged in, if you want to type Dvorak or any other layout.  (To shut down the virtual Linux system, just give it a halt command.)
4. You may need to edit the APT configuration (nano /etc/apt/apt.conf.d/70debconf) and add `APT::Cache-Limit 12582912;` and `APT::Force-LoopBreak true;` then save and exit, and do `apt-get update` (it may give some 404 errors which can be ignored as long as the main mirrors respond OK).
5. If your first `apt-get` command goes wrong, try this:

    echo '#!/bin/bash' > /usr/sbin/update-rc.d
    chmod +x /usr/sbin/update-rc.d
    apt-get -f install

and then repeat the apt-get command.

Once you’re up and running with CoLinux, you can apt-get the `vncserver` package; be sure to get the fonts packages also.  Put `exec /usr/NX/bin/nxclient` into `.vnc/xstartup` (or `.Xsession` if appropriate, see note above), then add the above `x11vnc` command before it (if you’re using x11vnc for the magnification, which is the recommended method).  If your Windows VNC client is TightVNC then you may need to add `-rfbauth ~/.vnc/passwd` to the x11vnc command line because some versions of TightVNC will hang if the server doesn’t need authentication.  If the NX server is on a VPN then you may find that coLinux can’t see this VPN, so you’ll need to set up an SSH tunnel and tell nxclient to connect to that instead, and `.vnc/xstartup` may be a good place to put the appropriate ssh command; if you’ve set up an RSA identity then you should just need something like `ssh -N -l userID -L localhost:8222:nx-host:22 ssh-relay-host &` (you may also want to include an `xsetroot -solid darkblue` command so you don’t have to look at a checkered background if the connection is delayed).

Finally, start the VNC server by using a command such as `vncserver :2 -geometry 480x300 -depth 16`

You may wish to add an echo command after that, reminding you to connect the VNC client to the IP address of the coLinux machine, display 3.

If your VNC client does not support bump scrolling, then the geometry should be half the screen’s resolution (assuming the magnification factor is 2 as it is in the above x11vnc command).  This may lead to a very restrictive display.  If nxclient says “press Next to continue” but the Next button is off-screen, you can just press Enter, but once you’re on the remote desktop, you may find that some applications give you dialogue boxes that are too big for the screen.  **If your VNC client supports bump scrolling** then you can multiply this reduced geometry by between 1.6 and 1.8 (i.e. make it 80-90% of the screen’s resolution if the magnification factor is 2) and then you’ll be able to pan around a virtual desktop.  (You probably don’t want to make the virtual desktop any larger than 1.6-1.8 times the screen size, otherwise you may “get lost” since even a full-screen window can disappear too easily.)

TightVNC 1.3.9 under Windows supports bump scrolling in full-screen mode, but it’s so fast that you’ll probably only be able to position the display on a corner.  If you need a more steady scrolling speed, try RealVNC Free Edition 4.1’s full-screen mode (you might want to turn off ‘Render Cursor Locally’ in the RealVNC Options, because x11vnc displays one).  I find that neither of these options are as good as bump scrolling in real X11.  You might want to have two configurations, one with a small geometry for normal use and another with a larger geometry for when you need to configure applications that have large dialogue boxes; you will however have to re-login to the remote machine each time you change between these configurations.

**ezoom:** if you install a Compiz desktop, you should see an option called “Enhanced Zoom Desktop” in CompizConfig Settings Manager, but if the hardware’s not right then attempting to zoom in may have no effect.  This, together with the awkwardness of setting up Compiz on many distributions and the restriction against using other window managers, puts me off suggesting ezoom.

### VNC with inverted colours

Depending on the age of your distribution, you may have to replace `vncserver` with `vnc4server` below.

People with certain eyesight conditions find bright backgrounds difficult to work with, but some applications cannot be configured otherwise.  If you have to use one of those applications, it might help a little to use a program that intercepts the display at a low level and inverts all the colours.  This is a **last resort** as the result is unlikely to be optimal.

The following hack will do this on a Linux system.  It relies on VNC (Virtual Network Computing) and we will be making a small modification to the VNC viewer.

Note: This works as-is only for Debian-based distributions.  If your distribution is not Debian then you need to modify this.

First, save [invert-vnc.diff](invert-vnc.diff) as `/tmp/invert-vnc.diff`

Then paste the following into a root prompt: the lines are long):

    apt-get --no-upgrade install xnest libjpeg62-dev \
      xvncviewer vncserver vnc-common libvncauth0 dpkg-dev
    cd /tmp
    apt-get source xvncviewer
    cat invert-vnc.diff | patch vnc-*/vncviewer/desktop.c

Then, if you are using Debian 3.0 aka Woody, paste in the following (ONLY if you are using Woody):

    cd vnc-*/vncviewer
    gcc-2.95 -I ../include/ -I ../vncviewer/ \
      -I ../Xvnc/lib/zlib $(
        echo *.c | sed -e 's/corre.c//' \
          -e 's/hextile.c//' -e 's/rre.c//' \
          -e 's/zlib.c//' -e 's/tight.c//') \
       /usr/X11R6/lib/libXt.so.6.0 \
       /usr/lib/libjpeg.so.62 \
       /usr/lib/libvncauth.so.0 \
       /usr/X11R6/lib/libXaw.so.7 \
       /usr/lib/libz.so.1
    mv a.out /usr/local/bin/invert-vnc

For Debian versions later than 3.0, paste the following instead of the above:

    cd vnc-*
    apt-get --no-upgrade install linux-kernel-headers
    ./configure && make && mv vncviewer/vncviewer \
                              /usr/local/bin/invert-vnc

Finally, paste in the following script.  You might like to change the variables at the beginning, especially `Geometry`.  Security note: all local users will be able to interfere with your X display.

    cat > /usr/local/bin/run-inverted <<'EOF'
    #!/bin/bash
    
    Geometry=640x480
    VNCDisplay=98
    XnestDisplay=99
    
    Xvnc :$VNCDisplay -geometry $Geometry -ac \
          -once -deferglyphs 16 -nolisten unix &
    vnc_server_pid=$!
    Xnest -geometry $Geometry -ac \
          -nolisten tcp :$XnestDisplay &
    xnest_pid=$!
    sleep 3
    DISPLAY=:$VNCDisplay xsetroot -solid white
    DISPLAY=:$VNCDisplay $* &
    application_pid=$!
    DISPLAY=:$XnestDisplay invert-vnc \
        -encodings raw -fullscreen :$VNCDisplay &
    DISPLAY=:$VNCDisplay xset s off
    DISPLAY=:$XnestDisplay xset s off
    wait $application_pid
    kill $vnc_server_pid ; kill $xnest_pid
    EOF
    chmod +x /usr/local/bin/run-inverted

If you use a different keyboard layout (e.g. Dvorak) then you may need to add the appropriate `xmodmap` command to `/usr/local/bin/run-inverted` (use `export DISPLAY=:$VNCDisplay` first; do not send it to `:$XnestDisplay` or the real display).  This should go before the `wait`.  `setxkbmap` is less likely to work than `xmodmap`.

The program is now ready for use.  As an ordinary user (not root) do `run-inverted application` where application is the command to run the application that you wish to use with inverted colours.  (If you need a window manager, create a script that launches the window manager and the application and run that.)

## 2. High-resolution non-scrollable desktop with larger fonts
* Needs newer software that does not use bitmap fonts
* You can work full-screen and use Alt-Tab or other mechanisms to switch windows (can be slightly more fiddly than panning, but not too much if you have tabbed terminals, editors and browsers)
  * GNOME 3 and Unity introduced a different behaviour: Alt-Tab switches between applications, and Alt-backquote switches between windows within an application—a bit like Command-Tab and Command-backquote on the Mac
* Works well with modern “desktop environments” and laptops

The easiest way to do this is usually to set the software’s DPI (dots per inch) value to something higher than the actual DPI.  For example, if your real DPI is 96 and you say it is 160, you get a basic 167% zoom and can then make further adjustments to individual applications.  (If the system-wide zoom is too big then the desktop manager might not fit on the screen, so it can be better not to set the DPI too high but to increase font sizes in applications.  However, some increase of DPI can be useful.) 

Your desktop’s setup options should have either a DPI option or a scale-factor option.  The details depend on your desktop environment: 

### Recent Gnome 3 (e.g. 3.32)

Settings > Devices > Displays can scale up to 400%; Settings > Universal Access can change mouse cursor size; gnome-tweaks can set dark themes.

* Some programs set a “minimum window size” which will also be enlarged by the scale factor, and this can result in windows that are too large for the screen with some controls unreachable.  If the application cannot be changed (for example because it is a proprietary program like Zoom) then you might be able to use Gnome’s Alt-F7 + arrow keys (or if your keyboard has a “Super” key—sometimes called a “Windows” key—you can hold that while clicking and dragging with the mouse) to move the affected window partially off-screen at the *top* when you need to bring its bottom part on-screen.

### Earlier Gnome 3.x
Set the scaling factor from the command line: `dconf write /org/gnome/desktop/interface/text-scaling-factor 1.67` (or whatever; maximum is 3.0)

Gnome 3 also has software magnification available under Universal Access / Zoom (or via keyboard shortcuts: hold down the Super “Windows” key and Alt, and 8 to toggle or +/-) but this can be unresponsive on some machines and can also make Gnome 3’s top-left “Activities hotspot” more disorienting.
### Gnome 2.x
Use System Preferences > Appearance > Fonts > Details > Resolution (DPI).
### Cinnamon 3
try Menu > System Settings > Fonts > Text scaling factor (set it to 1.7 or whatever); takes effect on most parts of the desktop immediately, and the taskbar after a restart
### LXDE
Set the DPI from the command line as follows (you can put this in a shell script and set it to run on startup):

    echo "Xft.dpi: 160" | xrdb + - &&
    if killall -w -u $(whoami) lxpanel pcmanfm; then
    lxpanel &
    pcmanfm --desktop --profile LXDE &
    openbox --replace &
    fi

You may wish to run `obconf` and set Openbox to put the Maximize button on the *left*, so it is more easily reachable when a window sizes itself larger than the screen and you want to bring it back to screen size.

For a larger mouse cursor in LXDE, try the `crystal-cursors` or `crystalcursors` package (you might need to select it in the “Appearance” or “Customise Look and Feel” settings).  Also try `qtconfig` in `qt4-qtconfig` for setting the font size of Qt applications.
### LXQt
You can either make a startup script like in LXDE:

    echo "Xft.dpi: 160" | xrdb + - &&
    if killall -w -u $(whoami) lxqt-panel pcmanfm-qt; then
    lxqt-panel &
    pcmanfm --desktop --profile=lxqt &
    openbox --replace &
    fi

Or you can use Preferences / LXQt Settings / Session / Global Screen Scaling, but I’ve not yet seen a version of this that also scales non-Qt applications like Firefox: these will need setting separately if you rely on the Global Screen Scaling option instead of `Xft.dpi`.

You can set a larger mouse cursor in Preferences / LXQt Settings / Appearance.

Here’s a [darker LXQt theme](.config/lxqt/lxqt.conf) if you need one (save as `.config/lxqt/lxqt.conf`)

Openbox’s ability to drag windows to other virtual desktops can be annoying if done accidentally when all you want is temporarily to place a window mostly off-screen; you can turn it off in `obconf-qt` by setting Desktops number to 1. (Then optionally remove the visible switcher via the panel’s Manage Widgets option, but this alone won't disable drag to new desktop without the obconf setting.)

### KDE 5
K menu > System settings > Display & Monitor > Scale Display. 
### Editor (for all the above)
FSF Emacs 23+ can use TrueType fonts with the DPI setting; my emacs configuration files might help.  Alternatively try `gedit`, or `leafpad` for very lightweight situations.

## Large print in Wayland
Wayland is a newer replacement for X11.  In Wayland, a lot depends on the “compositor” component.  These notes are for the Wayfire and LabWC compositors, as shipped with the Raspberry Pi Desktop OS based on Debian 12 in 2023 (Wayfire) and Debian 13 in 2025 (LabWC).
### Screen magnification
* Hold down the ‘Super” key (the one with the commercial logo on it, called the “start” key in Windows) while scrolling with the mouse wheel.
* This gives full-screen GPU-driven magnification with panning, but in Wayfire it blurs (and fails to enlarge the mouse pointer), and in both Wayfire and LabWC it doesn’t wait for the mouse to hit an edge before panning—so it doesn’t feel as stable as old X11 hardware zoom.
### Permanent desktop scaling
In LabWC on Raspberry Pi OS 13, use Preferences—Control Centre—Theme and increase Font size: this now controls the entire desktop.
* To get the same effect in Wayfire on Raspberry Pi OS 12, edit `~/.config/wayfire.ini` and add a line like `scale = 1.5` to an `[output]` section (e.g. `[output:HDMI]`—just create a section called `[output]` if none are there).  A bug in at least some versions of Wayfire results in the scale being lost whenever you open any Preferences dialogue (such as to set keyboard layout) and you have to restart the session to get it back.
* As the resulting desktop is not scrollable and it limits what can be shown on screen, you might want to use just a moderate scale factor here and make up for the rest by setting larger fonts in applications.  You can then maximise windows and use Alt-Tab to switch between them. 

## WINE
If you run Windows programs using WINE, you can change the overall size by adjusting the DPI setting `LogPixels` in `.wine/system.reg` (its value is hex).  In newer versions you can also use the `winecfg` program’s “Graphics” tab to set DPI.  WINE 9.17 introduced “surface scaling” which results in pixels being enlarged (making anti-aliasing worse) when DPI is high, unless turned off via the registry key `HKEY_CURRENT_USER\Software\Microsoft\Windows NT\CurrentVersion\AppCompatFlags\Layers` default value `~ HIGHDPIAWARE`

If some characters in menus etc don’t display, then you might also need to set `MenuFont` and/or `IconFont` and/or `MessageFont` (depending on WINE version) in `.wine/user.reg`.
For example, to set it to WenQuanYi Micro Hei, add `[Control Panel\\Desktop\\WindowMetrics]` with `"MenuFont"=hex:a,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,57,0,65,0,6e,0,51,0,75,0,61,0,6e,0,59,0,69,0,20,0,4d,0,69,0,63,0,72,0,6f,0,20,0,48,0,65,0,69,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0` and similarly with `"IconFont"` and `"MessageFont"`.

If you need a dark background in WINE apps you can try appending this [dark background registry snippet](dark-background.txt) to `.wine/user.reg` (here’s a [Unicode .reg version](dark-background.reg) in case you want that scheme on real Windows: works on XP but a restart might be required).  If you want anti-aliasing (and don’t already have it), try this [antialiasing registry snippet](antialias.txt) also (this sets greyscale anti-aliasing; ClearType is not recommended unless you can calibrate the gamma setting to your monitor).

If you use the “Wineskin Winery” wrapper on an old Mac, the `.reg` files will be located in your application’s `Contents/Resources` directory.

If those colour changes cause unreadable text in printouts, you might need to delete the relevant colour commands from the PostScript, e.g. if you’re using a machine behind a firewall that runs CUPS and you have administrator access then go to http://localhost:631 and Add Printer, device LPD/LPR printer, URI `socket://localhost:12000`, make Generic / Generic PostScript printer; before printing to this printer do
`nc -l -p 12000 | grep -v "^1.00 1.00 0.00 setrgbcolor$" | grep -v "^0.89 setgray$" | ps2pdf - output.pdf`
(may need adjusting for some CUPS/Wine versions).  Alternatively if you don’t have root access but have compiled your own WINE, you can temporarily disable accidental printing by moving `wineps*` out of `lib/wine`.

## Terminal applications

I usually find that Unix terminal applications work reasonably well in large print as long as the terminal itself can.  However, increasing the print size usually means that fewer rows and columns are available, and some applications don’t work very well on terminals with fewer than normal rows and columns.  This can sometimes be fixed by configuring the application.  Here are some of my dot-files for this and related fixes: 

* [.muttrc](.muttrc) is used to configure the terminal-based email client `mutt`.  This dot file works better if you add [.message-formatter](.message-formatter) (requires Python, tested in both 2 and 3)—check the comments at the start of `.muttrc` if you’re on a Mac or other BSD-based system.
  * If you use Alpine (e.g. for IMAP over a slow connection if the system’s version of Mutt doesn’t handle it efficiently), see [alpine.txt](alpine.txt).
* [.jedrc](.jedrc) and [.nanorc](.nanorc) are used to configure the `jed` and `nano` editors (you might also want my emacs configuration); `jmacs` is also useful but I do not have configuration files for it
* [.lynx.cfg](.lynx.cfg) and [.lynxrc](.lynxrc) are used to configure the terminal-based Web browser `lynx`.  This works only if you put `export LYNX_CFG=$HOME/.lynx.cfg` in your .bash_profile/.bashrc.
* [.tmux.conf](.tmux.conf) configures the terminal multiplexer `tmux` (which is like `screen` but might handle UTF-8 better)
* For `curl`, put `-sS` into `.curlrc` to stop problems with the progress bar on small terminals (also helps with programs that use curl, such as HomeBrew)

For GNU/Linux `top`, try pressing `f` and turn off columns you don’t really need, e.g. turn off priority (`h` on older versions of `top`), nice (`i`), RSS (`q`), etc and perhaps turn off username (`e`) and turn on uid (`d`) on single-user systems, press `c` to toggle extended commandline and press `W` to write to `.toprc` or `.config/procps/toprc`.  Mac/BSD `top` is less flexible.

If you need IRC via `weechat`, to make more room for messages try `/set weechat.look.prefix_align none` and `/set weechat.look.buffer_time_format "%H:%M"` and `/set weechat.bar.buflist.size 5` or `/bar hide buflist`  (and use `/buffer 1`, `/buffer list` etc to navigate them), and `/set weechat.bar.nicklist.size 10` or similar.

### Console

On modern GNU/Linux distributions the console font size is quite small.  You might be able to go some way toward enlarging it by using this [.console-setup](.console-setup) and putting `setupcon` in your `.bash_profile`, or if you don’t have setupcon then try `setfont /path/to/`[TerminusBold32x16.psf.gz](https://ssb22.user.srcf.net/setup/TerminusBold32x16.psf.gz).

On FreeBSD the command is `vidcontrol -f /usr/share/vt/fonts/terminus-b32.fnt` but to make this work in FreeBSD 11 and above you also need to put `hw.vga.textmode=0` into `/boot/loader.conf` (although at least text-mode on means the starting size is not quite as miniscule as became popular on GNU/Linux).

For fonts larger than 32px (and for CJK), on GNU/Linux you might be able to install fbterm—here’s an [example .fbtermrc](.fbtermrc), and if you want to turn off anti-aliasing you can try this [non-antialiasing .fonts.conf](.fonts.conf) (on some systems it needs to be saved as `.config/fontconfig/fonts.conf`) but some versions of fbterm corrupt the display when anti-aliasing is turned off.

## Emacs configuration

The [.xemacs directory](.xemacs/) (wrapper in [.emacs](.emacs)) has some Emacs configuration files for low vision (large fonts, saving screen real-estate, and dark backgrounds) with many Emacs features supported.  This is meant for:

* FSF Emacs 23 through 29, in high-resolution GNU/Linux (standard packages), Android (Termux), Mac OS X (Carbon or Aquamacs), and Windows (binary download from GNU)
* FSF Emacs 22 (some Mac OS X machines have only this version)
* XEmacs 21 with bitmap fonts, in older Linux distributions with low-resolution scrollable desktops (I haven’t used this since 2015)
* Terminal mode in any of the above

On a Mac in 2020 I was able to install FSF Emacs 27.1 even on old OS X 10.7 via MacPorts’ `sudo port install emacs-mac-app`, but MacPorts dropped pre-10.10 support for Emacs in 2022 (`port upgrade` fails without removing the old version).  Earlier versions of Emacs had more bugs: there’s a crash in Mac Emacs 23 and 24 (and Aquamacs) involving the menubar and FTP buffers (here’s a [patch to 24.3rc3 I submitted](emacs-patch.txt)); the bug is not present in Emacs 22 (but that lacks some of 23+’s functionality) and was fixed in Emacs 24.4 but that introduced *another* bug (albeit a less serious one) regarding use of the non-leftmost pull-down menus when zoom is in effect, which was still present in various forms as late as 26.1 (the last precompiled binary to work on older 10.7 Macs), but is gone in 27.1 if you can compile that with MacPorts.

If you are using the even older XEmacs 21, it might occasionally “mess up” the fonts: if that happens, try `M-x fix-fonts`.  Also in XEmacs, if you can’t zoom in, try `M-x pc-default-font-bigger` or `M-x pc-default-font-double` (if the result is too big for the screen, `M-x smaller-frame`, `M-x even-smaller-frame` or `M-x very-small-frame`).

Look through the `.el` files for other user functions and things you can customise.

If you’re on a machine that has only vim, I do also have a [.vimrc file](.vimrc).

## RISC OS and low vision
Modern versions of RISC OS have a function which magnifies almost everything by a scale factor of 2 without sacrificing resolution, i.e. it increases the assumed DPI from 90 to 180: middle-click the rightmost-but-one icon on the taskbar and move the mouse over the Mode option, delete any `EX1` and `EY1` options (if present), and add `EX0 EY0` so the complete mode string will look something like `X1280 Y1024 C256 EX0 EY0`—then press Enter.

* This might not work well on an emulator.  It works on old RISC PC hardware, and on the Raspberry Pi (RISC OS 5).
  * If the Raspberry Pi’s RISC OS gets stuck in “Mode 28” (640x480) and displays “This screen mode is not suitable for displaying the desktop” when you try to set a modern mode, then that *might* be due to corruption in the CMOS file and/or other files on the associated FAT partition—at least some versions of RISC OS do not properly “synchronise” all MicroSD cards when unmounting, meaning corruption can occur if power is lost too soon after the last write.
  * If this happens, boot into GNU/Linux to run `fsck.vfat` on the affected `/dev/mmcblk0` partition (`systemd` does this automatically if you’ve added the partition to `/etc/fstab` with a non-0 sixth field), mount it, and restore affected files from `packages.riscosopen.org/noobs/RISC_OS_Boot.tar.xz`—RISC OS should then boot correctly.
  * Corruption can happen again, especially if you’re on a Pi Zero (i.e. no network access from RISC OS) and you try to load software onto RISC OS by placing .zip files into the FAT partition—to do this more safely, rename the file to something other than .zip, and then Set Type to Zip from within RISC OS *after copying to non-FAT storage*, as it seems the Filer can corrupt the FAT as soon as it sees a directory with a zip file in it.
  * Corruption to `bootcode.bin` shouldn’t matter if you’re using the an OS switcher (like PINN or NOOBS) that bypasses the `bootcode.bin` of the OS it switches into, which is just as well as there seems to be a failure mode where the first file is partly overwritten by old FAT sectors on *every* boot—but this can be addressed by reformatting the FAT partition with `mkfs.vfat` (choose the partition carefully!) before restoring the files as above
  * I also suggest adding `disable_mode_changes` to cmdline to tell the Pi’s GPU to scale all modes to the monitor’s actual resolution and so you can specify your monitor’s characteristics in `config.txt` (e.g. `hdmi_group=1` and `hdmi_mode=16` for 1080p, or if you have a TFT monitor supporting 50Hz and want to run ADDFS, `hdmi_mode=31` for 1080p, which might need `hdmi_ignore_edid=0xa5000000` and perhaps `F50` before all `EX0` references once in RISC OS); you might also want to specify `disable_overscan=1` in `config.txt` so modes with a different aspect ratio aren’t stretched, and/or install AnyMode.
  * If you’re not sure what your monitor’s actual resolution is, try booting into GNU/Linux, let it auto-detect, and run `tvservice -s` (if you’re connecting to a flat-screen TV then you might find it’s something like 1360x768, in which case fonts will look better if you set `X1360 Y768 C16M EX0 EY0` in RISC OS)
* StrongED won’t scale bitmap fonts (and versions before 4.7 display them strangely) so right-click on the “tick” icon and choose an outline font.  This needs to be done for each editing mode you use.  For dark colours, left-click the “tick” and select Display / Alternative colour scheme.  (There’s also a “Windows” option that lets you set the default size and position.)
* Applications like NettleSSH (which uses ZapRedraw) can also have trouble displaying large sizes.
* NetSurf gives minor display corruption on scrolling (due to its Bug 513); this is improved by turning *off* the menu’s Display / Render / Buffer all rendering
* ADFFS has been known to corrupt the screen display on launch if an `EX0 EY0` mode is in effect, and to shrink the mouse cursor if an `EX0 EY0` mode is re-entered before ADFFS is *completely* quit (Quit > Filer and FS) 

To ensure RISC OS is magnified on startup, open $.!Boot.Choices.Boot.Tasks, edit one of the files (in RISC OS 4 edit `!Boot`; in RISC OS 5 edit one of the others, or create a new file of type ‘Obey’—try `ScrnSetup`) and add the line `WimpMode X1280 Y1024 C256 EX0 EY0` or whatever.

If you need more magnification, you can reduce the resolution, but if only one application needs to be magnified then it may be better to adjust the fonts in that application.

You can adjust the desktop font on RISC OS 4 by opening !Boot and choosing Fonts (I use Homerton.Medium).  This should not be necessary on RISC OS 5.

Alternatively, use a VNC server and magnify at the client (which has the advantage that you can share a one-input monitor without using a KVM switch).

### Setting the clock without NTP
If your sight condition makes it harder to look off-screen at a wall clock etc, then you might want to ensure the on-screen clock is correct.  The Raspberry Pi has no battery-backed clock, and old RISC PCs can have broken batteries, so if NTP is not available then you could

1. run a Telnet server on RISC OS and have another machine telnet in and set the time, or
2. on a Raspberry Pi that dual-boots between RISC OS and Raspbian / Raspberry Pi OS, arrange for Raspbian/PiOS to save its clock to the RISC OS partition before rebooting into RISC OS (assuming you’ve arranged for the Raspbian/PiOS clock to be correct, e.g. daytime query over PPP connection from a USB modem that RISC OS wouldn’t support...)

This [Python time-setting script](riscos/riscos-time.py) can either run on the other machine and perform the telnet commands, or run on Raspbian/PiOS and save the clock to RISC OS on shutdown.  See comments at start of the script for usage.

### High contrast mode for RISC OS 4
(in RISC OS 5 this is less helpful but still works partially)

Some people prefer to have dark backgrounds and light text.  To achieve this throughout RISC OS, use my [RISC OS high-contrast theme](riscos/high-contrast/): run the HighContrast obey file (you'll need to set the file types if fetching from this Git repository instead of my home page).  The NormalColours obey file can be used to temporarily switch back to normal colours, which you will sometimes need to do because some programs don’t work well with HighContrast.  After making each change, close and re-open any Edit windows (that way NormalColours still gives you high contrast in Edit).

If you want you can add the line `Filer_Run $.high-contrast/zip` (correcting the path as appropriate) into $.!Boot.Choices.Boot.Tasks.!Boot so that the options are available on startup (you could also add the line `Run $.high-contrast/zip.HighContrast` if you want it to be selected by default).  Both of these rely on SparkFS being run first (so put the line late in the file).  Some applications will display differently depending on whether they are loaded before or after HighContrast.  Sometimes you will have to explicitly set the application’s foreground colour to something other than black, or change the application’s Choices.

# Copyright and Trademarks
All material © Silas S. Brown unless otherwise stated.
Android is a trademark of Google LLC.
CJK was a registered trademark of The Research Libraries Group, Inc. and subsequently OCLC, but I believe the trademark has expired.
Debian is a trademark owned by Software in the Public Interest, Inc.
Firefox is a registered trademark of The Mozilla Foundation. 
FreeBSD is a registered trademark of the FreeBSD Foundation.
HDMI is a trademark or registered trademark of HDMI Licensing LLC in the United States and other countries.
Linux is the registered trademark of Linus Torvalds in the U.S. and other countries.
Mac is a trademark of Apple Inc.
Microsoft is a registered trademark of Microsoft Corp.
PostScript is a registered trademark of Adobe Systems Inc.
Python is a trademark of the Python Software Foundation.
Raspberry Pi is a trademark of the Raspberry Pi Foundation.
RISC OS is a trademark of Pace Micro Technology Plc which might now have passed to RISC OS Ltd but I was unable to find definitive documentation.
TeX is a trademark of the American Mathematical Society.
TrueType is a trademark of Apple Inc., registered in the United States and other countries. 
Unicode is a registered trademark of Unicode, Inc. in the United States and other countries.
Unix is a trademark of The Open Group.
VNC is a registered trademark of RealVNC Limited.
Windows is a registered trademark of Microsoft Corp.
Zoom is a trademark of Zoom Video Communications, Inc. 
Any other trademarks I mentioned without realising are trademarks of their respective holders.
