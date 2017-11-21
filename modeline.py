# Modeline for old PC connected to Philips 192EL2 monitor

freq_khz = 47.71 # from the monitor spec

hres = 680       # from the 1366 spec, /2 and round to mult of 8 for VGA
rhs_margin = 40  # found by trial and error
sync_len = 72    # xtimings standard
lhs_margin = 104 # found by trial and error

vres = 384       # from the 768 spec, /2
btm_margin = 8   # xtimings standard
vsync_len = 4    # xtimings standard
top_margin = 8   # xtimings standard

total_hpx = hres+rhs_margin+sync_len+lhs_margin
clock_mhz = freq_khz*1000*total_hpx/1000000

print 'Modeline "%dx%d" %.2f %d %d %d %d %d %d %d %d doublescan' % (hres,vres,
clock_mhz,
hres, hres+rhs_margin, hres+rhs_margin+sync_len, total_hpx,
vres, vres+btm_margin, vres+btm_margin+vsync_len, vres+btm_margin+vsync_len+top_margin)
