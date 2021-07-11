white_maps_to = (0,0,0)
black_maps_to = (255,255,0)

rgb = []
for step in range(8): rgb.append((
  black_maps_to[0]*step/8 + white_maps_to[0]*(8-step)/8,
  black_maps_to[1]*step/8 + white_maps_to[1]*(8-step)/8,
  black_maps_to[2]*step/8 + white_maps_to[2]*(8-step)/8))

rgb += [
  (0,0,128), (255,255,0), (0,196,0),
  (196,255,255), # text cursor colour
  (196,196,64), (0,128,0), (255,128,0), (0,196,196),
  (0,0,0), # colour of off-desktop area
  (255,196,0), # border colour of mouse pointer
  (255,0,0), # inside colour of mouse pointer
  (0,0,128) # ???
]

o=open("inverted","w")
for col in range(len(rgb)): o.write(chr(0x13)+chr(col)+chr(0x10)+chr(rgb[col][0])+chr(rgb[col][1])+chr(rgb[col][2]))
o.close()
import os
os.system("SetType inverted Palette")
print "Now move 'inverted' into the right directory and rename to _inverted"
