RestoreSession("plot-sn.session",0)
RecenterView()

# Set the save window attributes.
s = SaveWindowAttributes()
s.fileName = "grid.ps"
s.format = s.POSTSCRIPT
s.family = 0 # no counting in filename
SetSaveWindowAttributes(s)
name = SaveWindow()
print "name = %s" % name

quit()

