# Makefile

#Commands
RCMD = R --vanilla --args 

#Directories
RDIR = r_scripts
RDATADIR = r_data
IMGDIR = r_img
RIMGDIR = ${IMGDIR}

RSRC = $(wildcard $(RDIR)/*.R)

#Windows
ifeq ($(OS),Windows_NT)
	SHELL = C:/Windows/System32/cmd.exe
endif

rcmd : $(RSRC)
	for file in $(RSRC); do $(RCMD) $(RDATADIR) $(RIMGDIR) < $${file} ; done
	
clean :
	rm -rf ${TODO} ${RIMGDIR}/*
