      program fcrr
c
c  version : 05.02.99 13:41
c
c======================================================================
c*** Converts the dg data into the input files for Carre
c======================================================================
c      implicit none
#include <CARRETYPES.F>
#include <FCRCOM.F>
#include <impcon.inc>
      logical ex
      character*14 chtrg,chstr,chequ,chaux,chdgo,
     ,             chcfld,chcequ,chcstr,chccrr,chcslp
      data chtrg ,  chstr ,  chequ ,  chaux , chdgo  /
     /   'dg.trg','dg.str','dg.equ','dg.aux','dg.dgo'/,
     ,     chcfld    ,  chcequ   ,     chcstr    ,  chccrr   /
     /   'btor.dat'  ,'rzpsi.dat','structure.dat','carre.dat'/,
     ,      chcslp   /
     /   'selptx.inf'/
      external fcraxn,fcrtrn,fcrdgi
c======================================================================
c
      inquire(file=chtrg,exist=ex)
      if(.not.ex) then
        write(*,*) 'fcrr: the file ',chtrg,' must be present!'
      end if
      inquire(file=chstr,exist=ex)
      if(.not.ex) then
        write(*,*) 'fcrr: the file ',chstr,' must be present!'
      end if
      inquire(file=chequ,exist=ex)
      if(.not.ex) then
        write(*,*) 'fcrr: the file ',chequ,' must be present!'
      end if
c
      immed=.false.
      keylen=8
      mimp=1
      lena=8
c
c*** Read the data
c
      open(1,file=chtrg)
      call fcrchktp(1)
      call import(fcrtrn)
      close(1)
      open(1,file=chstr)
      call fcrstri(1)
      close(1)
      open(1,file=chdgo)
      call import(fcrdgi)
      close(1)
      open(1,file=chequ)
      call fcrfldi(1)
      close(1)
      inquire(file=chaux,exist=ex)
      if(ex) then
        open(1,file=chaux)
        call import(fcraxn)
        close(1)
      end if
c
c*** ... make the necessary transformations
c
      call fcrprp
c
c*** ... and produce the files to be read by carre
c
      call fcrequo(chcequ)
      call fcrstro(chcstr)
      call fcrcrro(chccrr)
      call fcrfldo(chcfld)
      call fcrslpo(chcslp)
c======================================================================
      end
