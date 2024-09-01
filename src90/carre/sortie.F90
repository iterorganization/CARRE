SUBROUTINE SORTIE(equ, grid, diag, par, numero)
  use KindDefinitions
  use CarreDiagnostics
  use carre_dimensions
  use carre_types
  use comlan
  use comrlx
  
  IMPLICIT NONE
  
  type(CarreEquilibrium), intent(in) :: equ
  type(CarreGrid), intent(in) :: grid
  type(CarreDiag), intent(in) :: diag
  type(CarreParameters), intent(in) :: par
  integer, intent(in) :: numero

!======================================================================

!..  Cette sous-routine imprime les resultats dans un fichier de sortie:
!  maille.sor

!  variables locales
      INTEGER i,j,isep,ireg

!======================================================================
!.. i,j: indices
!.. isep: indice de separatrice.
!.. ireg: indice de region.
!.. numero: 1= impression des parametres pouvant servir a un essai
!           ulterieur. 2= impression des donnees de la maille.
!======================================================================
!..Creation du nouveau fichier.

      IF (numero .EQ. 1) THEN

!..Impression des points X et O.

        if(sellan(1:8).eq.'francais') then
          write(10,111)equ%npx,(equ%ptx(i),equ%pty(i),equ%fctpx(i),i=1,equ%npx)
111       format(//' Nombre de points X:',i3/ & 
     &      t5,'x',t25,'y',t45,'psi'/(t4,1pe15.8,t24,1pe15.8, & 
     &      t44,1pe15.8))
         elseif(sellan(1:7).eq.'english') then
          write(10,211)equ%npx,(equ%ptx(i),equ%pty(i),equ%fctpx(i),i=1,equ%npx)
211       format(//' Number of X-points:',i3/ & 
     &      t5,'x',t25,'y',t45,'psi'/(t4,1pe15.8,t24,1pe15.8, & 
     &      t44,1pe15.8))
         endif
         write(10,112)equ%ptx(equ%npx+1),equ%pty(equ%npx+1),equ%fctpx(equ%npx+1)
112      format(/' Point O:'/(t4,1pe15.8,t24,1pe15.8,t44,1pe15.8))
         if(equ%npx.gt.1) then
            if(equ%racord) then
              if(sellan(1:8).eq.'francais') then
                write(10,113)' Les points X sont raccordes.'
113             format(a//)
              elseif(sellan(1:7).eq.'english') then
                write(10,113)' X-points are connected.'
              endif
            else
              if(sellan(1:8).eq.'francais') then
                write(10,113)' Les points X ne sont pas raccordes.'
              elseif(sellan(1:7).eq.'english') then
                write(10,113)' X-points are not connected.'
              endif
            endif
         endif
!
!..Ecriture des parametres qui pouront servir au prochain essai.
         if(sellan(1:8).eq.'francais') then
           write(10,*)'$parametres'
         elseif(sellan(1:7).eq.'english') then
           write(10,*)'$parameters'
         endif

        WRITE(10,'(A,I2)') 'carreMode =',par%carreMode
        WRITE(10,'(A,I2)') 'gridExtensionMode =',par%gridExtensionMode
        WRITE(10,'(A,I2)') 'equExtensionMode =',par%equExtensionMode
        WRITE(10,'(A,I2)') 'nVirtualStructs =',par%nVirtualStructs

         WRITE(10,100)par%repart
  100    FORMAT('repart =',I2)

         WRITE(10,1015)equ%distxo
 1015    FORMAT('distxo =',F11.8)

         WRITE(10,101)par%pntrat
  101    FORMAT('pntrat =',F11.8)

         DO 10 isep=1, equ%nsep
            WRITE(10,102)isep,grid%nptseg(isep),isep,par%deltp1(isep),isep, & 
     &                par%deltpn(isep)
  102       FORMAT('nptseg(',I1,') =',I3/'deltp1(',I1,') =',F10.7/ & 
     &          'deltpn(',I1,') =',F10.7)
   10    CONTINUE

         DO 20 ireg=1, grid%nreg
            WRITE(10,103)ireg,grid%nr(ireg),ireg,par%deltr1(ireg),ireg, & 
     &                par%deltrn(ireg)
  103       FORMAT('npr(',I1,') =',I3/'deltr1(',I1,') =',F10.7/ & 
     &          'deltrn(',I1,') =',F10.7)
   20    CONTINUE

         DO 25 i=1, 2
            WRITE(10,104)i,par%tgarde(i)
  104       FORMAT('tgarde(',I1,') =',F8.5)
   25    CONTINUE

         IF (equ%nsep .GT. 3) THEN
            DO 26 i=3, 4
               WRITE(10,104)i,par%tgarde(i)
   26       CONTINUE
         ENDIF
         write(10,115)'nrelax=',nrelax,'relax=',relax,'pasmin=',pasmin, & 
     &     'rlcept=',rlcept
 115     format(a,i5/(a,1pe11.4))

        WRITE(10,'(a,1pe11.4)') 'targetResolution =',par%targetRes

         if(sellan(1:8).eq.'francais') then
           WRITE(10,105)
  105      FORMAT('$fin')
         elseif(sellan(1:7).eq.'english') then
           write(10,205)
  205      format('$end')
         endif

!..Ecriture des points de mailles.

      ELSE IF (numero .EQ. 2) THEN
         write(10,114)
114      format(t2,'x(m)',t18,'y(m)',t34,'psi(SI)',t50,'dpsi/dx',t66, & 
     &     'dpsi/dy')
         write(10,*)'$maille'
         WRITE(10,106)grid%nreg
  106    FORMAT('''nreg=''',I5)

         DO ireg=1, grid%nreg
             ! write one region
             WRITE(10,107)ireg
107          FORMAT('''region:''',I5)
             WRITE(10,108)grid%np1(ireg),grid%nr(ireg)
108          FORMAT('''nppol=''',I5,'  ''nprad=''',I5)

             ! write grid point data
             DO j=1, grid%nr(ireg)
                 do i=1,grid%np1(ireg)
                     WRITE(10,109)grid%xmail(i,j,ireg),grid%ymail(i,j,ireg),&
                          & grid%psim(i,j,ireg), & 
                          & grid%psidxm(i,j,ireg),grid%psidym(i,j,ireg)
109                  FORMAT(1p5E16.8)
                 end do
             end do

             ! write grid cell data
             WRITE(10,"(a)") "'cellflags'"
             DO j=1, grid%nr(ireg) - 1
                 do i=1,grid%np1(ireg) - 1
                     WRITE(10,"(1i4,4i4)") grid%cellflag(i,j,ireg), &
                                         & grid%cellFaceIStruct(:,i,j,ireg)
                 end do
             end do

             ! write info on degenerate faces
             WRITE(10,"(a)") "'degenerate faces'"
             DO j=1, grid%nr(ireg) - 1
                 do i=1,grid%np1(ireg) - 1
                     WRITE(10,"(4i4)") grid%cellFaceDegen(:,i,j,ireg)
                 end do
             end do


         end DO

         if(sellan(1:8).eq.'francais') then
           WRITE(10,110)
  110      FORMAT('$fin')
         elseif(sellan(1:7).eq.'english') then
           write(10,210)
  210      format('$end')
         endif

         write(10,*)
         write(10,*) 'a= ',diag%a(1)
         write(10,124)
  124    format(t2,'ir',t8,'gdpsi',t24,'racpsi',t40,'gdr', & 
     &          t56,'r',t72,'rho',t88,'ra')
         if(sellan(1:8).eq.'francais') then
           write(10,*) '$Coordonnees'
         elseif(sellan(1:7).eq.'english') then
           write(10,*) '$Flux surface labels'
         endif

         DO ireg=1, grid%nreg
            WRITE(10,107) ireg
            DO i=1, grid%nr(ireg)
                    WRITE(10,129) i,diag%gdpsi(i,ireg),diag%racpsi(i,ireg), & 
                         & diag%gdr(i,ireg),diag%r(i,ireg),&
                         & diag%rho(i,ireg),diag%ra(i,ireg)
           enddo
         enddo
  129    FORMAT(i2,1p6E16.8)

         if(sellan(1:8).eq.'francais') then
           WRITE(10,110)
         elseif(sellan(1:7).eq.'english') then
           write(10,210)
         endif

         write(10,*)
         write(10,164)
  164    format(t2,'ir',t8,'somort',t24,'somortp',t40,'somortpur', & 
     &          t56,'somortpurp')
         if(sellan(1:8).eq.'francais') then
           write(10,*) '$Qualite des surfaces de maille'
         elseif(sellan(1:7).eq.'english') then
           write(10,*) '$Flux surface quality indices'
         endif

         DO ireg=1, grid%nreg
            WRITE(10,107) ireg
            DO i=1, grid%nr(ireg)
                    WRITE(10,139) i,diag%somort(i,ireg),diag%somortp(i,ireg), & 
                         & diag%somortpur(i,ireg),diag%somortpurp(i,ireg)
            enddo
         enddo
  139    FORMAT(i2,1p4E16.8)

         write(10,*)
         write(10,134)
  134    format(t2,'ir',t8,'sompropo',t24,'sompropop',t40,'somvarr', & 
     &          t56,'somvarrp')
         DO ireg=1, grid%nreg
            WRITE(10,107) ireg
            DO i=1, grid%nr(ireg)
                    WRITE(10,139) i,diag%sompropo(i,ireg),diag%sompropop(i,ireg), & 
                         & diag%somvarr(i,ireg),diag%somvarrp(i,ireg)
            enddo
         enddo

         write(10,*)
         write(10,144)
  144    format(t2,'ir',t40,'segt')
         DO ireg=1, grid%nreg
            WRITE(10,107) ireg
            DO i=1, grid%nr(ireg)
                    WRITE(10,149) i, & 
                         & diag%segt(i,ireg)
            enddo
         enddo
  149    FORMAT(i2,1p3E16.8)

         if(sellan(1:8).eq.'francais') then
           WRITE(10,110)
         elseif(sellan(1:7).eq.'english') then
           write(10,210)
         endif

         write(10,*)
         write(10,154)
  154    format(t2,'ireg',t8,'gdsomortp',t24,'gdsomortpurp', & 
     &          t40,'gdsompropop',t56,'gdsomvarrp')
         if(sellan(1:8).eq.'francais') then
           write(10,*) '$Totaux par region'
         elseif(sellan(1:7).eq.'english') then
           write(10,*) '$Totals per region'
         endif

         DO ireg=1, grid%nreg
                 WRITE(10,169) ireg,diag%gdsomortp(ireg),diag%gdsomortpurp(ireg), & 
                      & diag%gdsompropop(ireg),diag%gdsomvarrp(ireg)
         enddo
  169    FORMAT(i4,1p5E16.8)

         if(sellan(1:8).eq.'francais') then
           WRITE(10,110)
         elseif(sellan(1:7).eq.'english') then
           write(10,210)
         endif
      ENDIF

      RETURN
      END
