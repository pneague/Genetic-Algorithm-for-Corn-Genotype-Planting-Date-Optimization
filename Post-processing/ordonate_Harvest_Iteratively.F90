             program read_loop_time_ch
             implicit none
              character*5 c1,c2,c4
              character*6 c3
              character*4 model,scen
              integer i,j,k
              integer npar,niterx
              integer ntrat,it,ntfin,var4ord,irg
              parameter(ntrat=12)
              parameter(npar=6,niterx=100000)
! ordonate after HARWT (variable 4)
              parameter(var4ord=4)
              integer year,unit0,unit,unitt0,unitt,nstepsx
               parameter(nstepsx=10)
              integer nsteps(npar), vmin(npar), vmax(npar), vfac(npar)
              integer valpar(npar,nstepsx)
              integer vval1(nstepsx), vval2(nstepsx),vval3(nstepsx), &
     &            vval4(nstepsx), vval5(nstepsx),vval6(nstepsx)
              character*80 cfld
              character*80 cunits
              character*4 cyy, cline1

               integer ntreatx,nvarx,ivar
               parameter(nvarx=12,ntreatx=12)
!               real vout(ntreatx,nvarx,nyxx)
               real vout(niterx,ntreatx,nvarx)
               integer ntr,nexp,mrec,ibyte
               integer nt, isk, nskip
               parameter(nskip=0)
               parameter(ibyte=4)
               character*2 ccrop
               logical lrange

               integer iloop,ibase,par
               real ss,mm
               real a(niterx,ntreatx)
               integer ic0(npar,niterx),ic(npar,niterx)
               integer ict(npar,niterx,ntreatx),rg(npar,niterx,ntreatx)
               integer step(npar,niterx),cc
 

               namelist / namkode/ nsteps, &
     &               vval1,vval2,vval3,vval4,vval5,vval6

                open(4,file='namel_kode')
                   read(4,namkode)   
                close(4)
                cyy='1986'
                unit0=30
                unitt0=50
           
                
               open(66,file='Info_all_IDEOTYPE_miss',&
     &           form='formatted')

               open(71,file='o1_var1_Flo.dat',&
     &  access='direct',form='unformatted',recl=ibyte*(ntreatx+npar))
               open(72,file='o1_var2_Mat.dat',&
     &  access='direct',form='unformatted',recl=ibyte*(ntreatx+npar))
               open(73,file='o1_var3_Topwt.dat',&
     &  access='direct',form='unformatted',recl=ibyte*(ntreatx+npar))
               open(74,file='o1_var4_Harwt.dat',&
     &  access='direct',form='unformatted',recl=ibyte*(ntreatx+npar))
               open(75,file='o1_var5_Prec.dat',&
     &  access='direct',form='unformatted',recl=ibyte*(ntreatx+npar))
!               open(76,file='o1_var6_TIRR.dat',&
!     &  access='direct',form='unformatted',recl=ibyte*(ntreatx+npar))
               open(77,file='o1_var7_CET.dat',&
     &  access='direct',form='unformatted',recl=ibyte*(ntreatx+npar))
               open(78,file='o1_var8_PESW.dat',&
     &  access='direct',form='unformatted',recl=ibyte*(ntreatx+npar))
               open(79,file='o1_var9_TNUP.dat',&
     &  access='direct',form='unformatted',recl=ibyte*(ntreatx+npar))
               open(80,file='o1_var10_TNLF.dat',&
     &  access='direct',form='unformatted',recl=ibyte*(ntreatx+npar))
               open(81,file='o1_var11_TSON.dat',&
     &  access='direct',form='unformatted',recl=ibyte*(ntreatx+npar))

               open(84,file='o1_var4_Harwt_ORD.dat',access='direct',&
     & form='unformatted',recl=ibyte*(ntreatx+npar*ntreatx))
             open(94,file='o1_var4_Harwt_ORD+code.dat',access='direct',&
     & form='unformatted',recl=ibyte*(ntreatx+npar*ntreatx))

!       data nsteps /8,    9,    8,   5,  3,  3/
!       data vmin /100,  20,  550, 440,  5, 30/
!       data vmax /450, 220, 1075, 940, 15, 50/
!       data vfac /1,   100,    1,   1,  1,  1/

!       data (valpar(1,i),i=1,nstepsx) / 100,150,200,250,300,350, 400,  450,0,0/
!       data (valpar(2,i),i=1,nstepsx) /  20, 45, 70, 95,120,145, 170,  195,220,0/
!       data (valpar(3,i),i=1,nstepsx) / 550,625,700,775,850,925,1000,1075,0,0/
!       data (valpar(4,i),i=1,nstepsx) / 440,565,690,815,940,0,0,0,0,0/
!       data (valpar(5,i),i=1,nstepsx) /   5, 10, 15,0,0,0,0,0,0,0/
!       data (valpar(6,i),i=1,nstepsx) /  30, 40, 50,0,0,0,0,0,0,0/
                  do i=1,nstepsx
                   valpar(1,i)=vval1(i)
                   valpar(2,i)=vval2(i)
                   valpar(3,i)=vval3(i)
                   valpar(4,i)=vval4(i)
                   valpar(5,i)=vval5(i)
                   valpar(6,i)=vval6(i)
                  enddo

            print*,'valpar(1,i)=', (valpar(1,i),i=1,nstepsx)
            print*,'valpar(2,i)=', (valpar(2,i),i=1,nstepsx)
            print*,'valpar(3,i)=', (valpar(3,i),i=1,nstepsx)
            print*,'valpar(4,i)=', (valpar(4,i),i=1,nstepsx)
            print*,'valpar(5,i)=', (valpar(5,i),i=1,nstepsx)
            print*,'valpar(6,i)=', (valpar(6,i),i=1,nstepsx)

          

            do i=1,niterx
            read(66,*,end=100) cline1
            read(66,*,end=100) c1,year,c2,scen,c3,model,c4,&
     &        (ic0(par,i),par=1,npar)
!            print*,'cline1=', cline1
!            print*,'cline2=', c1,year,c2,scen,c3,model,c4,&
!     &        (ic0(par,i),par=1,npar)
                 read(66,*,END=100) cfld
!                 print*,'cfld=', cfld
                 read(66,*,END=100) cunits
!                 print*,'cunits=', cunits
              do j=1,ntreatx
                 read(66,*,end=100) ntr,ccrop,nexp,&
     &               (vout(i,j,ivar),ivar=1,nvarx)
                   if(vout(i,j,1).eq.-99) then
                    do ivar=1,nvarx
                      vout(i,j,ivar)=-99.
                    enddo
                   endif 
!                 print*,'Niterx=', i,'vals=',(ic0(par,i),par=1,npar)
!                 print*,' Nrtrat=',j,'NTR_read=',ntr
              enddo
            enddo
100         continue
              ntfin=i-1
              print*,'NTFIN=', ntfin


              mrec=0
              do i=1,ntfin
               mrec=mrec+1 
               write(71,rec=mrec) &
     &      (vout(i,j,1),j=1,ntreatx),(float(ic0(k,i)),k=1,npar)
               write(72,rec=mrec) &
     &      (vout(i,j,2),j=1,ntreatx),(float(ic0(k,i)),k=1,npar)
               write(73,rec=mrec) &
     &      (vout(i,j,3),j=1,ntreatx),(float(ic0(k,i)),k=1,npar)
               write(74,rec=mrec) &
     &      (vout(i,j,4),j=1,ntreatx),(float(ic0(k,i)),k=1,npar)
               write(75,rec=mrec) &
     &      (vout(i,j,5),j=1,ntreatx),(float(ic0(k,i)),k=1,npar)
               write(77,rec=mrec) &
     &      (vout(i,j,7),j=1,ntreatx),(float(ic0(k,i)),k=1,npar)
               write(78,rec=mrec) &
     &      (vout(i,j,8),j=1,ntreatx),(float(ic0(k,i)),k=1,npar)
               write(79,rec=mrec) &
     &      (vout(i,j,9),j=1,ntreatx),(float(ic0(k,i)),k=1,npar)
               write(80,rec=mrec) &
     &      (vout(i,j,10),j=1,ntreatx),(float(ic0(k,i)),k=1,npar)
               write(81,rec=mrec) &
     &      (vout(i,j,11),j=1,ntreatx),(float(ic0(k,i)),k=1,npar)
              enddo


! ordonarea per treatment 
         do j=1,ntreatx
!! re-initialisation:
         do i=1,ntfin
            a(i,j)=vout(i,j,var4ord)
            do par=1,npar
              ic(par,i)=ic0(par,i)
            enddo
         enddo
!              do i=1,ntfin
!               write(30,*) a(i,2), (ic(k,i),k=1,npar)
!              enddo

            ibase=1
  150       continue
            mm=a(ibase,j)
            iloop=ibase
!
            do i=iloop+1,ntfin
             if(mm.le.a(i,j)) then
              ss=a(ibase,j)
              a(ibase,j)=a(i,j)
              a(i,j)=ss
              iloop=i
               do par=1,npar
                 cc=ic(par,ibase)
                 ic(par,ibase)=ic(par,i)
                 ic(par,i)=cc
               enddo
              mm=a(ibase,j)
             else
               goto 200
             endif
 200         continue
             enddo
           ibase=ibase+1
           if(ibase.lt.ntfin) then
             goto 150
           endif


!       data (valpar(6,i),i=1,nstepsx) /  30, 40, 50,0,0,0,0,0,0,0/
!!!!!!!!!!!!!! save ordered data for this treatment:
             do par=1,npar
             do i=1,ntfin
! save ic for this treatment
               ict(par,i,j)=ic(par,i)
               lrange=.false.
                do irg=1,nsteps(par)
!                if(ic(par,i).eq.valpar(par,irg)) rg(par,i,j)=irg*1.e+par 
                  if(ic(par,i).eq.valpar(par,irg)) rg(par,i,j)=irg
                  lrange=.true.
                enddo
                if(lrange.eqv..false.) then
                 print*,'ERROR in finding range!'
                 stop
                endif
             enddo 
             enddo 

!!!!!!!!!!!!!! (indicatorul ic nu depinde de tratament, e acealsi pt
!!!!!!!!!!!!!!!!!!!!!!cele 12 tratamente )
              unit=unit0+j
              unitt=unitt0+j
              write(unit,*) 'TRATAMENT:', j
              do i=1,ntfin 
               write(unit,*) a(i,j),(float(ict(par,i,j)),par=1,npar)
               write(unitt,*) a(i,j),(float(rg(par,i,j)),par=1,npar)
              enddo
              do i=1,ntfin
               do par=1,npar
               ict(par,i,j)=ic(par,i)
               enddo
              enddo

! loop over treatment j
              enddo
!!!!!!!!!!!!

              mrec=0
              do i=1,ntfin
               mrec=mrec+1 
               write(84,rec=mrec) (a(i,j),j=1,ntreatx),&
     &          ((float(ict(k,i,j)),k=1,npar),j=1,ntreatx)
               write(94,rec=mrec) (a(i,j),j=1,ntreatx),&
     &          ((float(rg(k,i,j)),k=1,npar),j=1,ntreatx)
              enddo

              stop
              end

!cc 
!cc 
