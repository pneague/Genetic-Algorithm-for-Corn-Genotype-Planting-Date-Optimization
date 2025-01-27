       program read_4_DSSAT
       implicit none
       character(len=10) :: exper
!       character(len=3) ::  var
       character(len=4) ::  clexper
       integer :: y2,  ys,ye,ms,me,ds,de,freq,ndx,ndxx,im
       integer :: n0, y2y
       parameter(ndxx=100*366)
       integer d4,d4yy,ie,ir4,ntx,it,itd,idat29
       real loclon,loclat, locelev, locav, locamp,locrefth, locwndht
       integer ilocelev
       real undeff
       parameter(undeff=-99.)
       real rad(ndxx), tmax(ndxx), tmin(ndxx), prec(ndxx), td(ndxx)
       real wind(ndxx), par(ndxx),evap(ndxx),rh(ndxx)
       integer idat(ndxx)
       integer djant,dfeb, dfebt,dmart,daprt,dmayt,djunt
       integer djult,daugt,dsept,doctt,dnovt,ddect
       character cly2*5
       namelist /namexp / exper,clexper,  ys,ye,ds,de,ms,me,ndx,&
     &                    freq,loclon,loclat,locelev
!
! Fundulea: tmean_anual(locav); ampl_lunara(locamp)
       locav=10.9
       locamp=26.
        n0=0
        rad(:)=undeff
        tmax(:)=undeff
        tmin(:)=undeff
        prec(:)=undeff
        td(:)=undeff
        wind(:)=undeff
        par(:)=undeff
        evap(:)=undeff
        rh(:)=undeff

         locelev=undeff
         locrefth=undeff
!         locav=undeff
!         locamp=undeff
         locwndht=undeff
!
 
        open(64,file='fa_srad',form='formatted') 
        open(65,file='fa_tmax',form='formatted') 
        open(66,file='fa_tmin',form='formatted') 
        open(67,file='fa_rain',form='formatted') 
!        open(68,file='fa_dewp',form='formatted') 

!        open(69,file='fa_wind',form='formatted') 
!        open(70,file='fa_par',form='formatted') 
!        open(71,file='fa_evap',form='formatted') 
!        open(72,file='fa_rhum',form='formatted') 
        
        open(80,file='faout.txt',form='formatted') 

        open(4,file='namelist_descr.txt')
        read(nml=namexp,unit=4)
!        print*,exper,clexper, ys,ye,ms,me,ds,de,&
!     & ndx,freq,loclon,loclat,locelev

!          print*, int(locelev)

! cate grupe de cate 4 ani avem intre ye si y2, inclusiv        
! cati ani cu 366 ! zile avem intre ys si ye, inclusiv   

!!! this is only for 1 year!
        if(ye.ne.ys) then
         print*,'ERROR: ys .ne. ys'
         stop
        endif


! for 1 year:    
        d4yy=0
! 
        d4=(ys)/4 
        ir4=ys-ys/4*4
!        y2=ys-ys/100*100 
        y2y=(ys-ys/100*100)
        y2=(ys-ys/100*100)*1000 
         if(ir4.eq.0) then
!BUG       d4=d4+1
         d4yy=1
         endif
!         print*,'ir4=', ir4


!         ntx=(ye-ys+1)*365*freq+d4*freq
         ntx=365*freq+d4yy*freq
         print*,'freq=',freq,'d4yy=',d4yy,'NTX=', ntx, "year=", ys
!         write(33,*) 'NTX=', ntx, "year=", ys
! aici difera, la IPSL
!         if(ndx.ne.ntx) then
!           print*,'ERROR in nr of days:  ndx=', ndx,' ntx=', ntx
!         stop
!         endif

!
         djant=31
         if(ir4.eq.0) then
!           dfeb=29
           dfeb=28
         else
           dfeb=28
         endif
         dfebt=djant+dfeb


!         do it=1,ndx
!          idat(it)=y2+it
!         enddo
         do it=1,ntx
          idat(it)=y2+it
          print*,'IDAT=', idat(it)
         enddo
         
!
         write(80,'(a31)') '*WEATHER DATA : Grid cell 00001'
         write(80,'(a22)') '                      '
!
!         write(80,'(a42)') '@ INSI      LAT      LONG      ELEV      TAV&
!         write(80,*) '@ INSI   LAT       LONG      ELEV      TAV&
         write(80,'(a5,6x,a3,8x,a4,4x,a4,2x,a3,5x,a3,3x,a5,3x,a5)') &
     &  '@INSI','LAT','LONG','ELEV','TAV','AMP'

         write(80,&
     &'(2x,a4,2x,f9.3,2x,f9.3,2x,i4,2x,f5.1,2x,f5.1,2x,f5.1,2x,f5.1)') &
     &             clexper, loclat, loclon, int(locelev), locav, locamp,&
     &   locrefth, locwndht

           write(33,'(f5.1)') locav
           print*,'locav=',locav

!         write(80,'(a57)') &
!         write(80,*) &
         write(80,'(a5,2x,a4,2x,a4,2x,a4,2x,a4)') &
     &  '@DATE', 'SRAD','TMAX', 'TMIN','RAIN'
!     &   'DEWP','WIND','PAR','EVAP','RHUM'
  
! 
         do it=1,ndx
          read(64,*) rad(it)
          read(65,*) tmax(it)
          read(66,*) tmin(it)
          read(67,*) prec(it)
!          print*,'PREC=', prec(it)
!          read(68,*) td(it)
!          read(68,*) td(it)
!          read(69,*) wind(it)
!          read(70,*) par(it)
!          read(71,*) evap(it)
!          read(72,*) rh(it)
         enddo



         itd=0
         do it=1,ndx
          itd=itd+1
!         write(80,'(I5,2x,f3.1,2x,f3.1,2x,f3.1,2x,f3.1,2x,f3.1,2x,&
         if(y2y.ge.10) then
!         write(80,'(I5,2x,f5.1,2x,f4.1,2x,f4.1,2x,f4.1)') &
!         write(80,'(I5,2x,f5.1,2x,f5.1,2x,f5.1,2x,f5.1)') &
         write(80,'(I5,1x,f5.1,1x,f5.1,1x,f5.1,1x,f5.1)') &
!,2x,f5.1,2x,&
!     &f5.1,2x,f5.1,2x,f5.1,2x,f5.1)')&
     &     idat(itd),rad(it),tmax(it),tmin(it),prec(it)
         else
!         write(80,'(I1,I4,2x,f5.1,2x,f4.1,2x,f4.1,2x,f4.1)') &
!         write(80,'(I1,I4,2x,f5.1,2x,f5.1,2x,f5.1,2x,f5.1)') &
          if(idat(itd).lt.10) then
         write(80,'(I1,I1,I1,I1,I1,1x,f5.1,1x,f5.1,1x,f5.1,1x,f5.1)') &
     &     n0,n0,n0,n0,idat(itd),rad(it),tmax(it),tmin(it),prec(it)
          elseif (idat(itd).lt.100) then
         write(80,'(I1,I1,I1,I2,1x,f5.1,1x,f5.1,1x,f5.1,1x,f5.1)') &
     &     n0,n0,n0,idat(itd),rad(it),tmax(it),tmin(it),prec(it) 
          elseif (idat(itd).lt.1000) then
         write(80,'(I1,I1,I3,1x,f5.1,1x,f5.1,1x,f5.1,1x,f5.1)') &
     &     n0,n0,idat(itd),rad(it),tmax(it),tmin(it),prec(it) 
          elseif (idat(itd).lt.10000) then
         write(80,'(I1,I4,1x,f5.1,1x,f5.1,1x,f5.1,1x,f5.1)') &
     &     n0,idat(itd),rad(it),tmax(it),tmin(it),prec(it) 

         endif 
         endif 
!,td(it),&
!     &     wind(it),par(it),evap(it),rh(it) 
!!!!!!!!!!!!!!!!!!!!!!!!
! bisect year day
!!!!!!!!!!!!!!!!!!!!!!!!
          print*, "year=", ys,"IT=", it, "ITD=", itd
         enddo
          print*, "year=", ys, "ITD_final=", itd,"IT_final=",it, "ir4=", ir4
          print*,'ilocelev=',int(locelev), 'locelev=', locelev
         stop
         end
