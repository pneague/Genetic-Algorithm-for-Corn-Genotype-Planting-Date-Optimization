!
          program write_cul_file
          implicit none
!          character c360*80
          character*80 c360
          character*1 c1
          character*6 VARNO,VARTY,ECONO, cvar
          character*16 VRNAME, ctype 
          real p1, p2, p5, g2, g3, phint
          real p1n, p2n, p5n, g2n, g3n, phintn
          integer i,ip, il,errnum,isec,isecx, modline
          parameter(isecx=100)
          integer ilstart(isecx)
          integer nparam, npcult
          parameter(nparam=10)
          real p(nparam), zfac(nparam), pn(nparam)

          namelist /namexp / p, cvar,ctype, npcult
          read(4,namexp)
          print*,'procents,ctype=', p, cvar, ctype,npcult

!          proc=0.1
          do ip=1,nparam
           zfac(ip)=1.+p(ip)/100.
          enddo
! 
!           ctype='2600-2650 GDD   '
          open(66,file='input.txt', STATUS = 'OLD',IOSTAT=errnum)
          open(67,file='output.txt',form='formatted')

           isec=0
           modline=0 
           do i=1,isecx
            ilstart(i)=0
           enddo 

          if (errnum .NE. 0) then
           print*,'NO input file input.txt '
           stop
          endif
          il=0
  200     continue   
          read(66,'(a80)',err=70, end=70) c360
          il=il+1
          if (c360(1:1) .eq. ' ' .or. c360(1:1) .eq. '*' .or. &
     &       c360(1:1) .eq. '!'.or. c360(1:1) .eq. '@') then
            write(67,'(a80)') (c360)
            if(c360(1:1) .eq. '@') then
              isec=isec+1
              ilstart(isec)=il
            endif
            GO TO 200
          else

!            read(c360,'(A6,1X,A16,5X,A1,1X,A6,1x,F5.1,1x,F5.3, &
!     &          2(1x,F5.1),2(1x,F5.2))',IOSTAT=ERRNUM) &
!     &            VARTY,VRNAME,c1,ECONO,(p(ip),ip=1,npcult)
            read(c360,'(A6,1X,A16,5X,A1,1X,A6,1x,F5.1,1x,F5.3, &
     &           F6.1, F6.1 ,1X,F5.2,1x,F5.2)',IOSTAT=ERRNUM) &
     &            VARTY,VRNAME,c1,ECONO,(p(ip),ip=1,npcult)


!           write(33,*)'citit:', VARTY,',',VRNAME,',',ECONO,',',&
!     &       (p(ip),ip=1,npcult)
!
!           write(34,*) 'vrname=', vrname, 'ctype=', ctype

!           if(VRNAME.eq.ctype) then
           if( (VRNAME.eq.ctype).and.(VARTY.eq.cvar)) then
           do ip=1,npcult
            modline=il
!            pn(ip)=p(ip)*zfac(ip)
            p(ip)=p(ip)*zfac(ip)
           enddo
           endif
! test file
!           write(68,'(A6,1X,A16,5X,A1,1X,A6,1x,F5.1,1x,F5.3, &
!     &          2(1x,F5.1),2(1x,F5.2))',IOSTAT=ERRNUM) &
!     &            VARTY,VRNAME,c1,ECONO,(pn(ip),ip=1,npcult)
! old     &          2(1x,F5.1),2(1x,F5.2))',IOSTAT=ERRNUM) &

!           write(67,'(A6,1X,A16,5X,A1,1X,A6,1x,F5.1,1x,F5.3, &
!      &          1x, F6.1,1x, F6.1 ,1X,F5.2,1x,F5.2)',IOSTAT=ERRNUM) &
!     &            VARTY,VRNAME,c1,ECONO,(pn(ip),ip=1,npcult)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!  write data  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
             if((p(3).lt.1000.).and.(p(4).lt.1000.) ) then
           write(67,'(A6,1X,A16,5X,A1,1X,A6,1x,F5.1,1x,F5.3, &
     &      1x, F5.1,1x, F5.1 ,1X,F5.2,1x,F5.2)',IOSTAT=ERRNUM) &
     &            VARTY,VRNAME,c1,ECONO,(p(ip),ip=1,npcult)
             elseif((p(3).lt.1000.).and.(p(4).ge.1000.) ) then
           write(68,*) 'case: p(4)>1000', 'line=', modline
           write(67,'(A6,1X,A16,5X,A1,1X,A6,1x,F5.1,1x,F5.3, &
     &      1x, F5.1, F6.0 ,1X,F5.2,1x,F5.2)',IOSTAT=ERRNUM) &
     &            VARTY,VRNAME,c1,ECONO,(p(ip),ip=1,npcult)
             elseif((p(3).ge.1000.).and.(p(4).lt.1000.) ) then
           write(67,'(A6,1X,A16,5X,A1,1X,A6,1x,F5.1,1x,F5.3, &
     &      F6.0,1x, F5.1 ,1X,F5.2,1x,F5.2)',IOSTAT=ERRNUM) &
     &            VARTY,VRNAME,c1,ECONO,(p(ip),ip=1,npcult)
           write(68,*) 'case: p(3)>1000', 'line=', modline
             elseif((p(3).ge.1000.).and.(p(4).ge.1000.) ) then
           write(67,'(A6,1X,A16,5X,A1,1X,A6,1x,F5.1,1x,F5.3, &
     &      F6.0, F6.0 ,1X,F5.2,1x,F5.2)',IOSTAT=ERRNUM) &
     &            VARTY,VRNAME,c1,ECONO,(p(ip),ip=1,npcult)
           write(68,*) 'case: p(3,4)>1000', 'line=', modline
              else
               print*,'This case was not treated ! REVIEW !!'
             stop
           endif
!

!           else
!           write(67,'(A6,1X,A16,5X,A1,1X,A6,1x,F5.1,1x,F5.3, &
!     &      1x, F5.1,1x, F5.1 ,1X,F5.2,1x,F5.2)',IOSTAT=ERRNUM) &
!     &            VARTY,VRNAME,c1,ECONO,(p(ip),ip=1,npcult)

!

! test file
!           write(68,'(A6,1X,A16,5X,A1,1X,A6,1x,F5.1,1x,F5.3, &
!     &      F6.1,1x, F5.1 ,1X,F5.2,1x,F5.2)',IOSTAT=ERRNUM) &
!     &            VARTY,VRNAME,c1,ECONO,(p(ip),ip=1,npcult)
           endif
!
!          endif
          goto 200
  70       continue

           print*,'Nrsec=', isec, 'modline=', modline
           stop
           end 
           

