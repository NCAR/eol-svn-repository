
        subroutine weight(a,b,n,i)
        real A(n,4),b(2)
        integer i,n
c
c       A(,1)= lat of station
c	A(,2)= lon of station
c	A(,3)= weight for station
c	i = function selector
c
c	B()= reference station
c

c   	compute distance from ref station and put in A(,4)
c
        call deg2dist(A,b,n)

c	now select functions based on value of i

        do 10 i=1,n
c	this is i=1 => r^-1 case
        if (i .eq. 1)  a(j,3)=1/(a(j,4))
c	this is i=2 => r^-2 case
        if (i .eq. 2)  a(j,3)=1/(a(j,4)**2)
c	this is i=3 => r^-3 case
        if (i .eq. 3)  a(j,3)=1/(a(j,4)**3)
c	this is i=0 => r^-0 case
        if (i .eq. 0)  a(j,3)=1
10      continue
        return
        end


        subroutine expvalue(theta_o,theta_e,a,n)
        real theta_o(n),a(n,4),thata_e
        integer n
        theta_e=0.
        theta_eb=0.
        theta_et=0.
        do 10 i=1,n
        theta_et =theta_et + theta_o(i)*a(i,3)
        theta_eb =theta_eb + a(i,3)
10      continue
        theta_e=theta_et/theta_eb
        return
        end

        subroutine qc(theta_e,theta_o,sigma,flag)
        character*1 flag
        real theta_e, theta_o, sigma
        if (abs(theta_e -theta_o) .gt. 1.5*sigma) then
        flag='F'    !Failed test 
        goto 20
        endif
        if (abs(theta_e -theta_o) .lt. 0.5*sigma) then
        flag='L'    !okay passed low
        goto 20
        endif
        if (abs(theta_e -theta_o) .lt. 1.0*sigma) then
        flag='M'    !okay passed mod
        goto 20
        endif
        if (abs(theta_e -theta_o) .lt. 1.5*sigma)then
        flag='H'    !okay passed high
        goto 20
        endif
20      return
        end

