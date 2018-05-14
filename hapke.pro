

FUNCTION HAPKE, AMU,AMUNOT,GIR
  ;     gibmerc - program for gibbous phases
  ;     program to solve for rough surface reflectance
  ;     amu and amunot are the cosines of the emission and incidence
  ;     angles, respectively. gix is the phase in degrees.
  ;  The angles  in the program are in radians
  ;     refl is the output reflectance
  ;     FOR NEGATIVE PHASE ANGLES THE RESULT IS THE SAME
  ;     AS FOR POSITIVE PHASE ANGLES WITH THE SIGN OF THE
  ;     LONGITUDE REVERSED
  ;
  ; NEW VERSION 3-1-89 TO MATCH HAPKE '86 AND HELFENSTEIN PARAMS
  ; modified 11/13/2008 to match Mallama et al., 2002 by RMK
  ;
  ;      implicit double precision(a-h,o-z)
  ;      double precision munpn,mupnot,munotp,mupr,ff
  ; common /stuff/ corr
  ;      data thetab,ht/0.349d0,0.11d0/
  ; Mallama et al Icarus 2002
  ;      data thetab,ht/0.279d0,0.065d0/
  ; Warell, 2004
  ;      data pisurf,thetab,ht/0.15d0,0.175d0,0.08d0/
  ; Domingue et al., 2015, 558 nm values
  ;   test for Hapke paper w=0.25, thetab=0.436
  amu=double(amu)
  amunot=double(amunot)
  ;       pisurf=0.19738d0
  pisurf=0.25d0
  ht=0.08d0
  thetab=0.436d0
  api=!pi
  g=abs(GIR)
  test1=amu*amunot
  ; print, 'test1=', test1
  if(test1 le 3.d-4) then begin
    reflh=0.d0
    ;      print, 'cos(phi) lt 3.E-4'
    return, reflh
  endif
  ae=acos(amu)
  ai=acos(amunot)
  if(amunot eq 1.d0) then begin
    ;      cotai=1.d10
    cotai=72.0d0
  endif else begin
    cotai=1.d0/tan(ai)
  endelse
  if(amu eq 1.d0) then begin
    ;      cotae=1.d10
    cotae=72.0d0
  endif else begin
    cotae=1.d0/tan(ae)
  endelse
  ;     compute the B function
  ag=g

  if(ag gt !pi/2.d0) then begin
    bnot=0.d0
  endif else begin
    if(ht eq 0.d0) then  bnot=0.d0 else bnot=2.9d0
  endelse
  bg=bnot/(1.D0+TAN(AG/2.D0)/HT)
  ;      COMPUTE THE ANGLES
  tanthb=tan(thetab)
  cotthb=1.d0/tanthb
  t1=1.d0/sqrt(1.d0+api*tanthb^2)
  t2=sin(ai)*tanthb
  if(amunot eq 1.d0) then begin
    t3=0.d0
    t4=2.d0
  endif else begin
    t3=exp(-(cotthb^2)/((tan(ai)^2)*!pi))
    t4=2.d0-exp(-2.d0*cotthb/(tan(ai)*!pi))
  endelse
  ; print,'t1,t2,t2,t4=, t1,t2,t3,t4
  MUNPN=t1*(amunot+(t2*t3)/t4)
  munpn=abs(munpn)
  t22=sin(ae)*tanthb
  if(amu eq 1.d0) then begin
    t32=0.d0
    t42=2.d0
  endif else begin
    t32=exp(-(cotthb^2)/((tan(ae)^2)*!pi))
    t42=2.d0-exp(-2.d0*cotthb/(tan(ae)*!pi))
  endelse
  MUPNOT=t1*(amu+(t22*t32)/t42)
  mupnot=abs(mupnot)
  if(ai eq 0) or (ae eq 0) then begin
    psi=0.d0
  endif else begin
    cpsi=(cos(g)-amu*amunot)/(sin(ai)*sin(ae))
  endelse
  if(abs(cpsi) gt 1.d0) then begin
    psi=0.d0
  endif else begin
    psi=acos(cpsi)
  endelse
  if(AI GE AE) then begin
    af1=exp(-(cotthb^2)*(cotai^2)/!pi)
    af2=(sin(psi/2.d0)^2)*exp(-(cotthb^2)*cotae^2/!pi)
    af3=2.d0-exp(-2.d0*cotthb*cotai/!pi)
    af4=psi*exp(-2.d0*cotthb*cotae/!pi)/!pi
    ; print, 'af1,af2,af3,af4=', af1,af2,af3,af4
    af=(af1-af2)/(af3-af4)
    munotp=t1*(amunot+sin(ai)*tanthb*af)
    af1=cos(psi)*af1
    af=(af1-af2)/(af3-af4)
    mupr=t1*(amu+sin(ae)*tanthb*af)
  ENDIF ELSE begin
    af1=exp(-(cotthb^2)*(cotae^2)/!pi)
    af2=(sin(psi/2.d0)^2)*exp(-(cotthb^2)*cotai^2/!pi)
    af3=2.d0-exp(-2.d0*cotthb*cotae/!pi)
    af4=psi*exp(-2.d0*cotthb*cotai/!pi)/!pi
    ; print, 'af1,af2,af3,af4=', af1,af2,af3,af4
    af=(af1-af2)/(af3-af4)
    MUPR=t1*(amu+sin(ae)*tanthb*af)
    af1=cos(psi)*af1
    af=(af1-af2)/(af3-af4)
    MUNOTP=t1*(amunot+sin(ai)*tanthb*af)
  ENDelse
  mupr=abs(mupr)
  munotp=abs(munotp)
  ;     compute ff
  ff=exp(-2.d0*tan(psi/2.d0))
  ;      compute pg: new values from Domingue et al., 2015
  p1=cos(g)
  p2=(-1.d0+3.d0*cos(g)^2)/2.d0
  pg=1.d0+0.1365d0*p1+0.08d0*p2
  ; print, 'p1,p2,pg=', p1,p2,pg
  ;      compute the H functions
  den=1.d0+2.d0*munotp*sqrt(1.d0-pisurf)
  hmunot=(1.d0+2.d0*munotp)/den
  den=1.d0+2.d0*mupr*sqrt(1.d0-pisurf)
  hmu=(1.d0+2.d0*mupr)/den
  if(hmu lt 0.d0)then hmu=1.d0
  if(hmunot lt 0.d0)then hmunot=1.d0
  ;      write(*,1234) hmu,hmunot
  ; 1234 format(1x,'hmu=',e12.5,1x,'hmunot=',e12.5)
  ;     compute the reflection function
  if(ai lt ae) then begin
    ang=amunot/munpn
  endif else begin
    ang=amu/mupnot
    ang2=amunot/(amu+amunot)
  endelse
  root=sqrt(1.d0+!pi*(tan(thetab)^2))
  geom1=munotp/(munotp+mupr)
  geom2=mupr*amunot/(mupnot*munpn)
  geom=geom1*geom2
  geom3 = amunot /(amu + amunot)
  ;      den=root*(1.d0-ff+(ff*ang/root))
  den=root*(1.d0-ff+(ff*ang*root))
  reflh=pisurf*((1.d0+bg)*pg-1.d0+hmu*hmunot)/!pi/4.d0
  ; simple test
  reflh = reflh * geom3
  ; reflh = reflh * geom/den
  ; print, 'refl in Hapke=', reflh
  ; units are intensity
  theta=acos(amu)*180./!pi
  theta0=acos(amunot)*180./!pi
  ;  10 continue
  return, REFLH
end



