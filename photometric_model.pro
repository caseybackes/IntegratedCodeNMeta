

function photometric_model,GIX, DIAM, DEFECT, STEP

  ;arguments: PHASE ANGLE (DEG),Mercury’s Diameter (arcsec),Defect of Illumination (arcsec), pixel size (arcsec)

  imagesize=128*2 ;pixels, this should be an even integer




  ;     GIBMERC.F generates a 128X128 iraf image of Mercury Hapke reflectivity
  ;	modified by RMK to produce a 40x40 matrix at 0.5 arcsec resolution
  ;     in steps of arc sec, size of which are inputs to the program.
  ;     The Mercury image is centered.  Modification 4/98 by AEP of
  ;     GIBBOUS.FOR program written by THM.
  ;      Old  Hapke constants: w=0.18, h=0.11, Bo=2.4, b=0.349?, c=0.21
  ;      New constants, Mallama et al Icarus 2002:
  ;       w=0.20,h=0.065,Bo=2.4,b=0.20,c=0.18,thetabar= 16 deg (0.279 rad)
  ;       (Mallama's narrow search constants)
  ;	Warrel, 2004 constants are w=0.15, h=0.08, B0=2.9, b=0.18, c=1.2, thetabar=10 deg ( 0.1745 rad)
  ;       The average of all the reflectances is calculated and put into
  ;       the image header.  The number of pixels with values greater than
  ;       zero is also put into the image header.
  ;
  ;
  ;        DOUBLE PRECISION CAI,CAE,REFL,GIX,GIR
  ;        DOUBLE PRECISION RILON,RILAT,XI,YI,top
  ;        DOUBLE PRECISION CAIEX,CAEEX,RHAPKE
  ;        INTEGER NAXIS,PIXTYPE,AXLEN(7)
  ;        INTEGER IM,IER,DTYPE,NLINES,NCOLS,I,J
  ;        REAL PIX,SUM
  ;        CHARACTER*60 FNAME,MERIMAGE,PIXDIR
  ;1233    FORMAT(2X,I6,2X,I6,2X,D12.6)
  ;1234    FORMAT(a60)
  ;1235    FORMAT(2(F12.6))
  ;1236    FORMAT(1X,4(D12.6),I6)
  ;C1238   FORMAT(10(1x,d8.3))
  ;1238    FORMAT(1x,D12.6)
  ;1239    FORMAT(1X,F6.3)
  ;1237    FORMAT(2(D12.6))
  ;1999    FORMAT(10X,I6,10X,I6)
  ;1249    format(d12.6)
  refl_eq=dblarr(imagesize)
  XI=dblarr(imagesize)
  YI=dblarr(imagesize)
  RILON=dblarr(imagesize,imagesize)
  RILAT=dblarr(imagesize,imagesize)
  REFL=dblarr(imagesize,imagesize)
  ARRAY=dblarr(imagesize,imagesize)
  RILON_EQ=DBLARR(imagesize)


  for JINT=0,imagesize-1 do begin
    for KINT=0,imagesize-1 do begin
      REFL[JINT,KINT]=0.0d0
    endfor
  endfor
  ;        write(6,*)'This program is for phase angles less than pi/2'
  ;        WRITE(6,*) 'Enter the Mercury phase angle, degrees '
  ;        READ(5,1249)GIX
  GIR=GIX/57.29577d0
  ;        WRITE(6,*) 'Enter the diameter, arcsec'
  ;        READ(*,*)DIAM
  ;        write(6,*)'Enter the defect of illumination, arcsec'
  ;        read(5,*)defect
  ;	convert to degrees
  DIAM=DIAM/3600.D0
  DEFECT=DEFECT/3600.d0
  ;	print, 'defect_deg =',defect
  RADIUS=0.5D0*DIAM
  AELL=-(DEFECT-RADIUS)
  AELL=double(AELL)
  ;	print, 'aell=',aell
  ;;;;;;;;;;;;;;;;; IF(AELL LT 0.00) then  GOTO, jump1
  RAELL=AELL/RADIUS
  ;        write(6,*)'Enter the arcsec step in the output image'
  ;        read(*,*)step
  ;
  ;       Number of arcsec steps from center to limb:
  ;
  nstep=fix(3600.D*radius/step)
  ;	print, 'nstep=', nstep
  ;	first quadrant
  ;print, 'first quadrant'
  FOR IX=imagesize/2,(imagesize/2+nstep) DO BEGIN
    ;         XI(IX)=(1.0D-2)+(0.98D0/(nstep))*(IX-65)
    XI[IX]=(1.0D-2)+(0.98D0/(nstep))*(IX-imagesize/2)
    YLIM=SQRT(1.0D0-(XI[IX]^2))
    ;        DO 4000 IY=65,(65+nstep)
    FOR IY=imagesize/2,(imagesize/2+nstep) DO BEGIN
      ;        YI(IY)=(1.0D-2)+(0.98D0/(nstep))*(IY-65)
      YI[IY]=(1.0D-2)+(0.98D0/(nstep))*(IY-imagesize/2)
      IF(YI[IY] GT YLIM) THEN BEGIN
        RILON[IX,IY]=0.0D0
        RILAT[IX,IY]=0.0D0
        REFL[IX,IY]=0.0D0
      ENDIF ELSE BEGIN
        ;	SHOULD NOT SEE 90.00
        RILAT[IX,IY]=ASIN(YI[IY])
        TOP=(XI[IX])/(SQRT(1.d0-(YI[IY]^2)))
        RILON[IX,IY]=-ASIN(TOP)
        REFL[IX,IY]=1.0D0
        CAEEX=(COS(RILON[IX,IY])*(COS(RILAT[IX,IY])))
        CAIEX=COS(GIR+RILON[IX,IY])*COS(RILAT[IX,IY])
        IF(CAEEX GE 0.9998D0) THEN  CAEEX=0.9998D0
        IF(CAIEX GE 0.9998D0) THEN CAIEX=0.9998D0
        ;	;print, 'calling HAPKE'
        RHAPKE=HAPKE(CAEEX,CAIEX,GIR)
        ;print, caiex,caeex, rhapke,!pi*rhapke
        REFL[IX,IY]=RHAPKE
        ;	if (IY eq 20) then begin
        ;           print, 'ix, iy, refl', IX,IY,REFL[IX,IY]
        ;	endif
        ;       WRITE(*,1233)IX,IY,RHAPKE
      ENDELSE
    ENDFOR
    ;	2ND QUADRANT
    ;        for IYN=(64-nstep),64 do begin
    FOR IYN=(imagesize/2-nstep),imagesize/2 DO BEGIN
      ;        RILAT[IX,IYN]=-RILAT[IX,(129-IYN)]
      ;        RILON[IX,IYN]=RILON[IX,(129-IYN)]
      RILAT[IX,IYN]=-RILAT[IX,(imagesize-IYN)]
      RILON[IX,IYN]=RILON[IX,(imagesize-IYN)]
      ;        REFL(IX,IYN)=REFL(IX,129-IYN)
      REFL[IX,IYN]=REFL[IX,imagesize-IYN]
      IWT=-IYN
      ;       WRITE(*,1233)IX,IWT,REFL(IX,IYN)
      ;           print, 'ix, iy, refl', IX,IY,REFL(IX,IY)
    ENDFOR
  ENDFOR
  ;
  ;       Number of arcsec steps from center to terminator
  ;
  ;	print, ' step=', step
  ;	print, 'aell=', AELL
  ;        nstep=fix(3600.D*radius/step)
  nstept=3600.D*AELL/step
  ;print, 'nstept=', nstept
  nstept=FIX(nstept)
  ;	print, 'nstept=', nstept
  ;
  ;print, 'third quadrant'
  ;        DO 9000 IX=(64-nstept),64       ! 3RD QUADRANT
  ;        XI(IX)=(1.0D-2)+(0.98d0/(nstep))*(IX-65)
  FOR IX=(imagesize/2-nstept),imagesize/2 DO BEGIN
    XI[IX]=(1.0D-2)+(0.98d0/(nstep))*(IX-imagesize/2)
    if((1.0D0-((XI[IX]/(RAELL+.005d0))^2)) le 0.0d0) then begin
      ylim=0.0d0
    endif else begin
      YLIM=SQRT(1.0D0-((XI[IX]/(RAELL+.005d0))^2))
    endelse
    ;        DO 8000 IY=65,(65+nstep)
    ;        YI(IY)=(1.0D-2)+(.98/(nstep))*(IY-65)
    FOR IY=imagesize/2,(imagesize/2+nstep) DO BEGIN
      YI[IY]=(1.0D-2)+(.98d0/(nstep))*(IY-imagesize/2)
      ;	print,'IY, Y[IY]=', iy, YI[IY]
      IF(abs(YI[IY]) GE YLIM) then BEGIN
        RILON[IX,IY]=0.0D0
        RILAT[IX,IY]=0.0D0
        REFL[IX,IY]=0.0D0
      ENDIF else begin
        ;	SHOULD NOT SEE 90.00
        RILAT[IX,IY]=ASIN(YI[IY])
        TOP=(XI[IX])/(SQRT(1.d0-(YI[IY]^2)))
        if (top ge 1.d0) then top=1.d0
        RILON[IX,IY]=-ASIN(TOP)
        ;        REFL[IX,IY]=1.0D0
        CAEEX=(COS(RILON[IX,IY])*(COS(RILAT[IX,IY])))
        CAIEX=COS(GIR+RILON[IX,IY])*COS(RILAT[IX,IY])
        IF(CAEEX GE 0.9998D0) then CAEEX=0.9998D0
        IF(CAIEX GE 0.9998D0) then CAIEX=0.9998D0
        ;print, '3rd quadrant, x, y, caeex, caiex=',xi[ix], yi[iy], caeex, caiex
        ;	print, 'lat long =', rilat[ix,iy],rilon[ix,iy]
        RHAPKE=HAPKE(CAEEX,CAIEX,GIR)
        ;print, caiex,caeex, rhapke,!pi*rhapke
        REFL[IX,IY]=RHAPKE
        ;print, 'x,y, refl=', xi[ix], yi[iy], refl[ix,iy]
        IF(REFL[IX,IY] LE 0.0D0) THEN  REFL[IX,IY]=0.0D0
      endelse
      ;           print, 'ix, iy, refl', IX,IY,REFL[IX,IY]
    ENDFOR
    ;        ! 4TH QUADRANT
    ;print, '4th quadrant'
    ;print, 'nstept=', nstept
    FOR IYN=(imagesize/2-nstep),imagesize/2 DO BEGIN
      RILAT[IX,IYN]=-RILAT[IX,(imagesize-IYN)]
      RILON[IX,IYN]=RILON[IX,imagesize-IYN]
      REFL[IX,IYN]=REFL[IX,imagesize-IYN]
    ENDFOR
  ENDFOR
  for IX=(imagesize/2-NSTEPT), (imagesize/2) DO begin
    iix=ix-(imagesize/2-NSTEPT)
    refl_eq[iix]=Refl[ix,imagesize/2]
    rilon_eq[iix]=rilon[ix,imagesize/2]
    ;print,'X, Y , refl_eq', XI[IX],YI[imagesize/2],REFL_EQ[IIX]
  endfor

  FOR IX=imagesize/2,(imagesize/2+nstep) DO BEGIN
    iix=ix-10
    ;	print, iix
    ;;;;;;;;;;;	 refl_eq[iix]=Refl[ix,20]
    ;;;;;;;;;;;	 rilon_eq[iix]=rilon[ix,20]
    ;           print,'X, Y , refl_eq', XI[IX],YI[20],REFL_EQ[IIX]
  endfor
  ;      Compute average reflectivity, sum
  ;
  ;        num=0
  ;        sum=0.0
  ;        for i=0,39 do begin
  ;        for j=0,39 do begin
  ;        if(array[i,j] gt 0.0) then begin
  ;         sum=sum+array[i,j]
  ;         num=num+1
  ;         endif
  ;	endfor
  ;	endfor

  ;         xnum=float(num)
  ;         sum=sum/xnum
  ;        ;print,' Average reflectivity = ',sum

  ;;;;;;;;;plot,rilon_eq,refl_eq,xtitle='Hapke_10_eq'

  return,refl

end
