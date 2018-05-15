pro batch_dark_rot, merc_center, dark_array, $  ; input
  merc_bright, sky_raw_img, sky_bright, sky_center, merc_reduced, merc_timestamp, rotation_required, SINGLE=single; output

  ; Find and read image files
  findpair3, merc_center, sky_center, /sky
  sky_raw_img = readfits(sky_center, /silent)
  merc_raw = 1.0*readfits(merc_center, center_header, /silent)
  sky_raw = 1.0*readfits(sky_center, sky_header, /silent)


  ;determine the new fits formatted date of the mercury file
  if sxpar(center_header, 'date-new') ne 0 then begin  ; usually just in apr1999
    merc_timestamp = string(sxpar(center_header, 'date-new'))
  endif else if strmatch(sxpar(center_header, 'date-obs'), '????-??-??T??:??:??*') then begin
    merc_timestamp = sxpar(center_header, 'date-obs')
    merc_timestamp= strsplit(merc_timestamp, 'T', /extract)
    merc_timestamp = merc_timestamp[0]
  endif else if strmatch(sxpar(center_header, 'date-obs'), '??/??/??*') then begin
    dateobs= strsplit(sxpar(center_header, 'date-obs'), '/', /extract)
    day = dateobs[0]
    month = dateobs[1]
    yr = dateobs[2]
    merc_timestamp = string('19'+yr + '-' +month+ '-' +day )
    merc_timestamp = merc_timestamp
  endif
  ;stop
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; 2) Dark and sky reduction from merc img
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  if isa(dark_array) then begin
    merc_bright = merc_raw*1.0- dark_array*1.0 ; multiply by a float to ensure correct output data type
    sky_bright = sky_raw*1.0- dark_array*1.0
  endif else if  isa(dark_array) eq 0 then begin
    merc_bright =merc_raw*1.0
    sky_bright = sky_raw*1.0
    print, "THERE IS NO DARK FILE"
  endif
  ; subtract the sky from the image
  merc_reduced = merc_bright*1.0 - sky_bright*1.0
  ;___________________________________________________________


  ;____________________________________________________________
  ;Rotation problem
  ;
  ; if rotation of the original image is not set, ask user to set a rotation direction
  if isa(rotation_required) eq 0 and not keyword_set(single) then begin
    ;stop
    good_rotation = ''

    ; its nearly impossible to tell if the center is rotated correctly, so we find a nearby north image
    ; and it should have fainter slices in the image at the top. If the original image has vertical slices,
    ; the fainter ones will be on the left or right edges of the image. Rotate to find the best orientation
    ; that puts the fainter slices on the top of a horizontally oriented image.
    findpair3, merc_center, nthpath, /north
    findpair3, nthpath, skynorth, /sky
    nth=1.0*readfits(nthpath, /silent) & nthsky = 1.0*readfits(skynorth, /silent)
    ;stop
    if isa(dark_array) then nth-=(1.0*dark_array)
    nth-=nthsky

    merc_c_img = readfits(merc_center)
    
    ;start showing rotated images:
    while not strmatch(good_rotation,'y')  do begin
      ;___________________________________________________________
      north_median_across_spectrum = median(nth, dim = 1)-median(nth)
      ; a plot of the array "north_median_across_spectrum" under correct rotation orientiation should
      ; show a decreasing brigtness trend (from high values on the left of the plot to lower values
      ; on the right of the plot showing a decrease in brightness from bottom of the raw image to the
      ; top of it. Comparing two sums (of the left side of the plot to the right side of the plot)
      ; should indicate a cumulatively brighter "left side" of the plot compared to the "right side".
      leftside = total(north_median_across_spectrum[0:north_median_across_spectrum.length/2])
      rightside =total(north_median_across_spectrum[north_median_across_spectrum.length/2+1:*])
      if leftside gt rightside then good_rotation = 'y'

    endwhile
  endif

  if keyword_set(single) then begin
    dims = merc_bright.dim
    xdim=dims[0] & ydim= dims[1]
    if ydim gt xdim then begin
      rotation_required = 1
    endif
  endif



  ;__________________________________________________
  if isa(rotation_required) then begin
    if rotation_required ne 0 then begin
      merc_reduced = rotate(merc_reduced, rotation_required)
      merc_bright  = rotate(merc_bright, rotation_required)
      merc_raw     = rotate(merc_raw , rotation_required)
      sky_bright   = rotate(sky_bright, rotation_required)
      sky_raw      = rotate(sky_raw, rotation_required)
      if isa(dark_array) then dark_array = rotate(dark_array, rotation_required)
    endif
  endif
  ;stop
  ; FLATFIELD BY SKY IMAGE
  ; 
  ; ### Consolidated as a new function, 'flatfield.pro', and applied outside this subroutine (cb 5/1/2018)
  ;stop
  ;  sky_summed_over_spectra=transpose(median(sky_bright, dim=1))
  ;
  ;  flat_field_array=sky_summed_over_spectra
  ;  size=merc_reduced.dim
  ;  x_size=size[0]
  ;  ;build flat field
  ;  for k=1,x_size-1 do flat_field_array=[flat_field_array,sky_summed_over_spectra]
  ;
  ;
  ;  ;divide merc_reduced by flat_field_array
  ;  merc_reduced /= flat_field_array
  ;flatfield, merc_reduced, sky_bright, merc_reduced

  atv, merc_reduced
  ;stop
  return
end
