pro darkNflat2, merc_center, dark_array, $  ; input
  merc_bright, sky_raw_img, sky_bright, sky_center, merc_reduced, merc_timestamp, rotation_required, SINGLE=single; output
  
  ; find the sky for the center
  
  findpair3, merc_center, sky_center, /sky
  sky_raw_img = readfits(sky_center, /silent)
  ;check the file names
  print, "Merc image: ", file_basename(merc_center)
  print, "Sky image: ", file_basename(sky_center)

  ; RENAME THE BASE DIRECTORY TO THAT OF THE CENTER FILE!! removerows,
  base_directory = file_dirname(merc_center)

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
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; 2) Dark and sky reduction from merc img, correct orientation of imgs, flat field generation
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;dark_array = dark_reduce3(merc_center)

  
  if isa(dark_array) then begin
    merc_bright = merc_raw*1.0- dark_array*1.0
    sky_bright = sky_raw*1.0- dark_array*1.0
  endif else if  isa(dark_array) eq 0 then begin
    merc_bright =merc_raw*1.0
    sky_bright = sky_raw*1.0
    print, "THERE IS NO DARK FILE"
  endif
  ; subtract the sky from the image
  merc_reduced = merc_bright*1.0 - sky_bright*1.0
;___________________________________________________________
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
  nth_contrast = [median(nth), max(nth)]
  ;nth_contrast = nth_contrast(sort(nth_contrast))
  ;start showing rotated images:
  while not strmatch(good_rotation,'y')  do begin
 
    
    if nth.length eq 1 then begin
      i =image(merc_c_img, min_value = mean(merc_c_img)*.3, max_value = mean(merc_c_img)*1.8, title = "Center Image") 
    endif else  i =image(nth, title = "North Image for Rotation Detection", min_value = nth_contrast[0], max_value = nth_contrast[1])

    ;ask for rotation parameter
    rotation_required = ''
    print, "Showing a nearby north image for rotation visual aid (since the center is fairly symmetric)."
    read, rotation_required, prompt = "Rotate the image (CCW only)? [0=n,1=90d, 2=180d, 3=270d,4=rotation set]  :"
    
   
    ;apply rotation parameter and show the result
    if nth.LENGTH gt 1.0e3  then begin 
       nth_rotd=rotate(nth, rotation_required)
      ;im.erase 
      ;im = image(nth_rotd, min_value = minv, max_value = maxv, title = "Nearby north image" ,/current)
      ;atv, nth_rotd
      i =image(nth, title = "North Image for Rotation Detection", min_value = nth_contrast[0], max_value = nth_contrast[1])
    endif 

    ;ask if rotation is good
    read,good_rotation, prompt = "Good rotation? (y/n) : "
    closewin
  endwhile
endif  
    
if keyword_set(single) then begin
  
  dims = merc_bright.dim
  xdim=dims[0] & ydim= dims[1]
  if ydim gt xdim then begin
    rotation_required = 1
    
  endif 
endif 


closewin
  
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
  ;stop
  sky_summed_over_spectra=transpose(median(sky_bright, dim=1))
  
  flat_field_array=sky_summed_over_spectra
  size=merc_reduced.dim
  x_size=size[0]
  ;build flat field
  for k=1,x_size-1 do flat_field_array=[flat_field_array,sky_summed_over_spectra]
  

  ;divide merc_reduced by flat_field_array
  merc_reduced /= flat_field_array
  closewin
  
  atv, merc_reduced
  ;stop
  return
end
