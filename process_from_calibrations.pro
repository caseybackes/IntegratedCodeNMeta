pro process_from_calibrations, merc_img_name, calibration_filepath, rotation_required, CT_RANGE_FACTOR = ct_range_factor, $
                                    merc_reduced_o, continuum_reflectance_image, emission_image,ROWS_AUTOREMOVED,ROWS_MANUALLYREMOVED
  
  orig_name =merc_img_name
  ;-----------------------------------------------------------------
  ;Step 1: Unpack the calibration parameters from the IDL save file.
  ;-----------------------------------------------------------------
  fitless_name =file_basename(merc_img_name)
  fitless_name =fitless_name.replace(".fit", '')
  restore, calibration_filepath
  merc_img_name = orig_name

  ; restoring the calibrations after having been saved with new slice indices (later in this code)
  ; saves what the last image was that used this code. in the merc.pro code, that is the north image.
  ; so we simply remind IDL what image we orignally passed into this procedure, and that it isnt what was saved merc
  ; from saving all variables last time this code was run. 
  ;!null = readfits(merc_img_name, hdr, /silent)
  

  if isa(observation_date) eq 0 then begin 
    es = ephemeris_structure.DATE_OBS 
    observation_date = es[0]
  endif 
  observation_date= strmid(observation_date, 0,10)
  
    ; provides access to :
  ;     DARK_ARRAY
  ;     ROTATION
  ;     SLICE_INDICES
  ;     SHEAR_MATRIX
  ;     COEFF
  ;     EPHEMERIS_STRUCTURE
  ;     DOPPLER_SHIFT
  ;     CORRECTED_OBSERVED_WAVELENGTH
  ;     FINAL_OBSERVED_WL
  ;     D2_START
  ;     D2_END
  ;     CONTINUUM_START
  ;     CONTINUUM_END
  
;stop
  ;-----------------------------------------------------------------
  ;Step 2: Sky pairing, dark array, and flat field reductions.
  ;-----------------------------------------------------------------
  ; Dark and flat reductions: 
  ;     We use a meaned dark frame from the ones availible in the directory of the center image. If 
  ;     dark frames are not availible (which does happen occasionally) then we ask the user to find 
  ;     a dark from from an adjacent observing day and then the program finds all dark frames in that
  ;     directory, and uses the mean of those dark frames. 
  ;     Since the same dark frame can be used to reduce all observations of a given day, the 
  ;     "meaned dark frame" can be stored in a variable - restored from the calibration IDL save file. 
  ;     The returned products are the Mercury observation after subtracting the dark field, sky background,
  ;      
  ;stop
  dark_array = dark_reduce3( merc_img_name)
  s =size(readfits(merc_img_name, /silent))
  sx = s[1] & sy = s[2]
  if total(dark_array.dim eq [sx,sy]) ne 2 then dark_array = !null
  ;stop
;  darkNflat2, merc_img_name, dark_array, $  ; input
;    merc_bright, sky_raw_img, sky_bright, sky_raw_name, merc_reduced, merc_timestamp, rotation_required
  batch_dark_rot, merc_img_name, dark_array, $  ; input
    merc_bright, sky_raw_img, sky_bright, sky_center, merc_reduced, merc_timestamp, rotation_required, SINGLE=single; output

;stop
  ;-----------------------------------------------------------------
  ;Step 3: Registration.
  ;-----------------------------------------------------------------
  ; pull shear vectors from the restored shear matrix
   xi = shear_matrix[*,2] &   xo = shear_matrix[*,0]  & yi = shear_matrix[*,3]   &  yo = shear_matrix[*,1]

  ; Subtract the unreg'd sky from the unreg'd mercury observation, 
  ; then register the sky image and the (merc-sky) image.
  ; build a flat field from the reg'd sky image, and divide the (merc-sky)
  ; image by the flat field array. 

  merc_minus_sky = merc_bright*1.0 - sky_bright*1.0
  ;now we have a good merc image that needs to be reg'd
  merc_minus_sky_regd = warp_tri(xo, yo, xi, yi, merc_minus_sky)
  sky_regd= warp_tri(xo, yo, xi, yi, sky_bright)

  merc_reduced_o =  flatfield(merc_minus_sky_regd,sky_regd) 
  atv, merc_reduced_o
  ;-----------------------------------------------------------------
  ;Step 3b: Slice Gap removal.
  ;-----------------------------------------------------------------
  
  ; ON A COPY OF THIS IMAGE, APPLY THE AUTO SLICE GAP REMOVAL, THEN REMOVE EACH OF THE MANUAL REMOVED ROWS
  ; AND SHOW THE RESULTING IMAGE. IF THIS ISN'T A GOOD RESULT, RUN AUTO AND MANUAL SLICE GAP REMOVALS BUT 
  ; NOT SAVING THE RESULTING REMOVED ROWS TO THE CALIBRATION FILE.  remrows_manual
  ;stop
  ; MAKE A COPY OF THE IMAGE
  merc_copy = merc_reduced_o
  
  ; REMOVE AUTO REMOVED ROWS
  ;                                 *** Temporarily Disabled ***
  ; 
;  if isa(rows_autoremoved) then begin
;  if total(strmatch(rows_autoremoved, 'none')) eq 0 then begin ; only if 'none' is not in the array
;    merc_copy_post_autorowremove = removerows(merc_copy, rows_autoremoved)
;    ; ASK IF IT SHOULD BE KEPT
;    keep_autoremovedrows='' & read, keep_autoremovedrows, prompt = "Keep the auto removed rows image? (y/n): "
;    if strmatch(keep_autoremovedrows, '*[ ,y]*') then merc_copy = merc_copy_post_autorowremove
;  endif
;  endif 
;  ;REMOVE MANUALLY REMOVED ROWS
;  if isa(rows_manuallyremoved) then begin
;  if total(strmatch(rows_manuallyremoved, 'none')) eq 0 then begin ;only if 'none' is not in the array
;    foreach element,rows_manuallyremoved do merc_copy_post_manualremove = removerows(merc_copy, element)
;    ; ASK IF IT SHOULD BE KEPT
;    keep_manuallyremovedrows='' & read, keep_manuallyremovedrows, prompt = "Keep the manually removed rows image? (y/n): "
;    if strmatch(keep_manuallyremovedrows, '*[ ,y]*') then merc_copy = merc_copy_post_manualremove
;  endif ; total(strmatch(rows_manuallyremoved, 'none')) eq 0 
;  endif
  merc_reduced = merc_copy ; which reflects any and all slice gap removal done to the image
;atv, merc_reduced
;  ; if there exists rows to be removed from this image, 'rows_auto_removed' or 'rows_manually_removed'
;  ; then remove them from this image and check the image to see the result. It is possible that the image will
;  ; drift across the CCD during an observing run (I've seen this in the data). So removing the rows 
;  ; previously designated as noise in an adjacent image may not necessarily be noise rows in this particular
;  ; image. It must be verified (if possible). 
;  stop
;  ; if the rows_autoremoved key exists and is not 'none' then remove those rows and see how it looks / if it 
;  ; removed the correct rows. 
;  if isa(rows_autoremoved) and total(strmatch(rows_autoremoved,'none')) eq 0  then  begin 
;    ; apply the removal to a copy of the image
;    test_image_rows_auto_removed = removerows(merc_regd, rows_autoremoved)
;    show_test_image_rows_auto_removed = image(test_image_rows_auto_removed,$
;      min_value = median(test_image_rows_auto_removed)-2*stddev(test_image_rows_auto_removed), $
;      max_value = median(test_image_rows_auto_removed)+2*stddev(test_image_rows_auto_removed))
;    ask = '' & read, ask, prompt = "Apply the 'rows auto removed' correction? (y/n) : "
;    if strmatch(ask, '*[ ,y]*') then merc_regd_auto_gap_removal = test_image_rows_auto_removed
;  endif ; isa(rows_autoremoved)
; 
;  if 
; 
;  remrows2, merc_regd,slice_indices, merc_regd_pregapless,rows_autoremoved
;  closewin
;;  stop
;  ask_remrows = ''
;  read, ask_remrows, prompt = "Manually remove slice gaps and spikes?"
;  if ask_remrows eq 'y' OR ask_remrows eq '' then begin 
;    remrows_manual, merc_regd_pregapless, merc_regd_gapless, rows_manuallyremoved
;    ;remrows_manual, original_image, gapless_image, rows_manually_removed
;
;  endif
;  ;stop
;  ask_keep_new_gapless_image = ''
;  read, ask_keep_new_gapless_image, prompt = 'Keep the new "gapless" image moving forward?  '
;  if ask_keep_new_gapless_image eq 'y' OR ask_keep_new_gapless_image eq '' then  merc_regd_gapless
;print, "In this window, we can adjust the slice boundaries used in the pixel remapping process. "
;print, "Click near a real boundary (local minima) to move the closest slice index (vertical line) to that minima. " 
;slice_adjust, merc_reduced, slice_indices 

 
    save, OBSERVATION_DATE_SHORT, DARK_ARRAY,ROTATION_REQUIRED,SKY_RAW_NAME, $
      SLICE_INDICES, $
      SHEAR_MATRIX, ROWS_AUTO_REMOVED,ROWS_MANUALLY_REMOVED, $
      EPHEMERIS_STRUCTURE, $
      CORRECTED_OBSERVED_WAVELENGTH, $
      D2_START,D2_END, $
      CONTINUUM_START, CONTINUUM_END,$
      FINAL_OBSERVED_WL,FINAL_SOLAR_SPECTRUM, $
      EMISSION_IMAGE, CONTINUUM_REFLECTANCE_IMAGE, DELTA_LAMBDA_D2,DELTA_LAMBDA_CT,$
      filename = calibration_filepath




  ;-----------------------------------------------------------------
  ;Step 4: Observed spectrum of this image.
  ;-----------------------------------------------------------------
  observed_spectrum =  mean(merc_reduced[*,slice_indices[0]:slice_indices[-1]], dim = 2); this will work even if slice boundaries were manually chosen
  size_of_observed_spectrum = observed_spectrum.length
  ;-----------------------------------------------------------------
  ;Step 5: Match spectral limits between mercury and solar spectra.
  ;-----------------------------------------------------------------
  match_spectral_bounds4, corrected_observed_wavelength,$  ; from restored calibrations
     observed_spectrum, $                                 ; from step 4 above
     merc_reduced, $                                         ; from step 3 above
     final_observed_spectrum, final_observed_wl, $         ; outputs
     final_solar_spectrum, final_solar_wl, $               
     trimmed_merc_image                                    
closewin
;stop

  ;-----------------------------------------------------------------
  ;Step 6: Remapping to spatial image.
  ;-----------------------------------------------------------------
  if not keyword_set(ct_range_factor) then ct_range_factor =1.0
  if (ct_range_factor eq 1.0) then begin
     man_imbuild2, trimmed_merc_image, $
      slice_indices, d2_start, d2_end, continuum_start, continuum_end, $
      final_solar_spectrum, file_basename(merc_img_name) ,$
       emission_image, continuum_reflectance_image;, /showplots
  endif else begin
    
    
    spectral_integration_bounds, final_observed_spectrum, slice_indices, d2_start, d2_end, CONTINUUM_START, CONTINUUM_END, m=CT_RANGE_FACTOR
    man_imbuild2, trimmed_merc_image, slice_indices, $
      d2_start, d2_end, CONTINUUM_START, CONTINUUM_END, $
      final_solar_spectrum, file_basename(merc_img_name) , emission_image, continuum_reflectance_image

    
  endelse
end