;;;;;click through inactive windows

;+
; Description:
;    Reduce raw spectral images of mercury D2 observations to 2D spatial map of the disk.
;
; Params:
;    merc_center_image : the image from which calibrations are determined and applied to any adjacent images
; 
;
; Keywords:
;    _SINGLE :  set this keyword if only one image is to be used for the reduction and measurement of D2. 
;              no other images will be paired with it. 
;    _EAST (not yet implemented): set this keyword to search for a good 'east' image to pair with the center image. 
;    _WEST (not yet implemented): set this keyword to search for a good 'west' image to pair with the center image. 
;    
; NON-STANDARD (CUSTOM) PROCEDURES USED (FULL LIST MAP): 
;   MERC
;     SINGLEMERCPROC
;       DARK_REDUCE3
;       DARKNFLAT2
;         FINDPAIR3
;           REMOVECOPY
;         ATV
;         CLOSEWIN
;       SLICEBOUNDS
;         VLINE
;       REMROWS2
;         REMOVEROWS
;       REGISTER5
;         WARP_TRI 
;       MERC_EPHEM
;         REDATE
;         READ_CSV
;       DETERMINE_WAVELENGTH_SCALE
;         VLINE
;       LINFIT
;       MATCH_SPECTRAL_BOUNDS4
;         VALUEPOSITION
;         CONGRID
;         REGRESS
;       SPECTRAL_INTEGRATION_BOUNDS
;         VLINE
;       MAN_IMBUILD2
;         CONGRID
;         SCALE2MATCH
;     FINDPAIR3
;       REMOVECOPY
;     PROCESS_FROM_CALIBRATIONS
;       DARK_REDUCE3
;       DARKNFLAT2
;       WARP_TRI
;       ATV
;       MATCH_SPECTRAL_BOUNDS4
;         VALUEPOSITION
;         CONGRID
;         REGRESS
;       CLOSEWIN
;       MAN_IMBUILD2
;       SPECTRAL_INTEGRATION_BOUNDS
;     SWAPNS
;     CONVOLVE_AND_COMPARE3
;       PHOTOMETRIC_MODEL
;       GAUSS_SMOOTH
;     AUTOSTITCH5
;       CLOSEWIN
;       X2
;       SMALLSTITCH3
;     SIGFIG
;     DREWCOMP
;     
;     
;     
;     
;     
;     
;-      


pro merc, merc_center_image, SINGLE = single, CT_RANGE_FACTOR = ct_range_factor

curdir = strsplit(file_dirname(merc_center_image), '\', /extract)
while curdir[-1] ne "MercuryResearch" do curdir = curdir[0:-2]
curdir = strjoin(curdir, '\')

;stop
print, ""
print, "INSTRUCTIONS -"
print, "We automatically detect the most appropriate calibration file, if it exists, to process this image(or set). "
print, "If mulitple calibration files exist, you can choose which one to use - usually the latest is better. "
print, "If no calibration file exists yet, we jump strait into calibrating the center image you put in."
print, ''

; get the observation date of the image
;obsdate = strmid(sxpar(headfits(merc_center_image.compress()), 'date-obs'), 0, 10)
obsdate = strmid(sxpar(headfits(strtrim(merc_center_image)), 'date-obs'), 0, 10)

if not isa(obsdate) then obsdate = strmid(sxpar(headfits(merc_Center_IMAGE), 'date-new'), 0, 20)

; search for calibration params: 
cal_folder = "C:\Users\Casey Backes\Documents\IDLWorkspace84\Default\MercuryResearch\IntegratedCodeNMeta\calibration_params"
;serach for the observation date wihtin the calibrations folder 
cals = file_search(cal_folder, '*'+string(obsdate)+'*')

;for older fits date formats that used the slashes format(ex: 06/06/94) : 
;(line below didnt work when processing mercc-050a in 26 may 2011. Solved with following if-statement. 
;if cals eq '' and cals.length eq 1 then cals = file_search(cal_folder, '*'+ string(redate(merc_center_image))+ '*')
if cals.length eq 1 then begin
  if cals eq '' then begin
    cals = file_search(cal_folder, "*"+string(redate(merc_center_image))+ "*")
  endif
endif 

;if there are no predetermined calibrations for this day, start the singlemercpro program 
if cals.length eq 1 then begin
  
  if (strmatch(cals, "*calibr*")) lt 1 then begin 
  print, "Found no calibration files. Starting from 'singlemercproc.pro'... "
  ;stop
  singlemercproc, merc_center_image, center_ct, center_em , calibration_filepath
  restore, calibration_filepath
endif 
endif

if cals.length gt 1 then begin 
  
  print, "Found more than one calibration file..."
  
  for o=0,cals.length-1 do begin 
    print, o+1,': ',file_basename(cals[o])
  endfor  
  m = ''
  read, m, prompt = "Which calibration file? (0 or c = cancel and abort, 'Enter' = latest calibration):  "
  if strmatch(m, '*[0,c]*') then retall
  calibration_filepath = cals[m-1]
  restore, calibration_filepath
  
  
endif


if cals.length eq 1 and cals ne '' then begin
  if strmatch(cals, '*calibration*') gt 0 then begin 
    restoreme = '' & read, restoreme, prompt = "Found only one calibration file. Restore? (y/n)  "
    if restoreme eq 'y' or restoreme eq '' then begin 
      restore, cals[0] & calibration_filepath = cals[0] 
    endif else if restoreme eq 'n' then begin
      read, restoreme, prompt = "Start new calibration? (y/n=exit): "
      if restoreme eq 'n' then retall else if restoreme eq 'y' or restoreme eq '' then begin
        singlemercproc, merc_center_image, continuum_reflectance_image, emission_image, pos_savefile_name
        calibration_filepath = pos_savefile_name
        restore, pos_savefile_name
      endif
    endif
  endif
    
  
;stop
endif
print, "Calibration File:"
print, file_basename(calibration_filepath)
;__________________________________________________________________________________________________________________________
;  for the north/south/center set of mercury observsations to be used as one image...

if not keyword_set(single) then begin
  ;stop
  ; Find the best north and south images. 
  findpair3, merc_center_image, merc_north_image, /north
  findpair3, merc_center_image, merc_south_image, /south
  ;stop
  ; remove any spaces at the end of the filepath string (makes string compatible with readfits() and headfits() ) 
  merc_center_image=strtrim(merc_center_image)
  merc_south_image =strtrim(merc_south_image)
  merc_north_image = strtrim(merc_north_image)

  

  print, ""
  print, "INSTRUCTIONS -"
  print, "Ensure good files are chosen for the north, south and sky flats. "
  print, "Usually they will have a close range of indices, and need to have "
  print, "some signal that the file being used for 'north' is indeed a north "
  print, "hemisphere observation. Same for the south image. Be careful to check "
  print, "for 'mercsw-xxa.fits' where this would be a southwest image, not a south."
  
  print, ''
  Print, "Check the images in the set. They may be mislabed or incorrectly-selected by the autmation. "
  print, "For instance, to change the south image:
  print, "IDL> merc_south_image = filpath to better image
  print, ''
  print, "If you want to see a 'chronology' to help decide if the chosen files are the best, use :"
  print, '        IDL> chrono2, merc_center_image, /printout, [/saveme - to save output to CSV] '
  print, ''

  print, '-----------------------------------------------------------------'
  print, "north: ", file_basename(merc_north_image)
  print, "center: ", file_basename(merc_center_image)
  print, "south: ", file_basename(merc_south_image) 
  print, '-----------------------------------------------------------------'

  ;stop;" .c to continue" ; chrono2
  
  ; _______________________________
  ; Process / Restore Center Image:
  ; _______________________________
  restore, calibration_filepath
  print, "Processing center image..."
  if not isa(center_ct) then process_from_calibrations, merc_center_image, calibration_filepath, rotation_required,ct_range_factor=ct_range_factor, $
    merc_c_reduced, center_ct, center_em, ROWS_AUTOREMOVED, ROWS_MANUALLYREMOVED
  print, "... done with center."
  print, ''
  print, ''
  ;stop
  ; ________________________
  ; Process North Image: 
  ; ________________________  

    print, "Processing north image..."
    process_from_calibrations, merc_north_image, calibration_filepath, rotation_required,ct_range_factor=ct_range_factor, $
      merc_n_reduced, north_ct, north_em, ROWS_AUTOREMOVED, ROWS_MANUALLYREMOVED
    print, "... done with north."
    print, ''
    print, ''

    
  ; ________________________
  ; Process South Image:
  ; ________________________

    print, "Processing south image..."
    process_from_calibrations, merc_south_image, calibration_filepath, rotation_required,ct_range_factor=ct_range_factor, $
      merc_s_reduced, south_ct, south_em, ROWS_AUTOREMOVED, ROWS_MANUALLYREMOVED
    print, "... done with south."
    print, ''
    print, ''    


  
  
  
  ; __________________________________________
  ; Correct north/south labeling and placement:
  ; __________________________________________
;  north_slice = north_ct[5,*]
;  south_slice = south_ct[5,*]
;  if total(deriv(north_slice)) gt 0 and total(deriv(south_slice)) lt 0 then begin
;    print, '*****'
;    print, "*****North and south found to be reversed ... flipping the north and south images
;    print, '*****'
;    swapns, north_ct, south_ct
;    swapns, north_em, south_em
;    swapns, merc_north_image, merc_south_image
;  endif
;  
  
  ; ________________________________________________
  ; Kilorayleigh Calculation - Determine dLambda:
  ; ________________________________________________

 
  ephem=ephemeris_structure
  
  ; Check that the Delta lambda value is set, it is critical for the convolve_and_compare code to give
  ; a final scaling value. 
  if not isa(delta_lambda) then delta_lambda = delta_lambda_ct 
  ; if for some wild reason delta lambda ct didnt come with the restored variables, then simply calculate it from 
  ; the array "corrected observed wavelength". 
  if not isa(delta_lambda) then begin 
    print, "Auto calculating delta_lambda value from difference between two adjacent values in the corrected_observed_wavelength array..."
    delta_lambda = abs(corrected_observed_wavelength[d2_end]-corrected_observed_wavelength[d2_start])
    ; delta lambda should be somewhere around 0.13 to 0.18 (Angstroms)
    print, "Calculated delta lambda value of ", delta_lambda, '. Check it. '
;    wait, 1
  ENDIF


  good_compare = 'n'
  while good_compare eq 'n' do begin
    print, "Ephemeris data lists the position WRT the sun as ",(ephem.slashr eq '/T' ? '"trailing:"':'"leading:"'),"." 
    ; Lets do a visual check on the data at this point. The stacked images should show a "north" on top and "south" below the "center".
    check_correct_ns_labels = image([[south_ct],[center_ct],[north_ct]], layout= [1,1,1], rgb_table= 13, title= "Current Plancement")

    ; An example of leading and trailing images: 
    case !Version.os_family of 
      "Windows": READ_JPEG, "C:\Users\Casey Backes\Documents\IDLWorkspace84\Default\MercuryResearch\IntegratedCodeNMeta\mercphase_img.JPG", mercphase
    endcase
    check_correct_ns_labels = image(mercphase, /current, layout = [3,1,3])
    check_correct_ns_labels = image(reverse(mercphase, 2), /current, layout = [3,1,1])
    !null = text([40,466],[355,355],["EX: Trailing", "EX: Leading"], /device)
    !null = text(50, 450, "Position WRT Sun:", /device, font_size = 15)
    !null = text(50, 430, "Trailing or Leading? ", /device, font_size = 15)
    
    appearant_solar_position= ''; as seen in the observation. 
    read, appearant_solar_position, prompt = "Does the observation appear to be (l)eading or (t)railing the sun? : "
    if strmatch(appearant_solar_position, '*l*') then appearant_solar_position = 'L'
    if strmatch(appearant_solar_position, '*t*') then appearant_solar_position = 'T'
    closewin
    

    checkpoint_savefile =string(curdir +'\IntegratedCodeNMeta\'+ 'pre CandC checkpoint.sav')
    save, /variables, filename = checkpoint_savefile

   
    convolve_and_compare3, $
      ephemeris_structure, $      the structure that contains metadata for this observation geometry
      center_ct, $                image of the center of mercury in a quiet continuum band
      0.5, $                      angular height of one pixel row
      appearant_solar_position, $ mercury as leading or trailing the sun, from observers perspective
      delta_lambda, $             the spectral integration width for each remapped pixel (~0.1-0.2 Angstroms).
      image_pixel_multiplier, $   the multiplicative constant that brings the observed image to values and units of kR
      seeing, $                   the best estimate for the effective seeing in the center continuum observation                        imcenter_offset, eq_slice_pos
      model_img, $                10x10 image array of model, blurred with best estimate of observed seeing centered on disk. 
      ;full_model_img, $           256x256 image array of model, blurred with best estimate of observed seeing. 
      normalization_factor, $     multiplicative value bringing observation to intensity of the model. 
      imcenter_offset, $          the distance in the full size model from the center of the image array to the brightest pixel
      eq_slice_array, $           the array of the equitorial slice - used to identify the center in composite images
      eq_slice_pos;               pixel row in center continuum image that was chosen to be the equitorial pixel row. 
    ;                             Since the model and observation equitorial pixel rows are shifted to match intensity locations,
    ;                             this determined distance is taken to be the same between the two images. EX: the peak equitorial intensity
    ;                             of the model lies (say at ) 4 pixels from the center of the theoretical disk. When the peak intensity of the
    ;                             observation is put in the same place and the seeing is estimated, then the center of the simulated disk can
    ;                             reasonably be taken as the center of the observed disk.
    read, good_compare, prompt = "Satisfied with model building and seeing estimate? : (y/n)"
  
  endwhile
closewin

save, /variables, filename = checkpoint_savefile
print, "Check point Saved! Restore the .sav file:",checkpoint_savefile 

; stitch the composite image between the three
approvefinal = ''                   
while approvefinal ne 'y' do begin
  closewin 
  ; autostitch produces (among other things) the best found overlap in pixel size (not spatial). 
  autostitch5, model_img,  normalization_factor, $ 
    eq_slice_pos, imcenter_offset, $
    center_ct, north_ct, south_ct, $
    center_em, north_em, south_em, $
    best_nco, best_sco, $ 
    final_ct_img,final_em_img ,final_model
 
  !null = text(250,480, string("Center: " + string(file_basename(merc_center_image))), /device)
  !null = text(50, 5, "Updated with latest autostitch code", /device)
  !null = text(50, 60, string("N/C overlap: " + string(best_nco)), /device)
  !null = text(50, 45, string("S/C overlap: " + string(best_sco)), /device)
  !null = text(50, 30, string("Seeing: " + strmid(seeing,1,3)+'"'), /device)
  read, approvefinal, prompt = "Do you approve of final image? (y/n): "

  endif 
  
endwhile
save, /variables, filename= checkpoint_savefile
print, "Check point Saved! Restore the .sav file: ", checkpoint_savefile

;-------------Final Image Products------------- 
; The image pixel multiplier simply puts "counts" or "reduced ADU" 
; into values and units of kiloreyleighs. 
; (for some reason, the image pixel multiplier is an array of length 1, so we extract it
; and essentially make it a float-type value. 

d2 = image_pixel_multiplier[0]*final_em_img   ;|
ct = image_pixel_multiplier[0]*final_ct_img   ;|
;----------------------------------------------



;---------------------------------------------------------------------------
;   The End
;                 Roll Credits!!!
; 
; 
; At this point, we have the final 10x30-ish image arrays that are the product of this pipeline. Most of the code below 
; is just setting up a display of the final continuum and NaD2 emission images as JPGs for faster browsing later. 
; There is also some comparison to drews work, specifically to the maximum value of d2 image pixel.
;-----------------------------------------------------------------------------
; Make a nifty JPG file for the final images and include some nifty ephemeris data. 
  ;continuum_solar_flux_at_mercury = 4.965e14*(0.324^2./(ephemeris_structure.MERC2SUN_RANGE^2.)) ;photons/cm2 s Angstrom, value comes from Domingue et al 1997
  ang_diam =sigfig(ephemeris_structure.angular_diameter, 4)
  illum = string(ephemeris_structure.illum)
  ta = sigfig(ephemeris_structure.TRUE_ANOMALY, 3)*10.
  r = string(ephemeris_structure.merc2sun_range) 
  phs = ephemeris_structure.phi
  if ephemeris_structure.slashr eq "/L" then rel_sol_pos = "Leading"
  if ephemeris_structure.slashr eq '/T' then rel_sol_pos = "Trailing"
  
  fname = file_basename(merc_center_image)
  fname = fname.replace('.fits', '')
  fname = fname.replace('.fit', '')

 
  today = strsplit(string(systime()), ' ', /extract)
  ; example of 'today' varibale:
  ;IDL> today
  ;Wed        today[0]
  ;Jun        today[1]
  ;22         today[2]
  ;14:19:09   today[3]
  ;2016       today[4]
  curtime = strsplit(today[3], ':', /extract)
  curhr   = string(curtime[0])
  curmn   = string(curtime[1])
  cursc   = string(curtime[2])
  m = string(string(today[4])+'-'+ $
    string(today[1]) +'-'+$
    string(today[2])+' - '+$
    curhr+'h'+curmn+'m'+cursc+'s ')
  
  
  case !Version.os_family of 
    "Windows": begin 
                  img_savename = "\\lds\mascs_data\Ground_data\Casey\Mercury Processing\MercPipeline Results\Completed Pipeline Products (after 9 Aug 2016)\Images\"+obsdate + fname +' @'+m+".jpg"
                  savefile_savename ="\\lds\mascs_data\Ground_data\Casey\Mercury Processing\MercPipeline Results\Completed Pipeline Products (after 9 Aug 2016)\IDLSaveFiles\"+obsdate + fname +' @'+m+".sav"
               end
    "unix": begin 
                  img_savename = '/Volumes/mascs_data/Ground_data/Casey/Mercury Processing/MercPipeline Results/Completed Pipeline Products (after 9 Aug 2016)/Images/'+obsdate + fname +' @'+m+".jpg"
                  savefile_savename = '/Volumes/mascs_data/Ground_data/Casey/Mercury Processing/MercPipeline Results/Completed Pipeline Products (after 9 Aug 2016)/IDLSaveFiles/'+obsdate + fname +' @'+m+".sav"
            end
  endcase

  closewin
  w1 = window(dimensions = [800,700])
  w1.window.setcurrent
  rf = image(ct,  layout = [3,1,1], rgb_table = 13, /current)
  ; make the circle? 
  
  c = colorbar(target= i,position = [45,75,224,105], title = "Continuum Reflectance (kR)", /device )
  em = image(d2, layout = [3,1,2], /current, rgb_table = 13)
  c = colorbar(target= i,position = [312,75,490,105], title = "NaD2 Emission (kR)", /device )
  t = text(50 , 650, String(string(ephemeris_structure.date_obs) + " " + file_basename(merc_center_image) ) , /device, font_style = 1, font_size = 25)
  t = text(510, 570, "       Metadata", /device, font_style = 1, font_size= 20)
  t = text(510, 550, 'Images used',font_style = 2, /device)
  
  ; if there is only a single image that is being used for the pipeline, then
  ; we dont need to show more than the center image's file name and paired sky flat. 
  ; in the text function, hide=1 hides the output of the function, meaning it wont be there
  ; this approach may fail when the code wants to know what the north and south filename variables are, 
  ; since they will be undefined at this point with the keyword 'single' set. 
  r = r.compress()
  
  
  t = text(510, 530, string("   Center: "+file_basename(merc_center_image)), /device)
  t = text(510, 510, string("   North: "+file_basename(merc_north_image)), /device)
  t = text(510, 490, string("   South: "+file_basename(merc_south_image)), /device)
  t = text(510, 460, "Ephemeris", /device, font_style = 2)
  t = text(510, 440, string("   Angular Diameter: "+ang_diam+'"'), /device)
  t = text(510, 420, string("   Seeing($\sigma$):        "+string(sigfig(seeing,2))+'"'), /device)
  t = text(510, 400, string("   Illumination:   "+strmid(illum.compress(), 0,2)+'%'), /device)
  t = text(510, 380, string("   True Anomoly  :   "+strmid(strcompress(ta),1,5)+"$\deg$"), /device)
  t = text(510, 360, string("   Solar Range:   "+strmid(r,0,5)+" AU"), /device)
  t = text(510, 340, string("   Pos WRT Sun(NASA): " + (rel_sol_pos )), /device)
  t = text(510, 310, string("Max D2 Measured: "+ strcompress(round(max(d2)/10.)*10)+'kR'), /device)
  

  t = text(10,20, string("Calibrated from : " + file_basename(calibration_filepath)), /device, font_size = 7)
  t = text(10,5, string( "Saved as: " + savefile_savename), /device, font_size = 7)
  
  
endif else if keyword_set(single) then begin
;________________________________________________________________________________________________________________
; For single images of mercury, usually the 10" slicer images
  stop
  process_from_calibrations, merc_Center_image, calibration_filepath, rotation_required,$
    merc_reduced, center_ct, center_em, rows_autoremoved, rows_manuallyremoved
    
  ephem=ephemeris_structure

  stop
;-----------------------------------
  good_compare = 'n'
  while good_compare eq 'n' do begin
    print, "Ephemeris data lists the position WRT the sun as ",(ephem.slashr eq '/T' ? '"trailing:"':'"leading:"'),"."
    ; Lets do a visual check on the data at this point. The stacked images should show a "north" on top and "south" below the "center".
    check_correct_ns_labels = image(center_ct, layout= [3,1,2], rgb_table= 13, title= "Current Orientation")
  
    ; An example of leading and trailing images:
    case !Version.os_family of
      "unix": READ_JPEG, "/Volumes/mascs_data/Ground_data/Casey/Mercury Processing/mercphase_img.JPG", mercphase
      "Windows": READ_JPEG, "\\lds\mascs_data\Ground_data\Casey\Mercury Processing\mercphase_img.JPG", mercphase
    endcase
    !null = image(mercphase, /current, layout = [3,1,3])
    !null = image(reverse(mercphase, 2), /current, layout = [3,1,1])
    !null = text([40,466],[355,355],["EX: Trailing", "EX: Leading"], /device)
    !null = text(50, 450, "Position WRT Sun:", /device, font_size = 15)
    !null = text(50, 430, "Trailing or Leading? ", /device, font_size = 15)
  
    appearant_solar_position= ''; as seen in the observation.
    read, appearant_solar_position, prompt = "Does the observation appear to be (l)eading or (t)railing the sun? : "
    if strmatch(appearant_solar_position, '*l*') then appearant_solar_position = 'L'
    if strmatch(appearant_solar_position, '*t*') then appearant_solar_position = 'T'
    closewin
    case !Version.os_family of
      "unix": checkpoint_savefile = "/Volumes/mascs_data/Ground_data/Casey/Mercury Processing/hold last merc.sav"
      "Windows": checkpoint_savefile ="\\lds\mascs_data\Ground_data\Casey\Mercury Processing\hold last merc.sav"
    endcase
    save, /variables, filename = checkpoint_savefile
    print, "Checkpoint saved: ", checkpoint_savefile
    ; For calculating the scaling factor to kR, we need to provide the convolve and compare code with the wavelength resolution from the image. 
    delta_lambda = abs(corrected_observed_wavelength[d2_end]-corrected_observed_wavelength[d2_start])
  
    
    
    
    convolve_and_compare3, $
      ephemeris_structure, $      the structure that contains metadata for this observation geometry
      center_ct, $                image of the center of mercury in a quiet continuum band
      1.0, $                      angular height of one pixel row
      appearant_solar_position, $ mercury as leading or trailing the sun, from observers perspective
      delta_lambda, $             the spectral integration width for each remapped pixel (~0.1-0.2 Angstroms).
      image_pixel_multiplier, $   the multiplicative constant that brings the observed image to values and units of kR
      seeing, $                   the best estimate for the effective seeing in the center continuum observation                        imcenter_offset, eq_slice_pos
      model_img, $                10x10 image array of model, blurred with best estimate of observed seeing centered on disk.
      ;full_model_img, $           256x256 image array of model, blurred with best estimate of observed seeing.
      normalization_factor, $     multiplicative value bringing observation to intensity of the model.
      imcenter_offset, $          the distance in the full size model from the center of the image array to the brightest pixel
      eq_slice_array, $           the array of the equitorial slice - used to identify the center in composite images
      eq_slice_pos;               pixel row in center continuum image that was chosen to be the equitorial pixel row.
    ;                             Since the model and observation equitorial pixel rows are shifted to match intensity locations,
    ;                             this determined distance is taken to be the same between the two images. EX: the peak equitorial intensity
    ;                             of the model lies (say at ) 4 pixels from the center of the theoretical disk. When the peak intensity of the
    ;                             observation is put in the same place and the seeing is estimated, then the center of the simulated disk can
    ;                             reasonably be taken as the center of the observed disk.
    read, good_compare, prompt = "Satisfied with model building and seeing estimate? : (y/n)"
  
  endwhile
  closewin


  ;-------------Final Image Products-------------
  ; The image pixel multiplier simply puts "counts" or "reduced ADU"
  ; into values and units of kiloreyleighs.
  ; (for some reason, the image pixel multiplier is an array of length 1, so we extract it
  ; and essentially make it a float-type value.
  
  d2 = image_pixel_multiplier[0]*center_ct   ;|
  ct = image_pixel_multiplier[0]*center_em   ;|
  ;----------------------------------------------
  
  save, /variables, filename = checkpoint_savefile
  print, "Check point Saved! Restore the .sav file:",checkpoint_savefile
  stop

;------------------------------------
; Make a nifty JPG file for the final images and include some nifty ephemeris data. 
  ;continuum_solar_flux_at_mercury = 4.965e14*(0.324^2./(ephemeris_structure.MERC2SUN_RANGE^2.)) ;photons/cm2 s Angstrom, value comes from Domingue et al 1997
  ang_diam =sigfig(ephemeris_structure.angular_diameter, 4)
  illum = string(ephemeris_structure.illum)
  ta = sigfig(ephemeris_structure.TRUE_ANOMALY, 3)*10.
  r = string(ephemeris_structure.merc2sun_range) 
  phs = ephemeris_structure.phi
  if ephemeris_structure.slashr eq "/L" then rel_sol_pos = "Leading"
  if ephemeris_structure.slashr eq '/T' then rel_sol_pos = "Trailing"
  
  fname = file_basename(merc_center_image)
  fname = fname.replace('.fits', '')
  fname = fname.replace('.fit', '')

  obsdate = redate(merc_Center_image)
  today = strsplit(string(systime()), ' ', /extract)
  ; example of 'today' varibale:
  ;IDL> today
  ;Wed        today[0]
  ;Jun        today[1]
  ;22         today[2]
  ;14:19:09   today[3]
  ;2016       today[4]
  curtime = strsplit(today[3], ':', /extract)
  curhr   = string(curtime[0])
  curmn   = string(curtime[1])
  cursc   = string(curtime[2])
  m = string(string(today[4])+'-'+ $
    string(today[1]) +'-'+$
    string(today[2])+' - '+$
    curhr+'h'+curmn+'m'+cursc+'s ')
  
  

  img_savename = string(curdir + '\IntegratedCodeNMeta\Completed Pipeline Products\Images\' +obsdate +' '+ fname +' @'+m+".jpg")
  savefile_savename = string(curdir + '\IntegratedCodeNMeta\Completed Pipeline Products\IDLSaveFiles\'+obsdate+' ' + fname +' @'+m+".sav")
            

  closewin
  w1 = window(dimensions = [800,700])
  w1.window.setcurrent
  rf = image(ct,  layout = [3,1,1], rgb_table = 13, /current, title = "Reflected Continuum")
  ; make the circle? 
  
  c = colorbar(target= i,position = [45,75,224,105], title = "Continuum Reflectance (kR)", /device )
  em = image(d2, layout = [3,1,2], /current, rgb_table = 13, title = "NaD2 Map")
  c = colorbar(target= i,position = [312,75,490,105], title = "NaD2 Emission (kR)", /device )
  t = text(50 , 650, String(string(ephemeris_structure.date_obs) + " " + file_basename(merc_center_image) ) , /device, font_style = 1, font_size = 25)
  t = text(510, 570, "       Metadata", /device, font_style = 1, font_size= 20)
  t = text(510, 550, 'Images used',font_style = 2, /device)
  
  ; if there is only a single image that is being used for the pipeline, then
  ; we dont need to show more than the center image's file name and paired sky flat. 
  ; in the text function, hide=1 hides the output of the function, meaning it wont be there
  ; this approach may fail when the code wants to know what the north and south filename variables are, 
  ; since they will be undefined at this point with the keyword 'single' set. 
  r = r.compress()
  
  
  t = text(510, 530, string("   Center: "+file_basename(merc_center_image)), /device)
  t = text(510, 460, "Ephemeris", /device, font_style = 2)
  t = text(510, 440, string("   Angular Diameter: "+ang_diam+'"'), /device)
  t = text(510, 420, string("   Seeing($\sigma$):        "+string(sigfig(seeing,2))+'"'), /device)
  t = text(510, 400, string("   Illumination:   "+strmid(illum.compress(), 0,2)+'%'), /device)
  t = text(510, 380, string("   True Anomoly  :   "+strmid(strcompress(ta),1,5)+"$\deg$"), /device)
  t = text(510, 360, string("   Solar Range:   "+strmid(r,0,5)+" AU"), /device)
  t = text(510, 340, string("   Pos WRT Sun(NASA): " + (rel_sol_pos )), /device)
  t = text(510, 310, string("Max D2 Measured: "+ strcompress(round(max(d2)/10.)*10)+'kR'), /device)
  

  t = text(10,20, string("Calibrated from : " + file_basename(calibration_filepath)), /device, font_size = 7)
  t = text(10,5, string( "Saved as: " + file_basename(savefile_savename)), /device, font_size = 7)
    ;circle2 = ellipse(5.5,updated_equatorial_pixel_row, major = ephemeris_structure.ANGULAR_DIAMETER*.5, FILL_TRANSPARENCY = 80, /data, theta = [0,45,0], target = rf)
;________________________________________________________________________________________________________________
endif

save, /variables, filename = savefile_savename
print, "Data saved at :"
print, savefile_savename
rf.title= "Continuum"
em.title= "NaD2 Emission"
em.save, img_savename



print, "Saved image at : "
print, img_savename
print, ''
print, "Calibration used on this image:"
print, calibration_filepath
print, 'COMPLETE'

return

end



;pro Drewcomp, imgname
;;+
;; this simply images Potter's final images and puts them into a nice window, 
;; showing the max of the d2 image array in terms of kilorayleighs. 
;; 
;;  imgname is the filepath of Potter's fits file. 
;;  
;;-
;img = readfits(imgname)
;hdr = headfits(imgname)
;print, hdr
;
;
;original = ''
;read, original, prompt = "Enter original center image name:  "
;
;
;
;wDrew = window()
;wDrew.window.setcurrent
;
;cont =  image(img[25:50,50:80], rgb_table= 13, layout = [2,1,1], title ="Cont")
;d2   =  image(img[75:100, 50:80], rgb_table = 13, layout = [2,1,2], /current, title = "D2")
;t = text(142,460,string(original + '  '+strmid(sxpar(hdr,'date-obs'),0,10)), /device)
;t = text(375,90, string("Max d2 pixel: " +string(max(img[78:100,54:77]))), /device)
;
;end
