;+
;NAME:
;singlemercproc
;
;PROCEDURES AND FUNCTIONS USED: 
; The relevent procedures and functions that are not included in the standard IDL library.
; DARK_REDUCE3
; DARKNFLAT2
; SLICEBOUNDS
; REGISTER5
; MERC_EPHEM
; DETERMINE_WAVELENGTH_SCALE
; VLINE
; MATCH_SPECTRAL_BOUNDS4
; SPECTRAL_INTEGRATION_BOUNDS 
; MAN_IMBUILD2
; 
; 
; 
;PURPOSE:
;Process a single spectral image of Mercury and obtain calibration parameters for identically processing additional images.
;
;CALLING SEQUENCE:
;
;singlemercproc, merc_center, continuum_reflectance_image, emission_image, pos_savefile_name
;
;INPUTS:
;
;merc_center: the file path to the mercury image to reduce and calibrate, need not be a "center" image. 
;
;OUTPUTS: 
;
;continuum_reflectance_image: the named variable of the image of Mercury in the light of the reflected continuum. 
;emission_image: the named variable of the image of Mercury in the light of the D2 sodium emission. 
;pos_savefile_name: the named varible of the IDL save file that holds the calibrations for the input image.  
;
;KEYWORD PARAMETERS:
;
;None yet, though DIFCAL and NEW may be implemented in the future to accomodate holing muliple sets of calibrations for a single observing day. 
;
;SIDE EFFECTS:
;
;Causes mulitple windows and plots to open on screen. User interaction is required in several of the processing steps.
;There are occational stops that must be advanced through manually (".c" in IDL Console/shell or "Resume" button in IDE). 
;Sometimes the plot procedure will freeze up if the mouse is allowed to hover over an open ATV window, and is usually mitigated
;by clicking on the IDL console and back to the plot (procedure) window. If not, its required to close IDL and start again (sorry). 
;
;RESTRICTIONS:
;
;This program won't do anything else. Sorry, them's the
;restrictions.
;
;
;MODIFICATION HISTORY:
;Written sometime in early 2016 by Casey Backes, University of Colorado at Boulder, Astrophysical and Planetary Sciences
;under the guidance of Dr. Aimee Merkel, Dr. Tim Cassidy, and Dr. Rosemary Killeen for the project of archiving nearly 30 
;years of spectral observations of the D2 sodium component of Mercury's exosphere.   
;-





;_________________________________________________________________________________
pro singlemercproc, merc_center, $
  continuum_reflectance_image, emission_image, pos_savefile_name, SINGLE=single

dark_array = dark_reduce3(merc_center)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1) Get the associated sky, get timestamp to merc
; PARAMETERS SAVED FOR FUTURE PROCESSING:
;   ROTATION_REQUIRED: a boolean value, 1 for yes, 0 for no. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;if not keyword_set(single) then darkNflat3, merc_center, dark_array, merc_bright, sky_raw_img, sky_bright, sky_raw_name, merc_reduced, merc_timestamp,rotation_required else $;; find the sky for the center
; darkNflat3, merc_center, dark_array, merc_bright, sky_raw_img, sky_bright, sky_raw_name, merc_reduced, merc_timestamp, rotation_required, /single
;;closewin
if not keyword_set(single) then batch_dark_rot, merc_center, dark_array, merc_bright, sky_raw_img, sky_bright, sky_raw_name, merc_reduced, merc_timestamp,rotation_required else $;; find the sky for the center
batch_dark_rot, merc_center, dark_array, $  ; input
  merc_bright, sky_raw_img, sky_bright, sky_center, merc_reduced, merc_timestamp, rotation_required, SINGLE=single; output



; get the current system time and use it to identify this iteration of calibration. 
today = strsplit(string(systime()), ' ', /extract)
; example of 'today' varibale:   -mostly a development note...
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
m = string(string(today[4])+'-'+$
   string(today[1]) +'-'+$
   string(today[2])+' @'+$
   curhr+'h'+curmn+'m'+cursc+'s ')

case !Version.os_family of 
  "Windows": pos_savefile_name = string('C:\Users\Casey Backes\IDLWorkspace84\Default\MercuryResearch\IntegratedCodeNMeta\calibration_params\processing calibrations for ' +  merc_timestamp + ' created on '+ m +'.sav')
  "unix":    pos_savefile_name = string('/Volumes/mascs_data/Ground_data/Casey/Mercury Processing/calibration_params/processing calibrations for '+  merc_timestamp + ' created on '+ m +'.sav')  
endcase

if isa(rotation_required) eq 0 then rotation_required = 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 4) Determine the slice boundaries and register the image. 
; PARAMETERS SAVED FOR FUTURE PROCESSING:
;   SHEAR_MATRIX: 4x4 matrix, each of the four row vectors is an input for the warp_tri function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


save, /variables, filename = 'C:\Users\Casey Backes\Documents\IDLWorkspace84\Default\VarsBeforeSmartslice.sav'
imreg_y, merc_bright, merc_regd, shear_matrix
xi = shear_matrix[*,2] &   xo = shear_matrix[*,0]  & yi = shear_matrix[*,3]   &  yo = shear_matrix[*,1]

;image_registration, merc_bright, sheared_img, transformation_matrix
merc_regd = warp_tri(xo,yo,xi,yi,merc_reduced)
sky_regd = warp_tri(xo,yo,xi,yi,sky_bright)
;sky_regd[*,0:slice_indices[0]] = 0
;sky_regd[*,slice_indices[-1]:*]=0


merc_reduced = flatfield(merc_regd,sky_regd)


; ALPHA TESTING***************
smartslice, merc_reduced, slice_width, slice_indices
merc_reduced[*,0:slice_indices[0]] = 0
merc_reduced[*,slice_indices[-1]:*]=0
atv, merc_reduced


;/ALPHA TESTING***************
save, /variables, filename = 'C:\Users\Casey Backes\Documents\IDLWorkspace84\Default\VarsAfterSmartslice.sav'

imreg_x, merc_reduced, slice_indices, merc_reduced, transformation_matrix
;; *** OBSOLETE: REGISTRATION OF THE CLEAN MERCURY IMAGE 
;; Align the slices and spectra to be perfectly horizontal and vertical, respectively. The returned 'shear matrix'
;; can then be applied to the 'warp_tri' function to subsequently register the periphreal images.  
;;*** Note: register5.pro is now obsolete with the implementation of 'image_registration.pro'
;;;;register5, merc_reduced, slice_indices, merc_regd, shear_matrix

;;image_registration, merc_reduced, slice_indices, sheared_img, trans_matrix
;stop
;; separate the registration maxtrix into vectors.
;xi = shear_matrix[*,2] &   xo = shear_matrix[*,0]  & yi = shear_matrix[*,3]   &  yo = shear_matrix[*,1]
;; if the image required nothing for registration (original data was aligned on CCD very well), then we can skip the 
;; application of the shear matrix to the images. Programmatically, we can test for this change if the x_i and x_o arrays are
;; identical and if the y_i, y_o arrays are also identical. This indicates that there is no coordinate transformation ("shearing"). 
;if not total(xi eq xo) eq 4 and total(yi eq yo) eq 4 then begin 
;
;  ; Register the raw merc and sky images, then do the subtraction, then make a flat field, then divide by flat field. 
;  merc_minus_sky = merc_bright - sky_bright
;  ;now we have a good merc image that needs to be reg'd 
;  merc_minus_sky_regd = warp_tri(xo, yo, xi, yi, merc_minus_sky)
;  
;  ;we need a flat field built from a REG'D sky image! (much nicer than not reg'd!! ) 
;  sky_regd= warp_tri(xo, yo, xi, yi, sky_bright) ; that's sky minus the dark frame. 
;  sky_summed_over_spectra=transpose(mean(sky_regd, dim=1)); now a column vector...
;  flat_field_array=sky_summed_over_spectra
;  size=merc_minus_sky.dim
;  x_size=size[0]
;  
;  ; We now take this column vector and replicate it for as many columns there are in the image. 
;  for k=1,x_size-1 do flat_field_array=[flat_field_array,sky_summed_over_spectra]
;  
;  ; Since we are using the registered version of the images, there could be rows of 0's on the bottom or top of the
;  ; reduced image. In this case, the sum across the sky image at the bottom row will be zero and when we divide by 
;  ; the flat field array we will get an infinate array ( or div by 0 error, which are basically the same thing). So anywhere 
;  ; in the FFA there is a 0, we change it to be a 1.0. 
;  flat_field_array[where(flat_field_array eq 0)] = 1.0
;  ; divide by the flat field
;  merc_regd = merc_minus_sky_regd/flat_field_array ; this is why we cant have zeros in the flat field array. 
;  closewin
;  merc_reduced1 = merc_regd
;  ;atv, merc_regd
;
;  ; If registration was required, then we have to redefine the slice boundaries.  
;  slicebounds, merc_reduced1, slice_indices
;  slice_indices = slice_indices[sort(slice_indices)]
;endif else begin
;  merc_reduced1 = merc_regd
;  sky_regd = sky_bright ; <- which is the sky image minus the dark, if a dark frame was available. 
;endelse
;            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 5) Determine the wavelength values for pixels of the registered image.
; PARAMETERS SAVED FOR FUTURE PROCESSING: 
;   SLICE_INDICES
;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;stop

;; Some images have crazy gaps between the image slices (possibly due to a less than perfect columnation of the slicer
;; and spectrograph). Repsonding positively to the following prompt will intiate an auto gap removal process
;; that looks at each row for how erradically it changes from pixel to pixel, returning a large "m" value. Largely variant/noisey
;; rows will have a relatively larger m-value than the rows with strong signal. User defines the acceptable m-value, 
;; and the program removes the offenders.
;
;ask_slicegap_removal = '' & read,ask_slicegap_removal,prompt = "Address slice gap anomalies? (y/n) : "
;if strmatch(ask_slicegap_removal, '*y*') then begin
;  
;  ask_run_autoremove_rows = '' & read, ask_run_autoremove_rows, prompt = "Run auto gap removal? (y/n) : "
;  ;1) Run the auto removal of noisy rows
;  ;-------------------------------------
;  if strmatch(ask_run_autoremove_rows, '*y*') then begin
;    ; run the auto gap removal tool.  
;    remrows2, merc_reduced1, slice_indices, merc_reduced2,  rows_autoremoved
;    ; once rows are removed, we have to take stock of the slice indices again. 
;    print, "Now that we have removed some pixel rows from the observation, we need to redefine the slice boundaries."
;    slicebounds, merc_reduced2,slice_indices
;    slice_indices = slice_indices[sort(slice_indices)]
;  ; if we made no changes, then merc_reduced2 is simply a copy of merc_reduced1, where merc_reduced2 will be the standard
;  ; name that gets fed into the nex question of row removal. 
;  endif else merc_reduced2 = merc_reduced1
;  
;  
;  
;  ask_if_remrows_manual = '' & read,ask_if_remrows_manual, prompt = "Run manual slice clean up (spikes, gaps) ? "
;  ;2) Run the manual gap removal 
;  ;------------------------------
;  ;now we have to see if there are other rows that need to be removed manually. 
;  ; after manual row removal (or not) the merc image is then called "merc_reduced3"
;  if ask_if_remrows_manual eq 'y' then remrows_manual, merc_reduced2, slice_indices, merc_reduced3, rows_manuallyremoved else merc_reduced3 = merc_reduced2
;    ;else merc_reduced3 = merc_reduced2
;      
;    ask_keep_remrows = ''
;    read, ask_keep_remrows, prompt = "Use the gapless image going forward?  (y/n)"
;    if strmatch(ask_keep_remrows, '*y*') then begin 
;      slicebounds, merc_reduced3, slice_indices
;      slice_indices = slice_indices[sort(slice_indices)]
;    ; if the rows removed by hand make the image look like tihs then this last phase should only reflect the changes since last phase of row removal.  
;    endif else begin 
;      merc_reduced3 = merc_reduced2 
;      rows_manuallyremoved = 'none' 
;      rows_autoremoved = 'none'
;    endelse
;  
;  ; if we didnt go through the gap reduction process (from having a good image in the first place) then
;  ; we can jump to naming the merc_reduced1 image as the merc_regd_gapless image which will be used hereafter.
;endif else merc_reduced3 = merc_reduced1
;
;; for calibration saving purposes, set the rows removed (manual and auto) to 'none' if we didnt use row reduction:
;if not isa(rows_manuallyremoved) then rows_manuallyremoved = 'none'
;if not isa(rows_autoremoved) then rows_autoremoved = 'none'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 6) Solar Spectrum Reduction from the mercury spectrum
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; from the date of the observation of mercury, retrieve the super cool ephemeris spreadsheet 
; I have at \\lds\mascs_data\Ground_data\Casey\ephemeris.csv. The ephemeris variables for this
; particular observation day (ephem data of 20:00 that day) will be saved to 'ephemeris_strucure'
; THIS COULD BE PUT INTO ITS OWN 'FORMAT_OBSDATE.PRO' KIND OF FILE. 
observation_date = sxpar(headfits(merc_center), 'date-new') ; if 'date-new' is in header, it is fits format. common in 1999. 
if observation_date eq 0 then observation_date= sxpar(headfits(merc_center), 'date-obs')
if observation_date eq 0 or observation_date eq '' then begin
  d= dialog_message("A valid observation date is required to obstain ephemeris data. Put observation date into command line.", /information)
  stop
endif

; obtain general ephemeris information for this observation day. It is taken from Horizons JPL web applet
; and the time of each ephemeris is 20:00UT on the observation day, which is about half way through the observing
; sessions.
merc_ephem, merc_center, ephemeris_structure

merc_regd_gapless = merc_reduced
; set the observed spectrum by taking the mean of all slices in the slice direction. this leaves out the
; noise areas above and below the region of the image that contains the slices. with so many rows of pixels, the 
; natural noise within the spectrum is significantly reduced (by 1/sqrt(N) )  and gives a smoother signal. 
save, /variables, filename = 'C:\Users\Casey Backes\Documents\IDLWorkspace84\Default\VarsBeforeWavelength.sav'
stop,'At this point, work on developing the "fit_spectral_bounds.pro " function to pick out waterlines. This serves to automate the wavelength fit that takes place in "determine_wavelength" function'
fit_spectral_wavelength, merc_reduced, abs_lines, width = 30


wavelength_check = 'n'
save, /variables, filename = 'C:\Users\Casey Backes\Documents\IDLWorkspace84\Default\VarsBeforeWavelength.sav'
stop
while wavelength_check eq 'n' do begin
  determine_wavelength_scale, sky_regd, slice_indices, observed_wavelength

  observed_spectrum =  median(merc_reduced[*,slice_indices[0]:slice_indices[-1]], dim = 2); this will work even if slice boundaries were manually chosen
  size_of_observed_spectrum = observed_spectrum.length
  
  ; determine the doppler shift of the merc spectrum
  c = 299792.458 ;km/s, c must be given in km/s to match units with Horizons Ephemeris velocity data output. 
  merc_earth_velocity = ephemeris_structure.merc2earth_velocity ; km/s
  merc_sun_velocity = ephemeris_structure.merc2sun_velocity; km/s
;   
  d2_emission_wavelength = 5889.9500 ; Angstroms
  ;d2 emission is subject to one doppler shift with respect to mercury/earth relative radial velocity. 
  ;d2_dopshift = sigfig( 5889.9500*merc_earth_velocity*1.00/c, 8)/10.00
  d2_dopshift = 5889.9500*merc_sun_velocity*1.00/c
  
  d2_dopshift = d2_dopshift[0]
  d2_shifted_line = d2_emission_wavelength - d2_dopshift
  
  ;the solar continuum reflected from mercury is subject to two doppler shifts: (1) from mercury/sun 
  ;radial velocity and (2) mercury/earth radial velocity. the cumulative velocity is what determines
  ;the size of the doppler shift for the continuum.  
  ; for some reason, the ephemeris data comes out as array elements, so we have to pull it out
  ; of array form so that it can give a constant for a dopshift. 
  ; we found that leaving it as it was meant a 1024 element array - 1 element array = 1 element array. 
  continuum_dopshift =   5889.9500*( merc_sun_velocity + merc_earth_velocity)/c 
  continuum_dopshift = continuum_dopshift[0]
  ; now apply the doppler shift to the mercury wavelength scale
  corrected_observed_wavelength = observed_wavelength  -continuum_dopshift
  ;d2_shifted_line+=continuum_dopshift
  
  ;d = dialog_message(string("I have yet to integrate the solar spectrum reduction in to the pipeline. I have to decide how best to do the data reduction. I think we should apply the solar reduction subroutine to each subslice within the primary slices  when the image is being remapped into pixels. Each pixel (subslice) should have the solar spectrum subtracted from it  but this requires it to be integrated into the imbuild.pro (or manimbuild.pro) file. "), /information)
  plot,corrected_observed_wavelength, $
    observed_spectrum, $
    xrange = [5889.5, 5890.5],$
    ;yrange = [-.1,.3], $
    title = "Spectrum after Dmoppler Shift"
  vline, d2_emission_wavelength
  vline, d2_shifted_line
  xyouts, 5889.9, 0.2, "If the lines are at D2 emsission peak and absorption minimum, "
  xyouts, 5889.9, 0.18 ,"then wavelength calibration and Doppler shifts are all correct."
  wait, .1
  print, "Instructions: "
  print, $
    "In the plot, you should see a vertical line at the 5889.95A wavelength, "$
    ,"which is where the bottom of the D2 Fraunhofer absorption line should be. "$
    ,"The other vertical line should intersect the top of the D2 sodium emission line. "$
    ,"This is how we know we have a good wavelength calibration for the observed spectra."
  save, /variables, filename = "C:\Users\Casey Backes\Documents\IDLWorkspace84\Default\MercuryResearch\IntegratedCodeNMeta\processing_holds\processing_hold.sav"
  read, wavelength_check, prompt = "Wavelength calibration correct (or good enough)?: (y/n)"
endwhile
;wdelete

stop

;
 match_spectral_bounds4, corrected_observed_wavelength, observed_spectrum, merc_reduced, $ ; inputs
  final_observed_spectrum, final_observed_wl, final_solar_spectrum, final_solar_wl, trimmed_merc_image ;outputs


wdelete
  
; UPDATE: 5/15
      ; i think this is the place where I stopped and worked on spectral_fit.pro 
      ; to find the overlapping bounds of the mercury and solar spectra, and reduce the 
      ; solar light from the mercury light. 
; UPDATE: 5/18
      ; I have the first part of 'spectral_fit.pro' code separated into a new procedure 
      ; file that matches the spectral limits of the mercury and solar wavelengths and 
      ; the corresponding spectal intensities at those wavelengths. But the output arrays 
      ; are not scaled to a normalized intensity. That process must be done for every 
      ; subslice in the image building procedure 'imbuild.pro' or 'man_imbuild.pro' which
      ; equates to every pixel in the final remapped image. This is because the intensity 
      ; of reflected solar light changes with latitude (primary slices in the reduced spectral image are unique in latitude)
      ; so the solar light must be scaled to each subslice before reduced from each subslice. 
      ; Im going to put the second half of the 'spectral_fit.pro' code (which iteratively scales the 
      ; solar spectrum to match the observed spectrum intensities) into the man_imbuild.pro code
      ; since its the image building code that can take arbitrary slice boundaries - which is 
      ; apprearing to become the dominate way to get slice boundaries. 
; UPDATE: 5/24
      ; the second half of "spectral_fit.pro" is now contained in "scale2match.pro", which is a *function* that returns 
      ; the appropriately scaled down version of the solar spectrum with correct left and right spectral limits as afforded by 
      ; "match_spectral_bounds.pro". 
      ; with this returned array from spectral_fit(), we can subtract it from the final_observed_spectrum to isolate the 
      ; emission spectrum from Mercury. This is done iteratively within the manual image building procedure, "man_imbuild.pro". 
      ; It is done for every subslice within each of the (usually 10) primary slices in the spectral image. This is how we 
      ; account for the decreasing solar spectrum intensity as a function of latitude. 
      ;     
      ;     
      ;     
      ;     
; Chat with Tim: 
;                  you can get a better scaling of solar spectrum to merc spectrum if you avoid the region where the d2 emission is. set some sort of 'exclude' statement
;                  in the x2 routine that evaluates the difference between the solar and merc spectra to determine the best fit. 
;                 
;                  we also discussed the validity of using a constant width "d2 filter". it looks to me that
;                  0.136 Angstroms will be the best width, based on *ONE* observation of the d2 width. which is 
;                  indeed bad sampling but it gives a decent "order of magnitude" estimate of 0.135A width.  
;                  the broadening wont change appreciably with the low level dispersion that we have, so a constant
;                  width d2 filter (roughly 0.13 A wide) is indeed a valid approach. the only complication is that 
;                  these start and stop wavelength values must be found in terms of pixel location within the raw image
;                  (well, not "raw", but flat fielded and registered... so "mostly reduced" data, if "fully reduced" is the 
;                  remapping from spectral lines into a 2D image of the Mercury crescent. )
;                  





;stop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 7) Establish wavelength integration bounds for d2 emission and continuum reflectance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NOTE: THIS SECTION OF CODE CAN BE OBSOLETE IF A STANDARD 'FILTER' IS DETERMINED FOR THE 
; D2 AND CONTINUUM. IF IT WERE DECIDED THAT THE FILTER SHOULD BE 0.05 ANGSTROMS, THEN THE 
; FILTER WOULD BE d2_emission_wavelength (+/-) 0.05 = 5889.90 to 5890.00 AND A FILTER OF 
; SIMILAR WIDTH 0F 0.135 ANGSTROMS SHOULD BE SET FOR THE CONTINUUM IN ORDER TO HAVE RELATIVELY
; PROPORTIONATE IRRADIANCE VALUES. 
; 
;stop

isolation_check = ''
while isolation_check ne "y" do begin
  spectral_integration_bounds, final_observed_spectrum, slice_indices, $ 
    d2_start, d2_end, CONTINUUM_START, CONTINUUM_END
  ; we also need the wavelength integration bounds in terms of wavelength as well as array elements. 
read, isolation_check, prompt = "Do you approve of this d2 integration range? (y/n): "
endwhile
wdelete
dl = final_observed_wl[2] -final_observed_wl[1]

delta_lambda_d2= abs((d2_end-d2_start)* dl)
delta_lambda_ct= abs((CONTINUUM_END-CONTINUUM_START) * dl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 8) Remapping of spectral image to 2D map of sodium distrobution 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
man_imbuild2, trimmed_merc_image, slice_indices, $
  d2_start, d2_end, CONTINUUM_START, CONTINUUM_END, $
  final_solar_spectrum, file_basename(merc_center) , emission_image, continuum_reflectance_image, /showplots

;;-----------------------------------------------------------------------------------------------------------------------------------
;;Experiment with larger continuum window
;----- RESULTS: Didnt make much of any difference on the few images i tried it out on.
;------------   But its still down here if anyone wants to try their hand at it again.  
;-------------------------------------------------------------------------------------
;; make a new continuum with a larger integration width in the spectrum
;
;range_factor = 2.0
;spectral_integration_bounds, final_observed_spectrum, slice_indices, d2_start, d2_end, CONTINUUM_START2, CONTINUUM_END2, m=range_factor
;man_imbuild2, trimmed_merc_image, slice_indices, $
;  d2_start, d2_end, CONTINUUM_START2, CONTINUUM_END2, $
;  final_solar_spectrum, file_basename(merc_center) , emission_image, continuum_reflectance_image2
;
;;compare the two images based on original scaling:
;mx = max(continuum_reflectance_image) & mn = min(continuum_reflectance_image)
;ctcheck = image(continuum_reflectance_image, rgb_table =13, layout =[2,2,1], title= range_factor)
;ctcheck = image(continuum_reflectance_image2, max_value= mx, min_value=mn, rgb_table= 13, layout=[2,2,2], /current)
;mx = max(continuum_reflectance_image2) & mn = min(continuum_reflectance_image2)
;plot, final_observed_spectrum
;vline, [continuum_start, continuum_end, continuum_start2, continuum_end2]
;
;
;ctcheck = image(continuum_reflectance_image, max_value= mx, min_value=mn, rgb_table= 13, layout=[2,2,3], /current)
;ctcheck = image(continuum_reflectance_image2, max_value= mx, min_value=mn, rgb_table= 13, layout=[2,2,4], /current)
;-----------------------------------------------------------------------------------------------------------------------------------------

;stop
print, "make changes to bounds of continuum to see the effects in new image."



; chrono, 
d2_start        = string(d2_start)           & d2_end = string(d2_end)
d2_start        = d2_start.compress()        & d2_end = d2_end.compress()
continuum_start = string(continuum_start)    & continuum_end = string(continuum_end)
continuum_start = continuum_start.compress() & continuum_end = continuum_end.compress()

em = image(emission_image, layout = [2,2,1], rgb_table= 13, /no_toolbar, title ="NaD2 Emission")
em = image(continuum_reflectance_image, layout = [2,2,2], rgb_table = 13, /current , title= "Reflected Continuum")
obs_slices = mean(trimmed_merc_image, dim = 1)
pt = plot(obs_slices, xrange = [slice_indices[0]-10, slice_indices[-1]+10], layout = [2,2,3], /current)
pts = symbol(slice_indices, obs_slices[slice_indices], "*" , sym_color = 'red',target = pt, /data)

pt2 = plot(final_observed_wl, final_observed_spectrum, layout = [2,2,4],  /current)
pts2 = symbol([final_observed_wl[d2_start], final_observed_wl[d2_end]], [final_observed_spectrum[d2_start], final_observed_spectrum[d2_end]], "o", sym_color = 'red', target = pt2, /data)
pts3 = symbol([final_observed_wl[continuum_start], final_observed_wl[continuum_end]], [final_observed_spectrum[continuum_start], final_observed_spectrum[continuum_end]], "x", sym_color = 'blue', target = pt2, /data)
t = text(180,215,string("Slice indices: "), /device)
t = text(245,55, string(slice_indices), /device)
t = text(390, 215,"D2 and Continuum Bounds: ", /device)
t = text(480,180, string("D2: [" + d2_start        + ","+ d2_end        +"]"), /device)
t = text(480,155, string("CT: [" + continuum_start + ","+ continuum_end +"]"), /device)
fname = file_basename(merc_center) & fname = fname.replace(".fits", '') & fname = fname.replace('.fit', '')
;em.save, "\\lasp-store\home\caba5860\Desktop\MercPipeline Results\SINGLEMERCPROC results of "+strmid(observation_date, 0 ,10)+fname+".jpg"

center_ct = CONTINUUM_REFLECTANCE_IMAGE
center_em = emission_image

OBSERVATION_DATE_SHORT = STRMID(OBSERVATION_DATE, 0,10)
ask_save_params = ''
read, ask_save_params,prompt= "Save calibrations? (y/n): "
if ask_save_params eq 'y' or ask_save_params eq '' then begin
  save, OBSERVATION_DATE_SHORT, ROTATION_REQUIRED, $
    SLICE_INDICES, $
    SHEAR_MATRIX, ROWS_AUTOREMOVED,ROWS_MANUALLYREMOVED, $
    EPHEMERIS_STRUCTURE, $
    CORRECTED_OBSERVED_WAVELENGTH, $
    D2_START,D2_END, $
    CONTINUUM_START, CONTINUUM_END, $
    FINAL_OBSERVED_WL,FINAL_SOLAR_SPECTRUM, dark_array, $
    center_em, center_ct, delta_lambda_d2 ,delta_lambda_ct,$
    filename = pos_savefile_name
  
  print, "Calibration saved as file: "
  print, pos_savefile_name
endif
print, "Complete with singlemercproc.pro"
print, ''
print, ''
print, "The image window you see has the continuum reflectance image and the sodium d2 emission image," $
     , " with the lower left plot illustrating where the slice indices are located and the lower right" $
     , "plot showing the bounds of integration for the each of the images. "
print, "
end
