;+
;INPUTS:
;       corrected_observed_wavelength:  the doppler shifted wavelength values of the
;                                       observed spectrum in the target img
;       observed_spectrum:              the mean of the observed slices, avgd into a 1D array
;
;OUTPUTS:
;       final_observed_spectrum:        the spectrum of the target image trimmed to match the
;                                       bounds of the solar spectrum
;       final_observed_wl:              the wavelength array that starts and stops at the same
;                                       wavelengths as the solar spectrum.
;       final_solar_spectrum:           solar spectrum trimmed to match the bounds of the
;                                       target spectrum
;       final_solar_wl:                 the wavelength array that stars and stops a tthe same
;                                       wavelengths as the target spectrum
;
;
;
;-


pro match_spectral_bounds4, corrected_observed_wavelength, observed_spectrum, merc_regd,$ ; inputs
  final_observed_spectrum, final_observed_wl, final_solar_spectrum, final_solar_wl, trimmed_merc_image ; outputs
  ; access the 'spectrum' and 'wl' arrays for the reference solar spectrum
  restore, "C:\Users\Casey Backes\Documents\IDLWorkspace84\Default\MercuryResearch\IntegratedCodeNMeta\BASS 2000 solar spectrum.sav"
  ; wl = solar wavelength array
  ; spectrum = solar spectrum array centered on the sodium dublet.
   
  
  ; (1) match the begining and ending values for the solar "wl" and the merc "corrected_observed_wavelength"
      ; (a) flip the solar data to match the merc data, if necessary
      if corrected_observed_wavelength[0] lt corrected_observed_wavelength[1] then begin; the merc spectrum is low to high
        spectral_order = 'pos'
      endif else begin 
        spectral_order = 'neg'; the merc spectrum goes high to low.
        ; the restored solar spectrum data is already given to us by Boulder Astronomy and Space Society as positive order, 
        ; so if the merc spectrum is negative, we need to flip the solar spectrum from positive to negative
        wl = reverse(wl)
        spectrum =reverse(spectrum)
      endelse
      
      ; (b) determine smaller wavelength range, cut the other spectrum outside this range
      ; example of logic below, z = (a GT b) ? a : b

      lower_spectral_bound = (min(wl) gt min(corrected_observed_wavelength)) ?  min(wl) : min(corrected_observed_wavelength)
      upper_spectral_bound = (max(wl) lt max(corrected_observed_wavelength)) ?  max(wl) : max(corrected_observed_wavelength)
      if spectral_order eq 'pos' then begin
         ;adjust merc
        final_observed_wl = corrected_observed_wavelength[valueposition(corrected_observed_wavelength,lower_spectral_bound):valueposition(corrected_observed_wavelength,upper_spectral_bound)]
        final_observed_spectrum = observed_spectrum[valueposition(corrected_observed_wavelength,lower_spectral_bound):valueposition(corrected_observed_wavelength,upper_spectral_bound)]
        ; and trim the mercury image to have the same common spectral bounds
        trimmed_merc_image = merc_regd[valueposition(corrected_observed_wavelength,lower_spectral_bound):valueposition(corrected_observed_wavelength,upper_spectral_bound),*]
        
        ;adjust solar
        final_solar_wl = wl[valueposition(wl,lower_spectral_bound):valueposition(wl,upper_spectral_bound)]
        final_solar_spectrum = spectrum[valueposition(wl,lower_spectral_bound):valueposition(wl,upper_spectral_bound)]
        
      endif else begin ; for neg spectral order
        ;adjust merc
        final_observed_wl = corrected_observed_wavelength[valueposition(corrected_observed_wavelength,upper_spectral_bound):valueposition(corrected_observed_wavelength,lower_spectral_bound )]
        final_observed_spectrum = observed_spectrum[valueposition(corrected_observed_wavelength,upper_spectral_bound):valueposition(corrected_observed_wavelength,lower_spectral_bound )]
        ; and trim the mercury image to have the same common spectral bounds
        trimmed_merc_image =merc_regd[valueposition(corrected_observed_wavelength,upper_spectral_bound):valueposition(corrected_observed_wavelength,lower_spectral_bound ),*]
        
        ;adjust solar
        final_solar_wl = wl[valueposition(wl,upper_spectral_bound):valueposition(wl,lower_spectral_bound )]
        final_solar_spectrum = spectrum[valueposition(wl,upper_spectral_bound):valueposition(wl,lower_spectral_bound )]
      endelse
      
      
      ; rebin the solar data
      solar_spectrum = congrid(final_solar_spectrum, final_observed_wl.length) 
      rough_scale_factor = abs(regress(solar_spectrum, final_observed_spectrum))
      rough_scale_factor =rough_scale_factor[0]
      final_solar_spectrum = final_solar_spectrum*rough_scale_factor
      final_solar_wl = congrid(final_solar_wl, final_observed_wl.length)
      final_solar_spectrum = congrid(final_solar_spectrum, final_observed_wl.length)
      p = plot(final_observed_wl, final_observed_spectrum, color = 'blue', title = "Common Spectral Bounds")
      p = plot(final_solar_wl,final_solar_spectrum, color = 'red', /overplot)
      t = text(100,10,"This is how we know we are correctly subtracting the solar spectrum from the observed spectrum of Mercury.")
            
  return
end


