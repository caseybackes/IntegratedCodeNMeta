;+
; takes solar spectrum and scales it to mercury spectrum based on a chai sqr fit
; around 'center' position (usually the d2 emission peak) with size 'width' in terms of array elements
;
;
; 
;-



function scale2match, solar_spectrum, merc_spectrum, d2_peak, d2_bound, RIGHTBOUND = rightbound, LEFTBOUND= leftbound ; center should be d2 peak



; do a chai sqr fit on the observed and solar spectra. 
; the observed spectrum is taken as the model, the solar spectrum is taken
; as the data. the data array will be scaled to the model array for the best fit
; in terms of a least squares fit.  

line_halfwidth =abs(d2_peak-d2_bound)


if keyword_set(RIGHTBOUND) then begin 
  right_limit = rightbound
endif else right_limit = d2_peak+line_halfwidth+30
if keyword_set(LEFTBOUND) then begin
  left_limit = leftbound
endif else begin
  left_limit = d2_peak-line_halfwidth-30
  if left_limit lt 0 then left_limit = 0
endelse


;print, "left limit: ",left_limit, "right limit : ", right_limit




solar_spectrum_range1 = solar_spectrum[left_limit: d2_peak - line_halfwidth]
solar_spectrum_range2 = solar_spectrum[d2_peak + line_halfwidth: right_limit]

merc_spectrum_range1 = merc_spectrum[left_limit: d2_peak - line_halfwidth]
merc_spectrum_range2 = merc_spectrum[d2_peak + line_halfwidth: right_limit]


solar_spectrum_nod2 = [solar_spectrum_range1,solar_spectrum_range2]
merc_spectrum_nod2 = [merc_spectrum_range1,merc_spectrum_range2]
;plt = plot(merc_spectrum_nod2, color = 'orange')
;stop
chisqrold  = 1e10
  for scale=0.001,5e3,0.001 do begin
    solar_spectrum_scaled = solar_spectrum_nod2*scale
    x2, solar_spectrum_scaled, merc_spectrum_nod2, chisqrnew
    ;print, "chisqr: ", chisqrnew
    
    if chisqrnew gt chisqrold then break
    chisqrold = chisqrnew
    ;print, "X2: checking scale: ", scale

  endfor
;plt=plot(solar_spectrum_scaled*scale, color = 'red')
;plt=plot(merc_spectrum_nod2, color = 'blue', /overplot)
;print, "Min X2 (",chisqrold,") found with scaling factor ",scale
return, scale*solar_spectrum
end
