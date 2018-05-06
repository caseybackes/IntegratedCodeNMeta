pro x2, model, data, DATAERR=dataerr, result, RTRIM=rtrim, LTRIM = ltrim, RED = red, DEC=dec

if model.length ne data.length and not keyword_set(rtrim) and not keyword_set(ltrim) then begin
  print, "Arrays must be the same size"
  ;stop
  return
;test for match
endif else if model.length ne data.length and keyword_set(rtrim)then begin
  ml = model.length & dl = data.length
  if ml gt dl then begin
    dif = ml-dl
    model = model[0:-1*dif]
  endif else if ml lt dl then begin
    dif = dl - ml 
    data = data[0:-1*dif]
  endif else begin
    if model.length ne data.length and keyword_set(ltrim) then begin
      ml = model.length & dl = data.length
      if ml gt dl then begin
        dif = ml - dl
        model = model[dif:-1]
      endif else if ml lt dl then begin
        dif = dl - ml
        data = data[dif:-1]
      endif
    endif
  endelse
endif
if not isa(dataerr) then begin
  dataerr = []
  foreach element,data do dataerr=[dataerr,sqrt(abs(element))]
endif
if not keyword_set(red) then result = total((model-data)^2.0/dataerr)
if keyword_set(red) then begin 
 
 ;errs = [] & foreach element,data do errs = [errs,1.0/sqrt(element)]  
 result = total((model-data)^2 / dataerr)
endif  

return

end 