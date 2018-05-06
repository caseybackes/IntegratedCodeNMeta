;+
;NAME: redate
;PURPOSE: a function that returns the reformatted date of a given fits file
;that includes the 'date-obs' or 'date-new' header units.
;CALLING SEQUENCE: redate, fitsfile[, M_FORMAT,TIME=TIME] 
;
;-


function redate, obsfile, M_FORMAT = m_format, TIME=time


observation_date = sxpar(headfits(obsfile), 'date-obs')
if observation_date eq -1 then begin
   observation_date = string(string(sxpar(headfits(obsfile), 'date-new')) +"T"+ string(sxpar(headfits(obsfile), 'UT')))
endif
;stop, observation_date
if strmatch(observation_date, '????-??-??*') then begin
  ;stop
  ; the observation date is in FITS format. change to mm/dd/yyyy
  obstime = strsplit(observation_date, 'T', /extract)
  obstime = obstime[1]
  obstime = strsplit(obstime, ":", /extract)

  obshr = obstime[0]
  obsmin = obstime[1]
  ; round obstime to nearest hour
  if obsmin gt 30 then obshr +=1

  obstime = string(string(obshr)+":00")
  obstime=obstime.compress()
  obsdate_fits = strsplit(observation_date, '[-,T]', /extract)
  year = obsdate_fits[0]
  month = fix(obsdate_fits[1])



  if keyword_set(m_format) then begin
    ; convert the month from number to MMM format
    if month eq 1 then month = "Jan" else if month eq 2 then month = "Feb" else if month eq 3 then month = "Mar" else if month eq 4 then month = "Apr" else if month eq 5 then month = "May" else $
      if month eq 6 then month = "Jun" else if month eq 7 then month = "Jul" else   if month eq 8 then month = "Aug" else   if month eq 9 then month = "Sep" else  if month eq 10 then month = "Oct" else $
      if month eq 11 then month = "Nov" else   if month eq 12 then month = "Dec"
  endif



  day = obsdate_fits[2]
  ;day = day.replace('0', '')
  obsdate = string(year + '-' + month + '-' + day)
  obsdate = obsdate.compress()

endif else if strmatch(observation_date, '??/??/??*') then begin; old fits date format, pre 2000ad
  ;                             this is in dd/mm/yy format.
  ;stop
  obsdate_oldfits = strsplit(observation_date, '/', /extract)
  year = obsdate_oldfits[2]+1900
  month = obsdate_oldfits[1]
  obstime = strmid(sxpar(headfits(obsfile), 'ut'),0,2)+":00"


  ; convert the month from number to MMM format
  if keyword_set(m_format) then begin
    if month eq 1 then month = "Jan" else if month eq 2 then month = "Feb" else if month eq 3 then month = "Mar" else if month eq 4 then month = "Apr" else if month eq 5 then month = "May" else $
      if month eq 6 then month = "Jun" else if month eq 7 then month = "Jul" else   if month eq 8 then month = "Aug" else   if month eq 9 then month = "Sep" else  if month eq 10 then month = "Oct" else $
      if month eq 11 then month = "Nov" else   if month eq 12 then month = "Dec"
  endif
  day = obsdate_oldfits[0]
  obsdate = string(string(year) + '-' + string(month) + '-' + string(day))
  obsdate = obsdate.compress()

endif
if keyword_set(time) then new_date = string(obsdate + " " + obstime)
if not keyword_set(time) then new_date = string(obsdate)
return, new_date
end
