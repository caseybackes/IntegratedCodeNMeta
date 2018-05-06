pro merc_ephem, file ,  output_structure

; observation date will usualy come in new fits date format, example : 2014-11-04T20:25:41
obsdate_to_match_in_csv = redate(file, /m_format, /time)
obsdate = strmid(obsdate_to_match_in_csv, 0 ,11) 
obstime = strmid(obsdate_to_match_in_csv, 12,6)

;stop

; last known path to the ephemeris csv file.
ephemeris_csv_file = "C:\Users\Casey Backes\Documents\IDLWorkspace84\Default\MercuryResearch\IntegratedCodeNMeta\mercury ephemeris.csv"
print, "Looking up ephemeris information..."
; updated csv file that includes phase angle for use in hapke code
ephemeris_data = READ_CSV(ephemeris_csv_file, count = 10 , HEADER=EphHeader, $
  N_TABLE_HEADER=1, TABLE_HEADER=EphTableHeader,Record_start = 0)

  ; retrieve metadata for date of observation
  ephemeris_index = where(ephemeris_data.field01 eq obsdate_to_match_in_csv)
  

  ; if the relevent date is not in the ephemeris.csv file, print error.
  if (ephemeris_index eq -1) then begin
    print, "###########"
    print, "Observation date not found!" 
    print, "'http://ssd.jpl.nasa.gov/horizons.cgi#top' to obtain the relevent ephemeris data."
    print, "###########"
    output_structure = "ERROR! Observation date not found!"
    return
  endif
  
  
  if (ephemeris_index gt -1) then begin
    output_structure = {$
      date_obs: obsdate,$
      time_obs: obstime,$
      RA:ephemeris_data.field02[ephemeris_index], $
      DEC:ephemeris_data.field03[ephemeris_index], $
      AP_mag:float(ephemeris_data.field04[ephemeris_index]), $
      S_brt:float(ephemeris_data.field05[ephemeris_index]), $
      Illum:float(ephemeris_data.field06[ephemeris_index]), $
      Def_illum:float(ephemeris_data.field07[ephemeris_index]), $
      Angular_diameter:float(ephemeris_data.field08[ephemeris_index]), $
      Merc2sun_range:float(ephemeris_data.field09[ephemeris_index]), $
      Merc2sun_velocity:float(ephemeris_data.field10[ephemeris_index]), $
      Merc2earth_range:float(ephemeris_data.field11[ephemeris_index]), $
      Merc2earth_velocity:float(ephemeris_data.field12[ephemeris_index]), $
      VmagSn:float(ephemeris_data.field13[ephemeris_index]), $
      VmagOb:float(ephemeris_data.field14[ephemeris_index]), $
      SOT:float(ephemeris_data.field15[ephemeris_index]), $
      slashr:ephemeris_data.field16[ephemeris_index], $
      STO:float(ephemeris_data.field17[ephemeris_index]), $
      True_Anomaly:float(ephemeris_data.field20[ephemeris_index]), $
      phi:float(ephemeris_data.field21[ephemeris_index]), $
      PAB_LON:float(ephemeris_data.field22[ephemeris_index]), $
      PAB_LAT:float(ephemeris_data.field23[ephemeris_index]) }
    print, "...got it!"  
  endif

return
end
