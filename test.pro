vertecies = list()
for x=0, asmooth.length-1 do begin
  if ((asmooth[x] gt asmooth[x+1]) && (asmooth[x+2] lt asmooth[x+1])) then vertecies.add(x)
  endif
endfor
end
