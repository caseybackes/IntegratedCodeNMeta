function valueposition, array, value_to_locate

testarr = []

foreach element, array do begin
  testarr = [testarr, abs(element - value_to_locate)]
endforeach
answer = where(testarr eq min(testarr))

return,answer[0]
end
