function conky_leah2_time(...)
  local t = conky_parse('${time %a %d %b, %H:%M:%S %Z}')
  return string.lower(t)
end

function conky_leah2_mori(...)
  -- memento mori
  return string.format("%.2f", (os.time() - 550239000) / 3600 / 24 / 365.25 - 0.005)
end

function conky_strip(s)
  return conky_parse(s):match("%s*(%w+)%s*") or ''
end
