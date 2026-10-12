local case_number = 0

function Header(el)
  if el.classes:includes("case") then
    case_number = case_number + 1
    el.content:insert(pandoc.Str(tostring(case_number)))
    return el
  end
end