
 function Foo()
	local x = 5 * 7
	print(x)
	return x + 3
end

local function main()
	local bar = Foo()
	print(bar)
end

main()

